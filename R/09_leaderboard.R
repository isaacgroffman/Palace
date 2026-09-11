# LEAGUE LEADERBOARD ENGINE
# ------------------------------------------------------------
# One Arrow pushdown aggregation, grouped by
# Pitcher x PitcherTeam x TaggedPitchType, feeds BOTH league
# leaderboards: the "Pitches" board is that table directly, and the
# "Pitchers" board is a roll-up of it in R. Grouping once and rolling
# up is what keeps this viable — the grouped table is a few tens of
# thousands of rows, where collecting the league's raw pitches would be
# millions.
#
# Everything Arrow computes is a COUNT or a MEAN. All rates, ratios and
# run values are derived in R afterwards, because arithmetic on booleans
# inside an Arrow query is the part most likely to differ across arrow
# versions. Counts and means are the stable subset.
# ============================================================

# ============================================================
# LEAGUE LEADERBOARD ENGINE — TruMedia API
# ------------------------------------------------------------
# Same source of truth as the rest of the app: TruMedia, the shared team
# name mapping (tm_find_team_id), and the Pitch Profiler bundle for
# expected stats and the plus models. No parquet anywhere in this path.
#
# Two tiers, because they cost wildly different amounts:
#
#   TIER 1 — season lines (cheap, league wide).
#     One PlayerTotals call per team returns every pitcher on that staff
#     with W/L/SV/G/IP/ERA and the rate stats. ~1-2 calls per team covers
#     the whole country, so this tier populates the board for every team
#     in the season.
#
#   TIER 2 — pitch level (expensive, opt-in).
#     Release point, arm angle, velo/spin, Stuff+/Pitching+/Location+ and
#     xBA/xSLG/xwOBA all require the actual pitches, which is a
#     GamePitchesTrackman pull per team plus a model scoring pass. That
#     is minutes per team, so it runs only for teams the user has
#     selected, and the board says so.
#
# Everything is cached per (season, team) so re-filtering is free.
# ============================================================

LB_SWING_CALLS <- c("StrikeSwinging", "FoulBall", "FoulBallNotFieldable",
                    "FoulBallFieldable", "InPlay")
LB_BALL_CALLS  <- c("BallCalled", "BallinDirt", "BallIntentional",
                    "AutomaticBall")
LB_FB_TYPES    <- c("Fastball", "Four-Seam", "FourSeamFastBall", "Sinker",
                    "TwoSeamFastBall", "Two-Seam", "OneSeamFastBall")

LB_ZONE_SIDE <- 0.83
LB_ZONE_LO   <- 1.5
LB_ZONE_HI   <- 3.5

.lb_env <- new.env(parent = emptyenv())
.lb_env$line  <- list()   # season|team -> season-line frame
.lb_env$pitch <- list()   # season|team -> scored pitch frame

# ---- Disk cache ---------------------------------------------------------
# The in-memory cache dies with the process, so every container restart
# re-pulls the whole league. These frames are small (a few thousand rows)
# and a completed season never changes, so they persist to disk with a TTL:
# the current season goes stale after a day, past seasons effectively never.
LB_CACHE_DIR <- file.path(tempdir(), "lb_cache")
LB_TTL_HOURS <- 24

.lb_cache_path <- function(ck)
  file.path(LB_CACHE_DIR, paste0(gsub("[^A-Za-z0-9]+", "_", ck), ".rds"))

.lb_disk_get <- function(ck, season) {
  p <- .lb_cache_path(ck)
  if (!file.exists(p)) return(NULL)
  stale <- suppressWarnings(as.integer(season)) < TM_SEASON &&
    difftime(Sys.time(), file.mtime(p), units = "days") < 3650
  if (!stale &&
      difftime(Sys.time(), file.mtime(p), units = "hours") > LB_TTL_HOURS)
    return(NULL)
  tryCatch(readRDS(p), error = function(e) NULL)
}

.lb_disk_put <- function(ck, obj) {
  if (is.null(obj) || !is.data.frame(obj) || nrow(obj) == 0) return(invisible())
  tryCatch({
    dir.create(LB_CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
    saveRDS(obj, .lb_cache_path(ck))
  }, error = function(e) invisible())
}

# Read-through cache: memory -> disk -> build.
.lb_cached <- function(ck, season, build) {
  if (!is.null(.lb_env$line[[ck]])) return(.lb_env$line[[ck]])
  d <- .lb_disk_get(ck, season)
  if (!is.null(d)) {
    cat("[LB] disk cache hit:", ck, "(", nrow(d), "rows )\n")
    .lb_env$line[[ck]] <- d
    return(d)
  }
  out <- build()
  out <- if (is.null(out)) data.frame() else out
  .lb_env$line[[ck]] <- out
  .lb_disk_put(ck, out)
  out
}

# Pitcher rate/count columns. Rates that TruMedia defines as ratios of
# counts (Whiff%, Chase%, Zone%) are NOT atomic columns — asking for them
# 500s the backend, exactly as documented for the batter tokens above.
# So counts are pulled and the rates computed in R.
# A guessed column set 500s the whole request and takes the good tokens
# down with it, so each token is probed ALONE once per session and only
# the survivors are assembled into the real pull. Same approach the
# batter grades use, for the same reason: TruMedia's glossary spelling
# varies by site and there is no way to know it up front.
LB_TM_TOKENS <- list(
  pa    = c("PA|PIT", "PA"),
  # P|PIT is the accepted spelling on this site; Pitches|PIT and
  # Pitch|PIT both 500. Winner first so the probe costs one call.
  pit   = c("P|PIT", "Pitches|PIT", "Pitch|PIT", "Pitches"),
  swing = c("Swing#|PIT", "Swings#|PIT", "Swing#"),
  miss  = c("Miss#|PIT", "Whiff#|PIT", "SwStr#|PIT", "Miss#"),
  chase = c("Chase#|PIT", "OSwing#|PIT", "Chase#"),
  ooz   = c("OutOfZone#|PIT", "OutofZone#|PIT", "OOZ#|PIT", "OutOfZone#"),
  k     = c("K|PIT", "K#|PIT", "SO|PIT"),
  bb    = c("BB|PIT", "BB#|PIT"),
  hbp   = c("HBP|PIT", "HBP"),
  h     = c("H|PIT", "H"),
  d2    = c("2B|PIT", "2B"),
  d3    = c("3B|PIT", "3B"),
  hr    = c("HR|PIT", "HR")
)

# Shape/stuff columns. If TruMedia exposes these as aggregates, the whole
# raw-pitch tier becomes unnecessary for velo, spin, release and arm angle
# — one PlayerTotals call replaces a full season of GamePitchesTrackman
# pulls. Probed exactly like the count tokens, and simply absent if the
# site does not carry them.

# Probe against a team we know returns rows. Cached for the session.
.lb_probe_tokens <- function(tid = NULL) {
  if (!is.null(.tm_env$lb_tok)) return(.tm_env$lb_tok)
  ok_one <- function(tok) {
    p <- list(seasonYear = tm_season(), format = "RAW",
              columns = paste0("[", tok, "]"))
    if (!is.null(tid)) p$teamId <- tid
    out <- tm_get_csv("PlayerTotals", params = p)
    !is.null(out) && nrow(out) > 0 && !is.null(.lb_col(out, sub("\\|.*$", "", tok)))
  }
  pick <- function(cands) { for (t in cands) if (ok_one(t)) return(t); NULL }
  # Shape tokens are deliberately NOT probed. TruMedia's PlayerTotals
  # returns HTTP 500 for every spelling of velo / spin / release /
  # extension, so the old probe burned ~17 failing requests on every cold
  # start and still came back empty. Velo, spin, release and the plus
  # models now come from the pitch-level pool + score_promodel instead --
  # the same chain the Overview pitch-metrics table uses.
  res <- lapply(LB_TM_TOKENS, pick)
  got <- names(res)[!vapply(res, is.null, logical(1))]
  cat("[LB] TruMedia pitcher tokens accepted:",
      if (length(got)) paste(unlist(res[got]), collapse = ", ") else "(none)", "\n")
  miss <- setdiff(names(LB_TM_TOKENS), got)
  if (length(miss))
    cat("[LB] no accepted spelling for:", paste(miss, collapse = ", "),
        "- those columns will be blank\n")
  cat("[LB] velo/spin/release/Stuff+ come from the pitch-level pool",
      "(PlayerTotals cannot serve them)\n")
  .tm_env$lb_tok <- res
  res
}

.lb_rate_cols <- function(tok) {
  v <- unlist(tok[!vapply(tok, is.null, logical(1))], use.names = FALSE)
  if (!length(v)) return(NULL)
  paste0("[", v, "]", collapse = ",")
}

.lb_canon <- function(x) gsub("[^a-z0-9]", "", tolower(x))

# Per-pitch run value uses the same DRE weights the matchup engine prices
# on, so RV/100 on the leaderboard is the same currency as the matrix.
# Pitcher perspective: negative favours the hitter.
LB_DRE <- c(Ball = -0.09238320509256304, `Called Strike` = 0.12344088136926964,
            Whiff = 0.18879404070261246, `Foul Ball` = 0.06953559805880699,
            HBP = -0.41813357918459340, `Field Out` = 0.36137424728548606,
            Single = -0.54898855048196560, Double = -0.85276925535314780,
            Triple = -1.10169781931464180, `Home Run` = -1.38846328538985620)

.lb_pitch_rv <- function(pc, pr) {
  pc <- as.character(pc); pr <- as.character(pr)
  ev <- rep(NA_character_, length(pc))
  ev[pc %in% LB_BALL_CALLS] <- "Ball"
  ev[pc == "StrikeCalled"]  <- "Called Strike"
  ev[pc == "StrikeSwinging"] <- "Whiff"
  ev[pc %in% c("FoulBall","FoulBallNotFieldable","FoulBallFieldable")] <- "Foul Ball"
  ev[pc == "HitByPitch"] <- "HBP"
  ip <- pc == "InPlay"
  ev[ip & pr == "Single"] <- "Single"; ev[ip & pr == "Double"] <- "Double"
  ev[ip & pr == "Triple"] <- "Triple"
  ev[ip & pr %in% c("HomeRun","Homerun")] <- "Home Run"
  ev[ip & pr %in% c("Out","FieldersChoice","Sacrifice","Error")] <- "Field Out"
  unname(LB_DRE[ev])
}

# Value of a stat column from a PlayerTotals frame, matched loosely so
# "K#|PIT", "K#", "k_pit" and "K" all resolve to the same request.
.lb_col <- function(df, want) {
  if (is.null(df)) return(NULL)
  cn <- .lb_canon(names(df))
  w  <- .lb_canon(want)
  hit <- which(cn == w)
  if (!length(hit)) hit <- which(cn == paste0(w, "pit"))
  if (!length(hit)) return(NULL)
  suppressWarnings(as.numeric(df[[hit[1]]]))
}

.lb_chr <- function(df, pats) {
  cc <- .tm_pick_col(df, pats)
  if (is.null(cc)) return(NULL)
  as.character(df[[cc]])
}

# ---- TIER 1: one team's season line -------------------------------------
lb_tm_team_line <- function(team_disp, season) {
  ck <- paste(season, team_disp, sep = "|")
  .lb_cached(ck, season, function() tm_with_season(season, {
    tid <- tryCatch(tm_find_team_id(team_disp), error = function(e) NULL)
    if (is.null(tid)) return(NULL)
    trad  <- tm_team_player_totals(tid, "overall", TM_TRAD_COLS)
    if (is.null(trad) || nrow(trad) == 0) return(NULL)
    tok   <- .lb_probe_tokens(tid)
    rcols <- .lb_rate_cols(tok)
    rates <- if (is.null(rcols)) NULL else
      tm_team_player_totals(tid, "overall", rcols)

    nm <- .lb_chr(trad, c("^player$", "playerName", "^name$", "fullName"))
    if (is.null(nm)) return(NULL)
    d <- data.frame(Pitcher = nm, School = team_disp,
                    stringsAsFactors = FALSE)
    grab <- function(src, want) {
      v <- .lb_col(src, want)
      if (is.null(v)) rep(NA_real_, nrow(d)) else v
    }
    d$T   <- .lb_hand_from_payload(trad, nrow(d))
    d$W   <- grab(trad, "W");   d$L  <- grab(trad, "L")
    d$S   <- grab(trad, "SV");  d$G  <- grab(trad, "G")
    d$IP  <- tm_ip_decimal(grab(trad, "IP"))
    d$ERA <- round(grab(trad, "ERA"), 2)

    d <- .lb_join_rates(d, rates, tok)
    d[!is.na(d$Pitcher) & nzchar(d$Pitcher), , drop = FALSE]
  }))
}

# Attach the count-derived rates to a season-line frame, aligning on
# player name. Every column is created even when a token was rejected,
# so downstream code never has to test for their existence.
.lb_join_rates <- function(d, rates, tok) {
    if (!is.null(rates) && nrow(rates) > 0) {
      rn <- .lb_chr(rates, c("^player$", "playerName", "^name$", "fullName"))
      i  <- if (is.null(rn)) rep(NA_integer_, nrow(d)) else
              match(.tm_norm(d$Pitcher), .tm_norm(rn))
      pick <- function(slot) {
        t <- tok[[slot]]
        if (is.null(t)) return(rep(NA_real_, nrow(d)))
        v <- .lb_col(rates, sub("\\|.*$", "", t))
        if (is.null(v)) rep(NA_real_, nrow(d)) else v[i]
      }
      pa  <- pick("pa");    pit  <- pick("pit")
      sw  <- pick("swing"); miss <- pick("miss")
      ch  <- pick("chase"); ooz  <- pick("ooz")
      kk  <- pick("k");     bbv  <- pick("bb")
      hbp <- pick("hbp");   hh   <- pick("h")
      d2  <- pick("d2");    d3   <- pick("d3"); hr <- pick("hr")
      d$PA  <- pa
      d$P   <- pit
      d$`K%`      <- .lb_rate(kk, pa)
      d$`BB%`     <- .lb_rate(bbv, pa)
      d$`Whiff%`  <- .lb_rate(miss, sw)
      d$`Chase%`  <- .lb_rate(ch, ooz)
      # Zone% = 1 - OutOfZone/Pitches; TruMedia has no atomic Zone#
      d$`Zone%`   <- .lb_rate(pmax(pit - ooz, 0), pit)
      h1 <- hh - d2 - d3 - hr
      d$wOBA <- round(ifelse(is.finite(pa) & pa > 0,
        (0.690 * bbv + 0.720 * hbp + 0.890 * h1 + 1.270 * d2 +
         1.620 * d3 + 2.100 * hr) / pa, NA_real_), 3)
      d$.hr <- hr; d$.bb <- bbv; d$.k <- kk; d$.hbp <- hbp
      # aggregate shape, where the site exposes it — this is what lets the
      # board show velo / spin / release without any pitch-level pull
      d$Velo <- round(pick("velo"), 1)
      d$Spin <- round(pick("spin"), 0)
      d$relh <- pick("relh")
      d$rels <- pick("rels")
    } else {
      for (cc in c("PA","P","K%","BB%","Whiff%","Chase%","Zone%","wOBA",
                   "Velo","Spin","relh","rels"))
        d[[cc]] <- NA_real_
      d$.hr <- NA_real_; d$.bb <- NA_real_; d$.k <- NA_real_; d$.hbp <- NA_real_
    }
    d
}

.lb_rate <- function(num, den, mult = 100, digits = 1) {
  v <- ifelse(is.finite(den) & den > 0 & is.finite(num), mult * num / den, NA_real_)
  round(v, digits)
}

# ---- TIER 1b: the whole league in one call ------------------------------
# PlayerTotals without a team parameter returns every qualified player in
# the season. That is ONE request for the entire country, which is what
# makes opening the tab cheap enough to auto-build. Sorting and trimming
# happen in R rather than via a guessed sort/limit parameter — there is
# nothing to 500 that way, and the whole league is a few thousand rows.
#
# Falls back to the per-team path when the endpoint refuses an untargeted
# request, so nothing is lost if the site requires a team.
lb_tm_league_line <- function(season) {
  ck <- paste0(season, "|<league>")
  # Returns the FULL league frame, always. Truncation used to happen here,
  # which quietly meant every downstream filter was searching an already
  # clipped top-N instead of the league. Paging is now the board's job.
  d <- .lb_cached(ck, season, function() tm_with_season(season, {
    pull <- function(cols) {
      p <- list(seasonYear = tm_season(), format = "RAW")
      if (!is.null(cols)) p$columns <- cols
      tm_get_csv("PlayerTotals", params = p)
    }
    trad <- pull(TM_TRAD_COLS)
    if (is.null(trad) || nrow(trad) == 0) trad <- pull(TM_TRAD_COLS_MIN)
    if (is.null(trad) || nrow(trad) == 0) return(NULL)

    nm <- .lb_chr(trad, c("^player$", "playerName", "^name$", "fullName"))
    if (is.null(nm)) return(NULL)
    tmn <- .lb_chr(trad, c("teamName", "teamLocation", "^team$", "teamAbbrev"))

    d <- data.frame(Pitcher = nm,
                    School = tmn %||% NA_character_,
                    stringsAsFactors = FALSE)
    grab <- function(w) { v <- .lb_col(trad, w)
                          if (is.null(v)) rep(NA_real_, nrow(d)) else v }
    d$W <- grab("W"); d$L <- grab("L"); d$S <- grab("SV")
    d$G <- grab("G"); d$IP <- tm_ip_decimal(grab("IP"))
    d$ERA <- round(grab("ERA"), 2)
    d$T <- .lb_hand_from_payload(trad, nrow(d))
    # Pitchers only: a position player has no IP.
    d <- d[is.finite(d$IP) & d$IP > 0, , drop = FALSE]
    if (nrow(d) == 0) return(NULL)

    tok <- .lb_probe_tokens(NULL)
    rcols <- .lb_rate_cols(tok)
    rates <- if (is.null(rcols)) NULL else pull(rcols)
    d <- .lb_join_rates(d, rates, tok)

    d[order(-d$IP), , drop = FALSE]
  }))
  d
}

# ---- TIER 1c: the Pitches board, league wide, from filters --------------
# PlayerTotals accepts an event.pitchType filter (tm_pitch_side_row already
# relies on it). Applying it league wide gives the ENTIRE pitch-type board
# in one call per pitch type — about eight calls total, versus a full
# season of GamePitchesTrackman pulls for every team.
#
# Which is the whole efficiency story: a team's raw pitches are ~60 chunked
# requests plus a Python scoring pass, so six teams was several hundred
# requests and minutes of blocked app. This path is eight requests, cached,
# and covers every team at once.
LB_PITCH_TYPES <- c("Fastball", "Sinker", "Cutter", "Slider", "Sweeper",
                    "Curveball", "ChangeUp", "Splitter")

lb_tm_league_pitches <- function(season, progress = NULL) {
  ck <- paste0(season, "|<league-pitches>")
  .lb_cached(ck, season, function() tm_with_season(season, {
    tok <- .lb_probe_tokens(NULL)
    rcols <- .lb_rate_cols(tok)
    if (is.null(rcols)) return(NULL)
    acc <- list()
    for (i in seq_along(LB_PITCH_TYPES)) {
      pn <- LB_PITCH_TYPES[i]
      code <- unname(TM_PITCH_CODE[pn])
      if (is.na(code)) next
      if (is.function(progress))
        progress(i / length(LB_PITCH_TYPES), paste0("Pitch type: ", pn))
      rq <- paste0("&filters=(((event.pitchType%20%3D%20'", code, "')))")
      r <- tm_get_csv("PlayerTotals",
                      params = list(seasonYear = tm_season(), format = "RAW",
                                    columns = rcols),
                      raw_query = rq)
      if (is.null(r) || nrow(r) == 0) next
      nm <- .lb_chr(r, c("^player$", "playerName", "^name$", "fullName"))
      if (is.null(nm)) next
      d <- data.frame(Pitcher = nm,
                      School = .lb_chr(r, c("teamName", "teamLocation",
                                            "^team$", "teamAbbrev")) %||% NA_character_,
                      Pitch = pn, stringsAsFactors = FALSE)
      d$T <- .lb_hand_from_payload(r, nrow(d))
      d <- .lb_join_rates(d, r, tok)
      # a pitcher who doesn't throw the pitch comes back with no pitches
      d <- d[is.finite(d$P) & d$P > 0, , drop = FALSE]
      if (nrow(d)) acc[[length(acc) + 1]] <- d
    }
    if (!length(acc)) return(NULL)
    dplyr::bind_rows(acc)
  }))
}

# ---- TIER 2: one team's scored pitches ----------------------------------
# Reuses the app's own loader and the same model chain the Overview and
# Pitch Profiler pages use, so a number on the leaderboard reconciles
# with the number on the player page.
lb_tm_team_pitches <- function(team_disp, season) {
  ck <- paste(season, team_disp, sep = "|")
  if (!is.null(.lb_env$pitch[[ck]])) return(.lb_env$pitch[[ck]])
  out <- tm_with_season(season, {
    pool <- tryCatch(tm_load_team_pool(team_disp), error = function(e) NULL)
    if (is.null(pool) || nrow(pool) == 0) return(NULL)
    # keep only this team's pitchers (the pool carries both sides)
    if ("PitcherTeam" %in% names(pool)) {
      keep <- .tm_norm(as.character(pool$PitcherTeam)) == .tm_norm(team_disp)
      if (any(keep, na.rm = TRUE)) pool <- pool[which(keep), , drop = FALSE]
    }
    if (nrow(pool) == 0) return(NULL)
    # Stuff+/Pitching+/Location+ — same call the rest of the app makes
    pool <- tryCatch(score_promodel(pool, "ncaa_ind"), error = function(e) {
      cat("  [LB] proModel scoring failed for", team_disp, "-",
          conditionMessage(e), "\n"); pool })
    pool
  })
  .lb_env$pitch[[ck]] <- out %||% data.frame()
  .lb_env$pitch[[ck]]
}

# Pitch-level aggregate for one team: per pitcher x pitch type
.lb_pitch_agg <- function(pool, team_disp) {
  if (is.null(pool) || nrow(pool) == 0) return(NULL)
  gv <- function(cc) if (cc %in% names(pool)) pool[[cc]] else rep(NA, nrow(pool))
  pc <- as.character(gv("PitchCall"))
  ps <- suppressWarnings(as.numeric(gv("PlateLocSide")))
  ph <- suppressWarnings(as.numeric(gv("PlateLocHeight")))
  zone <- ps >= -LB_ZONE_SIDE & ps <= LB_ZONE_SIDE &
          ph >= LB_ZONE_LO   & ph <= LB_ZONE_HI
  d <- data.frame(
    Pitcher = as.character(gv("Pitcher")),
    Pitch   = as.character(gv("TaggedPitchType")),
    swing   = as.integer(pc %in% LB_SWING_CALLS),
    whiff   = as.integer(pc == "StrikeSwinging"),
    zone    = as.integer(!is.na(zone) & zone),
    ooz     = as.integer(!is.na(zone) & !zone),
    oswing  = as.integer(!is.na(zone) & !zone & pc %in% LB_SWING_CALLS),
    velo    = suppressWarnings(as.numeric(gv("RelSpeed"))),
    spin    = suppressWarnings(as.numeric(gv("SpinRate"))),
    relh    = suppressWarnings(as.numeric(gv("RelHeight"))),
    rels    = suppressWarnings(as.numeric(gv("RelSide"))),
    stuff   = suppressWarnings(as.numeric(gv("stuff_plus"))),
    pitchp  = suppressWarnings(as.numeric(gv("pitching_plus"))),
    locp    = suppressWarnings(as.numeric(gv("location_plus"))),
    rv      = .lb_pitch_rv(pc, gv("PlayResult")),
    thr     = toupper(substr(as.character(gv("PitcherThrows")), 1, 1)),
    stringsAsFactors = FALSE)
  d <- d[!is.na(d$Pitcher) & nzchar(d$Pitcher) & !is.na(d$Pitch), , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  m <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_
  out <- d %>%
    dplyr::group_by(Pitcher, Pitch) %>%
    dplyr::summarise(
      P = dplyr::n(),
      `Whiff%` = .lb_rate(sum(whiff, na.rm = TRUE), sum(swing, na.rm = TRUE)),
      `Chase%` = .lb_rate(sum(oswing, na.rm = TRUE), sum(ooz, na.rm = TRUE)),
      `Zone%`  = .lb_rate(sum(zone, na.rm = TRUE), dplyr::n()),
      Velo = round(m(velo), 1), Spin = round(m(spin), 0),
      relh = m(relh), rels = m(rels),
      RV = round(100 * m(rv), 2),
      `Stuff+` = round(m(stuff), 0), `Pitching+` = round(m(pitchp), 0),
      `Location+` = round(m(locp), 0),
      T = { h <- thr[thr %in% c("L","R")]
            if (length(h)) names(sort(table(h), decreasing = TRUE))[1]
            else NA_character_ },
      .groups = "drop") %>%
    as.data.frame()
  out$School <- team_disp
  out
}

# Roll pitch-level up to the arm: FB velo/spin and usage-weighted models
.lb_pitch_rollup <- function(pa) {
  if (is.null(pa) || nrow(pa) == 0) return(NULL)
  # The cheap filtered board has no model columns; the raw-pitch board
  # does. Fill the gaps so one rollup handles both shapes.
  for (cc in c("Stuff+", "Pitching+", "Location+", "RV", "relh", "rels",
               "Velo", "Spin"))
    if (!cc %in% names(pa)) pa[[cc]] <- NA_real_
  if (!"T" %in% names(pa)) pa$T <- NA_character_
  pa$.w   <- pa$P
  isfb    <- pa$Pitch %in% LB_FB_TYPES
  pa$.fbv <- ifelse(isfb, pa$Velo, NA_real_)
  pa$.fbs <- ifelse(isfb, pa$Spin, NA_real_)
  pa$.fbw <- ifelse(isfb, pa$P, 0)
  wm <- function(x, w) { ok <- is.finite(x) & is.finite(w) & w > 0
                         if (any(ok)) sum(x[ok] * w[ok]) / sum(w[ok]) else NA_real_ }
  pa %>%
    dplyr::group_by(Pitcher, School) %>%
    dplyr::summarise(
      `FB Velo` = round(wm(.fbv, .fbw), 1),
      `FB Spin` = round(wm(.fbs, .fbw), 0),
      relh = wm(relh, .w), rels = wm(rels, .w),
      `Stuff+`    = round(wm(`Stuff+`, .w), 0),
      `Pitching+` = round(wm(`Pitching+`, .w), 0),
      `Location+` = round(wm(`Location+`, .w), 0),
      RV = round(wm(RV, .w), 2),
      T = { h <- T[T %in% c("L","R")]
            if (length(h)) names(sort(table(h), decreasing = TRUE))[1]
            else NA_character_ },
      Arsenal = paste(Pitch[order(-.w)], collapse = ", "),
      .groups = "drop") %>%
    dplyr::mutate(`Rel Ht` = round(relh, 2), `Rel Sd` = round(rels, 2)) %>%
    as.data.frame()
}

# ---- Expected stats (xBA/xSLG/xwOBA) via the Pitch Profiler bundle ------
.lb_xstats <- function(pool) {
  if (is.null(pool) || nrow(pool) == 0) return(NULL)
  xs <- tryCatch(promodel_expected_stats(pool, group_cols = c("Pitcher")),
                 error = function(e) {
                   cat("  [LB] expected_stats failed -", conditionMessage(e), "\n")
                   NULL
                 })
  if (is.null(xs) || nrow(xs) == 0) return(NULL)
  cn <- .lb_canon(names(xs))
  nmc <- which(cn == "pitcher")
  if (!length(nmc)) return(NULL)
  get1 <- function(w) { i <- which(cn == .lb_canon(w))
                        if (length(i)) suppressWarnings(as.numeric(xs[[i[1]]]))
                        else rep(NA_real_, nrow(xs)) }
  data.frame(Pitcher = as.character(xs[[nmc[1]]]),
             xBA = round(get1("xBA"), 3), xSLG = round(get1("xSLG"), 3),
             xwOBA = round(get1("xwOBA"), 3), stringsAsFactors = FALSE)
}

# ---- Bio join: hand, class, age, logo -----------------------------------
# Same DRS26 roster file the rest of the app uses, so the hand colouring
# and the logo beside a name match the player pages.
.lb_attach_bio <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(d)
  n <- nrow(d)
  d$Logo <- NA_character_
  if (!"T" %in% names(d)) d$T <- NA_character_   # keep hand if already set
  d$Class <- NA_character_; d$Age <- NA_real_; d$.ht <- NA_real_
  if (!"School" %in% names(d)) d$School <- NA_character_
  if (is.null(drs_bio)) return(d)

  # Same resolution the rest of the app uses (drs_lookup): match on the
  # player key, and disambiguate shared keys with the SCHOOL. The old
  # code did a bare match() on name alone, which is why the startup log
  # reported ~14k of 28k arms unenriched -- every duplicate name across
  # two programs collapsed onto whichever row came first.
  bkey <- norm_player_key(d$Pitcher)
  tnorm <- function(x) gsub("[^a-z0-9]", "", tolower(as.character(x)))

  bio_team <- rep(NA_character_, nrow(drs_bio))
  for (tc in intersect(c("team_name", "newestTeamName",
                         "newestTeamAbbrevName", "newestTeamLocation"),
                       names(drs_bio))) {
    v <- tnorm(drs_bio[[tc]])
    bio_team[is.na(bio_team) & !is.na(v) & nzchar(v)] <-
      v[is.na(bio_team) & !is.na(v) & nzchar(v)]
  }

  # pass 1: key + school (exact pair)
  idx <- match(paste(bkey, tnorm(d$School)),
               paste(drs_bio$bio_key, bio_team))
  # pass 2: key alone, but ONLY where that key is unique in the file --
  # an ambiguous key with no school match stays unenriched rather than
  # silently borrowing another player's bio.
  uniq_key <- !(duplicated(drs_bio$bio_key) |
                duplicated(drs_bio$bio_key, fromLast = TRUE))
  solo <- match(bkey, ifelse(uniq_key, drs_bio$bio_key, NA_character_))
  idx[is.na(idx)] <- solo[is.na(idx)]

  pick <- function(cc) if (cc %in% names(drs_bio))
    as.character(drs_bio[[cc]])[idx] else rep(NA_character_, n)
  d$Logo  <- pick("team_logo")
  # Logo fallback: every row knows its School, so fill from the same
  # merged_teams file the rest of the app prettifies team names with.
  # A missed player-bio match should never cost a team crest.
  if (exists("team_name_map") && !is.null(team_name_map) &&
      "logo_url" %in% names(team_name_map)) {
    li <- match(tnorm(d$School), tnorm(team_name_map$team_name))
    fb <- as.character(team_name_map$logo_url)[li]
    d$Logo <- ifelse(is.na(d$Logo) | !nzchar(d$Logo), fb, d$Logo)
  }
  d$Class <- pick("Class")
  d$Age   <- suppressWarnings(as.numeric(pick("Age")))
  d$.ht   <- if ("height_in" %in% names(drs_bio))
    suppressWarnings(as.numeric(drs_bio$height_in))[idx] else NA_real_
  # DRS26 carries no throwing-hand column (the app infers a hitter's side
  # from pitch data for the same reason), so T is filled from the pitch
  # stream or the API payload instead — see .lb_hand_from_payload and the
  # PitcherThrows rollup. Anything the file happens to expose is a bonus.
  thr <- pick("Throws"); if (all(is.na(thr))) thr <- pick("T")
  if (!all(is.na(thr))) {
    h <- toupper(substr(ifelse(is.na(thr), "", thr), 1, 1))
    h[!h %in% c("L", "R")] <- NA_character_
    d$T <- h
  }
  d
}

# Throwing hand off any hand-ish column TruMedia returns on PlayerTotals.
.lb_hand_from_payload <- function(df, n) {
  cc <- .tm_pick_col(df, c("pitcherHand", "^throws$", "^hand$", "^T$",
                           "throwHand", "pitchHand"))
  if (is.null(cc)) return(rep(NA_character_, n))
  h <- toupper(substr(trimws(as.character(df[[cc]])), 1, 1))
  h[!h %in% c("L", "R")] <- NA_character_
  h
}

# ---- Safe enrichment join --------------------------------------------
# Pitcher names are NOT unique across a national dataset, so a plain
# left_join(by = "Pitcher") silently fans out: one row of season lines
# matches several rows of pitch-level data and vice versa, and an arm
# can end up wearing another same-named arm's Stuff+.
#
# Team can't be the whole answer either — the season line's school and
# the pitch pool's team string come from different TruMedia fields and
# do not always agree, so joining on both would drop real matches.
#
# So: match on name AND team where both sides agree, then allow a
# name-only match ONLY for names that appear exactly once on each side.
# Ambiguous names that never agreed on team are left unenriched rather
# than guessed at.
.lb_safe_enrich <- function(x, y, cols) {
  cols <- intersect(cols, names(y))
  if (!length(cols) || !nrow(x) || !nrow(y)) return(x)
  nx <- .tm_norm(x$Pitcher); ny <- .tm_norm(y$Pitcher)
  tx <- if ("School" %in% names(x)) .tm_norm(x$School) else rep(NA_character_, nrow(x))
  ty <- if ("School" %in% names(y)) .tm_norm(y$School) else rep(NA_character_, nrow(y))

  idx <- match(paste(nx, tx, sep = "|"), paste(ny, ty, sep = "|"))
  # name-only fallback, restricted to names unique on BOTH sides
  uniq_x <- !(nx %in% nx[duplicated(nx)])
  uniq_y <- !(ny %in% ny[duplicated(ny)])
  gap <- is.na(idx) & uniq_x
  if (any(gap)) {
    j <- match(nx[gap], ifelse(uniq_y, ny, NA_character_))
    idx[gap] <- j
  }
  for (cc in cols) x[[cc]] <- y[[cc]][idx]
  amb <- sum(is.na(idx))
  if (amb > 0)
    cat("[LB]", amb, "of", nrow(x),
        "arms left unenriched (no unambiguous name/team match)\n")
  x
}

.lb_arm_angle <- function(rel_h, rel_s, height_in) {
  h <- ifelse(is.finite(height_in), height_in, 73)
  adj <- pmax(12 * rel_h - 0.70 * h, 1e-6)
  opp <- abs(12 * rel_s)
  round(pmin(pmax(90 - atan2(opp, adj) * 180 / pi, 0), 90), 1)
}

# FIP constant solved so league FIP recenters on league RA/9
.lb_add_fip_rv <- function(d) {
  ip <- suppressWarnings(as.numeric(d$IP))
  hr <- d$.hr; bb <- d$.bb; k <- d$.k; hbp <- d$.hbp
  tip <- sum(ip[is.finite(ip)], na.rm = TRUE)
  fip_c <- 3.10
  if (is.finite(tip) && tip > 0) {
    era <- suppressWarnings(as.numeric(d$ERA))
    lg <- sum(era * ip, na.rm = TRUE) / tip
    raw <- (13 * sum(hr, na.rm = TRUE) + 3 * sum(bb + hbp, na.rm = TRUE) -
            2 * sum(k, na.rm = TRUE)) / tip
    if (is.finite(lg) && is.finite(raw)) fip_c <- lg - raw
  }
  d$FIP <- round(ifelse(is.finite(ip) & ip > 0,
    (13 * hr + 3 * (bb + hbp) - 2 * k) / ip + fip_c, NA_real_), 2)
  d
}

# ---- Public: build both boards for a season -----------------------------
# `teams` = display names to include. Tier 1 runs for all of them.
# `deep`  = subset of `teams` to also pull pitch level for.
# ---- Precomputed tables (scripts/build_leaderboards.R) -------------------
# lb/<season>/ in Storage: the two boards, the hitters table, the raw
# league-wide TruMedia totals and the team list. Loaded once per process.
.lb_env$pre <- list()
lb_precomputed <- function(season = TM_SEASON) {
  key <- as.character(season)
  if (!is.null(.lb_env$pre[[key]])) return(.lb_env$pre[[key]])
  if (!exists("storage_read_parquet", mode = "function") || !sb_storage_enabled()) {
    .lb_env$pre[[key]] <- list(); return(.lb_env$pre[[key]])
  }
  man <- tryCatch(storage_read_json(sprintf("lb/%s/manifest.json", key)), error = function(e) NULL)
  if (is.null(man)) { .lb_env$pre[[key]] <- list(); return(.lb_env$pre[[key]]) }
  t0 <- Sys.time()
  out <- list(manifest = man)
  for (nm in c("teams", "tm_pitching_totals", "tm_batting_totals", "pitchers", "pitches", "hitters"))
    out[[nm]] <- tryCatch(storage_read_parquet(sprintf("lb/%s/%s.parquet", key, nm)), error = function(e) NULL)
  cat(sprintf("[LB] precomputed %s tables from Storage (built %s) in %.0fs: %s\n", key, man$built_at %||% "?",
              as.numeric(difftime(Sys.time(), t0, units = "secs")),
              paste(names(out)[-1][!vapply(out[-1], is.null, logical(1))], collapse = ", ")))
  .lb_env$pre[[key]] <- out
  out
}

# teamIds matching a set of picker names (location / fullName / teamName / abbrev)
.lb_team_ids <- function(pre, picks) {
  tt <- pre$teams
  if (is.null(tt) || !length(picks)) return(character(0))
  pk <- .tm_norm(picks); ids <- character(0)
  for (nc in intersect(c("location", "fullName", "teamName", "abbrevName"), names(tt)))
    ids <- c(ids, as.character(tt$teamId)[.tm_norm(as.character(tt[[nc]])) %in% pk])
  unique(ids)
}

# ---- Public: build both boards for a season -----------------------------
# Precomputed tables when the season has them (every team, every column,
# instantly); otherwise the live TruMedia path below. `teams` filters.
lb_league_tables <- function(season = TM_SEASON, teams = NULL, deep = character(0),
                             progress = NULL, shape = TRUE) {
  pre <- tryCatch(lb_precomputed(season), error = function(e) list())
  if (is.data.frame(pre$pitchers) && nrow(pre$pitchers) > 0) {
    pitchers <- pre$pitchers; pitches <- pre$pitches %||% data.frame()
    if (length(teams)) {
      ids <- .lb_team_ids(pre, teams); pk <- .tm_norm(teams)
      keep <- function(d) (as.character(d$tm_team_id) %in% ids) | (.tm_norm(d$School) %in% pk)
      pitchers <- pitchers[keep(pitchers), , drop = FALSE]
      if (nrow(pitches)) pitches <- pitches[keep(pitches), , drop = FALSE]
    }
    return(list(pitchers = pitchers, pitches = pitches,
                note = paste0("Precomputed ", season, " tables (built ", pre$manifest$built_at %||% "?",
                              "): TruMedia season lines + TrackMan pitch data for every team.")))
  }
  .lb_league_tables_live(season, teams, deep, progress, shape)
}

.lb_league_tables_live <- function(season = TM_SEASON, teams = NULL, deep = character(0),
                                   progress = NULL, shape = TRUE) {
  # Picking teams now implies "score them too". Velo, spin, release height
  # and Stuff+/Pitching+/Location+ have no season-level source at all, so
  # requiring a second opt-in picker just meant those columns were blank
  # for everyone who didn't know to go find it.
  if (isTRUE(shape) && length(teams))
    deep <- union(as.character(deep), as.character(teams))
  if (!tm_enabled())
    return(list(pitchers = data.frame(), pitches = data.frame(),
                note = "TruMedia credentials not configured (TM_* secrets)."))

  if (is.null(teams) || length(teams) == 0) {
    if (is.function(progress)) progress(0.3, "League season lines")
    pitchers <- lb_tm_league_line(season)
    if (is.null(pitchers) || nrow(pitchers) == 0)
      return(list(pitchers = data.frame(), pitches = data.frame(),
                  note = paste0("TruMedia returned no ", season,
                                " season lines. Try a different season, or pick ",
                                "teams to pull them one staff at a time.")))
  } else {
    lines <- list()
    for (i in seq_along(teams)) {
      if (is.function(progress)) progress(i / length(teams),
        paste0("Season line: ", teams[i]))
      l <- lb_tm_team_line(teams[i], season)
      if (!is.null(l) && nrow(l) > 0) lines[[length(lines) + 1]] <- l
    }
    if (length(lines) == 0)
      return(list(pitchers = data.frame(), pitches = data.frame(),
                  note = paste0("No TruMedia season lines returned for ", season, ".")))
    pitchers <- dplyr::bind_rows(lines)
  }

  # Pitches board, league wide and cheap. Filtered PlayerTotals gives the
  # whole thing; the raw-pitch tier below only ADDS the model columns.
  pitches <- NULL
  if (is.null(teams) || length(teams) == 0) {
    lp <- lb_tm_league_pitches(season, progress)
    if (!is.null(lp) && nrow(lp) > 0) {
      pitches <- lp
      # fastball-family velo/spin for the arm board, from the same pull
      ru <- .lb_pitch_rollup(pitches)
      if (!is.null(ru))
        pitchers <- .lb_safe_enrich(pitchers, ru,
          c("FB Velo", "FB Spin", "Arsenal"))
    }
  }

  # Tier 2: model columns for the selected teams only
  if (length(deep)) {
    aggs <- list(); xss <- list()
    for (i in seq_along(deep)) {
      if (is.function(progress)) progress(i / length(deep),
        paste0("Pitch level: ", deep[i]))
      pool <- lb_tm_team_pitches(deep[i], season)
      if (is.null(pool) || nrow(pool) == 0) next
      a <- .lb_pitch_agg(pool, deep[i])
      if (!is.null(a)) aggs[[length(aggs) + 1]] <- a
      x <- .lb_xstats(pool)
      if (!is.null(x)) xss[[length(xss) + 1]] <- x
    }
    if (length(aggs)) {
      pagg <- dplyr::bind_rows(aggs)
      # merge model columns onto the cheap board rather than replacing it,
      # so the deep teams gain Stuff+/RV without the rest losing their rows
      if (is.null(pitches) || nrow(pitches) == 0) pitches <- pagg
      else {
        key <- function(d) paste(.tm_norm(d$Pitcher), d$Pitch, sep = "|")
        i <- match(key(pitches), key(pagg))
        for (cc in c("Stuff+","Pitching+","Location+","RV","relh","rels")) {
          if (!cc %in% names(pagg)) next
          if (!cc %in% names(pitches)) pitches[[cc]] <- NA_real_
          pitches[[cc]] <- ifelse(is.na(i), pitches[[cc]], pagg[[cc]][i])
        }
        # rows for deep teams that the filtered pull missed entirely
        extra <- pagg[!key(pagg) %in% key(pitches), , drop = FALSE]
        if (nrow(extra)) pitches <- dplyr::bind_rows(pitches, extra)
      }
      ru <- .lb_pitch_rollup(pagg)
      if (!is.null(ru))
        pitchers <- .lb_safe_enrich(pitchers, ru,
          c("Stuff+","Pitching+","Location+","FB Velo","FB Spin",
            "RV","Rel Ht","Rel Sd","relh","rels","Arsenal","T"))
    }
    if (length(xss)) {
      xa <- dplyr::bind_rows(xss)
      pitchers <- .lb_safe_enrich(pitchers, xa, c("xBA","xSLG","xwOBA"))
    }
  }

  pitchers <- .lb_attach_bio(pitchers)
  pitchers <- .lb_add_fip_rv(pitchers)
  pitchers$`Arm Ang` <- .lb_arm_angle(pitchers$relh %||% NA_real_,
                                      pitchers$rels %||% NA_real_, pitchers$.ht)
  if (!is.null(pitches) && nrow(pitches) > 0) {
    pitches <- .lb_attach_bio(pitches)
    pitches$`Arm Ang` <- .lb_arm_angle(pitches$relh, pitches$rels, pitches$.ht)
    pitches$`Rel Ht`  <- round(pitches$relh, 2)
    pitches$`Rel Sd`  <- round(pitches$rels, 2)
    car <- intersect(c("W","L","S","G","IP","K%","BB%","wOBA","ERA","FIP",
                       "xBA","xSLG","xwOBA","Age","Class","T","Logo"),
                     names(pitchers))
    pitches <- .lb_safe_enrich(pitches, pitchers[, c("Pitcher", "School", car)], car)
  } else {
    pitches <- data.frame()
  }

  for (lvl in c("pitchers", "pitches")) {
    d <- if (lvl == "pitchers") pitchers else pitches
    if (nrow(d) == 0) next
    for (cc in lb_cols_for(lvl)) if (!cc %in% names(d)) d[[cc]] <- NA
    if (lvl == "pitchers") pitchers <- d else pitches <- d
  }

  note <- if (!length(deep))
    paste0("Season-level TruMedia only. Velo, spin, release and ",
           "Stuff+ / Pitching+ / Location+ are blank because TruMedia has ",
           "no season-level source for them \u2014 pick one or more teams and ",
           "leave the pitch-level box ticked to score them.")
  else NULL
  list(pitchers = pitchers, pitches = pitches, note = note)
}

LB_SPEC <- list(
  list(col = "Pitcher",   lab = "Pitcher",   dir =  0, dig = NA, grp = "id",    w = 200),
  list(col = "School",    lab = "School",    dir =  0, dig = NA, grp = "id",    w = 150),
  list(col = "T",         lab = "T",         dir =  0, dig = NA, grp = "id",    w = 34),
  list(col = "Class",     lab = "Yr",        dir =  0, dig = NA, grp = "id",    w = 46),
  list(col = "Age",       lab = "Age",       dir =  0, dig = 1,  grp = "id",    w = 50),
  list(col = "W",         lab = "W",         dir = +1, dig = 0,  grp = "count", w = 40),
  list(col = "L",         lab = "L",         dir = -1, dig = 0,  grp = "count", w = 40),
  list(col = "S",         lab = "S",         dir = +1, dig = 0,  grp = "count", w = 40),
  list(col = "G",         lab = "G",         dir =  0, dig = 0,  grp = "count", w = 42),
  list(col = "IP",        lab = "IP",        dir =  0, dig = 1,  grp = "count", w = 52),
  list(col = "P",         lab = "Pit",       dir =  0, dig = 0,  grp = "count", w = 50),
  list(col = "PA",        lab = "PA",        dir =  0, dig = 0,  grp = "count", w = 46),
  list(col = "K%",        lab = "K%",        dir = +1, dig = 1,  grp = "rate",  w = 56),
  list(col = "BB%",       lab = "BB%",       dir = -1, dig = 1,  grp = "rate",  w = 56),
  list(col = "Whiff%",    lab = "Whiff%",    dir = +1, dig = 1,  grp = "rate",  w = 62),
  list(col = "Chase%",    lab = "Chase%",    dir = +1, dig = 1,  grp = "rate",  w = 62),
  list(col = "Zone%",     lab = "Zone%",     dir = +1, dig = 1,  grp = "rate",  w = 58),
  list(col = "wOBA",      lab = "wOBA",      dir = -1, dig = 3,  grp = "res",   w = 58),
  list(col = "ERA",       lab = "RA/9",      dir = -1, dig = 2,  grp = "res",   w = 56),
  list(col = "FIP",       lab = "FIP",       dir = -1, dig = 2,  grp = "res",   w = 56),
  list(col = "RV",        lab = "RV/100",    dir = -1, dig = 2,  grp = "res",   w = 62),
  list(col = "xBA",       lab = "xBA",       dir = -1, dig = 3,  grp = "xstat", w = 56),
  list(col = "xSLG",      lab = "xSLG",      dir = -1, dig = 3,  grp = "xstat", w = 58),
  list(col = "xwOBA",     lab = "xwOBA",     dir = -1, dig = 3,  grp = "xstat", w = 62),
  list(col = "Rel Ht",    lab = "Rel Ht",    dir =  0, dig = 2,  grp = "shape", w = 58),
  list(col = "Rel Sd",    lab = "Rel Sd",    dir =  0, dig = 2,  grp = "shape", w = 58),
  list(col = "Arm Ang",   lab = "Arm Ang",   dir =  0, dig = 1,  grp = "shape", w = 62),
  list(col = "FB Velo",   lab = "FB Velo",   dir = +1, dig = 1,  grp = "stuff", w = 62),
  list(col = "FB Spin",   lab = "FB Spin",   dir = +1, dig = 0,  grp = "stuff", w = 62),
  list(col = "Stuff+",    lab = "Stuff+",    dir = +1, dig = 0,  grp = "model", w = 58),
  list(col = "Pitching+", lab = "Pitching+", dir = +1, dig = 0,  grp = "model", w = 66),
  list(col = "Location+", lab = "Location+", dir = +1, dig = 0,  grp = "model", w = 66)
)

# The Pitches board is the same board split by pitch type: Pitch slots in
# after the identity block, and FB Velo/Spin become that pitch's own.
LB_SPEC_PITCHES <- local({
  s <- LB_SPEC
  s <- lapply(s, function(x) {
    if (identical(x$col, "FB Velo")) { x$col <- "Velo"; x$lab <- "Velo" }
    if (identical(x$col, "FB Spin")) { x$col <- "Spin"; x$lab <- "Spin" }
    x
  })
  # W/L/S are pitcher-of-record decisions and PA is a plate-appearance
  # count — none of them decompose by pitch type, so they're carried at
  # the arm level only and dropped from this spec.
  s <- Filter(function(x) !x$col %in% c("W", "L", "S", "PA"), s)
  i <- which(vapply(s, function(x) identical(x$col, "Age"), logical(1)))
  append(s, list(list(col = "Pitch", lab = "Pitch", dir = 0, dig = NA,
                      grp = "id", w = 90)), after = i)
})

lb_spec_for <- function(level)
  if (identical(level, "pitches")) LB_SPEC_PITCHES else LB_SPEC

lb_cols_for   <- function(level) vapply(lb_spec_for(level), `[[`, character(1), "col")

# Default selection: every column the spec defines EXCEPT ones that are
# entirely empty for this dataset (W/L/S with no decision source, model
# columns that need a pitch-level load). They stay in the picker so they can
# be switched on the moment a source appears — they just don't ship a
# column of dashes by default.
lb_default_cols <- function(d, level) {
  cols <- lb_cols_for(level)
  if (is.null(d) || nrow(d) == 0) return(cols)
  Filter(function(cc) {
    if (!cc %in% names(d)) return(FALSE)
    v <- d[[cc]]
    if (is.character(v)) any(!is.na(v) & nzchar(v)) else any(is.finite(suppressWarnings(as.numeric(v))))
  }, cols)
}
lb_spec_entry <- function(level, col) {
  s <- lb_spec_for(level)
  i <- which(vapply(s, function(x) identical(x$col, col), logical(1)))
  if (length(i)) s[[i[1]]] else NULL
}

# ---- Player cell: team logo + hand-coloured name ------------------------
# RHP black, LHP red -- matches how the staff reads a board at a glance.
LB_HAND_COLOR <- c(L = "#C0392B", R = "#111827")

lb_player_cell <- function(name, logo, hand, id = NULL) {
  if (is.null(id)) id <- rep(NA_character_, length(name))
  id <- ifelse(is.na(id), "", as.character(id))
  col <- unname(LB_HAND_COLOR[ifelse(is.na(hand), "", hand)])
  col[is.na(col)] <- "#33474B"
  img <- ifelse(!is.na(logo) & nzchar(logo),
                paste0("<img class='lb-logo' src='", logo, "' loading='lazy' />"),
                "<span class='lb-logo lb-logo-blank'></span>")
  paste0("<span class='lb-player'>", img,
         # attribute MUST be data-name: that is what the global
         # .tp-player-link click handler reads to route to the profile
         "<a href='#' class='tp-player-link lb-name' data-side='pitcher' ",
         "data-id=\"", htmltools::htmlEscape(id, attribute = TRUE), "\" ",
         "data-name=\"", htmltools::htmlEscape(name, attribute = TRUE),
         "\" style='color:", col, "'>",
         htmltools::htmlEscape(name), "</a></span>")
}

# ---- Green - white - red gradient ---------------------------------------
# Percentile-based rather than fixed cutpoints, so every column self-scales
# to whatever population is on screen after filtering. Ties and degenerate
# columns (all one value) fall back to plain white instead of a hard edge.
LB_GRAD_LO <- "#E1463E"   # bad
LB_GRAD_MID <- "#FFFFFF"
LB_GRAD_HI <- "#1E9E52"   # good

lb_gradient_style <- function(values, dir, n = 40) {
  v <- suppressWarnings(as.numeric(values))
  ok <- is.finite(v)
  if (!any(ok) || dir == 0) return(NULL)
  rng <- stats::quantile(v[ok], c(0.05, 0.95), na.rm = TRUE, names = FALSE)
  if (!all(is.finite(rng)) || diff(rng) <= 0) return(NULL)
  brks <- seq(rng[1], rng[2], length.out = n)
  pal  <- grDevices::colorRampPalette(
    if (dir > 0) c(LB_GRAD_LO, LB_GRAD_MID, LB_GRAD_HI)
    else         c(LB_GRAD_HI, LB_GRAD_MID, LB_GRAD_LO))(n + 1)
  list(brks = brks, cols = pal)
}

# ---- Build the DT --------------------------------------------------------
lb_render_table <- function(d, level, show_cols, rank_offset = 0) {
  spec  <- lb_spec_for(level)
  cols  <- intersect(show_cols, names(d))
  if (length(cols) == 0) cols <- intersect(lb_cols_for(level), names(d))
  keep  <- d[, unique(c("Pitcher", cols)), drop = FALSE]

  # player cell replaces the plain name
  if ("Pitcher" %in% cols) {
    keep$Pitcher <- lb_player_cell(d$Pitcher, d$Logo,
                                   if ("T" %in% names(d)) d$T else NA_character_,
                                   if ("tm_id" %in% names(d)) d$tm_id else NULL)
  } else {
    keep$Pitcher <- NULL
  }
  cols <- intersect(cols, names(keep))
  keep <- keep[, cols, drop = FALSE]

  labs <- vapply(cols, function(cc) {
    e <- lb_spec_entry(level, cc); if (is.null(e)) cc else e$lab
  }, character(1))

  # Rank is the row's place in the FULL sorted board, not on this page,
  # so #51 on page 2 really is the 51st arm.
  if (nrow(keep) > 0) {
    keep <- cbind(`#` = as.integer(rank_offset) + seq_len(nrow(keep)),
                  keep, stringsAsFactors = FALSE)
    cols <- c("#", cols)
    labs <- c(`#` = "#", labs)
  }

  dt <- DT::datatable(
    keep, escape = FALSE, rownames = FALSE, colnames = unname(labs),
    class = "lb-table stripe hover row-border",
    extensions = c("Buttons", "FixedColumns"),
    options = list(
      # No DT paging: the server already handed us exactly one page, which
      # is the whole point -- a 3,000-row payload never reaches the browser.
      dom = "Brt",
      paging = FALSE,
      buttons = list(list(extend = "csv", text = "Export Page CSV",
                          className = "lb-btn")),
      scrollX = TRUE, scrollY = "62vh", scrollCollapse = TRUE,
      fixedColumns = list(leftColumns = 1),
      order = list(),
      autoWidth = FALSE,
      columnDefs = list(
        list(className = "dt-center",
             targets = which(cols %in% setdiff(cols, c("Pitcher", "School"))) - 1)
      )
    )
  )

  for (cc in cols) {
    e <- lb_spec_entry(level, cc)
    if (is.null(e)) next
    if (!is.na(e$dig) && is.numeric(keep[[cc]]))
      dt <- DT::formatRound(dt, cc, digits = e$dig)
    g <- lb_gradient_style(keep[[cc]], e$dir)
    if (!is.null(g))
      dt <- DT::formatStyle(dt, cc,
        backgroundColor = DT::styleInterval(g$brks, g$cols))
  }
  dt
}

# ============================================================
# LEAGUE LEADERBOARD — filter bar + tab panels
# ============================================================

LB_CSS <- HTML("
  .lb-wrap { padding: 4px 10px 20px; }
  .lb-head { display:flex; align-items:baseline; gap:12px; margin: 6px 0 12px; }
  .lb-head h3 { margin:0; color:#006F71; font-weight:800; letter-spacing:-.01em; }
  .lb-head .lb-sub { color:#6B7280; font-size:13px; }

  .lb-filters {
    background:#fff; border:1px solid #E8ECEE; border-radius:12px;
    padding:12px 14px 4px; margin-bottom:14px;
    box-shadow:0 1px 2px rgba(16,24,40,.04);
  }
  .lb-filters .form-group { margin-bottom:10px; }
  .lb-filters label { font-size:11.5px; font-weight:700; color:#6B7280;
                      text-transform:uppercase; letter-spacing:.05em; }

  .lb-rangebox { border-top:1px dashed #E8ECEE; margin-top:4px; padding-top:10px; }
  .lb-range-item { background:#F7F9FA; border:1px solid #EDF1F2; border-radius:9px;
                   padding:6px 12px 0; margin-bottom:8px; }
  .lb-range-item label { color:#33474B; }

  /* player cell: logo + hand-coloured name */
  .lb-player { display:flex; align-items:center; gap:8px; white-space:nowrap; }
  .lb-logo { width:20px; height:20px; object-fit:contain; flex:0 0 20px; }
  .lb-logo-blank { display:inline-block; border-radius:50%; background:#EDF1F2; }
  .lb-name { font-weight:700; text-decoration:none !important; }
  .lb-name:hover { text-decoration:underline !important; }

  /* table chrome */
  table.lb-table { font-size:12.5px; border-collapse:separate !important; }
  table.lb-table thead th {
    background:#0E6E70 !important; color:#fff !important;
    font-size:11px; font-weight:700; text-transform:uppercase;
    letter-spacing:.04em; border:none !important; white-space:nowrap;
    padding:9px 8px !important; position:relative;
  }
  table.lb-table tbody td { padding:5px 8px !important; border-top:1px solid #F1F4F5 !important; }
  table.lb-table tbody tr:hover td { box-shadow: inset 0 0 0 9999px rgba(0,111,113,.045); }
  .dataTables_wrapper .lb-btn {
    background:#006F71 !important; color:#fff !important; border:none !important;
    border-radius:7px !important; padding:6px 14px !important;
    font-size:12px !important; font-weight:700 !important;
  }
  .dataTables_scrollBody { border-bottom:1px solid #E8ECEE; }
  /* Pager sits above the board; the table itself scrolls internally, so
     keeping the controls up top means they never scroll out of reach. */
  .lb-pager { display:flex; align-items:center; justify-content:center;
              gap:14px; margin:6px 0 10px; }
  .lb-pager .lb-pgbtn { background:#FFFFFF; border:1px solid #CDE5E5;
              color:#006F71 !important; font-weight:700; font-size:13px;
              padding:5px 14px; border-radius:8px; }
  .lb-pager .lb-pgbtn:hover { background:#006F71; color:#fff !important;
              border-color:#006F71; }
  .lb-pageinfo { color:#4B5563; font-size:13px; font-weight:600;
              min-width:210px; text-align:center; display:inline-block;
              font-variant-numeric:tabular-nums; }
  .lb-empty { padding:40px; text-align:center; color:#6B7280; }
  .lb-note { background:#F0F7F7; border:1px solid #CDE5E5; color:#2A5F60;
             border-radius:9px; padding:9px 13px; margin-bottom:12px;
             font-size:12.5px; }
")

# Reusable filter bar. `level` is 'pitchers' or 'pitches' and is suffixed
# onto every inputId so the two boards keep independent filter state.
lb_filter_ui <- function(level) {
  id <- function(x) paste0("lb_", level, "_", x)
  spec <- lb_spec_for(level)
  all_cols <- vapply(spec, `[[`, character(1), "col")
  num_cols <- vapply(Filter(function(x) !is.na(x$dig), spec),
                     `[[`, character(1), "col")

  div(class = "lb-filters",
    fluidRow(
      column(2, selectInput(id("season"), "Season", choices = rev(TM_SEASONS_ALL),
                            selected = TM_SEASON)),
      # Picking a conference fills the team picker with that whole league.
      # Teams stay individually editable afterwards -- it is a shortcut,
      # not a lock.
      column(3, pickerInput(id("conf"), "Conference", choices = LB_CONFERENCES,
        multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE,
          `selected-text-format` = "count > 2",
          `none-selected-text` = "Any conference",
          `count-selected-text` = "{0} conferences"))),
      column(4, pickerInput(id("school"), "Team / School", choices = NULL,
        multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE,
          `selected-text-format` = "count > 2",
          `none-selected-text` = "Whole league"))),
      column(3, pickerInput(id("class"), "Class / Yr", choices = NULL,
        multiple = TRUE, options = list(`actions-box` = TRUE,
          `selected-text-format` = "count > 2", `none-selected-text` = "All classes")))
    ),
    fluidRow(
      column(2, pickerInput(id("hand"), "Throws", choices = c("L", "R"),
        selected = c("L", "R"), multiple = TRUE,
        options = list(`actions-box` = TRUE))),
      if (identical(level, "pitches"))
        column(2, pickerInput(id("ptype"), "Pitch Type", choices = NULL,
          multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE,
            `selected-text-format` = "count > 2", `none-selected-text` = "All pitches")))
      else
        column(2, sliderInput(id("age"), "Age", min = 17, max = 26,
                              value = c(17, 26), step = 0.5, sep = "")),
      column(8, div(style = "padding-top:30px;",
                    uiOutput(id("confnote"))))
    ),
    # Row 2 is all display-side: none of it re-hits the API, it just
    # reshapes and re-pages the board already in memory.
    fluidRow(
      column(3, textInput(id("search"), "Search name / school",
                          value = "", placeholder = "e.g. Smith, or Clemson")),
      column(3, selectInput(id("sortby"), "Sort by", choices = num_cols,
                            selected = if ("IP" %in% num_cols) "IP" else num_cols[1])),
      column(2, selectInput(id("sortdir"), "Order",
                            choices = c("High \u2192 Low" = "desc",
                                        "Low \u2192 High" = "asc"),
                            selected = "desc")),
      column(2, sliderInput(id("minip"), "Min IP", min = 0, max = 120,
                            value = 0, step = 1, sep = "")),
      column(2, numericInput(id("minp"), "Min Pitches",
                             value = 0, min = 0, step = 25)),
      column(2, selectInput(id("perpage"), "Rows per page",
                            choices = c("50" = 50, "100" = 100,
                                        "500" = 500, "1000" = 1000),
                            selected = 50))
    ),
    fluidRow(
      column(9, div(style = "padding-top:22px;",
        checkboxInput(id("shape"),
          HTML(paste0("<b>Include velo, spin, release and ",
                      "Stuff+ / Pitching+ / Location+</b> \u2014 scores every ",
                      "pitch for the selected teams, so it takes noticeably ",
                      "longer. Requires at least one team.")),
          value = TRUE))),
      column(3, div(style = "padding-top:26px;",
        actionButton(id("build"), "Build Board", class = "btn btn-block",
                     style = "background:#006F71;color:#fff;font-weight:700;border:none;")))
    ),
    fluidRow(
      column(6, pickerInput(id("cols"), "Columns Shown", choices = all_cols,
        selected = all_cols, multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE,
                       `selected-text-format` = "count > 3"))),
      column(6, pickerInput(id("rangecols"), "Add a Range Filter", choices = num_cols,
        selected = character(0), multiple = TRUE,
        options = list(`live-search` = TRUE, `none-selected-text` = "None",
                       `selected-text-format` = "count > 2")))
    ),
    uiOutput(id("ranges"))
  )
}

lb_board_ui <- function(level, title, subtitle) {
  id <- function(x) paste0("lb_", level, "_", x)
  div(class = "lb-wrap",
    div(class = "lb-head", h3(title), span(class = "lb-sub", subtitle)),
    lb_filter_ui(level),
    uiOutput(id("status")),
    div(class = "lb-pager",
        actionButton(id("prev"), "\u2039 Prev", class = "lb-pgbtn"),
        uiOutput(id("pageinfo"), inline = TRUE),
        actionButton(id("nxt"), "Next \u203a", class = "lb-pgbtn")),
    DT::dataTableOutput(id("table"))
  )
}

# ============================================================
# LEAGUE LEADERBOARD — server logic
# ------------------------------------------------------------
# One function wires up one board. Called twice (pitchers, pitches)
# so both boards get identical behaviour with independent state.
# ============================================================

lb_register_board <- function(input, output, session, level) {
  id  <- function(x) paste0("lb_", level, "_", x)
  key <- if (identical(level, "pitches")) "pitches" else "pitchers"

  # Team list for the season, straight off AllTeams (the same directory
  # tm_find_team_id resolves against, so anything listed here is loadable).
  teams_for_season <- reactive({
    req(isTRUE(lb_opened()))
    sn <- suppressWarnings(as.integer(input[[id("season")]] %||% TM_SEASON))
    if (!tm_enabled()) return(character(0))
    tm_with_season(sn, {
      tt <- tryCatch(tm_all_teams(), error = function(e) NULL)
      if (is.null(tt) || nrow(tt) == 0) return(character(0))
      nc <- intersect(c("location", "fullName", "abbrevName"), names(tt))
      if (!length(nc)) return(character(0))
      v <- as.character(tt[[nc[1]]])
      sort(unique(v[!is.na(v) & nzchar(v)]))
    })
  })

  observe({
    tl <- teams_for_season()
    updatePickerInput(session, id("school"), choices = tl,
                      selected = isolate(input[[id("school")]]))
  })

  # ---- Conference -> teams ------------------------------------------
  # Selecting conferences pushes their schools into the team picker.
  # Matching is on letters only because TruMedia and the NCAA logo file
  # disagree on punctuation and St./State.
  .conf_applied <- reactiveVal(character(0))
  observeEvent(input[[id("conf")]], {
    confs <- input[[id("conf")]] %||% character(0)
    tl    <- teams_for_season()
    if (!length(tl)) return()

    want <- conf_team_names(confs)
    hit  <- tl[.conf_norm(tl) %in% .conf_norm(want)]

    # Drop the schools the PREVIOUS conference pick added, keep anything
    # the user chose by hand, then add the new conference's teams.
    prev <- .conf_applied()
    cur  <- isolate(input[[id("school")]]) %||% character(0)
    keep <- setdiff(cur, prev)
    sel  <- unique(c(keep, hit))
    .conf_applied(hit)

    updatePickerInput(session, id("school"), choices = tl, selected = sel)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  output[[id("confnote")]] <- renderUI({
    confs <- input[[id("conf")]] %||% character(0)
    if (!length(confs)) return(NULL)
    tl   <- teams_for_season()
    want <- conf_team_names(confs)
    hit  <- if (length(tl)) tl[.conf_norm(tl) %in% .conf_norm(want)] else character(0)
    miss <- length(want) - length(hit)
    div(style = "color:#6B7280; font-size:12px;",
        sprintf("%s conference%s \u2192 %s team%s matched in %s",
                length(confs), if (length(confs) == 1) "" else "s",
                length(hit), if (length(hit) == 1) "" else "s",
                input[[id("season")]] %||% ""),
        if (miss > 0)
          span(style = "color:#8A5A00;",
               sprintf("  \u00b7 %s not in this season's team list", miss)))
  })
  # Shape/model columns need at least one team: scoring is per staff.
  observe({
    sel <- input[[id("school")]] %||% character(0)
    if (!length(sel) && isTRUE(input[[id("shape")]]))
      updateCheckboxInput(session, id("shape"), value = FALSE)
  })

  # ---- Build triggers -------------------------------------------------
  # The board builds ITSELF the first time its tab is opened, and rebuilds
  # when season / team / limit / deep-load change. It deliberately does
  # NOT build at app startup: a league pull on boot would delay every
  # other page for a tab most sessions never open.
  lb_trigger <- reactiveVal(0)
  lb_opened  <- reactiveVal(FALSE)
  my_tab <- if (identical(level, "pitches")) "League Pitches" else "League Pitchers"

  # Opening the tab NO LONGER fires a pull. It used to, which meant simply
  # clicking through to the board kicked off a league-wide fetch (and for
  # the pitches board, one PlayerTotals call per pitch type) before the
  # user had chosen anything. The board is now built only when asked.
  observeEvent(list(input$main_tabs, input$lb_subtabs), {
    if (!identical(input$main_tabs, "Leaderboards")) return()
    if (!identical(input$lb_subtabs, my_tab)) return()
    lb_opened(TRUE)
  }, ignoreInit = FALSE)

  observeEvent(input[[id("build")]], lb_trigger(isolate(lb_trigger()) + 1),
               ignoreInit = TRUE)

  # Only these three change what gets FETCHED. Everything else on the bar
  # reshapes the frame already in memory, so it must never re-hit the API.
  # When one of them drifts from the last build we flag the board stale
  # rather than silently re-pulling.
  lb_fetch_key <- reactive({
    paste(input[[id("season")]] %||% "",
          paste(sort(input[[id("school")]] %||% character(0)), collapse = ","),
          paste(sort(input[[id("conf")]]   %||% character(0)), collapse = ","),
          as.character(isTRUE(input[[id("shape")]])),
          sep = "||")
  })
  .lb_built_key <- reactiveVal(NULL)
  lb_stale <- reactive({
    bk <- .lb_built_key()
    !is.null(bk) && !identical(bk, lb_fetch_key())
  })

  built <- eventReactive(lb_trigger(), {
    req(lb_trigger() > 0)
    sn    <- suppressWarnings(as.integer(input[[id("season")]] %||% TM_SEASON))
    teams <- input[[id("school")]] %||% character(0)
    shape <- isTRUE(input[[id("shape")]])
    .lb_built_key(isolate(lb_fetch_key()))
    withProgress(message = paste0("Loading ", sn, " leaderboard"), value = 0, {
      lb_league_tables(season = sn, teams = teams, deep = character(0),
                       shape = shape,
                       progress = function(f, m) setProgress(value = f, detail = m))
    })
  }, ignoreNULL = FALSE)

  base <- reactive({
    b <- built()
    if (is.null(b)) return(data.frame())
    b[[key]] %||% data.frame()
  })

  observe({
    d <- base(); req(nrow(d) > 0)
    cl <- unique(d$Class[!is.na(d$Class) & nzchar(d$Class)])
    ord <- c("FR", "RS-FR", "SO", "RS-SO", "JR", "RS-JR", "SR", "RS-SR", "GR")
    updatePickerInput(session, id("class"),
                      choices = c(intersect(ord, cl), sort(setdiff(cl, ord))),
                      selected = character(0))
    if (identical(level, "pitches")) {
      pt <- sort(unique(d$Pitch[!is.na(d$Pitch)]))
      updatePickerInput(session, id("ptype"), choices = pt, selected = character(0))
    }
    mx <- suppressWarnings(max(as.numeric(d$IP), na.rm = TRUE))
    if (is.finite(mx))
      updateSliderInput(session, id("minip"), max = ceiling(mx))

    all_cols <- lb_cols_for(level)
    def <- lb_default_cols(d, level)
    empty <- setdiff(all_cols, def)
    labs <- setNames(as.list(all_cols),
                     ifelse(all_cols %in% empty,
                            paste0(all_cols, "  (no data)"), all_cols))
    updatePickerInput(session, id("cols"), choices = labs, selected = def)
    if (length(empty))
      cat("[Leaderboard]", level, "- no source for:",
          paste(empty, collapse = ", "), "\n")
  })

  output[[id("ranges")]] <- renderUI({
    cc <- input[[id("rangecols")]]
    if (is.null(cc) || length(cc) == 0) return(NULL)
    d <- base(); req(nrow(d) > 0)
    items <- lapply(cc, function(col) {
      if (!col %in% names(d)) return(NULL)
      v <- suppressWarnings(as.numeric(d[[col]]))
      if (!any(is.finite(v))) return(NULL)
      lo <- floor(min(v, na.rm = TRUE) * 100) / 100
      hi <- ceiling(max(v, na.rm = TRUE) * 100) / 100
      if (!is.finite(lo) || !is.finite(hi) || lo == hi) return(NULL)
      e <- lb_spec_entry(level, col)
      stp <- if (!is.null(e) && !is.na(e$dig))
        max(10^(-e$dig), (hi - lo) / 200) else (hi - lo) / 100
      column(4, div(class = "lb-range-item",
        sliderInput(id(paste0("rng_", make.names(col))), col,
                    min = lo, max = hi, value = c(lo, hi), step = stp, sep = "")))
    })
    items <- Filter(Negate(is.null), items)
    if (!length(items)) return(NULL)
    div(class = "lb-rangebox", do.call(fluidRow, items))
  })

  filtered <- reactive({
    d <- base()
    if (is.null(d) || nrow(d) == 0) return(d)

    sel <- input[[id("class")]]
    if (!is.null(sel) && length(sel)) d <- d[d$Class %in% sel, , drop = FALSE]

    # Both hands ticked means "no hand filter" — unknown-hand arms stay.
    sel <- input[[id("hand")]]
    if (!is.null(sel) && length(sel) && length(sel) < 2)
      d <- d[!is.na(d$T) & d$T %in% sel, , drop = FALSE]

    if (identical(level, "pitches")) {
      sel <- input[[id("ptype")]]
      if (!is.null(sel) && length(sel)) d <- d[d$Pitch %in% sel, , drop = FALSE]
    } else {
      ag <- input[[id("age")]]
      if (!is.null(ag) && length(ag) == 2) {
        full <- ag[1] <= 17 && ag[2] >= 26
        d <- d[(is.na(d$Age) & full) |
               (!is.na(d$Age) & d$Age >= ag[1] & d$Age <= ag[2]), , drop = FALSE]
      }
    }

    q <- trimws(input[[id("search")]] %||% "")
    if (nzchar(q)) {
      hay <- paste(if ("Pitcher" %in% names(d)) d$Pitcher else "",
                   if ("School"  %in% names(d)) d$School  else "")
      hit <- tryCatch(grepl(q, hay, ignore.case = TRUE),
                      error = function(e)
                        grepl(q, hay, ignore.case = TRUE, fixed = TRUE))
      d <- d[hit, , drop = FALSE]
    }

    mi <- input[[id("minip")]]
    if (!is.null(mi) && is.finite(mi) && "IP" %in% names(d)) {
      ip <- suppressWarnings(as.numeric(d$IP))
      d <- d[is.finite(ip) & ip >= mi, , drop = FALSE]
    }

    # Min pitches: on the Pitches board this is per pitch TYPE, which is
    # the useful cut -- it drops the four-seamer a guy threw twice.
    mp <- suppressWarnings(as.numeric(input[[id("minp")]] %||% 0))
    if (is.finite(mp) && mp > 0 && "P" %in% names(d)) {
      pp <- suppressWarnings(as.numeric(d$P))
      d <- d[is.finite(pp) & pp >= mp, , drop = FALSE]
    }

    for (col in (input[[id("rangecols")]] %||% character(0))) {
      rv <- input[[id(paste0("rng_", make.names(col)))]]
      if (is.null(rv) || length(rv) != 2 || !col %in% names(d)) next
      v <- suppressWarnings(as.numeric(d[[col]]))
      d <- d[is.finite(v) & v >= rv[1] & v <= rv[2], , drop = FALSE]
    }
    d
  })

  # ---- Sort, then page -------------------------------------------------
  # Order has to be settled server side: we only ship one page to the
  # browser, so "top 50" is meaningless until we know what we sorted on.
  sorted <- reactive({
    d <- filtered()
    if (is.null(d) || nrow(d) == 0) return(d)
    sb <- input[[id("sortby")]] %||% "IP"
    if (!sb %in% names(d)) sb <- if ("IP" %in% names(d)) "IP" else names(d)[1]
    dec <- !identical(input[[id("sortdir")]], "asc")
    v <- d[[sb]]
    if (!is.character(v)) v <- suppressWarnings(as.numeric(v))
    d[order(v, na.last = TRUE, decreasing = dec), , drop = FALSE]
  })

  lb_page <- reactiveVal(1)
  lb_per  <- reactive({
    k <- suppressWarnings(as.integer(input[[id("perpage")]] %||% 50))
    if (is.finite(k) && k > 0) k else 50L
  })
  lb_npages <- reactive({
    d <- sorted()
    nr <- if (is.null(d)) 0 else nrow(d)
    max(1L, as.integer(ceiling(nr / lb_per())))
  })

  # Any reshape sends you back to page 1 — otherwise you can sit on page 6
  # of a board that now has two pages.
  observeEvent(list(input[[id("class")]], input[[id("hand")]],
                    input[[id("ptype")]], input[[id("age")]],
                    input[[id("minip")]], input[[id("minp")]],
                    input[[id("search")]],
                    input[[id("sortby")]], input[[id("sortdir")]],
                    input[[id("perpage")]], input[[id("rangecols")]],
                    lb_trigger()),
               lb_page(1L), ignoreInit = TRUE)

  observeEvent(input[[id("prev")]], {
    lb_page(max(1L, isolate(lb_page()) - 1L))
  }, ignoreInit = TRUE)
  observeEvent(input[[id("nxt")]], {
    lb_page(min(lb_npages(), isolate(lb_page()) + 1L))
  }, ignoreInit = TRUE)
  observe({
    if (lb_page() > lb_npages()) lb_page(lb_npages())
  })

  page_slice <- reactive({
    d <- sorted()
    if (is.null(d) || nrow(d) == 0) return(list(d = d, from = 0L))
    pg <- min(max(1L, lb_page()), lb_npages())
    k  <- lb_per()
    from <- (pg - 1L) * k + 1L
    to   <- min(nrow(d), pg * k)
    list(d = d[from:to, , drop = FALSE], from = from)
  })

  output[[id("pageinfo")]] <- renderUI({
    if (lb_trigger() == 0) return(span(class = "lb-pageinfo", ""))
    d <- sorted()
    nr <- if (is.null(d)) 0 else nrow(d)
    if (nr == 0) return(span(class = "lb-pageinfo", "No rows"))
    ps <- page_slice()
    from <- ps$from; to <- from + nrow(ps$d) - 1L
    span(class = "lb-pageinfo", sprintf(
      "%s\u2013%s of %s  \u00b7  page %s / %s",
      format(from, big.mark = ","), format(to, big.mark = ","),
      format(nr, big.mark = ","),
      min(max(1L, lb_page()), lb_npages()), lb_npages()))
  })

  output[[id("status")]] <- renderUI({
    msg <- function(txt, tone = "#6B7280")
      div(class = "lb-empty", style = paste0("color:", tone), txt)

    if (lb_trigger() == 0)
      return(msg(paste0("Pick a season \u2014 and, to keep it quick, a few ",
                        "teams \u2014 then press Build Board.")))

    b <- built()
    if (is.null(b) || (NROW(b$pitchers) == 0 && NROW(b$pitches) == 0))
      return(msg(b$note %||% "Loading the league\u2026"))
    if (NROW(base()) == 0 && identical(level, "pitches"))
      return(msg(paste0("No pitch-level data loaded. Add teams to ",
                        "\u201cLoad Pitch-Level For\u201d and rebuild.")))

    stale <- if (isTRUE(lb_stale()))
      div(class = "lb-note", style = "background:#FFF6E5;color:#8A5A00;",
          "Season / team / pitch-level selection changed \u2014 press ",
          tags$b("Build Board"), " to refetch.") else NULL

    d <- filtered()
    if (nrow(d) == 0)
      return(tagList(stale, msg("No one matches these filters.")))
    tagList(stale, if (!is.null(b$note)) div(class = "lb-note", b$note))
  })

  output[[id("table")]] <- DT::renderDataTable({
    ps <- page_slice()
    d  <- ps$d
    validate(need(!is.null(d) && nrow(d) > 0, ""))
    defs <- lb_default_cols(base(), level)
    lb_render_table(d, level, input[[id("cols")]] %||% defs,
                    rank_offset = ps$from - 1L)
  }, server = TRUE)
}

ncaa_raw_lookup <- setNames(ncaa_directory$raw_names, ncaa_directory$display)

is_ncaa_pitcher <- function(nm) {
  !is.null(nm) && nzchar(nm) && nm %in% ncaa_directory$display
}

# Map the app's season codes onto the NCAA parquet source years so the
# global search only offers arms that exist in the selected dataset(s).
#   Spring25              -> 2025 files
#   Fall25 / PreSpring26  -> lead-in to the 2026 season, served by 2026 files
#   Spring26              -> 2026 files
season_src_years <- function(seasons) {
  seasons <- as.character(seasons)
  seasons <- seasons[!is.na(seasons) & nzchar(seasons)]
  if (length(seasons) == 0) return(c("2025", "2026"))  # nothing chosen = show all
  out <- character(0)
  if ("Spring25" %in% seasons)    out <- c(out, "2025")
  if (any(c("Fall25", "PreSpring26", "Spring26", "Fall26") %in% seasons)) out <- c(out, "2026")
  if (length(out) == 0) c("2025", "2026") else unique(out)
}

# Named vector for the global selectize: Coastal roster first (plain
# names), then every NCAA pitcher labeled "Name — Team".
# `src_years` restricts the NCAA block to the selected season's dataset.
build_global_pitcher_choices <- function(coastal_names, src_years = NULL) {
  coastal_names <- unname(as.character(coastal_names))
  coastal_names <- coastal_names[!is.na(coastal_names) & nzchar(coastal_names)]
  cc <- setNames(coastal_names, coastal_names)
  if (nrow(ncaa_directory) == 0) return(cc)

  dir_df <- ncaa_directory
  # Keep only arms that appear in at least one of the selected source years.
  if (!is.null(src_years) && length(src_years) > 0 &&
      "srcs" %in% names(dir_df)) {
    keep <- vapply(dir_df$srcs, function(s) {
      s <- as.character(s)
      length(s) == 0 || any(s %in% src_years)
    }, logical(1))
    dir_df <- dir_df[keep, , drop = FALSE]
  }
  if (nrow(dir_df) == 0) return(cc)

  disp  <- unname(as.character(dir_df$display))
  teams <- unname(as.character(dir_df$teams))
  teams[is.na(teams)] <- ""
  ok <- !is.na(disp) & nzchar(disp)
  nc <- setNames(disp[ok], paste0(disp[ok], " \u2014 ", teams[ok]))
  out <- c(cc, nc[!nc %in% coastal_names])
  out[!is.na(out) & !is.na(names(out)) & nzchar(names(out))]
}

cat("NCAA directory:", nrow(ncaa_directory), "pitchers available for scouting\n")

# ============================================================
