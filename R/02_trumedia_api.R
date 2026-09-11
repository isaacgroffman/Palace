# ============================================================
# TruMedia API client
# ------------------------------------------------------------
# Credentials come from HF Space secrets (Settings -> Variables
# and secrets). NEVER hardcode the master token in source:
#   TM_USERNAME     = ingroffma@coastal.edu
#   TM_SITENAME     = coastalcarolina-ncaabaseball
#   TM_MASTER_TOKEN = <master token>
# Temp tokens last 24h -> we mint one and reuse it for 23h.
# 429s are retried per the Retry-After header; all requests are
# sequential (no parallel pulls) per TruMedia's guidance.
# ============================================================
TM_SEASON <- 2026
TM_BASE   <- "https://api.trumedianetworks.com/v1/mlbapi/custom/baseball/DirectedQuery"

.tm_env <- new.env(parent = emptyenv())

# Season resolution. Everything in the app runs on TM_SEASON; the
# leaderboards need to ask for a different year WITHOUT moving the rest
# of the app off the current season, so they wrap their pulls in
# tm_with_season(). The override is dynamically scoped and always
# restored, including on error, so a failed pull can't strand the app
# on the wrong year.
tm_season <- function() .tm_env$season_override %||% TM_SEASON

tm_with_season <- function(season, expr) {
  if (is.null(season) || !nzchar(as.character(season)))
    return(force(expr))
  old <- .tm_env$season_override
  .tm_env$season_override <- as.integer(season)
  on.exit(.tm_env$season_override <- old, add = TRUE)
  force(expr)
}

# Seasons offered in the leaderboard picker. TruMedia's college coverage
# starts at 2021; the list is trimmed to whatever actually returns teams.
TM_SEASONS_ALL <- 2021:TM_SEASON
.tm_env$cache <- list()
.tm_env$last_error <- NULL


tm_enabled <- function() {
  nzchar(Sys.getenv("TM_USERNAME")) &&
    nzchar(Sys.getenv("TM_SITENAME")) &&
    nzchar(Sys.getenv("TM_MASTER_TOKEN"))
}

tm_token <- function(force = FALSE) {
  if (!tm_enabled()) return(NULL)
  now <- Sys.time()
  if (!force && !is.null(.tm_env$token) &&
      difftime(now, .tm_env$token_time, units = "hours") < 23) {
    return(.tm_env$token)
  }
  body <- sprintf('{"username":"%s","sitename":"%s","token":"%s"}',
                  Sys.getenv("TM_USERNAME"),
                  Sys.getenv("TM_SITENAME"),
                  Sys.getenv("TM_MASTER_TOKEN"))
  res <- tryCatch(
    httr::POST("https://api.trumedianetworks.com/v1/siteadmin/api/createTempPBToken",
               httr::add_headers("Content-Type" = "application/json"),
               body = body, httr::timeout(30)),
    error = function(e) NULL)
  if (is.null(res) || httr::status_code(res) != 200) {
    cat("TruMedia: temp token request failed\n"); return(NULL)
  }
  tok <- tryCatch(httr::content(res)$pbTempToken, error = function(e) NULL)
  if (is.null(tok) || !nzchar(tok)) return(NULL)
  .tm_env$token <- tok
  .tm_env$token_time <- now
  tok
}

# Generic CSV query. `params` are URL-encoded name=value pairs;
# `raw_query` is for pre-encoded pieces (filters / qualification /
# sort strings copied from the TruMedia Filter Syntax Tool).
tm_get_csv <- function(dataFormat, params = list(), raw_query = "",
                       use_cache = TRUE) {
  key <- paste(dataFormat,
               paste(names(params), unlist(params), collapse = "&"),
               raw_query, sep = "|")
  if (use_cache && !is.null(.tm_env$cache[[key]])) return(.tm_env$cache[[key]])
  # GamePitchesTrackman responses are the dominant cost of a pitcher's
  # first load (each chunk is every pitch of ~20 games) and only change
  # when a new game finishes — persist them for the day so a restart, or
  # a second pitcher on the same team, never re-downloads a chunk.
  disk_path <- NULL
  if (use_cache && identical(dataFormat, "GamePitchesTrackman")) {
    disk_path <- file.path(.scout_cache_dir(),
                           paste0("tm_", rlang::hash(key), "_",
                                  format(Sys.Date(), "%Y%m%d"), ".rds"))
    if (file.exists(disk_path)) {
      hit <- tryCatch(readRDS(disk_path), error = function(e) NULL)
      if (!is.null(hit)) {
        .tm_env$cache[[key]] <- hit
        return(hit)
      }
    }
  }
  set_err <- function(msg) {
    .tm_env$last_error <- msg
    cat("TruMedia:", msg, "\n")
    NULL
  }
  for (attempt in 1:4) {
    tok <- tm_token()
    if (is.null(tok)) return(set_err("could not mint temp token (check TM_* secrets)"))
    qs <- if (length(params)) {
      paste0(names(params), "=",
             vapply(seq_along(params), function(i) {
               v <- as.character(params[[i]])
               # columns must be passed LITERALLY — TruMedia's own R
               # examples paste "[IP],[BA|PIT],[K%|PIT]" raw into the
               # URL; the backend doesn't decode this param, so any
               # %-encoding arrives as garbage stat names and 500s.
               # EXCEPTION: '#' is a URL fragment delimiter, so a raw
               # '[Swing#]' gets truncated to '[Swing' by the HTTP
               # layer before it ever reaches TruMedia. Encode ONLY
               # that one char (-> %23); everything else stays literal.
               if (names(params)[i] == "columns") gsub("#", "%23", v, fixed = TRUE)
               else utils::URLencode(v, reserved = TRUE)
             }, character(1)),
             collapse = "&")
    } else ""
    url <- paste0(TM_BASE, "/", dataFormat, ".csv?", qs, raw_query,
                  "&token=", tok)
    resp <- tryCatch(httr::GET(url, httr::timeout(90)), error = function(e) NULL)
    if (is.null(resp)) return(set_err(paste0("request failed (network) on ", dataFormat)))
    sc <- httr::status_code(resp)
    txt <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"),
                    error = function(e) "")
    if (sc == 200) {
      # error payloads often arrive as JSON/HTML with a 200 — don't
      # let read_csv silently swallow them
      if (grepl("^\\s*[{<]", txt)) {
        return(set_err(paste0(dataFormat, " returned error body: ",
                              substr(gsub("\\s+", " ", txt), 1, 300))))
      }
      out <- tryCatch(readr::read_csv(I(txt), show_col_types = FALSE,
                                      progress = FALSE),
                      error = function(e) NULL)
      if (is.null(out)) {
        return(set_err(paste0(dataFormat, ": response was not parseable CSV: ",
                              substr(txt, 1, 200))))
      }
      if (nrow(out) == 0) {
        return(set_err(paste0(dataFormat, ": query returned 0 rows (check season/team/filters)")))
      }
      .tm_env$cache[[key]] <- out
      if (!is.null(disk_path))
        tryCatch(saveRDS(out, disk_path), error = function(e) NULL)
      .tm_env$last_error <- NULL
      return(out)
    }
    if (sc == 429) {  # rate limited -> honor Retry-After
      wait <- suppressWarnings(as.numeric(httr::headers(resp)[["retry-after"]]))
      Sys.sleep(if (is.finite(wait) && wait > 0) wait else 5 * attempt)
      next
    }
    if (sc == 401) { tm_token(force = TRUE); next }  # stale temp token
    return(set_err(paste0("HTTP ", sc, " on ", dataFormat,
                          " [", substr(paste0(qs, raw_query), 1, 200), "] : ",
                          substr(gsub("\\s+", " ", txt), 1, 160))))
  }
  set_err(paste0("gave up after retries on ", dataFormat))
}

# ---- name / team normalizers -------------------------------------
.tm_norm <- function(x) {
  x <- iconv(as.character(x), to = "ASCII//TRANSLIT")
  x <- tolower(x)
  # "last, first" -> "first last"
  flip <- grepl(",", x)
  x[flip] <- sub("^\\s*([^,]+),\\s*(.+)\\s*$", "\\2 \\1", x[flip])
  gsub("[^a-z ]", "", x) |> trimws()
}

# ---- team directory (AllTeams once per session) -------------------
tm_all_teams <- function() {
  pre <- tryCatch(if (exists("lb_precomputed", mode = "function")) lb_precomputed(tm_season()) else NULL,
                  error = function(e) NULL)
  if (is.data.frame(pre$teams) && nrow(pre$teams) > 0) return(pre$teams)
  tm_get_csv("AllTeams", params = list(seasonYear = tm_season()))
}

# "overall" PlayerTotals for one team from the precomputed league-wide
# frames (scripts/build_leaderboards.R), when every requested column is
# there. Same shape the API returns, so callers are unchanged.
.tm_totals_precomputed <- function(team_id, columns) {
  pre <- tryCatch(if (exists("lb_precomputed", mode = "function")) lb_precomputed(tm_season()) else NULL,
                  error = function(e) NULL)
  if (is.null(pre) || !length(pre)) return(NULL)
  want <- if (is.null(columns)) character(0) else
    gsub("^\\[|\\]$", "", strsplit(columns, ",")[[1]])
  want <- sub("\\|.*$", "", trimws(want))
  canon <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  for (fr in list(pre$tm_pitching_totals, pre$tm_batting_totals)) {
    if (!is.data.frame(fr) || !nrow(fr) || !"mostRecentTeamId" %in% names(fr)) next
    have <- canon(names(fr))
    if (!all(canon(want) %in% have)) next
    out <- fr[as.character(fr$mostRecentTeamId) == as.character(team_id), , drop = FALSE]
    if (nrow(out)) return(out)
  }
  NULL
}

.tm_pick_col <- function(df, patterns) {
  for (p in patterns) {
    hit <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
    if (length(hit) > 0) return(hit[1])
  }
  NULL
}

tm_find_team_id <- function(team_disp) {
  if (is.null(team_disp) || !nzchar(team_disp)) return(NULL)
  teams <- tm_all_teams()
  if (is.null(teams) || nrow(teams) == 0) return(NULL)
  id_col <- .tm_pick_col(teams, c("^teamId$", "team.?id", "^id$"))
  if (is.null(id_col)) return(NULL)
  # AllTeams carries several name fields; teamName is the MASCOT
  # (e.g. "Chanticleers"), so match in priority order:
  # fullName ("Coastal Carolina University") -> location
  # ("Coastal Carolina") -> abbrevName ("CCAR") -> teamName last.
  name_cols <- intersect(c("fullName", "location", "abbrevName", "teamName"),
                         names(teams))
  if (length(name_cols) == 0)
    name_cols <- grep("name|location", names(teams), ignore.case = TRUE,
                      value = TRUE)
  tgt_raw <- .tm_norm(team_disp)
  # "Florida St." (TrackMan) vs "Florida State" (TruMedia); also
  # St. John's-style Saints
  tgts <- unique(c(tgt_raw,
                   gsub("\\bst\\b", "state", tgt_raw),
                   gsub("\\bst\\b", "saint", tgt_raw)))
  # pass 1: exact normalized match on any name column, any variant
  for (nc in name_cols) {
    nm <- .tm_norm(teams[[nc]])
    for (tg in tgts) {
      hit <- which(nm == tg)
      if (length(hit) > 0) return(teams[[id_col]][hit[1]])
    }
  }
  # pass 2: containment either direction (skip abbrevs — too short)
  for (nc in setdiff(name_cols, "abbrevName")) {
    nm <- .tm_norm(teams[[nc]])
    ok <- !is.na(nm) & nzchar(nm)
    for (tg in tgts) {
      hit <- which(ok & (grepl(tg, nm, fixed = TRUE) |
                           vapply(nm, function(z)
                             nzchar(z) && nchar(z) > 4 &&
                               grepl(z, tg, fixed = TRUE), logical(1))))
      if (length(hit) > 0) return(teams[[id_col]][hit[1]])
    }
  }
  # pass 3: DRS26 bridge — the roster file pairs each team's ncaa.com
  # display name with TruMedia's own newestTeamName / Location /
  # AbbrevName, so any roster player on the target team hands us the
  # exact strings AllTeams will match on ("Ole Miss" -> "University of
  # Mississippi" / "MISS"). Runs the display name through every alias.
  if (exists("drs_bio") && !is.null(drs_bio) && "team_name" %in% names(drs_bio)) {
    dn <- .tm_norm(as.character(drs_bio$team_name))
    dl <- if ("newestTeamLocation" %in% names(drs_bio))
      .tm_norm(as.character(drs_bio$newestTeamLocation)) else rep(NA_character_, nrow(drs_bio))
    sel <- (!is.na(dn) & dn %in% tgts) | (!is.na(dl) & dl %in% tgts)
    if (any(sel)) {
      acols <- intersect(c("newestTeamName", "newestTeamLocation",
                           "newestTeamAbbrevName"), names(drs_bio))
      aliases <- unique(unlist(lapply(acols, function(cc)
        as.character(drs_bio[[cc]][sel]))))
      aliases <- aliases[!is.na(aliases) & nzchar(aliases)]
      for (al in aliases) {
        an <- .tm_norm(al)
        if (!nzchar(an)) next
        for (nc in name_cols) {
          hit <- which(.tm_norm(teams[[nc]]) == an)
          if (length(hit) > 0) return(teams[[id_col]][hit[1]])
        }
      }
    }
  }
  .tm_env$last_error <- paste0("no AllTeams match for '", team_disp, "'")
  NULL
}

# ---- stat pulls ---------------------------------------------------
# Column abbreviations come from the TruMedia glossary
# (<site>.trumedianetworks.com/baseball/glossary). Adjust here if a
# pull comes back with an unknown-stat error.
TM_TRAD_COLS  <- "[G],[GS],[W],[L],[SV],[IP],[H|PIT],[R|PIT],[ER],[BB|PIT],[K|PIT],[HBP|PIT],[HR|PIT],[ERA],[WHIP],[BA|PIT]"
TM_SPLIT_COLS <- "[PA|PIT],[BA|PIT],[SLG|PIT],[K%|PIT],[BB%|PIT],[InZoneWhiff%|PIT],[Chase%|PIT],[FPStk%|PIT]"

TM_SPLIT_FILTERS <- list(
  overall = "",
  vLHH    = "&filters=(((event.batterHand%20%3D%20'L')))",
  vRHH    = "&filters=(((event.batterHand%20%3D%20'R')))",
  RISP    = "&filters=(((event.manOnSecond%20OR%20event.manOnThird)))",
  # batting splits (pitcher hand) for the hitter Overview
  vLHP    = "&filters=(((event.pitcherHand%20%3D%20'L')))",
  vRHP    = "&filters=(((event.pitcherHand%20%3D%20'R')))"
)

# Doc-verified minimal column sets (every abbreviation below appears
# verbatim in TruMedia's own documentation examples). The full sets
# above are attempted first; if the backend 500s on an unknown stat,
# we degrade to these, then to no columns at all (API defaults).
TM_TRAD_COLS_MIN  <- "[IP],[ERA],[WHIP],[K|PIT],[BB|PIT],[BA|PIT]"
# Batting columns are UNQUALIFIED: TruMedia stats default to the batting
# split (that's why the pitching sets above need |PIT). A |BAT qualifier
# does not exist and 500s the request.
TM_BAT_COLS       <- "[G],[PA],[AB],[BA],[OBP],[SLG],[HR],[SB]"
TM_BAT_COLS_MIN   <- "[PA],[AB],[BA]"
# Full traditional batting line for the hitter Overview strip. Batting
# columns are unqualified (batting is TruMedia's default split).
TM_BAT_TRAD_COLS  <- "[G],[PA],[AB],[H],[2B],[3B],[HR],[BB],[HBP],[K],[SB],[BA],[OBP],[SLG]"
TM_SPLIT_COLS_MIN <- "[IP],[BA|PIT],[InZoneWhiff%|PIT],[Chase%|PIT],[FPStk%|PIT]"

# ---- batter grade inputs (matrix pills) --------------------------
# TruMedia rate stats like Contact%/Chase%/Miss% are NOT atomic
# columns — the glossary defines them as ratios of COUNT columns
# (e.g. Contact% = [Contact#] / [Swing#], Chase% = [Chase#] /
# [OutOfZone#], Miss% = [Miss#] / [Swing#]). Asking for [Contact%]
# etc. 500s the backend. So we pull the count columns and compute the
# rates in R, exactly like the pitch-level indicator math elsewhere.
#
# Overall pull gives: swings, contact, miss (whiff), chase, out-of-zone.
# An in-zone filtered pull gives the same counts restricted to the
# zone -> Z-Contact% and Z-Whiff%. xwOBA / 90th-EV / Edge-Take% are
# atomic-if-present; they're probed separately and simply omitted from
# a grade when the site doesn't expose them.
#
# Every '#'-token is UNQUALIFIED (batting is TruMedia's default split).
# Candidate spellings are tried in order and the winner is remembered
# for the session, so we adapt to the site's exact glossary without a
# hard-coded guess.
TM_BAT_COUNT_CANDIDATES <- list(
  swing   = c("Swing#", "Swings#", "Swing", "Swings"),
  contact = c("Contact#", "Contacts#", "Contact"),
  miss    = c("Miss#", "Misses#", "Whiff#", "Miss", "SwStr#"),
  chase   = c("Chase#", "Chases#", "Chase", "OSwing#", "OutOfZoneSwing#"),
  ooz     = c("OutOfZone#", "OutofZone#", "OOZ#", "PitchesOutOfZone#"),
  pa      = c("PA")
)
# atomic (single-column) grade inputs, probed one at a time so one bad
# token can't sink the whole pull. EV90 / Max EV are NOT probed here
# anymore — the site 500s on every spelling, so both are computed from
# the pitch-by-pitch pool instead (see .pool_ev_table). Edge-Take% is
# dropped entirely.
TM_BAT_ATOMIC_CANDIDATES <- list(
  xwoba  = c("xwOBA", "xWOBA", "ExpectedWOBA")
)

# ---- EV90 / Max EV straight from pitch data -----------------------
# 90th-percentile and max exit velocity per batter, computed from the
# team's pitch-by-pitch pool (balls in play only). Memoised on a cheap
# frame fingerprint so a lineup render costs one pass.
.pool_ev_env <- new.env(parent = emptyenv())
.pool_ev_table <- function(pool) {
  if (is.null(pool) || !is.data.frame(pool) || nrow(pool) == 0 ||
      !all(c("Batter", "ExitSpeed") %in% names(pool))) return(NULL)
  ev_all <- suppressWarnings(as.numeric(pool$ExitSpeed))
  fp <- paste(nrow(pool), round(sum(ev_all, na.rm = TRUE), 1), sep = "|")
  hit <- .pool_ev_env[[fp]]
  if (!is.null(hit)) return(hit)
  bip <- if ("PitchCall" %in% names(pool))
    as.character(pool$PitchCall) == "InPlay" else rep(TRUE, nrow(pool))
  bip[is.na(bip)] <- FALSE
  ok <- bip & is.finite(ev_all) & !is.na(pool$Batter) & nzchar(pool$Batter)
  if (!any(ok)) return(NULL)
  d <- data.frame(key = .tm_norm(pool$Batter[ok]), ev = ev_all[ok],
                  stringsAsFactors = FALSE)
  sp <- split(d$ev, d$key)
  out <- data.frame(
    key   = names(sp),
    n_bip = vapply(sp, length, numeric(1)),
    ev90  = vapply(sp, function(v)
      if (length(v) >= 5) as.numeric(stats::quantile(v, 0.9, na.rm = TRUE))
      else NA_real_, numeric(1)),
    maxev = vapply(sp, function(v) max(v, na.rm = TRUE), numeric(1)),
    stringsAsFactors = FALSE)
  .pool_ev_env[[fp]] <- out
  out
}
# One PlayerTotals pull per (team, split); every pitcher on that
# team resolves from the cached frame, so a full staff costs 4 calls.
tm_team_player_totals <- function(team_id, split = "overall",
                                  columns = TM_TRAD_COLS) {
  if (is.null(team_id)) return(NULL)
  if (identical(split, "overall")) {
    pre <- tryCatch(.tm_totals_precomputed(team_id, columns), error = function(e) NULL)
    if (!is.null(pre)) return(pre)
  }
  rq <- TM_SPLIT_FILTERS[[split]] %||% ""
  col_min <- if (identical(columns, TM_TRAD_COLS)) TM_TRAD_COLS_MIN
             else if (identical(columns, TM_BAT_COLS) ||
                      identical(columns, TM_BAT_TRAD_COLS)) TM_BAT_COLS_MIN
             else TM_SPLIT_COLS_MIN
  variants <- list(columns, col_min, NULL)
  pnames   <- c("teamId", "team")   # PDF says teamId, sample says team
  # Variant memory is PER column spec: a batting request settling on the
  # API-default fallback must not degrade the next pitching request's
  # column set (that shared memory was dropping ERA/WHIP from Overview).
  if (is.null(.tm_env$pt_mem)) .tm_env$pt_mem <- list()
  mkey <- columns %||% "<default>"
  mem <- .tm_env$pt_mem[[mkey]]
  v_order <- unique(c(mem$v %||% 1L, seq_along(variants)))
  p_order <- unique(c(mem$p %||% 1L, seq_along(pnames)))
  for (vi in v_order) {
    cv <- variants[[vi]]
    for (pi in p_order) {
      params <- list(seasonYear = tm_season(), format = "RAW")
      params[[pnames[pi]]] <- team_id
      if (!is.null(cv)) params$columns <- cv
      out <- tm_get_csv("PlayerTotals", params = params, raw_query = rq)
      if (!is.null(out)) {
        if (is.null(mem) && vi > 1)
          cat("TruMedia: PlayerTotals [", substr(mkey, 1, 30), "...] settled on ",
              if (is.null(cv)) "API default columns" else "minimal column set",
              " — remembering for this session\n", sep = "")
        .tm_env$pt_mem[[mkey]] <- list(v = vi, p = pi)
        return(out)
      }
    }
  }
  NULL
}

# TruMedia pitch-type codes (from GamePitchesTrackman responses) for
# filter construction, keyed by TrackMan tagged names.
TM_PITCH_CODE <- c(Fastball = "FA", Sinker = "SI", Slider = "SL",
                   Sweeper = "ST", Cutter = "FC", Curveball = "CU",
                   CurveBall = "CU", `Knuckle Curve` = "KC", Slurve = "SV",
                   ChangeUp = "CH", Changeup = "CH", Splitter = "FS")

# BA/SLG for one pitcher, one batter hand, one pitch type — pulled
# from PlayerTotals with combined filters (more accurate than
# estimating from pitch data). One probe failure disables the whole
# path for the session so an unsupported filter can't spam the API.
tm_pitch_side_row <- function(team_id, pitcher_display, hand, pitch_name) {
  if (is.null(team_id) || isFALSE(.tm_env$pitchtype_ok)) return(NULL)
  code <- unname(TM_PITCH_CODE[as.character(pitch_name)])
  if (length(code) != 1 || is.na(code)) return(NULL)
  rq <- paste0("&filters=(((event.batterHand%20%3D%20'", hand,
               "')%20AND%20(event.pitchType%20%3D%20'", code, "')))")
  out <- NULL
  for (pn in c("teamId", "team")) {
    params <- list(seasonYear = tm_season(), format = "RAW",
                   columns = "[BA|PIT],[SLG|PIT]")
    params[[pn]] <- team_id
    out <- tm_get_csv("PlayerTotals", params = params, raw_query = rq)
    if (!is.null(out)) break
  }
  if (is.null(out)) {
    if (is.null(.tm_env$pitchtype_ok)) {
      .tm_env$pitchtype_ok <- FALSE
      cat("TruMedia: per-pitch-type filters unsupported - using pitch-data BA/SLG\n")
    }
    return(NULL)
  }
  .tm_env$pitchtype_ok <- TRUE
  tm_player_row(out, pitcher_display)
}

# Find the row for one pitcher inside a PlayerTotals frame.
tm_player_row <- function(df, pitcher_display) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  nm_col <- .tm_pick_col(df, c("playerName", "playerFullName", "^player$", "name"))
  if (is.null(nm_col)) return(NULL)
  tgt <- .tm_norm(pitcher_display)
  nm  <- .tm_norm(df[[nm_col]])
  hit <- which(nm == tgt)
  if (length(hit) == 0) {
    # fall back: last name + first initial
    parts <- strsplit(tgt, " ")[[1]]
    if (length(parts) >= 2) {
      last <- parts[length(parts)]; fi <- substr(parts[1], 1, 1)
      hit <- which(grepl(paste0("\\b", last, "\\b"), nm) &
                     substr(nm, 1, 1) == fi)
    }
  }
  if (length(hit) == 0) return(NULL)
  df[hit[1], , drop = FALSE]
}

# Pull one stat out of a row. The API's default columns use various
# header spellings, so match through a synonym table after
# normalizing (lowercase, strip |PIT suffix and punctuation).
tm_stat <- function(row, abbr) {
  if (is.null(row)) return(NA)
  syn <- list(
    IP  = c("ip", "inningspitched", "innings"),
    ERA = "era", WHIP = "whip",
    W   = c("w", "win", "wins"),
    L   = c("l", "loss", "losses"),
    G   = c("g", "games", "app", "appearances", "gp"),
    GS  = c("gs", "gamesstarted", "starts"),
    H   = c("h", "hits", "hitsallowed", "ha"),
    BB  = c("bb", "walks", "walksallowed", "bba"),
    K   = c("k", "so", "strikeouts", "ks"),
    SO  = c("so", "k", "strikeouts"),
    HBP = c("hbp", "hitbypitch", "hitbatters"),
    HR  = c("hr", "homeruns", "hra"),
    R   = c("r", "runs", "runsallowed", "ra"),
    ER  = c("er", "earnedruns"),
    BA  = c("ba", "avg", "baa", "battingaverage", "oppba", "opponentavg", "bavg"),
    SV  = c("sv", "saves"),
    PA  = c("pa", "plateappearances", "tbf", "battersfaced"),
    SLG = c("slg", "slugging", "oppslg"),
    # batter grade inputs — count columns + atomic metrics. Keys here
    # are matched after stripping punctuation, so 'Contact#' -> contact,
    # 'OutOfZone#' -> outofzone, etc.
    XWOBA        = c("xwoba", "expectedwoba", "xwobacon"),
    SWING        = c("swing", "swings"),
    CONTACT      = c("contact", "contacts"),
    MISS         = c("miss", "misses", "whiff", "swstr"),
    CHASE        = c("chase", "chases", "oswing"),
    OUTOFZONE    = c("outofzone", "ooz", "pitchesoutofzone"),
    EV90         = c("exitvelocity90", "ev90", "exitvelo90", "90ev",
                     "maxexitvelocity"),
    EDGETAKE     = c("edgetake", "edgetakepct")
  )
  clean <- function(z) {
    z <- tolower(as.character(z))
    z <- sub("\\|pit$", "", z)
    gsub("[^a-z0-9]", "", z)
  }
  keys <- clean(names(row))
  cand <- unique(c(clean(abbr), syn[[toupper(abbr)]]))
  hit <- which(keys %in% cand)
  if (length(hit) == 0) return(NA)
  row[[hit[1]]]
}

# Baseball IP notation helpers. Display fraction = outs (.0/.1/.2);
# decimal form is for math (P/IP, WHIP, etc).
tm_ip_display <- function(ip) {
  ip <- suppressWarnings(as.numeric(ip))
  if (!is.finite(ip)) return(NA_real_)
  frac10 <- (ip %% 1) * 10
  fr <- round(frac10)
  if (fr %in% c(0, 1, 2) && abs(frac10 - fr) < 0.05) return(round(ip, 1))
  outs <- round(ip * 3)
  outs %/% 3 + (outs %% 3) / 10
}
# Vectorized: the leaderboard converts a whole staff's IP at once.
# Identical output to the old scalar version for length-1 input.
tm_ip_decimal <- function(ip_disp) {
  v <- suppressWarnings(as.numeric(ip_disp))
  ifelse(is.finite(v), floor(v) + round((v %% 1) * 10) / 3, NA_real_)
}

# All four splits for one pitcher: list(overall=, vLHH=, vRHH=, RISP=)
tm_pitcher_splits <- function(pitcher_display, team_disp) {
  team_id <- tm_find_team_id(team_disp)
  if (is.null(team_id)) return(NULL)
  sps <- c("overall", "vLHH", "vRHH", "RISP")
  out <- lapply(sps, function(sp) {
    cols <- if (sp == "overall") TM_TRAD_COLS else TM_SPLIT_COLS
    tm_player_row(tm_team_player_totals(team_id, sp, cols), pitcher_display)
  })
  names(out) <- sps
  if (all(vapply(out, is.null, logical(1)))) return(NULL)
  out
}

# Batting splits for one hitter: list(overall=, vLHP=, vRHP=), each the
# full traditional batting line. Same team-level cache as the pitcher
# pulls, so a whole lineup costs three PlayerTotals calls.
tm_batter_splits <- function(batter_display, team_disp) {
  team_id <- tm_find_team_id(team_disp)
  if (is.null(team_id)) return(NULL)
  sps <- c("overall", "vLHP", "vRHP")
  out <- lapply(sps, function(sp)
    tm_player_row(tm_team_player_totals(team_id, sp, TM_BAT_TRAD_COLS),
                  batter_display))
  names(out) <- sps
  if (all(vapply(out, is.null, logical(1)))) return(NULL)
  out
}

# ---- batter grades for the matrix pills ---------------------------
# One PlayerTotals pull per batter team gets the raw grade inputs for
# the whole roster; grades are then computed relative to that pull's
# own distribution (so "100" = this section's average). Cached per
# team_id for the session.
.tm_env$bat_grade_cache <- list()

# league priors (NCAA D1, rough) used only to anchor the scale when a
# section is tiny; the section's own mean/sd dominate once it's large.
.BAT_GRADE_PRIORS <- list(
  xwoba   = c(m = 0.360, s = 0.060),
  zcon    = c(m = 85.0,  s = 6.0),   # in-zone contact %
  whiff   = c(m = 24.0,  s = 6.0),
  zwhiff  = c(m = 13.0,  s = 5.0),
  chase   = c(m = 28.0,  s = 6.0),
  ev90    = c(m = 100.0, s = 4.5)    # 90th-pct EV, mph (from pitch data)
)
.MM_GRADE_MIN_PA <- 25   # below this a hitter's grades are dashed/greyed

# z -> 100-centered grade. dir = +1 when higher is better, -1 when
# lower is better (whiff, chase). Clamped to a sane 40-160 band so one
# outlier can't blow out the pill.
.grade_from_z <- function(z, dir = 1) {
  if (!is.finite(z)) return(NA_real_)
  g <- 100 + dir * 15 * z          # 15 pts per SD, wOBA-plus feel
  max(40, min(160, round(g)))
}
# blended mean/sd: section stats shrunk toward the league prior so a
# 3-hitter opponent card doesn't grade off pure noise.
.blend_ms <- function(vals, prior, k = 8) {
  vals <- vals[is.finite(vals)]
  n <- length(vals)
  if (n == 0) return(list(m = prior["m"], s = prior["s"]))
  m <- (sum(vals) + k * prior["m"]) / (n + k)
  s <- if (n >= 3) stats::sd(vals) else prior["s"]
  if (!is.finite(s) || s <= 0) s <- prior["s"]
  list(m = unname(m), s = unname(s))
}

# Pull + grade an entire batter team. Returns a data.frame keyed by
# normalized player name with columns: contact, power, decision,
# aggregate (each 40-160, 100 = section avg) and pa.
tm_batter_grades_team <- function(team_disp, pool = NULL) {
  tid <- tryCatch(tm_find_team_id(team_disp), error = function(e) NULL)
  if (is.null(tid)) return(NULL)
  ck <- paste0(tid, "::", if (is.null(pool)) 0L else nrow(pool))
  if (!is.null(.tm_env$bat_grade_cache[[ck]]))
    return(.tm_env$bat_grade_cache[[ck]])

  # --- probe which count / atomic tokens this site accepts ---------
  # Cached across teams for the session (the glossary is site-wide).
  # EV90 / Edge-Take% are deliberately NOT probed: every EV90 spelling
  # 500s on this site (EV90 now comes from pitch data via .pool_ev_table)
  # and Edge-Take% is no longer part of the grade.
  if (is.null(.tm_env$bat_tok)) {
    ok_one <- function(tok) {
      out <- tm_get_csv("PlayerTotals",
        params = list(seasonYear = tm_season(), format = "RAW",
                      teamId = tid, columns = paste0("[PA],[", tok, "]")))
      !is.null(out) && nrow(out) > 0
    }
    pick <- function(cands) { for (t in cands) if (ok_one(t)) return(t); NULL }
    .tm_env$bat_tok <- list(
      swing   = pick(TM_BAT_COUNT_CANDIDATES$swing),
      contact = pick(TM_BAT_COUNT_CANDIDATES$contact),
      miss    = pick(TM_BAT_COUNT_CANDIDATES$miss),
      chase   = pick(TM_BAT_COUNT_CANDIDATES$chase),
      ooz     = pick(TM_BAT_COUNT_CANDIDATES$ooz),
      xwoba   = pick(TM_BAT_ATOMIC_CANDIDATES$xwoba))
    cat("TruMedia batter-grade tokens:",
        paste(names(.tm_env$bat_tok),
              vapply(.tm_env$bat_tok, function(x) x %||% "-", character(1)),
              sep = "=", collapse = " "), "\n")
  }
  tok <- .tm_env$bat_tok

  # --- assemble one pull with all accepted tokens ------------------
  want <- c("PA", unlist(tok[c("swing","contact","miss","chase","ooz",
                               "xwoba")]))
  want <- want[!vapply(want, is.null, logical(1)) & nzchar(want)]
  cols <- paste0("[", unique(want), "]", collapse = ",")
  df <- tryCatch(
    tm_team_player_totals(tid, "overall", cols),
    error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) {
    .tm_env$bat_grade_cache[[ck]] <- NA          # negative cache
    return(NULL)
  }

  nm_col <- .tm_pick_col(df, c("playerName", "playerFullName",
                               "^player$", "name"))
  if (is.null(nm_col)) return(NULL)

  num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  # pull a raw count/atomic column by its discovered token (NA vec if
  # the token wasn't accepted so downstream rate math degrades cleanly)
  col_by_tok <- function(t) {
    if (is.null(t) || !nzchar(t)) return(rep(NA_real_, nrow(df)))
    v <- num(vapply(seq_len(nrow(df)),
           function(i) as.character(tm_stat(df[i, , drop = FALSE], t)),
           character(1)))
    if (all(is.na(v))) {           # tm_stat synonym miss -> try exact header
      hit <- which(tolower(names(df)) == tolower(t))
      if (length(hit)) v <- num(df[[hit[1]]])
    }
    v
  }
  pdiv <- function(a, b) ifelse(is.finite(a) & is.finite(b) & b > 0,
                                100 * a / b, NA_real_)

  pa       <- col_by_tok("PA")
  swing_n  <- col_by_tok(tok$swing)
  contact_n<- col_by_tok(tok$contact)
  miss_n   <- col_by_tok(tok$miss)
  chase_n  <- col_by_tok(tok$chase)
  ooz_n    <- col_by_tok(tok$ooz)
  xwoba    <- col_by_tok(tok$xwoba)
  # EV90 from the pitch-by-pitch pool, matched by normalized name — the
  # PlayerTotals EV columns are unavailable on this site (HTTP 500).
  ev90 <- rep(NA_real_, nrow(df))
  evtab <- .pool_ev_table(pool)
  if (!is.null(evtab)) {
    idx <- match(.tm_norm(df[[nm_col]]), evtab$key)
    ev90 <- evtab$ev90[idx]
  }

  # computed rates (glossary formulas)
  contact_pct <- pdiv(contact_n, swing_n)          # Contact%
  whiff_pct   <- pdiv(miss_n,    swing_n)           # Miss%/Whiff%
  chase_pct   <- pdiv(chase_n,   ooz_n)             # Chase%
  # Z-Contact% / Z-Whiff% need an in-zone filtered pull; not yet
  # confirmed for this site, so they fall back to overall contact/
  # whiff (the grade still forms, just without the zone split).
  zcon   <- contact_pct
  zwhiff <- whiff_pct

  # section distributions from qualified hitters only
  q <- is.finite(pa) & pa >= .MM_GRADE_MIN_PA
  ms <- list(
    xwoba  = .blend_ms(xwoba[q],       .BAT_GRADE_PRIORS$xwoba),
    zcon   = .blend_ms(zcon[q],        .BAT_GRADE_PRIORS$zcon),
    whiff  = .blend_ms(whiff_pct[q],   .BAT_GRADE_PRIORS$whiff),
    zwhiff = .blend_ms(zwhiff[q],      .BAT_GRADE_PRIORS$zwhiff),
    chase  = .blend_ms(chase_pct[q],   .BAT_GRADE_PRIORS$chase),
    ev90   = .blend_ms(ev90[q],        .BAT_GRADE_PRIORS$ev90))

  z <- function(v, key) (v - ms[[key]]$m) / ms[[key]]$s

  z_xwoba  <- z(xwoba,     "xwoba")
  z_zcon   <- z(zcon,      "zcon")
  z_whiff  <- z(whiff_pct, "whiff")
  z_zwhiff <- z(zwhiff,    "zwhiff")
  z_chase  <- z(chase_pct, "chase")
  z_ev90   <- z(ev90,      "ev90")

  # Contact  = z-contact(+) , whiff(-) , z-whiff(-)
  # Power    = 90th EV(+)   , xwOBA(+)   [EV90 from pitch data]
  # Decision = chase(-)                  [Edge-Take% dropped]
  cavg <- function(...) {
    m <- rowMeans(cbind(...), na.rm = TRUE)
    m[is.nan(m)] <- NA_real_
    m
  }
  contact_z  <- cavg(z_zcon, -z_whiff, -z_zwhiff)
  power_z    <- cavg(z_ev90, z_xwoba)
  decision_z <- cavg(-z_chase)

  mk <- function(zz) vapply(zz, function(x) .grade_from_z(x, 1),
                            numeric(1))
  contact  <- mk(contact_z)
  power    <- mk(power_z)
  decision <- mk(decision_z)
  aggregate <- vapply(seq_along(contact), function(i) {
    parts <- c(contact[i], power[i], decision[i])
    parts <- parts[is.finite(parts)]
    if (length(parts) == 0) NA_real_ else round(mean(parts))
  }, numeric(1))

  out <- data.frame(
    key       = .tm_norm(df[[nm_col]]),
    pa        = pa,
    contact   = contact,
    power     = power,
    decision  = decision,
    aggregate = aggregate,
    stringsAsFactors = FALSE)
  .tm_env$bat_grade_cache[[ck]] <- out
  out
}

# One batter's grade row (name + team-aware). NULL if not found /
# below the PA floor.
tm_batter_grades <- function(name, team_disp, pool = NULL) {
  g <- tryCatch(tm_batter_grades_team(team_disp, pool = pool),
                error = function(e) NULL)
  if (is.null(g) || (length(g) == 1 && is.na(g))) return(NULL)
  k <- .tm_norm(name)
  r <- g[g$key == k, , drop = FALSE]
  if (nrow(r) == 0) {
    parts <- strsplit(k, " ")[[1]]
    if (length(parts) >= 2) {
      last <- parts[length(parts)]; fi <- substr(parts[1], 1, 1)
      r <- g[grepl(paste0("\\b", last, "\\b"), g$key) &
               substr(g$key, 1, 1) == fi, , drop = FALSE]
    }
  }
  if (nrow(r) == 0) return(NULL)
  r <- r[1, , drop = FALSE]
  if (!is.finite(r$pa) || r$pa < .MM_GRADE_MIN_PA) {
    r$contact <- r$power <- r$decision <- r$aggregate <- NA_real_
  }
  r
}

# grade -> pill color on the matrix red/cream/green scale (100 = avg).
# reuse matchup_score_color by mapping 40-160 onto its 0-100 domain.
grade_pill_color <- function(v) {
  if (!is.finite(v)) return("#EDEFF1")
  matchup_score_color(max(0, min(100, (v - 40) / 120 * 100)))
}

# ---- pitch-level pulls: 2026 NCAA data source ---------------------
# TeamGames -> gameId list -> GamePitchesTrackman (chunked, max 100
# ids per call) -> map to TrackMan column conventions so the frame
# drops straight into prep_ncaa_pitcher(). Chunk pulls are cached by
# URL, so every other pitcher on the same team is free afterward.

TM_PITCHTYPE_MAP <- c(FA="Fastball", FF="Fastball", FT="Sinker", SI="Sinker",
  TS="Sinker", SL="Slider", ST="Sweeper", SW="Sweeper", SV="Slurve",
  CU="Curveball", CB="Curveball", KC="Knuckle Curve", CH="ChangeUp",
  FC="Cutter", CT="Cutter", FS="Splitter", SP="Splitter", KN="Knuckleball",
  EP="Eephus", SC="Screwball", UN="Other")

TM_PITCHCALL_MAP <- c(B="BallCalled", BD="BallCalled", BI="BallIntentional",
  SL="StrikeCalled", SC="StrikeCalled", CS="StrikeCalled",
  SS="StrikeSwinging", SW="StrikeSwinging",
  F="FoulBall", FT="FoulTip", FB="FoulBall",
  IP="InPlay", HBP="HitByPitch", HB="HitByPitch")

TM_PLAYRESULT_MAP <- c(S="Single", `1B`="Single", D="Double", `2B`="Double",
  T="Triple", `3B`="Triple", HR="HomeRun", IP_OUT="Out", OUT="Out",
  E="Error", ROE="Error", FC="FieldersChoice", SAC="Sacrifice",
  SH="Sacrifice", SF="Sacrifice", DP="Out", TP="Out")

# pxNorm/pzNorm are zone-normalized (+/-1 = zone edge); convert to
# feet on the TrackMan scale. Tune here if locations look shifted.
TM_ZONE_HALF_W  <- 0.83
TM_ZONE_CTR_Z   <- 2.425
TM_ZONE_HALF_H  <- 0.94

tm_team_games <- function(team_id) {
  g <- tm_get_csv("TeamGames", params = list(seasonYear = tm_season(),
                                             teamId = team_id))
  if (is.null(g)) g <- tm_get_csv("TeamGames",
                                  params = list(seasonYear = tm_season(),
                                                team = team_id))
  g
}

tm_to_trackman <- function(d) {
  gc  <- function(nm) if (nm %in% names(d)) d[[nm]] else rep(NA, nrow(d))
  chr <- function(x) as.character(x)
  num <- function(x) suppressWarnings(as.numeric(x))
  lgl <- function(x) tolower(chr(x)) %in% c("true", "t", "1", "yes")
  game_key <- dplyr::coalesce(chr(gc("trackmanGameID")),
                              chr(gc("trackmanGameId")), chr(gc("gameId")))
  res <- data.frame(
    GameID          = game_key,
    PitchUID        = chr(gc("trackmanPitchUID")),
    Date            = as.Date(substr(chr(gc("gameDate")), 1, 10)),
    Inning          = num(gc("inning")),
    `Top.Bottom`    = ifelse(chr(gc("side")) == "T", "Top", "Bottom"),
    PAofInning      = num(gc("abNumInSide")),
    PitchofPA       = num(gc("pitchNumInAB")),
    Balls           = num(gc("balls")),
    Strikes         = num(gc("strikes")),
    Outs            = num(gc("outs")),
    Pitcher         = chr(gc("pitcherName")),
    PitcherId       = chr(gc("pitcherId")),
    PitcherThrows   = c(L = "Left", R = "Right")[toupper(substr(chr(gc("pitcherHand")), 1, 1))],
    Batter          = chr(gc("batterName")),
    BatterSide      = c(L = "Left", R = "Right")[toupper(substr(chr(gc("batterHand")), 1, 1))],
    RelSpeed        = num(gc("releaseVelocity")),
    SpinRate        = num(gc("spinRate")),
    SpinAxis        = num(gc("spinDir")),
    InducedVertBreak= num(gc("inducedVertBreak")),
    HorzBreak       = num(gc("horzBreak")),
    VertApprAngle   = num(gc("vertApprAngle")),
    HorzApprAngle   = num(gc("horzApprAngle")),
    Extension       = num(gc("extension")),
    # TruMedia x0 uses the opposite sign convention from TrackMan
    # RelSide (their RHPs come back negative) — negate to match.
    RelSide         = -num(gc("x0")),
    RelHeight       = num(gc("z0")),
    PlateLocSide    = num(gc("pxNorm")) * TM_ZONE_HALF_W,
    PlateLocHeight  = TM_ZONE_CTR_Z + num(gc("pzNorm")) * TM_ZONE_HALF_H,
    ExitSpeed       = num(gc("exitVelocity")),
    Angle           = num(gc("launchAngle")),
    ManOnFirst      = lgl(gc("manOnFirst")),
    ManOnSecond     = lgl(gc("manOnSecond")),
    ManOnThird      = lgl(gc("manOnThird")),
    SBAttempt       = lgl(gc("stolenBaseAttempt2B")) |
                      lgl(gc("stolenBaseAttempt3B")) |
                      lgl(gc("stolenBaseAttemptHome")),
    SBSuccess       = lgl(gc("stolenBase2B")) |
                      lgl(gc("stolenBase3B")) |
                      lgl(gc("stolenBaseHome")),
    BattingTeamRuns = num(gc("battingTeamRuns")),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  pt <- toupper(chr(gc("pitchType")))
  res$TaggedPitchType <- dplyr::coalesce(unname(TM_PITCHTYPE_MAP[pt]), "Other")
  pr <- toupper(chr(gc("pitchResult")))
  ab <- toupper(chr(gc("atBatResult")))
  res$PitchCall <- dplyr::coalesce(unname(TM_PITCHCALL_MAP[pr]), "Undefined")
  # K / BB land on the final pitch of each plate appearance
  grp  <- paste(game_key, res$Inning, res$`Top.Bottom`, res$PAofInning)
  last <- suppressWarnings(
    stats::ave(res$PitchofPA, grp,
               FUN = function(z) as.numeric(z == max(z, na.rm = TRUE)))) == 1
  last[is.na(last)] <- FALSE
  res$KorBB <- ifelse(last & ab %in% c("K", "KS", "KL", "SO"), "Strikeout",
               ifelse(last & ab %in% c("BB", "W", "IBB"), "Walk", "Undefined"))
  res$PlayResult <- ifelse(res$PitchCall == "InPlay",
                           dplyr::coalesce(unname(TM_PLAYRESULT_MAP[ab]), "Out"),
                           "Undefined")
  res
}

# Fetch pitches for a set of gameIds; if the backend 500s on a list,
# bisect down to single games and skip only the genuinely broken ones.
# ---- Supplemental per-event release angles -----------------------
# VertRelAngle|EVENT / HorzRelAngle|EVENT are event (= pitch) granularity
# stat tokens, but they live in TruMedia's stats-query system, not in
# GamePitchesTrackman -- which is why some game feeds arrive without
# angles. This probes candidate event-log endpoints ONCE per session
# (self-disabling, like the pitchtype_ok pattern). On success, thin
# frames get REAL measured angles and the geometric imputers drop to
# fallback duty. On failure it logs once and never retries.
tm_fetch_event_angles <- function(ids) {
  if (isFALSE(.tm_env$evt_ang_ok)) return(NULL)
  eps <- .tm_env$evt_ang_ep %||%
    c("PitchLog", "EventLog", "Events", "PlayerEvents", "GameEvents")
  for (ep in eps) {
    out <- tryCatch(
      tm_get_csv(ep, params = list(
        gameId  = paste(ids, collapse = ","),
        format  = "RAW",
        columns = "[VertRelAngle|EVENT],[HorzRelAngle|EVENT]")),
      error = function(e) NULL)
    if (!is.null(out) && nrow(out) > 0) {
      if (is.null(.tm_env$evt_ang_ep)) {
        cat("TruMedia: event release angles available via '", ep,
            "' - response cols: ",
            paste(utils::head(names(out), 20), collapse = ", "), "\n", sep = "")
        .tm_env$evt_ang_ep <- ep
      }
      return(out)
    }
  }
  if (is.null(.tm_env$evt_ang_ep)) {
    .tm_env$evt_ang_ok <- FALSE
    cat("TruMedia: no event-level release-angle endpoint found -",
        "geometric imputation stays primary\n")
  }
  NULL
}

# Merge event angles into a pulled per-pitch frame, matching on
# GameID + Inning + PAofInning + PitchofPA. Best effort: any missing
# identifier in the response aborts the merge (imputers cover the gap).
.tm_merge_event_angles <- function(res, ids) {
  has_real <- "VertRelAngle" %in% names(res) &&
    any(is.finite(suppressWarnings(as.numeric(res$VertRelAngle))))
  if (has_real) return(res)
  ang <- tm_fetch_event_angles(ids)
  if (is.null(ang)) return(res)
  vc <- .tm_pick_col(ang, c("vertRelAngle", "VertRelAngle"))
  hc <- .tm_pick_col(ang, c("horzRelAngle", "HorzRelAngle"))
  gi <- .tm_pick_col(ang, c("trackmanGameId", "^gameId$", "game.?id"))
  ii <- .tm_pick_col(ang, c("^inning$", "Inning"))
  pa <- .tm_pick_col(ang, c("paOfInning", "PAofInning", "pa.?of.?inning"))
  pp <- .tm_pick_col(ang, c("pitchOfPa", "PitchofPA", "pitch.?of.?pa"))
  if (any(vapply(list(vc, hc, gi, ii, pa, pp), is.null, logical(1)))) {
    cat("TruMedia: event-angle response lacks joinable pitch ids -",
        "skipping merge (cols:",
        paste(utils::head(names(ang), 15), collapse = ", "), ")\n")
    return(res)
  }
  norm_n <- function(x) {
    v <- suppressWarnings(as.numeric(as.character(x)))
    ifelse(is.na(v), trimws(as.character(x)), as.character(v))
  }
  key_a <- paste(trimws(as.character(ang[[gi]])), norm_n(ang[[ii]]),
                 norm_n(ang[[pa]]), norm_n(ang[[pp]]), sep = "|")
  key_r <- paste(trimws(as.character(res$GameID)), norm_n(res$Inning),
                 norm_n(res$PAofInning), norm_n(res$PitchofPA), sep = "|")
  m <- match(key_r, key_a)
  if (sum(!is.na(m)) == 0) {
    cat("TruMedia: event-angle keys did not match pitch rows - skipping merge\n")
    return(res)
  }
  res$VertRelAngle <- suppressWarnings(as.numeric(ang[[vc]][m]))
  res$HorzRelAngle <- suppressWarnings(as.numeric(ang[[hc]][m]))
  cat("TruMedia: merged measured release angles for", sum(!is.na(m)),
      "of", nrow(res), "pitches\n")
  res
}

tm_fetch_pitch_chunk <- function(ids) {
  p <- tm_get_csv("GamePitchesTrackman",
                  params = list(gameId = paste(ids, collapse = ",")))
  if (!is.null(p)) return(p)
  if (length(ids) == 1) return(NULL)
  mid <- ceiling(length(ids) / 2)
  parts <- Filter(function(x) !is.null(x) && nrow(x) > 0,
                  list(tm_fetch_pitch_chunk(ids[1:mid]),
                       tm_fetch_pitch_chunk(ids[(mid + 1):length(ids)])))
  if (length(parts) == 0) return(NULL)
  dplyr::bind_rows(parts)
}

# Opponent DISPLAY NAME per TeamGames row. TeamGames feeds vary: some
# carry a real name column, some only opponentTeamId — and a naive
# regex pick grabs the Id column and puts a bare number in the game
# labels. Rules:
#   1) among opp* columns, prefer name-like ones (fullName > location
#      > name > abbrev) and NEVER an *Id column
#   2) sanity-check the values — if they're mostly numeric it's an ID
#      in disguise, reject it
#   3) if only IDs exist, translate them through AllTeams
#      (teamId -> location/fullName)
# Returns a character vector aligned to rows of g, or NULL.
.tm_opp_display <- function(g) {
  if (is.null(g) || nrow(g) == 0) return(NULL)
  looks_numeric <- function(v) {
    v <- as.character(v); v <- v[!is.na(v) & nzchar(v)]
    if (length(v) == 0) return(TRUE)
    mean(grepl("^[0-9.]+$", v)) > 0.5
  }
  cand  <- grep("opp", names(g), ignore.case = TRUE, value = TRUE)
  idish <- grepl("id$", cand, ignore.case = TRUE)
  namey <- cand[!idish]
  if (length(namey) > 0) {
    sc <- vapply(tolower(namey), function(n) {
      if (grepl("fullname", n)) 1L else if (grepl("location", n)) 2L
      else if (grepl("abbrev", n)) 4L else if (grepl("name", n)) 3L else 5L
    }, integer(1))
    for (col in namey[order(sc)]) {
      if (!looks_numeric(g[[col]])) return(as.character(g[[col]]))
    }
  }
  # only IDs available -> translate via AllTeams
  idcand <- cand[idish]
  if (length(idcand) > 0) {
    teams <- tryCatch(tm_all_teams(), error = function(e) NULL)
    if (!is.null(teams) && nrow(teams) > 0) {
      tid <- .tm_pick_col(teams, c("^teamId$", "team.?id", "^id$"))
      tnm <- intersect(c("location", "fullName"), names(teams))[1]
      if (!is.null(tid) && !is.na(tnm)) {
        idmap <- setNames(as.character(teams[[tnm]]),
                          as.character(teams[[tid]]))
        v <- unname(idmap[as.character(g[[idcand[1]]])])
        if (any(!is.na(v) & nzchar(v))) return(v)
      }
    }
  }
  cat("TeamGames: no usable opponent name column [",
      paste(head(names(g), 12), collapse = ", "), "]\n")
  NULL
}

tm_load_pitcher_pitches <- function(display_name, team_disp) {
  if (!tm_enabled()) return(NULL)
  if (is.null(team_disp) || !nzchar(team_disp)) return(NULL)
  tid <- tm_find_team_id(team_disp)
  if (is.null(tid)) return(NULL)
  g <- tm_team_games(tid)
  if (is.null(g)) return(NULL)
  idc <- .tm_pick_col(g, c("^gameId$", "game.?id"))
  if (is.null(idc)) {
    .tm_env$last_error <- paste0("TeamGames: no gameId column [",
                                 paste(head(names(g), 6), collapse = ", "), "]")
    return(NULL)
  }
  # only games with TrackMan coverage have pitch data — skip the rest
  tmc <- .tm_pick_col(g, c("trackmanGameId", "trackmanGameID"))
  if (!is.null(tmc)) {
    tracked <- !is.na(g[[tmc]]) & nzchar(as.character(g[[tmc]]))
    if (any(tracked)) g <- g[tracked, , drop = FALSE]
  }
  ids <- unique(stats::na.omit(g[[idc]]))
  if (length(ids) == 0) return(NULL)
  chunks <- split(ids, ceiling(seq_along(ids) / 20))
  tgt <- .tm_norm(display_name)
  parts <- strsplit(tgt, " ")[[1]]
  out <- list()
  for (ch in chunks) {
    p <- tm_fetch_pitch_chunk(ch)
    Sys.sleep(0.25)   # sequential + polite, per TruMedia guidance
    if (is.null(p) || nrow(p) == 0) next
    nmc <- .tm_pick_col(p, c("pitcherName"))
    if (is.null(nmc)) next
    nn <- .tm_norm(p[[nmc]])
    keep <- nn == tgt
    if (!any(keep, na.rm = TRUE) && length(parts) >= 2) {
      lastn <- parts[length(parts)]; fi <- substr(parts[1], 1, 1)
      keep <- grepl(paste0("\\b", lastn, "\\b"), nn) & substr(nn, 1, 1) == fi
    }
    if (any(keep, na.rm = TRUE))
      out[[length(out) + 1]] <- p[which(keep), , drop = FALSE]
  }
  if (length(out) == 0) {
    .tm_env$last_error <- paste0("no TruMedia pitches found for '",
                                 display_name, "' in ", length(ids),
                                 " tracked games")
    return(NULL)
  }
  res <- tm_to_trackman(dplyr::bind_rows(out))
  # Label 2026 API games with TruMedia's OWN opponent name (the Games
  # picker was matching TrackMan team codes that these rows don't have
  # and showing "vs Unknown"). TeamGames is already in hand — resolve
  # an opponent display name per game and stamp it into BatterTeam,
  # which the picker reads. prettify_team() passes unknown strings
  # through verbatim, so the TruMedia name shows as-is.
  opp_disp <- .tm_opp_display(g)
  if (!is.null(opp_disp)) {
    keys <- character(0); vals <- character(0)
    for (kc in unique(c(idc, tmc))) {
      if (is.null(kc) || !kc %in% names(g)) next
      kk <- as.character(g[[kc]])
      ok <- !is.na(kk) & nzchar(kk)
      keys <- c(keys, kk[ok])
      vals <- c(vals, opp_disp[ok])
    }
    if (length(keys) > 0) {
      opp_map <- setNames(vals, keys)
      hit <- unname(opp_map[as.character(res$GameID)])
      res$BatterTeam <- ifelse(!is.na(hit) & nzchar(hit), hit, NA_character_)
    }
  }
  # thin feeds: try to backfill measured release angles at event
  # granularity before the geometric imputers have to
  res <- tryCatch(.tm_merge_event_angles(res, ids), error = function(e) res)

  # pitcher's team on every row — downstream team hints (scouting PDF
  # header, bio lookups) read PitcherTeam
  res$PitcherTeam <- team_disp
  res
}

# ---- TruMedia: every 2026 pitch a BATTER has faced -------------------------
# Mirrors tm_load_pitcher_pitches: same TeamGames -> chunked
# GamePitchesTrackman flow (chunk pulls are cached by URL, so a team whose
# pitchers were already loaded costs no extra API calls), filtered on
# batterName instead of pitcherName. The batter's team lands in BatterTeam
# and the per-game opponent (the staff he faced) in PitcherTeam.
tm_load_batter_pitches <- function(display_name, team_disp) {
  # Every early return here used to be silent, so a batter that never
  # reached the API logged "TruMedia unavailable ( no detail )" and fell
  # through to parquet. Record the reason instead.
  if (!tm_enabled()) {
    .tm_env$last_error <- "TruMedia disabled (no API key)"
    return(NULL)
  }
  if (is.null(team_disp) || !nzchar(team_disp %||% "")) {
    .tm_env$last_error <- paste0("no current team resolved for batter '",
                                 display_name, "' - cannot query TruMedia")
    return(NULL)
  }
  tid <- tm_find_team_id(team_disp)
  if (is.null(tid)) {
    .tm_env$last_error <- paste0("TruMedia has no team id for '", team_disp, "'")
    return(NULL)
  }
  g <- tm_team_games(tid)
  if (is.null(g)) {
    .tm_env$last_error <- paste0("TeamGames returned nothing for '", team_disp, "'")
    return(NULL)
  }
  idc <- .tm_pick_col(g, c("^gameId$", "game.?id"))
  if (is.null(idc)) {
    .tm_env$last_error <- paste0("TeamGames: no gameId column [",
                                 paste(head(names(g), 6), collapse = ", "), "]")
    return(NULL)
  }
  tmc <- .tm_pick_col(g, c("trackmanGameId", "trackmanGameID"))
  if (!is.null(tmc)) {
    tracked <- !is.na(g[[tmc]]) & nzchar(as.character(g[[tmc]]))
    if (any(tracked)) g <- g[tracked, , drop = FALSE]
  }
  ids <- unique(stats::na.omit(g[[idc]]))
  if (length(ids) == 0) {
    .tm_env$last_error <- paste0("no tracked games for '", team_disp, "'")
    return(NULL)
  }
  chunks <- split(ids, ceiling(seq_along(ids) / 20))
  tgt <- .tm_norm(display_name)
  parts <- strsplit(tgt, " ")[[1]]
  out <- list()
  for (ch in chunks) {
    p <- tm_fetch_pitch_chunk(ch)
    Sys.sleep(0.25)   # sequential + polite, per TruMedia guidance
    if (is.null(p) || nrow(p) == 0) next
    nmc <- .tm_pick_col(p, c("batterName"))
    if (is.null(nmc)) next
    nn <- .tm_norm(p[[nmc]])
    keep <- nn == tgt
    if (!any(keep, na.rm = TRUE) && length(parts) >= 2) {
      lastn <- parts[length(parts)]; fi <- substr(parts[1], 1, 1)
      keep <- grepl(paste0("\\b", lastn, "\\b"), nn) & substr(nn, 1, 1) == fi
    }
    if (any(keep, na.rm = TRUE))
      out[[length(out) + 1]] <- p[which(keep), , drop = FALSE]
  }
  if (length(out) == 0) {
    .tm_env$last_error <- paste0("no TruMedia pitches found for batter '",
                                 display_name, "' in ", length(ids),
                                 " tracked games")
    return(NULL)
  }
  res <- tm_to_trackman(dplyr::bind_rows(out))
  # per-game opponent name -> the staff this hitter was facing
  opp_disp <- .tm_opp_display(g)
  if (!is.null(opp_disp)) {
    keys <- character(0); vals <- character(0)
    for (kc in unique(c(idc, tmc))) {
      if (is.null(kc) || !kc %in% names(g)) next
      kk <- as.character(g[[kc]])
      ok <- !is.na(kk) & nzchar(kk)
      keys <- c(keys, kk[ok])
      vals <- c(vals, opp_disp[ok])
    }
    if (length(keys) > 0) {
      opp_map <- setNames(vals, keys)
      hit <- unname(opp_map[as.character(res$GameID)])
      res$PitcherTeam <- ifelse(!is.na(hit) & nzchar(hit), hit, NA_character_)
    }
  }
  res <- tryCatch(.tm_merge_event_angles(res, ids), error = function(e) res)
  res$BatterTeam <- team_disp
  res
}

# ---- player bio lookup (bioinfo.csv: jersey, class, throws, ht) ----
.bio_env <- new.env(parent = emptyenv())
load_bioinfo <- function() {
  if (!is.null(.bio_env$df)) return(.bio_env$df)
  cand <- c("bioinfo.csv", "reference/bioinfo.csv", "./bioinfo.csv", "/code/bioinfo.csv",
            file.path(getwd(), "bioinfo.csv"),
            "data/bioinfo.csv", "/data/bioinfo.csv", "www/bioinfo.csv",
            list.files(".", pattern = "(?i)^bio.?info.*\\.csv$",
                       full.names = TRUE, ignore.case = TRUE))
  path <- Filter(file.exists, unique(cand))
  if (length(path) == 0) {
    cat("bioinfo.csv not found (wd =", getwd(),
        ") - jersey/class/throws will be blank\n")
    return(NULL)
  }
  b <- tryCatch(utils::read.csv(path[1], stringsAsFactors = FALSE,
                                check.names = FALSE),
                error = function(e) NULL)
  if (!is.null(b) && "playerFullName" %in% names(b)) {
    b$norm_name <- .tm_norm(b$playerFullName)
    cat("bioinfo loaded:", nrow(b), "players from", path[1], "\n")
  } else {
    cat("bioinfo read failed or missing playerFullName column\n")
  }
  .bio_env$df <- b
  b
}

bio_lookup <- function(display_name, team_disp = NULL) {
  b <- load_bioinfo()
  if (is.null(b) || is.null(b$norm_name)) return(NULL)
  hit <- b[b$norm_name == .tm_norm(display_name), , drop = FALSE]
  if (nrow(hit) > 1 && !is.null(team_disp) && "newestTeamName" %in% names(hit)) {
    tn <- .tm_norm(team_disp)
    th <- hit[grepl(tn, .tm_norm(hit$newestTeamName), fixed = TRUE) |
                vapply(.tm_norm(hit$newestTeamName), function(z)
                  nzchar(z) && grepl(z, tn, fixed = TRUE), logical(1)), ,
              drop = FALSE]
    if (nrow(th) > 0) hit <- th
  }
  if (nrow(hit) == 0) return(NULL)
  as.list(hit[1, ])
}


# Generalized harmonizer: auto-detects type conflicts across frames
# and coerces every column to a single consistent type.
# ---- Defensive Date coercion ----------------------------------------
# A Date column does not always arrive as an atomic vector. A partially
# failed Arrow read, a struct column, or bind_rows() over frames whose
# Date types disagree can all leave a list-column or a nested data.frame
# column behind. dplyr::filter() then dies with
#   comparison (>=) is possible only for atomic and list types
# which points at the comparison rather than at the real problem. This
# flattens whatever it is given down to a plain Date vector, handling the
# formats that actually show up in these feeds: ISO strings, US-style
# m/d/Y, and numeric serials (both Excel's 1899 epoch and Unix days).
safe_as_date <- function(x) {
  # backstop shared by every path: a year outside anything this app could
  # hold is a misparse, not data. Better NA (the row drops out of a date
  # filter) than a silent wrong date that silently drops it anyway.
  sane <- function(d) {
    yr <- suppressWarnings(as.integer(format(d, "%Y")))
    d[!is.na(yr) & (yr < 1900 | yr > 2100)] <- NA
    d
  }
  if (is.null(x)) return(as.Date(character(0)))
  if (inherits(x, "Date")) return(x)
  if (is.data.frame(x)) x <- x[[1]]
  if (is.matrix(x)) x <- x[, 1]
  if (is.list(x)) {
    x <- vapply(x, function(e) {
      if (length(e) == 0 || is.null(e)) NA_character_ else as.character(e)[1]
    }, character(1))
  }
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(sane(as.Date(x)))
  if (is.numeric(x)) {
    v <- as.numeric(x)
    # Two serial conventions show up in these feeds and their ranges
    # overlap for modern dates, so split at the Unix epoch's Excel serial
    # (25569 = 1970-01-01). Excel serials for any season this app covers
    # sit well above it (~46000); Unix epoch days sit well below (~20500).
    return(sane(as.Date(ifelse(is.finite(v) & v >= 25569, v - 25569, v),
                        origin = "1970-01-01")))
  }
  s <- trimws(as.character(x))
  s[!nzchar(s) | s %in% c("NA", "NaN", "NULL")] <- NA_character_
  # Never call as.Date() without a format here. Base R resolves ambiguity
  # by trying "%Y-%m-%d" then "%Y/%m/%d", so "3/5/2026" does not come back
  # NA — it silently parses as year 3, month 5, day 20. That sails through
  # an is.na() check and then quietly drops every row from a date-range
  # filter. Pick the format from the shape of the string instead.
  out <- as.Date(rep(NA_character_, length(s)))
  fmt_for <- function(v) {
    ifelse(grepl("^\\d{4}-\\d{1,2}-\\d{1,2}", v),   "%Y-%m-%d",
    ifelse(grepl("^\\d{4}/\\d{1,2}/\\d{1,2}", v),   "%Y/%m/%d",
    ifelse(grepl("^\\d{1,2}/\\d{1,2}/\\d{4}", v),   "%m/%d/%Y",
    ifelse(grepl("^\\d{1,2}/\\d{1,2}/\\d{2}\\b", v), "%m/%d/%y",
    ifelse(grepl("^\\d{1,2}-\\d{1,2}-\\d{4}", v),   "%m-%d-%Y",
    ifelse(grepl("^\\d{8}$", v),                    "%Y%m%d", NA_character_))))))
  }
  f <- fmt_for(s)
  for (ff in unique(stats::na.omit(f))) {
    i <- which(!is.na(f) & f == ff)
    out[i] <- suppressWarnings(as.Date(s[i], format = ff))
  }
  # anything the shape test didn't recognize gets one permissive attempt
  left <- which(is.na(out) & !is.na(s) & is.na(f))
  if (length(left)) {
    out[left] <- suppressWarnings(as.Date(s[left],
      tryFormats = c("%Y-%m-%d", "%d-%b-%Y", "%b %d %Y"), optional = TRUE))
  }
  # backstop: catches the year-3 class of misparse if one slips through
  sane(out)
}

harmonize_types <- function(frames) {
  frames <- Filter(function(d) !is.null(d) && nrow(d) > 0, frames)
  if (length(frames) == 0) return(list())

  # Priority: character wins (safest common ground for IDs/mixed cols),
  # then date/datetime, then numeric, then logical.
  pick_target_type <- function(types) {
    types <- unique(types[!is.na(types)])
    if (length(types) == 0) return("character")
    if ("character" %in% types)                 return("character")
    if (any(c("POSIXct","POSIXt") %in% types))  return("POSIXct")
    if ("Date" %in% types)                      return("Date")
    if (any(c("numeric","double","integer") %in% types)) return("numeric")
    if ("logical" %in% types)                   return("logical")
    types[1]
  }

  coerce_col <- function(x, target) {
    if (target == "character") {
      if (is.numeric(x)) {
        return(ifelse(is.na(x), NA_character_,
                      format(x, scientific = FALSE, trim = TRUE)))
      }
      return(as.character(x))
    }
    if (target == "numeric") return(suppressWarnings(as.numeric(x)))
    if (target == "logical") return(as.logical(x))
    if (target == "POSIXct") return(suppressWarnings(as.POSIXct(x)))
    if (target == "Date")    return(suppressWarnings(as.Date(x)))
    x
  }

  all_cols <- unique(unlist(lapply(frames, names)))

  targets <- setNames(
    vapply(all_cols, function(col) {
      types <- unlist(lapply(frames, function(d) {
        if (col %in% names(d)) class(d[[col]])[1] else NA_character_
      }))
      pick_target_type(types)
    }, character(1)),
    all_cols
  )

  lapply(frames, function(d) {
    for (col in intersect(names(d), names(targets))) {
      if (!identical(class(d[[col]])[1], targets[[col]])) {
        d[[col]] <- coerce_col(d[[col]], targets[[col]])
      }
    }
    d
  })
}

# ============================================================
