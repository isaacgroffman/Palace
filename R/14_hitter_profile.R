# DETAILED HITTER PROFILE (process-based)
# ----------------------------------------------------------------------------
# Replaces the single season PRC/DEC/CON/POW strip with the same four grades
# resolved two ways -- by platoon (vRHP / vLHP) and by count state (overall /
# first pitch / two strikes) -- plus the pitch types he actually punishes and
# actually struggles against.
#
# Methodology, and the reasons it is stricter than it looks:
#
#  * Every cell is priced the same way the matrix cells are: hp_rates on the
#    slice, then hp_plus against the league basis. So a 2K number and an OVR
#    number are on one scale and can be read against each other directly.
#
#  * A cell that misses HP_PROFILE_MIN_CELL pitches returns NULL and renders
#    as a dash. It does NOT fall back to the overall number. A profile whose
#    vLHP column silently repeats the vRHP column is worse than one with
#    holes in it, because the repeat looks like a finding.
#
#  * Punishes / struggles are per pitch type WITHIN a platoon side, and a type
#    must clear BOTH gates to be named: at least HP_PROFILE_MIN_TYPE pitches
#    seen, and a Process+ at least HP_PROFILE_EDGE points off 100. Anything
#    inside that band is real but unremarkable and stays off the card.
#
#  * The two lists are disjoint by construction -- one Process+ per type per
#    side, assigned to whichever side of the band it falls on. A pitch type
#    can never appear in both, which is the failure mode to watch for in any
#    hand-rolled version of this.
# ============================================================================

HP_PROFILE_MIN_CELL <- 40   # pitches before Decision+ (and the cell) is shown
HP_PROFILE_MIN_SWING <- 40  # SWINGS before Contact+ is shown
HP_PROFILE_MIN_BIP  <- 40   # BATTED BALLS before Power+ is shown
# Backstop. Even a passing gate can land out of distribution on a lopsided
# slice -- a two-strike cell with 21 batted balls and two homers priced
# Power+ at 173, which is +7 SD and simply not a thing a hitter is. Anything
# this far out is a sample artifact, not a finding, so withhold it.
HP_PROFILE_MAX_DEV  <- 45   # plus points from 100 beyond which we do not believe it
HP_PROFILE_MIN_TYPE <- 30   # pitches before a pitch type can be named
HP_PROFILE_EDGE     <- 7    # plus points off 100 to count as punish/struggle
HP_PROFILE_MAX_TAGS <- 4    # most types listed per side per list

# ---- count-specific pitch call ----
HP_CALL_MIN_N    <- 15  # pitches of that type, in that count, vs that hand
HP_CALL_EDGE     <- 12  # plus points below 100 before it is worth calling
HP_CALL_SEPARATE <- 5   # points clear of the next-worst type -- the "outlier" test

.hp_profile_cache <- new.env(parent = emptyenv())

# One grade cell from an already-scored slice.
# ----------------------------------------------------------------------------
# The three layers have DIFFERENT denominators, which is why a single
# pitch-count gate was not enough and two-strike cells were printing Process+
# of 176 and 19. Decision prices every pitch; Contact prices only swings;
# Power prices only batted balls. Forty two-strike pitches can be six batted
# balls, and Power+ off six balls in play is noise with a number on it.
# Each layer is therefore gated on its OWN sample and withheld independently.
#
# The headline requires ALL THREE to clear, because Process+ is their sum --
# it cannot be more trustworthy than its least-supported component. That makes
# 2K headlines rare, which is the correct outcome: two-strike samples ARE thin
# and the honest answer is a dash.
.hp_profile_cell <- function(d, min_n = HP_PROFILE_MIN_CELL,
                             min_swing = HP_PROFILE_MIN_SWING,
                             min_bip = HP_PROFILE_MIN_BIP) {
  if (is.null(d) || nrow(d) < min_n) return(NULL)
  r <- tryCatch(hp_rates(d), error = function(e) NULL)
  if (is.null(r) || !is.finite(r$n %||% NA_real_) || r$n < min_n) return(NULL)

  n_sw  <- if ("hp_cv" %in% names(d)) sum(is.finite(d$hp_cv)) else 0L
  n_bip <- if ("hp_pv" %in% names(d)) sum(is.finite(d$hp_pv)) else 0L
  gated <- function(v, key, ok) {
    if (!ok) return(NA_real_)
    x <- unname(as.numeric(hp_plus(v, key)))
    if (!is.finite(x) || abs(x - 100) > HP_PROFILE_MAX_DEV) NA_real_ else x
  }

  ok_dec <- r$n   >= min_n
  ok_con <- n_sw  >= min_swing
  ok_pow <- n_bip >= min_bip
  dec <- gated(r$dv, "dv", ok_dec)
  con <- gated(r$cv, "cv", ok_con)
  pow <- gated(r$pv, "pv", ok_pow)
  prc <- if (is.finite(dec) && is.finite(con) && is.finite(pow))
    gated(r$prc, "prc", TRUE) else NA_real_
  list(prc = prc, dec = dec, con = con, pow = pow,
       n = as.integer(r$n), n_swing = as.integer(n_sw), n_bip = as.integer(n_bip))
}

# ---- what to throw him in this count -----------------------------------
# Only fires on a genuine outlier: the type must clear its own sample gate,
# sit at least HP_CALL_EDGE below average, AND be at least HP_CALL_SEPARATE
# points clear of the next-worst qualifying type in the same slice. That last
# test is what stops the call being "he's mediocre on everything, pick one" --
# if two pitches are equally bad there is no call to make and we return NULL.
.hp_profile_call <- function(d, min_n = HP_CALL_MIN_N, edge = HP_CALL_EDGE,
                             sep = HP_CALL_SEPARATE) {
  if (is.null(d) || nrow(d) == 0 || !"TaggedPitchType" %in% names(d)) return(NULL)
  pts <- unique(d$TaggedPitchType[!is.na(d$TaggedPitchType) &
                                    d$TaggedPitchType != "Other"])
  if (length(pts) == 0) return(NULL)
  rows <- lapply(pts, function(pt) {
    s <- d[!is.na(d$TaggedPitchType) & d$TaggedPitchType == pt, , drop = FALSE]
    if (nrow(s) < min_n) return(NULL)
    r <- tryCatch(hp_rates(s), error = function(e) NULL)
    if (is.null(r) || r$n < min_n) return(NULL)
    v <- unname(as.numeric(hp_plus(r$prc, "prc")))
    # same out-of-distribution backstop the grade cells use: a call built on
    # an implausible number is worse than no call
    if (!is.finite(v) || abs(v - 100) > HP_PROFILE_MAX_DEV) return(NULL)
    data.frame(pt = pt, prc = v, n = as.integer(r$n), stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(NULL)
  tab <- do.call(rbind, rows)
  tab <- tab[order(tab$prc), , drop = FALSE]
  if (tab$prc[1] > 100 - edge) return(NULL)              # nothing bad enough
  if (nrow(tab) > 1 && (tab$prc[2] - tab$prc[1]) < sep)   # not an outlier
    return(NULL)
  list(pt = tab$pt[1], prc = tab$prc[1], n = tab$n[1],
       margin = if (nrow(tab) > 1) tab$prc[2] - tab$prc[1] else NA_real_,
       alone = nrow(tab) == 1)
}

# Punishes / struggles for one platoon slice. Returns two disjoint frames.
.hp_profile_types <- function(d, min_n = HP_PROFILE_MIN_TYPE,
                              edge = HP_PROFILE_EDGE,
                              max_tags = HP_PROFILE_MAX_TAGS) {
  none <- list(punishes = NULL, struggles = NULL)
  if (is.null(d) || nrow(d) == 0 || !"TaggedPitchType" %in% names(d)) return(none)
  pts <- unique(d$TaggedPitchType[!is.na(d$TaggedPitchType) &
                                    d$TaggedPitchType != "Other"])
  if (length(pts) == 0) return(none)
  rows <- lapply(pts, function(pt) {
    s <- d[!is.na(d$TaggedPitchType) & d$TaggedPitchType == pt, , drop = FALSE]
    if (nrow(s) < min_n) return(NULL)          # sample gate, no exceptions
    r <- tryCatch(hp_rates(s), error = function(e) NULL)
    if (is.null(r) || r$n < min_n) return(NULL)
    v <- unname(as.numeric(hp_plus(r$prc, "prc")))
    if (!is.finite(v)) return(NULL)
    data.frame(pt = pt, prc = v, n = as.integer(r$n), stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(none)
  tab <- do.call(rbind, rows)
  pun <- tab[tab$prc >= 100 + edge, , drop = FALSE]
  str <- tab[tab$prc <= 100 - edge, , drop = FALSE]
  pun <- pun[order(-pun$prc), , drop = FALSE]
  str <- str[order(str$prc), , drop = FALSE]
  list(punishes  = if (nrow(pun)) utils::head(pun, max_tags) else NULL,
       struggles = if (nrow(str)) utils::head(str, max_tags) else NULL)
}

hp_hitter_profile <- function(pool, batter, batter_team = NULL) {
  if (is.null(pool) || !is.data.frame(pool) || nrow(pool) == 0 ||
      !"Batter" %in% names(pool)) return(NULL)
  ck <- paste0(gsub("[^A-Za-z0-9]", "_", batter), "::",
               batter_team %||% "", "::", nrow(pool))
  hit <- .hp_profile_cache[[ck]]
  if (!is.null(hit)) return(if (identical(hit, "none")) NULL else hit)
  .none <- function() { .hp_profile_cache[[ck]] <- "none"; NULL }

  sub <- pool[!is.na(pool$Batter) & pool$Batter == batter, , drop = FALSE]
  if (!is.null(batter_team) && nzchar(batter_team) && "BatterTeam" %in% names(sub))
    sub <- sub[!is.na(sub$BatterTeam) & sub$BatterTeam == batter_team, , drop = FALSE]
  if (nrow(sub) < HP_PROFILE_MIN_CELL) return(.none())

  scored <- tryCatch(hp_score_pitches(sub), error = function(e) NULL)
  if (is.null(scored) || !"hp_dv" %in% names(scored)) return(.none())
  if (!hp_build_league()) return(.none())
  scored <- scored[is.finite(scored$hp_dv), , drop = FALSE]
  if (nrow(scored) < HP_PROFILE_MIN_CELL) return(.none())

  has_count <- all(c("Balls", "Strikes") %in% names(scored))
  # PitcherThrows arrives as "Right"/"Left" from TrackMan and "R"/"L" from
  # some TruMedia frames -- match on the initial so both work. Matching the
  # full string silently returns zero rows and every split renders as a dash.
  by_hand <- function(h) {
    if (is.null(h)) return(scored)
    if (!"PitcherThrows" %in% names(scored)) return(scored[0, , drop = FALSE])
    ini <- toupper(substr(as.character(scored$PitcherThrows), 1, 1))
    scored[!is.na(ini) & ini == h, , drop = FALSE]
  }
  side_block <- function(h) {
    d <- by_hand(h)
    fp <- if (has_count) d[d$Balls == 0 & d$Strikes == 0, , drop = FALSE] else NULL
    tk <- if (has_count) d[d$Strikes == 2, , drop = FALSE] else NULL
    tt <- .hp_profile_types(d)
    list(n = nrow(d),
         ovr = .hp_profile_cell(d),
         fp  = .hp_profile_cell(fp),
         tk  = .hp_profile_cell(tk),
         # count-specific calls: what to throw him 0-0 and with two strikes
         call_fp = .hp_profile_call(fp),
         call_tk = .hp_profile_call(tk),
         punishes = tt$punishes, struggles = tt$struggles)
  }

  out <- list(batter = batter, n = nrow(scored),
              overall = .hp_profile_cell(scored),
              R = side_block("R"), L = side_block("L"),
              min_cell = HP_PROFILE_MIN_CELL, min_type = HP_PROFILE_MIN_TYPE)
  .hp_profile_cache[[ck]] <- out
  out
}

# ============================================================================
# TEAM HITTER REPORT (Advance tab) — analysis helpers
# ----------------------------------------------------------------------------
# The hitter-side complement of the Scouting Workbook. Everything here reads
# an already-scored pitch frame (hp_score_pitches has run, so hp_dv/hp_cv/
# hp_pv/hp_rv exist) and reduces it to the cells the printed card needs.
# Every helper returns NA rather than erroring on a thin sample, because a
# card for a 9-man lineup will always have somebody with 30 pitches.
# ============================================================================

THR_MIN_SPLIT <- 15    # pitches needed before a split cell is trusted
THR_MIN_TRAIT <- 12    # pitches needed before a trait weakness is named

# ---- pitch trait membership -------------------------------------------------
# The five traits the staff calls out: Ride (carry FB), Sink (heavy FB),
# 92+ (velo), Dropper (downer breaker), Sweep (horizontal breaker).
thr_pitch_traits <- function(df) {
  n <- nrow(df)
  num <- function(cc) if (cc %in% names(df))
    suppressWarnings(as.numeric(df[[cc]])) else rep(NA_real_, n)
  velo <- num("RelSpeed"); ivb <- num("InducedVertBreak"); hb <- num("HorzBreak")
  pt <- if ("TaggedPitchType" %in% names(df))
    toupper(as.character(df$TaggedPitchType)) else rep("", n)
  pt[is.na(pt)] <- ""
  fb <- grepl("FAST|SINK|CUT", pt)
  br <- grepl("SLID|CURV|SWEEP|BREAK|SLURVE", pt)
  f <- function(x) { x[is.na(x)] <- FALSE; x }
  list(
    Ride    = f(fb & ivb >= 16),
    Sink    = f(fb & ivb <= 10),
    `92+`   = f(velo >= 92),
    Dropper = f(br & ivb <= -5),
    Sweep   = f(br & abs(hb) >= 14)
  )
}

# ---- Process+ for an arbitrary subset --------------------------------------
thr_prc <- function(sub, min_n = THR_MIN_SPLIT) {
  if (is.null(sub) || !is.data.frame(sub) || nrow(sub) < min_n) return(NA_real_)
  if (!"hp_dv" %in% names(sub)) return(NA_real_)
  r <- tryCatch(hp_rates(sub), error = function(e) NULL)
  if (is.null(r) || r$n < min_n) return(NA_real_)
  hp_plus(r$prc, "prc")
}

thr_layer_grades <- function(sub, min_n = THR_MIN_SPLIT) {
  out <- c(Decision = NA_real_, Contact = NA_real_, Power = NA_real_)
  if (is.null(sub) || !is.data.frame(sub) || nrow(sub) < min_n) return(out)
  if (!"hp_dv" %in% names(sub)) return(out)
  r <- tryCatch(hp_rates(sub), error = function(e) NULL)
  if (is.null(r) || r$n < min_n) return(out)
  c(Decision = hp_plus(r$dv, "dv"),
    Contact  = hp_plus(r$cv, "cv"),
    Power    = hp_plus(r$pv, "pv"))
}

# ---- side helpers -----------------------------------------------------------
thr_vs <- function(df, hand) {
  if (!"PitcherThrows" %in% names(df)) return(df[0, , drop = FALSE])
  df[toupper(substr(as.character(df$PitcherThrows), 1, 1)) == hand, , drop = FALSE]
}

thr_batter_side <- function(df) {
  if (!"BatterSide" %in% names(df) || !nrow(df)) return(NA_character_)
  s <- toupper(substr(as.character(df$BatterSide), 1, 1))
  s <- s[s %in% c("L", "R")]
  if (!length(s)) return(NA_character_)
  tb <- table(s)
  if (length(tb) > 1 && min(tb) / sum(tb) > 0.2) return("S")   # switch
  names(tb)[which.max(tb)]
}

# ---- positioning ------------------------------------------------------------
# Bearing is degrees off centre, negative toward left field. A RHH pulls to
# LF, a LHH pulls to RF, so which sign counts as "pull" flips with the side.
thr_positioning <- function(df, side, min_bip = 8) {
  na3 <- c(Pull = NA_real_, Center = NA_real_, Oppo = NA_real_)
  if (is.null(df) || !nrow(df) || !"Bearing" %in% names(df)) return(na3)
  b <- suppressWarnings(as.numeric(df$Bearing))
  b <- b[is.finite(b)]
  if (length(b) < min_bip) return(na3)
  center <- abs(b) <= 15
  lf <- b < -15; rf <- b > 15
  pull <- if (identical(side, "L")) rf else lf
  oppo <- if (identical(side, "L")) lf else rf
  round(100 * c(Pull = mean(pull), Center = mean(center), Oppo = mean(oppo)), 1)
}

thr_pos_string <- function(p) {
  if (all(is.na(p))) return("")
  sprintf("%s / %s / %s",
          ifelse(is.na(p[["Pull"]]),   "-", round(p[["Pull"]])),
          ifelse(is.na(p[["Center"]]), "-", round(p[["Center"]])),
          ifelse(is.na(p[["Oppo"]]),   "-", round(p[["Oppo"]])))
}

# ---- count states -----------------------------------------------------------
thr_first_pitch <- function(df) {
  if (!all(c("Balls", "Strikes") %in% names(df))) return(df[0, , drop = FALSE])
  df[df$Balls %in% 0 & df$Strikes %in% 0, , drop = FALSE]
}
thr_two_strike <- function(df) {
  if (!"Strikes" %in% names(df)) return(df[0, , drop = FALSE])
  df[df$Strikes %in% 2, , drop = FALSE]
}
thr_pre_two_strike <- function(df) {
  if (!"Strikes" %in% names(df)) return(df[0, , drop = FALSE])
  df[df$Strikes %in% c(0, 1), , drop = FALSE]
}

# Aggressiveness = swing rate in that count state, flagged against the
# hitter's own overall swing rate so "aggressive" is relative to himself.
thr_swing_pct <- function(df) {
  if (is.null(df) || !nrow(df) || !"hp_swing" %in% names(df)) return(NA_real_)
  round(100 * mean(df$hp_swing %in% TRUE, na.rm = TRUE), 1)
}
thr_agg_flag <- function(sub_pct, base_pct) {
  if (!is.finite(sub_pct) || !is.finite(base_pct)) return("")
  d <- sub_pct - base_pct
  if (d >= 8)  return("AGG")
  if (d <= -8) return("PASS")
  "even"
}

# ---- worst-group finders ----------------------------------------------------
thr_worst_group <- function(df, key, min_n = THR_MIN_SPLIT) {
  if (is.null(df) || !nrow(df) || !key %in% names(df)) return("")
  kk <- as.character(df[[key]]); kk[is.na(kk) | !nzchar(kk)] <- NA
  idx <- split(seq_len(nrow(df)), kk)
  if (!length(idx)) return("")
  sc <- vapply(names(idx), function(k) thr_prc(df[idx[[k]], , drop = FALSE], min_n),
               numeric(1))
  if (!any(is.finite(sc))) return("")
  nm <- names(sc)[which.min(sc)]
  sprintf("%s (%s)", nm, round(min(sc, na.rm = TRUE)))
}

thr_worst_trait <- function(df, min_n = THR_MIN_TRAIT) {
  if (is.null(df) || !nrow(df)) return("")
  tr <- thr_pitch_traits(df)
  sc <- vapply(names(tr), function(k) {
    sel <- tr[[k]]
    if (sum(sel) < min_n) return(NA_real_)
    thr_prc(df[sel, , drop = FALSE], min_n)
  }, numeric(1))
  if (!any(is.finite(sc))) return("")
  nm <- names(sc)[which.min(sc)]
  sprintf("%s (%s)", nm, round(min(sc, na.rm = TRUE)))
}

# ---- zone / whiff location --------------------------------------------------
# Which third of the zone he misses in. Returns e.g. "Up-In" so the staff
# can read an attack plan straight off the card.
thr_miss_zone <- function(df, min_n = 6, fastball_only = FALSE) {
  if (is.null(df) || !nrow(df)) return("")
  if (isTRUE(fastball_only)) {
    if (!"TaggedPitchType" %in% names(df)) return("")
    pt <- toupper(as.character(df$TaggedPitchType))
    fb <- grepl("FAST|SINK|CUT", pt)
    fb[is.na(fb)] <- FALSE
    df <- df[fb, , drop = FALSE]
    if (!nrow(df)) return("")
  }
  if (!"PitchCall" %in% names(df)) return("")
  wh <- as.character(df$PitchCall) == "StrikeSwinging"
  wh[is.na(wh)] <- FALSE
  w <- df[wh, , drop = FALSE]
  if (nrow(w) < min_n) return("")
  ft <- tryCatch(.hp_feet(w), error = function(e) NULL)
  if (is.null(ft)) return("")
  x <- suppressWarnings(as.numeric(ft$PlateLocSide))
  z <- suppressWarnings(as.numeric(ft$PlateLocHeight))
  ok <- is.finite(x) & is.finite(z)
  if (sum(ok) < min_n) return("")
  x <- x[ok]; z <- z[ok]
  vert <- ifelse(z >= 2.85, "Up", ifelse(z <= 2.05, "Down", "Mid"))
  side <- thr_batter_side(w)
  inside <- if (identical(side, "L")) x > 0.28 else x < -0.28
  outside <- if (identical(side, "L")) x < -0.28 else x > 0.28
  horiz <- ifelse(inside, "In", ifelse(outside, "Away", "Mid"))
  cell <- paste(vert, horiz, sep = "-")
  tb <- sort(table(cell), decreasing = TRUE)
  sprintf("%s %s%%", names(tb)[1], round(100 * tb[[1]] / length(cell)))
}

# ---- short game -------------------------------------------------------------
thr_short_game <- function(df) {
  out <- c(SBA = NA_real_, Bunts = NA_real_)
  if (is.null(df) || !nrow(df)) return(out)
  bunt <- if ("TaggedHitType" %in% names(df))
    sum(grepl("BUNT", toupper(as.character(df$TaggedHitType))), na.rm = TRUE) else NA_real_
  if (!is.finite(bunt) && "PlayResult" %in% names(df))
    bunt <- sum(grepl("BUNT", toupper(as.character(df$PlayResult))), na.rm = TRUE)
  sba <- NA_real_
  for (cc in c("PlayResult", "Notes")) {
    if (cc %in% names(df)) {
      v <- toupper(as.character(df[[cc]]))
      hit <- sum(grepl("STOLEN|CAUGHTSTEALING|CAUGHT STEALING", v), na.rm = TRUE)
      sba <- if (is.na(sba)) hit else sba + hit
    }
  }
  c(SBA = sba, Bunts = bunt)
}

# ---- reliever rest ----------------------------------------------------------
# "Rested" = no appearance within `rested_days` of the game. "Non-rested" =
# threw within `nonrested_days`. Both thresholds are user-set on the tab so
# a Friday-doubleheader week can be graded differently from a normal one.
thr_reliever_rest <- function(pool, pitchers, game_date,
                              rested_days = 3, nonrested_days = 2) {
  gd <- suppressWarnings(as.Date(game_date))
  if (is.na(gd)) gd <- Sys.Date()
  has_date <- is.data.frame(pool) && "Date" %in% names(pool) &&
    "Pitcher" %in% names(pool)
  rows <- lapply(pitchers, function(p) {
    last <- as.Date(NA)
    if (has_date) {
      d <- pool[!is.na(pool$Pitcher) & pool$Pitcher == p, , drop = FALSE]
      dt <- suppressWarnings(as.Date(d$Date))
      dt <- dt[!is.na(dt) & dt < gd]
      if (length(dt)) last <- max(dt)
    }
    rest <- if (is.na(last)) NA_integer_ else as.integer(gd - last)
    cls <- if (is.na(rest)) "Unknown"
           else if (rest >= rested_days) "Rested"
           else if (rest <= nonrested_days) "Non-Rested"
           else "Unknown"
    data.frame(pitcher = p, last_app = last, days_rest = rest,
               rest_class = cls, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# ---- one hitter's full card row --------------------------------------------
# Everything Page 1 / Page 2 print for a single bat. Scored ONCE here and
# reused across the side splits so a 9-man lineup is nine scoring passes,
# not ninety.
thr_hitter_row <- function(df, name, bio = list()) {
  base <- list(
    Hitter = name, Jersey = bio$jersey %||% "", Pos = bio$pos %||% "",
    Class = bio$class %||% "", Side = NA_character_,
    vsLHP = NA_real_, vsRHP = NA_real_,
    Contact = NA_real_, Power = NA_real_, Decisions = NA_real_,
    SBA = NA_real_, Bunts = NA_real_,
    EV90 = NA_real_, MaxEV = NA_real_,
    MissZoneR = "", MissZoneL = "", MissFBZoneR = "", MissFBZoneL = "",
    PTWeak = "", TraitWeakL = "", TraitWeakR = "",
    Agg1P = "", Agg2K = "", Pitches1P = NA_real_, Pitches2K = NA_real_,
    Swing1P = NA_real_, Swing2K = NA_real_,
    PosL = "", PosR = "", PosPre2K = "", Pos2K = "", N = 0
  )
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(base)

  scored <- tryCatch(hp_score_pitches(df), error = function(e) NULL)
  if (is.null(scored) || !"hp_dv" %in% names(scored)) return(base)
  hp_build_league()

  side <- thr_batter_side(scored)
  vsL <- thr_vs(scored, "L"); vsR <- thr_vs(scored, "R")
  lay <- thr_layer_grades(scored, min_n = 25)
  sg  <- thr_short_game(scored)
  basesw <- thr_swing_pct(scored)

  bip <- if ("hp_bip" %in% names(scored))
    scored[scored$hp_bip %in% TRUE, , drop = FALSE] else scored[0, , drop = FALSE]

  base$Side      <- side
  base$N         <- nrow(scored)
  base$vsLHP     <- thr_prc(vsL, min_n = 25)
  base$vsRHP     <- thr_prc(vsR, min_n = 25)
  base$Contact   <- unname(lay[["Contact"]])
  base$Power     <- unname(lay[["Power"]])
  base$Decisions <- unname(lay[["Decision"]])
  base$SBA       <- unname(sg[["SBA"]])
  base$Bunts     <- unname(sg[["Bunts"]])
  # EV90 / Max EV straight from the pitch-by-pitch frame (balls in play)
  if ("ExitSpeed" %in% names(scored)) {
    ipc <- if ("PitchCall" %in% names(scored))
      as.character(scored$PitchCall) == "InPlay" else rep(TRUE, nrow(scored))
    ipc[is.na(ipc)] <- FALSE
    ev <- suppressWarnings(as.numeric(scored$ExitSpeed))
    ev <- ev[ipc & is.finite(ev)]
    if (length(ev) >= 5)
      base$EV90 <- round(as.numeric(stats::quantile(ev, 0.9)), 1)
    if (length(ev) >= 1)
      base$MaxEV <- round(max(ev), 1)
  }
  base$MissZoneR <- thr_miss_zone(vsR)
  base$MissZoneL <- thr_miss_zone(vsL)
  base$MissFBZoneR <- thr_miss_zone(vsR, fastball_only = TRUE)
  base$MissFBZoneL <- thr_miss_zone(vsL, fastball_only = TRUE)
  base$PTWeak    <- thr_worst_group(scored, "TaggedPitchType", min_n = 20)
  base$TraitWeakL <- thr_worst_trait(vsL)
  base$TraitWeakR <- thr_worst_trait(vsR)
  fp <- thr_first_pitch(scored)
  tk <- thr_two_strike(scored)
  base$Pitches1P <- nrow(fp)
  base$Pitches2K <- nrow(tk)
  base$Swing1P   <- thr_swing_pct(fp)
  base$Swing2K   <- thr_swing_pct(tk)
  base$Agg1P     <- thr_agg_flag(base$Swing1P, basesw)
  base$Agg2K     <- thr_agg_flag(base$Swing2K, basesw)
  base$PosL      <- thr_pos_string(thr_positioning(thr_vs(bip, "L"), side))
  base$PosR      <- thr_pos_string(thr_positioning(thr_vs(bip, "R"), side))
  base$PosPre2K  <- thr_pos_string(thr_positioning(thr_pre_two_strike(bip), side))
  base$Pos2K     <- thr_pos_string(thr_positioning(thr_two_strike(bip), side))
  base
}

# ---- Matchup-matrix detail heatmaps ------------------------------------------
# Per-BBE xSLG via the SAME Pitch Profiler expected_stats the leaderboards
# and summaries use, grouped by PitchUID so each ball in play carries its
# own expected slug. Falls back to actual total bases on contact when the
# bundle is unavailable, so the heatmap always draws something honest.
.mm_perpitch_xslg <- function(df) {
  n <- NROW(df)
  out <- rep(NA_real_, n)
  if (n == 0 || !is.data.frame(df)) return(out)
  bip <- if ("PitchCall" %in% names(df))
    as.character(df$PitchCall) == "InPlay" else rep(FALSE, n)
  bip[is.na(bip)] <- FALSE
  if (!any(bip)) return(out)

  if ("PitchUID" %in% names(df) &&
      exists("init_pitchprofiler") && isTRUE(tryCatch(init_pitchprofiler(),
                                                      error = function(e) FALSE))) {
    xs <- tryCatch({
      tmp <- tempfile(fileext = ".parquet")
      on.exit(unlink(tmp), add = TRUE)
      keep <- intersect(c(.pp_required_cols, "PlayResult", "ExitSpeed", "Angle",
                          "KorBB", "TaggedHitType", "Stadium", "Date",
                          "PitchUID"), names(df))
      arrow::write_parquet(df[, keep, drop = FALSE], tmp)
      raw_pl <- .pp_env$pl$read_parquet(tmp)
      r <- .pp_env$mod$expected_stats(raw_pl, .pp_env$bundle,
             group_cols = reticulate::tuple("PitchUID"))
      .pp_py_to_r_df(r)
    }, error = function(e) {
      cat("  [MM xSLG] per-pitch expected_stats failed:",
          conditionMessage(e), "- falling back to TB on contact\n")
      NULL
    })
    if (!is.null(xs) && is.data.frame(xs) && nrow(xs) > 0) {
      canon <- function(x) gsub("[^a-z0-9]", "", tolower(x))
      cn <- canon(names(xs))
      uc <- which(cn == "pitchuid"); sc <- which(cn == "xslg")
      if (length(uc) && length(sc)) {
        idx <- match(as.character(df$PitchUID), as.character(xs[[uc[1]]]))
        out <- suppressWarnings(as.numeric(xs[[sc[1]]]))[idx]
      }
    }
  }
  if (all(!is.finite(out[bip]))) {
    tbmap <- c(Single = 1, Double = 2, Triple = 3, HomeRun = 4)
    pr <- if ("PlayResult" %in% names(df))
      as.character(df$PlayResult) else rep(NA_character_, n)
    tb <- unname(tbmap[pr]); tb[is.na(tb)] <- 0
    out[bip] <- tb[bip]
  }
  out[!bip] <- NA_real_
  out
}

# Small strike-zone heatmap for the detailed matrix columns:
#   kind = "miss": kernel-smoothed Miss% (whiffs per swing) by location
#   kind = "xslg": kernel-smoothed per-BBE xSLG (df$mm_xslg) by location
# hand filters PitcherThrows; keeps rendering with a "n=" note when thin.
#
# The field is sampled on a REGULAR HEX LATTICE (pointy-top). Centre spacing
# is sqrt(3)*R across and 1.5*R up, so a cell covers the same distance in x
# as it does in z -- with coord_fixed() the hexes render as true regular
# hexagons and the map is uniform on both axes.
#
# Colour is gamma-warped: t = val/hi is clamped to [0,1] then raised to
# `gamma_warp`. >1 holds the mid-range light so only the genuinely hot/cold
# pockets carry saturation; <1 floods the map. Warping the colour position
# (not the value) keeps the underlying numbers honest and keeps the L/R
# panels on the same absolute scale, so they stay comparable side by side.
#
# flip_x mirrors the pitch locations only: the zone outline and home plate
# are already drawn from the pitcher's perspective, so the locations are the
# piece that needs negating.
mm_mini_heatmap <- function(df, hand, kind = c("miss", "xslg"), min_n = 8,
                            hex_r = 0.155, bw = 0.35, gamma_warp = 1.6,
                            flip_x = TRUE) {
  kind <- match.arg(kind)

  zx <- c(-0.83, 0.83); zz <- c(1.5, 3.5)
  zone_layers <- list(
    annotate("rect", xmin = zx[1], xmax = zx[2], ymin = zz[1], ymax = zz[2],
             fill = NA, color = "black", linewidth = 0.7),
    annotate("path",
             x = c(-0.708, 0.708, 0.708, 0, -0.708, -0.708),
             y = c(0.25, 0.25, 0.4, 0.6, 0.4, 0.25),
             color = "black", linewidth = 0.45))
  base_zone <- ggplot() +
    coord_fixed(xlim = c(-1.6, 1.6), ylim = c(0, 4.3), expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.subtitle = element_text(hjust = 0.5, size = 7,
                                       color = "#6B7280"),
          legend.position = "none",
          plot.margin = margin(1, 1, 1, 1))
  empty_plot <- function(msg)
    Reduce(`+`, c(list(base_zone), zone_layers)) + labs(subtitle = msg)

  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0)
    return(empty_plot("no data"))
  if ("PitcherThrows" %in% names(df)) {
    ph <- toupper(substr(as.character(df$PitcherThrows), 1, 1))
    df <- df[!is.na(ph) & ph == hand, , drop = FALSE]
  }
  if (nrow(df) == 0) return(empty_plot(paste0("no pitches vs ", hand, "HP")))

  d <- tryCatch(.hp_feet(df), error = function(e) df)
  x <- suppressWarnings(as.numeric(d$PlateLocSide))
  z <- suppressWarnings(as.numeric(d$PlateLocHeight))
  # Zone + plate are already pitcher's perspective; mirror the pitches only.
  if (isTRUE(flip_x)) x <- -x

  if (identical(kind, "miss")) {
    sw <- if ("PitchCall" %in% names(d)) .hp_is_swing(as.character(d$PitchCall))
          else rep(FALSE, nrow(d))
    v  <- as.numeric(as.character(d$PitchCall) == "StrikeSwinging")
    ok <- sw & is.finite(x) & is.finite(z) & is.finite(v)
    lab <- "swings"
  } else {
    v  <- if ("mm_xslg" %in% names(d))
      suppressWarnings(as.numeric(d$mm_xslg)) else rep(NA_real_, nrow(d))
    ok <- is.finite(x) & is.finite(z) & is.finite(v)
    lab <- "BIP"
  }
  x <- x[ok]; z <- z[ok]; v <- v[ok]
  if (length(v) < min_n)
    return(empty_plot(paste0(length(v), " ", lab, " vs ", hand, "HP")))

  # ---- regular hex lattice (pointy-top) --------------------------------------
  # Even rows sit at k*dx, odd rows at (k + 1/2)*dx; both sets are symmetric
  # about x = 0, so the lattice never leans to one side of the plate.
  R  <- max(as.numeric(hex_r), 0.05)
  dx <- sqrt(3) * R
  dy <- 1.5 * R
  cz <- seq(0.80, 4.10, by = dy)
  kmax <- ceiling(1.60 / dx)
  hexc <- do.call(rbind, lapply(seq_along(cz), function(i) {
    cxs <- if (i %% 2 == 0) ((-kmax:kmax) + 0.5) * dx else (-kmax:kmax) * dx
    data.frame(cx = cxs, cz = cz[i])
  }))
  hexc <- hexc[abs(hexc$cx) <= 1.58, , drop = FALSE]

  bw <- max(as.numeric(bw), 0.05)
  W <- exp(-(outer(hexc$cx, x, "-")^2 + outer(hexc$cz, z, "-")^2) / (2 * bw^2))
  wsum <- rowSums(W)
  hexc$val <- as.numeric(W %*% v) / pmax(wsum, 1e-9)
  hexc$val[wsum < 1.5] <- NA_real_
  hexc <- hexc[is.finite(hexc$val), , drop = FALSE]
  if (!nrow(hexc))
    return(empty_plot(paste0(length(v), " ", lab, " vs ", hand, "HP")))

  # ---- gamma warp ------------------------------------------------------------
  hi <- if (identical(kind, "miss"))
    max(as.numeric(stats::quantile(hexc$val, 0.95, na.rm = TRUE)), 0.25)
  else
    max(as.numeric(stats::quantile(hexc$val, 0.95, na.rm = TRUE)), 0.45)
  tt <- pmin(pmax(hexc$val / max(hi, 1e-9), 0), 1)
  hexc$shade <- tt ^ max(as.numeric(gamma_warp), 0.05)
  hexc <- hexc[order(hexc$shade), , drop = FALSE]   # hot cells drawn last

  # Hex vertices, nudged 2% outward so anti-aliasing leaves no white seams.
  ang <- (seq(0, 5) * 60 + 90) * pi / 180
  Rd  <- R * 1.02
  nh  <- nrow(hexc)
  hpoly <- data.frame(
    id    = rep(seq_len(nh), each = 6),
    hx    = rep(hexc$cx, each = 6) + Rd * cos(rep(ang, nh)),
    hy    = rep(hexc$cz, each = 6) + Rd * sin(rep(ang, nh)),
    shade = rep(hexc$shade, each = 6))

  fill_scale <- scale_fill_gradientn(
    colours = c("#FFFFFF", "#FDE0D9", "#F4A582", "#D6604D", "#B2182B"),
    limits = c(0, 1), oob = scales::squish,
    na.value = "white", guide = "none")

  p <- base_zone +
    geom_polygon(data = hpoly,
                 aes(x = hx, y = hy, group = id, fill = shade),
                 colour = NA)
  p <- Reduce(`+`, c(list(p), zone_layers))
  p + fill_scale +
    labs(subtitle = paste0(length(v), " ", lab, " vs ", hand, "HP"))
}

# ggplot -> base64 PNG data URI for inline <img> cells in the matrix.
.gg_data_uri <- function(p, w = 150, h = 176, res = 96) {
  f <- tempfile(fileext = ".png")
  ok <- tryCatch({
    if (requireNamespace("ragg", quietly = TRUE))
      ragg::agg_png(f, width = w, height = h, res = res)
    else grDevices::png(f, width = w, height = h, res = res)
    print(p); grDevices::dev.off(); TRUE
  }, error = function(e) { try(grDevices::dev.off(), silent = TRUE); FALSE })
  if (!ok || !file.exists(f)) return(NULL)
  on.exit(unlink(f), add = TRUE)
  raw <- readBin(f, "raw", file.info(f)$size)
  paste0("data:image/png;base64,", jsonlite::base64_enc(raw))
}

# ---- Hitter value heatmaps (Viz tab) -----------------------------------------
# Kernel-weighted average of a per-pitch value column (hp_dv / hp_cv /
# hp_pv / hp_rv) over plate location, drawn on the strike zone from the
# catcher's view. Red = runs added by the hitter, blue = runs lost.
hp_value_heatmap <- function(df, col, title, min_n = 25,
                             mode = c("net", "created", "lost")) {
  mode <- match.arg(mode)

  # PLV-style presentation: clean white field, thick zone with a 3x3
  # grid, home plate, and a "Stands Here" rail on the batter's side.
  zx <- c(-0.83, 0.83); zz <- c(1.5, 3.5)
  x3 <- zx[1] + diff(zx) * c(1, 2) / 3
  z3 <- zz[1] + diff(zz) * c(1, 2) / 3

  bs <- ""
  if (!is.null(df) && is.data.frame(df) && "BatterSide" %in% names(df)) {
    tb <- table(toupper(substr(trimws(as.character(df$BatterSide)), 1, 1)))
    tb <- tb[names(tb) %in% c("L", "R")]
    if (length(tb) > 0) bs <- names(sort(tb, decreasing = TRUE))[1]
  }
  # catcher's view: RHB stands on the negative-x side, LHB positive
  stands_x <- if (identical(bs, "R")) -1.55 else if (identical(bs, "L")) 1.55 else NA

  zone_layers <- list(
    annotate("rect", xmin = zx[1], xmax = zx[2], ymin = zz[1], ymax = zz[2],
             fill = NA, color = "black", linewidth = 1.1),
    annotate("segment", x = x3, xend = x3, y = zz[1], yend = zz[2],
             color = "black", linewidth = 0.4),
    annotate("segment", x = zx[1], xend = zx[2], y = z3, yend = z3,
             color = "black", linewidth = 0.4),
    annotate("path",
             x = c(-0.708, 0.708, 0.708, 0, -0.708, -0.708),
             y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15),
             color = "black", linewidth = 0.6)
  )
  stands_layer <- if (is.finite(stands_x)) {
    annotate("text", x = stands_x, y = 2.5, label = "Stands Here",
             angle = 90, size = 3.4, color = "#374151")
  } else NULL

  base_zone <- ggplot() +
    coord_fixed(xlim = c(-1.75, 1.75), ylim = c(0, 4.4), expand = FALSE) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold",
                                    color = "#0E6E70"),
          plot.subtitle = element_text(hjust = 0.5, size = 9, color = "#6B7280"),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          legend.position = "none",
          plot.margin = margin(4, 4, 4, 4)) +
    ggtitle(title)

  empty_plot <- function(msg) {
    Reduce(`+`, c(list(base_zone), zone_layers)) +
      labs(subtitle = msg)
  }

  if (is.null(df) || !is.data.frame(df) || !col %in% names(df))
    return(empty_plot("no scored pitches"))
  d <- .hp_feet(df)
  x <- suppressWarnings(as.numeric(d$PlateLocSide))
  z <- suppressWarnings(as.numeric(d$PlateLocHeight))
  v <- suppressWarnings(as.numeric(d[[col]]))
  ok <- is.finite(x) & is.finite(z) & is.finite(v)
  x <- x[ok]; z <- z[ok]; v <- v[ok]
  if (length(v) < min_n)
    return(empty_plot(paste0("only ", length(v), " scored pitches")))

  # created = positive contributions only, lost = magnitude of negative
  # ones; both smoothed over ALL pitches so a hot spot means "this
  # location produces a lot of that outcome per pitch seen there".
  if (identical(mode, "created")) v <- pmax(v, 0)
  if (identical(mode, "lost"))    v <- pmax(-v, 0)

  gx <- seq(-1.5, 1.5, length.out = 46)
  gz <- seq(0.7, 4.3, length.out = 54)
  grid <- expand.grid(gx = gx, gz = gz)
  h <- 0.30   # kernel bandwidth, feet
  W <- exp(-(outer(grid$gx, x, "-")^2 + outer(grid$gz, z, "-")^2) / (2 * h^2))
  wsum <- rowSums(W)
  grid$val <- 100 * as.numeric(W %*% v) / pmax(wsum, 1e-9)  # RV / 100 pitches
  grid$val[wsum < 2] <- NA    # thin-data cells stay white

  # Quantile color limits with clamping: the middle of the scale stays
  # near-white so only real deviations color, and hotspots saturate
  # instead of stretching the whole palette (PLV-style readability).
  fin <- grid$val[is.finite(grid$val)]
  if (identical(mode, "net")) {
    lim <- as.numeric(stats::quantile(abs(fin), 0.92, na.rm = TRUE))
    lim <- max(lim, 0.4)
    fill_scale <- scale_fill_gradientn(
      colours = c("#2166AC", "#67A9CF", "#D1E5F0", "#FFFFFF",
                  "#FDDBC7", "#EF8A62", "#B2182B"),
      limits = c(-lim, lim), oob = scales::squish,
      na.value = "white", guide = "none")
    sub_txt <- "blue = runs lost \u2022 white = neutral \u2022 red = runs created"
  } else {
    qs <- as.numeric(stats::quantile(fin, c(0.35, 0.95), na.rm = TRUE))
    if (!all(is.finite(qs)) || qs[2] <= qs[1]) qs <- c(0, max(fin, 0.2))
    hi_col <- if (identical(mode, "created")) c("#DCEFDC", "#7FBF7B", "#1B7837")
              else c("#FBDCD5", "#EF8A62", "#B2182B")
    fill_scale <- scale_fill_gradientn(
      colours = c("#FFFFFF", "#FFFFFF", hi_col),
      values = c(0, 0.18, 0.4, 0.7, 1),
      limits = qs, oob = scales::squish,
      na.value = "white", guide = "none")
    sub_txt <- if (identical(mode, "created"))
      "white = little \u2022 darker green = more runs created"
    else "white = little \u2022 darker red = more runs lost"
  }

  p <- base_zone +
    geom_raster(data = grid, aes(x = gx, y = gz, fill = val),
                interpolate = TRUE)
  p <- Reduce(`+`, c(list(p), zone_layers))
  if (!is.null(stands_layer)) p <- p + stands_layer
  p + fill_scale +
    labs(subtitle = paste0(format(length(v), big.mark = ","),
                           " pitches \u2022 ", sub_txt))
}


# ============================================================================
