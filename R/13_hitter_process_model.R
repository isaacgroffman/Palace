# HITTER PROCESS MODEL — Decision Value / Contact / Power / Process+
# ----------------------------------------------------------------------------
# The complement of Pitching+: every pitch a hitter faces carries an expected
# run value E0 conditioned on WHERE it was (zone region), WHEN it came (count
# group) and HOW GOOD it was (pitching_xrv quality quintile, i.e. Pitching+).
# What the hitter does is then priced against that expectation in three
# telescoping, non-interacting layers:
#
#   Decision Value  DV = E1 - E0   every pitch. E1 = league expected run value
#                                  of the CHOSEN action (swing/take) on
#                                  similar pitches. Swinging at a dotted 0-2
#                                  fastball and taking a waste slider both
#                                  price against what that exact pitch is
#                                  worth, so pitch quality is controlled.
#   Contact         CV = E2 - E1   swings only. E2 = run value of the contact
#                                  class: whiff and foul are deterministic
#                                  count transitions; in-play is the league
#                                  expected contact-quality value on similar
#                                  pitches. Controls for the Decision made.
#   Power           PV = A  - E2   batted balls only. A = contact-quality run
#                                  value from the exit-velo x hit-type grid
#                                  (a 110 mph line drive is priced as a
#                                  110 mph line drive regardless of outcome
#                                  luck). Controls for making Contact at all.
#
#   DV + CV + PV = A - E0: total runs above the pitch's expectation. Because
#   E0 is lower on a great pitch (hitter's perspective), damage done against
#   a dotted 98 outgrades the same ball off a hanging curveball.
#
# Aggregated per 100 pitches faced and scaled against the pooled college
# distribution -> Decision+, Contact+, Power+ and their sum Process+
# (100 = average, 10 = 1 SD), plus percentiles.
#
# All expectation tables are built ONCE, lazily, from full_data_p5_compare:
# the outcome layers (marginal tables, BIP grid) use every row (~520k); the
# quality-quintile layers use the proModel-scored rows (finite pitching_xrv).
# Frames without pitching_xrv still score cleanly — the quality dimension
# just falls back to the marginal table.
# ============================================================================

# ---- run-value primitives ----------------------------------------------------
# Count states: expected runs (offense POV) through end of PA, relative to
# league average PA. Standard linear-weight count values.
HP_COUNT_RV <- c(
  "0-0" =  0.000, "1-0" =  0.034, "2-0" =  0.090, "3-0" =  0.185,
  "0-1" = -0.040, "1-1" = -0.015, "2-1" =  0.037, "3-1" =  0.130,
  "0-2" = -0.098, "1-2" = -0.080, "2-2" = -0.037, "3-2" =  0.047)

HP_EVENT_RV <- c(bb = 0.30, hbp = 0.32, single = 0.45, double = 0.75,
                 triple = 1.03, hr = 1.38, out = -0.25, k = -0.28,
                 sac = -0.15)

.hp_count_key <- function(b, s) {
  b <- pmin(pmax(suppressWarnings(as.integer(b)), 0L), 3L)
  s <- pmin(pmax(suppressWarnings(as.integer(s)), 0L), 2L)
  paste0(b, "-", s)
}
.hp_v <- function(b, s) {
  v <- HP_COUNT_RV[.hp_count_key(b, s)]
  v[is.na(v)] <- 0
  unname(v)
}

# ---- classification of what happened ----------------------------------------
.hp_is_swing <- function(pc) {
  pc <- as.character(pc)
  pc %in% c("StrikeSwinging", "InPlay") | grepl("^Foul", pc)
}
.hp_contact_class <- function(pc) {
  pc <- as.character(pc)
  ifelse(pc == "StrikeSwinging", "whiff",
  ifelse(grepl("^Foul", pc), "foul",
  ifelse(pc == "InPlay", "inplay", NA_character_)))
}

# actual run value of the pitch, offense perspective (count transitions,
# terminal BB/K, HBP, and in-play outcomes via linear weights)
.hp_outcome_rv <- function(pr) {
  pr <- as.character(pr)
  out <- rep(HP_EVENT_RV["out"], length(pr))
  out[pr == "Single"]                    <- HP_EVENT_RV["single"]
  out[pr == "Double"]                    <- HP_EVENT_RV["double"]
  out[pr == "Triple"]                    <- HP_EVENT_RV["triple"]
  out[pr %in% c("HomeRun", "Homerun")]   <- HP_EVENT_RV["hr"]
  out[pr == "Sacrifice"]                 <- HP_EVENT_RV["sac"]
  unname(out)   # Error / FieldersChoice / Out / Undefined price as outs
}

.hp_actual_rv <- function(pc, pr, b, s) {
  pc <- as.character(pc)
  v0 <- .hp_v(b, s)
  rv <- rep(NA_real_, length(pc))

  is_ball  <- grepl("^Ball", pc)
  is_kcall <- pc == "StrikeCalled"
  is_whiff <- pc == "StrikeSwinging"
  is_foul  <- grepl("^Foul", pc)
  is_hbp   <- pc == "HitByPitch"
  is_bip   <- pc == "InPlay"

  bb <- suppressWarnings(as.integer(b)); ss <- suppressWarnings(as.integer(s))
  rv[is_ball] <- ifelse(bb[is_ball] >= 3,
                        HP_EVENT_RV["bb"] - v0[is_ball],
                        .hp_v(bb[is_ball] + 1, ss[is_ball]) - v0[is_ball])
  kmask <- is_kcall | is_whiff
  rv[kmask] <- ifelse(ss[kmask] >= 2,
                      HP_EVENT_RV["k"] - v0[kmask],
                      .hp_v(bb[kmask], ss[kmask] + 1) - v0[kmask])
  rv[is_foul] <- ifelse(ss[is_foul] >= 2, 0,
                        .hp_v(bb[is_foul], ss[is_foul] + 1) - v0[is_foul])
  rv[is_hbp] <- HP_EVENT_RV["hbp"] - v0[is_hbp]
  rv
}

# ---- pitch buckets -----------------------------------------------------------
# Zone regions in FEET (app zone: +/-0.83, 1.5-3.38): heart = inner core,
# shadow = the edge band, chase = reachable off the plate, waste = beyond.
.hp_zone_region <- function(x, z) {
  x <- abs(suppressWarnings(as.numeric(x)))
  z <- suppressWarnings(as.numeric(z))
  out <- rep("waste", length(x))
  out[x <= 1.55 & z >= 0.50 & z <= 4.30] <- "chase"
  out[x <= 1.05 & z >= 1.15 & z <= 3.75] <- "shadow"
  out[x <= 0.56 & z >= 1.85 & z <= 3.05] <- "heart"
  out[!is.finite(x) | !is.finite(z)] <- NA_character_
  out
}

.hp_count_group <- function(b, s) {
  k <- .hp_count_key(b, s)
  ifelse(k %in% c("0-2", "1-2", "2-2", "3-2"), "two_strike",
  ifelse(k %in% c("1-0", "2-0", "3-0", "2-1", "3-1"), "ahead",
  ifelse(k == "0-1", "behind", "even")))
}

# plate locations must be in feet before bucketing (the pooled comparison
# frame carries inches; NCAA loads carry feet)
.hp_feet <- function(df) {
  if (!all(c("PlateLocSide", "PlateLocHeight") %in% names(df))) return(df)
  med <- suppressWarnings(stats::median(abs(df$PlateLocHeight), na.rm = TRUE))
  if (is.finite(med) && med > 8) {
    df$PlateLocSide   <- df$PlateLocSide / 12
    df$PlateLocHeight <- df$PlateLocHeight / 12
  }
  df
}

# EV x hit-type contact-quality cell
.hp_ev_bucket <- function(ev) {
  ev <- suppressWarnings(as.numeric(ev))
  cut(ev, breaks = c(-Inf, 70, 80, 85, 90, 95, 100, 105, Inf),
      labels = c("<70","70-80","80-85","85-90","90-95","95-100","100-105","105+"),
      right = FALSE)
}
.hp_hit_type <- function(ht) {
  ht <- as.character(ht)
  ifelse(ht %in% c("GroundBall","LineDrive","FlyBall","Popup"), ht,
  ifelse(grepl("Bunt", ht), "GroundBall", "Unknown"))
}

# ---- lazy league tables ------------------------------------------------------
.hp_env <- new.env(parent = emptyenv())

.hp_shrunk_table <- function(keys, vals, k = 50) {
  ok <- !is.na(keys) & is.finite(vals)
  keys <- keys[ok]; vals <- vals[ok]
  overall <- mean(vals)
  n  <- tapply(vals, keys, length)
  mu <- tapply(vals, keys, mean)
  shrunk <- (n * mu + k * overall) / (n + k)
  list(tab = shrunk, overall = overall)
}
.hp_lookup <- function(tbl, keys) {
  v <- tbl$tab[keys]
  out <- as.numeric(v)
  out[is.na(out)] <- tbl$overall
  out
}

# ---- precomputed model state ---------------------------------------------------
# Building the tables needs the full league pool (~600k pitches) and the
# league distribution scores every one of them. scripts/build_reference_pools.R
# does that once per scored-table build and ships the resulting state
# (hp_model.rds in Storage); the app restores it instead of loading the pool.
.HP_STATE_FIELDS <- c("q_breaks", "T_bipgrid", "T_e0_m", "T_e0_q", "T_e1_m", "T_e1_q",
                      "T_e2_m", "T_e2_q", "ready", "league")
hp_export_state <- function() {
  st <- lapply(.HP_STATE_FIELDS, function(f) .hp_env[[f]])
  names(st) <- .HP_STATE_FIELDS
  st
}
hp_restore_state <- function(st) {
  if (!is.list(st) || !isTRUE(st$ready)) return(FALSE)
  for (f in .HP_STATE_FIELDS) .hp_env[[f]] <- st[[f]]
  TRUE
}
hp_restore_artifact <- function() {
  if (isTRUE(.hp_env$restore_tried)) return(isTRUE(.hp_env$ready))
  .hp_env$restore_tried <- TRUE
  if (!exists(".artifact", mode = "function")) return(FALSE)
  st <- tryCatch(.artifact("hp_model", "rds", function() NULL), error = function(e) NULL)
  ok <- hp_restore_state(st)
  if (ok) cat("[HitterModel] restored precomputed tables + league basis (",
              .hp_env$league$n_hitters %||% NA, "qualified hitters )\n")
  ok
}

hp_build_tables <- function(pool = NULL) {
  if (isTRUE(.hp_env$ready)) return(invisible(TRUE))
  if (is.null(pool) && hp_restore_artifact()) return(invisible(TRUE))
  if (is.null(pool)) {
    if (!exists("full_data_p5_compare")) return(invisible(FALSE))
    pool <- full_data_p5_compare
  }
  need <- c("PitchCall","PlayResult","Balls","Strikes",
            "PlateLocSide","PlateLocHeight")
  if (!all(need %in% names(pool))) return(invisible(FALSE))
  cat("[HitterModel] building league expectation tables from",
      nrow(pool), "pooled pitches...\n")

  d <- pool
  d <- .hp_feet(d)
  pc <- as.character(d$PitchCall)
  keeprow <- !is.na(pc) & pc %in% c("BallCalled","BallIntentional","BallinDirt",
                                    "StrikeCalled","StrikeSwinging","InPlay",
                                    "HitByPitch") | grepl("^Foul", pc)
  d <- d[keeprow, , drop = FALSE]

  b <- d$Balls; s <- d$Strikes
  zone  <- .hp_zone_region(d$PlateLocSide, d$PlateLocHeight)
  cgrp  <- .hp_count_group(b, s)
  swing <- .hp_is_swing(d$PitchCall)
  act   <- ifelse(swing, "swing", "take")

  # quality quintile from pitching_xrv where scored (Coastal rows)
  qx <- if ("pitching_xrv" %in% names(d))
    suppressWarnings(as.numeric(d$pitching_xrv)) else rep(NA_real_, nrow(d))
  qq <- stats::quantile(qx, probs = seq(0.2, 0.8, 0.2), na.rm = TRUE)
  if (all(is.finite(qq))) {
    .hp_env$q_breaks <- unname(qq)
    qual <- as.character(findInterval(qx, .hp_env$q_breaks) + 1L)  # "1"(best pitch, low xrv?)..
    qual[!is.finite(qx)] <- NA_character_
  } else { .hp_env$q_breaks <- NULL; qual <- rep(NA_character_, nrow(d)) }

  # ---- BIP contact-quality grid: EV x hit type -> mean outcome run value ----
  bip <- d$PitchCall == "InPlay"
  ev  <- if ("ExitSpeed" %in% names(d))
    suppressWarnings(as.numeric(d$ExitSpeed)) else rep(NA_real_, nrow(d))
  ht  <- if ("TaggedHitType" %in% names(d))
    .hp_hit_type(d$TaggedHitType) else rep("Unknown", nrow(d))
  bip_out_rv <- .hp_outcome_rv(d$PlayResult)   # linear-weight event value
  gk <- paste(.hp_ev_bucket(ev), ht, sep = "|")
  use <- bip & is.finite(ev) & ht != "Unknown"
  .hp_env$T_bipgrid <- .hp_shrunk_table(gk[use], bip_out_rv[use], k = 30)

  # per-pitch contact-quality run value ON the BIP rows themselves
  # (grid value where EV known, raw outcome value otherwise) — this is what
  # in-play expectations and Power are priced in
  xrv_contact <- rep(NA_real_, nrow(d))
  xrv_contact[bip] <- ifelse(use[bip],
                             .hp_lookup(.hp_env$T_bipgrid, gk[bip]),
                             bip_out_rv[bip])

  # ---- actual per-pitch rv, with in-play priced by contact quality ----
  rv <- .hp_actual_rv(d$PitchCall, d$PlayResult, b, s)
  rv[bip] <- xrv_contact[bip] - .hp_v(b, s)[bip]

  base_key <- paste(zone, cgrp, sep = "|")
  okb <- !is.na(zone)

  # E0: expected rv of the pitch (marginal + quality-conditioned)
  .hp_env$T_e0_m <- .hp_shrunk_table(base_key[okb], rv[okb])
  okq <- okb & !is.na(qual)
  .hp_env$T_e0_q <- if (any(okq))
    .hp_shrunk_table(paste(base_key[okq], qual[okq], sep = "|"), rv[okq]) else NULL

  # E1: expected rv given the action
  akey <- paste(base_key, act, sep = "|")
  .hp_env$T_e1_m <- .hp_shrunk_table(akey[okb], rv[okb])
  .hp_env$T_e1_q <- if (any(okq))
    .hp_shrunk_table(paste(akey[okq], qual[okq], sep = "|"), rv[okq]) else NULL

  # E2 (in-play only; whiff/foul are deterministic count transitions):
  # expected CONTACT-QUALITY value of a ball in play on similar pitches
  ipk <- bip & okb
  ip_val <- xrv_contact - .hp_v(b, s)      # contact value net of count state
  .hp_env$T_e2_m <- .hp_shrunk_table(base_key[ipk], ip_val[ipk])
  ipq <- ipk & !is.na(qual)
  .hp_env$T_e2_q <- if (any(ipq))
    .hp_shrunk_table(paste(base_key[ipq], qual[ipq], sep = "|"), ip_val[ipq]) else NULL

  .hp_env$ready <- TRUE
  cat("[HitterModel] tables ready (",
      length(.hp_env$T_e1_m$tab), "action cells,",
      length(.hp_env$T_bipgrid$tab), "BIP grid cells,",
      if (!is.null(.hp_env$T_e1_q)) "quality-conditioned )" else "marginal only )",
      "\n")
  invisible(TRUE)
}

# ---- per-pitch scoring -------------------------------------------------------
# Adds hp_dv / hp_cv / hp_pv / hp_rv / hp_swing / hp_bip to any pitch frame.
hp_score_pitches <- function(df) {
  need <- c("PitchCall","Balls","Strikes","PlateLocSide","PlateLocHeight")
  if (!is.data.frame(df) || nrow(df) == 0 || !all(need %in% names(df))) return(df)
  if (!isTRUE(.hp_env$ready) && !hp_build_tables()) return(df)

  d <- .hp_feet(df[, intersect(c(need, "PlayResult","ExitSpeed","TaggedHitType",
                                 "pitching_xrv"), names(df)), drop = FALSE])
  pc <- as.character(d$PitchCall)
  b <- d$Balls; s <- d$Strikes
  zone <- .hp_zone_region(d$PlateLocSide, d$PlateLocHeight)
  cgrp <- .hp_count_group(b, s)
  swing <- .hp_is_swing(pc)
  act <- ifelse(swing, "swing", "take")
  pr <- if ("PlayResult" %in% names(d)) d$PlayResult else rep(NA_character_, nrow(d))

  qual <- rep(NA_character_, nrow(d))
  if (!is.null(.hp_env$q_breaks) && "pitching_xrv" %in% names(d)) {
    qx <- suppressWarnings(as.numeric(d$pitching_xrv))
    qual <- as.character(findInterval(qx, .hp_env$q_breaks) + 1L)
    qual[!is.finite(qx)] <- NA_character_
  }

  base_key <- paste(zone, cgrp, sep = "|")
  akey  <- paste(base_key, act, sep = "|")
  qbase <- paste(base_key, qual, sep = "|")
  qakey <- paste(akey, qual, sep = "|")

  pick <- function(Tq, Tm, kq, km) {
    out <- .hp_lookup(Tm, km)
    if (!is.null(Tq)) {
      vq <- Tq$tab[kq]
      hit <- !is.na(vq)
      out[hit] <- as.numeric(vq[hit])
    }
    out
  }
  e0 <- pick(.hp_env$T_e0_q, .hp_env$T_e0_m, qbase, base_key)
  e1 <- pick(.hp_env$T_e1_q, .hp_env$T_e1_m, qakey, akey)

  # contact class values
  v0  <- .hp_v(b, s)
  cls <- .hp_contact_class(pc)
  whiff_rv <- .hp_actual_rv(rep("StrikeSwinging", nrow(d)), pr, b, s)
  foul_rv  <- .hp_actual_rv(rep("FoulBall", nrow(d)), pr, b, s)
  e2_ip <- pick(.hp_env$T_e2_q, .hp_env$T_e2_m, qbase, base_key)
  e2 <- ifelse(cls == "whiff", whiff_rv,
        ifelse(cls == "foul",  foul_rv,
        ifelse(cls == "inplay", e2_ip, NA_real_)))

  # A on batted balls: contact-quality value (EV x hit-type grid), net of count
  bip <- !is.na(cls) & cls == "inplay"
  a_pow <- rep(NA_real_, nrow(d))
  if (any(bip)) {
    ev <- if ("ExitSpeed" %in% names(d))
      suppressWarnings(as.numeric(d$ExitSpeed)) else rep(NA_real_, nrow(d))
    ht <- if ("TaggedHitType" %in% names(d))
      .hp_hit_type(d$TaggedHitType) else rep("Unknown", nrow(d))
    gk <- paste(.hp_ev_bucket(ev), ht, sep = "|")
    known <- bip & is.finite(ev) & ht != "Unknown"
    a_pow[known] <- .hp_lookup(.hp_env$T_bipgrid, gk[known]) - v0[known]
    a_pow[bip & !known] <- .hp_outcome_rv(pr[bip & !known]) - v0[bip & !known]
  }

  valid <- !is.na(zone) & pc %in% c("BallCalled","BallIntentional","BallinDirt",
                                    "StrikeCalled","StrikeSwinging","InPlay",
                                    "HitByPitch") | (!is.na(zone) & grepl("^Foul", pc))

  df$hp_swing <- swing & valid
  df$hp_bip   <- bip & valid
  df$hp_dv <- ifelse(valid, e1 - e0, NA_real_)
  df$hp_cv <- ifelse(valid & swing & !is.na(e2), e2 - e1, NA_real_)
  df$hp_pv <- ifelse(valid & bip & !is.na(a_pow), a_pow - e2, NA_real_)
  df$hp_rv <- rowSums(cbind(df$hp_dv,
                            ifelse(is.na(df$hp_cv), 0, df$hp_cv),
                            ifelse(is.na(df$hp_pv), 0, df$hp_pv)))
  df$hp_rv[!valid] <- NA_real_
  df
}

# ---- aggregation -------------------------------------------------------------
# Per-100-pitch rates for a scored frame (one hitter's pitches, or any subset)
hp_rates <- function(df) {
  ok <- is.finite(df$hp_dv)
  n <- sum(ok)
  if (n == 0) return(list(n = 0, dv = NA_real_, cv = NA_real_,
                          pv = NA_real_, prc = NA_real_))
  list(n = n,
       dv  = 100 * sum(df$hp_dv[ok]) / n,
       cv  = 100 * sum(df$hp_cv[ok & is.finite(df$hp_cv)]) / n,
       pv  = 100 * sum(df$hp_pv[ok & is.finite(df$hp_pv)]) / n,
       prc = 100 * sum(df$hp_rv[ok]) / n)
}

# League distribution across qualified hitters in the pool (lazy, cached):
# the plus scale (100/10) and percentile basis for every grade.
HP_QUALIFY_PITCHES <- 200
hp_build_league <- function(pool = NULL) {
  if (!is.null(.hp_env$league)) return(invisible(TRUE))
  if (is.null(pool) && hp_restore_artifact() && !is.null(.hp_env$league)) return(invisible(TRUE))
  if (is.null(pool)) {
    if (!exists("full_data_p5_compare")) return(invisible(FALSE))
    pool <- full_data_p5_compare
  }
  if (!"Batter" %in% names(pool)) return(invisible(FALSE))
  if (!hp_build_tables(pool)) return(invisible(FALSE))
  cat("[HitterModel] building league hitter distribution...\n")
  sc <- hp_score_pitches(pool)
  if (!"hp_dv" %in% names(sc)) return(invisible(FALSE))
  ok <- is.finite(sc$hp_dv) & !is.na(sc$Batter) & nzchar(as.character(sc$Batter))
  sc_full <- sc[ok, , drop = FALSE]   # full columns kept for the stat basis
  sc <- sc_full[, c("Batter","hp_dv","hp_cv","hp_pv","hp_rv")]
  n_by <- table(sc$Batter)
  qual <- names(n_by)[n_by >= HP_QUALIFY_PITCHES]
  if (length(qual) < 25) qual <- names(n_by)[n_by >= 100]
  sc <- sc[sc$Batter %in% qual, , drop = FALSE]
  agg <- function(v, g, base) 100 * tapply(ifelse(is.finite(v), v, 0), g, sum) /
    as.numeric(table(g))
  g <- sc$Batter
  dist <- data.frame(
    dv  = agg(sc$hp_dv, g), cv = agg(sc$hp_cv, g),
    pv  = agg(sc$hp_pv, g), prc = agg(sc$hp_rv, g))
  ms <- lapply(dist, function(x) {
    x <- x[is.finite(x)]
    c(m = mean(x), s = max(stats::sd(x), 1e-6))
  })

  # ---- raw-stat distribution across the same qualified hitters ----
  # (percentile basis for Swing%/Chase%/Whiff%/EV/etc. on the hitter page)
  stat_dist <- NULL
  pt_dist <- NULL
  tryCatch({
    sf <- sc_full[sc_full$Batter %in% qual, , drop = FALSE]
    sf <- .hp_feet(sf)
    gg  <- as.character(sf$Batter)
    pc  <- as.character(sf$PitchCall)
    sw  <- sf$hp_swing %in% TRUE
    bip <- sf$hp_bip %in% TRUE
    px  <- suppressWarnings(as.numeric(sf$PlateLocSide))
    pz  <- suppressWarnings(as.numeric(sf$PlateLocHeight))
    zin <- abs(px) <= 0.83 & pz >= 1.5 & pz <= 3.38
    ev  <- if ("ExitSpeed" %in% names(sf))
      suppressWarnings(as.numeric(sf$ExitSpeed)) else rep(NA_real_, nrow(sf))
    # per-hitter rate over a row subset, re-indexed onto `qual`
    rate_of <- function(vals, keep) {
      keep <- keep %in% TRUE & !is.na(vals)
      r <- tapply(vals[keep], gg[keep], mean)
      as.numeric(r[qual])
    }
    q90_of <- function(vals, keep) {
      keep <- keep %in% TRUE & is.finite(vals)
      r <- tapply(vals[keep], gg[keep],
                  function(z) as.numeric(stats::quantile(z, 0.9, na.rm = TRUE)))
      as.numeric(r[qual])
    }
    whf <- pc == "StrikeSwinging"
    stat_dist <- data.frame(
      swing_pct = 100 * rate_of(sw, rep(TRUE, length(sw))),
      z_swing   = 100 * rate_of(sw, zin %in% TRUE),
      chase     = 100 * rate_of(sw, zin %in% FALSE),
      whiff     = 100 * rate_of(whf, sw),
      z_whiff   = 100 * rate_of(whf, sw & zin %in% TRUE),
      contact   = 100 * rate_of(!whf, sw),
      avg_ev    = rate_of(ev, bip & is.finite(ev)),
      ev90      = q90_of(ev, bip),
      hard_hit  = 100 * rate_of(ev >= 95, bip & is.finite(ev)),
      row.names = qual)

    # per-pitch-type run-value distribution (RV/100 vs each type, per
    # hitter, min 30 of that type) — percentile basis for the diverging
    # value-by-pitch-type bars.
    if ("TaggedPitchType" %in% names(sf)) {
      pt <- as.character(sf$TaggedPitchType)
      kp <- !is.na(pt) & nzchar(pt) & !pt %in% c("Other", "Undefined") &
        is.finite(sf$hp_rv)
      key <- paste(gg[kp], pt[kp], sep = "\u241F")
      rv  <- sf$hp_rv[kp]
      n_k <- tapply(rv, key, length)
      m_k <- 100 * tapply(rv, key, sum) / as.numeric(n_k)
      okk <- is.finite(m_k) & as.numeric(n_k) >= 30
      tps <- sub("^.*\u241F", "", names(m_k))[okk]
      pt_dist <- split(as.numeric(m_k[okk]), tps)
    }
  }, error = function(e) {
    cat("[HitterModel] stat-distribution build failed:",
        conditionMessage(e), "\n")
  })

  .hp_env$league <- list(dist = dist, ms = ms, n_hitters = nrow(dist),
                         stat_dist = stat_dist, pt_dist = pt_dist)
  cat("[HitterModel] league basis:", nrow(dist), "qualified hitters",
      if (!is.null(stat_dist)) "| stat percentile basis OK" else "",
      if (!is.null(pt_dist)) paste0("| pitch-type basis: ",
                                    length(pt_dist), " types") else "", "\n")
  invisible(TRUE)
}

hp_plus <- function(x, key) {
  L <- .hp_env$league
  if (is.null(L) || !is.finite(x)) return(NA_real_)
  round(100 + 10 * (x - L$ms[[key]]["m"]) / L$ms[[key]]["s"])
}
hp_pctile <- function(x, key) {
  L <- .hp_env$league
  if (is.null(L) || !is.finite(x)) return(NA_real_)
  v <- L$dist[[key]]; v <- v[is.finite(v)]
  round(100 * mean(v <= x))
}

# Percentile of one raw stat vs the qualified-hitter basis.
# higher_better = FALSE flips the scale (chase, whiff) so a displayed
# 90 always means "90th percentile GOOD".
hp_stat_pctile <- function(x, key, higher_better = TRUE) {
  L <- .hp_env$league
  if (is.null(L) || is.null(L$stat_dist) || !is.finite(x)) return(NA_real_)
  v <- L$stat_dist[[key]]; v <- v[is.finite(v)]
  if (length(v) < 10) return(NA_real_)
  p <- 100 * mean(v <= x)
  round(if (higher_better) p else 100 - p)
}

# Percentile of a hitter's RV/100 vs one pitch type, against every
# qualified hitter's RV/100 vs that same type.
hp_pt_pctile <- function(x, type) {
  L <- .hp_env$league
  if (is.null(L) || is.null(L$pt_dist) || !is.finite(x)) return(NA_real_)
  v <- L$pt_dist[[as.character(type)]]
  if (is.null(v)) return(NA_real_)
  v <- v[is.finite(v)]
  if (length(v) < 10) return(NA_real_)
  round(100 * mean(v <= x))
}

# Full hitter card: overall + vs L / vs R splits + per-pitch-type values.
hp_hitter_summary <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0 || !"hp_dv" %in% names(df)) return(NULL)
  hp_build_league()
  grade_block <- function(sub) {
    r <- hp_rates(sub)
    if (r$n == 0) return(NULL)
    swings <- sub$hp_swing %in% TRUE
    bip    <- sub$hp_bip %in% TRUE
    pc <- as.character(sub$PitchCall)
    zone_in <- abs(suppressWarnings(as.numeric(.hp_feet(sub)$PlateLocSide))) <= 0.83 &
      suppressWarnings(as.numeric(.hp_feet(sub)$PlateLocHeight)) >= 1.5 &
      suppressWarnings(as.numeric(.hp_feet(sub)$PlateLocHeight)) <= 3.38
    ev <- if ("ExitSpeed" %in% names(sub))
      suppressWarnings(as.numeric(sub$ExitSpeed)) else rep(NA_real_, nrow(sub))
    st <- list(
      swing_pct  = 100 * mean(swings, na.rm = TRUE),
      z_swing    = 100 * mean(swings[zone_in %in% TRUE], na.rm = TRUE),
      chase      = 100 * mean(swings[zone_in %in% FALSE], na.rm = TRUE),
      whiff      = 100 * mean(pc[swings] == "StrikeSwinging", na.rm = TRUE),
      z_whiff    = 100 * mean(pc[swings & zone_in %in% TRUE] == "StrikeSwinging",
                              na.rm = TRUE),
      contact    = 100 * mean(pc[swings] != "StrikeSwinging", na.rm = TRUE),
      avg_ev     = mean(ev[bip], na.rm = TRUE),
      ev90       = as.numeric(stats::quantile(ev[bip], 0.9, na.rm = TRUE)),
      hard_hit   = 100 * mean(ev[bip] >= 95, na.rm = TRUE),
      n_bip      = sum(bip, na.rm = TRUE)
    )
    list(
      n = r$n, rates = r,
      plus = list(dv = hp_plus(r$dv, "dv"), cv = hp_plus(r$cv, "cv"),
                  pv = hp_plus(r$pv, "pv"), prc = hp_plus(r$prc, "prc")),
      pct  = list(dv = hp_pctile(r$dv, "dv"), cv = hp_pctile(r$cv, "cv"),
                  pv = hp_pctile(r$pv, "pv"), prc = hp_pctile(r$prc, "prc")),
      stats = st,
      # goodness percentiles vs the qualified basis (chase/whiff flipped
      # so higher displayed percentile always = better)
      stat_pct = list(
        swing_pct = hp_stat_pctile(st$swing_pct, "swing_pct"),
        z_swing   = hp_stat_pctile(st$z_swing,   "z_swing"),
        chase     = hp_stat_pctile(st$chase,     "chase",   higher_better = FALSE),
        whiff     = hp_stat_pctile(st$whiff,     "whiff",   higher_better = FALSE),
        z_whiff   = hp_stat_pctile(st$z_whiff,   "z_whiff", higher_better = FALSE),
        contact   = hp_stat_pctile(st$contact,   "contact"),
        avg_ev    = hp_stat_pctile(st$avg_ev,    "avg_ev"),
        ev90      = hp_stat_pctile(st$ev90,      "ev90"),
        hard_hit  = hp_stat_pctile(st$hard_hit,  "hard_hit")
      ))
  }
  side <- if ("PitcherThrows" %in% names(df))
    toupper(substr(trimws(as.character(df$PitcherThrows)), 1, 1)) else
      rep(NA_character_, nrow(df))
  out <- list(
    overall = grade_block(df),
    vs_l    = grade_block(df[side == "L", , drop = FALSE]),
    vs_r    = grade_block(df[side == "R", , drop = FALSE])
  )
  # per pitch type faced — overall plus vs-RHP / vs-LHP versions, each
  # row carrying the RV/100 percentile vs the qualified basis for that type
  if ("TaggedPitchType" %in% names(df)) {
    by_pitch_of <- function(frame) {
      if (is.null(frame) || nrow(frame) == 0) return(NULL)
      pt <- as.character(frame$TaggedPitchType)
      keep <- !is.na(pt) & nzchar(pt) & !pt %in% c("Other","Undefined") &
        is.finite(frame$hp_dv)
      if (!any(keep)) return(NULL)
      d <- frame[keep, , drop = FALSE]; ptk <- pt[keep]
      do.call(rbind, lapply(
        names(sort(table(ptk), decreasing = TRUE)), function(tp) {
          sub <- d[ptk == tp, , drop = FALSE]
          r <- hp_rates(sub)
          sw <- sub$hp_swing %in% TRUE
          data.frame(PitchType = tp, N = r$n,
                     Usage = round(100 * r$n / nrow(d), 1),
                     SwingPct = round(100 * mean(sw), 1),
                     WhiffPct = round(100 * mean(
                       as.character(sub$PitchCall[sw]) == "StrikeSwinging",
                       na.rm = TRUE), 1),
                     DV100 = round(r$dv, 2), Con100 = round(r$cv, 2),
                     Pow100 = round(r$pv, 2), Prc100 = round(r$prc, 2),
                     PrcPct = hp_pt_pctile(r$prc, tp),
                     stringsAsFactors = FALSE)
        }))
    }
    out$by_pitch   <- by_pitch_of(df)
    out$by_pitch_r <- by_pitch_of(df[side == "R", , drop = FALSE])
    out$by_pitch_l <- by_pitch_of(df[side == "L", , drop = FALSE])
  }
  out
}

# ---- Matchup Matrix grades ---------------------------------------------------
# Process-model grades for one batter from a team pitch pool (per-pitch,
# pitch-quality conditioned when the pool carries pitching_xrv). Cached per
# pool fingerprint + batter. Falls back to NULL when the sample is too thin
# so the caller can keep the TruMedia rate-stat pills.
HP_MATRIX_MIN_PITCHES <- 80
.hp_mm_cache <- new.env(parent = emptyenv())
hp_matrix_grades <- function(pool, batter) {
  if (is.null(pool) || !is.data.frame(pool) || nrow(pool) == 0 ||
      !"Batter" %in% names(pool)) return(NULL)
  ck <- paste0(gsub("[^A-Za-z0-9]", "_", batter), "::", nrow(pool))
  hit <- .hp_mm_cache[[ck]]
  if (!is.null(hit)) return(if (identical(hit, "none")) NULL else hit)

  sub <- pool[!is.na(pool$Batter) & pool$Batter == batter, , drop = FALSE]
  if (nrow(sub) < HP_MATRIX_MIN_PITCHES) {
    .hp_mm_cache[[ck]] <- "none"; return(NULL)
  }
  sub <- tryCatch(hp_score_pitches(sub), error = function(e) NULL)
  if (is.null(sub) || !"hp_dv" %in% names(sub)) {
    .hp_mm_cache[[ck]] <- "none"; return(NULL)
  }
  hp_build_league()
  r <- hp_rates(sub)
  if (r$n < HP_MATRIX_MIN_PITCHES) { .hp_mm_cache[[ck]] <- "none"; return(NULL) }
  g <- list(decision = hp_plus(r$dv, "dv"), contact = hp_plus(r$cv, "cv"),
            power = hp_plus(r$pv, "pv"), aggregate = hp_plus(r$prc, "prc"),
            n = r$n, source = "process")
  .hp_mm_cache[[ck]] <- g
  g
}

# ============================================================================
