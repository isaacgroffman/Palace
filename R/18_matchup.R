# ADVANCE: MATCHUP ENGINE (ported from the standalone matchup app)
# Similarity-based batter-vs-pitcher projections over the pooled
# Coastal + P5 + SBC pitch database (full_data_p5_compare).
# ============================================================================

# ---- Run-expectancy reference tables (optional; used by future DRE work) ----

# ---- DRE event weights (pitcher perspective; mean delta run expectancy) ----
weights_vec <- c(
  "Ball"          = -0.09238320509256304,
  "Called Strike" =  0.12344088136926964,
  "Double"        = -0.8527692553531478,
  "Field Out"     =  0.36137424728548606,
  "Foul Ball"     =  0.06953559805880699,
  "HBP"           = -0.4181335791845934,
  "Home Run"      = -1.3884632853898562,
  "Single"        = -0.5489885504819656,
  "Triple"        = -1.1016978193146418,
  "Whiff"         =  0.18879404070261246,
  "NA"            =  0
)

# Map TrackMan PitchCall/PlayResult to the DRE event vocabulary.
# Errors are left neutral ("NA" -> 0) rather than credited as outs.
map_dre_event <- function(pitch_call, play_result) {
  pc <- as.character(pitch_call); pr <- as.character(play_result)
  dplyr::case_when(
    pc %in% c("BallCalled", "BallinDirt", "AutomaticBall",
              "BallIntentional")                                  ~ "Ball",
    pc == "StrikeCalled"                                          ~ "Called Strike",
    pc == "StrikeSwinging"                                        ~ "Whiff",
    pc %in% c("FoulBall", "FoulBallNotFieldable",
              "FoulBallFieldable")                                ~ "Foul Ball",
    pc == "HitByPitch"                                            ~ "HBP",
    pc == "InPlay" & pr == "Single"                               ~ "Single",
    pc == "InPlay" & pr == "Double"                               ~ "Double",
    pc == "InPlay" & pr == "Triple"                               ~ "Triple",
    pc == "InPlay" & pr %in% c("HomeRun", "Homerun")              ~ "Home Run",
    pc == "InPlay" & pr %in% c("Out", "FieldersChoice",
                               "Sacrifice")                       ~ "Field Out",
    TRUE                                                          ~ "NA"
  )
}

# ---- Pricing: actual DRE, every pitch ---------------------------------------
# The matchup engine's currency is straight delta run expectancy — the
# weights_vec above (or mean_DRE where the pool carries it) — for every
# pitch, with no expected-value layer of any kind. No proPitching+ / xRV,
# no xwOBA on contact, no TruMedia xPitchRV. What a pitch actually did is
# what it is worth, and a batted ball is worth its actual outcome.
#
# Pitches are still split into DECISION value (balls, called strikes,
# whiffs, fouls, HBP) and DAMAGE value (balls in play) so the matrix can
# show where a matchup is won. Both sides are actual DRE; the split is
# just a partition of the same pitches, and the two sum exactly to the
# pitch-type and composite scores.

# Per-pitch actual hitter RV (mean_DRE if present, else event mapping).
# Sign flipped to BATTING perspective: + favors the hitter.
.mm_actual_hitter_rv <- function(pool) {
  if (NROW(pool) == 0) return(numeric(0))
  if ("mean_DRE" %in% names(pool)) {
    rv_p <- as.numeric(pool$mean_DRE)
  } else {
    rv_p <- as.numeric(weights_vec[map_dre_event(pool$PitchCall, pool$PlayResult)])
  }
  rv_p[is.na(rv_p)] <- weights_vec[["NA"]]
  -rv_p
}

# Per-pitch price + BIP flag (feeds the Decision/Damage split)
.mm_price_pitch <- function(pool) {
  n <- NROW(pool)
  if (n == 0) return(list(rv = numeric(0), bip = logical(0)))
  pc <- if ("PitchCall" %in% names(pool)) as.character(pool$PitchCall)
        else rep(NA_character_, n)
  list(rv  = .mm_actual_hitter_rv(pool),
       bip = !is.na(pc) & pc == "InPlay")
}

# ---- Engine constants (tunable) ----
MATCHUP_FEATURES <- c("RelSpeed", "InducedVertBreak", "HorzBreak",
                      "SpinRate", "RelSide", "RelHeight", "Extension")
SIMILARITY_THRESHOLD <- 2.0   # z-space Euclidean radius for "similar pitch"
DISTANCE_FLOOR       <- 0.10  # avoids infinite weight at distance 0
SCORE_MU             <- 0     # rv100 center for 0-100 score mapping
SCORE_SD             <- 1.5   # rv100 spread for 0-100 score mapping

# Credibility ramp: n pitches against a stabilization constant k.
.cred <- function(n, k) {
  if (is.null(n) || is.na(n) || n <= 0) return(0)
  n / (n + k)
}

# Unweighted hitter RV/100 over an arbitrary slice of pitches, hybrid
# priced, decomposed into Decision (non-BIP net RV) + Damage (BIP
# expected RV). dec100 + dmg100 == rv100 by construction.
.slice_rv100 <- function(pool) {
  if (is.null(pool) || nrow(pool) == 0)
    return(list(rv100 = NA_real_, n = 0L,
                dec100 = NA_real_, dmg100 = NA_real_))
  pr <- .mm_price_pitch(pool)
  n <- nrow(pool)
  dec <- sum(pr$rv[!pr$bip], na.rm = TRUE) / n * 100
  dmg <- sum(pr$rv[pr$bip],  na.rm = TRUE) / n * 100
  list(rv100 = dec + dmg, n = n, dec100 = dec, dmg100 = dmg)
}

# TrackMan code -> display name lookup, built from the NCAA directory.
teams_data <- tryCatch({
  data.frame(
    trackman_abbr = unlist(ncaa_team_codes, use.names = FALSE),
    team_name     = rep(names(ncaa_team_codes), lengths(ncaa_team_codes)),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::filter(!is.na(trackman_abbr), nzchar(trackman_abbr)) %>%
    dplyr::distinct(trackman_abbr, .keep_all = TRUE)
}, error = function(e) {
  data.frame(trackman_abbr = character(0), team_name = character(0))
})

similar_pool_stats <- function(df) {
  empty <- data.frame(
    n = 0L, AvgEV = NA_real_, BA = NA_real_, SLG = NA_real_,
    `Whiff%` = NA_real_, `Chase%` = NA_real_, `CSW%` = NA_real_,
    `HardHit%` = NA_real_, wOBA = NA_real_, `K%` = NA_real_,
    check.names = FALSE)
  if (is.null(df) || nrow(df) == 0) return(empty)
  has <- function(c) c %in% names(df)
  # Raw-column derivations: the matchup pools (TruMedia / parquet slices)
  # don't carry the engineered is_* flags, so compute everything from
  # PitchCall / PlayResult / KorBB / ExitSpeed / plate location directly.
  pc <- if (has("PitchCall"))  as.character(df$PitchCall)  else rep(NA_character_, nrow(df))
  pr <- if (has("PlayResult")) as.character(df$PlayResult) else rep(NA_character_, nrow(df))
  kb <- if (has("KorBB"))      as.character(df$KorBB)      else rep(NA_character_, nrow(df))

  inplay <- !is.na(pc) & pc == "InPlay"
  swing  <- !is.na(pc) & pc %in% c("StrikeSwinging", "InPlay", "FoulBall",
                                   "FoulBallNotFieldable", "FoulBallFieldable")
  whiff  <- !is.na(pc) & pc == "StrikeSwinging"
  csw    <- !is.na(pc) & pc %in% c("StrikeCalled", "StrikeSwinging")

  chase_pct <- NA_real_
  if (has("PlateLocSide") && has("PlateLocHeight")) {
    pls <- suppressWarnings(as.numeric(df$PlateLocSide))
    plh <- suppressWarnings(as.numeric(df$PlateLocHeight))
    med <- suppressWarnings(median(abs(plh), na.rm = TRUE))
    if (is.finite(med) && med > 7) { pls <- pls / 12; plh <- plh / 12 }
    in_zone <- abs(pls) <= 0.83 & plh >= 1.5 & plh <= 3.5
    ooz <- which(!is.na(in_zone) & !in_zone)
    if (length(ooz) > 0) chase_pct <- round(100 * mean(swing[ooz], na.rm = TRUE), 1)
  }

  hits <- !is.na(pr) & pr %in% c("Single", "Double", "Triple", "HomeRun")
  tb   <- ifelse(is.na(pr), 0, (pr == "Single") + 2 * (pr == "Double") +
                   3 * (pr == "Triple") + 4 * (pr == "HomeRun"))
  k    <- !is.na(kb) & kb == "Strikeout"
  bbw  <- !is.na(kb) & kb == "Walk"
  hbp  <- !is.na(pc) & pc == "HitByPitch"
  outs_bip <- inplay & !is.na(pr) & pr %in% c("Out", "FieldersChoice", "Error")
  sac  <- inplay & !is.na(pr) & pr %in% c("Sacrifice", "SacrificeFly")

  bip <- sum(inplay)
  ab  <- sum(hits) + sum(outs_bip) + sum(k)
  pa  <- ab + sum(bbw) + sum(hbp) + sum(sac)
  sw  <- sum(swing)

  ev <- if (has("ExitSpeed")) suppressWarnings(as.numeric(df$ExitSpeed)) else rep(NA_real_, nrow(df))
  ev_ok <- ev[inplay & is.finite(ev)]

  woba <- if (pa > 0) {
    round((0.69 * sum(bbw) + 0.72 * sum(hbp) +
             0.89 * sum(pr == "Single",  na.rm = TRUE) +
             1.27 * sum(pr == "Double",  na.rm = TRUE) +
             1.62 * sum(pr == "Triple",  na.rm = TRUE) +
             2.10 * sum(pr == "HomeRun", na.rm = TRUE)) / pa, 3)
  } else NA_real_

  data.frame(
    n        = nrow(df),
    AvgEV    = if (length(ev_ok) > 0) round(mean(ev_ok), 1) else NA_real_,
    BA       = if (ab > 0) round(sum(hits) / ab, 3) else NA_real_,
    SLG      = if (ab > 0) round(sum(tb) / ab, 3) else NA_real_,
    `Whiff%` = if (sw > 0) round(100 * sum(whiff) / sw, 1) else NA_real_,
    `Chase%` = chase_pct,
    `CSW%`   = round(100 * mean(csw), 1),
    `HardHit%` = if (length(ev_ok) > 0) round(100 * mean(ev_ok >= 95), 1) else NA_real_,
    wOBA     = woba,
    `K%`     = if (pa > 0) round(100 * sum(k) / pa, 1) else NA_real_,
    check.names = FALSE
  )
}

split_batter_key <- function(k) {
  if (is.null(k) || is.na(k) || k == "") return(list(name = k, team = NULL))
  parts <- strsplit(k, "||", fixed = TRUE)[[1]]
  list(name = parts[1],
       team = if (length(parts) >= 2 && nzchar(parts[2])) parts[2] else NULL)
}
split_pitcher_key <- split_batter_key

# ============================================================================
# MATCHUP ENGINE
# ============================================================================
build_feature_scaler <- function(data) {
  d <- data %>%
    dplyr::filter(
      !is.na(TaggedPitchType), TaggedPitchType != "Other",
      !is.na(RelSpeed), !is.na(InducedVertBreak), !is.na(HorzBreak),
      !is.na(SpinRate), !is.na(RelSide), !is.na(RelHeight), !is.na(Extension)
    )
  if (nrow(d) < 100) return(NULL)
  feat_mat <- as.matrix(d[, MATCHUP_FEATURES])
  list(
    means = colMeans(feat_mat, na.rm = TRUE),
    sds   = apply(feat_mat, 2, sd, na.rm = TRUE),
    n_pitches_in_db = nrow(d)
  )
}

standardize_features <- function(feat_df, scaler) {
  if (is.null(scaler)) return(NULL)
  feat_mat <- as.matrix(feat_df[, MATCHUP_FEATURES])
  sweep(sweep(feat_mat, 2, scaler$means, "-"), 2, scaler$sds, "/")
}

pitcher_pitch_rv100 <- function(data, pitcher_name, pitch_type,
                                pitcher_team = NULL, batter_side = NULL) {
  d <- data %>%
    dplyr::filter(Pitcher == pitcher_name, TaggedPitchType == pitch_type)
  if (!is.null(pitcher_team) && "PitcherTeam" %in% names(d)) {
    d <- d %>% dplyr::filter(PitcherTeam == pitcher_team)
  }
  if (!is.null(batter_side) && !is.na(batter_side) && "BatterSide" %in% names(d)) {
    d_hand <- d %>% dplyr::filter(BatterSide == batter_side)
    if (nrow(d_hand) >= 20) d <- d_hand
  }
  if (nrow(d) == 0) return(list(rv100 = NA_real_, n = 0))
  # Straight DRE over his own pitches of this type.
  hitter_rv <- .mm_price_pitch(d)$rv
  list(rv100 = mean(hitter_rv, na.rm = TRUE) * 100,
       n     = sum(!is.na(hitter_rv)))
}

build_target_profile <- function(data, pitcher_name, pitcher_team = NULL, min_n = 5) {
  d <- data %>%
    dplyr::filter(
      Pitcher == pitcher_name,
      !is.na(TaggedPitchType), TaggedPitchType != "Other",
      dplyr::if_all(dplyr::all_of(MATCHUP_FEATURES), ~ !is.na(.))
    )
  if (!is.null(pitcher_team) && "PitcherTeam" %in% names(d)) {
    d <- d %>% dplyr::filter(PitcherTeam == pitcher_team)
  }
  d %>%
    dplyr::group_by(TaggedPitchType) %>%
    dplyr::summarise(
      n_thrown   = dplyr::n(),
      RelSpeed         = mean(RelSpeed, na.rm = TRUE),
      InducedVertBreak = mean(InducedVertBreak, na.rm = TRUE),
      HorzBreak        = mean(HorzBreak, na.rm = TRUE),
      SpinRate         = mean(SpinRate, na.rm = TRUE),
      RelSide          = mean(RelSide, na.rm = TRUE),
      RelHeight        = mean(RelHeight, na.rm = TRUE),
      Extension        = mean(Extension, na.rm = TRUE),
      PitcherThrows    = dplyr::first(PitcherThrows),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_thrown >= min_n) %>%
    dplyr::mutate(usage_pct = n_thrown / sum(n_thrown))
}

find_similar_pitches_seen <- function(
    data, batter_name, target_row, scaler,
    threshold = SIMILARITY_THRESHOLD, batter_team = NULL
) {
  pitcher_hand <- target_row$PitcherThrows
  .bkey <- if (!is.null(batter_team)) paste0(batter_name, "||", batter_team) else batter_name

  pool <- if (exists("batter_pool_index") && .bkey %in% names(batter_pool_index)) {
    batter_pool_index[[.bkey]] %>% dplyr::filter(PitcherThrows == pitcher_hand)
  } else {
    p <- data %>%
      dplyr::filter(
        Batter == batter_name, PitcherThrows == pitcher_hand,
        !is.na(TaggedPitchType), TaggedPitchType != "Other",
        dplyr::if_all(dplyr::all_of(MATCHUP_FEATURES), ~ !is.na(.))
      )
    if (!is.null(batter_team) && "BatterTeam" %in% names(p)) {
      p <- p %>% dplyr::filter(BatterTeam == batter_team)
    }
    p
  }

  if (nrow(pool) == 0) {
    return(list(similar = data.frame(), pool_size = 0, n_similar = 0))
  }
  pool_std   <- standardize_features(pool, scaler)
  target_std <- standardize_features(target_row, scaler)
  if (is.null(pool_std) || is.null(target_std)) {
    return(list(similar = data.frame(), pool_size = nrow(pool), n_similar = 0))
  }
  diff_mat <- sweep(pool_std, 2, target_std[1, ], "-")
  distances <- sqrt(rowSums(diff_mat^2))
  keep_idx <- which(distances <= threshold)
  if (length(keep_idx) == 0) {
    return(list(similar = data.frame(), pool_size = nrow(pool), n_similar = 0))
  }
  similar <- pool[keep_idx, , drop = FALSE]
  similar$distance <- distances[keep_idx]
  similar$raw_weight <- 1 / (similar$distance + DISTANCE_FLOOR)
  similar$weight <- similar$raw_weight / sum(similar$raw_weight)
  list(similar = similar, pool_size = nrow(pool), n_similar = nrow(similar))
}

calc_weighted_rv100 <- function(similar_df) {
  if (nrow(similar_df) == 0) {
    return(list(rv100 = NA_real_, n = 0, effective_n = NA_real_,
                dec100 = NA_real_, dmg100 = NA_real_))
  }
  pr <- .mm_price_pitch(similar_df)
  w  <- similar_df$weight
  contrib <- pr$rv * w
  dec <- sum(contrib[!pr$bip], na.rm = TRUE) * 100
  dmg <- sum(contrib[pr$bip],  na.rm = TRUE) * 100
  list(rv100 = dec + dmg, n = nrow(similar_df),
       effective_n = 1 / sum(w^2), dec100 = dec, dmg100 = dmg)
}

.calculate_matchup_raw <- function(
    data, pitcher_name, batter_name, scaler,
    pitcher_team = NULL, batter_team = NULL,
    min_pitches_per_type = 5, min_similar = 10,
    pitcher_weight = 0.5, sim_threshold = SIMILARITY_THRESHOLD
) {
  target_profile <- build_target_profile(data, pitcher_name,
                                         pitcher_team = pitcher_team,
                                         min_n = min_pitches_per_type)
  if (nrow(target_profile) == 0) return(NULL)
  pitcher_throws <- target_profile$PitcherThrows[1]
  batter_data <- data %>% dplyr::filter(Batter == batter_name)
  if (!is.null(batter_team) && "BatterTeam" %in% names(batter_data)) {
    batter_data <- batter_data %>% dplyr::filter(BatterTeam == batter_team)
  }
  if (nrow(batter_data) == 0) return(NULL)
  batter_side <- batter_data$BatterSide[1]

  usage_rows <- data %>%
    dplyr::filter(Pitcher == pitcher_name, BatterSide == batter_side,
                  !is.na(TaggedPitchType), TaggedPitchType != "Other")
  if (!is.null(pitcher_team) && "PitcherTeam" %in% names(usage_rows)) {
    usage_rows <- usage_rows %>% dplyr::filter(PitcherTeam == pitcher_team)
  }
  usage_by_hand <- usage_rows %>% dplyr::count(TaggedPitchType, name = "n_vs_hand")

  if (nrow(usage_by_hand) > 0) {
    usage_by_hand$usage_vs_hand <- usage_by_hand$n_vs_hand / sum(usage_by_hand$n_vs_hand)
    target_profile <- target_profile %>%
      dplyr::left_join(usage_by_hand, by = "TaggedPitchType") %>%
      dplyr::mutate(usage_vs_hand = ifelse(is.na(usage_vs_hand), usage_pct, usage_vs_hand))
  } else {
    target_profile$usage_vs_hand <- target_profile$usage_pct
  }
  target_profile$usage_vs_hand <- target_profile$usage_vs_hand / sum(target_profile$usage_vs_hand)

  per_pitch_results <- list()
  for (i in seq_len(nrow(target_profile))) {
    pt <- target_profile$TaggedPitchType[i]
    target_row <- target_profile[i, ]

    # Pitcher prior: his own actual DRE on this pitch type vs this side,
    # credibility weighted by how much of it there is.
    p_pitch <- pitcher_pitch_rv100(data, pitcher_name, pt,
                                   pitcher_team = pitcher_team,
                                   batter_side = batter_side)

    sim_result <- find_similar_pitches_seen(data, batter_name, target_row, scaler,
                                            threshold = sim_threshold,
                                            batter_team = batter_team)
    sim_rv <- if (sim_result$n_similar >= min_similar) {
      calc_weighted_rv100(sim_result$similar)
    } else {
      list(rv100 = NA_real_, n = sim_result$n_similar,
           effective_n = sim_result$n_similar,
           dec100 = NA_real_, dmg100 = NA_real_)
    }

    .bkey <- if (!is.null(batter_team)) paste0(batter_name, "||", batter_team) else batter_name
    bpool <- if (exists("batter_pool_index") && .bkey %in% names(batter_pool_index)) {
      batter_pool_index[[.bkey]] %>% dplyr::filter(PitcherThrows == target_row$PitcherThrows)
    } else {
      bp <- data %>% dplyr::filter(Batter == batter_name,
                                   PitcherThrows == target_row$PitcherThrows)
      if (!is.null(batter_team) && "BatterTeam" %in% names(bp)) {
        bp <- bp %>% dplyr::filter(BatterTeam == batter_team)
      }
      bp
    }
    r_platoon <- .slice_rv100(bpool)

    # Blend the similar-pitch and platoon estimates; Decision and Damage
    # components ride the SAME credibility weights as the total, so the
    # split stays exactly additive after blending.
    blend_weights <- list(similar = 0, platoon = 0)
    blend <- function() {
      acc_rv <- 0; acc_dec <- 0; acc_dmg <- 0; acc_w <- 0; remaining <- 1
      add_level <- function(rv, n, k, cap, tag, dec = NA_real_, dmg = NA_real_) {
        if (is.null(rv) || length(rv) != 1 || is.na(rv) ||
            is.null(n) || length(n) != 1 || is.na(n) || n <= 0) return(invisible())
        w <- .cred(n, k) * remaining
        w <- min(w, cap * remaining)
        if (!is.finite(dec) || !is.finite(dmg)) { dec <- rv; dmg <- 0 }
        acc_rv    <<- acc_rv  + rv  * w
        acc_dec   <<- acc_dec + dec * w
        acc_dmg   <<- acc_dmg + dmg * w
        acc_w     <<- acc_w  + w
        remaining <<- remaining - w
        blend_weights[[tag]] <<- w
      }
      add_level(sim_rv$rv100, sim_rv$n %||% 0, k = 12, cap = 1.00, "similar",
                dec = sim_rv$dec100 %||% NA_real_, dmg = sim_rv$dmg100 %||% NA_real_)
      .pcap <- if ((sim_result$n_similar %||% 0) < 25) 0.60 else 0.15
      add_level(r_platoon$rv100, r_platoon$n, k = 300, cap = .pcap, "platoon",
                dec = r_platoon$dec100 %||% NA_real_, dmg = r_platoon$dmg100 %||% NA_real_)
      if (acc_w <= 0) return(list(rv = NA_real_, dec = NA_real_, dmg = NA_real_))
      list(rv = acc_rv / acc_w, dec = acc_dec / acc_w, dmg = acc_dmg / acc_w)
    }
    bl <- blend()
    blended_rv100 <- bl$rv

    batter_side_rv <- blended_rv100

    if (!is.na(batter_side_rv) && !is.na(p_pitch$rv100) && p_pitch$n > 0 &&
        pitcher_weight > 0) {
      w_pitcher <- pitcher_weight * .cred(p_pitch$n, k = 80)
      w_batter  <- 1 - w_pitcher
      pitch_blended_rv100 <- w_batter * batter_side_rv + w_pitcher * p_pitch$rv100
    } else if (!is.na(batter_side_rv)) {
      pitch_blended_rv100 <- batter_side_rv
    } else {
      # No batter data at all -> the cell drops out of the composite.
      pitch_blended_rv100 <- NA_real_
    }

    per_pitch_results[[pt]] <- list(
      pitch_type = pt, target = target_row,
      n_similar = sim_result$n_similar, pool_size = sim_result$pool_size,
      rv100 = pitch_blended_rv100,
      batter_rv100 = batter_side_rv, pitcher_rv100 = p_pitch$rv100,
      pitcher_n = p_pitch$n, effective_n = sim_rv$effective_n,
      # batter-side Decision / Damage split (sums to batter_rv100)
      dec100 = bl$dec, dmg100 = bl$dmg,
      similar = sim_result$similar,
      usage_vs_hand = target_profile$usage_vs_hand[i]
    )
  }

  # NULL-safe: rv100 can come back NULL (not just NA) on degenerate pools,
  # and sapply over an empty/NULL-holding list returns a list, which is an
  # invalid subscript. Filter() + vapply() are shape-stable.
  .valid_rv <- function(x) {
    v <- x$rv100
    is.numeric(v) && length(v) == 1 && !is.na(v)
  }
  with_data <- Filter(.valid_rv, per_pitch_results)
  if (length(with_data) == 0) {
    composite_rv100 <- NA_real_; coverage <- 0
  } else {
    usages <- vapply(with_data, function(x) as.numeric(x$usage_vs_hand %||% 0), numeric(1))
    rvs    <- vapply(with_data, function(x) as.numeric(x$rv100), numeric(1))
    if (sum(usages, na.rm = TRUE) > 0) {
      composite_rv100 <- sum(rvs * usages, na.rm = TRUE) / sum(usages, na.rm = TRUE)
      coverage <- sum(usages, na.rm = TRUE)
    } else {
      composite_rv100 <- mean(rvs, na.rm = TRUE)
      coverage <- 0
    }
  }
  .n_sim <- if (length(per_pitch_results) > 0) {
    vapply(per_pitch_results, function(x) as.numeric(x$n_similar %||% 0), numeric(1))
  } else numeric(0)

  # Usage-weighted Decision / Damage across pitch types (batter side)
  .wmean <- function(field) {
    if (length(with_data) == 0) return(NA_real_)
    u <- vapply(with_data, function(x) as.numeric(x$usage_vs_hand %||% 0), numeric(1))
    v <- vapply(with_data, function(x) as.numeric(x[[field]] %||% NA_real_), numeric(1))
    ok <- is.finite(v) & is.finite(u)
    if (!any(ok) || sum(u[ok]) <= 0) return(NA_real_)
    sum(v[ok] * u[ok]) / sum(u[ok])
  }
  dec_rv100 <- .wmean("dec100")
  dmg_rv100 <- .wmean("dmg100")

  list(
    pitcher = pitcher_name, batter = batter_name,
    pitcher_throws = pitcher_throws, batter_side = batter_side,
    composite_rv100 = composite_rv100, coverage = coverage,
    per_pitch = per_pitch_results,
    decision_rv100 = dec_rv100, damage_rv100 = dmg_rv100,
    total_similar = sum(.n_sim),
    low_confidence = (sum(.n_sim) < 25)
  )
}

# ---- LRU cache for matchup results ----
.matchup_cache <- new.env(parent = emptyenv())
.matchup_cache_order <- character(0)
.MATCHUP_CACHE_MAX <- 4000

calculate_matchup <- function(
    data, pitcher_name, batter_name, scaler,
    pitcher_team = NULL, batter_team = NULL,
    min_pitches_per_type = 5, min_similar = 10,
    pitcher_weight = 0.5, sim_threshold = SIMILARITY_THRESHOLD
) {
  key <- paste(pitcher_name, pitcher_team %||% "",
               batter_name, batter_team %||% "",
               min_pitches_per_type, min_similar,
               round(pitcher_weight, 3), round(sim_threshold, 3),
               nrow(data), sep = "\u241F")
  if (exists(key, envir = .matchup_cache, inherits = FALSE)) {
    return(get(key, envir = .matchup_cache, inherits = FALSE))
  }
  res <- .calculate_matchup_raw(
    data, pitcher_name, batter_name, scaler,
    pitcher_team = pitcher_team, batter_team = batter_team,
    min_pitches_per_type = min_pitches_per_type, min_similar = min_similar,
    pitcher_weight = pitcher_weight, sim_threshold = sim_threshold
  )
  assign(key, res, envir = .matchup_cache)
  .matchup_cache_order[[length(.matchup_cache_order) + 1]] <<- key
  if (length(.matchup_cache_order) > .MATCHUP_CACHE_MAX) {
    drop_n <- length(.matchup_cache_order) - .MATCHUP_CACHE_MAX
    old <- .matchup_cache_order[seq_len(drop_n)]
    suppressWarnings(rm(list = old, envir = .matchup_cache))
    .matchup_cache_order <<- .matchup_cache_order[-seq_len(drop_n)]
  }
  res
}

# ---- 0-100 score (hardcoded reference) ----
rv_to_matchup_score <- function(rv100) {
  if (is.na(rv100)) return(NA_real_)
  z <- -(rv100 - SCORE_MU) / SCORE_SD   # negative RV = pitcher adv -> higher score
  score <- 100 / (1 + exp(-z * 1.6))
  round(pmax(0, pmin(100, score)), 0)
}

build_matchup_matrix <- function(
    data, pitcher_keys, batter_names, scaler,
    progress_callback = NULL, pitcher_weight = 0.5,
    sim_threshold = SIMILARITY_THRESHOLD
) {
  results <- list()
  total <- length(pitcher_keys) * length(batter_names)
  counter <- 0

  .infer_side_for <- function(bname, bteam) {
    if (is.null(bname) || is.na(bname) || bname == "") return(NA_character_)
    if (!("BatterSide" %in% names(data))) return(NA_character_)
    d <- data[!is.na(data$Batter) & data$Batter == bname, , drop = FALSE]
    if (!is.null(bteam) && !is.na(bteam) && bteam != "" &&
        "BatterTeam" %in% names(d)) {
      d <- d[!is.na(d$BatterTeam) & d$BatterTeam == bteam, , drop = FALSE]
    }
    if (nrow(d) == 0) return(NA_character_)

    # A switch hitter is someone who has actually batted from BOTH sides
    # across multiple at-bats. Counting raw pitches misclassifies long-PA
    # appearances. We collapse to a unique PA identifier (GameUID + Inning
    # + PAofInning if available, else GameUID + Batter + side+count run).
    pa_keys <- if (all(c("GameUID", "Inning", "PAofInning") %in% names(d))) {
      paste(d$GameUID, d$Inning, d$PAofInning, sep = "|")
    } else if ("GameUID" %in% names(d)) {
      paste(d$GameUID, d$BatterSide, sep = "|")
    } else {
      seq_len(nrow(d))
    }
    # One side per PA (mode of side in case of small data noise)
    pa_side <- tapply(d$BatterSide, pa_keys, function(s) {
      s <- s[s %in% c("Left", "Right")]
      if (!length(s)) return(NA_character_)
      tab <- table(s)
      names(tab)[which.max(tab)]
    })
    pa_side <- pa_side[!is.na(pa_side)]
    if (!length(pa_side)) return(NA_character_)

    L_pa <- sum(pa_side == "Left")
    R_pa <- sum(pa_side == "Right")

    # Switch hitter: batted from BOTH sides in at least 2 PAs each
    if (L_pa >= 2 && R_pa >= 2) return("S")
    if (L_pa == 0 && R_pa == 0) return(NA_character_)
    if (L_pa >= R_pa) "L" else "R"
  }

  for (pk in pitcher_keys) {
    parts <- strsplit(pk, "||", fixed = TRUE)[[1]]
    p_name <- parts[1]
    p_team <- if (length(parts) >= 2) parts[2] else NULL
    for (b in batter_names) {
      counter <- counter + 1
      bparts <- strsplit(b, "||", fixed = TRUE)[[1]]
      b_name <- bparts[1]
      b_team <- if (length(bparts) >= 2 && nzchar(bparts[2])) bparts[2] else NULL
      if (!is.null(progress_callback)) {
        progress_callback(counter / total, paste("Processing", p_name, "vs", b_name))
      }
      m <- tryCatch(
        calculate_matchup(data, p_name, b_name, scaler,
                          pitcher_team = p_team, batter_team = b_team,
                          pitcher_weight = pitcher_weight,
                          sim_threshold = sim_threshold),
        error = function(e) NULL
      )
      if (!is.null(m)) {
        score <- rv_to_matchup_score(m$composite_rv100)
        pitch_rvs <- {
          .pp <- Filter(function(x) is.numeric(x$rv100) && length(x$rv100) == 1 &&
                          !is.na(x$rv100), m$per_pitch)
          if (length(.pp) == 0) NA_character_ else
            paste(vapply(.pp, function(x)
              sprintf("%s=%.3f=%d", x$pitch_type, x$rv100,
                      as.integer(x$n_similar %||% 0)), character(1)),
              collapse = ";")
        }
        results[[length(results) + 1]] <- data.frame(
          Pitcher = p_name, PitcherTeam = p_team %||% NA_character_,
          Batter = b_name, BatterTeam = b_team %||% NA_character_,
          PitcherThrows = m$pitcher_throws,
          BatterSide = {
            .inf <- .infer_side_for(b_name, b_team)
            if (is.na(.inf)) substr(m$batter_side, 1, 1) else .inf
          },
          CompositeRV = round(m$composite_rv100, 2),
          MatchupScore = score, PitchRVs = pitch_rvs,
          DecisionRV = round(m$decision_rv100 %||% NA_real_, 2),
          DamageRV   = round(m$damage_rv100 %||% NA_real_, 2),
          TotalSimilar = m$total_similar,
          LowConfidence = isTRUE(m$low_confidence), Coverage = round(m$coverage * 100, 0),
          stringsAsFactors = FALSE
        )
      } else {
        results[[length(results) + 1]] <- data.frame(
          Pitcher = p_name, PitcherTeam = p_team %||% NA_character_,
          Batter = b_name, BatterTeam = b_team %||% NA_character_,
          PitcherThrows = NA, BatterSide = NA,
          CompositeRV = NA_real_, MatchupScore = NA_real_,
          PitchRVs = NA_character_,
          DecisionRV = NA_real_, DamageRV = NA_real_,
          TotalSimilar = 0, LowConfidence = TRUE, Coverage = 0,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  dplyr::bind_rows(results)
}

# ============================================================================
# BETA: MATCHUP+ ENGINE (process-model matrix)
# ----------------------------------------------------------------------------
# Same similarity spine as the production Matchup Matrix -- for every pitch in
# the arm's arsenal, find every pitch this hitter has actually seen inside a
# z-space radius of that pitch's 7-feature profile -- but priced with the
# HITTER PROCESS MODEL instead of straight DRE.
#
# The DRE matrix asks "what run value did he produce against pitches like
# this?". Matchup+ asks "how well did he PROCESS pitches like this?", split
# into the three telescoping layers the hitter page already uses:
#
#   Decision+  E1 - E0   swing/take choice, priced against what the pitch
#                        was worth (zone x count x pitch-quality quintile)
#   Contact+   E2 - E1   given the decision, did the bat get there
#   Power+     A  - E2   given contact, how hard, by the EV x hit-type grid
#   Process+   the sum   -- the cell's headline number
#
# Because every layer prices against the pitch's own expectation, fouling off
# a dotted 0-2 slider grades better than taking the identical pitch for
# strike three, and damage on a good pitch outgrades the same damage on a
# hanger. That is the whole point of the second read: it separates a hitter
# who is beaten by the stuff from one who is beating himself.
#
# Scale is the hitter page's scale, not a 0-100 score: 100 = average against
# the qualified-hitter basis (hp_build_league), 10 = one SD, and HIGHER
# ALWAYS FAVORS THE HITTER in all four numbers. The perspective toggle flips
# the COLOR, never the number, so a Process+ printed here means the same
# thing it means on the hitter page.
#
# Thin cells shrink toward the hitter's own platoon-wide process rates
# (credibility on the effective similar-pitch count) rather than dropping
# out, so the grid stays readable on a mid-season sample.
# ============================================================================

MMP_MIN_SIMILAR <- 10   # fewer similar pitches than this -> platoon baseline only
MMP_CRED_K      <- 60   # shrinkage constant on effective n vs the platoon prior
MMP_MIN_POOL    <- 40   # hitter needs this many priced pitches vs the hand
MMP_LC_SIMILAR  <- 25   # total similar pitches below this -> low-confidence cell

# ---- per-hitter scored + standardized pool (cached) -------------------------
# hp_score_pitches() is the expensive step, so it runs ONCE per hitter/hand
# and we keep only the numeric vectors the engine needs. Every pitch type in
# the arsenal then reuses the same standardized matrix.
.mmp_env <- new.env(parent = emptyenv())

.mmp_batter_pool <- function(data, batter_name, hand, scaler, batter_team = NULL) {
  if (is.null(scaler)) return(NULL)
  ck <- paste(gsub("[^A-Za-z0-9]", "_", batter_name), batter_team %||% "",
              hand %||% "", nrow(data), sep = "\u241F")
  hit <- .mmp_env[[ck]]
  if (!is.null(hit)) return(if (identical(hit, "none")) NULL else hit)
  .none <- function() { .mmp_env[[ck]] <- "none"; NULL }

  pool <- data %>%
    dplyr::filter(
      Batter == batter_name,
      !is.na(TaggedPitchType), TaggedPitchType != "Other",
      dplyr::if_all(dplyr::all_of(MATCHUP_FEATURES), ~ !is.na(.)))
  if (!is.null(batter_team) && nzchar(batter_team) && "BatterTeam" %in% names(pool))
    pool <- pool %>% dplyr::filter(BatterTeam == batter_team)
  if (!is.null(hand) && !is.na(hand) && "PitcherThrows" %in% names(pool))
    pool <- pool[!is.na(pool$PitcherThrows) & pool$PitcherThrows == hand, , drop = FALSE]

  need <- c("PitchCall", "Balls", "Strikes", "PlateLocSide", "PlateLocHeight")
  if (nrow(pool) < MMP_MIN_POOL || !all(need %in% names(pool))) return(.none())

  scored <- tryCatch(hp_score_pitches(pool), error = function(e) NULL)
  if (is.null(scored) || !"hp_dv" %in% names(scored)) return(.none())
  scored <- scored[is.finite(scored$hp_dv), , drop = FALSE]
  if (nrow(scored) < MMP_MIN_POOL) return(.none())

  std <- tryCatch(standardize_features(scored, scaler), error = function(e) NULL)
  if (is.null(std) || nrow(std) != nrow(scored)) return(.none())

  n <- nrow(scored)
  fsum <- function(v) { ok <- is.finite(v); if (!any(ok)) 0 else sum(v[ok]) }
  out <- list(
    n   = n,
    std = std,
    dv  = scored$hp_dv, cv = scored$hp_cv,
    pv  = scored$hp_pv, rv = scored$hp_rv,
    pt  = as.character(scored$TaggedPitchType),
    # platoon prior: his process rates against EVERYTHING from this hand
    base = list(dv  = 100 * fsum(scored$hp_dv) / n,
                cv  = 100 * fsum(scored$hp_cv) / n,
                pv  = 100 * fsum(scored$hp_pv) / n,
                prc = 100 * fsum(scored$hp_rv) / n))
  .mmp_env[[ck]] <- out
  out
}

# Similarity-weighted process rates per 100 pitches over one slice.
# dv + cv + pv == prc by construction, so the sub-pills always sum to the
# headline number after the plus conversion.
.mmp_rates_w <- function(dv, cv, pv, rv, w) {
  W <- sum(w, na.rm = TRUE)
  if (!is.finite(W) || W <= 0) return(NULL)
  s <- function(v) { ok <- is.finite(v); if (!any(ok)) 0 else sum(v[ok] * w[ok]) }
  list(dv = 100 * s(dv) / W, cv = 100 * s(cv) / W,
       pv = 100 * s(pv) / W, prc = 100 * s(rv) / W)
}

# hp_plus() carries the league-mean's name attribute through; unname() here
# or every data.frame built from these picks up "m...1" style rownames.
.mmp_plus_block <- function(r) {
  if (is.null(r)) return(list(dv = NA_real_, cv = NA_real_,
                              pv = NA_real_, prc = NA_real_))
  u <- function(x) unname(as.numeric(x %||% NA_real_))
  list(dv  = u(hp_plus(r$dv,  "dv")),  cv  = u(hp_plus(r$cv,  "cv")),
       pv  = u(hp_plus(r$pv,  "pv")),  prc = u(hp_plus(r$prc, "prc")))
}

.calculate_matchup_plus_raw <- function(
    data, pitcher_name, batter_name, scaler,
    pitcher_team = NULL, batter_team = NULL,
    min_pitches_per_type = 5, min_similar = MMP_MIN_SIMILAR,
    sim_threshold = SIMILARITY_THRESHOLD, shrink_k = MMP_CRED_K
) {
  if (!hp_build_league()) return(NULL)

  target_profile <- build_target_profile(data, pitcher_name,
                                         pitcher_team = pitcher_team,
                                         min_n = min_pitches_per_type)
  if (is.null(target_profile) || nrow(target_profile) == 0) return(NULL)
  pitcher_throws <- target_profile$PitcherThrows[1]

  batter_data <- data %>% dplyr::filter(Batter == batter_name)
  if (!is.null(batter_team) && "BatterTeam" %in% names(batter_data))
    batter_data <- batter_data %>% dplyr::filter(BatterTeam == batter_team)
  if (nrow(batter_data) == 0) return(NULL)
  batter_side <- batter_data$BatterSide[1]

  P <- .mmp_batter_pool(data, batter_name, pitcher_throws, scaler, batter_team)
  if (is.null(P)) return(NULL)

  # ---- usage vs this hitter's side (same weighting the DRE engine uses) ----
  usage_rows <- data %>%
    dplyr::filter(Pitcher == pitcher_name, BatterSide == batter_side,
                  !is.na(TaggedPitchType), TaggedPitchType != "Other")
  if (!is.null(pitcher_team) && "PitcherTeam" %in% names(usage_rows))
    usage_rows <- usage_rows %>% dplyr::filter(PitcherTeam == pitcher_team)
  usage_by_hand <- usage_rows %>% dplyr::count(TaggedPitchType, name = "n_vs_hand")
  if (nrow(usage_by_hand) > 0) {
    usage_by_hand$usage_vs_hand <- usage_by_hand$n_vs_hand / sum(usage_by_hand$n_vs_hand)
    target_profile <- target_profile %>%
      dplyr::left_join(usage_by_hand, by = "TaggedPitchType") %>%
      dplyr::mutate(usage_vs_hand = ifelse(is.na(usage_vs_hand), usage_pct, usage_vs_hand))
  } else {
    target_profile$usage_vs_hand <- target_profile$usage_pct
  }
  target_profile$usage_vs_hand <-
    target_profile$usage_vs_hand / sum(target_profile$usage_vs_hand)

  # ---- per pitch type: similar pitches seen -> weighted process rates ----
  per_pitch <- list()
  for (i in seq_len(nrow(target_profile))) {
    pt  <- target_profile$TaggedPitchType[i]
    tgt <- tryCatch(standardize_features(target_profile[i, , drop = FALSE], scaler),
                    error = function(e) NULL)
    if (is.null(tgt)) next

    dist  <- sqrt(rowSums(sweep(P$std, 2, tgt[1, ], "-")^2))
    keep  <- which(is.finite(dist) & dist <= sim_threshold)
    n_sim <- length(keep)

    r_sim <- NULL; eff_n <- 0
    if (n_sim > 0) {
      w <- 1 / (dist[keep] + DISTANCE_FLOOR)
      w <- w / sum(w)
      eff_n <- 1 / sum(w^2)
      r_sim <- .mmp_rates_w(P$dv[keep], P$cv[keep], P$pv[keep], P$rv[keep], w)
    }

    # credibility blend toward the platoon prior; every layer rides the SAME
    # weight so the three sub-pills keep summing to Process+
    cw <- if (is.null(r_sim) || n_sim < min_similar) 0 else .cred(eff_n, shrink_k)
    mix <- function(f) {
      b <- P$base[[f]]
      if (is.null(r_sim) || !is.finite(r_sim[[f]])) return(b)
      cw * r_sim[[f]] + (1 - cw) * b
    }
    rates <- list(dv = mix("dv"), cv = mix("cv"), pv = mix("pv"), prc = mix("prc"))

    per_pitch[[pt]] <- list(
      pitch_type = pt, n_similar = n_sim, effective_n = eff_n,
      sim_weight = cw, rates = rates, plus = .mmp_plus_block(rates),
      raw_plus = .mmp_plus_block(r_sim),
      usage_vs_hand = target_profile$usage_vs_hand[i],
      velo = target_profile$RelSpeed[i],
      ivb  = target_profile$InducedVertBreak[i],
      hb   = target_profile$HorzBreak[i])
  }
  if (length(per_pitch) == 0) return(NULL)

  # ---- arsenal composite: usage-weighted rates, THEN the plus conversion ----
  # (plus is linear in the rate, so this equals a usage-weighted average of
  #  the per-pitch plus values -- doing it on rates keeps prc additive.)
  u <- vapply(per_pitch, function(x) as.numeric(x$usage_vs_hand %||% 0), numeric(1))
  comp_rate <- function(f) {
    v <- vapply(per_pitch, function(x) as.numeric(x$rates[[f]] %||% NA_real_), numeric(1))
    ok <- is.finite(v) & is.finite(u)
    if (!any(ok) || sum(u[ok]) <= 0) return(NA_real_)
    sum(v[ok] * u[ok]) / sum(u[ok])
  }
  rates <- list(dv = comp_rate("dv"), cv = comp_rate("cv"),
                pv = comp_rate("pv"), prc = comp_rate("prc"))
  plus <- .mmp_plus_block(rates)

  n_sim_vec <- vapply(per_pitch, function(x) as.numeric(x$n_similar %||% 0), numeric(1))
  cov_vec   <- u[is.finite(vapply(per_pitch,
                  function(x) as.numeric(x$rates$prc %||% NA_real_), numeric(1)))]

  list(
    pitcher = pitcher_name, batter = batter_name,
    pitcher_throws = pitcher_throws, batter_side = batter_side,
    rates = rates, plus = plus, per_pitch = per_pitch,
    pool_n = P$n, base = P$base,
    total_similar = sum(n_sim_vec),
    coverage = sum(cov_vec, na.rm = TRUE),
    low_confidence = (sum(n_sim_vec) < MMP_LC_SIMILAR))
}

# ---- LRU cache (mirrors calculate_matchup) ----
.mmp_cache <- new.env(parent = emptyenv())
.mmp_cache_order <- character(0)
.MMP_CACHE_MAX <- 4000

calculate_matchup_plus <- function(
    data, pitcher_name, batter_name, scaler,
    pitcher_team = NULL, batter_team = NULL,
    min_pitches_per_type = 5, min_similar = MMP_MIN_SIMILAR,
    sim_threshold = SIMILARITY_THRESHOLD, shrink_k = MMP_CRED_K
) {
  key <- paste(pitcher_name, pitcher_team %||% "",
               batter_name, batter_team %||% "",
               min_pitches_per_type, min_similar,
               round(sim_threshold, 3), round(shrink_k, 1),
               nrow(data), sep = "\u241F")
  if (exists(key, envir = .mmp_cache, inherits = FALSE))
    return(get(key, envir = .mmp_cache, inherits = FALSE))
  res <- .calculate_matchup_plus_raw(
    data, pitcher_name, batter_name, scaler,
    pitcher_team = pitcher_team, batter_team = batter_team,
    min_pitches_per_type = min_pitches_per_type, min_similar = min_similar,
    sim_threshold = sim_threshold, shrink_k = shrink_k)
  assign(key, res, envir = .mmp_cache)
  .mmp_cache_order[[length(.mmp_cache_order) + 1]] <<- key
  if (length(.mmp_cache_order) > .MMP_CACHE_MAX) {
    drop_n <- length(.mmp_cache_order) - .MMP_CACHE_MAX
    old <- .mmp_cache_order[seq_len(drop_n)]
    suppressWarnings(rm(list = old, envir = .mmp_cache))
    .mmp_cache_order <<- .mmp_cache_order[-seq_len(drop_n)]
  }
  res
}

# One row per pitcher x batter. PitchPlus packs the per-pitch-type breakdown
# as "PT=prc=dec=con=pow=nsim;..." the same way PitchRVs does on the DRE
# matrix, so the renderer stays a pure string parse.
build_matchup_plus_matrix <- function(
    data, pitcher_keys, batter_names, scaler,
    progress_callback = NULL, sim_threshold = SIMILARITY_THRESHOLD,
    shrink_k = MMP_CRED_K
) {
  results <- list()
  total <- max(1L, length(pitcher_keys) * length(batter_names))
  counter <- 0

  .pack <- function(pp) {
    if (length(pp) == 0) return(NA_character_)
    ok <- Filter(function(x) is.finite(x$plus$prc %||% NA_real_), pp)
    if (length(ok) == 0) return(NA_character_)
    ok <- ok[order(-vapply(ok, function(x) as.numeric(x$usage_vs_hand %||% 0),
                           numeric(1)))]
    paste(vapply(ok, function(x) sprintf(
      "%s=%.1f=%.1f=%.1f=%.1f=%d", x$pitch_type,
      x$plus$prc, x$plus$dv %||% NA_real_, x$plus$cv %||% NA_real_,
      x$plus$pv %||% NA_real_, as.integer(x$n_similar %||% 0)), character(1)),
      collapse = ";")
  }

  for (pk in pitcher_keys) {
    parts  <- strsplit(pk, "||", fixed = TRUE)[[1]]
    p_name <- parts[1]
    p_team <- if (length(parts) >= 2 && nzchar(parts[2])) parts[2] else NULL
    for (b in batter_names) {
      counter <- counter + 1
      bparts <- strsplit(b, "||", fixed = TRUE)[[1]]
      b_name <- bparts[1]
      b_team <- if (length(bparts) >= 2 && nzchar(bparts[2])) bparts[2] else NULL
      if (!is.null(progress_callback))
        progress_callback(counter / total,
                          paste("Matchup+", p_name, "vs", b_name))
      m <- tryCatch(
        calculate_matchup_plus(data, p_name, b_name, scaler,
                               pitcher_team = p_team, batter_team = b_team,
                               sim_threshold = sim_threshold,
                               shrink_k = shrink_k),
        error = function(e) NULL)
      results[[length(results) + 1]] <- if (!is.null(m)) data.frame(
        Pitcher = p_name, PitcherTeam = p_team %||% NA_character_,
        Batter = b_name, BatterTeam = b_team %||% NA_character_,
        PitcherThrows = m$pitcher_throws %||% NA_character_,
        BatterSide = substr(m$batter_side %||% "", 1, 1),
        ProcessPlus  = round(m$plus$prc %||% NA_real_),
        DecisionPlus = round(m$plus$dv  %||% NA_real_),
        ContactPlus  = round(m$plus$cv  %||% NA_real_),
        PowerPlus    = round(m$plus$pv  %||% NA_real_),
        PitchPlus = .pack(m$per_pitch),
        PoolN = m$pool_n %||% 0L,
        TotalSimilar = m$total_similar %||% 0,
        LowConfidence = isTRUE(m$low_confidence),
        Coverage = round((m$coverage %||% 0) * 100),
        stringsAsFactors = FALSE
      ) else data.frame(
        Pitcher = p_name, PitcherTeam = p_team %||% NA_character_,
        Batter = b_name, BatterTeam = b_team %||% NA_character_,
        PitcherThrows = NA_character_, BatterSide = NA_character_,
        ProcessPlus = NA_real_, DecisionPlus = NA_real_,
        ContactPlus = NA_real_, PowerPlus = NA_real_,
        PitchPlus = NA_character_, PoolN = 0L, TotalSimilar = 0,
        LowConfidence = TRUE, Coverage = 0, stringsAsFactors = FALSE)
    }
  }
  dplyr::bind_rows(results)
}

# ============================================================================
# BETA: UNIFIED MATCHUP MATRIX
# ----------------------------------------------------------------------------
# One grid, three different kinds of evidence about the same pitcher x batter
# cell, each answering a different question:
#
#   PREDICTED   Process+ off the process model. How well does this hitter
#               HANDLE pitches shaped like this arm's? Outcome-independent --
#               a 105 mph liner at the shortstop counts as a 105 mph liner.
#               Stuff is already implied: the similarity search draws the
#               nastiest comparable pitches out of the hitter's own history,
#               so beating a filthy shape is what a high number means.
#
#   ACTUAL      The production DRE matrix. What run value did this hitter
#               produce against pitches like this? Descriptive, luck-inclusive,
#               and it carries a pitcher term the process read does not.
#
#   HEAD-TO-HEAD  What has literally happened between these two men. Usually
#               a handful of PAs, often zero -- shown because coaches ask, and
#               labelled small so nobody mistakes 2-for-3 for a projection.
#
#   DIVERGENCE  Where PREDICTED and ACTUAL disagree, standardized within the
#               grid so the two scales are comparable. Positive means the
#               process read likes the hitter more than his results do --
#               he has been hitting into outs, and the DRE cell is likely to
#               regress toward him. Negative means the results flatter him.
#               This is the number neither grid could produce alone.
#
# Nothing here recomputes anything: it joins the two existing engines and adds
# the head-to-head tally, so both grids stay independently auditable.
# ============================================================================

# Actual head-to-head between one arm and one bat. Deliberately thin -- PA
# counts at this granularity are tiny and the display treats them as anecdote.
.mmu_head_to_head <- function(data, pitcher_name, batter_name,
                              pitcher_team = NULL, batter_team = NULL) {
  empty <- list(pa = 0L, pitches = 0L, h = 0L, hr = 0L, k = 0L, bb = 0L,
                rv100 = NA_real_)
  if (is.null(data) || nrow(data) == 0) return(empty)
  d <- data[!is.na(data$Pitcher) & data$Pitcher == pitcher_name &
              !is.na(data$Batter) & data$Batter == batter_name, , drop = FALSE]
  if (!is.null(pitcher_team) && "PitcherTeam" %in% names(d))
    d <- d[!is.na(d$PitcherTeam) & d$PitcherTeam == pitcher_team, , drop = FALSE]
  if (!is.null(batter_team) && "BatterTeam" %in% names(d))
    d <- d[!is.na(d$BatterTeam) & d$BatterTeam == batter_team, , drop = FALSE]
  if (nrow(d) == 0) return(empty)

  pa_key <- if (all(c("GameID", "Inning", "PAofInning") %in% names(d)))
    paste(d$GameID, d$Inning, d$PAofInning) else NULL
  pr <- if ("PlayResult" %in% names(d)) as.character(d$PlayResult) else character(nrow(d))
  kb <- if ("KorBB" %in% names(d)) as.character(d$KorBB) else character(nrow(d))
  rv <- tryCatch(.mm_actual_hitter_rv(d), error = function(e) rep(NA_real_, nrow(d)))
  ok <- is.finite(rv)

  list(pa      = if (is.null(pa_key)) NA_integer_ else length(unique(pa_key)),
       pitches = nrow(d),
       h  = sum(pr %in% c("Single", "Double", "Triple", "HomeRun"), na.rm = TRUE),
       hr = sum(pr == "HomeRun", na.rm = TRUE),
       k  = sum(kb == "Strikeout", na.rm = TRUE),
       bb = sum(kb == "Walk", na.rm = TRUE),
       rv100 = if (any(ok)) 100 * mean(rv[ok]) else NA_real_)
}

# Join the two engines into one frame. Column names are kept distinct
# (ProcessPlus vs MatchupScore) so a downstream reader can never confuse the
# predictive number with the descriptive one.
build_unified_matchup_matrix <- function(
    data, pitcher_keys, batter_names, scaler,
    progress_callback = NULL, sim_threshold = SIMILARITY_THRESHOLD,
    shrink_k = MMP_CRED_K, pitcher_weight = 0.5,
    want_dre = TRUE, want_h2h = TRUE
) {
  step <- function(f, m) if (!is.null(progress_callback)) progress_callback(f, m)

  step(0.02, "Building predicted grid (process model)...")
  mp <- build_matchup_plus_matrix(
    data, pitcher_keys, batter_names, scaler,
    progress_callback = function(f, m) step(0.02 + 0.50 * f, m),
    sim_threshold = sim_threshold, shrink_k = shrink_k)
  if (is.null(mp) || nrow(mp) == 0) return(mp)

  mp$.k <- paste0(mp$Pitcher, "\u241F", ifelse(is.na(mp$PitcherTeam), "", mp$PitcherTeam),
                  "\u241F", mp$Batter, "\u241F", ifelse(is.na(mp$BatterTeam), "", mp$BatterTeam))

  if (isTRUE(want_dre)) {
    step(0.54, "Building actual grid (DRE)...")
    md <- tryCatch(build_matchup_matrix(
      data, pitcher_keys, batter_names, scaler,
      progress_callback = function(f, m) step(0.54 + 0.38 * f, m),
      pitcher_weight = pitcher_weight, sim_threshold = sim_threshold),
      error = function(e) NULL)
    if (!is.null(md) && nrow(md) > 0) {
      md$.k <- paste0(md$Pitcher, "\u241F", ifelse(is.na(md$PitcherTeam), "", md$PitcherTeam),
                      "\u241F", md$Batter, "\u241F", ifelse(is.na(md$BatterTeam), "", md$BatterTeam))
      cols <- intersect(c(".k", "MatchupScore", "CompositeRV", "DecisionRV",
                          "DamageRV", "PitchRVs"), names(md))
      keep <- md[!duplicated(md$.k), cols, drop = FALSE]
      names(keep)[names(keep) == "LowConfidence"] <- "DreLowConf"
      mp <- dplyr::left_join(mp, keep, by = ".k")
    }
  }
  for (cc in c("MatchupScore", "CompositeRV", "DecisionRV", "DamageRV"))
    if (!cc %in% names(mp)) mp[[cc]] <- NA_real_
  if (!"PitchRVs" %in% names(mp)) mp$PitchRVs <- NA_character_

  # ---- ORIENTATION ------------------------------------------------------
  # rv_to_matchup_score is PITCHER-facing by construction ("negative RV =
  # pitcher adv -> higher score"), while Process+ is hitter-facing. Joining
  # them raw points the two metrics in opposite directions, which inverts the
  # colours and turns the divergence subtraction into an addition. Flip once,
  # here, so everything downstream is unambiguously hitter-facing: higher is
  # better for the bat, in every column, always. MatchupScore is retained
  # untouched so the value still matches the production tab.
  mp$DreHitter <- ifelse(is.finite(mp$MatchupScore), 100 - mp$MatchupScore, NA_real_)

  if (isTRUE(want_h2h)) {
    step(0.94, "Tallying head-to-head history...")
    h <- lapply(seq_len(nrow(mp)), function(i)
      .mmu_head_to_head(data, mp$Pitcher[i], mp$Batter[i],
                        pitcher_team = if (is.na(mp$PitcherTeam[i])) NULL else mp$PitcherTeam[i],
                        batter_team  = if (is.na(mp$BatterTeam[i]))  NULL else mp$BatterTeam[i]))
    mp$H2H_PA      <- vapply(h, function(x) as.numeric(x$pa %||% NA_real_), numeric(1))
    mp$H2H_Pitches <- vapply(h, function(x) as.numeric(x$pitches %||% 0), numeric(1))
    mp$H2H_H       <- vapply(h, function(x) as.numeric(x$h %||% 0), numeric(1))
    mp$H2H_HR      <- vapply(h, function(x) as.numeric(x$hr %||% 0), numeric(1))
    mp$H2H_K       <- vapply(h, function(x) as.numeric(x$k %||% 0), numeric(1))
    mp$H2H_BB      <- vapply(h, function(x) as.numeric(x$bb %||% 0), numeric(1))
    mp$H2H_RV100   <- vapply(h, function(x) as.numeric(x$rv100 %||% NA_real_), numeric(1))
  } else {
    for (cc in c("H2H_PA","H2H_Pitches","H2H_H","H2H_HR","H2H_K","H2H_BB","H2H_RV100"))
      mp[[cc]] <- NA_real_
  }

  mp$.k <- NULL
  step(1, "Done.")
  mp
}

# Process+ -> cell color. grade_pill_color is already the 40-160 red/white/
# green ramp with high = good for the HITTER; pitcher perspective mirrors the
# value around 100 so green always means "this side is winning".
mmp_plus_color <- function(v, view = "hitter", center = 100, spread = 1) {
  if (is.null(v) || length(v) != 1 || !is.finite(v)) return("#EDEFF1")
  # `center`/`spread` support the relative mode: re-express the value on a
  # 100-centred scale using the GRID's own mean and SD, so the colour answers
  # "which of these matchups is best" instead of "is this lineup good". In
  # absolute mode (center = 100, spread = 1) this is the identity.
  z <- if (is.finite(center) && is.finite(spread) && spread > 0)
    100 + (v - center) / spread * 10 else v
  grade_pill_color(if (identical(view, "pitcher")) 200 - z else z)
}

# Pitch-type abbreviations (shared by the matrix PDF and game-plan notes;
# mirrors the Excel report's .xl_abbrev map)
pitch_abbrev <- function(pt) {
  map <- c("Fastball"="FB","Four-Seam"="FB","FourSeamFastBall"="FB",
           "4-Seam Fastball"="FB","FF"="FB","Sinker"="SNK","Two-Seam"="SNK",
           "TwoSeamFastBall"="SNK","2-Seam Fastball"="SNK","SI"="SNK",
           "Slider"="SL","Sweeper"="SWP","Cutter"="CT","Curveball"="CB",
           "CurveBall"="CB","Knuckle Curve"="KC","Slurve"="SLV",
           "ChangeUp"="CH","Changeup"="CH","Splitter"="SPL")
  out <- map[as.character(pt)]
  ifelse(is.na(out), toupper(substr(as.character(pt), 1, 3)), out)
}

# Per-stat pill color for the matchup detail table. mid/spread are rough
# college baselines; direction flags whether HIGH values favor the PITCHER.
.MM_STAT_SCALE <- list(
  AvgEV      = c(mid = 87,    spread = 4,     dir = -1),
  BA         = c(mid = 0.270, spread = 0.060, dir = -1),
  SLG        = c(mid = 0.420, spread = 0.110, dir = -1),
  `Whiff%`   = c(mid = 24,    spread = 8,     dir =  1),
  `Chase%`   = c(mid = 26,    spread = 7,     dir =  1),
  `CSW%`     = c(mid = 28,    spread = 5,     dir =  1),
  `HardHit%` = c(mid = 32,    spread = 12,    dir = -1),
  wOBA       = c(mid = 0.360, spread = 0.070, dir = -1),
  `K%`       = c(mid = 22,    spread = 8,     dir =  1)
)
mm_stat_color <- function(stat, value) {
  if (is.null(value) || is.na(value)) return("#EDEFF0")
  sc <- .MM_STAT_SCALE[[stat]]
  if (is.null(sc)) return("#EDEFF0")
  z <- sc[["dir"]] * (as.numeric(value) - sc[["mid"]]) / sc[["spread"]]
  matchup_score_color(100 / (1 + exp(-z * 1.4)))
}

matchup_score_color <- function(score) {
  if (is.na(score)) return("#E0E0E0")
  # green <- white -> red, NO yellow midtone: neutral is pure white so
  # only real advantages carry color.
  pal <- scales::gradient_n_pal(c("#E1463E", "#FFFFFF", "#1B7837"))
  pal(score / 100)
}

# Text color for any pill/cell painted with the gradient above: perceived
# luminance of the background decides between dark slate and white, so a
# super-dark green (or deep red) automatically flips to white text.
pill_text_color <- function(bg, dark = "#1F2937") {
  if (is.null(bg) || length(bg) != 1 || is.na(bg)) return(dark)
  rgb <- tryCatch(grDevices::col2rgb(bg), error = function(e) NULL)
  if (is.null(rgb)) return(dark)
  lum <- (0.299 * rgb[1, 1] + 0.587 * rgb[2, 1] + 0.114 * rgb[3, 1]) / 255
  if (lum < 0.47) "#FFFFFF" else dark
}

# ============================================================================
# PLOTLY DETAIL CHARTS (ggplotly-based)
#   Shape encodes pitch result; color encodes pitch type.
# ============================================================================
.shape_map_results <- c(
  "Ball" = 1, "2B" = 18, "Foul" = 2, "HBP" = 10, "Sac" = 3,
  "1B" = 19, "Called" = 5, "Whiff" = 8, "3B" = 17, "HR" = 15,
  "Out" = 4, "Error" = 0, "Other" = 16
)

# Decorate a similar-pitch frame with ResultDisplay, TeamDisp, and a hover tip
.decorate_sim <- function(sim) {
  if (is.null(sim) || nrow(sim) == 0) return(sim)
  team_lookup <- if (nrow(teams_data) > 0) {
    teams_data %>%
      dplyr::select(trackman_abbr, team_name) %>%
      dplyr::filter(!is.na(trackman_abbr), trackman_abbr != "")
  } else data.frame(trackman_abbr = character(), team_name = character())

  sim <- sim %>%
    dplyr::left_join(team_lookup, by = c("PitcherTeam" = "trackman_abbr")) %>%
    dplyr::mutate(
      TeamDisp = dplyr::coalesce(team_name, PitcherTeam, "Unknown"),
      ResultDisplay = dplyr::case_when(
        PitchCall %in% c("BallCalled", "BallinDirt")            ~ "Ball",
        PlayResult == "Double"                                   ~ "2B",
        PitchCall %in% c("FoulBall", "FoulBallNotFieldable",
                         "FoulBallFieldable")                    ~ "Foul",
        PitchCall == "HitByPitch"                               ~ "HBP",
        PlayResult %in% c("Sacrifice", "SacrificeFly")          ~ "Sac",
        PlayResult == "Single"                                  ~ "1B",
        PitchCall == "StrikeCalled"                             ~ "Called",
        PitchCall == "StrikeSwinging"                           ~ "Whiff",
        PlayResult == "Triple"                                  ~ "3B",
        PlayResult == "HomeRun"                                 ~ "HR",
        PlayResult == "Out"                                     ~ "Out",
        PlayResult == "Error"                                   ~ "Error",
        TRUE                                                    ~ "Other"
      ),
      .tip = paste0(
        "Pitcher: ",    Pitcher,                                "<br>",
        "Team: ",       TeamDisp,                               "<br>",
        "Date: ",       ifelse(is.na(Date), "-",
                               as.character(as.Date(Date))),    "<br>",
        "Inning: ",     ifelse(is.na(Inning), "-", Inning),     "<br>",
        "Velo: ",       ifelse(is.na(RelSpeed), "-",
                               paste0(round(RelSpeed, 1), " mph")), "<br>",
        "Pitch: ",      TaggedPitchType,                        "<br>",
        "Pitch Call: ", PitchCall,
        ifelse(PitchCall == "InPlay" & !is.na(PlayResult),
               paste0("<br>Result: ", PlayResult), ""),
        ifelse(PitchCall == "InPlay" & "ExitSpeed" %in% names(sim) &
                 !is.na(ExitSpeed),
               paste0("<br>EV: ", round(ExitSpeed, 1), " mph"), "")
      )
    )
  sim
}

# Movement chart (HB vs IVB) for one pitch type's similar pool
.detail_movement_plotly <- function(sim, target_row, pt) {
  if (is.null(sim) || nrow(sim) == 0) {
    return(plotly::ggplotly(
      ggplot() + theme_void() +
        annotate("text", x = 0, y = 0, label = "No pitches match filters",
                 size = 4, color = "#888")))
  }
  sim <- .decorate_sim(sim)
  present_pitches <- intersect(names(pitch_colors),
                               unique(sim$TaggedPitchType))
  sim$TaggedPitchType <- factor(sim$TaggedPitchType, levels = present_pitches)
  cols <- pitch_colors[present_pitches]
  shp <- .shape_map_results[names(.shape_map_results) %in%
                              unique(sim$ResultDisplay)]

  g <- ggplot(sim, aes(x = HorzBreak, y = InducedVertBreak, text = .tip)) +
    geom_hline(yintercept = 0, color = "gray70", linewidth = .3) +
    geom_vline(xintercept = 0, color = "gray70", linewidth = .3) +
    geom_point(aes(color = TaggedPitchType, shape = ResultDisplay,
                   size = weight),
               alpha = .78, stroke = .4) +
    scale_color_manual(values = cols, name = "Pitch", drop = TRUE) +
    scale_shape_manual(values = shp, name = "Result", drop = TRUE) +
    scale_size_continuous(range = c(1.8, 5), guide = "none") +
    coord_cartesian(xlim = c(-30, 30), ylim = c(-30, 30)) +
    labs(x = "HB (in)", y = "IVB (in)") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#eee", linewidth = .2))

  if (!is.null(target_row) && nrow(target_row) > 0) {
    g <- g + suppressWarnings(geom_point(
      data = target_row,
      aes(x = HorzBreak, y = InducedVertBreak, text = "TARGET PITCH"),
      shape = 23, fill = pitch_colors[[pt]] %||% "red",
      color = "black", size = 5, stroke = 1.4, inherit.aes = FALSE))
  }
  plotly::ggplotly(g, tooltip = "text") %>%
    plotly::config(displaylogo = FALSE, displayModeBar = FALSE)
}

# Location chart (catcher's view). Units are FEET. NCAA zone:
# horizontal +/- 0.83 ft, vertical 1.5 - 3.5 ft. We use coord_cartesian (not
# coord_fixed with xlim) because coord_fixed silently drops out-of-range
# points; we want every pitch on the plot.
.detail_location_plotly <- function(sim, pt) {
  blank <- function(msg) plotly::ggplotly(
    ggplot() + theme_void() +
      annotate("text", x = 0, y = 0, label = msg, size = 4, color = "#888"))
  if (is.null(sim) || nrow(sim) == 0) return(blank("No pitches match filters"))
  if (!all(c("PlateLocSide", "PlateLocHeight") %in% names(sim)))
    return(blank("No location columns"))

  sim <- sim[is.finite(sim$PlateLocSide) & is.finite(sim$PlateLocHeight), ,
             drop = FALSE]
  if (nrow(sim) == 0) return(blank("No location data"))

  # Defensive unit check: if the values still look like inches (typical
  # pitch heights are 1-5 ft, anything > 7 means inches), auto-convert here.
  # The pooled frame stores inches; this makes the plot unit-proof.
  h_med <- median(abs(sim$PlateLocHeight), na.rm = TRUE)
  if (!is.na(h_med) && h_med > 7) {
    sim$PlateLocHeight <- sim$PlateLocHeight / 12
    sim$PlateLocSide   <- sim$PlateLocSide   / 12
  }

  sim <- .decorate_sim(sim)
  present_pitches <- intersect(names(pitch_colors),
                               unique(sim$TaggedPitchType))
  sim$TaggedPitchType <- factor(sim$TaggedPitchType, levels = present_pitches)
  cols <- pitch_colors[present_pitches]
  shp <- .shape_map_results[names(.shape_map_results) %in%
                              unique(sim$ResultDisplay)]

  g <- ggplot(sim, aes(x = PlateLocSide, y = PlateLocHeight, text = .tip)) +
    annotate("rect", xmin = -.83, xmax = .83, ymin = 1.5, ymax = 3.5,
             fill = NA, color = "black", linewidth = .7) +
    annotate("path",
             x = c(-0.708, 0.708, 0.708, 0, -0.708, -0.708),
             y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15),
             color = "black", linewidth = .4) +
    geom_point(aes(color = TaggedPitchType, shape = ResultDisplay),
               alpha = .78, size = 2.8, stroke = .4) +
    scale_color_manual(values = cols, name = "Pitch", drop = TRUE) +
    scale_shape_manual(values = shp, name = "Result", drop = TRUE) +
    coord_cartesian(xlim = c(-2.5, 2.5), ylim = c(-0.2, 5)) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(), axis.ticks = element_blank())

  plotly::ggplotly(g, tooltip = "text") %>%
    plotly::config(displaylogo = FALSE, displayModeBar = FALSE)
}

# ============================================================================
# PDF EXPORT (matrix only)
# ============================================================================
create_matchup_matrix_pdf <- function(matrix_df, output_file,
                                      view = "pitcher",
                                      pitcher_team_lbl = "", batter_team_lbl = "",
                                      detailed = FALSE,
                                      detail_fetch = NULL,
                                      grades_fetch = NULL) {
  if (length(dev.list()) > 0) try(dev.off(), silent = TRUE)
  if (!("BatterTeam"  %in% names(matrix_df))) matrix_df$BatterTeam  <- NA_character_
  if (!("PitcherTeam" %in% names(matrix_df))) matrix_df$PitcherTeam <- NA_character_
  m <- matrix_df %>%
    dplyr::mutate(
      .pkey = paste0(Pitcher, "||", ifelse(is.na(PitcherTeam), "", PitcherTeam)),
      .bkey = paste0(Batter,  "||", ifelse(is.na(BatterTeam),  "", BatterTeam)))

  pit <- m %>% dplyr::distinct(.pkey, Pitcher, PitcherTeam, PitcherThrows)
  bat <- m %>% dplyr::distinct(.bkey, Batter, BatterTeam, BatterSide)
  n_p <- nrow(pit); n_b <- nrow(bat)
  cellmap <- setNames(
    lapply(seq_len(nrow(m)), function(i)
      list(score = m$MatchupScore[i], lc = isTRUE(m$LowConfidence[i]),
           rvs = if ("PitchRVs" %in% names(m)) m$PitchRVs[i] else NA_character_)),
    paste0(m$.pkey, "\u241F", m$.bkey))

  .parse_rvs <- function(s) {
    if (is.null(s) || length(s) != 1 || is.na(s) || !nzchar(s)) return(NULL)
    kv <- strsplit(strsplit(s, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
    d <- data.frame(pt = vapply(kv, function(z) z[1], character(1)),
                    rv = suppressWarnings(as.numeric(
                      vapply(kv, function(z) z[2] %||% NA_character_, character(1)))),
                    n  = suppressWarnings(as.numeric(
                      vapply(kv, function(z) z[3] %||% NA_character_, character(1)))),
                    stringsAsFactors = FALSE)
    d$n[is.na(d$n)] <- 0
    d[!is.na(d$rv), , drop = FALSE]
  }
  hand_col <- function(h) {
    if (identical(h, "L")) "#E1463E" else if (identical(h, "S")) "#1E88E5" else "#111827"
  }
  .fmt <- function(val, suffix = "", digits = 0) {
    if (is.null(val) || length(val) == 0) return("\u2014")
    if (is.numeric(val)) {
      if (!is.finite(val[1])) return("\u2014")
      return(paste0(round(val[1], digits), suffix))
    }
    txt <- trimws(as.character(val[1]))
    if (!nzchar(txt)) "\u2014" else paste0(txt, suffix)
  }

  # ---- detailed mode: prefetch the SAME cached cards the UI rendered ----
  detailed <- isTRUE(detailed) && is.function(detail_fetch)
  dets <- if (detailed)
    lapply(seq_len(n_b), function(i)
      tryCatch(detail_fetch(bat$Batter[i], bat$.bkey[i]),
               error = function(e) NULL))
  else NULL
  grs <- if (is.function(grades_fetch))
    lapply(seq_len(n_b), function(i)
      tryCatch(grades_fetch(bat$Batter[i], bat$.bkey[i]),
               error = function(e) NULL))
  else NULL
  heat_keys <- c("missR", "missL", "xslgR", "xslgL")
  heat_labs <- list(c("Miss%", "vs RHP"), c("Miss%", "vs LHP"),
                    c("xSLG", "vs RHP"),  c("xSLG", "vs LHP"))

  # ---- page geometry: one page sized to the grid ----
  label_w_in <- if (detailed) 3.35 else 2.9
  heat_w_in  <- if (detailed) 1.05 else 0
  n_heat     <- if (detailed) 4 else 0
  cell_w_in  <- 1.22
  row_h_in   <- if (detailed) 2.30 else 0.72
  pdf_w <- max(11, label_w_in + n_heat * heat_w_in + n_p * cell_w_in + 0.7)
  pdf_h <- max(6.5, 1.7 + n_b * row_h_in + 0.8)
  pdf(output_file, width = pdf_w, height = pdf_h)
  on.exit(try(dev.off(), silent = TRUE), add = TRUE)
  grid::grid.newpage()

  # header band (matches the UI card header)
  hdr_h <- 0.9 / pdf_h
  grid::grid.rect(x = .5, y = 1 - hdr_h/2, width = 1, height = hdr_h,
                  gp = grid::gpar(fill = "#0E6E70", col = NA))
  grid::grid.text("Matchup Projections", x = 0.02, y = 1 - hdr_h * 0.38,
                  just = "left",
                  gp = grid::gpar(fontsize = 17, fontface = "bold", col = "white"))
  grid::grid.text(paste0(pitcher_team_lbl, " Pitchers vs ", batter_team_lbl,
                         " Batters \u2022 ",
                         if (identical(view, "hitter")) "Hitter Facing" else "Pitcher Facing",
                         if (detailed) " \u2022 Detailed" else ""),
                  x = 0.02, y = 1 - hdr_h * 0.74, just = "left",
                  gp = grid::gpar(fontsize = 9.5, col = "#CFE7E7"))
  grid::grid.text(format(Sys.Date(), "%B %d, %Y"), x = 0.985, y = 1 - hdr_h * 0.5,
                  just = "right", gp = grid::gpar(fontsize = 9, col = "white"))

  top    <- 1 - hdr_h - 0.02
  bottom <- 0.55 / pdf_h
  label_w <- label_w_in / pdf_w
  heat_w  <- heat_w_in  / pdf_w
  cell_w  <- cell_w_in  / pdf_w
  grid_x0 <- label_w + n_heat * heat_w   # left edge of the pitcher grid
  colhdr_h <- 0.55 / pdf_h
  row_h  <- (top - colhdr_h - bottom) / max(n_b, 1)

  # ---- heatmap column headers (detailed only) ----
  if (detailed) {
    for (k in seq_len(n_heat)) {
      hx <- label_w + (k - 0.5) * heat_w
      grid::grid.text(heat_labs[[k]][1], x = hx, y = top - colhdr_h * 0.30,
                      gp = grid::gpar(fontsize = 8.5, fontface = "bold",
                                      col = "#0B3D3E"))
      grid::grid.text(heat_labs[[k]][2], x = hx, y = top - colhdr_h * 0.68,
                      gp = grid::gpar(fontsize = 7.5, col = "#0B3D3E"))
    }
  }

  # ---- pitcher column headers: "#21 Name" / "(L) - JR" ----
  for (j in seq_len(n_p)) {
    cx <- grid_x0 + (j - 0.5) * cell_w
    d <- tryCatch(drs_info(pit$Pitcher[j], .mm_team_display(pit$PitcherTeam[j] %||% "")),
                  error = function(e) list(jersey="", pos="", class="", overall=NA))
    thr <- substr(pit$PitcherThrows[j] %||% "", 1, 1)
    nm <- paste0(if (nzchar(d$jersey)) paste0("#", d$jersey, " ") else "", pit$Pitcher[j])
    nm_lines <- strwrap(nm, width = 16); if (length(nm_lines) > 2) nm_lines <- nm_lines[1:2]
    for (li in seq_along(nm_lines))
      grid::grid.text(nm_lines[li], x = cx,
                      y = top - colhdr_h * (0.24 + 0.30 * (li - 1)),
                      gp = grid::gpar(fontsize = 8.5, fontface = "bold",
                                      col = hand_col(thr)))
    sub_bits <- c(if (nzchar(thr) && !is.na(thr)) paste0("(", thr, ")"),
                  if (nzchar(d$class)) d$class)
    grid::grid.text(paste(sub_bits, collapse = " \u00B7 "), x = cx,
                    y = top - colhdr_h * 0.85,
                    gp = grid::gpar(fontsize = 7, col = "#8A9296"))
  }
  grid::grid.lines(x = c(0.01, 0.99), y = top - colhdr_h,
                   gp = grid::gpar(col = "#ECEFF1", lwd = 1.4))

  # grade pill (two-line: label over value), colored like the UI pills
  draw_gpill <- function(x, y, w, h, lab, v, agg = FALSE) {
    na <- is.null(v) || length(v) != 1 || !is.finite(v)
    bgc <- if (na) "#EDEFF1" else grade_pill_color(v)
    fgc <- if (na) "#9AA1A6" else pill_text_color(bgc)
    grid::grid.roundrect(x = x, y = y, width = w, height = h,
                         r = grid::unit(0.25, "snpc"),
                         gp = grid::gpar(fill = bgc,
                                         col = if (agg) "#0E6E70" else NA,
                                         lwd = if (agg) 1.2 else 0))
    grid::grid.text(toupper(lab), x = x, y = y + h * 0.22,
                    gp = grid::gpar(fontsize = 4.6, fontface = "bold", col = fgc))
    grid::grid.text(if (na) "\u2014" else as.character(round(v)),
                    x = x, y = y - h * 0.18,
                    gp = grid::gpar(fontsize = 7.5, fontface = "bold", col = fgc))
  }

  # ---- rows ----
  for (i in seq_len(n_b)) {
    ry_top <- top - colhdr_h - (i - 1) * row_h
    ry_mid <- ry_top - row_h / 2
    if (i %% 2 == 0)
      grid::grid.rect(x = .5, y = ry_mid, width = .98, height = row_h,
                      gp = grid::gpar(fill = "#FAFBFB", col = NA))
    d <- tryCatch(drs_info(bat$Batter[i], .mm_team_display(bat$BatterTeam[i] %||% "")),
                  error = function(e) list(jersey="", pos="", class="", overall=NA))
    side <- substr(bat$BatterSide[i] %||% "", 1, 1)
    nm <- paste0(if (nzchar(d$jersey)) paste0("#", d$jersey, " ") else "", bat$Batter[i])
    meta_bits <- c(if (nzchar(side) && !is.na(side)) paste0("(", side, ")"),
                   if (nzchar(d$pos)) d$pos, if (nzchar(d$class)) d$class)

    # detailed rows anchor content from the TOP of the row; the compact
    # matrix keeps the old vertically-centered layout
    name_y <- if (detailed) ry_top - row_h * 0.07 else ry_mid + row_h * 0.16
    grid::grid.text(nm, x = 0.015, y = name_y, just = "left",
                    gp = grid::gpar(fontsize = 9.5, fontface = "bold",
                                    col = hand_col(side)))
    nm_w <- grid::convertWidth(grid::grobWidth(
      grid::textGrob(nm, gp = grid::gpar(fontsize = 9.5, fontface = "bold"))),
      "npc", valueOnly = TRUE)
    grid::grid.text(paste(meta_bits, collapse = " \u00B7 "),
                    x = 0.015 + nm_w + 0.006, y = name_y, just = "left",
                    gp = grid::gpar(fontsize = 7.5, col = "#9AA1A6"))
    if (!is.na(d$overall)) {
      .dbg <- drs_pill_color(d$overall)
      dr_y <- if (detailed) name_y else ry_mid - row_h * 0.22
      dr_x <- if (detailed) label_w - 0.032 else 0.015 + 0.022
      grid::grid.roundrect(x = dr_x, y = dr_y,
                           width = 0.044, height = if (detailed) row_h * 0.075 else row_h * 0.30,
                           r = grid::unit(0.5, "snpc"), just = "center",
                           gp = grid::gpar(fill = .dbg, col = NA))
      grid::grid.text(sprintf("%+d", round(as.numeric(d$overall))),
                      x = dr_x, y = dr_y,
                      gp = grid::gpar(fontsize = 7.5, fontface = "bold",
                                      col = pill_text_color(.dbg)))
    }

    # ---- grade pills + detail chips + heatmaps (detailed mode) ----
    if (detailed) {
      g <- if (!is.null(grs)) grs[[i]] else NULL
      if (!is.null(g)) {
        gp_w <- 0.55 / pdf_w; gp_h <- row_h * 0.105
        gy <- ry_top - row_h * 0.185
        labs <- c(if (isTRUE(g$process)) "Prc" else "Agg", "Con", "Pow", "Dec")
        vals <- list(g$aggregate, g$contact, g$power, g$decision)
        for (k in 1:4)
          draw_gpill(0.015 + gp_w/2 + (k - 1) * (gp_w + 0.004 / pdf_w * 0),
                     gy, gp_w * 0.92, gp_h, labs[k],
                     suppressWarnings(as.numeric(vals[[k]])), agg = (k == 1))
      }
      r <- dets[[i]]$row
      if (!is.null(r)) {
        line_y <- function(f) ry_top - row_h * f
        chip_line <- function(y, txt, size = 6.4) {
          txt <- strwrap(txt, width = 74)[1]
          grid::grid.text(txt, x = 0.015, y = y, just = "left",
                          gp = grid::gpar(fontsize = size, col = "#31423F"))
        }
        swing_line <- function(flag, pct, n)
          paste0(.fmt(flag), " ", .fmt(pct, "%"), " / ", .fmt(n), " pitches")
        chip_line(line_y(0.30),
                  paste0("SB att: ", .fmt(r$SBA), "   Bunts: ", .fmt(r$Bunts),
                         "   EV90: ", .fmt(r$EV90, " mph", 1),
                         "   Max EV: ", .fmt(r$MaxEV, " mph", 1)))
        chip_line(line_y(0.39),
                  paste0("1P agg: ", swing_line(r$Agg1P, r$Swing1P, r$Pitches1P),
                         "   2K agg: ", swing_line(r$Agg2K, r$Swing2K, r$Pitches2K)))
        chip_line(line_y(0.48),
                  paste0("PT weakness: ", .fmt(r$PTWeak),
                         "   Trait vL: ", .fmt(r$TraitWeakL),
                         "   Trait vR: ", .fmt(r$TraitWeakR)))
        chip_line(line_y(0.57),
                  paste0("Pull/Ctr/Oppo vL: ", .fmt(r$PosL),
                         "   vR: ", .fmt(r$PosR)))
        chip_line(line_y(0.66),
                  paste0("Pre-2K: ", .fmt(r$PosPre2K), "   2K: ", .fmt(r$Pos2K)))
        grid::grid.text("Notes:", x = 0.015, y = line_y(0.80), just = "left",
                        gp = grid::gpar(fontsize = 6.4, fontface = "bold",
                                        col = "#9AA5A5"))
        grid::grid.lines(x = c(0.015 + 0.028, label_w - 0.012),
                         y = rep(line_y(0.83), 2),
                         gp = grid::gpar(col = "#C9CFD2", lwd = 0.7))
        grid::grid.lines(x = c(0.015, label_w - 0.012),
                         y = rep(line_y(0.93), 2),
                         gp = grid::gpar(col = "#C9CFD2", lwd = 0.7))
      }
      # heatmap cells — the SAME ggplots the UI columns rendered
      for (k in seq_len(n_heat)) {
        hp <- tryCatch(dets[[i]]$heats[[heat_keys[k]]], error = function(e) NULL)
        if (is.null(hp)) next
        vp <- grid::viewport(x = label_w + (k - 0.5) * heat_w, y = ry_mid,
                             width = heat_w * 0.96, height = row_h * 0.94)
        grid::pushViewport(vp)
        tryCatch(grid::grid.draw(ggplot2::ggplotGrob(hp)),
                 error = function(e) NULL)
        grid::popViewport()
      }
    }

    # ---- matchup cells ----
    cell_h <- if (detailed) min(row_h * 0.82, 0.62 / pdf_h) else row_h * 0.82
    for (j in seq_len(n_p)) {
      cx <- grid_x0 + (j - 0.5) * cell_w
      cm <- cellmap[[paste0(pit$.pkey[j], "\u241F", bat$.bkey[i])]]
      sc <- if (is.null(cm)) NA_real_ else cm$score
      disp <- if (identical(view, "hitter") && !is.na(sc)) 100 - sc else sc
      lc <- !is.null(cm) && isTRUE(cm$lc)
      bg <- matchup_score_color(disp)
      fg <- if (is.na(disp)) "#9AA1A6" else pill_text_color(bg)
      grid::grid.roundrect(x = cx, y = ry_mid, width = cell_w * 0.88,
                           height = cell_h, r = grid::unit(0.14, "snpc"),
                           gp = grid::gpar(fill = bg,
                                           col = if (lc) "#757575" else "#E4E7E9",
                                           lty = if (lc) 2 else 1,
                                           lwd = if (lc) 1.1 else 0.5))
      grid::grid.text(if (is.na(disp)) "\u2014" else as.character(round(disp)),
                      x = cx, y = ry_mid + cell_h * 0.20,
                      gp = grid::gpar(fontsize = 11.5, fontface = "bold", col = fg))
      rvd <- .parse_rvs(if (is.null(cm)) NULL else cm$rvs)
      if (!is.null(rvd) && nrow(rvd) > 0) {
        rvd <- rvd[order(if (identical(view, "hitter")) -rvd$rv else rvd$rv), ,
                   drop = FALSE]
        rvd <- head(rvd, 4)
        mini_sc <- vapply(rvd$rv, rv_to_matchup_score, numeric(1))
        if (identical(view, "hitter")) mini_sc <- 100 - mini_sc
        mw <- cell_w * 0.19; mh <- cell_h * 0.30
        x0 <- cx - (nrow(rvd) * mw + (nrow(rvd) - 1) * cell_w * 0.012) / 2 + mw/2
        for (kk in seq_len(nrow(rvd))) {
          mx <- x0 + (kk - 1) * (mw + cell_w * 0.012)
          .mbg <- matchup_score_color(mini_sc[kk])
          grid::grid.roundrect(x = mx, y = ry_mid - cell_h * 0.25,
                               width = mw, height = mh,
                               r = grid::unit(0.35, "snpc"),
                               gp = grid::gpar(fill = .mbg,
                                               col = if (rvd$n[kk] < 10) "#616161" else NA,
                                               lty = if (rvd$n[kk] < 10) 2 else 1,
                                               lwd = 0.9))
          grid::grid.text(pitch_abbrev(rvd$pt[kk]), x = mx,
                          y = ry_mid - cell_h * 0.25,
                          gp = grid::gpar(fontsize = 5.6, fontface = "bold",
                                          col = pill_text_color(.mbg)))
        }
      }
    }
  }

  # ---- legend + verdict footer ----
  leg_y <- bottom * 0.62
  leg <- list(c("#1B7837", if (identical(view, "hitter")) "Hitter advantage" else "Pitcher advantage"),
              c("#FFFFFF", "Neutral"),
              c("#E1463E", if (identical(view, "hitter")) "Pitcher advantage" else "Hitter advantage"))
  lx <- 0.02
  for (lg in leg) {
    grid::grid.roundrect(x = lx + 0.011, y = leg_y, width = 0.022, height = 0.35 * bottom,
                         r = grid::unit(0.3, "snpc"),
                         gp = grid::gpar(fill = lg[1], col = "#C9CFD2", lwd = 0.5))
    grid::grid.text(lg[2], x = lx + 0.026, y = leg_y, just = "left",
                    gp = grid::gpar(fontsize = 7.5, col = "#374151"))
    lx <- lx + 0.024 + max(0.06, nchar(lg[2]) * 0.0058)
  }
  grid::grid.text(if (detailed)
                    paste0("Dashed = low confidence. Heatmaps: Miss% per swing / ",
                           "xSLG per ball in play \u2014 darker red = more.")
                  else
                    paste0("Dashed = low confidence (<25 similar; mini pills <10). ",
                           "Mini pills rank the arsenal by RV vs similar pitches, best first."),
                  x = 0.02, y = bottom * 0.20, just = "left",
                  gp = grid::gpar(fontsize = 7, col = "#8A9296", fontface = "italic"))
  sc_ok <- m$MatchupScore[!is.na(m$MatchupScore)]
  if (length(sc_ok) > 0) {
    avg <- mean(sc_ok)
    avg_disp <- if (identical(view, "hitter")) 100 - avg else avg
    fav <- if (avg >= 55) paste0(pitcher_team_lbl, " arms favored") else
      if (avg <= 45) paste0(batter_team_lbl, " bats favored") else "Even matchup"
    grid::grid.text(sprintf("%s \u2014 average score %d/100 across %d matchups.",
                            fav, round(avg_disp), length(sc_ok)),
                    x = 0.985, y = leg_y, just = "right",
                    gp = grid::gpar(fontsize = 8, fontface = "bold", col = "#0B3D3E"))
  }
  invisible(output_file)
}


# ============================================================================
# MATCHUP MATRIX DATA POOLS
# Team-level pitch pools sourced the same way the 2026 pitcher pages are:
# TruMedia API is the primary 2026 source; the NCAA parquet datasets are the
# 2025 complement and the 2026 fallback. The in-memory P5/SBC/CCU pool serves
# TrackMan-code teams directly.
# ============================================================================
MM_POOL_COLS <- c("Pitcher", "PitcherTeam", "PitcherThrows", "Batter",
                  "BatterTeam", "BatterSide", "TaggedPitchType",
                  "RelSpeed", "InducedVertBreak", "HorzBreak", "SpinRate",
                  "RelSide", "RelHeight", "Extension",
                  "PitchCall", "PlayResult", "KorBB", "Balls", "Strikes",
                  "PlateLocSide", "PlateLocHeight",
                  "ExitSpeed", "Bearing", "Distance", "TaggedHitType",
                  "Date", "Inning", "PAofInning",
                  "GameUID", "GameID", "PitchUID",
                  # proModel scoring: join key + model inputs (all optional;
                  # score_promodel NA-fills gaps and imputes kinematics)
                  "PitchofPA", "Outs", "SpinAxis", "Angle", "Stadium",
                  "VertRelAngle", "HorzRelAngle",
                  "VertApprAngle", "HorzApprAngle", "vy0", "ay0", "y0",
                  # xrv passthrough, kept only so already-scored Coastal rows
                  # survive the select intact. The matchup engine does not
                  # read these — matrix pricing is actual DRE, full stop.
                  "stuff_xrv", "pitching_xrv", "location_xrv")

.mm_pool_cache <- new.env(parent = emptyenv())

# TrackMan code -> directory display name (unknown codes pass through)
.mm_team_display <- function(x) {
  if (is.null(x)) return(x)
  x <- as.character(x)
  hit <- teams_data$team_name[match(x, teams_data$trackman_abbr)]
  ifelse(!is.na(hit) & nzchar(hit), hit, x)
}

# NA-safe PitchUID dedupe (plain distinct() would collapse all NA ids)
.mm_dedupe_uid <- function(out) {
  if (!("PitchUID" %in% names(out)) || nrow(out) == 0) return(out)
  id <- as.character(out$PitchUID)
  has_id <- !is.na(id) & nzchar(id)
  dplyr::bind_rows(
    out[!has_id, , drop = FALSE],
    out[has_id, , drop = FALSE] %>% dplyr::distinct(PitchUID, .keep_all = TRUE)
  )
}

# ---- TruMedia: every pitch in a team's tracked games (both halves) ----
# Same game-list -> chunked GamePitchesTrackman flow as
# tm_load_pitcher_pitches, but without the single-pitcher filter, so the
# pool contains the team's pitchers AND its batters. Per-row team
# attribution, in order of reliability:
#   1) explicit pitcher/batter team columns in the pitch payload
#   2) TeamGames home/away flag (home team pitches the Top half)
#   3) heuristic: players who appear against >= 2 distinct opponents
#      belong to the team (opponents only ever face this one team here)
tm_load_team_pool <- function(team_disp) {
  if (!tm_enabled()) return(NULL)
  if (is.null(team_disp) || !nzchar(team_disp)) return(NULL)
  tid <- tm_find_team_id(team_disp)
  if (is.null(tid)) return(NULL)
  g <- tm_team_games(tid)
  if (is.null(g) || nrow(g) == 0) return(NULL)
  idc <- .tm_pick_col(g, c("^gameId$", "game.?id"))
  if (is.null(idc)) {
    .tm_env$last_error <- "TeamGames: no gameId column"
    return(NULL)
  }
  tmc <- .tm_pick_col(g, c("trackmanGameId", "trackmanGameID"))
  if (!is.null(tmc)) {
    tracked <- !is.na(g[[tmc]]) & nzchar(as.character(g[[tmc]]))
    if (any(tracked)) g <- g[tracked, , drop = FALSE]
  }
  ids <- unique(stats::na.omit(g[[idc]]))
  if (length(ids) == 0) return(NULL)

  chunks <- split(ids, ceiling(seq_along(ids) / 20))
  raw <- list()
  for (ch in chunks) {
    p <- tm_fetch_pitch_chunk(ch)
    Sys.sleep(0.25)   # sequential + polite, per TruMedia guidance
    if (!is.null(p) && nrow(p) > 0) raw[[length(raw) + 1]] <- p
  }
  if (length(raw) == 0) {
    .tm_env$last_error <- paste0("no TruMedia pitches for team '", team_disp, "'")
    return(NULL)
  }
  raw_df <- dplyr::bind_rows(raw)
  res <- tm_to_trackman(raw_df)

  # game -> opponent display map (same construction as the pitcher loader)
  opp_disp <- .tm_opp_display(g)
  opp_map <- setNames(character(0), character(0))
  if (!is.null(opp_disp)) {
    keys <- character(0); vals <- character(0)
    for (kc in unique(c(idc, tmc))) {
      if (is.null(kc) || !kc %in% names(g)) next
      kk <- as.character(g[[kc]])
      ok <- !is.na(kk) & nzchar(kk)
      keys <- c(keys, kk[ok])
      vals <- c(vals, opp_disp[ok])
    }
    if (length(keys) > 0) opp_map <- setNames(vals, keys)
  }
  row_opp <- unname(opp_map[as.character(res$GameID)])

  # -- attribution path 1: payload team columns --
  pt_col <- .tm_pick_col(raw_df, c("pitcherTeamName", "pitcherTeamLocation",
                                   "pitcherTeamAbbrev", "pitcherTeam"))
  bt_col <- .tm_pick_col(raw_df, c("batterTeamName", "batterTeamLocation",
                                   "batterTeamAbbrev", "batterTeam"))
  team_pitching <- NULL
  if (!is.null(pt_col)) {
    pv <- .tm_norm(as.character(raw_df[[pt_col]]))
    team_pitching <- pv == .tm_norm(team_disp) |
      grepl(.tm_norm(team_disp), pv, fixed = TRUE)
    cat("[MM pool] team attribution via payload column", pt_col, "\n")
  }

  # -- attribution path 2: home/away flag on TeamGames --
  if (is.null(team_pitching)) {
    ha <- names(g)[grepl("home", names(g), ignore.case = TRUE) &
                     !grepl("opp|team.?name|runs|score", names(g),
                            ignore.case = TRUE)]
    ha <- ha[vapply(ha, function(cc) {
      v <- tolower(as.character(g[[cc]]))
      all(v[!is.na(v) & nzchar(v)] %in%
            c("true", "false", "t", "f", "1", "0", "h", "a",
              "home", "away", "yes", "no"))
    }, logical(1))]
    if (length(ha) > 0) {
      hv <- tolower(as.character(g[[ha[1]]]))
      is_home <- hv %in% c("true", "t", "1", "h", "home", "yes")
      hmap <- character(0)
      for (kc in unique(c(idc, tmc))) {
        if (is.null(kc) || !kc %in% names(g)) next
        kk <- as.character(g[[kc]])
        ok <- !is.na(kk) & nzchar(kk)
        hmap <- c(hmap, setNames(ifelse(is_home[ok], "H", "A"), kk[ok]))
      }
      row_ha <- unname(hmap[as.character(res$GameID)])
      # home team pitches the Top half (visitors bat first)
      team_pitching <- ifelse(row_ha == "H", res$`Top.Bottom` == "Top",
                       ifelse(row_ha == "A", res$`Top.Bottom` == "Bottom", NA))
      if (all(is.na(team_pitching))) team_pitching <- NULL else
        cat("[MM pool] team attribution via TeamGames home/away ('",
            ha[1], "')\n", sep = "")
    }
  }

  # -- attribution path 3: distinct-opponent heuristic, voted per half --
  if (is.null(team_pitching) || anyNA(team_pitching)) {
    n_opp <- tapply(row_opp, res$Pitcher,
                    function(z) length(unique(z[!is.na(z) & nzchar(z)])))
    core <- names(n_opp)[!is.na(n_opp) & n_opp >= 2]
    half_key <- paste(res$GameID, res$`Top.Bottom`)
    core_share <- tapply(res$Pitcher %in% core, half_key, mean)
    guess <- unname(core_share[half_key]) > 0.5
    if (is.null(team_pitching)) {
      team_pitching <- guess
      cat("[MM pool] team attribution via distinct-opponent heuristic\n")
    } else {
      team_pitching[is.na(team_pitching)] <- guess[is.na(team_pitching)]
    }
  }
  team_pitching[is.na(team_pitching)] <- FALSE

  opp_lbl <- ifelse(!is.na(row_opp) & nzchar(row_opp), row_opp, "Opponent")
  res$PitcherTeam <- ifelse(team_pitching, team_disp, opp_lbl)
  res$BatterTeam  <- ifelse(team_pitching, opp_lbl, team_disp)
  res %>% dplyr::select(dplyr::any_of(MM_POOL_COLS))
}

# ---- Scoring pass for matchup pools (RETIRED) ---------------------------
# The matrix prices every pitch on actual DRE, so no model scoring pass
# feeds it — not the xRV chain, not the xstats xwOBA chain. Kept as a
# pass-through so the pool loader's call site stays intact; the time both
# passes used to cost on every matrix build is given back.
mm_score_pool <- function(pool, context = "") pool

# ---- Unified per-team pool for the Matchup Matrix ----
# team_key is either a TrackMan code from the in-memory P5/SBC/CCU pool,
# or "ncaa::<display name>" for any team in the NCAA directory.
load_matchup_team_pool <- function(team_key) {
  ck <- gsub("[^A-Za-z0-9]", "_", team_key)
  cached <- .mm_pool_cache[[ck]]
  if (!is.null(cached)) return(cached)

  if (!startsWith(team_key, "ncaa::")) {
    out <- full_data_p5_compare %>%
      dplyr::filter(PitcherTeam == team_key |
                      (!is.na(BatterTeam) & BatterTeam == team_key)) %>%
      dplyr::select(dplyr::any_of(MM_POOL_COLS)) %>%
      as.data.frame()
    # non-Coastal arms in the P5/SBC pool carry raw operator tags —
    # reclassify them; Coastal arms pass through untouched
    out <- reclassify_noncoastal_pitches(out, context = team_key)
    .mm_pool_cache[[ck]] <- out
    return(out)
  }

  disp <- sub("^ncaa::", "", team_key)
  codes <- unique(c(ncaa_team_codes[[disp]], disp))
  codes <- codes[!is.na(codes) & nzchar(codes)]

  pull_team <- function(ds) {
    if (is.null(ds)) return(NULL)
    both <- tryCatch(
      ds %>% dplyr::select(dplyr::any_of(MM_POOL_COLS)) %>%
        dplyr::filter(PitcherTeam %in% codes | BatterTeam %in% codes) %>%
        dplyr::collect() %>% as.data.frame(),
      error = function(e) NULL)
    if (!is.null(both)) return(both)
    tryCatch(
      ds %>% dplyr::select(dplyr::any_of(MM_POOL_COLS)) %>%
        dplyr::filter(PitcherTeam %in% codes) %>%
        dplyr::collect() %>% as.data.frame(),
      error = function(e) {
        cat("WARNING: MM parquet pull failed -", conditionMessage(e), "\n")
        NULL
      })
  }

  # 2026: TruMedia API is the primary source, exactly like the pitcher
  # pages; parquet is the fallback. 2025 complements from parquet.
  sb26 <- tryCatch(pp_team_rows(codes), error = function(e) NULL)
  tm26 <- if (!is.null(sb26) && nrow(sb26) > 0) sb26 else
    tryCatch(tm_load_team_pool(disp), error = function(e) {
      cat("TruMedia team pool failed:", conditionMessage(e), "\n")
      NULL
    })
  if (!is.null(tm26) && nrow(tm26) > 0) {
    cat("  [MM 2026 source]", if (!is.null(sb26) && nrow(sb26) > 0) "Supabase pitchprofiler:" else "TruMedia API:",
        nrow(tm26), "pitches\n")
    pq26 <- NULL
  } else {
    cat("  [MM 2026 source] TruMedia unavailable (",
        .tm_env$last_error %||% "no detail", ") - parquet fallback\n")
    pq26 <- pull_team(NCAA26_DS)
  }
  pq25 <- pull_team(NCAA25_DS)

  fix_pq <- function(d) {
    if (is.null(d) || nrow(d) == 0) return(NULL)
    if ("Pitcher" %in% names(d))
      d$Pitcher <- ifelse(grepl(",", d$Pitcher),
                          normalize_lastfirst(d$Pitcher), d$Pitcher)
    if ("Batter" %in% names(d))
      d$Batter <- ifelse(grepl(",", d$Batter),
                         normalize_lastfirst(d$Batter), d$Batter)
    d <- tryCatch(normalize_plate_location(d), error = function(e) d)
    if ("PitcherTeam" %in% names(d)) d$PitcherTeam <- .mm_team_display(d$PitcherTeam)
    if ("BatterTeam"  %in% names(d)) d$BatterTeam  <- .mm_team_display(d$BatterTeam)
    d
  }

  # Supabase rows carry TrackMan team codes like the parquet rows do
  if (!is.null(sb26) && nrow(sb26) > 0) tm26 <- fix_pq(tm26)
  frames <- Filter(function(d) !is.null(d) && nrow(d) > 0,
                   list(fix_pq(pq25), fix_pq(pq26), tm26))
  if (length(frames) == 0) {
    .mm_pool_cache[[ck]] <- data.frame()
    return(data.frame())
  }
  out <- dplyr::bind_rows(harmonize_types(frames)) %>%
    dplyr::select(dplyr::any_of(MM_POOL_COLS)) %>%
    .mm_dedupe_uid()
  # matchup pools are non-Coastal staffs (plus opponents): reclassify
  # every non-Coastal arm so the matrix/spray group on corrected types.
  # Reclassify BEFORE scoring — TaggedPitchType feeds the bundle's
  # times_seen_pitch_type sequencing feature.
  out <- reclassify_noncoastal_pitches(out, context = disp)
  out <- mm_score_pool(out, context = disp)
  .mm_pool_cache[[ck]] <- out
  cat("[MM pool]", disp, "-", nrow(out), "pitches total\n")
  out
}


# ============================================================================
