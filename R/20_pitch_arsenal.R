# PITCH ARSENAL  —  exposure-aware "optimal pitch percentage" diagnostic
# ---------------------------------------------------------------------------
# Wired to the app's existing per-pitch data. The comparison pool and the
# selected pitcher both come from selected_data() (the season frame already in
# memory) which is TruMedia-sourced for NCAA arms via load_ncaa_pitcher_data()
# and TrackMan for Coastal arms. It reuses that data rather than re-hammering
# the TruMedia API pitch-by-pitch for the whole league (which would blow the
# rate limits) — same TruMedia pitches, no redundant pulls.
#
# DIAGNOSTIC, not prescriptive: current vs modeled mix + a MODELED run-value
# impact, and a stuff-tier read that separates "struggling because of MIX" from
# "struggling because the STUFF isn't there." Coach makes the call.
# ---------------------------------------------------------------------------

PA_FLOOR    <- 0.05     # keep-honest floor: never zero a pitch the hitter knows exists
PA_DELTA    <- 0.0019   # familiarity tax per cumulative batter-look (MLB placeholder;
                        #   recalibrate on college data via within-matchup demeaned slope)
                        #   samples are thin, so this leans on comparables — by design)
PA_K_RV     <- 60       # credibility k for a per-type run-value estimate: n/(n+k) toward
                        #   the prior. Stops a 4-pitch changeup from being trusted.
PA_K_USAGE  <- 100      # USAGE credibility: how much evidence before the model is
                        #   allowed to move a pitch's share. Shrinking VALUE to neutral
                        #   is not enough -- a no-information pitch scores 'average',
                        #   which beats a well-measured below-average pitch. This anchors
                        #   thin pitches to their OBSERVED usage instead.

# ---- how much each force drives the recommendation (must sum to 1) ----------
PA_W_RV  <- 1/3    # run value: how good the pitch is, count-adjusted
PA_W_SEQ <- 1/3    # setup: what the pitch does for the pitch that follows it
PA_W_FAM <- 1/3    # familiarity: the "they've seen it 4+ times" decay

# water-filling equilibrium solver (ports the paper's brentq version)
# ---- Empirical familiarity curve: measures delta (RV lost per repeat look) ----
# The familiarity tax is what stops the optimizer from throwing the best pitch
# 100% of the time: delta is the price per repeat look, and the quadratic penalty
# it creates is what makes a second pitch worth throwing. Measured on the pooled
# league DB (full_data_p5_compare) using the matchup engine's DRE currency.
pa_pitch_dre <- function(df) {
  rv <- as.numeric(weights_vec[map_dre_event(df$PitchCall, df$PlayResult)])
  rv[is.na(rv)] <- 0; rv                     # pitcher perspective: higher = better
}
pa_familiarity_curve <- function(pool, min_looks = 4, date_col = NULL,
                                 n_boot = 200, prior = PA_DELTA) {
  need <- c("Pitcher","Batter","TaggedPitchType","PitchCall","PlayResult")
  bail <- function(reason, nm = 0) list(delta = prior, delta_used = prior,
            identified = FALSE, reason = reason, n_matchups = nm, curve = NULL)
  if (is.null(pool) || !is.data.frame(pool)) return(bail("no data pool found"))
  miss <- setdiff(need, names(pool))
  if (length(miss)) return(bail(paste("missing columns:", paste(miss, collapse = ", "))))
  d <- pool[!is.na(pool$TaggedPitchType) &
              !pool$TaggedPitchType %in% c("", "Other"), , drop = FALSE]
  if (nrow(d) < 500) return(bail("too few pitches"))
  if (is.null(date_col)) {
    cand <- c("Date","UTCDate","GameDate","game_date","UTCDateTime","PitchTime","GameUID","GameID")
    hit <- cand[cand %in% names(d)]; date_col <- if (length(hit)) hit[1] else NA
  }
  if (!is.na(date_col) && date_col %in% names(d)) d <- d[order(d[[date_col]]), ]
  d$rv  <- pa_pitch_dre(d)
  mkey  <- paste(d$Pitcher, d$Batter, d$TaggedPitchType, sep = "\u241F")
  seen  <- ave(seq_along(mkey), mkey, FUN = function(i) seq_along(i) - 1L)
  df <- data.frame(mkey, rv = d$rv, seen, pitcher = d$Pitcher, stringsAsFactors = FALSE)
  tab  <- table(df$mkey); keep <- names(tab)[tab >= min_looks]
  df   <- df[df$mkey %in% keep, , drop = FALSE]
  if (nrow(df) < 200 || length(keep) < 30)
    return(bail("insufficient repeat-exposure", length(keep)))
  df$rv_dm   <- df$rv   - ave(df$rv,   df$mkey)
  df$seen_dm <- df$seen - ave(df$seen, df$mkey)
  den <- sum(df$seen_dm^2)
  if (den <= 0) return(bail("no within-look variation", length(keep)))
  delta <- -(sum(df$seen_dm * df$rv_dm) / den)
  sidx <- split(seq_len(nrow(df)), df$pitcher); pids <- names(sidx)
  boot <- vapply(seq_len(n_boot), function(b) {
    idx <- unlist(sidx[sample(pids, length(pids), TRUE)], use.names = FALSE)
    x <- df$seen_dm[idx]; dd <- sum(x^2)
    if (dd > 0) -(sum(x * df$rv_dm[idx]) / dd) else NA_real_
  }, numeric(1))
  boot <- boot[is.finite(boot)]
  se <- if (length(boot) > 10) stats::sd(boot) else NA_real_
  z  <- if (!is.na(se) && se > 0) delta / se else NA_real_
  bucket <- cut(df$seen, c(-Inf,0,1,2,4,9,Inf), labels = c("0","1","2","3-4","5-9","10+"))
  curve  <- tapply(df$rv_dm, bucket, mean, na.rm = TRUE)
  ident <- !is.na(z) && abs(z) >= 2 && delta > 0
  list(delta = delta, se = se, z = z,
       ci = if (length(boot) > 10) stats::quantile(boot, c(.025,.975), na.rm = TRUE) else c(NA,NA),
       n_matchups = length(keep), n_pitches = nrow(df), curve = curve,
       identified = ident, delta_used = if (ident) delta else prior,
       reason = if (ident) "identified" else "underidentified -> prior")
}
# memoized: estimate once on the pooled league DB, reuse everywhere
.pa_fam_cache <- new.env(parent = emptyenv())
pa_get_familiarity <- function(pool = NULL) {
  if (exists("val", envir = .pa_fam_cache, inherits = FALSE))
    return(get("val", envir = .pa_fam_cache, inherits = FALSE))
  # prefer the widest league pool; fall back to whatever the tab passed in
  big <- tryCatch(get("full_data_p5_compare", inherits = TRUE),
                  error = function(e) NULL)
  use <- if (is.data.frame(big) && nrow(big) > 0) big else pool
  val <- pa_familiarity_curve(use)
  assign("val", val, envir = .pa_fam_cache); val
}

# ---- local re-tagging of mis-classified pitches -----------------------------
# When a pitch type is unticked (e.g. a splitter logged as a changeup), those
# pitches are NOT thrown away -- they are re-assigned to whichever KEPT pitch
# type they most resemble on shape. Nearest-centroid in standardized shape
# space: the pitcher's own kept pitches define the centroid where he has enough
# of them, the league's centroid for that type where he doesn't. Anything that
# looks like none of his kept pitches (beyond PA_RETAG_MAX_D) is dropped rather
# than forced onto a type it doesn't belong to.
# This is LOCAL to the Pitch Arsenal model -- no other tab sees the re-tag.
PA_SHAPE_FEATURES <- c("RelSpeed", "InducedVertBreak", "HorzBreak", "SpinRate")
PA_RETAG_MAX_D    <- 2.5   # z-space distance beyond which a pitch is left out
PA_CENTROID_MIN_N <- 20    # own-pitch count needed before trusting his centroid

pa_retag <- function(pool, pitcher, keep_types) {
  none <- list(pool = pool, note = NULL)
  f <- PA_SHAPE_FEATURES
  if (!all(f %in% names(pool)) || !"TaggedPitchType" %in% names(pool)) return(none)
  isP  <- pool$Pitcher == pitcher & !is.na(pool$TaggedPitchType) &
            pool$TaggedPitchType != ""
  badv <- isP & !(pool$TaggedPitchType %in% keep_types)
  if (!any(badv)) return(none)

  mu <- vapply(f, function(cc) mean(pool[[cc]], na.rm = TRUE), numeric(1))
  sg <- vapply(f, function(cc) stats::sd(pool[[cc]],  na.rm = TRUE), numeric(1))
  sg[!is.finite(sg) | sg <= 0] <- 1
  zof <- function(df) sweep(sweep(as.matrix(df[, f, drop = FALSE]), 2, mu, "-"),
                            2, sg, "/")

  cents <- list()
  for (pt in keep_types) {
    own <- pool[isP & pool$TaggedPitchType == pt, , drop = FALSE]
    own <- own[stats::complete.cases(own[, f, drop = FALSE]), , drop = FALSE]
    if (nrow(own) >= PA_CENTROID_MIN_N) {
      cents[[pt]] <- colMeans(zof(own))
    } else {
      lg <- pool[pool$TaggedPitchType == pt, , drop = FALSE]
      lg <- lg[stats::complete.cases(lg[, f, drop = FALSE]), , drop = FALSE]
      if (nrow(lg) >= PA_CENTROID_MIN_N) cents[[pt]] <- colMeans(zof(lg))
    }
  }
  cents <- cents[vapply(cents, function(z) all(is.finite(z)), logical(1))]
  if (length(cents) == 0) return(none)
  cm <- do.call(rbind, cents); rownames(cm) <- names(cents)

  idx     <- which(badv)
  newtags <- as.character(pool$TaggedPitchType)
  dropv   <- rep(FALSE, nrow(pool))
  moved   <- character(0); from_lab <- character(0)
  sub <- pool[idx, , drop = FALSE]
  okr <- stats::complete.cases(sub[, f, drop = FALSE])
  zs  <- if (any(okr)) zof(sub[okr, , drop = FALSE]) else NULL
  zi  <- 0L
  for (a in seq_along(idx)) {
    r <- idx[a]
    if (!okr[a]) { dropv[r] <- TRUE; next }
    zi <- zi + 1L
    dd <- sqrt(rowSums(sweep(cm, 2, zs[zi, ], "-")^2))
    j  <- which.min(dd)
    if (is.finite(dd[j]) && dd[j] <= PA_RETAG_MAX_D) {
      from_lab <- c(from_lab, newtags[r])
      newtags[r] <- rownames(cm)[j]
      moved <- c(moved, rownames(cm)[j])
    } else dropv[r] <- TRUE
  }
  pool$TaggedPitchType <- newtags
  if (any(dropv)) pool <- pool[!dropv, , drop = FALSE]

  note <- NULL
  if (length(moved)) {
    tt <- table(paste(from_lab, "\u2192", moved))
    note <- paste0("Re-tagged ", length(moved), " pitch",
                   if (length(moved) == 1) "" else "es", ": ",
                   paste(sprintf("%s (%d)", names(tt), as.integer(tt)),
                         collapse = ", "),
                   if (sum(dropv)) sprintf("; %d left out (matched nothing)",
                                           sum(dropv)) else "")
  } else if (any(dropv)) {
    note <- sprintf("%d pitch(es) left out (matched none of the kept types).",
                    sum(dropv))
  }
  list(pool = pool, note = note)
}

# ---- count adjustment: removes the count confound ---------------------------
# Raw mean run value by pitch type is confounded by WHICH COUNTS a pitch is
# thrown in. Fastballs live in hitters' counts (2-0, 3-1) and get hit; breaking
# balls live in 0-2/1-2 and collect cheap whiffs. Subtracting the mean within
# each ball-strike cell compares a pitch only to other pitches in the SAME
# count -- which is what lets a genuinely good fastball grade as good.
pa_count_adjust <- function(rv, balls, strikes) {
  cell <- paste(pmin(pmax(round(balls), 0), 3),
                pmin(pmax(round(strikes), 0), 2), sep = "-")
  rv - stats::ave(rv, cell, FUN = function(z) mean(z, na.rm = TRUE))
}

# ---- similarity-pooled value: the Matchup Matrix's own logic ----------------
# Values a pitch by its SHAPE, not by the handful of times this pitcher threw
# it: find every similar pitch in the league (same 7 MATCHUP_FEATURES, same
# z-radius) and score that large pool. This is the reason the Matchup Matrix
# can rank a fastball correctly while a per-pitcher raw average cannot.
pa_similar_value <- function(pool_std, rv_pool, target_std,
                             threshold = SIMILARITY_THRESHOLD) {
  d2  <- rowSums(sweep(pool_std, 2, target_std, "-")^2)
  idx <- which(d2 <= threshold^2)
  if (length(idx) < 25) return(c(value = NA_real_, n = length(idx)))
  w <- 1 / pmax(sqrt(d2[idx]), DISTANCE_FLOOR)
  c(value = sum(w * rv_pool[idx], na.rm = TRUE) / sum(w, na.rm = TRUE),
    n = length(idx))
}

# ---- setup value: what a pitch does for the pitch that FOLLOWS it ----------
# This is the fastball's and the sinker's actual job, and no per-pitch run value
# can see it: they are not valuable in a vacuum, they are valuable because they
# set up the next pitch.
#
# Measured as LIFT, not as the raw value of the next pitch. Raw next-pitch value
# is confounded by WHICH pitch tends to follow: a sinker is often followed by
# another sinker (a modest-value type), which made the sinker look like a poor
# setup pitch even when the sliders it did set up were beating their baseline.
# Lift credits a pitch with how much the FOLLOWING pitch outperformed the league
# average for that following pitch's own type, which removes the confound.
#
# Returns league-level values per pitch type plus this-pitcher-per-type values
# and their sample sizes, so the caller can shrink one toward the other.
pa_setup_value <- function(d) {
  if (!all(c("PitchNo", "Batter") %in% names(d))) return(NULL)
  o  <- order(d$Pitcher, d$Batter, d$PitchNo)
  dd <- d[o, , drop = FALSE]
  n  <- nrow(dd); if (n < 200) return(NULL)
  key    <- paste(dd$Pitcher, dd$Batter)
  ty     <- as.character(dd$TaggedPitchType)
  nxt_rv <- c(utils::tail(dd$rv_adj, -1), NA_real_)
  nxt_ty <- c(utils::tail(ty, -1), NA_character_)
  same   <- c(key[-1] == utils::head(key, -1), FALSE)
  nxt_rv[!same] <- NA_real_; nxt_ty[!same] <- NA_character_
  ok <- !is.na(nxt_rv) & !is.na(nxt_ty)
  if (sum(ok) < 100) return(NULL)
  base_ty <- tapply(dd$rv_adj, ty, mean, na.rm = TRUE)   # league baseline per type
  lift <- nxt_rv[ok] - as.numeric(base_ty[nxt_ty[ok]])
  cur  <- ty[ok]; pit <- as.character(dd$Pitcher)[ok]
  k2   <- paste(pit, cur, sep = "\u241F")
  list(league = tapply(lift, cur, mean, na.rm = TRUE),
       own    = tapply(lift, k2,  mean, na.rm = TRUE),
       own_n  = tapply(lift, k2,  function(z) sum(is.finite(z))))
}

# ---- staff exposure reference ----------------------------------------------
# Median exposure per pitch (A / N) across the staff. A starter who faces a
# lineup three times has a high A/N; a reliever who sees each hitter once has a
# low one. Used to modulate how hard each pitcher is pushed to diversify.
.pa_expo_cache <- new.env(parent = emptyenv())
pa_staff_expo <- function(d) {
  key <- paste0("e_", nrow(d))
  hit <- .pa_expo_cache[[key]]
  if (!is.null(hit)) return(hit)
  sp <- split(seq_len(nrow(d)), d$Pitcher)
  vals <- numeric(0)
  for (nm in names(sp)) {
    idx <- sp[[nm]]; N <- length(idx)
    if (N < 100) next
    A <- if ("BatterId" %in% names(d)) {
      bb <- table(d$BatterId[idx])
      if (length(bb)) sum(as.numeric(bb)^2) else N
    } else N
    if (!is.finite(A) || A <= 0) A <- N
    vals <- c(vals, A / N)
  }
  out <- if (length(vals) >= 5) stats::median(vals) else NA_real_
  .pa_expo_cache[[key]] <- out
  out
}

# ---- fixed league scale for the value components ----------------------------
# Spread of per-(pitcher, pitch-type) run values across the whole league. Used
# as a constant divisor so a pitch's score never depends on what else is in the
# pitcher's repertoire.
.pa_scale_cache <- new.env(parent = emptyenv())
pa_league_scale <- function(d) {
  key <- paste0("sc_", nrow(d))
  hit <- .pa_scale_cache[[key]]
  if (!is.null(hit)) return(hit)
  agg <- tapply(d$rv_adj, paste(d$Pitcher, d$TaggedPitchType, sep = "\u241F"),
                mean, na.rm = TRUE)
  s <- stats::sd(as.numeric(agg), na.rm = TRUE)
  if (!is.finite(s) || s <= 0) s <- 1
  .pa_scale_cache[[key]] <- s
  s
}

# ---- solve the mix to a target concentration --------------------------------
# Bisects the penalty strength q until the mix reaches the requested Herfindahl.
# q is searched RELATIVE TO THE COST SPREAD, so no magnitude of delta or A can
# push the answer to an even mix -- the earlier design let a large delta*A
# swamp the cost vector and flatten everything. delta and exposure now enter
# through the TARGET (see pa_build_model), not through q directly.
pa_mix_for_target <- function(c_vec, target_h, floor = PA_FLOOR) {
  c_vec <- as.numeric(c_vec); k <- length(c_vec)
  if (k < 2) return(rep(1 / max(k, 1), max(k, 1)))
  sp <- diff(range(c_vec))
  if (!is.finite(sp) || sp <= 0) return(rep(1 / k, k))
  if (!is.finite(target_h)) target_h <- 1 / k
  hf  <- function(q) sum(pa_water_fill(c_vec, q, floor)^2)
  qlo <- sp * 1e-4; qhi <- sp * 1e4
  if (hf(qlo) <= target_h) return(pa_water_fill(c_vec, qlo, floor))
  if (hf(qhi) >= target_h) return(pa_water_fill(c_vec, qhi, floor))
  for (it in seq_len(60)) {
    qm <- sqrt(qlo * qhi)
    if (hf(qm) > target_h) qlo <- qm else qhi <- qm
  }
  pa_water_fill(c_vec, sqrt(qlo * qhi), floor)
}


# ---- robust water-filling core ---------------------------------------------
# Usage shares minimising sum(c*p) + (q/2)*sum(p^2) on the simplex under a
# per-pitch floor. Every failure path returns an even mix instead of throwing,
# and a flat cost vector short-circuits immediately.
pa_water_fill <- function(c_vec, q, floor = PA_FLOOR) {
  c_vec <- as.numeric(c_vec); k <- length(c_vec)
  if (k < 1) return(numeric(0))
  if (k == 1) return(1)
  fl <- min(floor, 1 / k)
  if (any(!is.finite(c_vec))) {
    fin <- c_vec[is.finite(c_vec)]
    c_vec[!is.finite(c_vec)] <- if (length(fin)) mean(fin) else 0
  }
  if (diff(range(c_vec)) <= 0) return(rep(1 / k, k))   # nothing to separate
  if (!is.finite(q) || q <= 0) q <- 1e-9
  g  <- function(mu) sum(pmax(fl, (mu - c_vec) / q)) - 1
  lo <- min(c_vec) - 2 * q - 1
  hi <- max(c_vec) + 2 * q + 1
  mu <- tryCatch(stats::uniroot(g, lower = lo, upper = hi,
                                extendInt = "yes")$root,
                 error = function(e) NA_real_)
  if (!is.finite(mu)) return(rep(1 / k, k))
  p <- pmax(fl, (mu - c_vec) / q)
  s <- sum(p)
  if (!is.finite(s) || s <= 0) return(rep(1 / k, k))
  p / s
}



# Main builder. pool = full season frame (all pitchers); pitcher = target name.
pa_build_model <- function(pool, pitcher) {
  # stuff_plus is OPTIONAL: it only drives the stuff-tier readout, so an NCAA
  # frame without it still models a mix (a hard requirement here was silently
  # blanking opponent arms that had thousands of pitches).
  need <- c("Pitcher", "TaggedPitchType", "PitchCall", "PlayResult")
  if (!all(need %in% names(pool))) return(NULL)
  has_stuff <- "stuff_plus" %in% names(pool)
  d <- pool[!is.na(pool$TaggedPitchType) & pool$TaggedPitchType != "", , drop = FALSE]
  if (nrow(d) < 50 || !(pitcher %in% d$Pitcher)) return(NULL)

  # 1. per-pitch run value in the SAME currency as the Matchup Matrix
  #    (map_dre_event + weights_vec, PITCHER perspective: higher = better),
  #    then COUNT-ADJUSTED so a pitch is only compared to pitches in the same
  #    ball-strike count.
  d$rv_p   <- pa_pitch_dre(d)
  d$rv_adj <- if (all(c("Balls", "Strikes") %in% names(d))) {
    pa_count_adjust(d$rv_p, d$Balls, d$Strikes)
  } else d$rv_p

  dp <- d[d$Pitcher == pitcher, , drop = FALSE]
  if (nrow(dp) < 20) return(NULL)
  cnt_all <- table(dp$TaggedPitchType)
  types   <- names(cnt_all)[cnt_all > 0]
  if (length(types) < 2) return(NULL)
  n_by    <- as.numeric(cnt_all[types])

  # 2. value each pitch by its SHAPE against the whole league, the way the
  #    Matchup Matrix does -- not by the handful of times he threw it.
  scaler  <- tryCatch(build_feature_scaler(d), error = function(e) NULL)
  feat_ok <- !is.null(scaler) && all(MATCHUP_FEATURES %in% names(d))
  v_sim <- setNames(rep(NA_real_, length(types)), types)
  n_sim <- setNames(rep(0L,       length(types)), types)
  if (feat_ok) {
    ok <- stats::complete.cases(d[, MATCHUP_FEATURES])
    if (sum(ok) > 200) {
      pool_std <- standardize_features(d[ok, , drop = FALSE], scaler)
      rv_pool  <- d$rv_adj[ok]
      for (pt in types) {
        rows <- dp[dp$TaggedPitchType == pt, , drop = FALSE]
        rows <- rows[stats::complete.cases(rows[, MATCHUP_FEATURES]), , drop = FALSE]
        if (nrow(rows) == 0) next
        tstd <- colMeans(standardize_features(rows, scaler), na.rm = TRUE)
        if (any(!is.finite(tstd))) next
        res <- pa_similar_value(pool_std, rv_pool, tstd)
        v_sim[pt] <- res[["value"]]; n_sim[pt] <- as.integer(res[["n"]])
      }
    }
  }
  own_adj <- tapply(dp$rv_adj, dp$TaggedPitchType, mean, na.rm = TRUE)
  for (pt in types) if (is.na(v_sim[[pt]])) v_sim[pt] <- own_adj[[pt]]
  v_sim[!is.finite(v_sim)] <- 0

  # 3. usage credibility. A pitch he has barely thrown is pulled toward
  #    NEUTRAL (his repertoire mean), so the model cannot recommend 27% of a
  #    changeup he has thrown three times. Well-sampled pitches keep their
  #    own value.
  m_rep <- if (sum(n_by) > 0) sum(n_by * v_sim[types]) / sum(n_by) else 0
  value <- setNames(vapply(seq_along(types), function(i)
             m_rep + .cred(n_by[i], PA_K_RV) * (v_sim[[types[i]]] - m_rep),
             numeric(1)), types)

  # 4. setup value: credit each pitch for how much the FOLLOWING pitch beat its
  #    own league baseline. His own setup record is shrunk toward the league
  #    value for that pitch type, so a sinker he actually tunnels off gets
  #    credit without a thin sample inventing one.
  seqv  <- tryCatch(pa_setup_value(d), error = function(e) NULL)
  s_val <- setNames(rep(0, length(types)), types)
  if (!is.null(seqv)) {
    for (pt in types) {
      lg <- if (pt %in% names(seqv$league)) as.numeric(seqv$league[[pt]]) else 0
      if (!is.finite(lg)) lg <- 0
      kk <- paste(pitcher, pt, sep = "\u241F")
      ow <- if (kk %in% names(seqv$own))   as.numeric(seqv$own[[kk]])   else NA_real_
      nn <- if (kk %in% names(seqv$own_n)) as.numeric(seqv$own_n[[kk]]) else 0
      s_val[pt] <- if (is.finite(ow) && is.finite(nn) && nn > 0) {
        lg + .cred(nn, PA_K_RV) * (ow - lg)
      } else lg
    }
    s_val[!is.finite(s_val)] <- 0
  }

  # 5. composite objective: run value PA_W_RV + setup PA_W_SEQ.
  #    Each component is divided by a FIXED LEAGUE SCALE, not by the spread of
  #    this pitcher's own repertoire. Repertoire-relative z-scoring made every
  #    pitch's score depend on which OTHER pitches were in the set, so
  #    un-ticking one 3-pitch changeup visibly moved the fastball. With a fixed
  #    scale each pitch is scored on its own merits and the mix is stable.
  #    (Centering is irrelevant here: adding a constant to every cost shifts the
  #    water level, not the shares.)
  sc_rv <- pa_league_scale(d)
  sc_sq <- if (!is.null(seqv)) stats::sd(as.numeric(seqv$league), na.rm = TRUE) else NA_real_
  if (!is.finite(sc_sq) || sc_sq <= 0) sc_sq <- 1
  vv <- as.numeric(value[types]); vv[!is.finite(vv)] <- 0
  ss <- as.numeric(s_val[types]); ss[!is.finite(ss)] <- 0
  composite <- PA_W_RV * (vv / sc_rv) + PA_W_SEQ * (ss / sc_sq)
  names(composite) <- types

  # 6. solve.
  #    SIGN: the solver MINIMISES sum(base * p), so LOWER base earns
  #    MORE usage. composite is "higher = better for the pitcher", so the base
  #    is its negative -- this is what makes a good pitch get thrown more.
  fam <- pa_get_familiarity(pool)
  N_total    <- sum(n_by)
  actual_mix <- setNames(n_by / N_total, types)
  A <- if ("BatterId" %in% names(dp)) {
    sum(as.numeric(table(dp$BatterId))^2)
  } else N_total
  base_by <- -composite
  k_types <- length(types)
  h_uni   <- 1 / k_types
  h_obs   <- tryCatch(pa_observed_herf(d, k_types), error = function(e) NA_real_)
  anchor  <- if (is.finite(h_obs)) max(h_obs, h_uni) else max(0.35, h_uni)

  # Familiarity enters here: PA_W_FAM sets how far the mix is flattened from
  # what real pitchers throw toward an even mix, scaled by THIS pitcher's
  # exposure relative to the staff. More exposure (a starter facing a lineup
  # repeatedly) = more repeat looks = pushed to diversify harder.
  expo_med <- tryCatch(pa_staff_expo(d), error = function(e) NA_real_)
  expo_i   <- if (N_total > 0) A / N_total else NA_real_
  ratio    <- if (is.finite(expo_med) && expo_med > 0 && is.finite(expo_i)) {
    max(0.5, min(1.5, expo_i / expo_med))
  } else 1
  target_h <- max(anchor - PA_W_FAM * (anchor - h_uni) * ratio, h_uni)

  modeled_raw <- pa_mix_for_target(N_total * as.numeric(base_by), target_h)

  # USAGE ANCHOR: only move a pitch as far as the evidence supports. A pitch
  # thrown 3 times stays at its observed share no matter how its value grades;
  # a pitch thrown 300 times is free to move. Without this, a no-information
  # pitch inherits the repertoire average, which outranks a well-measured
  # below-average pitch and gets recommended at 40%.
  cred_u  <- vapply(n_by, function(z) .cred(z, PA_K_USAGE), numeric(1))
  blended <- cred_u * as.numeric(modeled_raw) +
             (1 - cred_u) * as.numeric(actual_mix)
  sb <- sum(blended)
  modeled <- if (is.finite(sb) && sb > 0) blended / sb else as.numeric(modeled_raw)
  names(modeled) <- types
  div_scale <- ratio

  # Displayed run value stays in RUNS (not z units): use the count-adjusted
  # value. Negative = fewer runs allowed = good.
  rv_change <- N_total * (-as.numeric(value[types])) *
                 (as.numeric(modeled) - as.numeric(actual_mix))
  gap_pp <- 100 * 0.5 * sum(abs(actual_mix - modeled))

  # stuff tier vs the pool median (skipped when the frame has no stuff grades)
  this_stuff <- NA_real_; tier <- "unknown"
  if (has_stuff && any(is.finite(d$stuff_plus))) {
    pstuff <- tapply(d$stuff_plus, d$Pitcher, mean, na.rm = TRUE)
    med <- stats::median(pstuff, na.rm = TRUE)
    this_stuff <- as.numeric(pstuff[pitcher])
    if (is.finite(this_stuff) && is.finite(med))
      tier <- if (this_stuff >= med) "above-average" else "below-average"
  }

  # ---- diagnostics: where does the signal come from / die? ----
  dbg <- list(
    n_rows_pool  = nrow(d),
    n_rows_pitch = nrow(dp),
    n_pitchers   = length(unique(d$Pitcher)),
    ev_na_pct    = round(100 * mean(map_dre_event(d$PitchCall, d$PlayResult) == "NA",
                                    na.rm = TRUE), 1),
    rv_sd        = signif(stats::sd(d$rv_p, na.rm = TRUE), 3),
    rv_adj_sd    = signif(stats::sd(d$rv_adj, na.rm = TRUE), 3),
    feat_ok      = feat_ok,
    v_sim        = round(v_sim[types] * 100, 3),
    n_sim        = n_sim[types],
    value100     = round(as.numeric(value[types]) * 100, 3),
    seq100       = round(as.numeric(s_val[types]) * 100, 3),
    setup_n      = vapply(types, function(pt) {
                     kk <- paste(pitcher, pt, sep = "\u241F")
                     if (!is.null(seqv) && kk %in% names(seqv$own_n))
                       as.integer(seqv$own_n[[kk]]) else 0L
                   }, integer(1)),
    composite    = round(as.numeric(composite), 3),
    comp_sd      = signif(stats::sd(as.numeric(composite)), 3),
    h_obs        = signif(h_obs, 3),
    h_uni        = signif(1 / length(types), 3),
    div_scale    = signif(div_scale, 4),
    cost_spread  = signif(diff(range(N_total * as.numeric(base_by))), 4),
    target_h     = signif(target_h, 4),
    cred_u       = cred_u,
    modeled_raw  = modeled_raw,
    achieved_h   = signif(sum(as.numeric(modeled)^2), 4),
    delta_used   = signif(fam$delta_used, 4),
    A            = signif(A, 4),
    N_total      = N_total
  )

  list(pitcher = pitcher, types = types, dbg = dbg,
       actual = actual_mix, modeled = modeled, rv_change = rv_change,
       total_rv_change = sum(rv_change), gap_pp = gap_pp,
       value = value, n_by = n_by, n_sim = n_sim, h_obs = h_obs,
       stuff_plus = this_stuff, stuff_tier = tier, n_pitches = N_total,
       fam = fam)
}
