# PITCH RECLASSIFICATION — non-Coastal pitchers only
# ----------------------------------------------------------------------------
# Implements the "partition before naming" methodology (pitch-classification-
# methodology.md): the labels arriving in a TruMedia/TrackMan frame are not
# trusted for non-Coastal arms (operator tag vs auto-tag agree 64.7%; sweepers
# 0%). Coastal pitchers are custom-tagged at home games and are NEVER touched.
#
# Pipeline, per pitcher-season, geometry first and labels never consulted:
#   Stage 0    multi-pass cleaning + spin-error detection (movement-cluster
#              baseline, tags may rescue a pitch but never condemn one)
#   Stage 1    velocity bands first, then movement-weighted Ward over-
#              clustering on robust-scaled features (deliberate over-split)
#   Stage 2    agglomerate fragment centroids in a FIXED physical space
#              (same mph/inch/rpm conversion for every pitcher), fold tiny
#              fragments into the nearest survivor (floor = 3 pitches)
#   Stage 2.5  false-split repair: 1-D projection onto the between-centroid
#              axis, seeded 2-component vs 1-component BIC; negative score =
#              no valley = one pitch. One merge per iteration, most unimodal
#              first, centroids recomputed.
#   Stage 2.6  misplaced-boundary repair: refit the seeded 2-component model
#              on kept pairs, reassign by posterior. Pitches only move within
#              the pair; a reassignment that would dissolve a side is skipped.
#   Stage 3    family-aware naming. NOTE: the paper's namer is a GBM
#              recalibrated from operator corrections; no correction store or
#              shipped model exists in this app, so naming here is a
#              deterministic prototype namer over the same cross-pitcher
#              features (velocity gap from the hardest cluster, IVB, arm-side-
#              mirrored HB, spin), with the paper's structural rule kept
#              intact: changeup + splitter are ONE family, named by POOLED
#              spin against a threshold, splitting into two pitches only when
#              the sides pool apart by a real gap.
#
# Error identification (nothing is silently dropped):
#   OriginalPitchType     the tag as it arrived
#   PitchTypeReclassified TRUE where the new name differs from the tag
#   PitchFamilyChanged    TRUE where fastball/breaking/offspeed FAMILY moved —
#                         a real error under any vocabulary, vs a naming
#                         convention difference (cutter-vs-slider etc.)
#   SpinErrorFlag         spin value physically extreme AND far from its own
#                         movement-cluster median (harmonic misreads); the
#                         value is imputed for clustering, the row is kept
# Determinism: rows are lexicographically pre-sorted before any clustering,
# Ward/average linkage only, one merge per iteration — same pitches in a
# different row order produce the same arsenal.
# ============================================================================

RC_MIN_PITCHES   <- 25    # below this, clustering is unstable: keep the tag
RC_FRAGMENT_MIN  <- 3     # a genuine 3-pitch changeup must survive
RC_CUT_H         <- 1.55  # agglomeration cut, fixed physical units
RC_PAIR_RADIUS   <- 2.6   # centroid radius for merge/boundary tests
# Fixed physical space: 1 unit = 2.5 mph = 5.5 in of break = 1200 rpm
# (spin present but weighted lightly — noisiest axis for low-spin offspeed)
RC_PHYS_SCALE    <- c(velo = 2.5, ivb = 5.5, hb = 5.5, spin = 1200)
RC_OFFSPEED_SPIN_SPLIT <- 1300  # pooled-spin ruler (median SPL 947, CH 1703)
RC_OFFSPEED_SPIN_GAP   <- 700   # real CH+SPL pairs sit >1000 apart; a single
                                # noisy offspeed straddled the ruler by 439

# ---- canonical vocabulary + families ---------------------------------------
.RC_TAG_MAP <- c(
  Fastball="Fastball", FourSeamFastball="Fastball", FourSeamFastBall="Fastball",
  `Four-Seam`="Fastball", `4-Seam`="Fastball", `Four-Seam Fastball`="Fastball",
  `4-Seam Fastball`="Fastball", FF="Fastball", FA="Fastball",
  Sinker="Sinker", TwoSeamFastBall="Sinker", TwoSeamFastball="Sinker",
  `Two-Seam`="Sinker", SI="Sinker", FT="Sinker",
  Cutter="Cutter", FC="Cutter", CT="Cutter",
  Slider="Slider", SL="Slider", Slurve="Slider", SV="Slider",
  Sweeper="Sweeper", ST="Sweeper", SW="Sweeper",
  Curveball="Curveball", CurveBall="Curveball", CU="Curveball", CB="Curveball",
  KC="Curveball", `Knuckle Curve`="Curveball",
  ChangeUp="ChangeUp", Changeup="ChangeUp", CH="ChangeUp",
  Splitter="Splitter", FS="Splitter", SP="Splitter",
  Knuckleball="Knuckleball", KN="Knuckleball")

.rc_canon_tag <- function(x) {
  x <- as.character(x)
  hit <- unname(.RC_TAG_MAP[x])
  ifelse(is.na(hit), x, hit)
}

.RC_FAMILY_MAP <- c(Fastball="fastball", Sinker="fastball",
                    Cutter="breaking", Slider="breaking", Sweeper="breaking",
                    Curveball="breaking",
                    ChangeUp="offspeed", Splitter="offspeed")

.rc_family <- function(x) unname(.RC_FAMILY_MAP[as.character(x)])

# ---- small numeric helpers --------------------------------------------------
.rc_robust_scale <- function(x) {
  med <- stats::median(x, na.rm = TRUE)
  iqr <- stats::IQR(x, na.rm = TRUE)
  if (!is.finite(iqr) || iqr < 1e-6) iqr <- max(stats::sd(x, na.rm = TRUE), 1e-6)
  (x - med) / iqr
}

# 1-component Gaussian BIC on a 1-D projection
.rc_bic1 <- function(x) {
  n <- length(x)
  s2 <- max(stats::var(x) * (n - 1) / n, 1e-9)
  ll <- sum(stats::dnorm(x, mean(x), sqrt(s2), log = TRUE))
  -2 * ll + 2 * log(n)
}

# Seeded 2-component 1-D Gaussian EM. Returns BIC and posteriors for comp 1.
.rc_gmm2 <- function(x, seed1) {
  n <- length(x)
  r <- as.numeric(seed1)                     # responsibility of component 1
  mu <- c(0, 0); s2 <- c(1, 1); pi1 <- 0.5
  for (it in 1:60) {
    w1 <- sum(r); w2 <- n - w1
    if (w1 < 1e-6 || w2 < 1e-6) break
    mu[1] <- sum(r * x) / w1;        mu[2] <- sum((1 - r) * x) / w2
    s2[1] <- max(sum(r * (x - mu[1])^2) / w1, 1e-9)
    s2[2] <- max(sum((1 - r) * (x - mu[2])^2) / w2, 1e-9)
    pi1   <- w1 / n
    d1 <- pi1 * stats::dnorm(x, mu[1], sqrt(s2[1]))
    d2 <- (1 - pi1) * stats::dnorm(x, mu[2], sqrt(s2[2]))
    r_new <- d1 / pmax(d1 + d2, 1e-300)
    if (max(abs(r_new - r)) < 1e-7) { r <- r_new; break }
    r <- r_new
  }
  d1 <- pi1 * stats::dnorm(x, mu[1], sqrt(s2[1]))
  d2 <- (1 - pi1) * stats::dnorm(x, mu[2], sqrt(s2[2]))
  ll <- sum(log(pmax(d1 + d2, 1e-300)))
  list(bic = -2 * ll + 5 * log(n), post1 = d1 / pmax(d1 + d2, 1e-300))
}

# Ward clustering with deterministic lexicographic pre-sort; returns integer
# labels in ORIGINAL row order. `k` clusters on the scaled feature matrix.
.rc_ward <- function(M, k) {
  ord <- do.call(order, as.data.frame(M))
  hc  <- stats::hclust(stats::dist(M[ord, , drop = FALSE]), method = "ward.D2")
  lab_sorted <- stats::cutree(hc, k = min(k, nrow(M)))
  lab <- integer(nrow(M)); lab[ord] <- lab_sorted
  lab
}

# ---- Stage 0: spin-error detection ------------------------------------------
# Cluster on movement ALONE (spin excluded so a spin error cannot distort the
# baseline it is judged against); flag values that are both physically extreme
# and far from their own movement-cluster median. The operator tag may RESCUE
# a flagged pitch (plausible for the tag) but never condemns one.
.rc_spin_flags <- function(ivb, hb, spin, tag) {
  n <- length(spin)
  flag <- rep(FALSE, n)
  ok <- is.finite(ivb) & is.finite(hb) & is.finite(spin)
  if (sum(ok) < 12) return(flag)
  M <- cbind(.rc_robust_scale(ivb[ok]), .rc_robust_scale(hb[ok]))
  k <- max(1L, min(4L, floor(sum(ok) / 30)))
  lab <- if (k > 1) .rc_ward(M, k) else rep(1L, sum(ok))
  med <- tapply(spin[ok], lab, stats::median)
  cl_med <- med[as.character(lab)]
  cand <- spin[ok] > 3200 & spin[ok] > 1.6 * cl_med   # harmonic misreads ~2x
  # tag rescue: genuinely high-spin breaking balls stay
  rescue <- .rc_canon_tag(tag[ok]) %in%
    c("Curveball","Slider","Sweeper","Cutter") & spin[ok] <= 3600
  flag[ok] <- cand & !rescue
  flag
}

# ---- prototype namer (Stage 3 model surrogate) -------------------------------
# Features per cluster mean: vdiff (mph below the pitcher's hardest cluster),
# IVB, arm-side-mirrored HB (arm side positive for both hands), spin.
.rc_prototypes <- function() {
  rbind(
    Fastball  = c(vdiff = 0.0, ivb = 16, hb =   8, spin = 2200),
    Sinker    = c(vdiff = 1.5, ivb =  9, hb =  15, spin = 2100),
    Cutter    = c(vdiff = 3.5, ivb = 10, hb =  -1, spin = 2300),
    Slider    = c(vdiff = 8.0, ivb =  3, hb =  -5, spin = 2350),
    Sweeper   = c(vdiff = 9.0, ivb =  2, hb = -14, spin = 2500),
    Curveball = c(vdiff = 12., ivb = -8, hb =  -8, spin = 2400),
    ChangeUp  = c(vdiff = 9.0, ivb =  7, hb =  14, spin = 1700),
    Splitter  = c(vdiff = 8.0, ivb =  4, hb =   8, spin =  950)
  )
}

.rc_name_clusters <- function(cl_stats) {
  # cl_stats: data.frame(cluster, n, velo, ivb, hb_as, spin)
  proto <- .rc_prototypes()
  scl <- c(vdiff = 2.5, ivb = 4.5, hb = 4.5, spin = 600)
  vmax <- max(cl_stats$velo)
  feats <- cbind(vdiff = vmax - cl_stats$velo, ivb = cl_stats$ivb,
                 hb = cl_stats$hb_as, spin = cl_stats$spin)
  D <- sapply(rownames(proto), function(p)
    sqrt(rowSums(((feats - matrix(proto[p, ], nrow(feats), 4, byrow = TRUE)) /
                    matrix(scl, nrow(feats), 4, byrow = TRUE))^2)))
  D <- matrix(D, nrow = nrow(feats), dimnames = list(NULL, rownames(proto)))
  # soft class weights so the changeup+splitter FAMILY can be pooled
  W <- exp(-D); W <- W / pmax(rowSums(W), 1e-12)
  name <- rownames(proto)[apply(D, 1, which.min)]

  # Structural rule: CH + SPL are one family. A cluster whose combined
  # offspeed mass beats every single class is offspeed even if a third class
  # edges out each half individually.
  off_mass <- W[, "ChangeUp"] + W[, "Splitter"]
  best_single <- apply(W, 1, max)
  is_off <- name %in% c("ChangeUp", "Splitter") | off_mass >= best_single
  if (any(is_off)) {
    off <- which(is_off)
    pooled <- sum(cl_stats$spin[off] * cl_stats$n[off]) / sum(cl_stats$n[off])
    if (length(off) >= 2) {
      m <- cl_stats$spin[off]
      lo <- min(m); hi <- max(m)
      if (hi - lo >= RC_OFFSPEED_SPIN_GAP &&
          lo < RC_OFFSPEED_SPIN_SPLIT && hi >= RC_OFFSPEED_SPIN_SPLIT) {
        # genuinely both: each side of the ruler, far enough apart
        name[off] <- ifelse(m < RC_OFFSPEED_SPIN_SPLIT, "Splitter", "ChangeUp")
      } else {
        name[off] <- if (pooled < RC_OFFSPEED_SPIN_SPLIT) "Splitter" else "ChangeUp"
      }
    } else {
      name[off] <- if (pooled < RC_OFFSPEED_SPIN_SPLIT) "Splitter" else "ChangeUp"
    }
  }
  name   # clusters sharing a name surface as one pitch downstream
}

# ---- the per-pitcher(-season) engine ----------------------------------------
.rc_classify_one <- function(velo, ivb, hb, spin_in, hand, tag) {
  n <- length(velo)
  spin <- spin_in
  # impute nulls from the pitcher's OWN distribution, not a league constant
  for (v in c("velo", "ivb", "hb", "spin")) {
    x <- get(v)
    if (anyNA(x)) { x[!is.finite(x)] <- stats::median(x, na.rm = TRUE); assign(v, x) }
  }
  # spin can be entirely missing on some feeds: neutralize the axis so it
  # contributes nothing rather than poisoning every distance with NaN
  if (!any(is.finite(spin))) spin <- rep(2200, n)
  spin_flag <- .rc_spin_flags(ivb, hb, spin, tag)
  if (any(spin_flag)) {
    # flagged values are imputed for clustering; the pitch itself is kept
    keep_med <- stats::median(spin[!spin_flag], na.rm = TRUE)
    spin[spin_flag] <- keep_med
  }

  # ---- Stage 1: velocity bands first, then movement within band ----
  ordv <- order(velo, ivb, hb, spin)
  hcv <- stats::hclust(stats::dist(velo[ordv]), method = "average")
  band_sorted <- stats::cutree(hcv, h = 6)          # bands >~6 mph apart
  band <- integer(n); band[ordv] <- band_sorted

  frag <- integer(n); base <- 0L
  for (b in sort(unique(band))) {
    idx <- which(band == b); m <- length(idx)
    k_fine <- max(1L, min(m, floor(m / 8) + 2L, 20L))
    if (m <= 2 || k_fine == 1) {
      frag[idx] <- base + 1L; base <- base + 1L; next
    }
    M <- cbind(.rc_robust_scale(velo[idx]),
               1.6 * .rc_robust_scale(ivb[idx]),    # movement weighted within band
               1.6 * .rc_robust_scale(hb[idx]),
               0.5 * .rc_robust_scale(spin[idx]))
    M[!is.finite(M)] <- 0
    lab <- .rc_ward(M, k_fine)
    frag[idx] <- base + lab
    base <- base + max(lab)
  }

  # ---- Stage 2: agglomerate fragment centroids in FIXED physical space ----
  phys <- cbind(velo / RC_PHYS_SCALE["velo"], ivb / RC_PHYS_SCALE["ivb"],
                hb / RC_PHYS_SCALE["hb"],    spin / RC_PHYS_SCALE["spin"])
  cent <- function(assign_vec) {
    ids <- sort(unique(assign_vec))
    t(sapply(ids, function(i) colMeans(phys[assign_vec == i, , drop = FALSE])))
  }
  fr_ids <- sort(unique(frag))
  fr_cent <- cent(frag)
  if (length(fr_ids) > 1) {
    hc2 <- stats::hclust(stats::dist(fr_cent), method = "average")
    macro_of_frag <- stats::cutree(hc2, h = RC_CUT_H)
  } else macro_of_frag <- 1L
  cl <- macro_of_frag[match(frag, fr_ids)]

  # fold fragments below the floor into the nearest surviving cluster
  repeat {
    sizes <- table(cl)
    small <- as.integer(names(sizes)[sizes < RC_FRAGMENT_MIN])
    if (length(small) == 0 || length(sizes) <= 1) break
    s <- small[1]
    cc <- cent(cl); ids <- sort(unique(cl))
    d <- sqrt(rowSums((cc - matrix(cc[match(s, ids), ], nrow(cc), 4, byrow = TRUE))^2))
    d[match(s, ids)] <- Inf
    cl[cl == s] <- ids[which.min(d)]
  }

  # ---- Stage 2.5: false-split merge (one pair per iteration) ----
  repeat {
    ids <- sort(unique(cl))
    if (length(ids) <= 1) break
    cc <- cent(cl)
    best <- NULL; best_score <- 0
    for (a in seq_along(ids)) for (b in seq_along(ids)) {
      if (b <= a) next
      axis <- cc[b, ] - cc[a, ]
      if (sqrt(sum(axis^2)) > RC_PAIR_RADIUS) next
      sel <- cl %in% ids[c(a, b)]
      x <- as.numeric(phys[sel, , drop = FALSE] %*% (axis / sqrt(sum(axis^2))))
      seed1 <- cl[sel] == ids[a]
      score <- .rc_bic1(x) - .rc_gmm2(x, seed1)$bic   # negative = no valley
      if (score < best_score) { best_score <- score; best <- c(ids[a], ids[b]) }
    }
    if (is.null(best)) break
    cl[cl == best[2]] <- best[1]                      # most unimodal pair merges
  }

  # ---- Stage 2.6: misplaced-boundary refinement on kept pairs ----
  ids <- sort(unique(cl))
  if (length(ids) > 1) {
    cc <- cent(cl)
    for (a in seq_along(ids)) for (b in seq_along(ids)) {
      if (b <= a) next
      axis <- cc[b, ] - cc[a, ]
      dlen <- sqrt(sum(axis^2))
      if (dlen > RC_PAIR_RADIUS) next
      sel <- which(cl %in% ids[c(a, b)])
      x <- as.numeric(phys[sel, , drop = FALSE] %*% (axis / dlen))
      seed1 <- cl[sel] == ids[a]
      post1 <- .rc_gmm2(x, seed1)$post1
      new_lab <- ifelse(post1 >= 0.5, ids[a], ids[b])  # only within the pair
      if (sum(new_lab == ids[a]) >= RC_FRAGMENT_MIN &&
          sum(new_lab == ids[b]) >= RC_FRAGMENT_MIN) {
        cl[sel] <- new_lab                             # else: skip entirely
      }
    }
  }

  # ---- Stage 3: family-aware naming ----
  h1 <- hand[!is.na(hand) & nzchar(hand)][1]
  if (is.na(h1) || length(h1) == 0) h1 <- "R"
  hb_as <- if (toupper(substr(trimws(h1), 1, 1)) == "L") -hb else hb
  ids <- sort(unique(cl))
  cl_stats <- data.frame(
    cluster = ids,
    n     = as.integer(table(cl)[as.character(ids)]),
    velo  = tapply(velo,  cl, mean)[as.character(ids)],
    ivb   = tapply(ivb,   cl, mean)[as.character(ids)],
    hb_as = tapply(hb_as, cl, mean)[as.character(ids)],
    spin  = tapply(spin,  cl, mean)[as.character(ids)]
  )
  nm <- .rc_name_clusters(cl_stats)
  list(type = nm[match(cl, ids)], spin_flag = spin_flag,
       n_clusters = length(ids), n_named = length(unique(nm)))
}

# ---- public entry point ------------------------------------------------------
# Reclassifies TaggedPitchType for every NON-COASTAL pitcher in `df` with
# enough pitches, per pitcher-season. Coastal arms (custom-tagged TrackMan)
# pass through untouched. Adds OriginalPitchType / PitchTypeReclassified /
# PitchFamilyChanged / SpinErrorFlag. Safe on any frame: silently returns the
# input when required columns are missing.
reclassify_noncoastal_pitches <- function(df, context = "") {
  need <- c("Pitcher", "TaggedPitchType", "RelSpeed",
            "InducedVertBreak", "HorzBreak", "SpinRate")
  if (!is.data.frame(df) || nrow(df) == 0 || !all(need %in% names(df))) return(df)
  if (isTRUE(attr(df, "reclassified"))) return(df)

  if (!"OriginalPitchType" %in% names(df))
    df$OriginalPitchType <- as.character(df$TaggedPitchType)
  if (!"PitchTypeReclassified" %in% names(df)) df$PitchTypeReclassified <- FALSE
  if (!"PitchFamilyChanged"    %in% names(df)) df$PitchFamilyChanged    <- FALSE
  if (!"SpinErrorFlag"         %in% names(df)) df$SpinErrorFlag         <- FALSE

  yr <- if ("Date" %in% names(df)) {
    y <- suppressWarnings(format(safe_as_date(df$Date), "%Y"))
    ifelse(is.na(y), "NA", y)
  } else rep("all", nrow(df))

  pitchers <- unique(as.character(df$Pitcher))
  pitchers <- pitchers[!is.na(pitchers) & nzchar(pitchers)]

  for (p in pitchers) {
    # Coastal arms are custom-tagged: never reclassified
    if (exists("coastal_pitcher_set") && p %in% coastal_pitcher_set) next
    if ("PitcherTeam" %in% names(df)) {
      tms <- unique(as.character(df$PitcherTeam[df$Pitcher == p]))
      if (any(tms %in% c("COA_CHA", "Coastal Carolina",
                         "Coastal Carolina University"), na.rm = TRUE)) next
    }
    for (season in unique(yr[df$Pitcher == p])) {
      idx <- which(df$Pitcher == p & yr == season)
      # rows from Supabase pitchprofiler.pitches arrive already classified by
      # the same engine (scripts/palace_pipeline/reclassify.py); keep them
      if ("PitchTypeSource" %in% names(df)) {
        src <- as.character(df$PitchTypeSource[idx])
        if (length(src) > 0 && all(!is.na(src) & nzchar(src))) next
      }
      velo <- suppressWarnings(as.numeric(df$RelSpeed[idx]))
      ivb  <- suppressWarnings(as.numeric(df$InducedVertBreak[idx]))
      hb   <- suppressWarnings(as.numeric(df$HorzBreak[idx]))
      spin <- suppressWarnings(as.numeric(df$SpinRate[idx]))
      usable <- is.finite(velo) & is.finite(ivb) & is.finite(hb)
      if (sum(usable) < RC_MIN_PITCHES) next        # keep the tag: too little data
      idx <- idx[usable]
      hand <- if ("PitcherThrows" %in% names(df))
        as.character(df$PitcherThrows[idx]) else rep("Right", length(idx))

      res <- tryCatch(
        .rc_classify_one(velo[usable], ivb[usable], hb[usable], spin[usable],
                         hand, as.character(df$TaggedPitchType[idx])),
        error = function(e) {
          cat("  [Reclass] ", p, " (", season, ") failed: ",
              conditionMessage(e), " — tags kept\n", sep = "")
          NULL
        })
      if (is.null(res)) next
      if (any(is.na(res$type))) {                      # never write NA tags
        cat("  [Reclass] ", p, " (", season, "): naming produced NA — tags kept\n",
            sep = ""); next
      }

      old <- .rc_canon_tag(df$TaggedPitchType[idx])
      new <- res$type
      changed <- !is.na(old) & nzchar(old) & old != "Other" &
        old != "Undefined" & old != new
      changed[is.na(changed)] <- FALSE
      fam_changed <- changed & !is.na(.rc_family(old)) &
        !is.na(.rc_family(new)) & .rc_family(old) != .rc_family(new)

      df$TaggedPitchType[idx]       <- new
      df$PitchTypeReclassified[idx] <- changed
      df$PitchFamilyChanged[idx]    <- fam_changed
      df$SpinErrorFlag[idx]         <- res$spin_flag

      cat(sprintf(
        "  [Reclass] %s%s %s (%s): %d pitches -> %d clusters -> %d pitch types | %.1f%% tags changed, %.1f%% family-level, %d spin flags\n",
        if (nzchar(context)) paste0("[", context, "] ") else "",
        p, "", season, length(idx), res$n_clusters, res$n_named,
        100 * mean(changed), 100 * mean(fam_changed), sum(res$spin_flag)))
    }
  }
  attr(df, "reclassified") <- TRUE
  df
}


# ============================================================================
