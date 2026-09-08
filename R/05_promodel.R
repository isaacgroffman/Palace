# PITCH PROFILER MODEL BUNDLE (proStuff+ / proPitching+ / proLocation+)
# ------------------------------------------------------------
# Replaces the old in-R xgboost recipes/models. Scoring runs in
# Python (LightGBM) via reticulate against the production bundle in
# ./models + ./pitchprofiler_models. See MODELS-IMPLEMENTATION-GUIDE.md.
#
# Deployment requirements (Dockerfile):
#   RUN pip3 install --no-cache-dir lightgbm==4.7.0 polars pandas numpy pyarrow
#   COPY pitchprofiler_models/ models/ sample/ verify_install.py -> image
#   RUN python3 verify_install.py     # build gate: image fails if env drifts
# ============================================================

.pp_env <- new.env(parent = emptyenv())

init_pitchprofiler <- function() {
  if (!is.null(.pp_env$bundle)) return(invisible(TRUE))
  ok <- tryCatch({
    .pp_env$mod    <- reticulate::import("pitchprofiler_models",
                                         delay_load = FALSE, convert = FALSE)
    .pp_env$pl     <- reticulate::import("polars",
                                         delay_load = FALSE, convert = FALSE)
    .pp_env$bundle <- .pp_env$mod$load_bundle("models")
    TRUE
  }, error = function(e) {
    cat("[proModel] INIT FAILED:", conditionMessage(e),
        "- all plus scores will be NA\n")
    FALSE
  })
  invisible(ok)
}

# ---- Fixed plus-scaling reference (guide §4.3) -----------------
# 100 = average college pitcher-season at the July 2026 production
# refit. Lower xRV is better, hence (mean - xrv) in the numerator.
# Tries production_constants.json first; falls back to the values
# printed in the implementation guide (identical by construction).
.pp_ref_fallback <- list(
  stuff    = c(m = -0.03754883905254666,  s = 0.008123982201152579),
  pitching = c(m =  0.0051336046728626475, s = 0.010454838367460088),
  location = c(m =  0.04268244372540931,  s = 0.010574526980210715)
)

.pp_load_reference <- function() {
  path <- "models/promodel/production_constants.json"
  out <- tryCatch({
    if (!file.exists(path) || !requireNamespace("jsonlite", quietly = TRUE)) stop("skip")
    j <- jsonlite::fromJSON(path, simplifyVector = TRUE)
    # recursive hunt for a node keyed by the xrv metric names carrying mean/sd
    find_node <- function(x, key) {
      if (is.list(x)) {
        if (!is.null(x[[key]])) return(x[[key]])
        for (el in x) { r <- find_node(el, key); if (!is.null(r)) return(r) }
      }
      NULL
    }
    pick <- function(node) {
      if (is.null(node)) return(NULL)
      nm <- names(node)
      m <- node[[grep("mean", nm, ignore.case = TRUE)[1]]]
      s <- node[[grep("^(sd|std)", nm, ignore.case = TRUE)[1]]]
      if (is.numeric(m) && is.numeric(s)) c(m = m, s = s) else NULL
    }
    ref <- list(stuff    = pick(find_node(j, "stuff_xrv")),
                pitching = pick(find_node(j, "pitching_xrv")),
                location = pick(find_node(j, "location_xrv")))
    if (any(vapply(ref, is.null, logical(1)))) stop("shape mismatch")
    cat("[proModel] Reference constants read from production_constants.json\n")
    ref
  }, error = function(e) {
    cat("[proModel] Using guide-embedded reference constants\n")
    .pp_ref_fallback
  })
  # sanity: JSON must agree with the guide within float noise
  for (k in names(out)) {
    if (abs(out[[k]]["m"] - .pp_ref_fallback[[k]]["m"]) > 1e-6 ||
        abs(out[[k]]["s"] - .pp_ref_fallback[[k]]["s"]) > 1e-6) {
      cat("[proModel] WARNING: JSON reference for", k,
          "disagrees with guide values - using JSON (bundle may be a newer refit)\n")
    }
  }
  out
}

PP_REF <- .pp_load_reference()

xrv_to_plus <- function(xrv, ref) 100 + (ref["m"] - xrv) / ref["s"] * 10

# ---- Kinematic imputation for TruMedia frames ------------------
# The bundle solves zoneTime from vy0/ay0/y0 (guide §4.1). TrackMan
# CSVs carry them; the TruMedia API does not. vy0 and ay0 are almost
# fully determined by RelSpeed, so we fit imputers on our own
# TrackMan rows at startup and fill TruMedia rows from those fits.
# Physics-derived fallbacks cover the cold-start case.
.pp_kin <- new.env(parent = emptyenv())

fit_kinematic_imputers <- function(df) {
  if (!is.null(.pp_kin$vy0_fit)) return(invisible(TRUE))
  if (!all(c("vy0", "ay0", "RelSpeed") %in% names(df))) return(invisible(FALSE))
  d <- df[is.finite(df$vy0) & is.finite(df$ay0) & is.finite(df$RelSpeed),
          c("vy0", "ay0", "RelSpeed"), drop = FALSE]
  if (nrow(d) < 200) return(invisible(FALSE))
  if (nrow(d) > 50000) d <- d[sample.int(nrow(d), 50000), , drop = FALSE]
  .pp_kin$vy0_fit <- lm(vy0 ~ 0 + RelSpeed, data = d)
  .pp_kin$ay0_fit <- lm(ay0 ~ 0 + I(RelSpeed^2), data = d)
  cat(sprintf("[proModel] Kinematic imputers fit on %d TrackMan rows (vy0 R2=%.4f, ay0 R2=%.4f)\n",
              nrow(d),
              summary(.pp_kin$vy0_fit)$r.squared,
              summary(.pp_kin$ay0_fit)$r.squared))

  # Release-angle imputers for TruMedia payloads that omit VertRelAngle /
  # HorzRelAngle (a direct model feature; vra_diff drives sequencing).
  # Geometry determines them almost completely from release point, plate
  # location, movement and velo, so fit on the same TrackMan rows.
  ang_cols <- c("VertRelAngle", "HorzRelAngle", "RelHeight", "RelSide",
                "PlateLocHeight", "PlateLocSide", "RelSpeed",
                "InducedVertBreak", "HorzBreak", "Extension")
  if (all(ang_cols %in% names(df))) {
    a <- df[, ang_cols, drop = FALSE]
    # normalize plate loc to feet inside the imputer regardless of frame units
    med <- suppressWarnings(stats::median(abs(a$PlateLocHeight), na.rm = TRUE))
    if (is.finite(med) && med > 10) {
      a$PlateLocHeight <- a$PlateLocHeight / 12
      a$PlateLocSide   <- a$PlateLocSide / 12
    }
    a <- a[stats::complete.cases(a), , drop = FALSE]
    if (nrow(a) >= 200) {
      if (nrow(a) > 50000) a <- a[sample.int(nrow(a), 50000), , drop = FALSE]
      .pp_kin$vra_fit <- tryCatch(
        lm(VertRelAngle ~ RelHeight + PlateLocHeight + RelSpeed +
             InducedVertBreak + Extension, data = a),
        error = function(e) NULL)
      .pp_kin$hra_fit <- tryCatch(
        lm(HorzRelAngle ~ RelSide + PlateLocSide + RelSpeed +
             HorzBreak + Extension, data = a),
        error = function(e) NULL)
      if (!is.null(.pp_kin$vra_fit) && !is.null(.pp_kin$hra_fit)) {
        cat(sprintf("[proModel] Release-angle imputers fit on %d rows (VRA R2=%.4f, HRA R2=%.4f)\n",
                    nrow(a),
                    summary(.pp_kin$vra_fit)$r.squared,
                    summary(.pp_kin$hra_fit)$r.squared))
      }
    }
  }

  # VAA-augmented angle imputers. Approach angle is MEASURED per pitch
  # and is the other end of the same trajectory as the release angle, so
  # adding it recovers VRA/HRA nearly exactly AND restores real
  # pitch-to-pitch variation (which the geometry-only fit smooths away,
  # flattening vra_diff -- the release-consistency signal proPitching+
  # reads for sequencing). Preferred at predict time when VAA is present.
  vaa_cols <- c(ang_cols, "VertApprAngle", "HorzApprAngle")
  if (all(vaa_cols %in% names(df))) {
    a2 <- df[, vaa_cols, drop = FALSE]
    med <- suppressWarnings(stats::median(abs(a2$PlateLocHeight), na.rm = TRUE))
    if (is.finite(med) && med > 10) {
      a2$PlateLocHeight <- a2$PlateLocHeight / 12
      a2$PlateLocSide   <- a2$PlateLocSide / 12
    }
    a2 <- a2[stats::complete.cases(a2), , drop = FALSE]
    if (nrow(a2) >= 200) {
      if (nrow(a2) > 50000) a2 <- a2[sample.int(nrow(a2), 50000), , drop = FALSE]
      .pp_kin$vra_fit_vaa <- tryCatch(
        lm(VertRelAngle ~ VertApprAngle + RelHeight + PlateLocHeight +
             RelSpeed + InducedVertBreak + Extension, data = a2),
        error = function(e) NULL)
      .pp_kin$hra_fit_vaa <- tryCatch(
        lm(HorzRelAngle ~ HorzApprAngle + RelSide + PlateLocSide +
             RelSpeed + HorzBreak + Extension, data = a2),
        error = function(e) NULL)
      if (!is.null(.pp_kin$vra_fit_vaa) && !is.null(.pp_kin$hra_fit_vaa)) {
        cat(sprintf("[proModel] VAA-augmented angle imputers fit on %d rows (VRA R2=%.5f resid SD=%.3f deg, HRA R2=%.5f resid SD=%.3f deg)\n",
                    nrow(a2),
                    summary(.pp_kin$vra_fit_vaa)$r.squared,
                    stats::sd(stats::resid(.pp_kin$vra_fit_vaa)),
                    summary(.pp_kin$hra_fit_vaa)$r.squared,
                    stats::sd(stats::resid(.pp_kin$hra_fit_vaa))))
      }
    }
  }
  invisible(TRUE)
}

impute_kinematics <- function(df) {
  if (!"RelSpeed" %in% names(df)) return(df)
  for (cc in c("vy0", "ay0", "y0")) if (!cc %in% names(df)) df[[cc]] <- NA_real_
  need_vy0 <- !is.finite(df$vy0) & is.finite(df$RelSpeed)
  need_ay0 <- !is.finite(df$ay0) & is.finite(df$RelSpeed)
  if (any(need_vy0)) {
    df$vy0[need_vy0] <- if (!is.null(.pp_kin$vy0_fit)) {
      predict(.pp_kin$vy0_fit, newdata = df[need_vy0, , drop = FALSE])
    } else -1.44 * df$RelSpeed[need_vy0]                 # ~ -1.467 mph->fps * 98%
  }
  if (any(need_ay0)) {
    df$ay0[need_ay0] <- if (!is.null(.pp_kin$ay0_fit)) {
      predict(.pp_kin$ay0_fit, newdata = df[need_ay0, , drop = FALSE])
    } else 0.003395 * df$RelSpeed[need_ay0]^2            # drag ~ v^2 (27.5 fps^2 @ 90)
  }
  df$y0[!is.finite(df$y0)] <- 50
  if (any(need_vy0) || any(need_ay0)) {
    cat(sprintf("  [proModel] Imputed kinematics for %d rows (%s)\n",
                sum(need_vy0 | need_ay0),
                if (!is.null(.pp_kin$vy0_fit)) "fitted" else "physics fallback"))
  }

  # Release angles: fill only when absent. Tier 1 uses the VAA-augmented
  # fit on rows with measured approach angles (near-exact, keeps real
  # pitch-to-pitch variation for vra_diff); tier 2 falls back to the
  # geometry-only fit.
  for (cc in c("VertRelAngle", "HorzRelAngle")) if (!cc %in% names(df)) df[[cc]] <- NA_real_
  ang_pred <- c("RelHeight", "RelSide", "PlateLocHeight", "PlateLocSide",
                "RelSpeed", "InducedVertBreak", "HorzBreak", "Extension")
  if (!is.null(.pp_kin$vra_fit) && all(ang_pred %in% names(df))) {
    p <- df[, ang_pred, drop = FALSE]
    med <- suppressWarnings(stats::median(abs(p$PlateLocHeight), na.rm = TRUE))
    if (is.finite(med) && med > 10) {
      p$PlateLocHeight <- p$PlateLocHeight / 12
      p$PlateLocSide   <- p$PlateLocSide / 12
    }
    p$VertApprAngle <- if ("VertApprAngle" %in% names(df))
      suppressWarnings(as.numeric(df$VertApprAngle)) else NA_real_
    p$HorzApprAngle <- if ("HorzApprAngle" %in% names(df))
      suppressWarnings(as.numeric(df$HorzApprAngle)) else NA_real_

    geo_ok  <- stats::complete.cases(p[, ang_pred, drop = FALSE])
    vaa_ok  <- geo_ok & is.finite(p$VertApprAngle) & is.finite(p$HorzApprAngle) &
      !is.null(.pp_kin$vra_fit_vaa)

    need_vra <- !is.finite(df$VertRelAngle)
    need_hra <- !is.finite(df$HorzRelAngle)

    t1v <- need_vra & vaa_ok
    t1h <- need_hra & vaa_ok
    if (any(t1v)) df$VertRelAngle[t1v] <- predict(.pp_kin$vra_fit_vaa, newdata = p[t1v, , drop = FALSE])
    if (any(t1h)) df$HorzRelAngle[t1h] <- predict(.pp_kin$hra_fit_vaa, newdata = p[t1h, , drop = FALSE])

    t2v <- need_vra & !vaa_ok & geo_ok
    t2h <- need_hra & !vaa_ok & geo_ok
    if (any(t2v)) df$VertRelAngle[t2v] <- predict(.pp_kin$vra_fit, newdata = p[t2v, , drop = FALSE])
    if (any(t2h)) df$HorzRelAngle[t2h] <- predict(.pp_kin$hra_fit, newdata = p[t2h, , drop = FALSE])

    n1 <- sum(t1v | t1h); n2 <- sum(t2v | t2h)
    if (n1 + n2 > 0) {
      cat(sprintf("  [proModel] Imputed release angles: %d via VAA, %d via geometry\n",
                  n1, n2))
    }
  }
  df
}

# ---- The scorer ------------------------------------------------
# Attaches per-pitch stuff_xrv / pitching_xrv / location_xrv plus the
# fixed-reference stuff_plus / pitching_plus / location_plus columns.
# MUST be called on the RAW-convention frame: full pitch stream (no
# filtering - sequencing features need every pitch), RelSide unflipped,
# plate locations in native units (the bundle auto-detects feet/inches).
# Join-back is on the five-column key, never positional (guide §6).
.pp_required_cols <- c(
  "GameID", "Inning", "BatterTeam", "PAofInning", "PitchofPA",
  "Pitcher", "Batter", "PitcherThrows", "BatterSide", "TaggedPitchType",
  "RelSpeed", "SpinRate", "RelHeight", "RelSide", "Extension",
  "InducedVertBreak", "HorzBreak", "SpinAxis",
  "PlateLocSide", "PlateLocHeight", "VertRelAngle", "HorzRelAngle",
  "Balls", "Strikes", "Outs", "PitchCall", "vy0", "ay0", "y0"
)

.pp_na_fill <- function(df) {
  for (cc in c("stuff_xrv", "pitching_xrv", "location_xrv",
               "stuff_plus", "pitching_plus", "location_plus")) {
    if (!cc %in% names(df)) df[[cc]] <- NA_real_
  }
  df
}

.pp_key_cols <- c("GameID", "Inning", "BatterTeam", "PAofInning", "PitchofPA")

# Build the five-part join key with type-drift protection: numeric parts
# are normalized through as.numeric so 3L, 3.0 and "3.0" all key as "3";
# text parts are trimmed. A missing column keys as "<missing:col>" so a
# schema mismatch produces visibly unmatchable keys instead of paste()
# silently dropping the NULL and shortening the key.
.pp_key_of <- function(d) {
  n <- nrow(d)
  parts <- lapply(.pp_key_cols, function(cc) {
    x <- d[[cc]]
    if (is.null(x)) return(rep(paste0("<missing:", cc, ">"), n))
    if (cc %in% c("Inning", "PAofInning", "PitchofPA")) {
      v <- suppressWarnings(as.numeric(as.character(x)))
      ifelse(is.na(v), trimws(as.character(x)), as.character(v))
    } else {
      trimws(as.character(x))
    }
  })
  do.call(paste, c(parts, sep = "|"))
}

# The scored frame's key columns may come back from the Python side under
# snake_case or lowercase names; map any recognized variant back to the
# TrackMan names before keying.
.pp_resolve_keys <- function(scored) {
  alts <- list(
    GameID     = c("GameID", "game_id", "GameId", "gameid", "gameID"),
    Inning     = c("Inning", "inning"),
    BatterTeam = c("BatterTeam", "batter_team", "batterteam"),
    PAofInning = c("PAofInning", "pa_of_inning", "paofinning", "PaofInning"),
    PitchofPA  = c("PitchofPA", "pitch_of_pa", "pitchofpa", "PitchofPa")
  )
  for (k in names(alts)) {
    if (k %in% names(scored)) next
    hit <- intersect(alts[[k]], names(scored))
    if (length(hit) > 0) names(scored)[match(hit[1], names(scored))] <- k
  }
  scored
}

# Python -> R data.frame via a parquet round-trip, mirroring the R -> Python
# handoff. This sidesteps reticulate/pandas conversion entirely: pandas 3.x
# Arrow-backed string columns do NOT convert cleanly to R character vectors
# (they collapse into whole-array reprs, which silently corrupted the join
# keys). polars$write_parquet -> arrow::read_parquet is exact.
.pp_py_to_r_df <- function(x) {
  if (is.null(x)) stop("expected a data frame, got NULL")
  if (is.data.frame(x)) return(as.data.frame(x))
  if (inherits(x, "python.builtin.object")) {
    tmp <- tempfile(fileext = ".parquet")
    on.exit(unlink(tmp), add = TRUE)
    # polars DataFrames expose write_parquet; pandas exposes to_parquet.
    # Never fall back to reticulate::py_to_r for frames: pandas>=3 Arrow-
    # backed string columns corrupt into whole-array reprs on that path.
    path <- "polars"
    ok <- tryCatch({ x$write_parquet(tmp); TRUE }, error = function(e) FALSE)
    if (!ok) {
      path <- "pandas"
      ok <- tryCatch({ x$to_parquet(tmp, index = FALSE); TRUE },
                     error = function(e) FALSE)
    }
    if (!ok) stop("object is neither polars (write_parquet) nor pandas (to_parquet); ",
                  "class: ", paste(class(x), collapse = "/"))
    if (is.null(.pp_env$conv_logged)) {
      cat("[proModel] Python->R conversion via", path, "parquet round-trip\n")
      .pp_env$conv_logged <- TRUE
    }
    return(as.data.frame(arrow::read_parquet(tmp)))
  }
  as.data.frame(x)
}

# Datasets that receive proModel scoring. Coastal custom-tagged TrackMan
# sets and TruMedia-sourced NCAA loads are graded; the P5/SBC frames are
# legacy percentile pools kept for display only -- scoring ~520k rows of
# them at every boot is wasted time. Remove labels here to skip more.
PP_SCORE_LABELS <- c("data_ind", "fall_ind", "prespring_ind",
                     "spring26_ind", "ncaa_ind",
                     "mm_pool")   # matchup-matrix team pools (on-demand, cached)

score_promodel <- function(df, label = "") {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(.pp_na_fill(df))
  if (nzchar(label) && !label %in% PP_SCORE_LABELS) {
    cat("  [proModel]", label, "skipped (legacy pool, not graded)\n")
    return(.pp_na_fill(df))
  }
  if (!init_pitchprofiler()) return(.pp_na_fill(df))

  out <- tryCatch({
    fit_kinematic_imputers(df)          # learns from TrackMan rows, no-op after first fit
    send <- impute_kinematics(df)

    missing_req <- setdiff(.pp_required_cols, names(send))
    if (length(missing_req) > 0) {
      cat("  [proModel]", label, "missing columns (NA-filled, rows may be invalid):",
          paste(missing_req, collapse = ", "), "\n")
      for (cc in missing_req) send[[cc]] <- NA_real_
    }

    if (!"Stadium" %in% names(send)) send$Stadium <- NA_character_
    keep <- intersect(c(.pp_required_cols, "Stadium", "Date"), names(send))
    send <- send[, keep, drop = FALSE]

    tmp <- tempfile(fileext = ".parquet")
    on.exit(unlink(tmp), add = TRUE)
    arrow::write_parquet(send, tmp)
    raw_pl <- .pp_env$pl$read_parquet(tmp)

    res <- .pp_env$mod$run_all(raw_pl, .pp_env$bundle)
    sc  <- tryCatch(res[["scored"]], error = function(e) NULL)
    if (is.null(sc)) stop("run_all result has no 'scored' element")
    scored <- .pp_py_to_r_df(sc)

    xrv_cols <- c("stuff_xrv", "pitching_xrv", "location_xrv")
    if (!all(xrv_cols %in% names(scored))) {
      stop("scored frame missing xrv columns; got: ",
           paste(names(scored), collapse = ", "))
    }

    scored <- .pp_resolve_keys(scored)
    scored$.pp_key <- .pp_key_of(scored)
    n_finite_src <- sum(is.finite(scored$stuff_xrv))
    scored <- scored[!duplicated(scored$.pp_key),
                     c(".pp_key", xrv_cols), drop = FALSE]

    df$.pp_key <- .pp_key_of(df)
    n_match <- sum(df$.pp_key %in% scored$.pp_key)

    if (n_match == 0 && nrow(scored) > 0) {
      # Zero-overlap join: print everything needed to see why.
      cat("  [proModel] JOIN DIAGNOSTIC (0 matches):\n")
      cat("    scored:", nrow(scored), "rows,", n_finite_src, "finite stuff_xrv\n")
      cat("    scored cols:", paste(utils::head(names(scored), 45), collapse = ", "), "\n")
      cat("    df key ex:    ", paste(substr(utils::head(df$.pp_key, 2), 1, 120), collapse = "  ||  "), "\n")
      cat("    scored key ex:", paste(substr(utils::head(scored$.pp_key, 2), 1, 120), collapse = "  ||  "), "\n")
    }

    for (cc in c(xrv_cols, "stuff_plus", "pitching_plus", "location_plus")) {
      df[[cc]] <- NULL   # drop stale copies before the join
    }
    df <- dplyr::left_join(df, scored, by = ".pp_key")
    df$.pp_key <- NULL

    df$stuff_plus    <- xrv_to_plus(df$stuff_xrv,    PP_REF$stuff)
    df$pitching_plus <- xrv_to_plus(df$pitching_xrv, PP_REF$pitching)
    df$location_plus <- xrv_to_plus(df$location_xrv, PP_REF$location)

    cat(sprintf("  [proModel] %s scored %d/%d pitches (matched %d; scored frame had %d finite xrv)\n",
                label, sum(is.finite(df$stuff_xrv)), nrow(df), n_match, n_finite_src))
    df
  }, error = function(e) {
    cat("  [proModel] WARNING:", label, "scoring failed (",
        conditionMessage(e), ") - plus scores set to NA\n")
    .pp_na_fill(df)
  })
  out
}

# Expected stats (xBA / xSLG / xwOBA) off the RAW frame (guide §6:
# the cleaned frame drops the outcome columns these need). Returns a
# data.frame grouped as requested, or NULL on failure.
promodel_expected_stats <- function(df, group_cols = c("Pitcher")) {
  if (is.null(df) || nrow(df) == 0 || !init_pitchprofiler()) return(NULL)
  tryCatch({
    tmp <- tempfile(fileext = ".parquet")
    on.exit(unlink(tmp), add = TRUE)
    keep <- intersect(c(.pp_required_cols, "PlayResult", "ExitSpeed", "Angle",
                        "KorBB", "TaggedHitType", "Stadium", "Date"), names(df))
    arrow::write_parquet(df[, keep, drop = FALSE], tmp)
    raw_pl <- .pp_env$pl$read_parquet(tmp)
    xs <- .pp_env$mod$expected_stats(raw_pl, .pp_env$bundle,
                                     group_cols = do.call(reticulate::tuple, as.list(group_cols)))
    .pp_py_to_r_df(xs)
  }, error = function(e) {
    cat("[proModel] expected_stats failed:", conditionMessage(e), "\n")
    NULL
  })
}

# ---- Cached expected stats + column resolution -----------------
# Every xstats display re-fires on filter ticks, and each call is a
# Python round trip, so results are memoised on a cheap frame
# fingerprint (n rows, date span, in-play mass) rather than a full
# digest. The resolver maps whatever casing the bundle returns
# (xBA / xba / xwOBA...) onto canonical display names, and never
# guesses: a stat the bundle didn't return stays absent.
.pp_xs_cache <- new.env(parent = emptyenv())

.pp_xs_resolve <- function(xs) {
  if (is.null(xs) || !is.data.frame(xs) || nrow(xs) == 0) return(NULL)
  canon <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  nm <- names(xs)
  pick <- function(want) {
    hit <- nm[canon(nm) == want]
    if (length(hit) > 0) hit[1] else NA_character_
  }
  cols <- c(xBA = pick("xba"), xSLG = pick("xslg"), xwOBA = pick("xwoba"))
  cols <- cols[!is.na(cols)]
  if (length(cols) == 0) {
    cat("[proModel] xstats frame had no recognizable columns:",
        paste(utils::head(nm, 20), collapse = ", "), "\n")
    return(NULL)
  }
  keep_grp <- intersect(c("Pitcher", "TaggedPitchType", "Batter"), nm)
  out <- xs[, c(keep_grp, unname(cols)), drop = FALSE]
  names(out) <- c(keep_grp, names(cols))
  for (cc in names(cols)) out[[cc]] <- round(suppressWarnings(as.numeric(out[[cc]])), 3)
  out
}

pp_expected_stats_cached <- function(df, group_cols = c("Pitcher")) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  dr <- if ("Date" %in% names(df))
    paste(range(as.character(df$Date), na.rm = TRUE), collapse = "|") else ""
  key <- paste(
    nrow(df), paste(group_cols, collapse = ","), dr,
    length(unique(df$Pitcher)),
    if ("TaggedPitchType" %in% names(df)) length(unique(df$TaggedPitchType)) else 0,
    sum(df$PitchCall == "InPlay", na.rm = TRUE),
    round(sum(suppressWarnings(as.numeric(df$ExitSpeed)), na.rm = TRUE), 1),
    sep = "\u241F")
  hit <- .pp_xs_cache[[key]]
  if (!is.null(hit)) return(hit)
  out <- .pp_xs_resolve(promodel_expected_stats(df, group_cols = group_cols))
  if (!is.null(out) && is.null(.pp_xs_cache$.logged)) {
    cat("[proModel] Expected stats live:",
        paste(intersect(c("xBA", "xSLG", "xwOBA"), names(out)), collapse = "/"),
        "attached to summaries\n")
    .pp_xs_cache$.logged <- TRUE
  }
  # cache misses too (as NULL sentinel via list wrapper would complicate
  # callers; a failed python call just retries next tick, which is fine)
  if (!is.null(out)) .pp_xs_cache[[key]] <- out
  out
}

to_inches <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  s <- trimws(tolower(as.character(x)))
  s <- gsub('[\"`]', "", s)
  s <- gsub("ft", "'", s); s <- gsub("in", "", s)
  m <- regmatches(s, regexec("^\\s*(\\d+)\\s*[-' ]\\s*(\\d+)\\s*$", s))
  out <- rep(NA_real_, length(s))
  for (i in seq_along(m)) {
    if (length(m[[i]]) == 3) {
      out[i] <- as.numeric(m[[i]][2]) * 12 + as.numeric(m[[i]][3])
    } else if (grepl("^\\s*\\d+\\s*$", s[i])) {
      out[i] <- as.numeric(s[i])
    }
  }
  out
}

# Create bio_heights lookup
bio_heights <- bio %>%
  transmute(
    Pitcher_FL = gsub("^(.*),\\s*(.*)$", "\\2 \\1", Pitcher),
    pitcher_height_inches = dplyr::coalesce(
      suppressWarnings(as.numeric(Height)),
      to_inches(Height)
    )
  )

normalize_lastfirst <- function(x) {
  stringr::str_replace(x, "^\\s*([^,]+),\\s*(.+)\\s*$", "\\2 \\1")
}

parse_date_fallback <- function(x, alt = NULL) {
  d <- suppressWarnings(as.Date(x))
  na_rate <- mean(is.na(d))
  if (!is.null(alt)) {
    alt <- suppressWarnings(lubridate::ymd(alt))
  }
  if (isTRUE(na_rate > 0.6)) {
    try1 <- suppressWarnings(
      lubridate::parse_date_time(
        as.character(x),
        orders = c("Y-m-d","m/d/y","m/d/Y","Y/m/d")
      )
    )
    try1 <- as.Date(try1)
    if (mean(is.na(try1)) < na_rate) d <- try1
    if (!is.null(alt) && mean(is.na(d)) > 0.6) d <- as.Date(alt)
  }
  
  return(d)
}


dedupe_safely_by_uid <- function(df) {
  if ("PitchUID" %in% names(df) && any(!is.na(df$PitchUID))) {
    has_uid <- df %>% dplyr::filter(!is.na(PitchUID))
    no_uid  <- df %>% dplyr::filter(is.na(PitchUID))
    
    has_uid <- has_uid %>%
      dplyr::arrange(Date, GameID, PitchNo, PitchofPA) %>%
      dplyr::distinct(PitchUID, .keep_all = TRUE)
    
    if (nrow(no_uid) > 0) {
      no_uid <- no_uid %>%
        dplyr::arrange(Date, GameID, PitchNo, PitchofPA) %>%
        dplyr::distinct(
          GameID, PitcherId, Inning, PAofInning, PitchofPA, PitchNo,
          .keep_all = TRUE
        )
    }
    
    dplyr::bind_rows(has_uid, no_uid)
  } else {
    df %>%
      dplyr::arrange(Date, GameID, PitchNo, PitchofPA) %>%
      dplyr::distinct(
        GameID, PitcherId, Inning, PAofInning, PitchofPA, PitchNo,
        .keep_all = TRUE
      )
  }
}

pitch_colors <- c(
  "Fastball" = "#3465cb",
  "Four-Seam" = "#3465cb",
  "FourSeamFastBall" = "#3465cb",
  "4-Seam Fastball" = "#3465cb",
  "FF" = "#3465cb",
  "Sinker" = "#e5e501",
  "TwoSeamFastBall" = "#e5e501",
  "Two-Seam" = "#e5e501",
  "2-Seam Fastball" = "#e5e501",
  "SI" = "#e5e501",
  "Slider" = "#65aa02",
  "SL" = "#65aa02",
  "Sweeper" = "#dc4476",
  "SW" = "#dc4476",
  "Curveball" = "#d73813",
  "CB" = "#d73813",
  "Knuckle Curve" = "#d73813",
  "KC" = "#d73813",
  "ChangeUp" = "#980099",
  "Changeup" = "#980099",
  "CH" = "#980099",
  "Splitter" = "#23a999",
  "FS" = "#23a999",
  "SP" = "#23a999",
  "Cutter" = "#ff9903",
  "FC" = "#ff9903",
  "Slurve" = "#9370DB",
  "Other" = "gray50"
)
