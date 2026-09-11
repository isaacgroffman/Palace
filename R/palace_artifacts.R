# =============================================================================
# R/palace_artifacts.R — precomputed reference artifacts.
#
# The percentile maps, movement grids and the slim "grading pool" are computed
# once per scored-table build by scripts/build_reference_pools.R and stored in
# Supabase Storage (bucket palace-serving, ref/<stamp>/...). The app downloads
# them in seconds; the full 146-column reference pools (~750 MB in R) are only
# loaded for the features that need raw pitches (matchup matrix, hitter
# process model, pitch arsenal).
#
# Sourced by app.R before the numbered files and by the build script on its
# own (no app boot), so it only depends on palace_supabase.R + packages.
# =============================================================================

# ---- slim ecdf maps ---------------------------------------------------------
# stats::ecdf() closures keep every distinct value in their environment: the
# P5 maps serialize to ~180 MB. A step function over 1,001 quantile knots
# answers the same calls (f(z) -> percentile) to within 0.1% and is ~1 MB.
slim_ecdf <- function(f, n = 1001L) {
  if (!is.function(f)) return(f)
  if (!inherits(f, "ecdf")) {
    # build_p5_ecdf_maps()' fallbacks (constant 0.5, or pnorm((z-100)/10) for
    # the plus metrics) are closures over the builder's frame, i.e. the whole
    # reference pool. They reference nothing from it, so detach them.
    environment(f) <- new.env(parent = baseenv())
    return(f)
  }
  x <- stats::knots(f)
  if (length(x) <= n) return(f)
  y <- f(x)
  idx <- unique(pmin(pmax(findInterval(seq(0, 1, length.out = n), y) + 1L, 1L), length(x)))
  g <- stats::approxfun(x[idx], y[idx], method = "constant", f = 0,
                        yleft = 0, yright = 1, ties = "ordered")
  class(g) <- c("slim_ecdf", "function")
  g
}

slim_ecdf_maps <- function(maps) {
  slim_tbl <- function(tb) {
    for (cn in names(tb)) {
      if (!is.list(tb[[cn]]) || is.data.frame(tb[[cn]])) next
      if (grepl("^ecdf_", cn)) tb[[cn]] <- lapply(tb[[cn]], slim_ecdf)
      else tb[[cn]] <- NULL          # nested per-pitcher tibbles (`data`): never read
    }
    tb
  }
  for (nm in intersect(c("mov", "res"), names(maps))) maps[[nm]] <- slim_tbl(maps[[nm]])
  maps
}

# ---- slim grading pool ------------------------------------------------------
# Everything grade_pool_df() consumers read (summarise_grade_stats,
# pp_pct_per_arm, build_gauge_facets, the xwOBAcon block) and nothing else.
# ... plus what the Advance tab's league reference reads: the matchup
# feature scaler (shape), pitch-arsenal similarity + familiarity (pitcher x
# batter x pitch type over time) and the hitter-model fallback.
GRADE_POOL_COLS <- c(
  "Pitcher", "PitcherId", "PitcherTeam", "PitcherThrows", "TaggedPitchType", "League",
  "Batter", "BatterSide", "BatterTeam", "Date", "PitchUID",
  "GameID", "Inning", "PAofInning", "Balls", "Strikes", "KorBB", "PitchCall", "PlayResult",
  "TaggedHitType", "PlateLocSide", "PlateLocHeight",
  "RelSpeed", "InducedVertBreak", "HorzBreak", "SpinRate", "RelSide", "RelHeight",
  "Extension", "ExitSpeed", "VertApprAngle", "HorzApprAngle", "pitching_xrv",
  "stuff_plus", "pitching_plus", "location_plus",
  "StrikeIndicator", "StrikeZoneIndicator", "WalkIndicator", "PAindicator",
  "WhiffIndicator", "SwingIndicator", "Zwhiffind", "Zswing", "Chaseindicator",
  "OutofZone", "ABindicator", "HitIndicator", "totalbases", "GBindicator", "BIPind",
  "HHindicator", "biphh", "is_bbe", "xwobacon_bbe", "xba_bbe", "xtb_bbe"
)

build_grade_pool <- function(pool) {
  keep <- intersect(GRADE_POOL_COLS, names(pool))
  d <- pool[, keep, drop = FALSE]
  # 0/1 indicator columns as integers: half the memory of doubles
  for (cn in names(d)) {
    x <- d[[cn]]
    if (is.numeric(x) && !is.integer(x) && all(is.na(x) | x %in% c(0, 1))) d[[cn]] <- as.integer(x)
  }
  d
}

# ---- units --------------------------------------------------------------------
# Idempotent inches -> feet for plate locations (the app-wide convention).
pool_to_feet <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)
  if ("Date" %in% names(df)) df$Date <- as.Date(df$Date)
  if (all(c("PlateLocHeight", "PlateLocSide") %in% names(df))) {
    med <- suppressWarnings(stats::median(abs(df$PlateLocHeight), na.rm = TRUE))
    if (is.finite(med) && med > 8) {
      df$PlateLocHeight <- df$PlateLocHeight / 12
      df$PlateLocSide   <- df$PlateLocSide / 12
    }
  }
  df
}

# ---- Storage I/O ----------------------------------------------------------------
# Storage object names per pool (the P5 pool is shipped as one file per league)
REF_PARTS <- list(P5 = c("P5_SEC", "P5_ACC", "P5_Big12", "P5_BigTen"), SBC = "SBC")

# A processed reference pool from Storage (all parts), in feet. NULL if any
# part is missing, so a caller never works from a partial pool.
storage_pool <- function(label, stamp) {
  if (!nzchar(stamp %||% "") || !sb_storage_enabled()) return(NULL)
  t0 <- Sys.time(); parts <- list()
  for (part in REF_PARTS[[label]]) {
    dest <- tempfile(fileext = ".parquet"); t1 <- Sys.time()
    if (sb_storage_download(sprintf("ref/%s/%s.parquet", stamp, part), dest)) {
      dl <- as.numeric(difftime(Sys.time(), t1, units = "secs")); t1 <- Sys.time()
      parts[[part]] <- tryCatch(as.data.frame(arrow::read_parquet(dest)), error = function(e) NULL)
      cat(sprintf("[pools]   %s: %.1f MB downloaded in %.0fs, read in %.0fs\n", part,
                  file.size(dest) / 1e6, dl, as.numeric(difftime(Sys.time(), t1, units = "secs"))))
      unlink(dest)
    }
  }
  parts <- Filter(function(d) is.data.frame(d) && nrow(d) > 0, parts)
  if (length(parts) != length(REF_PARTS[[label]])) return(NULL)
  t1 <- Sys.time()
  out <- pool_to_feet(if (length(parts) == 1) parts[[1]] else dplyr::bind_rows(harmonize_types(parts)))
  rm(parts)
  cat(sprintf("[pools] %s reference pool: %d pitches from Storage in %.0fs\n", label, nrow(out),
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  out
}

# Small artifacts: <name>.rds (R objects) or <name>.parquet (frames).
artifact_download <- function(name, stamp, kind = c("rds", "parquet")) {
  kind <- match.arg(kind)
  if (!nzchar(stamp %||% "") || !sb_storage_enabled()) return(NULL)
  dest <- tempfile(fileext = paste0(".", kind)); t0 <- Sys.time()
  if (!sb_storage_download(sprintf("ref/%s/%s.%s", stamp, name, kind), dest)) return(NULL)
  obj <- tryCatch(if (kind == "rds") readRDS(dest) else as.data.frame(arrow::read_parquet(dest)),
                  error = function(e) NULL)
  sz <- file.size(dest); unlink(dest)
  if (!is.null(obj)) cat(sprintf("[artifacts] %s: %.1f MB from Storage in %.0fs\n", name, sz / 1e6,
                                 as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  obj
}

# Any parquet / json object in the bucket by path (lb/<season>/pitchers.parquet ...).
storage_read_parquet <- function(path) {
  if (!sb_storage_enabled()) return(NULL)
  dest <- tempfile(fileext = ".parquet")
  if (!sb_storage_download(path, dest)) return(NULL)
  obj <- tryCatch(as.data.frame(arrow::read_parquet(dest)), error = function(e) NULL)
  unlink(dest); obj
}
storage_read_json <- function(path) {
  if (!sb_storage_enabled()) return(NULL)
  dest <- tempfile(fileext = ".json")
  if (!sb_storage_download(path, dest)) return(NULL)
  obj <- tryCatch(jsonlite::fromJSON(dest), error = function(e) NULL)
  unlink(dest); obj
}

# A frame stored as <name>_<part>.parquet per league, bound on read.
artifact_download_parts <- function(name, parts, stamp) {
  out <- list()
  for (part in parts) {
    d <- artifact_download(paste0(name, "_", part), stamp, "parquet")
    if (is.null(d)) return(NULL)
    out[[part]] <- d
  }
  if (length(out) == 1) out[[1]] else dplyr::bind_rows(harmonize_types(out))
}
artifact_upload_parts <- function(df, name, by_col, part_of, stamp) {
  for (part in names(part_of)) {
    d <- df[df[[by_col]] %in% part_of[[part]], , drop = FALSE]
    if (!nrow(d)) stop("no rows for part ", part)
    artifact_upload(d, paste0(name, "_", part), stamp, "parquet")
  }
}
# league -> part name, shared by the builder and the app
GRADE_PARTS <- list(P5 = c(SEC = "NCAA SEC", ACC = "NCAA ACC", Big12 = "NCAA Big 12", BigTen = "NCAA Big Ten"),
                    SBC = c(SBC = "NCAA Sun Belt"))

artifact_upload <- function(obj, name, stamp, kind = c("rds", "parquet")) {
  kind <- match.arg(kind)
  tmp <- tempfile(fileext = paste0(".", kind))
  if (kind == "rds") saveRDS(obj, tmp, compress = "xz") else arrow::write_parquet(obj, tmp, compression = "zstd")
  path <- sprintf("ref/%s/%s.%s", stamp, name, kind)
  cat(sprintf("[ref build] uploading %s (%.1f MB)\n", path, file.size(tmp) / 1e6))
  sb_storage_upload(tmp, path)
  cat(sprintf("[ref build] uploaded %s: %.1f MB\n", path, file.size(tmp) / 1e6))
  unlink(tmp)
}
