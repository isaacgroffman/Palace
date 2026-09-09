#!/usr/bin/env Rscript
# =============================================================================
# Build the processed P5 / Sun Belt reference pools and upload them to
# Supabase Storage (bucket palace-serving, ref/<build stamp>/<part>.parquet).
#
#   Rscript scripts/build_reference_pools.R           # both pools
#   Rscript scripts/build_reference_pools.R P5        # any of COASTAL / SBC / P5
#
# Run from the repo root after every scripts/upload_supabase.R reload. Needs
# the app's env (SB_DB_*, SUPABASE_URL, SUPABASE_SECRET_KEY). Pulls the pools
# from pitchprofiler.pitches (slow through the pooler, ~15-20 min for P5),
# runs process_pitcher_data() on them exactly as the app would, and uploads
# one parquet per league so the app downloads them in seconds instead.
# =============================================================================
Sys.setenv(PALACE_PREWARM = "FALSE")
t_start <- Sys.time()
source("app.R")   # boots the app's globals; shinyApp() only returns an object here

stamp <- gsub("[^0-9]", "", pp_built_at() %||% "")
if (!nzchar(stamp)) stop("no build stamp: is pitchprofiler.pitcher_season loaded?")
if (!sb_storage_enabled()) stop("set SUPABASE_URL and SUPABASE_SECRET_KEY")
cat("[ref build] build stamp", stamp, "\n")

want <- commandArgs(trailingOnly = TRUE)
if (length(want) == 0) want <- c("COASTAL", "SBC", "P5")

up <- function(df, part) {
  tmp <- tempfile(fileext = ".parquet")
  arrow::write_parquet(df, tmp, compression = "zstd")
  path <- sprintf("ref/%s/%s.parquet", stamp, part)
  sb_storage_upload(tmp, path)
  cat(sprintf("[ref build] uploaded %s: %d rows, %.1f MB\n", path, nrow(df), file.size(tmp) / 1e6))
  unlink(tmp)
}

if ("COASTAL" %in% want) {
  # raw (unprocessed) Coastal games: the app processes them at boot in seconds
  cg <- pp_coastal_games()
  if (is.null(cg) || nrow(cg) == 0) stop("Coastal games pull came back empty - nothing uploaded")
  up(cg, "coastal_games")
}

if ("SBC" %in% want) {
  sbc <- .ref_processed("SBC", SBC_LEAGUES, "SBC_ind", force = TRUE)
  if (nrow(sbc) == 0) stop("SBC pool came back empty - nothing uploaded")
  up(sbc, "SBC")
}

if ("P5" %in% want) {
  p5 <- .ref_processed("P5", P5_LEAGUES, "P5_ind", force = TRUE)
  if (nrow(p5) == 0) stop("P5 pool came back empty - nothing uploaded")
  parts <- c("NCAA SEC" = "P5_SEC", "NCAA ACC" = "P5_ACC", "NCAA Big 12" = "P5_Big12", "NCAA Big Ten" = "P5_BigTen")
  counts <- table(p5$League)
  cat("[ref build] P5 pitches by league:", paste(names(counts), counts, collapse = " | "), "\n")
  if (any(!names(parts) %in% names(counts))) stop("a P5 league is missing from the pool - nothing uploaded")
  for (lg in names(parts)) up(p5[p5$League == lg, , drop = FALSE], parts[[lg]])
}

cat("[ref build] done in", round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), "min\n")
