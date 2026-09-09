#!/usr/bin/env Rscript
# =============================================================================
# Build the processed P5 / Sun Belt reference pools and upload them to
# Supabase Storage (bucket palace-serving, ref/<build stamp>/<part>.parquet).
#
#   Rscript scripts/build_reference_pools.R           # both pools
#   Rscript scripts/build_reference_pools.R P5        # any of COASTAL / SBC / P5 / artifacts
#
# Run from the repo root after every scripts/upload_supabase.R reload. Needs
# the app's env (SB_DB_*, SUPABASE_URL, SUPABASE_SECRET_KEY). Pulls the pools
# from pitchprofiler.pitches (slow through the pooler, ~15-20 min for P5),
# runs process_pitcher_data() on them exactly as the app would, and uploads
# one parquet per league so the app downloads them in seconds instead.
# =============================================================================
#   Rscript scripts/build_reference_pools.R artifacts --stamp 20260908130506
#       -> percentile maps, movement grids and slim grading pools from the pools
#          already in Storage. Needs no database: only SUPABASE_URL/SECRET_KEY.
Sys.setenv(PALACE_PREWARM = "FALSE")
t_start <- Sys.time()
args <- commandArgs(trailingOnly = TRUE)
stamp_arg <- if ("--stamp" %in% args) args[which(args == "--stamp") + 1] else NULL
want <- setdiff(args, c("--stamp", stamp_arg))
if (length(want) == 0) want <- c("COASTAL", "SBC", "P5", "artifacts")

if (!is.null(stamp_arg)) {
  # no app boot: just the function files the artifact builders need
  suppressPackageStartupMessages({
    library(dplyr); library(readr); library(tidyr); library(purrr); library(stringr)
    library(tibble); library(magrittr); library(lubridate); library(arrow); library(httr2)
    library(pool); library(RPostgres); library(DBI); library(ggplot2)
  })
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  source("R/palace_supabase.R"); source("R/palace_artifacts.R")
  source("R/01_reference.R"); source("R/06_pitch_processing.R")
  l02 <- readLines("R/02_trumedia_api.R"); i <- grep("^harmonize_types <- function", l02)
  eval(parse(text = l02[i:(i + which(l02[(i + 1):length(l02)] == "}")[1])]))
  build_p5_slot_grid <- function(p5, bin = 0.10) {
    p5 %>% filter(!is.na(TaggedPitchType), !is.na(RelHeight), !is.na(HorzBreak), !is.na(InducedVertBreak)) %>%
      mutate(RH_bin = round(RelHeight / bin) * bin) %>% group_by(TaggedPitchType, RH_bin) %>%
      summarise(slot_hb = median(HorzBreak, na.rm = TRUE), slot_ivb = median(InducedVertBreak, na.rm = TRUE), n = n(), .groups = "drop")
  }
  stamp <- gsub("[^0-9]", "", stamp_arg)
  if (any(want %in% c("COASTAL", "SBC", "P5"))) stop("--stamp mode builds artifacts only; pool rebuilds need the database (drop --stamp)")
} else {
  source("app.R")   # boots the app's globals; shinyApp() only returns an object here
  stamp <- gsub("[^0-9]", "", pp_built_at() %||% "")
}
if (!nzchar(stamp)) stop("no build stamp: is pitchprofiler.pitcher_season loaded?")
if (!sb_storage_enabled()) stop("set SUPABASE_URL and SUPABASE_SECRET_KEY")
cat("[ref build] build stamp", stamp, "\n")

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

if ("artifacts" %in% want) {
  # pools: whatever was just built in this run, else the Storage copies
  get_pool <- function(label) {
    if (exists(".ref_env") && !is.null(.ref_env[[paste0(label, "_proc")]])) return(.ref_env[[paste0(label, "_proc")]])
    pool <- storage_pool(label, stamp)
    if (is.null(pool)) stop(label, " pool is not complete in Storage for build ", stamp)
    pool
  }
  p5 <- get_pool("P5"); sbc <- get_pool("SBC")

  maps_full <- build_p5_ecdf_maps(p5)
  maps <- slim_ecdf_maps(maps_full)
  # self-check: slim vs full percentile on random probes, every function
  worst <- 0
  for (tb in c("mov", "res")) for (cn in names(maps[[tb]])) if (is.list(maps[[tb]][[cn]]) && !is.data.frame(maps[[tb]][[cn]]))
    for (i in seq_along(maps[[tb]][[cn]])) {
      f0 <- maps_full[[tb]][[cn]][[i]]; f1 <- maps[[tb]][[cn]][[i]]
      if (!inherits(f0, "ecdf")) next
      k <- stats::knots(f0); z <- stats::runif(300, min(k), max(k))
      worst <- max(worst, max(abs(f0(z) - f1(z))))
    }
  cat(sprintf("[ref build] slim ecdf maps: max |percentile error| = %.4f (%d mov x %d res rows)\n",
              worst, nrow(maps$mov), nrow(maps$res)))
  if (worst > 0.005) stop("slim ecdf approximation too coarse")
  artifact_upload(maps, "p5_maps", stamp, "rds")
  artifact_upload(build_expected_movement_grid(p5, bin_size = 0.10), "exp_movement_grid", stamp, "rds")
  artifact_upload(build_p5_slot_grid(p5, bin = 0.10), "p5_slot_grid", stamp, "rds")
  gp5 <- build_grade_pool(p5); gsbc <- build_grade_pool(sbc)
  cat(sprintf("[ref build] slim grading pools: P5 %d x %d (%.0f MB in R), SBC %d x %d (%.0f MB in R)\n",
              nrow(gp5), ncol(gp5), as.numeric(object.size(gp5)) / 1e6, nrow(gsbc), ncol(gsbc), as.numeric(object.size(gsbc)) / 1e6))
  artifact_upload(gp5, "grade_P5", stamp, "parquet")
  artifact_upload(gsbc, "grade_SBC", stamp, "parquet")
}

cat("[ref build] done in", round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), "min\n")
