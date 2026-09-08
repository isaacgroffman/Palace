#!/usr/bin/env Rscript
# =============================================================================
# Build the app-shaped serving data Palace reads in PALACE_DATA_MODE=serve.
#
#   Rscript scripts/build_serving_data.R                 # -> ./serving_build/
#   Rscript scripts/build_serving_data.R --upload        # ...and push to PALACE_SERVING_REPO
#   Rscript scripts/build_serving_data.R /some/dir       # custom output dir
#
# Run from the repo root. Needs the same env vars as the app (HF token,
# TruMedia, Supabase) plus the raw inputs the legacy boot path reads:
#   raw/fall_data_pitcher.parquet, raw/prespring_data_pitcher.parquet,
#   raw/3_1, 2_27, 2_28 - values_times_12_set2.csv, raw/Sat_Vs_ODU_Sheet1_.csv (optional)
#   DRS26.csv, trumedia_player_bio_master.csv, CCU_Hitter_Bio.csv, bioinfo.csv (root or data/)
#
# It runs the ORIGINAL startup pipeline (R/07_pools.R build branch) and
# writes what it produced, so the app can never disagree with the build.
# =============================================================================
args   <- commandArgs(trailingOnly = TRUE)
upload <- "--upload" %in% args
out    <- setdiff(args, "--upload")
out    <- if (length(out)) out[1] else "serving_build"

if (!file.exists("app.R")) stop("run this from the repo root (app.R not found in ", getwd(), ")")
Sys.setenv(PALACE_DATA_MODE = "build")
if (!nzchar(Sys.getenv("RETICULATE_PYTHON"))) Sys.setenv(RETICULATE_PYTHON = "/usr/bin/python3")

t_start <- Sys.time()
suppressPackageStartupMessages({
  library(shiny); library(DT); library(dplyr); library(scales); library(readr)
  library(tidyverse); library(bslib); library(htmltools); library(shinyWidgets)
  library(ggplot2); library(grid); library(magick); library(arrow); library(gt)
  library(gtExtras); library(lubridate); library(ggimage); library(plotly)
  library(purrr); library(tidyr); library(ggforce); library(httr); library(reticulate)
  library(openxlsx); library(pool); library(RPostgres); library(DBI); library(httr2)
})

source("R/00_config.R")
source("R/palace_supabase.R")
source("R/palace_video.R")
source("R/palace_bullpen.R")
# Everything up to the NCAA data layer is enough: pools (07) + ncaa_dir_pairs (08).
for (f in sort(list.files("R", pattern = "^0[1-8]_.*\\.R$", full.names = TRUE)))
  source(f)

cat("\n[build] pipeline finished in",
    round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), "min\n")

# ---- write ------------------------------------------------------------------
for (d in c("pools", "artifacts", "files")) dir.create(file.path(out, d), recursive = TRUE, showWarnings = FALSE)

write_pool <- function(df, name) {
  if (is.null(df) || !is.data.frame(df)) { cat("[build] skip", name, "(not a data frame)\n"); return(invisible(NULL)) }
  lc <- vapply(df, is.list, logical(1))
  if (any(lc)) { cat("[build]", name, ": dropping list columns", paste(names(df)[lc], collapse = ", "), "\n"); df <- df[!lc] }
  p <- file.path(out, "pools", paste0(name, ".parquet"))
  arrow::write_parquet(df, p, compression = "zstd")
  cat(sprintf("[build] pool %-18s %8d rows  %6.1f MB\n", name, nrow(df), file.size(p) / 1e6))
}
write_artifact <- function(obj, name) {
  p <- file.path(out, "artifacts", paste0(name, ".rds"))
  saveRDS(obj, p, compress = "xz")
  cat(sprintf("[build] artifact %-14s %6.1f MB\n", name, file.size(p) / 1e6))
}

write_pool(data,           "spring25")
write_pool(spraydata,      "spray_spring25")
write_pool(fall25,         "fall25")
write_pool(sprayfall25,    "spray_fall25")
write_pool(prespring,      "prespring26")
write_pool(sprayprespring, "spray_prespring26")
write_pool(spring26,       "spring26")
write_pool(sprayspring26,  "spray_spring26")
write_pool(P5_2025,        "P5_2025")
write_pool(SBC_2025,       "SBC_2025")

write_artifact(p5_maps,           "p5_maps")
write_artifact(exp_movement_grid, "exp_movement_grid")
write_artifact(p5_slot_grid,      "p5_slot_grid")
write_artifact(ncaa_dir_pairs,    "ncaa_dir_pairs")

# Bio / lookup CSVs that used to be committed to the app repo.
for (fn in c("DRS26.csv", "trumedia_player_bio_master.csv", "CCU_Hitter_Bio.csv", "bioinfo.csv")) {
  src <- Filter(file.exists, c(fn, file.path("data", fn), file.path("raw", fn)))
  if (length(src)) { file.copy(src[1], file.path(out, "files", fn), overwrite = TRUE); cat("[build] file", fn, "\n") }
  else cat("[build] file", fn, "NOT FOUND locally - the app will fall back to whatever it finds\n")
}

manifest <- list(
  built_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  rows = list(spring25 = nrow(data), fall25 = nrow(fall25), prespring26 = nrow(prespring),
              spring26 = nrow(spring26), P5_2025 = nrow(P5_2025), SBC_2025 = nrow(SBC_2025),
              ncaa_dir_pairs = nrow(ncaa_dir_pairs)),
  spring26_games = length(spring26_game_choices),
  manual_data_loaded = manual_data_loaded, odu_data_loaded = odu_data_loaded
)
writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE), file.path(out, "manifest.json"))
cat("\n[build] wrote", out, "\n")

# ---- upload -----------------------------------------------------------------
if (upload) {
  tok <- .hf_token()
  if (!nzchar(tok)) stop("--upload needs HF_TOKEN (write scope) in the environment")
  hf  <- reticulate::import("huggingface_hub")
  api <- hf$HfApi()
  api$create_repo(repo_id = PALACE_SERVING_REPO, repo_type = "dataset", private = TRUE, exist_ok = TRUE, token = tok)
  api$upload_folder(folder_path = out, repo_id = PALACE_SERVING_REPO, repo_type = "dataset", token = tok,
                    commit_message = paste("palace serving build", manifest$built_at))
  cat("[build] uploaded to", PALACE_SERVING_REPO, "\n")
} else {
  cat("[build] not uploaded (pass --upload). To run the app against this folder locally:\n",
      "   PALACE_DATA_MODE=serve PALACE_SERVING_DIR=", normalizePath(out), "\n", sep = "")
}
