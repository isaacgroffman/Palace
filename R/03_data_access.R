# ============================================================
# DATA ACCESS — the only place that knows where data lives.
#
# Everything is Supabase (R/palace_supabase.R) or a small file in this repo:
#   pitchprofiler.pitches + aggregates   every 2026 NCAA pitch, already
#                                        reclassified + scored by the Pitch
#                                        Profiler bundle (scripts/process_master.py)
#   public.pitches                       Coastal bullpens (Fall 2026 pill)
#   reference/*.csv, DRS26.csv, ...      lookups committed with the app
#
# Nothing is downloaded from anywhere else at boot.
# ============================================================

# Coastal + P5 + SBC pitch database. Built once on first use (Matchup
# Matrix, pitch arsenal, hitter expectation tables) and memoised.
.pool_all_env <- new.env(parent = emptyenv())
pool_all <- function() {
  if (!is.null(.pool_all_env$df)) return(.pool_all_env$df)
  cat("[pools] binding full_data_p5_compare (first use)\n")
  .pool_all_env$df <- dplyr::bind_rows(
    harmonize_types(list(spring26, P5_2026, SBC_2026)))
  .pool_all_env$df
}

# Local-only lookup for a bio/lookup CSV: repo root, reference/, data/, www/
# and the deployment host's /data and /code folders.
palace_file_candidates <- function(filename) {
  Filter(file.exists, unique(c(
    filename, file.path("reference", filename), file.path("data", filename),
    file.path("www", filename), file.path("/data", filename), file.path("/code", filename))))
}
