# HF DOWNLOAD CACHE LAYER (load-time optimization)
# Every private HF file is cached on the persistent /data disk
# (falls back to /tmp locally). huggingface_hub handles etag
# revalidation, so files only re-download when they actually
# change on the Hub. Restarts skip gigabytes of transfer.
# ============================================================
hf_cache_dir <- function() {
  d <- if (dir.exists("/data")) "/data/hf_cache" else file.path(tempdir(), "hf_cache")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

.hf_token <- function() {
  tok <- Sys.getenv("HF_Token")
  if (!nzchar(tok)) tok <- Sys.getenv("HF_TOKEN")
  if (!nzchar(tok)) tok <- Sys.getenv("Advance_Reader")
  tok
}

# Returns a local path to the (cached) file. Prefers huggingface_hub's
# etag-validated cache; falls back to a simple httr download that reuses
# any previously cached copy if the network call fails.
hf_cached_file <- function(repo_id, filename, token = .hf_token()) {
  # 1) huggingface_hub (etag caching — re-downloads only on change)
  path <- tryCatch({
    hf <- reticulate::import("huggingface_hub")
    hf$hf_hub_download(repo_id = repo_id, filename = filename,
                       repo_type = "dataset", token = token,
                       cache_dir = hf_cache_dir())
  }, error = function(e) NULL)
  if (!is.null(path) && file.exists(path)) return(path)

  # 2) manual fallback with naive on-disk reuse
  dest <- file.path(hf_cache_dir(), gsub("[^A-Za-z0-9._-]", "_", paste0(repo_id, "__", filename)))
  resp <- tryCatch(
    httr::GET(paste0("https://huggingface.co/datasets/", repo_id, "/resolve/main/", filename),
              httr::add_headers(Authorization = paste("Bearer", token))),
    error = function(e) NULL)
  if (!is.null(resp) && httr::status_code(resp) == 200) {
    writeBin(httr::content(resp, "raw"), dest)
    return(dest)
  }
  if (file.exists(dest) && file.info(dest)$size > 0) {
    cat("WARNING: using stale cached copy of", filename, "\n")
    return(dest)
  }
  stop(paste("Failed to download", filename, "from", repo_id))
}

# ---- Cached overrides of the download helpers defined above ----
# Same signatures / same return values, but backed by the disk cache.
download_private_parquet <- function(repo_id, filename) {
  arrow::read_parquet(hf_cached_file(repo_id, filename))
}

download_private_csv <- function(repo_id, filename) {
  readr::read_csv(hf_cached_file(repo_id, filename), show_col_types = FALSE)
}

# Cached master-dataset loader: lists the folder, pulls each parquet
# through the cache (only new game files transfer), then binds with the
# same harmonize_types() + PitchUID dedupe as before.
download_master_dataset <- function(repo_id, folder, token = .hf_token()) {
  pq_files <- tryCatch({
    hf <- reticulate::import("huggingface_hub")
    api <- hf$HfApi()
    files <- api$list_repo_files(repo_id = repo_id, repo_type = "dataset", token = token)
    files[grepl(paste0("^", folder, "/.*\\.parquet$"), files)]
  }, error = function(e) {
    cat("WARNING: could not list", repo_id, folder, "-", conditionMessage(e), "\n")
    character(0)
  })

  if (length(pq_files) == 0) return(data.frame())

  all_data <- lapply(pq_files, function(f) {
    tryCatch(arrow::read_parquet(hf_cached_file(repo_id, f, token)),
             error = function(e) { cat("WARNING: skipping", f, "\n"); NULL })
  })

  combined <- bind_rows(harmonize_types(all_data))
  if ("PitchUID" %in% names(combined)) {
    combined <- combined %>% distinct(PitchUID, .keep_all = TRUE)
  }
  combined
}

# Download every parquet in a master-dataset folder to one local dir and
# return the dir path — used to open the folder as a LAZY Arrow dataset
# (nothing is loaded into RAM until a pitcher is actually selected).
download_master_dataset_dir <- function(repo_id, folder, token = .hf_token()) {
  local_dir <- file.path(hf_cache_dir(), gsub("[^A-Za-z0-9._-]", "_", paste0(repo_id, "__", folder)))
  dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)

  pq_files <- tryCatch({
    hf <- reticulate::import("huggingface_hub")
    api <- hf$HfApi()
    files <- api$list_repo_files(repo_id = repo_id, repo_type = "dataset", token = token)
    files[grepl(paste0("^", folder, "/.*\\.parquet$"), files)]
  }, error = function(e) {
    cat("WARNING: could not list", repo_id, folder, "-", conditionMessage(e), "\n")
    character(0)
  })

  for (f in pq_files) {
    dest <- file.path(local_dir, basename(f))
    if (file.exists(dest) && file.info(dest)$size > 0) next
    src <- tryCatch(hf_cached_file(repo_id, f, token), error = function(e) NULL)
    if (!is.null(src)) file.copy(src, dest, overwrite = TRUE)
  }
  local_dir
}



# ============================================================
# SERVING LAYER — the only place that knows where app-shaped
# data lives. Today: a Hugging Face dataset repo (or a local
# folder via PALACE_SERVING_DIR). Later: Supabase — swap the
# bodies of palace_serving_file()/pool_read()/artifact_read()
# and nothing else in the app changes.
#
# Layout of the serving repo (produced by scripts/build_serving_data.R):
#   pools/<name>.parquet      processed + scored pitch frames
#   artifacts/<name>.rds      derived R objects (ecdf maps, grids, directories)
#   files/<name>.csv          bio/lookup CSVs that used to sit in the app repo
# ============================================================
palace_serving_file <- function(rel) {
  if (nzchar(PALACE_SERVING_DIR)) {
    p <- file.path(PALACE_SERVING_DIR, rel)
    if (file.exists(p)) return(p)
  }
  hf_cached_file(PALACE_SERVING_REPO, rel)
}

pool_read <- function(name) {
  t0 <- Sys.time()
  out <- arrow::read_parquet(palace_serving_file(file.path("pools", paste0(name, ".parquet"))))
  cat(sprintf("[serve] pool %-18s %8d rows  %.1fs\n", name, nrow(out),
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  out
}

artifact_read <- function(name) {
  t0 <- Sys.time()
  out <- readRDS(palace_serving_file(file.path("artifacts", paste0(name, ".rds"))))
  cat(sprintf("[serve] artifact %-14s %.1fs\n", name,
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  out
}

# Coastal + P5 + SBC pitch database. Built once on first use (Matchup
# Matrix, pitch arsenal, hitter expectation tables) and memoised.
.pool_all_env <- new.env(parent = emptyenv())
pool_all <- function() {
  if (!is.null(.pool_all_env$df)) return(.pool_all_env$df)
  cat("[serve] binding full_data_p5_compare (first use)\n")
  .pool_all_env$df <- dplyr::bind_rows(
    harmonize_types(list(data, fall25, prespring, spring26, P5_2025, SBC_2025)))
  .pool_all_env$df
}

# Local-first lookup for a bio/lookup CSV; falls back to files/<name> in the
# serving repo so these files no longer need to be committed to git.
palace_file_candidates <- function(filename) {
  local <- Filter(file.exists, unique(c(
    filename, file.path("data", filename), file.path("www", filename),
    file.path("/data", filename), file.path("/code", filename))))
  if (length(local)) return(local)
  remote <- tryCatch(palace_serving_file(file.path("files", filename)), error = function(e) NULL)
  if (!is.null(remote) && file.exists(remote)) remote else character(0)
}
