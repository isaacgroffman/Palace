# ============================================================
# POOLS — the pitch frames the app reads. All 2026, all from Supabase.
#
# Objects produced (everything downstream depends on them):
#   spring26, sprayspring26, spring26_game_choices   Coastal 2026 games
#   data, spraydata, fall25, sprayfall25,            retired seasons: ZERO-ROW
#   prespring, sprayprespring                        frames with the game schema
#   P5_2026, SBC_2026          2026 reference pools (LAZY: pulled on first use)
#   full_data_p5_compare       everything bound together (LAZY)
#   P5_slim, SBC_slim          slim grading pools (LAZY, precomputed artifacts)
#   p5_maps, exp_movement_grid, p5_slot_grid   precomputed artifacts (LAZY)
#
# Every frame arrives already scored (stuff_xrv / plus per pitch) and typed,
# so process_pitcher_data() only builds indicators, heights, arm angles and
# spray tables here; no Python scoring runs at boot.
# ============================================================

# Lookups process_pitcher_data() needs (heights, leagues, run environments)
batter_info <- read_csv("reference/batter_heights.csv", show_col_types = FALSE)
team_leagues <- read_csv("reference/team_leagues.csv", show_col_types = FALSE)
league_run_environments <- read_csv("reference/old_league_run_environments.csv", show_col_types = FALSE)

# Median slot (HB, IVB) per pitch type x release-height bin, from the P5 pool.
build_p5_slot_grid <- function(p5, bin = 0.10) {
  req_cols <- c("TaggedPitchType","RelHeight","HorzBreak","InducedVertBreak")
  if (!all(req_cols %in% names(p5))) return(tibble())
  p5 %>%
    filter(!is.na(TaggedPitchType), !is.na(RelHeight),
           !is.na(HorzBreak), !is.na(InducedVertBreak)) %>%
    mutate(RH_bin = round(RelHeight / bin) * bin) %>%
    group_by(TaggedPitchType, RH_bin) %>%
    summarise(
      slot_hb  = median(HorzBreak, na.rm = TRUE),
      slot_ivb = median(InducedVertBreak, na.rm = TRUE),
      n = n(), .groups = "drop"
    )
}

# process_pitcher_data() works in inches internally; the app-wide convention
# for a finished pool is FEET (pool_to_feet in R/palace_artifacts.R detects
# the unit, so it is safe on frames that are already in feet).
.pool_to_feet <- pool_to_feet

# ---- Coastal 2026 games -------------------------------------------------
# Storage first (ref/<stamp>/coastal_games.parquet, written by
# scripts/build_reference_pools.R): a 3 MB download is reliable, the database
# pooler is not. Live pull as the fallback.
.coastal_from_storage <- function() {
  stamp <- gsub("[^0-9]", "", pp_built_at() %||% "")
  if (!nzchar(stamp) || !sb_storage_enabled()) return(NULL)
  dest <- tempfile(fileext = ".parquet")
  if (!sb_storage_download(sprintf("ref/%s/coastal_games.parquet", stamp), dest)) return(NULL)
  d <- tryCatch(as.data.frame(arrow::read_parquet(dest)), error = function(e) NULL)
  unlink(dest)
  if (is.data.frame(d) && nrow(d) > 0) { if ("Date" %in% names(d)) d$Date <- as.Date(d$Date); cat("[pools] Coastal 2026 games from Storage\n"); d } else NULL
}
spring26_raw <- tryCatch(.coastal_from_storage(), error = function(e) NULL)
if (is.null(spring26_raw)) spring26_raw <- tryCatch(pp_coastal_games(), error = function(e) {
  cat("WARNING: Coastal 2026 games unavailable -", conditionMessage(e), "\n"); NULL
})
if (is.null(spring26_raw) || nrow(spring26_raw) == 0) {
  stop("No Coastal 2026 pitches from Supabase (pitchprofiler.pitches). ",
       "Check SB_DB_HOST / SB_DB_USER / SB_DB_PASS.")
}
cat("[pools] Coastal 2026 games:", nrow(spring26_raw), "pitches,",
    length(unique(spring26_raw$GameID)), "games\n")

spring26_processed <- process_pitcher_data(spring26_raw, bio_heights, "spring26_ind")
spring26      <- .pool_to_feet(add_roster_flag(spring26_processed$data))
sprayspring26 <- spring26_processed$spray

if (nrow(spring26) > 0) {
  spring26 <- spring26 %>%
    mutate(
      opponent_code = ifelse(!is.na(BatterTeam), BatterTeam,
                             ifelse(PitcherTeam == HomeTeam, AwayTeam, "Unknown")),
      opponent_name = prettify_team(opponent_code),
      game_label = paste0(as.character(Date), " vs ", opponent_name)
    )
  if (is.data.frame(sprayspring26) && nrow(sprayspring26) > 0) {
    sprayspring26 <- sprayspring26 %>%
      mutate(
        opponent_code = ifelse(!is.na(BatterTeam), BatterTeam,
                               ifelse(PitcherTeam == HomeTeam, AwayTeam, "Unknown")),
        opponent_name = prettify_team(opponent_code),
        game_label = paste0(as.character(Date), " vs ", opponent_name)
      )
  }
}
spring26_game_choices <- sort(unique(spring26$game_label))
cat("Spring 2026 games available:", length(spring26_game_choices), "\n")

# Retired seasons (2025, pre-spring 2026): zero-row frames with the game
# schema so every season_map entry, filter and bind keeps working.
data           <- spring26[0, , drop = FALSE]
fall25         <- spring26[0, , drop = FALSE]
prespring      <- spring26[0, , drop = FALSE]
spraydata      <- sprayspring26[0, , drop = FALSE]
sprayfall25    <- sprayspring26[0, , drop = FALSE]
sprayprespring <- sprayspring26[0, , drop = FALSE]
manual_data_loaded <- FALSE
odu_data_loaded    <- FALSE

# ---- 2026 reference pools (lazy) ---------------------------------------
# Power 5 and Sun Belt pitches from Supabase, processed exactly like the
# Coastal pool and kept in the app-wide FEET convention.
.ref_env <- new.env(parent = emptyenv())
# Processed reference pools are cached on disk, keyed by the scored tables'
# build stamp: /data survives restarts on the deployment host, tempdir()
# locally. A redeploy then reads them back in seconds instead of pulling and
# processing 700k pitches again.
.ref_cache_dir <- function() {
  d <- if (dir.exists("/data")) "/data/ref_cache" else file.path(tempdir(), "ref_cache")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}
.ref_cache_path <- function(label) {
  stamp <- gsub("[^0-9]", "", pp_built_at() %||% "")
  if (!nzchar(stamp)) return(NULL)
  file.path(.ref_cache_dir(), paste0(label, "_", stamp, ".rds"))
}
# force = TRUE (the build script) skips the memory, disk and Storage tiers
# and pulls from the database, so a rebuild never reuses a stale file.
.ref_processed <- function(label, leagues, ind, force = FALSE) {
  key <- paste0(label, "_proc")
  if (!force && !is.null(.ref_env[[key]])) return(.ref_env[[key]])
  # 1) disk cache
  cp <- tryCatch(.ref_cache_path(label), error = function(e) NULL)
  if (!force && !is.null(cp) && file.exists(cp)) {
    proc <- tryCatch(readRDS(cp), error = function(e) NULL)
    if (is.data.frame(proc) && nrow(proc) > 0) {
      cat("[pools]", label, "reference pool: disk cache hit,", nrow(proc), "pitches\n")
      .ref_env[[key]] <- proc
      return(proc)
    }
  }
  # 2) Supabase Storage: the processed pool built by scripts/build_reference_pools.R
  stamp <- gsub("[^0-9]", "", pp_built_at() %||% "")
  if (!force) {
    proc <- tryCatch(storage_pool(label, stamp), error = function(e) NULL)
    if (is.data.frame(proc) && nrow(proc) > 0) {
      if (!is.null(cp)) tryCatch(saveRDS(proc, cp), error = function(e) NULL)
      .ref_env[[key]] <- proc
      return(proc)
    }
    if (nzchar(stamp)) cat("[pools]", label, "reference pool not in Storage for build", stamp, "- pulling from the database\n")
  }
  # 3) live pull from pitchprofiler.pitches (slow: the pooler throttles big pulls)
  t0 <- Sys.time()
  raw <- pp_league_pool(leagues)
  if (is.null(raw) || nrow(raw) == 0) {
    cat("WARNING:", label, "reference pool is empty\n")
    .ref_env[[key]] <- spring26_processed$data[0, , drop = FALSE]
    return(.ref_env[[key]])
  }
  proc <- .pool_to_feet(add_roster_flag(process_pitcher_data(raw, bio_heights, ind)$data))
  cat(sprintf("[pools] %s reference pool: %d pitches ready in %.0fs\n", label, nrow(proc),
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  if (!is.null(cp)) tryCatch(saveRDS(proc, cp), error = function(e) NULL)
  .ref_env[[key]] <- proc
  proc
}
P5_LEAGUES  <- c("NCAA SEC", "NCAA ACC", "NCAA Big 12", "NCAA Big Ten")
SBC_LEAGUES <- c("NCAA Sun Belt")

# ---- precomputed artifacts (R/palace_artifacts.R) --------------------------
# Percentile maps, movement grids and the slim grading pools are built once
# per scored-table build by scripts/build_reference_pools.R and read from
# Storage; the fallbacks compute them from the full pools.
.artifact <- function(name, kind, compute) {
  cp <- tryCatch(.ref_cache_path(paste0("art_", name)), error = function(e) NULL)
  if (!is.null(cp) && file.exists(cp)) {
    obj <- tryCatch(readRDS(cp), error = function(e) NULL)
    if (!is.null(obj)) { cat("[artifacts]", name, ": disk cache hit\n"); return(obj) }
  }
  stamp <- gsub("[^0-9]", "", pp_built_at() %||% "")
  obj <- tryCatch(artifact_download(name, stamp, kind), error = function(e) NULL)
  if (is.null(obj)) {
    cat("[artifacts]", name, "not in Storage - computing from the reference pool\n")
    obj <- compute()
  }
  if (!is.null(cp) && !is.null(obj)) tryCatch(saveRDS(obj, cp), error = function(e) NULL)
  obj
}

# One copy of each full pool in memory (feet convention); only the matchup
# matrix, hitter process model and pitch arsenal touch these.
delayedAssign("P5_2026",  .ref_processed("P5",  P5_LEAGUES,  "P5_ind"))
delayedAssign("SBC_2026", .ref_processed("SBC", SBC_LEAGUES, "SBC_ind"))
delayedAssign("full_data_p5_compare", pool_all())

# What the pitcher page needs: ~1 MB of maps plus a ~40-column grading pool.
delayedAssign("p5_maps",           .artifact("p5_maps", "rds", function() slim_ecdf_maps(build_p5_ecdf_maps(P5_2026))))
delayedAssign("exp_movement_grid", .artifact("exp_movement_grid", "rds", function() build_expected_movement_grid(P5_2026, bin_size = 0.10)))
delayedAssign("p5_slot_grid",      .artifact("p5_slot_grid", "rds", function() build_p5_slot_grid(P5_2026, bin = 0.10)))
delayedAssign("P5_slim",  .artifact("grade_P5",  "parquet", function() build_grade_pool(P5_2026)))
delayedAssign("SBC_slim", .artifact("grade_SBC", "parquet", function() build_grade_pool(SBC_2026)))

if (PALACE_PREWARM) {
  cat("[pools] PALACE_PREWARM=TRUE: forcing lazy reference pools at boot\n")
  invisible(list(p5_maps, exp_movement_grid, p5_slot_grid, P5_slim, SBC_slim, P5_2026, SBC_2026, full_data_p5_compare))
}
