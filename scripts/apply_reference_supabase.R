#!/usr/bin/env Rscript
# =============================================================================
# Push the id-keyed reference into the Supabase project so the database and
# Storage carry the same team / conference / player mapping the app uses.
#
#   Rscript scripts/apply_reference_supabase.R            # Storage + database
#   Rscript scripts/apply_reference_supabase.R --no-db    # Storage only
#   Rscript scripts/apply_reference_supabase.R --no-storage
#
# Storage (SUPABASE_URL / SUPABASE_SECRET_KEY, bucket palace-serving):
#   ref/reference/ref_teams.parquet, ref_conferences.parquet,
#   ref_players.parquet, trackman_teams.parquet
# Database (SB_DB_HOST / SB_DB_USER / SB_DB_PASS):
#   pitchprofiler.ref_teams, ref_conferences, ref_players, trackman_teams
#   (dropped and recreated), then team_id / conference / conf_id / level_ref
#   columns on pitcher_season, pitcher_season_pitch_type, pitcher_game and
#   pitcher_game_pitch_type filled from the TrackMan team code through the
#   crosswalk. The per-pitch table is left alone (4 GB; join at query time).
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr); library(arrow); library(DBI); library(RPostgres); library(httr2); library(jsonlite) })
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
args <- commandArgs(trailingOnly = TRUE)
do_db <- !"--no-db" %in% args; do_storage <- !"--no-storage" %in% args
.scout_cache_dir <- function() { d <- file.path(tempdir(), "scout_cache"); dir.create(d, showWarnings = FALSE); d }
source("R/palace_supabase.R"); source("R/palace_artifacts.R"); source("R/03_data_access.R"); source("R/01_reference.R")
if (is.null(REF_TEAMS) || is.null(REF_PLAYERS)) stop("reference files missing (reference/d1_teams.csv, lower_levels_*.csv, bioinfo.csv)")

teams <- REF_TEAMS[, c("team_id", "team_short", "team_full_name", "nickname", "team_abbrev", "location", "association",
                       "level_group", "division", "conf_id", "conf_name", "conf_short", "conf_code", "logo_url")]
confs <- REF_CONFS
players <- REF_PLAYERS[, c("player_id", "name", "first", "last", "pos", "bats", "throws", "jersey", "class", "height", "height_in",
                           "weight", "dob", "age", "hometown", "team_id", "team", "level", "conf", "conf_id", "headshot_url", "logo_url", "source")]
xwalk <- .ref_env$xwalk
xw <- read_csv("reference/trackman_teams.csv", show_col_types = FALSE, col_types = cols(.default = "c"))
xw$team_id <- .ref_id(xw$team_id)
# the latest build's votes (Storage) outrank the shipped file
if (sb_storage_enabled()) {
  for (season in c(2026L, 2025L)) {
    v <- tryCatch(storage_read_parquet(sprintf("lb/%d/team_xwalk.parquet", season)), error = function(e) NULL)
    if (is.data.frame(v) && nrow(v)) { ref_add_team_xwalk(v); cat("[ref] folded", nrow(v), "crosswalk votes from lb/", season, "\n"); break }
  }
}
xwalk <- .ref_env$xwalk
ti <- ref_team_info(team_id = xwalk$team_id)
xwalk <- data.frame(trackman_code = xwalk$trackman_code, team_id = xwalk$team_id, team = ti$team, level = ti$level,
                    conf = ti$conf, conf_id = ti$conf_id, source = xwalk$source, stringsAsFactors = FALSE)
cat("[ref] teams", nrow(teams), "| conferences", nrow(confs), "| players", nrow(players), "| TrackMan codes", nrow(xwalk), "\n")

# ---- Storage ------------------------------------------------------------------
if (do_storage) {
  if (!sb_storage_enabled()) stop("set SUPABASE_URL / SUPABASE_SECRET_KEY (or pass --no-storage)")
  up <- function(df, name) {
    tmp <- tempfile(fileext = ".parquet"); write_parquet(df, tmp, compression = "zstd")
    sb_storage_upload(tmp, sprintf("ref/reference/%s.parquet", name)); unlink(tmp)
    cat(sprintf("[ref] Storage ref/reference/%s.parquet: %d rows\n", name, nrow(df)))
  }
  up(teams, "ref_teams"); up(confs, "ref_conferences"); up(players, "ref_players"); up(xwalk, "trackman_teams")
}

# ---- Database -----------------------------------------------------------------
if (do_db) {
  host <- Sys.getenv("SB_DB_HOST"); user <- Sys.getenv("SB_DB_USER"); pass <- Sys.getenv("SB_DB_PASS")
  port <- as.integer(Sys.getenv("SB_DB_PORT", "5432"))
  if (!nzchar(host) || !nzchar(user) || !nzchar(pass)) stop("set SB_DB_HOST, SB_DB_USER, SB_DB_PASS (or pass --no-db)")
  con <- dbConnect(RPostgres::Postgres(), host = host, port = port, dbname = "postgres", user = user, password = pass, sslmode = "require")
  on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)
  dbExecute(con, "create schema if not exists pitchprofiler")
  load_table <- function(df, name, keys) {
    target <- DBI::Id(schema = "pitchprofiler", table = name)
    dbExecute(con, sprintf("drop table if exists pitchprofiler.%s", name))
    dbCreateTable(con, target, df)
    step <- 20000L
    for (start in seq(1, nrow(df), by = step)) {
      end <- min(start + step - 1L, nrow(df))
      dbAppendTable(con, target, df[start:end, , drop = FALSE])
    }
    for (k in keys) dbExecute(con, sprintf("create index if not exists %s_%s_idx on pitchprofiler.%s (%s)", name, k, name, k))
    cat(sprintf("[ref] pitchprofiler.%s: %d rows\n", name, nrow(df)))
  }
  load_table(teams, "ref_teams", c("team_id", "conf_id", "level_group"))
  load_table(confs, "ref_conferences", c("conf_id", "level_group"))
  load_table(players, "ref_players", c("player_id", "team_id"))
  load_table(xwalk, "trackman_teams", c("trackman_code", "team_id"))

  # conference / level / team id on the aggregate tables, by TrackMan team code
  have <- dbGetQuery(con, "select table_name from information_schema.tables where table_schema = 'pitchprofiler'")$table_name
  for (tbl in intersect(c("pitcher_season", "pitcher_season_pitch_type", "pitcher_game", "pitcher_game_pitch_type"), have)) {
    cols <- dbGetQuery(con, sprintf("select column_name from information_schema.columns where table_schema = 'pitchprofiler' and table_name = '%s'", tbl))$column_name
    if (!"pitcher_team" %in% cols) { cat("[ref]", tbl, "has no pitcher_team column, skipped\n"); next }
    for (cc in c("team_id text", "conference text", "conf_id text", "level_ref text"))
      dbExecute(con, sprintf("alter table pitchprofiler.%s add column if not exists %s", tbl, cc))
    n <- dbExecute(con, sprintf(
      "update pitchprofiler.%s p set team_id = x.team_id, conference = x.conf, conf_id = x.conf_id, level_ref = x.level
         from pitchprofiler.trackman_teams x where upper(p.pitcher_team) = x.trackman_code", tbl))
    cat(sprintf("[ref] pitchprofiler.%s: %d rows mapped to a reference team\n", tbl, n))
  }
  cat("[ref] database done\n")
}
cat("[ref] done\n")
