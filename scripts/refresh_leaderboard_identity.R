#!/usr/bin/env Rscript
# =============================================================================
# Refresh the identity columns of an existing lb/<season>/ build in Storage
# WITHOUT TruMedia or database access: Level / Conf / conf_id / Logo /
# team_id / tm_player_id / bio on pitchers, pitches, hitters; conference /
# level / team_id on pitcher_day; level / conference on teams; plus the
# identity and team_xwalk tables the app's search resolves from.
#
#   Rscript scripts/refresh_leaderboard_identity.R [--season 2026] [--dry-run]
#
# Stats are untouched. The previous files are copied to
# lb/<season>/backup_<stamp>/ before anything is overwritten. Needs only
# SUPABASE_URL / SUPABASE_SECRET_KEY. Run scripts/build_leaderboards.R for a
# full rebuild (fresh TruMedia lines, batter_dir) when the TM_* secrets and
# the pipeline output are available.
# =============================================================================
suppressPackageStartupMessages({
  library(shiny); library(htmltools); library(dplyr); library(readr); library(tidyr); library(purrr); library(stringr)
  library(tibble); library(magrittr); library(arrow); library(httr); library(httr2); library(jsonlite)
})
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
args <- commandArgs(trailingOnly = TRUE)
arg_val <- function(flag, default = NULL) if (flag %in% args) args[which(args == flag) + 1] else default
season <- as.integer(arg_val("--season", "2026")); dry <- "--dry-run" %in% args
t_start <- Sys.time()
.scout_cache_dir <- function() { d <- file.path(tempdir(), "scout_cache"); dir.create(d, showWarnings = FALSE); d }
source("R/palace_supabase.R"); source("R/palace_artifacts.R"); source("R/03_data_access.R"); source("R/01_reference.R"); source("R/02_trumedia_api.R")
l10 <- readLines("R/10_ncaa_bio.R"); eval(parse(text = l10[1:(grep("^bio_heights_ncaa <- ", l10) - 1)]))
l09 <- readLines("R/09_leaderboard.R"); eval(parse(text = l09[1:(grep("^ncaa_raw_lookup <- ", l09) - 1)]))
lb <- readLines("scripts/build_leaderboards.R")
a <- grep("^\\.team_level_map <- local", lb); b <- grep("^# 1\\) TruMedia: league-wide raw frames", lb)
eval(parse(text = lb[a:(b - 2)]))                      # team_level / team_conf / team_ident
normalize_lastfirst <- function(x) sub("^\\s*([^,]+),\\s*(.+)\\s*$", "\\2 \\1", as.character(x))
idk <- function(x) { x <- trimws(as.character(x)); x[is.na(x) | !nzchar(x) | x %in% c("NA", "0")] <- NA_character_; x }
if (!sb_storage_enabled()) stop("set SUPABASE_URL / SUPABASE_SECRET_KEY")

# ---- read --------------------------------------------------------------------
rd <- function(nm) tryCatch(as.data.frame(storage_read_parquet(sprintf("lb/%d/%s.parquet", season, nm))), error = function(e) NULL)
nms <- c("teams", "tm_pitching_totals", "tm_batting_totals", "tm_pitching_team", "tm_batting_team", "pitchers", "pitches", "hitters", "pitcher_day")
pre <- setNames(lapply(nms, rd), nms)
for (nm in c("teams", "tm_pitching_totals", "tm_batting_totals", "pitchers", "pitches", "hitters", "pitcher_day"))
  if (is.null(pre[[nm]]) || !nrow(pre[[nm]])) stop("lb/", season, "/", nm, ".parquet missing in Storage")
man <- storage_read_json(sprintf("lb/%d/manifest.json", season)) %||% list()
cat("[refresh] read", paste(names(pre)[!vapply(pre, is.null, logical(1))], collapse = ", "), "\n")
teams <- pre$teams; tm_pitching <- pre$tm_pitching_totals; tm_batting <- pre$tm_batting_totals
pitchers <- pre$pitchers; pitches <- pre$pitches; hitters <- pre$hitters; pitcher_day <- pre$pitcher_day

# ---- crosswalk votes ---------------------------------------------------------
codes <- pitcher_day %>% count(pitcher_id, pitcher_team) %>% arrange(desc(n)) %>% distinct(pitcher_id, .keep_all = TRUE)
codes$pitcher_id <- idk(codes$pitcher_id)
xw_pairs <- data.frame(tm_id = codes$pitcher_id, trackman_code = codes$pitcher_team, stringsAsFactors = FALSE)
xw_ids <- bind_rows(
  if (!is.null(pre$tm_pitching_team)) data.frame(tm_id = idk(pre$tm_pitching_team$trackmanPlayerId), team_id = as.character(pre$tm_pitching_team$teamId)) else NULL,
  if (!is.null(pre$tm_batting_team))  data.frame(tm_id = idk(pre$tm_batting_team$trackmanPlayerId),  team_id = as.character(pre$tm_batting_team$teamId))  else NULL,
  data.frame(tm_id = idk(pitchers$tm_id), team_id = as.character(pitchers$tm_team_id)),
  data.frame(tm_id = idk(hitters$tm_id), team_id = as.character(hitters$tm_team_id)),
  data.frame(tm_id = idk(tm_pitching$trackmanPlayerId), team_id = as.character(tm_pitching$mostRecentTeamId)))
team_xwalk <- ref_team_votes(xw_pairs, xw_ids)
if (!is.null(team_xwalk) && nrow(team_xwalk)) {
  ti0 <- ref_team_info(team_id = team_xwalk$team_id); team_xwalk$team <- ti0$team; team_xwalk$level <- ti0$level; team_xwalk$conf <- ti0$conf
  ref_add_team_xwalk(team_xwalk)
}
cat("[refresh] crosswalk votes:", NROW(team_xwalk), "codes\n")

# ---- teams -------------------------------------------------------------------
tn <- intersect(c("location", "fullName", "teamName"), names(teams))
tmi <- team_ident(team_id = teams$teamId, name = if (length(tn)) teams[[tn[1]]] else NULL)
teams$level <- tmi$level; teams$conference <- tmi$conf; teams$conf_id <- tmi$conf_id; teams$logo_url <- tmi$logo
cat("[refresh] teams by level:", paste(names(table(teams$level)), table(teams$level), collapse = ", "), "\n")

# ---- pitchers ----------------------------------------------------------------
scoped <- !is.null(pre$tm_pitching_team) && nrow(pre$tm_pitching_team) > 0
pitchers$tm_id <- idk(pitchers$tm_id)
pitchers$tm_player_id <- idk(tm_pitching$playerId)[match(pitchers$tm_id, idk(tm_pitching$trackmanPlayerId))]
code <- codes$pitcher_team[match(pitchers$tm_id, codes$pitcher_id)]
tid <- as.character(pitchers$tm_team_id)
if (!scoped) tid <- ifelse(is.na(ref_team_id_for_code(code)), tid, NA_character_)   # a known code beats the calendar-year club
old_level <- pitchers$Level
di <- team_ident(team_id = tid, code = code, name = pitchers$School, tk_level = pitchers$Level)
pitchers$team_id <- di$team_id; pitchers$Level <- di$level; pitchers$Conf <- di$conf; pitchers$conf_id <- di$conf_id; pitchers$Logo <- di$logo
looks_code <- !is.na(pitchers$School) & grepl("^[A-Z0-9_]{3,}$", pitchers$School)
pitchers$School <- ifelse(looks_code & !is.na(di$team), di$team, pitchers$School)
pitchers <- .lb_attach_bio(pitchers)
pitchers$`Arm Ang` <- .lb_arm_angle(pitchers$relh %||% NA_real_, pitchers$rels %||% NA_real_, pitchers$.ht)
cat("[refresh] pitchers:", nrow(pitchers), "| with reference team:", sum(!is.na(ref_team_level(pitchers$team_id))), "| Level old -> new:\n"); print(table(old = old_level, new = pitchers$Level))

# ---- pitches -----------------------------------------------------------------
pitches$tm_id <- idk(pitches$tm_id)
k <- match(pitches$tm_id, pitchers$tm_id)
car <- intersect(c("Level", "Conf", "conf_id", "Logo", "team_id", "tm_player_id", "Class", "Age", "School", ".ht"), names(pitchers))
for (cc in car) { if (!cc %in% names(pitches)) pitches[[cc]] <- NA; pitches[[cc]] <- ifelse(is.na(k), pitches[[cc]], pitchers[[cc]][k]) }
gap <- is.na(k)
if (any(gap)) { g <- team_ident(name = pitches$School[gap], tk_level = pitches$Level[gap]); pitches$Level[gap] <- g$level; pitches$Conf[gap] <- g$conf; pitches$conf_id[gap] <- g$conf_id; pitches$Logo[gap] <- g$logo; pitches$team_id[gap] <- g$team_id }
pitches$`Arm Ang` <- .lb_arm_angle(pitches$relh, pitches$rels, pitches$.ht)
cat("[refresh] pitches:", nrow(pitches), "rows |", sum(!gap), "matched to a pitcher row\n")

# ---- hitters -----------------------------------------------------------------
hitters$tm_id <- idk(hitters$tm_id)
hitters$tm_player_id <- idk(tm_batting$playerId)[match(hitters$tm_id, idk(tm_batting$trackmanPlayerId))]
hold <- hitters$Level
hdi <- team_ident(team_id = as.character(hitters$tm_team_id), name = hitters$School, tk_level = hitters$Level)
hitters$team_id <- hdi$team_id; hitters$Level <- hdi$level; hitters$Conf <- hdi$conf; hitters$conf_id <- hdi$conf_id; hitters$Logo <- hdi$logo
looks_code <- !is.na(hitters$School) & grepl("^[A-Z0-9_]{3,}$", hitters$School)
hitters$School <- ifelse(looks_code & !is.na(hdi$team), hdi$team, hitters$School)
hitters <- .lb_attach_bio(hitters)
cat("[refresh] hitters:", nrow(hitters), "| Level old -> new:\n"); print(table(old = hold, new = hitters$Level))

# ---- pitcher_day -------------------------------------------------------------
pdi <- team_ident(code = pitcher_day$pitcher_team, name = prettify_team(pitcher_day$pitcher_team), tk_level = pitcher_day$level)
pitcher_day$team_id <- pdi$team_id; pitcher_day$conference <- pdi$conf; pitcher_day$conf_id <- pdi$conf_id; pitcher_day$level <- pdi$level
cat("[refresh] pitcher_day: conference filled on", sum(!is.na(pitcher_day$conference)), "of", nrow(pitcher_day), "rows\n")

# ---- identity ----------------------------------------------------------------
pd_names <- pitcher_day %>% filter(!is.na(pitcher_name), nzchar(pitcher_name)) %>% count(pitcher_id, pitcher_name) %>% arrange(desc(n)) %>% distinct(pitcher_id, .keep_all = TRUE)
tk <- data.frame(kind = "pit", tm_id = codes$pitcher_id, name = ifelse(grepl(",", pd_names$pitcher_name), normalize_lastfirst(pd_names$pitcher_name), pd_names$pitcher_name)[match(codes$pitcher_id, idk(pd_names$pitcher_id))],
                 trackman_code = codes$pitcher_team, tk_level = pitchers$Level[match(codes$pitcher_id, pitchers$tm_id)], stringsAsFactors = FALSE)
identity <- ref_identity_table(list(tm_pitching_team = pre$tm_pitching_team, tm_batting_team = pre$tm_batting_team,
                                    tm_pitching_totals = tm_pitching, tm_batting_totals = tm_batting), trackman = tk, season = season)
ref_identity_report(identity)

# ---- upload ------------------------------------------------------------------
if (dry) { cat("[refresh] dry run: nothing uploaded (", round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), "min )\n"); quit(save = "no") }
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir <- file.path("serving_build", sprintf("lb_%d_refresh", season)); dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
up <- function(df, name, backup = NULL) {
  if (!is.null(backup)) { tb <- tempfile(fileext = ".parquet"); arrow::write_parquet(backup, tb, compression = "zstd")
    sb_storage_upload(tb, sprintf("lb/%d/backup_%s/%s.parquet", season, stamp, name)); unlink(tb) }
  tmp <- file.path(out_dir, paste0(name, ".parquet")); arrow::write_parquet(df, tmp, compression = "zstd")
  sb_storage_upload(tmp, sprintf("lb/%d/%s.parquet", season, name))
  cat(sprintf("[refresh] uploaded lb/%d/%s.parquet: %d rows x %d cols, %.1f MB\n", season, name, nrow(df), ncol(df), file.size(tmp) / 1e6))
}
up(teams, "teams", pre$teams); up(pitchers, "pitchers", pre$pitchers); up(pitches, "pitches", pre$pitches); up(hitters, "hitters", pre$hitters)
up(pitcher_day, "pitcher_day", pre$pitcher_day); up(identity, "identity"); if (!is.null(team_xwalk) && nrow(team_xwalk)) up(team_xwalk, "team_xwalk")
man$identity_refreshed_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"); man$backup <- sprintf("lb/%d/backup_%s/", season, stamp)
man$rows <- modifyList(as.list(man$rows %||% list()), list(pitchers = nrow(pitchers), pitches = nrow(pitches), hitters = nrow(hitters),
                                                        pitcher_day = nrow(pitcher_day), identity = nrow(identity), team_xwalk = NROW(team_xwalk)))
tmp <- tempfile(fileext = ".json"); writeLines(jsonlite::toJSON(man, auto_unbox = TRUE, pretty = TRUE), tmp)
sb_storage_upload(tmp, sprintf("lb/%d/manifest.json", season), content_type = "application/json"); unlink(tmp)
cat("[refresh] done in", round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), "min | previous files in", man$backup, "\n")
