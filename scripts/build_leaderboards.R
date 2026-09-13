#!/usr/bin/env Rscript
# =============================================================================
# Precomputed leaderboard + Advance-tab tables -> Supabase Storage
# (bucket palace-serving, lb/<season>/*.parquet).
#
#   Rscript scripts/build_leaderboards.R --pipeline-dir serving_build/master_2026
#   Rscript scripts/build_leaderboards.R                 # TrackMan side from the database
#
# TruMedia (traditional stats): ONE league-wide PlayerTotals pull each for
# pitching and batting, plus the count tokens the boards derive rates from,
# and the season team list. Stored raw (API column names) so every
# tm_team_player_totals() / tm_all_teams() caller in the app is served from
# these frames without a request.
#
# TrackMan (pitch-level columns): pitcher_season / pitcher_season_pitch_type
# from the scoring pipeline (velo, spin, release, Stuff+/Pitching+/Location+,
# xBA/xSLG/xwOBA, whiff/chase/zone) and RV/100 + hitter contact/discipline
# aggregates computed from the per-pitch table. Joined to TruMedia on the
# TrackMan player id both sides carry.
#
# Outputs: teams, tm_pitching_totals, tm_batting_totals, pitchers (board),
# pitches (board, by pitch type), hitters, identity (player x team x season
# type by id, with collision flags), team_xwalk (TrackMan code -> TruMedia
# team id votes), pitcher_day, manifest.json.
#
# Level / conference / logo on every row come from the id-keyed reference
# (reference/d1_teams.csv, lower_levels_teams.csv, ...; see R/01_reference.R)
# through the TruMedia team id or the TrackMan team code, never the name.
# Needs TM_USERNAME / TM_SITENAME / TM_MASTER_TOKEN and SUPABASE_URL /
# SUPABASE_SECRET_KEY; SB_DB_* only without --pipeline-dir.
# =============================================================================
suppressPackageStartupMessages({
  library(shiny); library(htmltools); library(dplyr); library(readr); library(tidyr)
  library(purrr); library(stringr); library(tibble); library(magrittr); library(lubridate)
  library(arrow); library(httr); library(httr2); library(jsonlite); library(pool)
  library(RPostgres); library(DBI)
})
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
t_start <- Sys.time()
args <- commandArgs(trailingOnly = TRUE)
arg_val <- function(flag, default = NULL) if (flag %in% args) args[which(args == flag) + 1] else default
pipeline_dir <- arg_val("--pipeline-dir")
season <- as.integer(arg_val("--season", "2026"))

# ---- function files only (no app boot) --------------------------------------
.scout_cache_dir <- function() { d <- file.path(tempdir(), "scout_cache"); dir.create(d, showWarnings = FALSE); d }
source("R/palace_supabase.R"); source("R/palace_artifacts.R")
source("R/03_data_access.R"); source("R/01_reference.R"); source("R/02_trumedia_api.R")
l10 <- readLines("R/10_ncaa_bio.R")   # drs_bio / hs_bio / lookups; stop before the app-only height table
eval(parse(text = l10[1:(grep("^bio_heights_ncaa <- ", l10) - 1)]))
l09 <- readLines("R/09_leaderboard.R")
eval(parse(text = l09[1:(grep("^ncaa_raw_lookup <- ", l09) - 1)]))   # functions + constants only
normalize_lastfirst <- function(x) sub("^\\s*([^,]+),\\s*(.+)\\s*$", "\\2 \\1", as.character(x))
# TrackMan ids as join keys: blank / "NA" become NA so they never match each other
idk <- function(x) { x <- trimws(as.character(x)); x[is.na(x) | !nzchar(x) | x %in% c("NA", "0")] <- NA_character_; x }
# one TruMedia row per player: the one with the most playing time
dedupe_players <- function(df, qty) {
  q <- suppressWarnings(as.numeric(df[[qty]])); q[!is.finite(q)] <- -Inf
  df <- df[order(-q), , drop = FALSE]
  df[!duplicated(df$playerId), , drop = FALSE]
}
if (!sb_storage_enabled()) stop("set SUPABASE_URL / SUPABASE_SECRET_KEY")
# TruMedia frames: live pulls when the TM_* secrets are set, otherwise the
# frames already in Storage from the last live build (--tm-from-storage
# forces that even with credentials). The TrackMan side rebuilds either way.
tm_live <- tm_enabled() && !"--tm-from-storage" %in% args
team_scoped <- tm_live && !"--no-team-scoped" %in% args
.tm_env$season_override <- season
cat("[lb build] season", season, "| TruMedia:", if (tm_live) "live" else "from Storage",
    "| team-scoped lines:", team_scoped, "\n")

# ---- team classification (level + conference) --------------------------------
# reference/merged_teams.csv carries NCAA D1/D2/D3/NAIA + conference by school
# name; TrackMan rows carry their own level/league. Anything TruMedia lists
# that matches neither is a summer / independent club ("Other").
.team_level_map <- local({
  m <- read_csv("reference/merged_teams.csv", show_col_types = FALSE)
  m <- m[!is.na(m$team_name) & nzchar(m$team_name), , drop = FALSE]
  lvl <- toupper(trimws(sub("^NCAA\\s*", "", as.character(m$source %||% NA))))
  lvl[!lvl %in% c("D1", "D2", "D3", "NAIA")] <- NA_character_
  data.frame(key = .conf_norm(m$team_name), level = lvl,
             conf = ifelse(is.na(m$conference) | m$conference %in% c("", "Team"), NA_character_, m$conference),
             abbr = as.character(m$trackman_abbr), stringsAsFactors = FALSE)
})
team_level <- function(school) {
  i <- match(.conf_norm(school), .team_level_map$key)
  out <- .team_level_map$level[i]; out[is.na(i)] <- "Other"; out
}
team_conf <- function(school) .team_level_map$conf[match(.conf_norm(school), .team_level_map$key)]
code_conf <- function(code) .team_level_map$conf[match(as.character(code), .team_level_map$abbr)]

# Row identity from the id-keyed reference (R/01_reference.R): the TruMedia
# team id first, the TrackMan code through the crosswalk second, the name
# last, the TrackMan level tag as the final fallback for Level. Summer clubs
# sit under the boards' "Summer/Other" pill; their league still fills Conf.
LB_LEVELS <- c("D1", "D2", "D3", "NAIA", "JUCO")
team_ident <- function(team_id = NULL, code = NULL, name = NULL, tk_level = NULL) {
  n <- max(length(team_id), length(code), length(name), 1L)
  ti <- ref_team_info(team_id = team_id, code = code, name = name,
                      level = if (!is.null(tk_level)) rep_len(as.character(tk_level), n) else NULL, by_name = TRUE)
  tk <- if (!is.null(tk_level)) rep_len(as.character(tk_level), n) else rep(NA_character_, n)
  lvl <- ti$level
  lvl <- ifelse(is.na(lvl) & tk %in% LB_LEVELS, tk, lvl)
  if (!is.null(name)) { nm <- rep_len(as.character(name), n); lvl <- ifelse(is.na(lvl), team_level(nm), lvl) }
  lvl[is.na(lvl) | !lvl %in% LB_LEVELS] <- "Other"
  logo <- ti$logo
  if (!is.null(name) && exists("team_name_map") && "logo_url" %in% names(team_name_map)) {
    nm <- rep_len(as.character(name), n)
    fb <- as.character(team_name_map$logo_url)[match(.conf_norm(nm), .conf_norm(team_name_map$team_name))]
    logo <- ifelse(is.na(logo) | !nzchar(logo), fb, logo)
  }
  data.frame(team_id = ti$team_id, level = lvl, conf = ti$conf, conf_id = ti$conf_id, conf_name = ti$conf_name,
             logo = logo, team = ti$team, stringsAsFactors = FALSE)
}

# =============================================================================
# 1) TruMedia: league-wide raw frames (+ team-scoped lines)
# =============================================================================
pull <- function(cols, rq = "", extra = list()) {
  p <- c(list(seasonYear = season, format = "RAW"), extra)
  if (!is.null(cols)) p$columns <- cols
  tm_get_csv("PlayerTotals", params = p, raw_query = rq)
}
id_cols <- c("playerId", "trackmanPlayerId", "abbrevName", "fullName", "mostRecentTeamId", "mostRecentTeamName")
from_storage <- function(name) {
  d <- storage_read_parquet(sprintf("lb/%d/%s.parquet", season, name))
  if (is.null(d) || !nrow(d)) stop("lb/", season, "/", name, ".parquet not in Storage: run a live build first")
  d
}

if (tm_live) {
  teams <- tm_all_teams()
  if (is.null(teams) || nrow(teams) == 0) stop("AllTeams returned nothing")
  cat("[lb build] teams:", nrow(teams), "\n")

  trad <- pull(TM_TRAD_COLS)
  if (is.null(trad) || nrow(trad) == 0) trad <- pull(TM_TRAD_COLS_MIN)
  if (is.null(trad) || nrow(trad) == 0) stop("PlayerTotals pitching returned nothing")
  tok <- .lb_probe_tokens(NULL)
  rcols <- .lb_rate_cols(tok)
  rates <- if (is.null(rcols)) NULL else pull(rcols)
  join_rates <- function(a, b) {
    if (!is.null(b) && "playerId" %in% names(b)) {
      extra <- b[, setdiff(names(b), setdiff(names(a), "playerId")), drop = FALSE]
      dplyr::left_join(a, extra, by = "playerId")
    } else a
  }
  tm_pitching <- dedupe_players(join_rates(trad, rates), "IP")
  cat("[lb build] pitching totals:", nrow(tm_pitching), "rows x", ncol(tm_pitching), "cols\n")

  bat <- pull(TM_BAT_TRAD_COLS)
  if (is.null(bat) || nrow(bat) == 0) bat <- pull(TM_BAT_COLS)
  if (is.null(bat) || nrow(bat) == 0) stop("PlayerTotals batting returned nothing")
  # batter count tokens (same probe tm_batter_grades_team runs, against a real team)
  probe_tid <- tryCatch(tm_find_team_id("Coastal Carolina"), error = function(e) NULL)
  ok_one <- function(t) { out <- tm_get_csv("PlayerTotals", params = list(seasonYear = season, format = "RAW",
                                              teamId = probe_tid, columns = paste0("[PA],[", t, "]")))
                          !is.null(out) && nrow(out) > 0 }
  pick <- function(c) { for (t in c) if (ok_one(t)) return(t); NULL }
  bat_tok <- list(swing = pick(TM_BAT_COUNT_CANDIDATES$swing), contact = pick(TM_BAT_COUNT_CANDIDATES$contact),
                  miss = pick(TM_BAT_COUNT_CANDIDATES$miss), chase = pick(TM_BAT_COUNT_CANDIDATES$chase),
                  ooz = pick(TM_BAT_COUNT_CANDIDATES$ooz), xwoba = pick(TM_BAT_ATOMIC_CANDIDATES$xwoba))
  bt <- unlist(bat_tok[!vapply(bat_tok, is.null, logical(1))])
  cat("[lb build] batter tokens:", paste(names(bt), bt, sep = "=", collapse = " "), "\n")
  bcols <- if (length(bt)) paste0("[", unique(c("PA", bt)), "]", collapse = ",") else NULL
  bat_rates <- if (!is.null(bcols)) pull(bcols) else NULL
  tm_batting <- dedupe_players(join_rates(bat, bat_rates), "PA")
  cat("[lb build] batting totals:", nrow(tm_batting), "rows x", ncol(tm_batting), "cols\n")

  # ---- team-scoped season lines -------------------------------------------
  # The league-wide frames above give ONE row per player for the whole
  # calendar year, so a spring NCAA line and a summer-league line collapse
  # into each other (and the player's "team" is whichever he played for
  # last). A PlayerTotals pull per NCAA team returns that team's games
  # only: one row per player x team, which is what the boards and the
  # player-page season table show.
  tm_pitching_team <- NULL; tm_batting_team <- NULL
  if (team_scoped) {
    nc <- intersect(c("location", "fullName", "teamName"), names(teams))
    ncaa <- teams[vapply(seq_len(nrow(teams)), function(i)
      any(vapply(nc, function(cc) team_level(teams[[cc]][i]) != "Other", logical(1))), logical(1)), , drop = FALSE]
    cat("[lb build] team-scoped pulls for", nrow(ncaa), "NCAA/NAIA teams\n")
    pp <- list(); bb <- list()
    for (i in seq_len(nrow(ncaa))) {
      tid <- as.character(ncaa$teamId[i])
      tp <- pull(TM_TRAD_COLS, extra = list(teamId = tid))
      if (!is.null(tp) && nrow(tp)) {
        tr <- if (is.null(rcols)) NULL else pull(rcols, extra = list(teamId = tid))
        tp <- join_rates(tp, tr); tp$teamId <- tid; pp[[tid]] <- tp
      }
      tb <- pull(TM_BAT_TRAD_COLS, extra = list(teamId = tid))
      if (!is.null(tb) && nrow(tb)) {
        br <- if (is.null(bcols)) NULL else pull(bcols, extra = list(teamId = tid))
        tb <- join_rates(tb, br); tb$teamId <- tid; bb[[tid]] <- tb
      }
      if (i %% 25 == 0) cat(sprintf("[lb build]   %d / %d teams\n", i, nrow(ncaa)))
    }
    if (length(pp)) tm_pitching_team <- dplyr::bind_rows(pp)
    if (length(bb)) tm_batting_team  <- dplyr::bind_rows(bb)
    cat("[lb build] team-scoped rows: pitching", NROW(tm_pitching_team), "| batting", NROW(tm_batting_team), "\n")
  }
} else {
  teams <- from_storage("teams"); tm_pitching <- from_storage("tm_pitching_totals"); tm_batting <- from_storage("tm_batting_totals")
  man0 <- storage_read_json(sprintf("lb/%d/manifest.json", season))
  tok <- as.list(man0$tokens$pitching %||% list()); bt <- unlist(man0$tokens$batting %||% list())
  bat_tok <- as.list(bt)
  tm_pitching_team <- tryCatch(from_storage("tm_pitching_team"), error = function(e) NULL)
  tm_batting_team  <- tryCatch(from_storage("tm_batting_team"),  error = function(e) NULL)
  cat("[lb build] TruMedia frames from Storage: teams", nrow(teams), "| pitching", nrow(tm_pitching),
      "| batting", nrow(tm_batting), "| team-scoped", NROW(tm_pitching_team), "/", NROW(tm_batting_team), "\n")
}
# team list with level + conference for the app's pickers
teams <- as.data.frame(teams)
tn <- intersect(c("location", "fullName", "teamName"), names(teams))
tmi <- team_ident(team_id = teams$teamId, name = if (length(tn)) teams[[tn[1]]] else NULL)
teams$level <- tmi$level; teams$conference <- tmi$conf; teams$conf_id <- tmi$conf_id; teams$logo_url <- tmi$logo
teams$ref_level <- ref_team_level(teams$teamId)
cat("[lb build] teams by level:", paste(names(table(teams$level)), table(teams$level), collapse = ", "),
    "| in reference:", sum(!is.na(teams$ref_level)), "of", nrow(teams), "\n")

# =============================================================================
# 2) TrackMan: pipeline aggregates + per-pitch RV and hitter aggregates
# =============================================================================
if (!is.null(pipeline_dir)) {
  ps  <- as.data.frame(read_parquet(file.path(pipeline_dir, "pitcher_season.parquet")))
  pst <- as.data.frame(read_parquet(file.path(pipeline_dir, "pitcher_season_pitch_type.parquet")))
  px  <- open_dataset(file.path(pipeline_dir, "pitches.parquet"))
  pcols <- c("pitcher_id", "batter_id", "batter_name", "batter_team", "pitch_type", "pitch_call", "play_result",
             "plate_loc_side", "plate_loc_height", "exit_speed", "is_bbe", "bbe_tracked", "xwobacon_bbe", "level")
  px <- px %>% select(all_of(pcols)) %>% collect() %>% as.data.frame()
} else {
  ps  <- pp_table("pitcher_season");  pst <- pp_table("pitcher_season_pitch_type")
  px  <- pp_query_pitches("true", cols = c("pitcher_id","batter_id","batter_name","batter_team","pitch_type","pitch_call",
                                            "play_result","plate_loc_side","plate_loc_height","exit_speed","is_bbe",
                                            "bbe_tracked","xwobacon_bbe","level"), context = "all pitches")
  names(px) <- c(pitcher_id = "PitcherId", batter_id = "BatterId", batter_name = "Batter", batter_team = "BatterTeam",
                 pitch_type = "TaggedPitchType", pitch_call = "PitchCall", play_result = "PlayResult",
                 plate_loc_side = "PlateLocSide", plate_loc_height = "PlateLocHeight", exit_speed = "ExitSpeed",
                 is_bbe = "is_bbe", bbe_tracked = "bbe_tracked", xwobacon_bbe = "xwobacon_bbe", level = "Level")[names(px)] %||% names(px)
  names(px) <- c("pitcher_id","batter_id","batter_name","batter_team","pitch_type","pitch_call","play_result",
                 "plate_loc_side","plate_loc_height","exit_speed","is_bbe","bbe_tracked","xwobacon_bbe","level")
}
cat("[lb build] TrackMan: pitcher_season", nrow(ps), "| by pitch type", nrow(pst), "| pitches", nrow(px), "\n")

# per-pitch DRE run value (same currency as the matrix / .lb_pitch_rv)
px$rv <- .lb_pitch_rv(px$pitch_call, px$play_result)
rv_pt <- px %>% filter(!is.na(pitcher_id), nzchar(pitcher_id)) %>%
  group_by(pitcher_id, Pitch = pitch_type) %>% summarise(RV = round(100 * mean(rv, na.rm = TRUE), 2), .groups = "drop")
rv_p  <- px %>% filter(!is.na(pitcher_id), nzchar(pitcher_id)) %>%
  group_by(pitcher_id) %>% summarise(RV = round(100 * mean(rv, na.rm = TRUE), 2), .groups = "drop")

# hitter contact / discipline
inz <- with(px, is.finite(plate_loc_side) & is.finite(plate_loc_height) &
              abs(plate_loc_side) <= LB_ZONE_SIDE & plate_loc_height >= LB_ZONE_LO & plate_loc_height <= LB_ZONE_HI)
sw  <- px$pitch_call %in% LB_SWING_CALLS
whf <- px$pitch_call == "StrikeSwinging"
bip <- px$is_bbe %in% TRUE
ev  <- suppressWarnings(as.numeric(px$exit_speed))
.mode <- function(x) { x <- x[!is.na(x) & nzchar(as.character(x))]; if (!length(x)) NA_character_ else names(sort(table(x), decreasing = TRUE))[1] }
hit_agg <- px %>% mutate(.sw = sw, .whf = whf, .inz = inz, .bip = bip, .ev = ev) %>%
  filter(!is.na(batter_id), nzchar(batter_id)) %>%
  group_by(batter_id) %>%
  summarise(
    Batter = .mode(batter_name), TeamCode = .mode(batter_team), Level = .mode(level),
    Pitches = n(),
    `Swing%`   = .lb_rate(sum(.sw), n()),
    `Z-Swing%` = .lb_rate(sum(.sw & .inz), sum(.inz)),
    `Chase%`   = .lb_rate(sum(.sw & !.inz), sum(!.inz)),
    `Whiff%`   = .lb_rate(sum(.whf), sum(.sw)),
    `Z-Whiff%` = .lb_rate(sum(.whf & .inz), sum(.sw & .inz)),
    `Contact%` = .lb_rate(sum(.sw & !.whf), sum(.sw)),
    BBE = sum(.bip),
    `Avg EV` = round(mean(.ev[.bip & is.finite(.ev)]), 1),
    EV90 = round(as.numeric(quantile(.ev[.bip & is.finite(.ev)], 0.9, na.rm = TRUE)), 1),
    `HardHit%` = .lb_rate(sum(.bip & is.finite(.ev) & .ev >= 95), sum(.bip & is.finite(.ev))),
    xwOBAcon = round(mean(xwobacon_bbe[.bip], na.rm = TRUE), 3),
    .groups = "drop") %>% as.data.frame()
hit_agg$Batter <- ifelse(grepl(",", hit_agg$Batter), normalize_lastfirst(hit_agg$Batter), hit_agg$Batter)
hit_agg$School <- prettify_team(hit_agg$TeamCode)
rm(px); invisible(gc())

# ---- TrackMan code -> TruMedia team id, from this build's own data ------------------
# Players both systems id vote a TrackMan team code onto a TruMedia team id;
# the votes outrank the shipped reference/trackman_teams.csv for this run and
# go to Storage (lb/<season>/team_xwalk.parquet) for the app.
xw_pairs <- bind_rows(
  data.frame(tm_id = idk(ps$pitcher_id), trackman_code = ps$pitcher_team, stringsAsFactors = FALSE),
  data.frame(tm_id = idk(hit_agg$batter_id), trackman_code = hit_agg$TeamCode, stringsAsFactors = FALSE))
xw_ids <- bind_rows(
  if (!is.null(tm_pitching_team)) data.frame(tm_id = idk(tm_pitching_team$trackmanPlayerId), team_id = as.character(tm_pitching_team$teamId)) else NULL,
  if (!is.null(tm_batting_team))  data.frame(tm_id = idk(tm_batting_team$trackmanPlayerId),  team_id = as.character(tm_batting_team$teamId))  else NULL,
  data.frame(tm_id = idk(tm_pitching$trackmanPlayerId), team_id = as.character(tm_pitching$mostRecentTeamId)),
  data.frame(tm_id = idk(tm_batting$trackmanPlayerId),  team_id = as.character(tm_batting$mostRecentTeamId)))
team_xwalk <- ref_team_votes(xw_pairs, xw_ids)
if (!is.null(team_xwalk) && nrow(team_xwalk)) {
  ti0 <- ref_team_info(team_id = team_xwalk$team_id)
  team_xwalk$team <- ti0$team; team_xwalk$level <- ti0$level; team_xwalk$conf <- ti0$conf
  ref_add_team_xwalk(team_xwalk)
}
cat("[lb build] TrackMan team crosswalk:", NROW(team_xwalk), "codes from data votes |",
    length(unique(xw_pairs$trackman_code)), "codes in the pitch data\n")

# ---- pitcher x game-date x pitch-type sums (date-range boards) --------------------
# Every board column is a COUNT or a SUM here; the app divides after it has
# filtered the rows to a date range, so a "Mar 1 - Apr 15" board is exact.
pitcher_day <- NULL
if (!is.null(pipeline_dir)) {
  dcols <- c("pitcher_id", "pitcher_name", "pitcher_team", "pitcher_throws", "league", "level", "game_date", "pitch_type", "session_type",
             "pitch_call", "play_result", "plate_loc_side", "plate_loc_height", "outs_on_play",
             "is_k", "is_bb", "is_hbp", "is_sf", "is_sac_bunt", "is_bbe", "is_ab_bbe", "is_hit", "total_bases",
             "xba_bbe", "xtb_bbe", "xwobacon_bbe", "wobacon_actual",
             "stuff_plus", "pitching_plus", "location_plus", "rel_speed", "spin_rate", "rel_height", "rel_side")
  dd <- open_dataset(file.path(pipeline_dir, "pitches.parquet")) %>% select(all_of(dcols)) %>% collect() %>% as.data.frame()
  dd <- dd[!is.na(idk(dd$pitcher_id)) & !is.na(dd$game_date) & !dd$session_type %in% "BattingPractice", , drop = FALSE]
  lg <- function(x) x %in% TRUE
  inz <- is.finite(dd$plate_loc_side) & is.finite(dd$plate_loc_height) &
    abs(dd$plate_loc_side) <= LB_ZONE_SIDE & dd$plate_loc_height >= LB_ZONE_LO & dd$plate_loc_height <= LB_ZONE_HI
  known <- is.finite(dd$plate_loc_side) & is.finite(dd$plate_loc_height)
  sw <- dd$pitch_call %in% LB_SWING_CALLS
  pr <- as.character(dd$play_result)
  ab <- lg(dd$is_ab_bbe) | lg(dd$is_k)
  fin <- function(x) ifelse(is.finite(x), x, NA_real_)
  pitcher_day <- dd %>%
    mutate(pitcher_id = idk(pitcher_id), pitch_type = ifelse(is.na(pitch_type) | pitch_type %in% c("", "Undefined", "Other"), "Other", pitch_type),
           .inz = inz, .known = known, .sw = sw, .whf = pitch_call == "StrikeSwinging",
           .ab = ab, .k = lg(is_k), .bb = lg(is_bb), .hbp = lg(is_hbp), .sf = lg(is_sf), .sac = lg(is_sac_bunt),
           .bbe = lg(is_bbe), .abbbe = lg(is_ab_bbe), .hit = .abbbe & lg(is_hit), .tb = ifelse(.abbbe, fin(total_bases), 0),
           .d2 = pr == "Double", .d3 = pr == "Triple", .hr = pr %in% c("HomeRun", "Homerun"),
           .xba = ifelse(.abbbe, fin(xba_bbe), 0), .xtb = ifelse(.abbbe, fin(xtb_bbe), 0),
           .xcon = ifelse(.bbe & !.sac, fin(xwobacon_bbe), 0), .acon = ifelse(.bbe & !.sac, fin(wobacon_actual), 0),
           .rv = .lb_pitch_rv(pitch_call, play_result), .outs = fin(outs_on_play),
           .st = fin(stuff_plus), .pi = fin(pitching_plus), .lo = fin(location_plus),
           .ve = fin(rel_speed), .sp = fin(spin_rate), .rh = fin(rel_height), .rs = fin(rel_side)) %>%
    group_by(pitcher_id, game_date, pitcher_team, pitch_type) %>%
    summarise(pitcher_name = first(pitcher_name), pitcher_throws = first(pitcher_throws), league = first(league), level = first(level),
              n = n(), n_scored = sum(is.finite(.st)), swing = sum(.sw), whiff = sum(.whf),
              zone = sum(.inz), ooz = sum(.known & !.inz), chase = sum(.sw & .known & !.inz),
              pa = sum(.ab | .bb | .hbp | .sf | .sac), ab = sum(.ab), k = sum(.k), bb = sum(.bb), hbp = sum(.hbp), sf = sum(.sf),
              bbe = sum(.bbe), hits = sum(.hit), tb = sum(.tb, na.rm = TRUE), d2 = sum(.d2), d3 = sum(.d3), hr = sum(.hr),
              outs = sum(.outs, na.rm = TRUE) + sum(.k),
              xba_num = sum(.xba, na.rm = TRUE), xtb_num = sum(.xtb, na.rm = TRUE), xcon = sum(.xcon, na.rm = TRUE), acon = sum(.acon, na.rm = TRUE),
              stuff_sum = sum(.st, na.rm = TRUE), pitching_sum = sum(.pi, na.rm = TRUE), location_sum = sum(.lo, na.rm = TRUE),
              rv_sum = sum(.rv, na.rm = TRUE), rv_n = sum(is.finite(.rv)),
              velo_sum = sum(.ve, na.rm = TRUE), velo_n = sum(is.finite(.ve)), spin_sum = sum(.sp, na.rm = TRUE), spin_n = sum(is.finite(.sp)),
              relh_sum = sum(.rh, na.rm = TRUE), relh_n = sum(is.finite(.rh)), rels_sum = sum(.rs, na.rm = TRUE), rels_n = sum(is.finite(.rs)),
              .groups = "drop") %>% as.data.frame()
  pitcher_day$game_date <- as.character(pitcher_day$game_date)
  # practice / intrasquad sessions ride along in the master as "games" with
  # no plate-appearance outcomes: a pitcher-day with zero PA is not a game
  pa_day <- pitcher_day %>% group_by(pitcher_id, game_date) %>% summarise(.pa = sum(pa), .groups = "drop")
  pitcher_day <- pitcher_day %>% inner_join(filter(pa_day, .pa > 0), by = c("pitcher_id", "game_date")) %>% select(-.pa)
  # level / conference belong to the TEAM the code names (a D1 arm at a JUCO
  # field is still D1); the TrackMan level tag only backstops unknown codes
  tk_lv <- ps$level[match(pitcher_day$pitcher_id, idk(ps$pitcher_id))]
  pdi <- team_ident(code = pitcher_day$pitcher_team, name = prettify_team(pitcher_day$pitcher_team), tk_level = tk_lv)
  pitcher_day$team_id <- pdi$team_id; pitcher_day$conference <- pdi$conf; pitcher_day$conf_id <- pdi$conf_id
  pitcher_day$level <- pdi$level
  rm(dd); invisible(gc())
  cat("[lb build] pitcher_day:", nrow(pitcher_day), "pitcher x date x pitch-type rows\n")
} else cat("[lb build] pitcher_day skipped (needs --pipeline-dir)\n")

# ---- TrackMan pitcher frame in board vocabulary -------------------------------
hand1 <- function(x) { h <- toupper(substr(as.character(x), 1, 1)); h[!h %in% c("L", "R")] <- NA_character_; h }
MODEL_COLS <- c("Stuff+", "Pitching+", "Location+", "RV", "xBA", "xSLG", "xwOBA")
tk_pt <- pst %>% filter(!is.na(pitch_type), !pitch_type %in% c("Undefined", "Other", ""), !is.na(idk(pitcher_id))) %>%
  transmute(pitcher_id = idk(pitcher_id), Pitch = pitch_type, P = pitches, n_scored = pitches_scored,
            `Whiff%` = round(100 * whiff_pct, 1), `Chase%` = round(100 * chase_pct, 1), `Zone%` = round(100 * zone_pct, 1),
            Velo = round(velo_avg, 1), Spin = round(spin_avg, 0), relh = rel_height_avg, rels = rel_side_avg,
            `Stuff+` = round(stuff_plus, 0), `Pitching+` = round(pitching_plus, 0), `Location+` = round(location_plus, 0),
            xBA = round(xba, 3), xSLG = round(xslg, 3), xwOBA = round(xwoba, 3),
            T = hand1(pitcher_throws), TkName = ifelse(grepl(",", pitcher_name), normalize_lastfirst(pitcher_name), pitcher_name),
            TkSchool = prettify_team(pitcher_team), Level = level) %>%
  left_join(mutate(rv_pt, pitcher_id = as.character(pitcher_id)), by = c("pitcher_id", "Pitch"))
tk_p <- ps %>% filter(!is.na(idk(pitcher_id))) %>% transmute(pitcher_id = idk(pitcher_id), n_scored = pitches_scored, TkName = ifelse(grepl(",", pitcher_name), normalize_lastfirst(pitcher_name), pitcher_name),
                         TkSchool = prettify_team(pitcher_team), TkCode = pitcher_team, Level = level, T = hand1(pitcher_throws), P = pitches,
                         `Whiff%` = round(100 * whiff_pct, 1), `Chase%` = round(100 * chase_pct, 1), `Zone%` = round(100 * zone_pct, 1),
                         relh = rel_height_avg, rels = rel_side_avg, `Rel Ht` = round(rel_height_avg, 2), `Rel Sd` = round(rel_side_avg, 2),
                         `Stuff+` = round(stuff_plus, 0), `Pitching+` = round(pitching_plus, 0), `Location+` = round(location_plus, 0),
                         xBA = round(xba, 3), xSLG = round(xslg, 3), xwOBA = round(xwoba, 3)) %>%
  left_join(mutate(rv_p, pitcher_id = as.character(pitcher_id)), by = "pitcher_id")
# fastball velo/spin + arsenal, usage-weighted from the pitch-type rows
ru <- tk_pt %>% group_by(pitcher_id) %>% summarise(
  `FB Velo` = { w <- P * (Pitch %in% LB_FB_TYPES); if (sum(w) > 0) round(sum(Velo * w, na.rm = TRUE) / sum(w[is.finite(Velo)]), 1) else NA_real_ },
  `FB Spin` = { w <- P * (Pitch %in% LB_FB_TYPES); if (sum(w) > 0) round(sum(Spin * w, na.rm = TRUE) / sum(w[is.finite(Spin)]), 0) else NA_real_ },
  Arsenal = paste(Pitch[order(-P)], collapse = ", "), .groups = "drop")
tk_p <- left_join(tk_p, ru, by = "pitcher_id")
tk_p <- tk_p[order(-tk_p$P), , drop = FALSE]; tk_p <- tk_p[!duplicated(tk_p$pitcher_id), , drop = FALSE]
# model / expected-stat columns only on a real sample (same floor the app's
# grading distributions use); velo, release and rates stay
for (cc in MODEL_COLS) { tk_p[[cc]][!is.finite(tk_p$n_scored) | tk_p$n_scored < 100] <- NA; tk_pt[[cc]][!is.finite(tk_pt$n_scored) | tk_pt$n_scored < 25] <- NA }

# =============================================================================
# 3) Boards
# =============================================================================
# TruMedia pitcher lines (the lb_tm_league_line recipe, keeping the ids)
# team-scoped rows (one per player x NCAA team, that team's games only) when
# the build had them; the league-wide calendar-year frame otherwise
team_name_of <- function(ids) {
  nc <- intersect(c("location", "fullName", "teamName"), names(teams))
  as.character(teams[[nc[1]]])[match(as.character(ids), as.character(teams$teamId))]
}
tm_src <- if (!is.null(tm_pitching_team) && nrow(tm_pitching_team)) tm_pitching_team else tm_pitching
scoped <- "teamId" %in% names(tm_src)
nm <- .lb_chr(tm_src, c("^player$", "playerName", "^name$", "fullName"))
d <- data.frame(Pitcher = nm,
                School = if (scoped) team_name_of(tm_src$teamId) else .lb_chr(tm_src, c("mostRecentTeamName", "teamName", "^team$")) %||% NA_character_,
                tm_id = idk(tm_src$trackmanPlayerId),
                tm_team_id = as.character(if (scoped) tm_src$teamId else tm_src$mostRecentTeamId),
                tm_player_id = if ("playerId" %in% names(tm_src)) idk(tm_src$playerId) else NA_character_,
                stringsAsFactors = FALSE)
grab <- function(w) { v <- .lb_col(tm_src, w); if (is.null(v)) rep(NA_real_, nrow(d)) else v }
d$W <- grab("W"); d$L <- grab("L"); d$S <- grab("SV"); d$G <- grab("G"); d$IP <- tm_ip_decimal(grab("IP")); d$ERA <- round(grab("ERA"), 2)
d$T <- .lb_hand_from_payload(tm_src, nrow(d))
d <- .lb_join_rates(d, tm_src, tok)
cat("[lb build] pitcher lines:", if (scoped) "team-scoped" else "league-wide (calendar year)", "\n")
d <- d[is.finite(d$IP) & d$IP > 0 & !is.na(d$Pitcher) & nzchar(d$Pitcher), , drop = FALSE]
cat("[lb build] TruMedia pitchers with IP:", nrow(d), "\n")

# TrackMan columns by id; TrackMan-derived Whiff/Chase/Zone/P replace the
# token-derived ones where present (same definitions, no API dependency)
tk_cols <- c("P", "Whiff%", "Chase%", "Zone%", "relh", "rels", "Rel Ht", "Rel Sd", "Stuff+", "Pitching+", "Location+",
             "xBA", "xSLG", "xwOBA", "RV", "FB Velo", "FB Spin", "Arsenal")
i <- match(d$tm_id, tk_p$pitcher_id)
cat("[lb build] TruMedia pitchers matched to TrackMan by id:", sum(!is.na(i)), "of", nrow(d), "\n")
for (cc in tk_cols) { v <- tk_p[[cc]][i]; if (!cc %in% names(d)) d[[cc]] <- NA; d[[cc]] <- ifelse(is.na(i), d[[cc]], v) }
d$T <- ifelse(is.na(d$T), tk_p$T[i], d$T)
# TrackMan D1 arms with no TruMedia line (no traditional stats, but graded)
extra <- tk_p[!tk_p$pitcher_id %in% d$tm_id & tk_p$Level %in% "D1" & tk_p$P >= 50, , drop = FALSE]
if (nrow(extra)) {
  e <- data.frame(Pitcher = extra$TkName, School = extra$TkSchool, tm_id = extra$pitcher_id, tm_team_id = NA_character_,
                  tm_player_id = NA_character_, stringsAsFactors = FALSE)
  for (cc in c("W","L","S","G","IP","ERA","PA","K%","BB%","wOBA",".hr",".bb",".k",".hbp","Velo","Spin")) e[[cc]] <- NA_real_
  e$T <- extra$T
  for (cc in tk_cols) e[[cc]] <- extra[[cc]]
  d <- dplyr::bind_rows(d, e)
  cat("[lb build] TrackMan-only D1 arms appended:", nrow(extra), "\n")
}
# School: the TrackMan (spring, NCAA) team for every arm TrackMan has; the
# league-wide TruMedia frame's "most recent team" is the summer club for
# anyone who played summer ball. Level / conference belong to that team.
ti <- match(d$tm_id, tk_p$pitcher_id)
if (!scoped) d$School <- ifelse(is.na(ti), d$School, tk_p$TkSchool[ti])
# identity by id: team-scoped rows carry the NCAA team id; on the calendar
# frame the most-recent team may be a summer club, so a known TrackMan code
# outranks it there. Level / Conf / Logo come from the reference.
code <- tk_p$TkCode[ti]
tid <- d$tm_team_id
if (!scoped) tid <- ifelse(is.na(ref_team_id_for_code(code)), tid, NA_character_)
di <- team_ident(team_id = tid, code = code, name = d$School, tk_level = tk_p$Level[ti])
d$team_id <- di$team_id; d$Level <- di$level; d$Conf <- di$conf; d$conf_id <- di$conf_id; d$Logo <- di$logo
cat("[lb build] pitchers with a reference team:", sum(!is.na(d$team_id)), "of", nrow(d),
    "| by level:", paste(names(table(d$Level)), table(d$Level), collapse = ", "), "\n")
pitchers <- .lb_attach_bio(d)
pitchers <- .lb_add_fip_rv(pitchers)
pitchers$`Arm Ang` <- .lb_arm_angle(pitchers$relh %||% NA_real_, pitchers$rels %||% NA_real_, pitchers$.ht)
for (cc in lb_cols_for("pitchers")) if (!cc %in% names(pitchers)) pitchers[[cc]] <- NA
pitchers <- pitchers[order(-ifelse(is.finite(pitchers$IP), pitchers$IP, 0)), , drop = FALSE]

# pitch-type board: TrackMan rows, named/teamed like the pitchers board
j <- match(tk_pt$pitcher_id, pitchers$tm_id)
pitches <- tk_pt
pitches$Pitcher <- ifelse(is.na(j), tk_pt$TkName, pitchers$Pitcher[j])
pitches$School  <- ifelse(is.na(j), tk_pt$TkSchool, pitchers$School[j])
pitches$tm_id <- tk_pt$pitcher_id; pitches$tm_team_id <- pitchers$tm_team_id[j]
pitches <- pitches[!is.na(j) | (pitches$Level %in% "D1" & pitches$P >= 25), , drop = FALSE]
pitches <- .lb_attach_bio(pitches)
pitches$`Arm Ang` <- .lb_arm_angle(pitches$relh, pitches$rels, pitches$.ht)
pitches$`Rel Ht` <- round(pitches$relh, 2); pitches$`Rel Sd` <- round(pitches$rels, 2)
car <- intersect(c("W","L","S","G","IP","K%","BB%","wOBA","ERA","FIP","Age","Class","Logo","Conf","conf_id","team_id","tm_player_id"), names(pitchers))
k <- match(pitches$tm_id, pitchers$tm_id)
pitches$Level <- ifelse(is.na(k), ifelse(pitches$Level %in% LB_LEVELS, pitches$Level, "Other"), pitchers$Level[k])
for (cc in car) pitches[[cc]] <- pitchers[[cc]][k]
for (cc in lb_cols_for("pitches")) if (!cc %in% names(pitches)) pitches[[cc]] <- NA
pitches <- pitches[order(-ifelse(is.finite(pitches$P), pitches$P, 0)), , drop = FALSE]
cat("[lb build] boards: pitchers", nrow(pitchers), "| pitches", nrow(pitches), "\n")

# hitters: TruMedia line + TrackMan contact/discipline by id
tb_src <- if (!is.null(tm_batting_team) && nrow(tm_batting_team)) tm_batting_team else tm_batting
bscoped <- "teamId" %in% names(tb_src)
bn <- .lb_chr(tb_src, c("^player$", "playerName", "^name$", "fullName"))
h <- data.frame(Batter = bn,
                School = if (bscoped) team_name_of(tb_src$teamId) else .lb_chr(tb_src, c("mostRecentTeamName", "teamName", "^team$")) %||% NA_character_,
                tm_id = idk(tb_src$trackmanPlayerId),
                tm_team_id = as.character(if (bscoped) tb_src$teamId else tb_src$mostRecentTeamId),
                tm_player_id = if ("playerId" %in% names(tb_src)) idk(tb_src$playerId) else NA_character_,
                stringsAsFactors = FALSE)
gb <- function(w) { v <- .lb_col(tb_src, w); if (is.null(v)) rep(NA_real_, nrow(h)) else v }
for (cc in c("G","PA","AB","H","2B","3B","HR","BB","HBP","K","SB","BA","OBP","SLG")) h[[cc]] <- gb(cc)
keep_h <- is.finite(h$PA) & h$PA > 0 & !is.na(h$Batter) & nzchar(h$Batter)
h <- h[keep_h, , drop = FALSE]; bi <- which(keep_h)
tmtok <- function(slot) { t <- bat_tok[[slot]]; if (is.null(t)) rep(NA_real_, nrow(h)) else { v <- .lb_col(tb_src, sub("\\|.*$", "", t)); if (is.null(v)) rep(NA_real_, nrow(h)) else v[bi] } }
h$`TM Swing%` <- .lb_rate(tmtok("swing"), gb("PA")[bi])
h$`TM Contact%` <- .lb_rate(tmtok("contact"), tmtok("swing")); h$`TM Whiff%` <- .lb_rate(tmtok("miss"), tmtok("swing"))
h$`TM Chase%` <- .lb_rate(tmtok("chase"), tmtok("ooz")); h$`TM xwOBA` <- round(tmtok("xwoba"), 3)
hit_agg$batter_id <- idk(hit_agg$batter_id); hit_agg <- hit_agg[!is.na(hit_agg$batter_id), , drop = FALSE]
hi <- match(h$tm_id, hit_agg$batter_id)
cat("[lb build] TruMedia hitters with PA:", nrow(h), "| matched to TrackMan by id:", sum(!is.na(hi)), "\n")
for (cc in setdiff(names(hit_agg), c("batter_id", "Batter", "TeamCode", "School", "Level"))) h[[cc]] <- hit_agg[[cc]][hi]
hx <- hit_agg[!hit_agg$batter_id %in% h$tm_id & hit_agg$Level %in% "D1" & hit_agg$Pitches >= 100, , drop = FALSE]
if (nrow(hx)) {
  e <- data.frame(Batter = hx$Batter, School = hx$School, tm_id = as.character(hx$batter_id), tm_team_id = NA_character_,
                  tm_player_id = NA_character_, stringsAsFactors = FALSE)
  for (cc in setdiff(names(h), names(e))) e[[cc]] <- if (cc %in% names(hx)) hx[[cc]] else NA
  h <- dplyr::bind_rows(h, e[, names(h)])
}
hi2 <- match(h$tm_id, hit_agg$batter_id)
if (!bscoped) h$School <- ifelse(is.na(hi2), h$School, hit_agg$School[hi2])
hcode <- hit_agg$TeamCode[hi2]
htid <- h$tm_team_id
if (!bscoped) htid <- ifelse(is.na(ref_team_id_for_code(hcode)), htid, NA_character_)
hdi <- team_ident(team_id = htid, code = hcode, name = h$School, tk_level = hit_agg$Level[hi2])
h$team_id <- hdi$team_id; h$Level <- hdi$level; h$Conf <- hdi$conf; h$conf_id <- hdi$conf_id; h$Logo <- hdi$logo
h <- .lb_attach_bio(h)
cat("[lb build] hitters with a reference team:", sum(!is.na(h$team_id)), "of", nrow(h),
    "| by level:", paste(names(table(h$Level)), table(h$Level), collapse = ", "), "\n")
hitters <- h[order(-ifelse(is.finite(h$PA), h$PA, 0)), , drop = FALSE]

# ---- identity: who is who, by id, per season type and level ----------------------------
# One row per player x team x season type from every frame that carries ids,
# with the collisions flagged (a TrackMan id under two TruMedia ids, a name
# shared by two ids at the same level). The app resolves player pages from
# this instead of names.
identity <- ref_identity_table(
  list(tm_pitching_team = tm_pitching_team, tm_batting_team = tm_batting_team,
       tm_pitching_totals = tm_pitching, tm_batting_totals = tm_batting),
  trackman = bind_rows(
    data.frame(kind = "pit", tm_id = idk(ps$pitcher_id), name = ifelse(grepl(",", ps$pitcher_name), normalize_lastfirst(ps$pitcher_name), ps$pitcher_name),
               trackman_code = ps$pitcher_team, tk_level = ps$level, stringsAsFactors = FALSE),
    data.frame(kind = "bat", tm_id = idk(hit_agg$batter_id), name = hit_agg$Batter, trackman_code = hit_agg$TeamCode,
               tk_level = hit_agg$Level, stringsAsFactors = FALSE)),
  season = season)
ref_identity_report(identity)

# =============================================================================
# 4) Upload
# =============================================================================
out_dir <- arg_val("--out-dir", file.path("serving_build", sprintf("lb_%d", season)))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
up <- function(df, name) {
  tmp <- file.path(out_dir, paste0(name, ".parquet")); arrow::write_parquet(df, tmp, compression = "zstd")
  path <- sprintf("lb/%d/%s.parquet", season, name); sb_storage_upload(tmp, path)
  cat(sprintf("[lb build] uploaded %s: %d rows x %d cols, %.1f MB\n", path, nrow(df), ncol(df), file.size(tmp) / 1e6))
}
up(as.data.frame(teams), "teams"); up(as.data.frame(tm_pitching), "tm_pitching_totals"); up(as.data.frame(tm_batting), "tm_batting_totals")
up(pitchers, "pitchers"); up(pitches, "pitches"); up(hitters, "hitters")
up(identity, "identity")
if (!is.null(team_xwalk) && nrow(team_xwalk)) up(team_xwalk, "team_xwalk")
if (!is.null(pitcher_day)) up(pitcher_day, "pitcher_day")
if (!is.null(tm_pitching_team)) up(as.data.frame(tm_pitching_team), "tm_pitching_team")
if (!is.null(tm_batting_team))  up(as.data.frame(tm_batting_team),  "tm_batting_team")
man <- list(season = season, built_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
            team_scoped = !is.null(tm_pitching_team),
            rows = list(teams = nrow(teams), tm_pitching_totals = nrow(tm_pitching), tm_batting_totals = nrow(tm_batting),
                        tm_pitching_team = NROW(tm_pitching_team), tm_batting_team = NROW(tm_batting_team),
                        pitchers = nrow(pitchers), pitches = nrow(pitches), hitters = nrow(hitters),
                        pitcher_day = NROW(pitcher_day), identity = nrow(identity), team_xwalk = NROW(team_xwalk)),
            tokens = list(pitching = tok[!vapply(tok, is.null, logical(1))], batting = as.list(bt)))
tmp <- tempfile(fileext = ".json"); writeLines(jsonlite::toJSON(man, auto_unbox = TRUE, pretty = TRUE), tmp)
sb_storage_upload(tmp, sprintf("lb/%d/manifest.json", season), content_type = "application/json"); unlink(tmp)
cat("[lb build] done in", round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), "min\n")
