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
# pitches (board, by pitch type), hitters, manifest.json.
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
if (!tm_enabled()) stop("set TM_USERNAME / TM_SITENAME / TM_MASTER_TOKEN")
if (!sb_storage_enabled()) stop("set SUPABASE_URL / SUPABASE_SECRET_KEY")
.tm_env$season_override <- season
cat("[lb build] season", season, "\n")

# =============================================================================
# 1) TruMedia: league-wide raw frames
# =============================================================================
pull <- function(cols, rq = "") {
  p <- list(seasonYear = season, format = "RAW")
  if (!is.null(cols)) p$columns <- cols
  tm_get_csv("PlayerTotals", params = p, raw_query = rq)
}
id_cols <- c("playerId", "trackmanPlayerId", "abbrevName", "fullName", "mostRecentTeamId", "mostRecentTeamName")

teams <- tm_all_teams()
if (is.null(teams) || nrow(teams) == 0) stop("AllTeams returned nothing")
cat("[lb build] teams:", nrow(teams), "\n")

trad <- pull(TM_TRAD_COLS)
if (is.null(trad) || nrow(trad) == 0) trad <- pull(TM_TRAD_COLS_MIN)
if (is.null(trad) || nrow(trad) == 0) stop("PlayerTotals pitching returned nothing")
tok <- .lb_probe_tokens(NULL)
rcols <- .lb_rate_cols(tok)
rates <- if (is.null(rcols)) NULL else pull(rcols)
tm_pitching <- if (!is.null(rates) && "playerId" %in% names(rates)) {
  extra <- rates[, setdiff(names(rates), setdiff(names(trad), "playerId")), drop = FALSE]
  dplyr::left_join(trad, extra, by = "playerId")
} else trad
tm_pitching <- dedupe_players(tm_pitching, "IP")
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
bat_rates <- if (length(bt)) pull(paste0("[", unique(c("PA", bt)), "]", collapse = ",")) else NULL
tm_batting <- if (!is.null(bat_rates) && "playerId" %in% names(bat_rates)) {
  extra <- bat_rates[, setdiff(names(bat_rates), setdiff(names(bat), "playerId")), drop = FALSE]
  dplyr::left_join(bat, extra, by = "playerId")
} else bat
tm_batting <- dedupe_players(tm_batting, "PA")
cat("[lb build] batting totals:", nrow(tm_batting), "rows x", ncol(tm_batting), "cols\n")

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
                         TkSchool = prettify_team(pitcher_team), Level = level, T = hand1(pitcher_throws), P = pitches,
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
nm <- .lb_chr(tm_pitching, c("^player$", "playerName", "^name$", "fullName"))
d <- data.frame(Pitcher = nm, School = .lb_chr(tm_pitching, c("mostRecentTeamName", "teamName", "^team$")) %||% NA_character_,
                tm_id = idk(tm_pitching$trackmanPlayerId), tm_team_id = as.character(tm_pitching$mostRecentTeamId),
                stringsAsFactors = FALSE)
grab <- function(w) { v <- .lb_col(tm_pitching, w); if (is.null(v)) rep(NA_real_, nrow(d)) else v }
d$W <- grab("W"); d$L <- grab("L"); d$S <- grab("SV"); d$G <- grab("G"); d$IP <- tm_ip_decimal(grab("IP")); d$ERA <- round(grab("ERA"), 2)
d$T <- .lb_hand_from_payload(tm_pitching, nrow(d))
d <- .lb_join_rates(d, tm_pitching, tok)
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
  e <- data.frame(Pitcher = extra$TkName, School = extra$TkSchool, tm_id = extra$pitcher_id, tm_team_id = NA_character_, stringsAsFactors = FALSE)
  for (cc in c("W","L","S","G","IP","ERA","PA","K%","BB%","wOBA",".hr",".bb",".k",".hbp","Velo","Spin")) e[[cc]] <- NA_real_
  e$T <- extra$T
  for (cc in tk_cols) e[[cc]] <- extra[[cc]]
  d <- dplyr::bind_rows(d, e)
  cat("[lb build] TrackMan-only D1 arms appended:", nrow(extra), "\n")
}
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
car <- intersect(c("W","L","S","G","IP","K%","BB%","wOBA","ERA","FIP","Age","Class","Logo"), names(pitchers))
k <- match(pitches$tm_id, pitchers$tm_id)
for (cc in car) pitches[[cc]] <- pitchers[[cc]][k]
for (cc in lb_cols_for("pitches")) if (!cc %in% names(pitches)) pitches[[cc]] <- NA
pitches <- pitches[order(-ifelse(is.finite(pitches$P), pitches$P, 0)), , drop = FALSE]
cat("[lb build] boards: pitchers", nrow(pitchers), "| pitches", nrow(pitches), "\n")

# hitters: TruMedia line + TrackMan contact/discipline by id
bn <- .lb_chr(tm_batting, c("^player$", "playerName", "^name$", "fullName"))
h <- data.frame(Batter = bn, School = .lb_chr(tm_batting, c("mostRecentTeamName", "teamName", "^team$")) %||% NA_character_,
                tm_id = idk(tm_batting$trackmanPlayerId), tm_team_id = as.character(tm_batting$mostRecentTeamId),
                stringsAsFactors = FALSE)
gb <- function(w) { v <- .lb_col(tm_batting, w); if (is.null(v)) rep(NA_real_, nrow(h)) else v }
for (cc in c("G","PA","AB","H","2B","3B","HR","BB","HBP","K","SB","BA","OBP","SLG")) h[[cc]] <- gb(cc)
h <- h[is.finite(h$PA) & h$PA > 0 & !is.na(h$Batter) & nzchar(h$Batter), , drop = FALSE]
bi <- match(h$tm_id, idk(tm_batting$trackmanPlayerId))
tmtok <- function(slot) { t <- bat_tok[[slot]]; if (is.null(t)) rep(NA_real_, nrow(h)) else { v <- .lb_col(tm_batting, sub("\\|.*$", "", t)); if (is.null(v)) rep(NA_real_, nrow(h)) else v[bi] } }
h$`TM Swing%` <- .lb_rate(tmtok("swing"), gb("PA")[bi])
h$`TM Contact%` <- .lb_rate(tmtok("contact"), tmtok("swing")); h$`TM Whiff%` <- .lb_rate(tmtok("miss"), tmtok("swing"))
h$`TM Chase%` <- .lb_rate(tmtok("chase"), tmtok("ooz")); h$`TM xwOBA` <- round(tmtok("xwoba"), 3)
hit_agg$batter_id <- idk(hit_agg$batter_id); hit_agg <- hit_agg[!is.na(hit_agg$batter_id), , drop = FALSE]
hi <- match(h$tm_id, hit_agg$batter_id)
cat("[lb build] TruMedia hitters with PA:", nrow(h), "| matched to TrackMan by id:", sum(!is.na(hi)), "\n")
for (cc in setdiff(names(hit_agg), c("batter_id", "Batter", "TeamCode", "School", "Level"))) h[[cc]] <- hit_agg[[cc]][hi]
hx <- hit_agg[!hit_agg$batter_id %in% h$tm_id & hit_agg$Level %in% "D1" & hit_agg$Pitches >= 100, , drop = FALSE]
if (nrow(hx)) {
  e <- data.frame(Batter = hx$Batter, School = hx$School, tm_id = as.character(hx$batter_id), tm_team_id = NA_character_, stringsAsFactors = FALSE)
  for (cc in setdiff(names(h), names(e))) e[[cc]] <- if (cc %in% names(hx)) hx[[cc]] else NA
  h <- dplyr::bind_rows(h, e[, names(h)])
}
hitters <- h[order(-ifelse(is.finite(h$PA), h$PA, 0)), , drop = FALSE]

# =============================================================================
# 4) Upload
# =============================================================================
up <- function(df, name) {
  tmp <- tempfile(fileext = ".parquet"); arrow::write_parquet(df, tmp, compression = "zstd")
  path <- sprintf("lb/%d/%s.parquet", season, name); sb_storage_upload(tmp, path)
  cat(sprintf("[lb build] uploaded %s: %d rows x %d cols, %.1f MB\n", path, nrow(df), ncol(df), file.size(tmp) / 1e6)); unlink(tmp)
}
up(as.data.frame(teams), "teams"); up(as.data.frame(tm_pitching), "tm_pitching_totals"); up(as.data.frame(tm_batting), "tm_batting_totals")
up(pitchers, "pitchers"); up(pitches, "pitches"); up(hitters, "hitters")
man <- list(season = season, built_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
            rows = list(teams = nrow(teams), tm_pitching_totals = nrow(tm_pitching), tm_batting_totals = nrow(tm_batting),
                        pitchers = nrow(pitchers), pitches = nrow(pitches), hitters = nrow(hitters)),
            tokens = list(pitching = tok[!vapply(tok, is.null, logical(1))], batting = as.list(bt)))
tmp <- tempfile(fileext = ".json"); writeLines(jsonlite::toJSON(man, auto_unbox = TRUE, pretty = TRUE), tmp)
sb_storage_upload(tmp, sprintf("lb/%d/manifest.json", season), content_type = "application/json"); unlink(tmp)
cat("[lb build] done in", round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), "min\n")
