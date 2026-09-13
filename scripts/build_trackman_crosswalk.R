#!/usr/bin/env Rscript
# =============================================================================
# reference/trackman_teams.csv: TrackMan team code -> TruMedia team id.
#
#   Rscript scripts/build_trackman_crosswalk.R [--lb-dir serving_build/lb_2026]
#
# Sources, strongest first:
#   votes     players both systems id (TrackMan id on the TruMedia line):
#             pitcher_day / pitchers / hitters / tm_*_team frames from the
#             leaderboard build (--lb-dir, default serving_build/lb_<season>;
#             Storage lb/<season>/ when SUPABASE_URL is set and no dir exists)
#   bio       trumedia_player_bio_master.csv verified trackman_abbr -> team_id
#   name      merged_teams.csv trackman_abbr -> team name -> reference team
# scripts/build_leaderboards.R regenerates the votes on every run and ships
# them to Storage as lb/<season>/team_xwalk.parquet; this file is the copy
# the app boots with.
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr); library(arrow) })
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
args <- commandArgs(trailingOnly = TRUE)
arg_val <- function(flag, default = NULL) if (flag %in% args) args[which(args == flag) + 1] else default
season <- as.integer(arg_val("--season", "2026"))
lb_dir <- arg_val("--lb-dir", file.path("serving_build", sprintf("lb_%d", season)))
source("R/01_reference.R")

rd <- function(name) {
  f <- file.path(lb_dir, paste0(name, ".parquet"))
  if (file.exists(f)) return(as.data.frame(read_parquet(f)))
  if (nzchar(Sys.getenv("SUPABASE_URL")) && exists("storage_read_parquet", mode = "function"))
    return(tryCatch(storage_read_parquet(sprintf("lb/%d/%s.parquet", season, name)), error = function(e) NULL))
  NULL
}
if (!dir.exists(lb_dir) && nzchar(Sys.getenv("SUPABASE_URL"))) { source("R/palace_supabase.R"); source("R/palace_artifacts.R") }

# ---- 1) votes ------------------------------------------------------------------
pairs <- list(); ids <- list()
pd <- rd("pitcher_day")
if (!is.null(pd)) pairs$pd <- unique(data.frame(tm_id = pd$pitcher_id, trackman_code = pd$pitcher_team))
for (nm in c("pitchers", "pitches")) {
  b <- rd(nm)
  if (!is.null(b) && all(c("tm_id", "tm_team_id") %in% names(b))) ids[[nm]] <- data.frame(tm_id = b$tm_id, team_id = b$tm_team_id)
  if (!is.null(b) && all(c("tm_id", "TkCode") %in% names(b))) pairs[[nm]] <- data.frame(tm_id = b$tm_id, trackman_code = b$TkCode)
}
h <- rd("hitters")
if (!is.null(h) && all(c("tm_id", "tm_team_id") %in% names(h))) ids$h <- data.frame(tm_id = h$tm_id, team_id = h$tm_team_id)
if (!is.null(h) && all(c("tm_id", "TeamCode") %in% names(h))) pairs$h <- data.frame(tm_id = h$tm_id, trackman_code = h$TeamCode)
for (nm in c("tm_pitching_team", "tm_batting_team")) {
  f <- rd(nm)
  if (!is.null(f) && all(c("trackmanPlayerId", "teamId") %in% names(f))) ids[[nm]] <- data.frame(tm_id = f$trackmanPlayerId, team_id = f$teamId)
}
votes <- ref_team_votes(bind_rows(pairs), bind_rows(ids))
cat("[xwalk] votes:", NROW(votes), "codes from", NROW(bind_rows(pairs)), "pairs x", NROW(bind_rows(ids)), "ids\n")

# ---- 2) bio master verified codes ------------------------------------------------
bio <- tryCatch(read_csv("trumedia_player_bio_master.csv", show_col_types = FALSE, col_types = cols(.default = "c")), error = function(e) NULL)
bio_x <- NULL
if (!is.null(bio) && all(c("trackman_abbr", "team_id") %in% names(bio))) {
  b <- bio[!is.na(bio$trackman_abbr) & nzchar(bio$trackman_abbr) & (bio$trackman_abbr_confidence %||% "verified") == "verified", , drop = FALSE]
  bio_x <- b %>% count(trackman_code = toupper(trimws(trackman_abbr)), team_id = .ref_id(team_id), name = "n") %>%
    group_by(trackman_code) %>% mutate(share = n / sum(n)) %>% arrange(desc(n), .by_group = TRUE) %>% slice(1) %>% ungroup() %>%
    filter(share >= 0.6, !is.na(team_id)) %>% mutate(source = "bio", share = round(share, 3)) %>% as.data.frame()
}
cat("[xwalk] bio master:", NROW(bio_x), "codes\n")

# ---- 3) merged_teams name matches -------------------------------------------------
mt <- read_csv("reference/merged_teams.csv", show_col_types = FALSE, col_types = cols(.default = "c"))
mt <- mt[!is.na(mt$trackman_abbr) & nzchar(mt$trackman_abbr), , drop = FALSE]
lvl <- toupper(trimws(sub("^NCAA\\s*", "", mt$source %||% NA)))
lvl[!lvl %in% REF_LEVELS] <- NA_character_
name_x <- data.frame(trackman_code = toupper(trimws(mt$trackman_abbr)),
                     team_id = ref_team_id_for_name(mt$team_name, lvl), stringsAsFactors = FALSE)
name_x <- name_x[!is.na(name_x$team_id), , drop = FALSE]
name_x$n <- NA_integer_; name_x$share <- NA_real_; name_x$source <- "name"
cat("[xwalk] merged_teams names:", nrow(name_x), "of", nrow(mt), "codes\n")

out <- bind_rows(votes, bio_x, name_x)
out <- out[!duplicated(out$trackman_code), , drop = FALSE]
# a vote that disagrees with the verified bio code is worth flagging
if (!is.null(votes) && !is.null(bio_x)) {
  cmp <- merge(votes, bio_x, by = "trackman_code", suffixes = c(".votes", ".bio"))
  dis <- cmp[cmp$team_id.votes != cmp$team_id.bio, , drop = FALSE]
  if (nrow(dis)) { cat("[xwalk] votes vs bio disagree on", nrow(dis), "codes (votes kept):\n"); print(head(dis[, c("trackman_code", "team_id.votes", "n", "team_id.bio")], 20)) }
}
ti <- ref_team_info(team_id = out$team_id)
out$team <- ti$team; out$level <- ti$level; out$conf <- ti$conf
out <- out[order(out$trackman_code), c("trackman_code", "team_id", "team", "level", "conf", "source", "n", "share")]
write_csv(out, "reference/trackman_teams.csv", na = "")
cat("[xwalk] wrote reference/trackman_teams.csv:", nrow(out), "codes |", paste(names(table(out$source)), table(out$source), collapse = ", "),
    "| by level:", paste(names(table(out$level)), table(out$level), collapse = ", "), "\n")
