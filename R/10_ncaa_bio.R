# DRS26.csv — bio info for NCAA pitcher headers
# ============================================================
norm_player_key <- function(x) {
  x <- as.character(x)
  x <- ifelse(grepl(",", x), sub("^\\s*([^,]+),\\s*(.+)\\s*$", "\\2 \\1", x), x)
  # TrackMan vs TruMedia spelling drift: transliterate accents/okinas
  # ("Pe\u00f1a" -> "Pena", "Nu\u2019u" -> "Nuu") so either spelling keys the same,
  # then drop generational suffixes ("Scott Campbell Jr." == "Scott Campbell").
  # stringi is locale-proof (iconv TRANSLIT garbles in C locales); it is a
  # hard dep of stringr, which this app already loads.
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
  } else {
    tx <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    x  <- ifelse(is.na(tx), x, tx)
  }
  x <- tolower(trimws(gsub("\\s+", " ", x)))
  x <- sub("\\s+(jr|sr|ii|iii|iv)\\.?$", "", x)
  trimws(gsub("[^a-z ]", "", x))
}

parse_drs_height <- function(h) {
  h <- as.character(h)
  ft   <- suppressWarnings(as.numeric(sub("^\\s*(\\d+).*$", "\\1", h)))
  inch <- suppressWarnings(as.numeric(sub("^\\s*\\d+\\D+(\\d+).*$", "\\1", h)))
  inch[is.na(inch)] <- 0
  ifelse(is.na(ft), NA_real_, ft * 12 + inch)
}

drs_bio <- tryCatch({
  cand <- palace_file_candidates("DRS26.csv")
  if (length(cand) == 0) stop("DRS26.csv not present in container")
  cat("DRS bio source:", normalizePath(cand[1]), "\n")
  d <- readr::read_csv(cand[1], show_col_types = FALSE)
  d %>%
    mutate(
      team_name = sub("\\|.*$", "", Team),
      team_logo = ifelse(grepl("\\|", Team), sub("^[^|]*\\|", "", Team), NA_character_),
      bio_key   = norm_player_key(Player),
      height_in = parse_drs_height(Height)
    )
}, error = function(e) {
  cat("WARNING: DRS26.csv not found or unreadable -", conditionMessage(e), "\n")
  NULL
})

# Loud startup diagnostic — if the Space is still carrying an old
# DRS26.csv (no Jersey column) this is where it shows up in the logs.
if (!is.null(drs_bio)) {
  cat("DRS bio loaded:", nrow(drs_bio), "rows |",
      if ("Jersey" %in% names(drs_bio))
        paste0("Jersey col OK (", sum(!is.na(drs_bio$Jersey)), " filled)")
      else
        "!! NO Jersey column - old DRS26.csv, NCAA jersey numbers will be blank !!",
      "|", if ("newestTeamName" %in% names(drs_bio)) "team-alias cols OK"
           else "!! newestTeam* cols missing !!", "\n")
}

drs_lookup <- function(name, team = NULL) {
  if (is.null(drs_bio)) return(NULL)
  k <- norm_player_key(name)

  # rows matching the team hint on any team-name flavor the file
  # carries (ncaa.com display, TruMedia full name, abbrev, location);
  # NULL when no hint given or nothing matches
  match_team <- function(r) {
    if (is.null(team) || length(team) != 1 || is.na(team) ||
        !nzchar(team) || nrow(r) == 0) return(NULL)
    tn <- .tm_norm(team)
    tcols <- intersect(c("team_name", "newestTeamName",
                         "newestTeamAbbrevName", "newestTeamLocation"),
                       names(r))
    keep <- rep(FALSE, nrow(r))
    for (tc in tcols) {
      v  <- .tm_norm(as.character(r[[tc]]))
      ok <- !is.na(v) & nzchar(v)
      keep <- keep | (ok & (v == tn |
                              grepl(tn, v, fixed = TRUE) |
                              vapply(v, function(z) nzchar(z) && nchar(z) > 3 &&
                                       grepl(z, tn, fixed = TRUE), logical(1))))
    }
    if (any(keep)) r[keep, , drop = FALSE] else NULL
  }

  # pass 1: exact key match; team hint disambiguates ~2,600 shared keys
  r <- drs_bio[!is.na(drs_bio$bio_key) & drs_bio$bio_key == k, , drop = FALSE]
  if (nrow(r) > 1) {
    tm <- match_team(r)
    if (!is.null(tm)) r <- tm
  }

  # pass 2: last name + first initial, ONLY when the team hint confirms
  # it (TrackMan spellings drift from the TruMedia roster — "Nick" vs
  # "Nicholas", stray middle names — never guess without the team)
  if (nrow(r) == 0) {
    parts <- strsplit(k, " ", fixed = TRUE)[[1]]
    if (length(parts) >= 2 && nchar(parts[1]) > 0) {
      last <- parts[length(parts)]
      cand <- drs_bio[!is.na(drs_bio$bio_key) &
                        endsWith(drs_bio$bio_key, paste0(" ", last)) &
                        substr(drs_bio$bio_key, 1, 1) == substr(parts[1], 1, 1),
                      , drop = FALSE]
      tm <- match_team(cand)
      if (!is.null(tm)) r <- tm
      # team-string drift (TrackMan vs TruMedia vs ncaa.com spellings)
      # was blanking bio for most matrix hitters — when last name +
      # first initial land on exactly ONE player nationally, that IS
      # the player, team confirm or not.
      else if (nrow(cand) == 1 ||
               (nrow(cand) > 0 && length(unique(cand$bio_key)) == 1))
        r <- cand
    }
  }
  if (nrow(r) == 0) return(NULL)

  out <- r[1, ]
  # same player often has one DRS row per position — roll them up
  # ("2B/SS") so headers show the full defensive picture
  rp <- r[r$bio_key == out$bio_key, , drop = FALSE]
  pos_all <- unique(as.character(rp$Position))
  pos_all <- pos_all[!is.na(pos_all) & nzchar(pos_all)]
  if (length(pos_all) > 1) out$Position <- paste(pos_all, collapse = "/")
  out
}

# ============================================================
# trumedia_player_bio_master.csv — headshots + team logos for the
# player-page bio cards
# ============================================================
hs_bio <- tryCatch({
  cand <- palace_file_candidates("trumedia_player_bio_master.csv")
  if (length(cand) == 0) stop("trumedia_player_bio_master.csv not present")
  cat("Headshot bio source:", normalizePath(cand[1]), "\n")
  d <- readr::read_csv(cand[1], show_col_types = FALSE)
  d$hs_key <- norm_player_key(d$player_name)
  d
}, error = function(e) {
  cat("WARNING: trumedia_player_bio_master.csv not found -",
      conditionMessage(e), "\n")
  NULL
})
if (!is.null(hs_bio)) {
  cat("Headshot bio loaded:", nrow(hs_bio), "players |",
      sum(!is.na(hs_bio$trumedia_headshot_url) &
            nzchar(hs_bio$trumedia_headshot_url)), "headshot urls\n")
}

# Headshot + team-logo lookup for the bio cards. Same matching contract
# as drs_lookup: exact normalized-name key first, team hint disambiguates
# shared names, last name + first initial only when the team confirms or
# the match is nationally unique.
headshot_lookup <- function(name, team = NULL) {
  if (is.null(hs_bio) || is.null(name) || length(name) != 1 ||
      is.na(name) || !nzchar(as.character(name))) return(NULL)
  k <- norm_player_key(name)

  match_team <- function(r) {
    if (is.null(team) || length(team) != 1 || is.na(team) ||
        !nzchar(team) || nrow(r) == 0) return(NULL)
    tn <- .tm_norm(team)
    keep <- rep(FALSE, nrow(r))
    if (nzchar(tn)) {
      tcols <- intersect(c("team_name", "trackman_team", "team_location",
                           "team_abbrev"), names(r))
      for (tc in tcols) {
        v  <- .tm_norm(as.character(r[[tc]]))
        ok <- !is.na(v) & nzchar(v)
        keep <- keep | (ok & (v == tn |
                                grepl(tn, v, fixed = TRUE) |
                                vapply(v, function(z) nzchar(z) && nchar(z) > 3 &&
                                         grepl(z, tn, fixed = TRUE), logical(1))))
      }
    }
    # a raw TrackMan code hint ("COA_CHA") matches trackman_abbr directly
    if ("trackman_abbr" %in% names(r)) {
      keep <- keep | (!is.na(r$trackman_abbr) &
                        toupper(trimws(as.character(r$trackman_abbr))) ==
                          toupper(trimws(as.character(team))))
    }
    if (any(keep)) r[keep, , drop = FALSE] else NULL
  }

  r <- hs_bio[!is.na(hs_bio$hs_key) & hs_bio$hs_key == k, , drop = FALSE]
  if (nrow(r) > 1) {
    tm <- match_team(r)
    if (!is.null(tm)) r <- tm
  }
  if (nrow(r) == 0) {
    parts <- strsplit(k, " ", fixed = TRUE)[[1]]
    if (length(parts) >= 2 && nchar(parts[1]) > 0) {
      last <- parts[length(parts)]
      cand <- hs_bio[!is.na(hs_bio$hs_key) &
                       endsWith(hs_bio$hs_key, paste0(" ", last)) &
                       substr(hs_bio$hs_key, 1, 1) == substr(parts[1], 1, 1),
                     , drop = FALSE]
      tm <- match_team(cand)
      if (!is.null(tm)) r <- tm
      else if (nrow(cand) > 0 && length(unique(cand$hs_key)) == 1) r <- cand
    }
  }
  if (nrow(r) == 0) return(NULL)

  vurl <- function(x) {
    x <- trimws(as.character(x[1]))
    if (length(x) == 1 && !is.na(x) && nzchar(x)) x else NULL
  }
  list(
    headshot = if ("trumedia_headshot_url" %in% names(r))
      vurl(r$trumedia_headshot_url) else NULL,
    logo     = if ("trumedia_team_logo_url" %in% names(r))
      vurl(r$trumedia_team_logo_url) else NULL
  )
}

# ---- DRS overall rating + compact info line (matrix labels, workbook) ----
# The DRS rating column name can drift between exports; first match wins.
.DRS_RATING_CANDIDATES <- c("DRS", "drs", "DRS Total", "Total DRS", "TotalDRS",
                            "Total", "Runs Saved", "RunsSaved",
                            "DefensiveRunsSaved")
.drs_rating_col <- if (!is.null(drs_bio)) {
  hit <- intersect(.DRS_RATING_CANDIDATES, names(drs_bio))
  if (length(hit) > 0) {
    cat("DRS rating column:", hit[1], "\n")
    hit[1]
  } else {
    cat("!! No DRS rating column found in DRS26.csv — overall pills will be blank.",
        "Columns present:", paste(head(names(drs_bio), 20), collapse = ", "), "\n")
    NULL
  }
} else NULL

# Sum a player's DRS across their position rows (one row per position in
# the file) -> the "overall" number shown in the matrix pills.
drs_overall <- function(name, team = NULL) {
  if (is.null(drs_bio) || is.null(.drs_rating_col)) return(NA_real_)
  r <- tryCatch(drs_lookup(name, team), error = function(e) NULL)
  if (is.null(r)) return(NA_real_)
  rows <- drs_bio[!is.na(drs_bio$bio_key) & drs_bio$bio_key == r$bio_key, ,
                  drop = FALSE]
  if ("team_name" %in% names(rows) && "team_name" %in% names(r) &&
      !is.na(r$team_name)) {
    tr <- rows[!is.na(rows$team_name) & rows$team_name == r$team_name, ,
               drop = FALSE]
    if (nrow(tr) > 0) rows <- tr
  }
  v <- suppressWarnings(as.numeric(rows[[.drs_rating_col]]))
  if (all(is.na(v))) return(NA_real_)
  sum(v, na.rm = TRUE)
}

# jersey / position / class / overall in one lookup (matrix row labels)
drs_info <- function(name, team = NULL) {
  r <- tryCatch(drs_lookup(name, team), error = function(e) NULL)
  gv <- function(cn) {
    if (is.null(r) || !(cn %in% names(r))) return("")
    v <- as.character(r[[cn]])
    if (length(v) == 0 || is.na(v) || !nzchar(v)) "" else v
  }
  list(jersey  = gv("Jersey"),
       pos     = gv("Position"),
       class   = gv("Class"),
       overall = drs_overall(name, team))
}

# DRS overall -> pill color on the same red/cream/green scale as the matrix
drs_pill_color <- function(v) {
  if (is.null(v) || is.na(v)) return("#E0E0E0")
  matchup_score_color(100 / (1 + exp(-as.numeric(v) / 4)))
}

# Universal jersey source: DRS26.csv Jersey column is authoritative
# for EVERY pitcher (Coastal included — CCU players are in the file
# under team "Coastal Carolina"). Callers may pass a fallback from
# another bio source, used only when DRS has nothing.
get_jersey_number <- function(name, team = NULL, fallback = "") {
  r <- tryCatch(drs_lookup(name, team), error = function(e) NULL)
  if (!is.null(r) && "Jersey" %in% names(r) && !is.na(r$Jersey) &&
      nzchar(as.character(r$Jersey))) {
    return(as.character(r$Jersey))
  }
  if (is.null(fallback) || length(fallback) == 0 || is.na(fallback)) "" else
    as.character(fallback)
}

# The CURRENT-season (TM_SEASON = 2026) team for a pitcher. Transfers
# break the old alphabetical-first directory pick — Albert Roblez was
# Long Beach St. in '25 and Oregon St. in '26, and the API call was
# searching LBSU's 2026 roster. Priority:
#   1) the team attached to his 2026 parquet rows (he's already thrown
#      tracked pitches this spring)
#   2) DRS26's newestTeamName — a current-season TruMedia export, so
#      it IS the 2026 team, in the exact string AllTeams matches on
#   3) the merged directory (old behavior) as a last resort
tm_current_team <- function(display_name) {
  if (is.null(display_name) || !nzchar(display_name)) return(NULL)
  if (exists("ncaa_dir_pairs") && nrow(ncaa_dir_pairs) > 0 &&
      "src" %in% names(ncaa_dir_pairs)) {
    r26 <- ncaa_dir_pairs[ncaa_dir_pairs$display == display_name &
                            ncaa_dir_pairs$src == "2026" &
                            !is.na(ncaa_dir_pairs$team_disp) &
                            nzchar(ncaa_dir_pairs$team_disp), , drop = FALSE]
    if (nrow(r26) > 0) return(r26$team_disp[1])
  }
  r <- tryCatch(drs_lookup(display_name), error = function(e) NULL)
  if (!is.null(r) && "newestTeamName" %in% names(r) &&
      !is.na(r$newestTeamName) && nzchar(as.character(r$newestTeamName))) {
    return(as.character(r$newestTeamName))
  }
  dir_row <- ncaa_directory[ncaa_directory$display == display_name, , drop = FALSE]
  if (nrow(dir_row) > 0)
    return(prettify_team(trimws(strsplit(dir_row$teams[1], "/")[[1]][1])))
  NULL
}

# Height string for the scouting PDF header (DRS first, CCU bio fallback)
get_pitcher_height_display <- function(name) {
  r <- drs_lookup(name)
  if (!is.null(r) && !is.na(r$Height) && nzchar(as.character(r$Height))) {
    return(as.character(r$Height))
  }
  if (exists("bio") && "Height" %in% names(bio)) {
    b <- bio[bio$Pitcher_FL == name | bio$Pitcher == name, , drop = FALSE]
    if (nrow(b) > 0 && !is.na(b$Height[1]) && nzchar(as.character(b$Height[1]))) {
      return(as.character(b$Height[1]))
    }
  }
  ""
}

# bio_heights augmented with DRS heights so arm-angle math works for
# NCAA arms when their data runs through process_pitcher_data().
bio_heights_ncaa <- if (!is.null(drs_bio)) {
  bind_rows(
    bio_heights,
    drs_bio %>%
      filter(!is.na(height_in)) %>%
      transmute(Pitcher_FL = Player, pitcher_height_inches = height_in)
  ) %>% distinct(Pitcher_FL, .keep_all = TRUE)
} else bio_heights

# ============================================================

# ============================================================
# Unified bio: id first, name last
# ------------------------------------------------------------
# One lookup for the bio cards, headers and season tables. Resolution:
#   1) TrackMan id -> identity table (lb/<season>/identity.parquet) ->
#      TruMedia player id + team id
#   2) reference players by TruMedia id, else by name + team
#   3) legacy DRS26 / headshot tables by name (+ team) for what is left
# Team fields (level, conference, logo) come from the reference team the
# player's id resolves to, so two programs sharing a name never swap
# crests. Every field is a length-1 character/numeric or NA.
player_bio <- function(name, team = NULL, tm_id = NULL, kind = c("pit", "bat"), player_id = NULL) {
  kind <- match.arg(kind)
  nm <- if (is.null(name) || !length(name) || is.na(name[1])) "" else as.character(name[1])
  tm_id <- if (is.null(tm_id) || !length(tm_id)) NA_character_ else .ref_id(tm_id[1])
  player_id <- if (is.null(player_id) || !length(player_id)) NA_character_ else .ref_id(player_id[1])
  team <- if (is.null(team) || !length(team) || is.na(team[1]) || !nzchar(team[1])) NULL else as.character(team[1])
  chr1 <- function(x) { if (is.null(x) || !length(x)) return(NA_character_); x <- as.character(x[1]); if (is.na(x) || !nzchar(trimws(x))) NA_character_ else x }
  num1 <- function(x) { v <- suppressWarnings(as.numeric(chr1(x))); if (is.finite(v) && v > 0) v else NA_real_ }
  out <- list(name = nm, player_id = player_id, tm_id = tm_id, team_id = NA_character_, team = NA_character_,
              level = NA_character_, conf = NA_character_, conf_name = NA_character_, logo = NA_character_,
              headshot = NA_character_, jersey = NA_character_, pos = NA_character_, class = NA_character_,
              height = NA_character_, height_in = NA_real_, weight = NA_real_, hometown = NA_character_,
              age = NA_real_, bats = NA_character_, throws = NA_character_, source = character(0))

  # 1) identity by id
  idr <- if (!is.na(tm_id) || !is.na(player_id)) tryCatch(tm_identity(tm_id = tm_id, player_id = player_id, kind = kind), error = function(e) NULL) else NULL
  if (!is.null(idr)) {
    if (is.na(out$player_id)) out$player_id <- chr1(idr$player_id)
    out$team_id <- chr1(idr$team_id)
    out$source <- c(out$source, "identity")
  }
  # 2) reference player
  rp <- if (!is.na(out$player_id)) ref_player_by_id(out$player_id) else NULL
  if (is.null(rp) && nzchar(nm)) {
    rp <- tryCatch(ref_player_lookup(nm, team_id = out$team_id, team = team,
                                     level = if (!is.null(idr)) chr1(idr$level) else NULL), error = function(e) NULL)
    # a name match is only trusted when the ids agree or nothing else identified the player
    if (!is.null(rp) && !is.na(out$player_id) && !identical(chr1(rp$player_id), out$player_id)) rp <- NULL
  }
  if (!is.null(rp)) {
    out$player_id <- chr1(rp$player_id)
    if (is.na(out$team_id)) out$team_id <- chr1(rp$team_id)
    out$jersey <- chr1(rp$jersey); out$pos <- chr1(rp$pos); out$class <- chr1(rp$class)
    out$height <- chr1(rp$height); out$height_in <- num1(rp$height_in); out$weight <- num1(rp$weight)
    out$hometown <- chr1(rp$hometown); out$age <- num1(rp$age); out$bats <- chr1(rp$bats); out$throws <- chr1(rp$throws)
    out$headshot <- chr1(rp$headshot_url)
    out$source <- c(out$source, "reference")
  }
  # 3) team fields from the reference team (id, else code, else name). The
  #    caller's team hint is the TrackMan (college) team; when the id only
  #    resolved a summer club (calendar-year frame), the college team wins.
  ti <- ref_team_info(team_id = out$team_id, code = team, name = team, by_name = TRUE)
  if (!is.null(team) && (is.na(ti$level[1]) || ti$level[1] == "Summer")) {
    hint <- ref_team_info(code = team, name = team, by_name = TRUE)
    if (!is.na(hint$level[1]) && hint$level[1] != "Summer") ti <- hint
  }
  if (!is.na(ti$team_id[1])) {
    out$team_id <- ti$team_id[1]; out$team <- chr1(ti$team); out$level <- chr1(ti$level)
    out$conf <- chr1(ti$conf); out$conf_name <- chr1(ti$conf_name); out$logo <- chr1(ti$logo)
  }
  if (is.na(out$team) && !is.null(team)) out$team <- prettify_team(team)

  # 4) legacy tables for the gaps
  need <- is.na(out$jersey) || is.na(out$pos) || is.na(out$class) || is.na(out$height) || is.na(out$hometown) || is.na(out$logo) || is.na(out$headshot)
  if (need && nzchar(nm)) {
    r <- tryCatch(drs_lookup(nm, team %||% out$team), error = function(e) NULL)
    if (!is.null(r)) {
      if (is.na(out$jersey)) out$jersey <- chr1(r$Jersey)
      if (is.na(out$pos)) out$pos <- chr1(r$Position)
      if (is.na(out$class)) out$class <- chr1(r$Class)
      if (is.na(out$height)) { out$height <- chr1(r$Height); out$height_in <- num1(r$height_in) }
      if (is.na(out$weight)) out$weight <- num1(r$Weight)
      if (is.na(out$hometown)) out$hometown <- chr1(r$Hometown)
      if (is.na(out$age)) out$age <- num1(r$Age)
      if (is.na(out$logo)) out$logo <- chr1(r$team_logo)
      if (is.na(out$team)) out$team <- chr1(r$team_name)
      out$source <- c(out$source, "drs")
    }
    hs <- tryCatch(headshot_lookup(nm, team %||% out$team), error = function(e) NULL)
    if (!is.null(hs)) {
      if (is.na(out$headshot)) out$headshot <- chr1(hs$headshot)
      if (is.na(out$logo)) out$logo <- chr1(hs$logo)
      out$source <- c(out$source, "headshot")
    }
  }
  out
}
