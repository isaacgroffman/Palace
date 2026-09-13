# ============================================================================
# REFERENCE LOOKUPS
#
# Two layers:
#
#   1) The id-keyed reference (reference/d1_teams.csv, lower_levels_teams.csv,
#      d1_conferences.csv, lower_levels_conferences.csv, bioinfo.csv,
#      lower_levels_players.csv, trackman_teams.csv). Teams are keyed by the
#      TruMedia teamId that the PlayerTotals / AllTeams frames carry, players
#      by the TruMedia playerId. Level, conference, logo and bio all resolve
#      from those ids, so a name that two programs share can never borrow the
#      other program's crest or conference.
#
#   2) The legacy name map (reference/merged_teams.csv): TrackMan team code ->
#      display name. Still what prettify_team() shows; conference / level are
#      no longer taken from it.
# ============================================================================

.ref_norm <- function(x) gsub("[^a-z0-9]", "", tolower(as.character(x)))
.conf_norm <- .ref_norm
.ref_chr <- function(x) { x <- trimws(as.character(x)); x[is.na(x) | !nzchar(x) | x %in% c("NA", "-", "nan")] <- NA_character_; x }
.ref_id  <- function(x) {
  # "730292224", 730292224, 7.30292224e8 and "730292224.0" all key the same
  x <- suppressWarnings(as.numeric(as.character(x)))
  out <- ifelse(is.finite(x) & x > 0, sprintf("%.0f", x), NA_character_)
  out
}
.ref_read <- function(path, ...) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readr::read_csv(path, show_col_types = FALSE, progress = FALSE, guess_max = 5000, ...),
           error = function(e) { cat("[Ref] failed to read", path, "-", conditionMessage(e), "\n"); NULL })
}

# ---- conference short codes ------------------------------------------------
# The app's historic D1 codes (what the pills, pickers and URLs already use);
# every other conference gets an acronym built from its name.
REF_D1_CONF_CODES <- c(
  "America East Conference" = "AMEAST", "American Athletic Conference" = "AMER",
  "Atlantic 10 Conference" = "A10", "Atlantic Coast Conference" = "ACC",
  "Atlantic Sun Conference" = "ASUN", "Big 12 Conference" = "BIG12",
  "Big East Conference" = "BEAST", "Big South Conference" = "BSOU",
  "Big Ten Conference" = "BIG10", "Big West Conference" = "BW",
  "Coastal Athletic Association" = "CAA", "Conference USA" = "CUSA",
  "Horizon League" = "HORZ", "Independent" = "IND", "Ivy Group" = "IVY",
  "Metro Atlantic Athletic Conference" = "MAAC", "Mid-American Conference" = "MAC",
  "Missouri Valley Conference" = "MVC", "Mountain West" = "MW",
  "Northeast Conference" = "NEC", "Ohio Valley Conference" = "OVC",
  "Patriot League" = "PAT", "Southeastern Conference" = "SEC",
  "Southern Conference" = "SOCON", "Southland Conference" = "SLAND",
  "Southwestern Athletic Conf." = "SWAC", "Sun Belt Conference" = "SBELT",
  "The Summit League" = "SUM", "West Coast Conference" = "WCC",
  "Western Athletic Conference" = "WAC")
REF_CONF_CODE_OVERRIDES <- c(
  "Cape Cod League" = "CCBL", "Coastal Plain League" = "CPL", "Northwoods League" = "NWL",
  "West Coast League" = "WCL", "New England Collegiate Baseball League" = "NECBL",
  "Northwest Athletic Conference" = "NWAC", "Frontier" = "FRONT", "Sooner" = "SAC",
  "Chicagoland" = "CCAC", "Appalachian" = "AAC", "The Sun" = "SUN", "Mid-South" = "MSC",
  "Red River" = "RRAC", "River States" = "RSC", "Great Plains" = "GPAC", "Kansas Collegiate" = "KCAC",
  "Heart of America" = "HAAC", "American Midwest" = "AMC", "Cascade Collegiate" = "CCC",
  "Southern States" = "SSAC", "Wolverine-Hoosier Athletic" = "WHAC", "Crossroads League" = "CL",
  "Empire 8" = "E8", "Liberty League" = "LL", "Landmark Conference" = "LAND",
  "Skyline Conference" = "SKY", "Midwest Conference" = "MWC", "Northeast-10 Conference" = "NE10",
  "Northwest Conference" = "NWC")
ref_conf_code <- function(conf_short, level_group = NA_character_) {
  cs <- .ref_chr(conf_short)
  out <- rep(NA_character_, length(cs))
  ok <- !is.na(cs)
  if (!any(ok)) return(out)
  d1 <- unname(REF_D1_CONF_CODES[cs]); ov <- unname(REF_CONF_CODE_OVERRIDES[cs])
  acro <- vapply(cs, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- gsub("\\[.*?\\]|\\(.*?\\)", "", s)
    if (grepl("^Region\\s+\\d+$", s, ignore.case = TRUE)) return(toupper(sub("^Region\\s+", "R", s, ignore.case = TRUE)))
    w <- strsplit(gsub("[^A-Za-z0-9 ]", " ", s), "\\s+")[[1]]
    w <- w[nzchar(w) & !tolower(w) %in% c("the", "of", "and", "s")]
    if (!length(w)) return(NA_character_)
    if (length(w) == 1) return(toupper(substr(w, 1, 5)))
    paste(ifelse(grepl("^[0-9]+$", w), w, toupper(substr(w, 1, 1))), collapse = "")
  }, character(1), USE.NAMES = FALSE)
  lv <- rep_len(as.character(level_group), length(cs))
  out <- ifelse(!is.na(lv) & lv == "D1" & !is.na(d1), d1,
                ifelse(!is.na(ov), ov, ifelse(!is.na(d1), d1, acro)))
  out
}

# ---- teams -----------------------------------------------------------------
REF_TEAMS <- local({
  parts <- list(.ref_read("reference/d1_teams.csv", col_types = readr::cols(.default = "c")),
                .ref_read("reference/lower_levels_teams.csv", col_types = readr::cols(.default = "c")))
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) {
    cat("[Ref] no id-keyed team files (reference/d1_teams.csv, lower_levels_teams.csv)\n")
    return(NULL)
  }
  t <- dplyr::bind_rows(parts)
  t <- t[!is.na(.ref_id(t$team_id)), , drop = FALSE]
  out <- data.frame(
    team_id        = .ref_id(t$team_id),
    team_short     = .ref_chr(t$team_short),
    team_full_name = .ref_chr(t$team_full_name),
    nickname       = .ref_chr(t$nickname),
    team_abbrev    = .ref_chr(t$team_abbrev),
    location       = .ref_chr(t$location),
    association    = .ref_chr(t$association),
    level_group    = .ref_chr(t$level_group),
    division       = .ref_chr(t$division),
    conf_id        = .ref_chr(t$conf_id),
    conf_name      = .ref_chr(t$conf_name),
    conf_short     = .ref_chr(t$conf_short),
    logo_url       = .ref_chr(t$logo_url),
    stringsAsFactors = FALSE)
  out$level_group[is.na(out$level_group)] <- "Other"
  out$conf_code <- ref_conf_code(out$conf_short, out$level_group)
  # name keys for the (fallback) name matches
  out$key_short <- .ref_norm(out$team_short)
  out$key_full  <- .ref_norm(out$team_full_name)
  out$key_loc   <- .ref_norm(out$location)
  out <- out[!duplicated(out$team_id), , drop = FALSE]
  rownames(out) <- NULL
  cat("[Ref] teams:", nrow(out), "|", paste(names(table(out$level_group)), table(out$level_group), collapse = ", "), "\n")
  out
})
REF_LEVELS <- c("D1", "D2", "D3", "NAIA", "JUCO", "Summer")

REF_CONFS <- local({
  parts <- list(.ref_read("reference/d1_conferences.csv", col_types = readr::cols(.default = "c")),
                .ref_read("reference/lower_levels_conferences.csv", col_types = readr::cols(.default = "c")))
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) return(NULL)
  c0 <- dplyr::bind_rows(parts)
  out <- data.frame(conf_id = .ref_chr(c0$conf_id), conf_name = .ref_chr(c0$conf_name),
                    conf_short = .ref_chr(c0$conf_short), association = .ref_chr(c0$association),
                    level_group = .ref_chr(c0$level_group), stringsAsFactors = FALSE)
  out <- out[!is.na(out$conf_id) & !is.na(out$conf_short), , drop = FALSE]
  out$conf_code <- ref_conf_code(out$conf_short, out$level_group)
  # a code shared by two conferences at the same level would be ambiguous on
  # a pill: suffix the later one
  dup <- duplicated(paste(out$level_group, out$conf_code))
  if (any(dup)) out$conf_code[dup] <- paste0(out$conf_code[dup], "2")
  if (!is.null(REF_TEAMS)) {
    n <- table(REF_TEAMS$conf_id)
    out$n_teams <- as.integer(n[out$conf_id]); out$n_teams[is.na(out$n_teams)] <- 0L
  }
  out <- out[order(match(out$level_group, REF_LEVELS), out$conf_short), , drop = FALSE]
  rownames(out) <- NULL
  out
})
# keep the team table's codes identical to the conference table's
if (!is.null(REF_TEAMS) && !is.null(REF_CONFS)) {
  i <- match(REF_TEAMS$conf_id, REF_CONFS$conf_id)
  REF_TEAMS$conf_code <- ifelse(is.na(i), REF_TEAMS$conf_code, REF_CONFS$conf_code[i])
}

# ---- TrackMan team code -> TruMedia team id --------------------------------
# reference/trackman_teams.csv is built by scripts/build_trackman_crosswalk.R
# (bio master verified codes + name matches + majority votes from the
# players both systems id). lb/<season>/team_xwalk.parquet in Storage carries
# the latest data votes; ref_add_team_xwalk() folds it in at runtime.
.ref_env <- new.env(parent = emptyenv())
.ref_env$xwalk <- local({
  x <- .ref_read("reference/trackman_teams.csv", col_types = readr::cols(.default = "c"))
  if (is.null(x) || !all(c("trackman_code", "team_id") %in% names(x))) {
    cat("[Ref] no TrackMan crosswalk (reference/trackman_teams.csv)\n")
    return(data.frame(trackman_code = character(0), team_id = character(0), source = character(0), stringsAsFactors = FALSE))
  }
  x <- data.frame(trackman_code = toupper(trimws(x$trackman_code)), team_id = .ref_id(x$team_id),
                  source = .ref_chr(x$source %||% NA_character_), stringsAsFactors = FALSE)
  x <- x[!is.na(x$trackman_code) & nzchar(x$trackman_code) & !is.na(x$team_id), , drop = FALSE]
  x <- x[!duplicated(x$trackman_code), , drop = FALSE]
  cat("[Ref] TrackMan team crosswalk:", nrow(x), "codes\n")
  x
})
ref_add_team_xwalk <- function(x) {
  if (is.null(x) || !is.data.frame(x) || !nrow(x) || !all(c("trackman_code", "team_id") %in% names(x))) return(invisible(NULL))
  x <- data.frame(trackman_code = toupper(trimws(as.character(x$trackman_code))), team_id = .ref_id(x$team_id),
                  source = if ("source" %in% names(x)) as.character(x$source) else "storage", stringsAsFactors = FALSE)
  x <- x[!is.na(x$trackman_code) & nzchar(x$trackman_code) & !is.na(x$team_id), , drop = FALSE]
  cur <- .ref_env$xwalk
  # data votes from the build outrank the shipped file
  cur <- cur[!cur$trackman_code %in% x$trackman_code, , drop = FALSE]
  .ref_env$xwalk <- rbind(x[, names(cur)], cur)
  invisible(nrow(x))
}
ref_team_id_for_code <- function(code) {
  code <- toupper(trimws(as.character(code)))
  xw <- .ref_env$xwalk
  out <- xw$team_id[match(code, xw$trackman_code)]
  out[is.na(code) | !nzchar(code)] <- NA_character_
  unname(out)
}

# ---- team lookups by id ----------------------------------------------------
ref_team_rows <- function(team_id) {
  if (is.null(REF_TEAMS)) return(NULL)
  REF_TEAMS[match(.ref_id(team_id), REF_TEAMS$team_id), , drop = FALSE]
}
.ref_team_col <- function(team_id, col) {
  r <- ref_team_rows(team_id)
  if (is.null(r)) return(rep(NA_character_, length(team_id)))
  unname(as.character(r[[col]]))
}
ref_team_level <- function(team_id) .ref_team_col(team_id, "level_group")
ref_team_conf  <- function(team_id) .ref_team_col(team_id, "conf_code")
ref_team_conf_id <- function(team_id) .ref_team_col(team_id, "conf_id")
ref_team_conf_name <- function(team_id) .ref_team_col(team_id, "conf_short")
ref_team_logo  <- function(team_id) .ref_team_col(team_id, "logo_url")
ref_team_name  <- function(team_id) .ref_team_col(team_id, "team_short")
ref_team_full  <- function(team_id) .ref_team_col(team_id, "team_full_name")

# Name -> team id, LAST resort (TruMedia spells a school three ways and two
# levels can share a name). A level hint narrows it; otherwise higher levels
# win ties, which is right far more often than not for a TrackMan pool.
ref_team_id_for_name <- function(name, level = NULL) {
  if (is.null(REF_TEAMS)) return(rep(NA_character_, length(name)))
  k <- .ref_norm(name)
  vapply(seq_along(k), function(i) {
    if (is.na(k[i]) || !nzchar(k[i])) return(NA_character_)
    hit <- REF_TEAMS$key_short == k[i] | REF_TEAMS$key_full == k[i] | REF_TEAMS$key_loc == k[i]
    hit[is.na(hit)] <- FALSE
    if (!is.null(level) && length(level) >= i && !is.na(level[i]) && level[i] %in% REF_LEVELS)
      if (any(hit & REF_TEAMS$level_group == level[i])) hit <- hit & REF_TEAMS$level_group == level[i]
    if (!any(hit)) return(NA_character_)
    r <- REF_TEAMS[hit, , drop = FALSE]
    r$team_id[order(match(r$level_group, REF_LEVELS))][1]
  }, character(1))
}

# One call for everything a board / card needs about a team. Resolution
# order: explicit TruMedia team id, then the TrackMan code through the
# crosswalk, then (only if allowed) the display name.
ref_team_info <- function(team_id = NULL, code = NULL, name = NULL, level = NULL, by_name = TRUE) {
  n <- max(length(team_id), length(code), length(name), 1L)
  id <- if (!is.null(team_id)) .ref_id(rep_len(team_id, n)) else rep(NA_character_, n)
  if (!is.null(code)) { alt <- ref_team_id_for_code(rep_len(code, n)); id <- ifelse(is.na(id), alt, id) }
  if (isTRUE(by_name) && !is.null(name)) {
    gap <- is.na(id)
    if (any(gap)) id[gap] <- ref_team_id_for_name(rep_len(name, n)[gap], if (!is.null(level)) rep_len(level, n)[gap] else NULL)
  }
  r <- ref_team_rows(id)
  if (is.null(r)) r <- data.frame(team_id = id, level_group = NA_character_, conf_code = NA_character_, conf_id = NA_character_,
                                  conf_short = NA_character_, logo_url = NA_character_, team_short = NA_character_,
                                  stringsAsFactors = FALSE)[rep(1, n), ]
  data.frame(team_id = id, level = as.character(r$level_group), conf = as.character(r$conf_code), conf_id = as.character(r$conf_id),
             conf_name = as.character(r$conf_short), logo = as.character(r$logo_url), team = as.character(r$team_short),
             stringsAsFactors = FALSE)
}

# ---- players ---------------------------------------------------------------
.ref_height_in <- function(h) {
  h <- as.character(h)
  ft   <- suppressWarnings(as.numeric(sub("^\\s*(\\d+).*$", "\\1", h)))
  inch <- suppressWarnings(as.numeric(sub("^\\s*\\d+\\D+(\\d+).*$", "\\1", h)))
  inch[is.na(inch)] <- 0
  ifelse(is.na(ft), NA_real_, ft * 12 + inch)
}
.ref_name_key <- function(x) {
  x <- as.character(x)
  x <- ifelse(grepl(",", x), sub("^\\s*([^,]+),\\s*(.+)\\s*$", "\\2 \\1", x), x)
  if (requireNamespace("stringi", quietly = TRUE)) x <- stringi::stri_trans_general(x, "Latin-ASCII")
  else { tx <- iconv(x, from = "", to = "ASCII//TRANSLIT"); x <- ifelse(is.na(tx), x, tx) }
  x <- tolower(trimws(gsub("\\s+", " ", x)))
  x <- sub("\\s+(jr|sr|ii|iii|iv)\\.?$", "", x)
  trimws(gsub("[^a-z ]", "", x))
}
REF_PLAYERS <- local({
  d1 <- .ref_read("reference/bioinfo.csv", col_types = readr::cols(.default = "c"))
  ll <- .ref_read("reference/lower_levels_players.csv", col_types = readr::cols(.default = "c"))
  parts <- list()
  if (!is.null(d1) && "playerId" %in% names(d1)) {
    parts$d1 <- data.frame(
      player_id = .ref_id(d1$playerId), name = .ref_chr(d1$playerFullName),
      first = .ref_chr(d1$playerFirstName), last = .ref_chr(d1$player),
      pos = .ref_chr(d1$pos), bats = .ref_chr(d1$batsHand), throws = .ref_chr(d1$throwsHand),
      jersey = .ref_chr(d1$Jersey), class = .ref_chr(d1$ClassYear), height = .ref_chr(d1$Height),
      weight = suppressWarnings(as.numeric(d1$Weight)), dob = .ref_chr(d1$DOB), age = suppressWarnings(as.numeric(d1$Age)),
      hometown = .ref_chr(d1$Hometown), team_id = .ref_id(d1$newestTeamId), team_name_src = .ref_chr(d1$newestTeamName),
      headshot_url = NA_character_, source = "bioinfo.csv", stringsAsFactors = FALSE)
  }
  if (!is.null(ll) && "player_id" %in% names(ll)) {
    parts$ll <- data.frame(
      player_id = .ref_id(ll$player_id), name = .ref_chr(ll$player_full_name),
      first = .ref_chr(ll$first_name), last = .ref_chr(ll$last_name),
      pos = .ref_chr(ll$pos), bats = .ref_chr(ll$bats), throws = .ref_chr(ll$throws),
      jersey = .ref_chr(ll$jersey), class = NA_character_, height = .ref_chr(ll$height),
      weight = suppressWarnings(as.numeric(ll$weight_lb)), dob = .ref_chr(ll$dob), age = suppressWarnings(as.numeric(ll$age)),
      hometown = .ref_chr(ll$hometown), team_id = .ref_id(ll$team_id), team_name_src = .ref_chr(ll$team_short),
      headshot_url = .ref_chr(ll$headshot_url), source = .ref_chr(ll$source_file), stringsAsFactors = FALSE)
  }
  if (!length(parts)) { cat("[Ref] no player reference files\n"); return(NULL) }
  p <- dplyr::bind_rows(parts)
  p <- p[!is.na(p$player_id) & !is.na(p$name), , drop = FALSE]
  p <- p[!duplicated(p$player_id), , drop = FALSE]
  p$age[!is.finite(p$age) | p$age <= 0] <- NA_real_
  p$weight[!is.finite(p$weight) | p$weight <= 0] <- NA_real_
  p$height_in <- .ref_height_in(p$height)
  p$height[!is.finite(p$height_in)] <- NA_character_
  # TruMedia serves a headshot for every player id it knows (blank image
  # when it has none); the lower-level file carries the same URL explicitly
  p$headshot_url <- ifelse(is.na(p$headshot_url),
                           sprintf("https://static.trumedianetworks.com/images/ncaabaseball/players/%s.png?v=2", p$player_id),
                           p$headshot_url)
  ti <- ref_team_info(team_id = p$team_id)
  p$level <- ti$level; p$conf <- ti$conf; p$conf_id <- ti$conf_id; p$logo_url <- ti$logo
  p$team <- ifelse(is.na(ti$team), p$team_name_src, ti$team)
  p$key <- .ref_name_key(p$name)
  p$key_team <- .ref_norm(p$team)
  rownames(p) <- NULL
  cat("[Ref] players:", nrow(p), "|", paste(names(table(p$level, useNA = "ifany")), table(p$level, useNA = "ifany"), collapse = ", "), "\n")
  p
})

ref_player_by_id <- function(player_id) {
  if (is.null(REF_PLAYERS)) return(NULL)
  id <- .ref_id(player_id)
  if (!length(id) || is.na(id[1])) return(NULL)
  r <- REF_PLAYERS[match(id[1], REF_PLAYERS$player_id), , drop = FALSE]
  if (nrow(r) && !is.na(r$player_id[1])) r else NULL
}
# Name fallback with the same contract as drs_lookup: exact key, team hint
# (id, code or name) disambiguates, last name + first initial only when the
# team confirms it or the match is unique nationally.
ref_player_lookup <- function(name, team_id = NULL, team = NULL, level = NULL) {
  if (is.null(REF_PLAYERS) || is.null(name) || length(name) != 1 || is.na(name) || !nzchar(name)) return(NULL)
  k <- .ref_name_key(name)
  tid <- if (!is.null(team_id)) .ref_id(team_id[1]) else NA_character_
  if (is.na(tid) && !is.null(team) && length(team) == 1 && !is.na(team) && nzchar(team)) {
    tid <- ref_team_id_for_code(team)
    if (is.na(tid)) tid <- ref_team_id_for_name(team, level)
  }
  tk <- if (!is.null(team) && length(team) == 1 && !is.na(team)) .ref_norm(team) else NA_character_
  narrow <- function(r) {
    if (!nrow(r)) return(r)
    if (!is.na(tid) && any(r$team_id %in% tid)) return(r[r$team_id %in% tid, , drop = FALSE])
    if (!is.na(tk) && nzchar(tk)) {
      hit <- !is.na(r$key_team) & (r$key_team == tk | (nchar(tk) > 3 & grepl(tk, r$key_team, fixed = TRUE)))
      if (any(hit)) return(r[hit, , drop = FALSE])
    }
    if (!is.null(level) && length(level) == 1 && !is.na(level) && any(r$level %in% level)) return(r[r$level %in% level, , drop = FALSE])
    r
  }
  r <- REF_PLAYERS[REF_PLAYERS$key == k, , drop = FALSE]
  if (nrow(r) > 1) r <- narrow(r)
  if (nrow(r) > 1) r <- r[order(match(r$level, REF_LEVELS)), , drop = FALSE]
  if (!nrow(r)) {
    parts <- strsplit(k, " ", fixed = TRUE)[[1]]
    if (length(parts) >= 2 && nchar(parts[1]) > 0) {
      last <- parts[length(parts)]
      cand <- REF_PLAYERS[endsWith(REF_PLAYERS$key, paste0(" ", last)) & substr(REF_PLAYERS$key, 1, 1) == substr(parts[1], 1, 1), , drop = FALSE]
      nn <- narrow(cand)
      if (nrow(nn) && nrow(nn) < nrow(cand)) r <- nn
      else if (nrow(cand) == 1 || (nrow(cand) > 0 && length(unique(cand$key)) == 1)) r <- cand
    }
  }
  if (!nrow(r)) return(NULL)
  r[1, , drop = FALSE]
}

# ---- legacy name map (display names) ----------------------------------------
team_name_map <- read_csv("reference/merged_teams.csv", show_col_types = FALSE) %>%
  select(any_of(c("trackman_abbr", "team_name", "conference", "logo_url"))) %>%
  filter(!is.na(team_name) & team_name != "")
if (!"conference" %in% names(team_name_map))
  team_name_map$conference <- NA_character_

# conference -> the team names that belong to it (TruMedia short name,
# full name and location, so a picker built from any of them matches).
# Values are conf codes; LB_CONFERENCE_CHOICES groups them by level for the
# picker (label = full name, value = code).
CONF_TEAMS <- if (!is.null(REF_TEAMS)) {
  t <- REF_TEAMS[!is.na(REF_TEAMS$conf_code), , drop = FALSE]
  dplyr::bind_rows(
    data.frame(conference = t$conf_code, conf_id = t$conf_id, team_name = t$team_short, level = t$level_group, stringsAsFactors = FALSE),
    data.frame(conference = t$conf_code, conf_id = t$conf_id, team_name = t$team_full_name, level = t$level_group, stringsAsFactors = FALSE),
    data.frame(conference = t$conf_code, conf_id = t$conf_id, team_name = t$location, level = t$level_group, stringsAsFactors = FALSE)) %>%
    filter(!is.na(team_name), team_name != "") %>% distinct()
} else {
  team_name_map %>%
    filter(!is.na(conference), conference != "", conference != "Team", !is.na(team_name), team_name != "") %>%
    distinct(conference, team_name) %>% mutate(conf_id = conference, level = "D1")
}
LB_CONFERENCE_CHOICES <- local({
  if (is.null(REF_CONFS)) return(sort(unique(CONF_TEAMS$conference)))
  c0 <- REF_CONFS[REF_CONFS$n_teams > 0 & !is.na(REF_CONFS$conf_code), , drop = FALSE]
  out <- lapply(REF_LEVELS, function(lv) {
    r <- c0[c0$level_group == lv, , drop = FALSE]
    if (!nrow(r)) return(NULL)
    setNames(as.list(r$conf_id), paste0(r$conf_short, " (", r$conf_code, ")"))
  })
  names(out) <- REF_LEVELS
  out[!vapply(out, is.null, logical(1))]
})
LB_CONFERENCES <- sort(unique(CONF_TEAMS$conference))
cat("[Conf] conferences loaded:", length(LB_CONFERENCES),
    "covering", length(unique(CONF_TEAMS$team_name)), "team names\n")

# `confs` may be conf ids (the picker's values) or the short codes
conf_team_names <- function(confs) {
  if (is.null(confs) || length(confs) == 0) return(character(0))
  unique(CONF_TEAMS$team_name[CONF_TEAMS$conference %in% confs | CONF_TEAMS$conf_id %in% confs])
}
conf_codes_for <- function(confs) {
  if (is.null(confs) || length(confs) == 0) return(character(0))
  unique(c(CONF_TEAMS$conference[CONF_TEAMS$conf_id %in% confs], confs))
}
conf_full_name <- function(code) {
  if (is.null(REF_CONFS)) return(as.character(code))
  out <- REF_CONFS$conf_short[match(as.character(code), REF_CONFS$conf_code)]
  ifelse(is.na(out), as.character(code), out)
}

prettify_team <- function(x) {
  lookup <- setNames(team_name_map$team_name, team_name_map$trackman_abbr)
  out <- lookup[as.character(x)]
  # unname the whole result: lookup[] attaches names (NA names for codes
  # not in the map) and ifelse() propagates them onto the output, which
  # later crashes updateSelectizeInput(server = TRUE) with
  # "NAs are not allowed in subscripted assignments".
  out <- unname(ifelse(is.na(out), NA_character_, unname(out)))
  # codes the legacy map never had: the crosswalk + reference name
  gap <- is.na(out)
  if (any(gap)) out[gap] <- ref_team_name(ref_team_id_for_code(x[gap]))
  ifelse(is.na(out), as.character(x), out)
}

# Zero-row frame with the core columns (correctly typed) that
# downstream reactives filter on. Returned instead of data.frame()
# when a pitcher's data fails to load, so filters yield empty results
# rather than "object 'BatterSide' not found" errors.
empty_pitch_frame <- function() {
  chr_cols <- c("Pitcher", "PitcherId", "PitcherThrows", "PitcherTeam",
                "Batter", "BatterId", "BatterSide", "BatterTeam",
                "TaggedPitchType", "AutoPitchType", "PitchCall", "KorBB",
                "TaggedHitType", "PlayResult", "GameID", "PitchUID",
                "Top.Bottom", "Count", "source_file")
  num_cols <- c("Balls", "Strikes", "Outs", "Inning", "PAofInning",
                "PitchofPA", "PitchNo", "RelSpeed", "SpinRate", "SpinAxis",
                "InducedVertBreak", "HorzBreak", "VertBreak",
                "VertApprAngle", "HorzApprAngle", "RelHeight", "RelSide",
                "Extension", "PlateLocHeight", "PlateLocSide", "ExitSpeed",
                "Angle", "Distance", "Bearing", "arm_angle_savant",
                "stuff_plus", "pitching_plus", "location_plus",
                "stuff_xrv", "pitching_xrv", "location_xrv",
                "height_inches", "pitcher_height_inches",
                "StrikeIndicator", "StrikeZoneIndicator", "WhiffIndicator",
                "SwingIndicator", "HitIndicator", "ABindicator",
                "PAindicator", "WalkIndicator", "HBPIndicator", "BIPind",
                "SolidContact", "HHindicator", "biphh", "BarrelIndicator",
                "IZBarrelIndicator", "FBindicator", "OSindicator",
                "FBstrikeind", "OSstrikeind", "Zwhiffind", "Zswing",
                "GBindicator", "Chaseindicator", "OutofZone", "totalbases")
  out <- c(
    setNames(lapply(chr_cols, function(z) character(0)), chr_cols),
    setNames(lapply(num_cols, function(z) numeric(0)), num_cols),
    list(Date = as.Date(character(0)),
         ManOnFirst = logical(0), ManOnSecond = logical(0),
         ManOnThird = logical(0))
  )
  as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
}

# ---- TrackMan code -> team id votes ------------------------------------------
# Players both systems identify (TrackMan id on the TruMedia line) tie a
# TrackMan team code to a TruMedia team id. `pairs` = data.frame(tm_id,
# trackman_code) from the pitch data; `ids` = data.frame(tm_id, team_id) from
# the TruMedia frames. Summer clubs are excluded (a college code never means
# a summer team) and a code needs a clear majority to be trusted.
ref_team_votes <- function(pairs, ids, min_n = 2, min_share = 0.5) {
  if (is.null(pairs) || is.null(ids) || !nrow(pairs) || !nrow(ids)) return(NULL)
  p <- data.frame(tm_id = .ref_id(pairs$tm_id), code = toupper(trimws(as.character(pairs$trackman_code))), stringsAsFactors = FALSE)
  i <- data.frame(tm_id = .ref_id(ids$tm_id), team_id = .ref_id(ids$team_id), stringsAsFactors = FALSE)
  p <- p[!is.na(p$tm_id) & !is.na(p$code) & nzchar(p$code), , drop = FALSE]
  i <- i[!is.na(i$tm_id) & !is.na(i$team_id), , drop = FALSE]
  if (!is.null(REF_TEAMS)) i <- i[i$team_id %in% REF_TEAMS$team_id[REF_TEAMS$level_group != "Summer"], , drop = FALSE]
  p <- unique(p); i <- unique(i)
  j <- merge(p, i, by = "tm_id")
  if (!nrow(j)) return(NULL)
  v <- j %>% dplyr::count(code, team_id, name = "n") %>%
    dplyr::group_by(code) %>% dplyr::mutate(share = n / sum(n)) %>%
    dplyr::arrange(dplyr::desc(n), .by_group = TRUE) %>% dplyr::slice(1) %>% dplyr::ungroup() %>% as.data.frame()
  v <- v[v$n >= min_n & v$share >= min_share, , drop = FALSE]
  data.frame(trackman_code = v$code, team_id = v$team_id, n = as.integer(v$n), share = round(v$share, 3),
             source = "votes", stringsAsFactors = FALSE)
}

# ---- identity table ------------------------------------------------------------
# One row per player x team x season type from every frame that carries ids.
#   frames   list(tm_pitching_team, tm_batting_team, tm_pitching_totals,
#            tm_batting_totals) in the raw TruMedia vocabulary
#   trackman data.frame(kind, tm_id, name, trackman_code, tk_level) from the
#            scoring pipeline (players with tracked pitches)
# Flags: tm_id_multi (one TrackMan id under several TruMedia ids),
# player_id_multi (one TruMedia id under several TrackMan ids), name_dupe
# (the same name on different ids at the same level and side).
ref_identity_table <- function(frames, trackman = NULL, season = NA_integer_) {
  pick <- function(f, pats) { for (p in pats) { h <- grep(p, names(f), ignore.case = TRUE, value = TRUE); if (length(h)) return(as.character(f[[h[1]]])) }; rep(NA_character_, nrow(f)) }
  rows <- list()
  for (kind in c("pit", "bat")) {
    f <- frames[[if (kind == "pit") "tm_pitching_team" else "tm_batting_team"]]
    if (is.data.frame(f) && nrow(f) && "teamId" %in% names(f))
      rows[[paste0("team_", kind)]] <- data.frame(
        kind = kind, tm_id = .ref_id(f$trackmanPlayerId), player_id = .ref_id(f$playerId), team_id = .ref_id(f$teamId),
        name = pick(f, c("^fullName$", "^player$", "playerName", "^name$")), season_type = "Spring", source = "team",
        tk_level = NA_character_, stringsAsFactors = FALSE)
  }
  # the league-wide calendar frames list every player once (the pitching and
  # batting pulls return the same roster), so they go in once as kind "any"
  for (fn in c("tm_pitching_totals", "tm_batting_totals")) {
    f <- frames[[fn]]
    if (!is.null(rows$cal_any)) break
    if (is.data.frame(f) && nrow(f) && "playerId" %in% names(f))
      rows$cal_any <- data.frame(
        kind = "any", tm_id = .ref_id(f$trackmanPlayerId), player_id = .ref_id(f$playerId),
        team_id = .ref_id(if ("mostRecentTeamId" %in% names(f)) f$mostRecentTeamId else NA),
        name = pick(f, c("^fullName$", "^player$", "playerName", "^name$")), season_type = NA_character_, source = "calendar",
        tk_level = NA_character_, stringsAsFactors = FALSE)
  }
  if (is.data.frame(trackman) && nrow(trackman))
    rows$trackman <- data.frame(
      kind = as.character(trackman$kind), tm_id = .ref_id(trackman$tm_id), player_id = NA_character_,
      team_id = ref_team_id_for_code(trackman$trackman_code), name = as.character(trackman$name),
      season_type = "Spring", source = "trackman",
      tk_level = if ("tk_level" %in% names(trackman)) as.character(trackman$tk_level) else NA_character_, stringsAsFactors = FALSE)
  d <- dplyr::bind_rows(rows)
  if (!nrow(d)) return(d)
  d <- d[!(is.na(d$tm_id) & is.na(d$player_id)), , drop = FALSE]
  # TruMedia id for the TrackMan-only rows, when the TrackMan id maps to one player
  tm <- d[!is.na(d$tm_id) & !is.na(d$player_id), c("tm_id", "player_id")]
  tm <- unique(tm)
  one <- tm[!(duplicated(tm$tm_id) | duplicated(tm$tm_id, fromLast = TRUE)), , drop = FALSE]
  gap <- is.na(d$player_id) & !is.na(d$tm_id)
  d$player_id[gap] <- one$player_id[match(d$tm_id[gap], one$tm_id)]
  ti <- ref_team_info(team_id = d$team_id, by_name = FALSE)
  d$team <- ti$team; d$level <- ti$level; d$conf <- ti$conf; d$conf_id <- ti$conf_id; d$logo <- ti$logo
  d$level <- ifelse(is.na(d$level) & d$tk_level %in% c("D1", "D2", "D3", "NAIA", "JUCO"), d$tk_level, d$level)
  d$season_type <- ifelse(is.na(d$season_type), ifelse(!is.na(d$level) & d$level == "Summer", "Summer", "Calendar"), d$season_type)
  d$season <- as.integer(season)
  d$key <- .ref_name_key(d$name)
  d <- d[, c("kind", "tm_id", "player_id", "team_id", "team", "level", "conf", "conf_id", "logo", "name", "key", "season", "season_type", "source")]
  d <- d[!duplicated(d[, c("kind", "tm_id", "player_id", "team_id", "season_type")]), , drop = FALSE]
  # collision flags
  n_pid <- tapply(d$player_id, d$tm_id, function(x) length(unique(x[!is.na(x)])))
  n_tm  <- tapply(d$tm_id, d$player_id, function(x) length(unique(x[!is.na(x)])))
  d$tm_id_multi <- !is.na(d$tm_id) & unname(n_pid[d$tm_id]) > 1
  d$player_id_multi <- !is.na(d$player_id) & unname(n_tm[d$player_id]) > 1
  d$uid <- ifelse(is.na(d$tm_id), paste0("p", d$player_id), paste0("t", d$tm_id))
  # same name, different player, same level: the case name matching gets wrong
  grp <- paste(d$level, d$key)
  n_uid <- tapply(d$uid, grp, function(x) length(unique(x)))
  d$name_dupe <- !is.na(d$key) & nzchar(d$key) & unname(n_uid[grp]) > 1
  d$tm_id_multi[is.na(d$tm_id_multi)] <- FALSE; d$player_id_multi[is.na(d$player_id_multi)] <- FALSE
  d$name_dupe[is.na(d$name_dupe)] <- FALSE
  d$uid <- NULL
  rownames(d) <- NULL
  d
}
ref_identity_report <- function(d) {
  if (!is.data.frame(d) || !nrow(d)) { cat("[identity] empty\n"); return(invisible(NULL)) }
  cat("[identity]", nrow(d), "rows |", length(unique(d$tm_id[!is.na(d$tm_id)])), "TrackMan ids |",
      length(unique(d$player_id[!is.na(d$player_id)])), "TruMedia ids |",
      length(unique(d$team_id[!is.na(d$team_id)])), "teams\n")
  s <- d %>% dplyr::group_by(kind, season_type, level = ifelse(is.na(level), "?", level)) %>%
    dplyr::summarise(rows = dplyr::n(), tm_ids = dplyr::n_distinct(tm_id, na.rm = TRUE),
                     player_ids = dplyr::n_distinct(player_id, na.rm = TRUE), teams = dplyr::n_distinct(team_id, na.rm = TRUE),
                     name_dupes = sum(name_dupe), .groups = "drop") %>% as.data.frame()
  print(s, row.names = FALSE)
  cat("[identity] TrackMan ids under >1 TruMedia id:", length(unique(d$tm_id[d$tm_id_multi])),
      "| TruMedia ids under >1 TrackMan id:", length(unique(d$player_id[d$player_id_multi])),
      "| names shared by different players at one level:", sum(!duplicated(paste(d$level, d$key)) & d$name_dupe), "\n")
  invisible(s)
}

# ---- runtime identity lookups ----------------------------------------------------
# The precomputed identity table (lb/<season>/identity.parquet) when the
# build shipped one, else the same table derived from the frames already in
# memory. Cached per season.
ref_identity <- function(season = NULL) {
  if (!exists("lb_precomputed", mode = "function")) return(NULL)
  if (is.null(season)) season <- if (exists("TM_SEASON")) TM_SEASON else 2026L
  key <- as.character(season)
  if (!is.null(.ref_env$identity[[key]])) return(.ref_env$identity[[key]])
  pre <- tryCatch(lb_precomputed(season), error = function(e) list())
  d <- pre$identity
  if (!is.data.frame(d) || !nrow(d)) {
    d <- tryCatch(ref_identity_table(pre, season = season), error = function(e) NULL)
    if (is.data.frame(d) && nrow(d)) cat("[identity] derived", nrow(d), "rows from the precomputed frames\n")
  }
  if (!is.data.frame(d)) d <- data.frame()
  if (is.null(.ref_env$identity)) .ref_env$identity <- list()
  .ref_env$identity[[key]] <- d
  d
}
# Best identity row for a TrackMan id (or a TruMedia id): a spring NCAA row
# first, a calendar-year row after, summer last.
tm_identity <- function(tm_id = NULL, player_id = NULL, kind = NULL, season = NULL) {
  d <- ref_identity(season)
  if (!is.data.frame(d) || !nrow(d)) return(NULL)
  id <- .ref_id(tm_id); pid <- .ref_id(player_id)
  hit <- rep(FALSE, nrow(d))
  if (length(id) && !is.na(id[1])) hit <- hit | (!is.na(d$tm_id) & d$tm_id == id[1])
  if (!any(hit) && length(pid) && !is.na(pid[1])) hit <- !is.na(d$player_id) & d$player_id == pid[1]
  if (!any(hit)) return(NULL)
  r <- d[hit, , drop = FALSE]
  if (!is.null(kind) && any(r$kind %in% c(kind, "any"))) r <- r[r$kind %in% c(kind, "any"), , drop = FALSE]
  r <- r[order(match(r$season_type, c("Spring", "Calendar", "Summer")), match(r$source, c("team", "trackman", "calendar")),
             match(r$level, REF_LEVELS)), , drop = FALSE]
  r[1, , drop = FALSE]
}
