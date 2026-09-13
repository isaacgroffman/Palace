# =============================================================================
# R/18_player_registry.R — every player the app can open, by IDENTITY
#
# One row per player x identity, never per name. A token identifies the row:
#   t:<trackman id>            pitcher with a TrackMan id (games or pens)
#   c:<name key>               Coastal player without an id yet (roster only)
#   n:<name key>|<team code>   NCAA pitcher without an id
#   ht:/hc:/hn:                the same three for hitters
# The header search lists these rows (label, team, seasons, class, logo,
# headshot); picking one sets the legacy name inputs the pages read AND the
# identity (tm_id + team) the loaders, bio card and season table resolve on,
# so two players who share a name never share a page.
# =============================================================================
.reg_env <- new.env(parent = emptyenv())

REG_SEASON_LABELS <- c(Spring25 = "Spring 2025", Fall25 = "Fall 2025", PreSpring26 = "Pre-Spring 2026",
                       Spring26 = "Spring 2026", Fall26 = "Fall 2026")
.reg_key <- function(x) bp_norm_name(x)
.reg_id  <- function(x) { x <- trimws(as.character(x)); x[is.na(x) | !nzchar(x) | x %in% c("NA", "0", "NaN")] <- NA_character_; x }
.reg_looks_like_code <- function(x) !is.na(x) & grepl("^[A-Z0-9_]{3,}$", x)

# ---- pitchers ----------------------------------------------------------------
.reg_build_pitchers <- function() {
  rows <- list()
  # 1) NCAA pairs (games in Supabase): one identity per TrackMan id
  pr <- if (exists("ncaa_dir_pairs") && is.data.frame(ncaa_dir_pairs) && nrow(ncaa_dir_pairs)) ncaa_dir_pairs else NULL
  coastal_ids <- character(0)
  if (!is.null(pr)) {
    pr$PitcherId <- .reg_id(pr$PitcherId)
    pr$key <- .reg_key(pr$display)
    ccu <- !is.na(pr$PitcherTeam) & pr$PitcherTeam == "COA_CHA"
    coastal_ids <- unique(stats::na.omit(pr$PitcherId[ccu]))
    .reg_env$coastal_spring_keys <- unique(pr$key[ccu])
    nc <- pr[!ccu, , drop = FALSE]
    nc$grp <- ifelse(is.na(nc$PitcherId), paste0("n:", nc$key, "|", nc$PitcherTeam), paste0("t:", nc$PitcherId))
    g <- nc %>% dplyr::group_by(grp) %>% dplyr::summarise(
      name = display[1], tm_id = PitcherId[1],
      team_code = { tc <- PitcherTeam[!is.na(PitcherTeam) & nzchar(PitcherTeam)]; if (length(tc)) names(sort(table(tc), decreasing = TRUE))[1] else NA_character_ },
      teams = paste(sort(unique(team_disp[!is.na(team_disp) & nzchar(team_disp)])), collapse = " / "),
      .groups = "drop") %>% as.data.frame()
    rows$ncaa <- data.frame(token = g$grp, kind = "pit", name = g$name, tm_id = g$tm_id, team_code = g$team_code,
                            team = g$teams, seasons = "Spring26", coastal = FALSE, stringsAsFactors = FALSE)
  }
  # 2) Coastal: spring game data + 2027 roster + bullpens, merged by name
  ccu <- list()
  add_ccu <- function(nm, season, tm_id = NA_character_, class = NA_character_, head = NA_character_) {
    nm <- as.character(nm); nm <- nm[!is.na(nm) & nzchar(nm)]
    if (!length(nm)) return()
    ccu[[length(ccu) + 1]] <<- data.frame(name = nm, key = .reg_key(nm), season = season,
                                          tm_id = rep_len(.reg_id(tm_id), length(nm)), class = rep_len(as.character(class), length(nm)),
                                          head = rep_len(as.character(head), length(nm)), stringsAsFactors = FALSE)
  }
  if (exists("spring26") && is.data.frame(spring26) && nrow(spring26) && "Pitcher" %in% names(spring26)) {
    s <- spring26
    if ("PitcherTeam" %in% names(s)) s <- s[is.na(s$PitcherTeam) | s$PitcherTeam == "COA_CHA", , drop = FALSE]
    s <- s[!is.na(s$Pitcher) & nzchar(s$Pitcher), , drop = FALSE]
    ids <- if ("PitcherId" %in% names(s)) tapply(.reg_id(s$PitcherId), s$Pitcher, function(x) { x <- x[!is.na(x)]; if (length(x)) names(sort(table(x), decreasing = TRUE))[1] else NA_character_ }) else NULL
    nms <- unique(s$Pitcher)
    add_ccu(nms, "Spring26", tm_id = if (is.null(ids)) NA_character_ else unname(ids[nms]))
  }
  if (exists("bio") && is.data.frame(bio) && "Pitcher_FL" %in% names(bio)) {
    b <- bio[toupper(trimws(as.character(bio$Roster_2026 %||% "Yes"))) %in% c("YES", "Y", "TRUE", "1"), , drop = FALSE]
    add_ccu(b$Pitcher_FL, "Spring26", class = if ("Class" %in% names(b)) b$Class else NA, head = if ("Headshot" %in% names(b)) b$Headshot else NA)
  }
  if (exists("roster27") && is.data.frame(roster27) && nrow(roster27)) {
    r <- roster27[roster27$IsPitcher, , drop = FALSE]
    add_ccu(r$Name, "Fall26", class = r$Class, head = if ("Headshot" %in% names(r)) r$Headshot else NA)
  }
  bp_ids <- NULL
  if (exists("bullpen_pitchers_fl") && length(bullpen_pitchers_fl)) {
    bp <- tryCatch(bullpen_pitches(), error = function(e) NULL)
    if (is.data.frame(bp) && nrow(bp) && all(c("Pitcher", "PitcherId") %in% names(bp))) {
      nm <- sub("^\\s*([^,]+),\\s*(.+)\\s*$", "\\2 \\1", bp_fix_names(as.character(bp$Pitcher)))
      bp_ids <- tapply(.reg_id(bp$PitcherId), .reg_key(nm), function(x) { x <- x[!is.na(x)]; if (length(x)) names(sort(table(x), decreasing = TRUE))[1] else NA_character_ })
    }
    add_ccu(bullpen_pitchers_fl, "Fall26")
  }
  if (length(ccu)) {
    c0 <- dplyr::bind_rows(ccu)
    if (!is.null(bp_ids)) c0$tm_id <- ifelse(is.na(c0$tm_id), unname(bp_ids[c0$key]), c0$tm_id)
    first_nonna <- function(x) { x <- x[!is.na(x) & nzchar(as.character(x))]; if (length(x)) x[1] else NA_character_ }
    cg <- c0 %>% dplyr::group_by(key) %>% dplyr::summarise(
      name = { n <- name; f <- n[season == "Fall26"]; if (length(f)) f[1] else n[1] },   # roster spelling wins
      tm_id = first_nonna(tm_id), class = first_nonna(class), head = first_nonna(head),
      seasons = paste(unique(season[order(match(season, names(REG_SEASON_LABELS)))]), collapse = "|"),
      .groups = "drop") %>% as.data.frame()
    rows$ccu <- data.frame(token = ifelse(is.na(cg$tm_id), paste0("c:", cg$key), paste0("t:", cg$tm_id)), kind = "pit",
                           name = cg$name, tm_id = cg$tm_id, team_code = "COA_CHA", team = "Coastal Carolina",
                           seasons = cg$seasons, coastal = TRUE, class = cg$class, head = cg$head, stringsAsFactors = FALSE)
  }
  d <- dplyr::bind_rows(rows)
  if (!nrow(d)) return(d)
  if (!"class" %in% names(d)) d$class <- NA_character_
  if (!"head" %in% names(d)) d$head <- NA_character_
  # a Coastal identity beats an NCAA row with the same TrackMan id (his own spring games)
  d <- d[order(!d$coastal), , drop = FALSE]
  d <- d[!duplicated(d$token), , drop = FALSE]
  d
}

# ---- hitters -------------------------------------------------------------------
.reg_build_hitters <- function(require_dir = FALSE) {
  rows <- list()
  ccu <- list()
  if (exists("hitter_bio") && is.data.frame(hitter_bio) && "Batter_FL" %in% names(hitter_bio)) {
    h <- hitter_bio
    ccu$bio <- data.frame(name = h$Batter_FL, season = "Spring26", class = if ("Class" %in% names(h)) as.character(h$Class) else NA_character_,
                          head = if ("Headshot" %in% names(h)) as.character(h$Headshot) else NA_character_, stringsAsFactors = FALSE)
  }
  if (exists("roster27") && is.data.frame(roster27) && nrow(roster27)) {
    r <- roster27[!roster27$IsPitcher, , drop = FALSE]
    ccu$r27 <- data.frame(name = r$Name, season = "Fall26", class = r$Class,
                          head = if ("Headshot" %in% names(r)) as.character(r$Headshot) else NA_character_, stringsAsFactors = FALSE)
  }
  pairs <- if (require_dir || !is.null(.hb_env$pairs)) { tryCatch(ncaa_batter_directory(), error = function(e) NULL); .hb_env$pairs } else NULL
  ccu_ids <- NULL
  if (is.data.frame(pairs) && nrow(pairs)) {
    pairs$BatterId <- .reg_id(pairs$BatterId); pairs$key <- .reg_key(pairs$display)
    isc <- !is.na(pairs$BatterTeam) & pairs$BatterTeam == "COA_CHA"
    ccu_ids <- tapply(pairs$BatterId[isc], pairs$key[isc], function(x) { x <- x[!is.na(x)]; if (length(x)) x[1] else NA_character_ })
    if (any(isc)) ccu$games <- data.frame(name = unique(pairs$display[isc]), season = "Spring26", class = NA_character_, head = NA_character_, stringsAsFactors = FALSE)
    nc <- pairs[!isc, , drop = FALSE]
    nc$grp <- ifelse(is.na(nc$BatterId), paste0("hn:", nc$key, "|", nc$BatterTeam), paste0("ht:", nc$BatterId))
    g <- nc %>% dplyr::group_by(grp) %>% dplyr::summarise(
      name = display[1], tm_id = BatterId[1],
      team_code = { tc <- BatterTeam[!is.na(BatterTeam) & nzchar(BatterTeam)]; if (length(tc)) names(sort(table(tc), decreasing = TRUE))[1] else NA_character_ },
      teams = paste(sort(unique(team_disp[!is.na(team_disp) & nzchar(team_disp)])), collapse = " / "),
      .groups = "drop") %>% as.data.frame()
    rows$ncaa <- data.frame(token = g$grp, kind = "bat", name = g$name, tm_id = g$tm_id, team_code = g$team_code,
                            team = g$teams, seasons = "Spring26", coastal = FALSE, class = NA_character_, head = NA_character_, stringsAsFactors = FALSE)
  }
  if (length(ccu)) {
    c0 <- dplyr::bind_rows(ccu); c0 <- c0[!is.na(c0$name) & nzchar(c0$name), , drop = FALSE]; c0$key <- .reg_key(c0$name)
    first_nonna <- function(x) { x <- x[!is.na(x) & nzchar(as.character(x))]; if (length(x)) x[1] else NA_character_ }
    cg <- c0 %>% dplyr::group_by(key) %>% dplyr::summarise(
      name = { n <- name; f <- n[season == "Fall26"]; if (length(f)) f[1] else n[1] },
      class = first_nonna(class), head = first_nonna(head),
      seasons = paste(unique(season[order(match(season, names(REG_SEASON_LABELS)))]), collapse = "|"), .groups = "drop") %>% as.data.frame()
    cg$tm_id <- if (!is.null(ccu_ids)) unname(ccu_ids[cg$key]) else NA_character_
    rows$ccu <- data.frame(token = ifelse(is.na(cg$tm_id), paste0("hc:", cg$key), paste0("ht:", cg$tm_id)), kind = "bat",
                           name = cg$name, tm_id = cg$tm_id, team_code = "COA_CHA", team = "Coastal Carolina",
                           seasons = cg$seasons, coastal = TRUE, class = cg$class, head = cg$head, stringsAsFactors = FALSE)
  }
  d <- dplyr::bind_rows(rows)
  if (!nrow(d)) return(d)
  d <- d[order(!d$coastal), , drop = FALSE]
  d[!duplicated(d$token), , drop = FALSE]
}

# ---- enrichment: reference team, identity, bio -------------------------------------
.reg_enrich <- function(d) {
  if (!nrow(d)) return(d)
  # team through the crosswalk / reference: level, conference, logo, a real name
  ti <- ref_team_info(code = d$team_code, name = d$team, by_name = TRUE)
  d$team_id <- ti$team_id; d$level <- ti$level; d$conf <- ti$conf; d$logo <- ti$logo
  d$team <- ifelse(is.na(d$team) | !nzchar(d$team) | .reg_looks_like_code(d$team), ifelse(is.na(ti$team), d$team, ti$team), d$team)
  d$team[is.na(d$team)] <- ""
  if (any(d$coastal)) { d$level[d$coastal] <- "D1"; d$conf[d$coastal] <- "SBELT"; d$logo[d$coastal] <- tryCatch(ccu_logo_url, error = function(e) NA_character_) }
  # identity table: TruMedia player id (class, headshot), summer clubs
  idt <- tryCatch(ref_identity(), error = function(e) NULL)
  d$player_id <- NA_character_; d$summer <- FALSE
  if (is.data.frame(idt) && nrow(idt) && any(!is.na(d$tm_id))) {
    ok <- idt[!is.na(idt$tm_id) & !is.na(idt$player_id), , drop = FALSE]
    ok <- ok[order(match(ok$season_type, c("Spring", "Calendar", "Summer"))), , drop = FALSE]
    d$player_id <- ok$player_id[match(d$tm_id, ok$tm_id)]
    sm <- unique(idt$tm_id[!is.na(idt$tm_id) & idt$season_type %in% "Summer"])
    d$summer <- !is.na(d$tm_id) & d$tm_id %in% sm
  }
  if (exists("REF_PLAYERS") && !is.null(REF_PLAYERS)) {
    ri <- match(d$player_id, REF_PLAYERS$player_id)
    gap <- is.na(ri) & !d$coastal
    if (any(gap)) ri[gap] <- match(paste(.ref_name_key(d$name), d$team_id)[gap], paste(REF_PLAYERS$key, REF_PLAYERS$team_id))
    cls <- REF_PLAYERS$class[ri]; hd <- REF_PLAYERS$headshot_url[ri]
    d$class <- ifelse(is.na(d$class) | !nzchar(d$class), cls, d$class)
    d$head  <- ifelse(is.na(d$head) | !nzchar(d$head), hd, d$head)
  }
  d$class <- ifelse(is.na(d$class), "", as.character(d$class))
  d$head  <- ifelse(is.na(d$head), "", as.character(d$head))
  d$logo  <- ifelse(is.na(d$logo), "", as.character(d$logo))
  # labels
  sl <- vapply(strsplit(d$seasons, "|", fixed = TRUE), function(s) paste(REG_SEASON_LABELS[s][!is.na(REG_SEASON_LABELS[s])], collapse = " · "), character(1))
  sl <- ifelse(d$summer, paste0(sl, " · Summer 2026"), sl)
  d$meta  <- trimws(paste0(sl, ifelse(nzchar(d$class), paste0(" · ", d$class), ""),
                           ifelse(!is.na(d$conf) & nzchar(d$conf) & !d$coastal, paste0(" · ", d$conf), "")))
  d$label <- paste0(d$name, " — ", d$team, if (TRUE) ifelse(d$kind == "bat", " (Hitter)", ""))
  d$value <- d$token
  rownames(d) <- NULL
  d
}

# ---- public --------------------------------------------------------------------------
# Cached per process. Hitters from the NCAA batter directory join once that
# directory has been indexed (it is a distinct over the per-pitch table);
# player_registry(with_hitters = TRUE) forces the index.
player_registry <- function(with_hitters = FALSE, refresh = FALSE) {
  need_h <- with_hitters && !isTRUE(.reg_env$has_ncaa_hitters)
  if (!refresh && !need_h && !is.null(.reg_env$reg)) return(.reg_env$reg)
  t0 <- Sys.time()
  p <- tryCatch(.reg_build_pitchers(), error = function(e) { cat("[registry] pitchers failed:", conditionMessage(e), "\n"); data.frame() })
  h <- tryCatch(.reg_build_hitters(require_dir = with_hitters), error = function(e) { cat("[registry] hitters failed:", conditionMessage(e), "\n"); data.frame() })
  d <- dplyr::bind_rows(p, h)
  d <- tryCatch(.reg_enrich(d), error = function(e) { cat("[registry] enrich failed:", conditionMessage(e), "\n"); d })
  # Coastal first, then by name
  d <- d[order(!d$coastal, d$kind != "pit", d$name), , drop = FALSE]
  .reg_env$reg <- d
  .reg_env$has_ncaa_hitters <- is.data.frame(.hb_env$pairs) && nrow(.hb_env$pairs) > 0
  cat(sprintf("[registry] %d players (%d pitchers, %d hitters, %d Coastal) in %.1fs\n", nrow(d), sum(d$kind == "pit"),
              sum(d$kind == "bat"), sum(d$coastal), as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  d
}
registry_row <- function(token) {
  if (is.null(token) || !length(token) || is.na(token[1]) || !nzchar(token[1])) return(NULL)
  d <- player_registry()
  r <- d[d$token == token[1], , drop = FALSE]
  if (nrow(r)) r[1, , drop = FALSE] else NULL
}
# the data frame the header search is fed (value / label + render fields)
registry_choices <- function() {
  d <- player_registry()
  if (!nrow(d)) return(data.frame(value = character(0), label = character(0)))
  d[, c("value", "label", "name", "team", "meta", "logo", "head", "kind")]
}
# Resolve a legacy pick (a plain name, an HB:: name, or a name + TrackMan id
# from a link) to a registry token. Coastal identities win when the Fall
# 2026 context is on; an id always wins.
registry_token_for <- function(name = NULL, tm_id = NULL, kind = c("pit", "bat"), prefer_coastal = FALSE, team = NULL) {
  kind <- match.arg(kind)
  d <- player_registry()
  if (!nrow(d)) return(NULL)
  id <- .reg_id(tm_id)
  if (length(id) && !is.na(id[1])) {
    r <- d[!is.na(d$tm_id) & d$tm_id == id[1] & d$kind == kind, , drop = FALSE]
    if (nrow(r)) return(r$token[order(!r$coastal)][1])
  }
  nm <- as.character(name %||% ""); if (is_hitter_pick(nm)) { nm <- sub(paste0("^", HB_PREFIX), "", nm); kind <- "bat" }
  if (!nzchar(nm)) return(NULL)
  r <- d[d$kind == kind & (d$name == nm | .reg_key(d$name) == .reg_key(nm)), , drop = FALSE]
  if (!nrow(r)) return(NULL)
  if (!is.null(team) && nzchar(team %||% "")) {
    tk <- .conf_norm(team)
    hit <- .conf_norm(r$team) == tk | (!is.na(r$team_code) & toupper(r$team_code) == toupper(team))
    if (any(hit)) r <- r[hit, , drop = FALSE]
  }
  r <- r[order(if (prefer_coastal) !r$coastal else r$coastal, is.na(r$tm_id)), , drop = FALSE]
  r$token[1]
}
