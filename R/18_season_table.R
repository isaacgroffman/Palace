# =============================================================================
# R/21_season_table.R — the player-page "season table"
#
# Replaces the one-line traditional-stats strip with a table of SEASON ROWS:
# one row per year x team x league, spring NCAA lines separate from summer
# clubs, ordered by year then Spring -> Summer -> Fall, so a transfer's old
# school, his summer league and his current club never blend together.
#
#   Traditional stats  TruMedia. Team-scoped rows (lb/<season>/tm_*_team,
#                      built by scripts/build_leaderboards.R with the TM_*
#                      secrets) when they exist; a live per-team PlayerTotals
#                      pull otherwise; the league-wide calendar-year frame
#                      only as a last resort (flagged "calendar year").
#   TrackMan columns   the precomputed lb/<season>/pitchers|hitters tables
#                      (Stuff+, Pitching+, Location+, RV, xBA/xSLG/xwOBA,
#                      whiff/chase/zone, EV90, HardHit%, xwOBAcon) on the
#                      spring NCAA row of the scored season.
#   Fall bullpens      the Supabase practice frame, as its own row.
#
# Cells are coloured green / white / red by percentile against every
# qualified D1 arm (or bat) on the precomputed board.
# =============================================================================

.ps_env <- new.env(parent = emptyenv())

PS_SEASON_ORDER <- c(Spring = 1, Summer = 2, Fall = 3)

# ---- lookups ------------------------------------------------------------------
.ps_norm <- function(x) gsub("[^a-z0-9]", "", tolower(as.character(x)))
.ps_num  <- function(x) suppressWarnings(as.numeric(as.character(x)))

# an NCAA team's conference code (merged_teams.csv) or NA for a summer club
ps_team_conf <- function(team) {
  if (is.null(team) || is.na(team) || !nzchar(team)) return(NA_character_)
  m <- team_name_map[.conf_norm(team_name_map$team_name) == .conf_norm(team), , drop = FALSE]
  if (nrow(m) && !is.na(m$conference[1]) && nzchar(m$conference[1]) && m$conference[1] != "Team")
    return(as.character(m$conference[1]))
  if (!is.null(hs_bio) && all(c("team_name", "conference") %in% names(hs_bio))) {
    h <- hs_bio[.conf_norm(hs_bio$team_name) == .conf_norm(team), , drop = FALSE]
    if (nrow(h) && !is.na(h$conference[1]) && nzchar(h$conference[1])) return(as.character(h$conference[1]))
  }
  NA_character_
}
ps_team_is_ncaa <- function(team) {
  if (is.null(team) || is.na(team) || !nzchar(team)) return(FALSE)
  .conf_norm(team) %in% .conf_norm(team_name_map$team_name) ||
    (!is.null(hs_bio) && "team_name" %in% names(hs_bio) && .conf_norm(team) %in% .conf_norm(hs_bio$team_name))
}

# TrackMan pitcher id for a display name (any team, Coastal included)
ps_pitcher_id <- function(name) {
  if (!exists("ncaa_dir_pairs") || !is.data.frame(ncaa_dir_pairs) || !nrow(ncaa_dir_pairs)) return(NA_character_)
  r <- ncaa_dir_pairs[ncaa_dir_pairs$display == name & !is.na(ncaa_dir_pairs$PitcherId), , drop = FALSE]
  if (!nrow(r)) r <- ncaa_dir_pairs[bp_norm_name(ncaa_dir_pairs$display) == bp_norm_name(name) &
                                      !is.na(ncaa_dir_pairs$PitcherId), , drop = FALSE]
  if (!nrow(r)) return(NA_character_)
  id <- as.character(r$PitcherId); id <- id[nzchar(id) & !id %in% c("NA", "0")]
  if (length(id)) id[1] else NA_character_
}

# TruMedia's name spelling for a player -> row(s) of a PlayerTotals frame
.ps_rows_for <- function(df, name, id = NA_character_) {
  if (is.null(df) || !nrow(df)) return(df[0, , drop = FALSE])
  hit <- rep(FALSE, nrow(df))
  if (!is.na(id) && "trackmanPlayerId" %in% names(df))
    hit <- hit | (!is.na(df$trackmanPlayerId) & as.character(df$trackmanPlayerId) == id)
  if (!any(hit)) {
    nc <- .tm_pick_col(df, c("fullName", "playerName", "^player$", "^name$"))
    if (!is.null(nc)) hit <- .tm_norm(df[[nc]]) == .tm_norm(name)
  }
  df[hit, , drop = FALSE]
}

# The teams a player is known to have played for (any season): current
# TrackMan team(s), DRS / headshot bios, the calendar-year "most recent"
# team on the precomputed frames. Candidate list for live per-team pulls.
ps_candidate_teams <- function(name, kind = c("pit", "bat"), id = NA_character_) {
  kind <- match.arg(kind)
  out <- character(0)
  add <- function(x) out <<- c(out, as.character(x[!is.na(x) & nzchar(as.character(x))]))
  if (kind == "pit") {
    add(tryCatch(tm_current_team(name), error = function(e) NULL))
    if (exists("ncaa_dir_pairs") && is.data.frame(ncaa_dir_pairs) && nrow(ncaa_dir_pairs)) {
      r <- ncaa_dir_pairs[ncaa_dir_pairs$display == name | (!is.na(id) & ncaa_dir_pairs$PitcherId %in% id), , drop = FALSE]
      add(r$team_disp)
    }
    if (isTRUE(is_coastal_pitcher(name)) || isTRUE(is_fall26_pitcher(name))) add("Coastal Carolina")
  } else {
    add(tryCatch(tm_current_batter_team(name), error = function(e) NULL))
    if (isTRUE(is_coastal_hitter(name))) add("Coastal Carolina")
  }
  r <- tryCatch(drs_lookup(name), error = function(e) NULL)
  if (!is.null(r)) { add(r$newestTeamName); add(r$team_name) }
  h <- tryCatch(headshot_lookup(name), error = function(e) NULL)
  if (!is.null(h) && !is.null(h$team_name)) add(h$team_name)
  pre <- tryCatch(lb_precomputed(TM_SEASON), error = function(e) list())
  fr <- if (kind == "pit") pre$tm_pitching_totals else pre$tm_batting_totals
  rr <- .ps_rows_for(fr, name, id)
  if (nrow(rr) && "mostRecentTeamName" %in% names(rr)) add(rr$mostRecentTeamName)
  unique(out)
}

# ---- one season's TruMedia rows ------------------------------------------------
# returns a data.frame with one row per team the player has a line for, in
# the API's raw column vocabulary plus Team / TeamId / Source
ps_tm_rows <- function(name, yr, kind = c("pit", "bat"), id = NA_character_) {
  kind <- match.arg(kind)
  pre <- tryCatch(lb_precomputed(yr), error = function(e) list())
  team_frame <- if (kind == "pit") pre$tm_pitching_team else pre$tm_batting_team
  wide_frame <- if (kind == "pit") pre$tm_pitching_totals else pre$tm_batting_totals
  team_name_of <- function(ids) {
    tt <- pre$teams; if (is.null(tt)) return(rep(NA_character_, length(ids)))
    nc <- intersect(c("location", "fullName", "teamName"), names(tt))
    as.character(tt[[nc[1]]])[match(as.character(ids), as.character(tt$teamId))]
  }
  # 1) team-scoped precomputed rows
  if (is.data.frame(team_frame) && nrow(team_frame)) {
    rr <- .ps_rows_for(team_frame, name, id)
    if (nrow(rr)) {
      rr$Team <- team_name_of(rr$teamId); rr$TeamId <- as.character(rr$teamId); rr$Source <- "team"
      return(rr)
    }
  }
  # 2) live, per candidate team
  if (tm_enabled()) {
    cols <- if (kind == "pit") TM_TRAD_COLS else TM_BAT_TRAD_COLS
    tok <- pre$manifest$tokens
    rate_cols <- if (kind == "pit") {
      t <- unlist(tok$pitching %||% list()); if (length(t)) paste0("[", t, "]", collapse = ",") else NULL
    } else {
      t <- unlist(tok$batting %||% list()); if (length(t)) paste0("[", unique(c("PA", t)), "]", collapse = ",") else NULL
    }
    rows <- list()
    for (team in ps_candidate_teams(name, kind, id)) {
      tid <- tryCatch(tm_with_season(yr, tm_find_team_id(team)), error = function(e) NULL)
      if (is.null(tid)) next
      df <- tryCatch(tm_with_season(yr, .ps_live_totals(tid, cols)), error = function(e) NULL)
      row <- tm_player_row(df, name)
      if (is.null(row)) next
      if (!is.null(rate_cols)) {
        rd <- tryCatch(tm_with_season(yr, .ps_live_totals(tid, rate_cols)), error = function(e) NULL)
        rrow <- tm_player_row(rd, name)
        if (!is.null(rrow)) for (cc in setdiff(names(rrow), names(row))) row[[cc]] <- rrow[[cc]]
      }
      row$Team <- team; row$TeamId <- as.character(tid); row$Source <- "live"
      rows[[length(rows) + 1]] <- row
    }
    if (length(rows)) {
      out <- dplyr::bind_rows(rows)
      return(out[!duplicated(out$TeamId), , drop = FALSE])
    }
  }
  # 3) league-wide calendar-year row
  if (is.data.frame(wide_frame) && nrow(wide_frame)) {
    rr <- .ps_rows_for(wide_frame, name, id)
    if (nrow(rr)) {
      rr$Team <- if ("mostRecentTeamName" %in% names(rr)) as.character(rr$mostRecentTeamName) else NA_character_
      rr$TeamId <- if ("mostRecentTeamId" %in% names(rr)) as.character(rr$mostRecentTeamId) else NA_character_
      rr$Source <- "calendar"
      return(rr[1, , drop = FALSE])
    }
  }
  NULL
}
# a per-team PlayerTotals pull that bypasses the precomputed shortcut
.ps_live_totals <- function(team_id, columns) {
  for (pn in c("teamId", "team")) {
    params <- list(seasonYear = tm_season(), format = "RAW", columns = columns)
    params[[pn]] <- team_id
    out <- tm_get_csv("PlayerTotals", params = params)
    if (!is.null(out)) return(out)
  }
  NULL
}

# ---- season rows, pitcher ----------------------------------------------------------
ps_bio_age <- function(name) {
  r <- tryCatch(drs_lookup(name), error = function(e) NULL)
  a <- if (!is.null(r) && "Age" %in% names(r)) .ps_num(r$Age) else NA_real_
  if (!is.finite(a) || a <= 0) {
    h <- if (!is.null(hs_bio)) hs_bio[hs_bio$hs_key == norm_player_key(name), , drop = FALSE] else NULL
    if (!is.null(h) && nrow(h) && "age" %in% names(h)) a <- .ps_num(h$age[1])
  }
  if (is.finite(a) && a > 0) a else NA_real_
}

ps_rate <- function(num, den, digits = 1) {
  v <- ifelse(is.finite(den) & den > 0 & is.finite(num), 100 * num / den, NA_real_); round(v, digits)
}

season_rows_pitcher <- function(name, seasons = c(TM_SEASON - 1, TM_SEASON), include_bullpens = TRUE) {
  if (is.null(name) || !nzchar(name)) return(NULL)
  key <- paste("pit", name, paste(seasons, collapse = ","), include_bullpens)
  hit <- .ps_env[[key]]
  if (!is.null(hit) && difftime(Sys.time(), hit$at, units = "mins") < 30) return(hit$rows)
  id  <- ps_pitcher_id(name)
  age <- ps_bio_age(name)
  out <- list()
  for (yr in seasons) {
    rr <- tryCatch(ps_tm_rows(name, yr, "pit", id), error = function(e) NULL)
    if (is.null(rr) || !nrow(rr)) next
    g <- function(ab, i) { v <- tm_stat(rr[i, , drop = FALSE], ab); if (length(v) == 0) NA_real_ else .ps_num(v) }
    for (i in seq_len(nrow(rr))) {
      team <- as.character(rr$Team[i]); ncaa <- ps_team_is_ncaa(team) || identical(as.character(rr$Source[i]), "calendar")
      pa <- g("PA", i); k <- g("K", i); bb <- g("BB", i); hbp <- g("HBP", i); hr <- g("HR", i); h <- g("H", i)
      sw <- g("SWING", i); miss <- g("MISS", i); ch <- g("CHASE", i); ooz <- g("OUTOFZONE", i); p <- g("P", i)
      d2 <- g("2B", i); d3 <- g("3B", i); ip_raw <- g("IP", i)
      ip_dec <- tm_ip_decimal(ip_raw)
      h1 <- h - d2 - d3 - hr
      row <- data.frame(
        Year = yr, Season = if (ncaa) "Spring" else "Summer", Team = team,
        LG = if (ncaa) ps_team_conf(team) %||% NA_character_ else "Summer",
        Age = if (is.finite(age)) age - (TM_SEASON - yr) else NA_real_,
        G = g("G", i), GS = g("GS", i), W = g("W", i), L = g("L", i), SV = g("SV", i),
        IP = tm_ip_display(ip_raw), H = h, R = g("R", i), ER = g("ER", i), BB = bb, K = k, HBP = hbp, HR = hr,
        ERA = round(g("ERA", i), 2), WHIP = round(g("WHIP", i), 2), BAA = round(g("BA", i), 3),
        PA = pa, P = p,
        `K%` = ps_rate(k, pa), `BB%` = ps_rate(bb, pa), `Whiff%` = ps_rate(miss, sw),
        `Chase%` = ps_rate(ch, ooz), `Zone%` = ps_rate(pmax(p - ooz, 0), p),
        wOBA = round(ifelse(is.finite(pa) & pa > 0,
                            (LB_WOBA_W[["bb"]] * bb + LB_WOBA_W[["hbp"]] * hbp + 0.899 * h1 + 1.21 * d2 + 1.436 * d3 + 1.781 * hr) / pa,
                            NA_real_), 3),
        FIP = round(ifelse(is.finite(ip_dec) & ip_dec > 0, (13 * hr + 3 * (bb + hbp) - 2 * k) / ip_dec + 3.10, NA_real_), 2),
        `Stuff+` = NA_real_, `Pitching+` = NA_real_, `Location+` = NA_real_, RV = NA_real_, `FB Velo` = NA_real_,
        xBA = NA_real_, xSLG = NA_real_, xwOBA = NA_real_,
        Source = as.character(rr$Source[i]), check.names = FALSE, stringsAsFactors = FALSE)
      out[[length(out) + 1]] <- row
    }
  }
  rows <- if (length(out)) dplyr::bind_rows(out) else NULL

  # TrackMan model columns on the scored season's spring row
  pre <- tryCatch(lb_precomputed(TM_SEASON), error = function(e) list())
  tk <- pre$pitchers
  if (is.data.frame(tk) && nrow(tk)) {
    j <- if (!is.na(id)) which(as.character(tk$tm_id) == id) else integer(0)
    if (!length(j)) j <- which(.tm_norm(tk$Pitcher) == .tm_norm(name))
    if (length(j)) {
      j <- j[1]
      tk_team <- as.character(tk$School[j])
      tk_conf <- ps_team_conf(tk_team); if (is.na(tk_conf) && "Conf" %in% names(tk)) tk_conf <- as.character(tk$Conf[j])
      tkrow <- data.frame(Year = TM_SEASON, Season = "Spring", Team = tk_team, LG = tk_conf,
                          Age = if (is.finite(age)) age else NA_real_, Source = "trackman",
                          check.names = FALSE, stringsAsFactors = FALSE)
      cols <- c("Stuff+", "Pitching+", "Location+", "RV", "FB Velo", "xBA", "xSLG", "xwOBA", "Whiff%", "Chase%", "Zone%", "P")
      for (cc in cols) tkrow[[cc]] <- .ps_num(tk[[cc]][j])
      # attach to the matching spring row when there is one, else own row
      target <- if (!is.null(rows)) which(rows$Year == TM_SEASON & rows$Season == "Spring" &
                                           (.conf_norm(rows$Team) == .conf_norm(tk_team) | rows$Source == "calendar")) else integer(0)
      if (length(target)) {
        t <- target[1]
        for (cc in cols) if (is.finite(tkrow[[cc]]) && (!cc %in% c("Whiff%", "Chase%", "Zone%", "P") || !is.finite(rows[[cc]][t])))
          rows[[cc]][t] <- tkrow[[cc]]
        if (rows$Source[t] == "calendar" || is.na(rows$LG[t])) { rows$Team[t] <- tk_team; rows$LG[t] <- tk_conf }
      } else {
        rows <- dplyr::bind_rows(rows, tkrow)
      }
    }
  }

  # Fall bullpens (Coastal practice sessions) as their own row
  if (include_bullpens) {
    bp <- tryCatch(bullpen_pitches(), error = function(e) NULL)
    if (is.data.frame(bp) && nrow(bp)) {
      mine <- bp[bp_norm_name(bp$Pitcher) == bp_norm_name(name), , drop = FALSE]
      if (nrow(mine)) {
        fbv <- mine$RelSpeed[mine$TaggedPitchType %in% LB_FB_TYPES & is.finite(mine$RelSpeed)]
        yr <- as.integer(format(max(mine$Date, na.rm = TRUE), "%Y"))
        rows <- dplyr::bind_rows(rows, data.frame(
          Year = yr, Season = "Fall", Team = "Coastal Carolina", LG = "Bullpens",
          Age = if (is.finite(age)) age - (TM_SEASON - yr) else NA_real_,
          G = length(unique(mine$session_id)), P = nrow(mine),
          `FB Velo` = if (length(fbv)) round(mean(fbv), 1) else NA_real_,
          Source = "bullpen", check.names = FALSE, stringsAsFactors = FALSE))
      }
    }
  }
  if (!is.null(rows) && nrow(rows)) {
    if (!"IP" %in% names(rows)) rows$IP <- NA_real_
    rows <- rows[order(rows$Year, PS_SEASON_ORDER[rows$Season], -ifelse(is.finite(rows$IP), rows$IP, 0)), , drop = FALSE]
    rows$LG[is.na(rows$LG) | !nzchar(rows$LG)] <- "—"
  }
  .ps_env[[key]] <- list(rows = rows, at = Sys.time())
  rows
}

# ---- season rows, hitter -----------------------------------------------------------
season_rows_batter <- function(name, seasons = c(TM_SEASON - 1, TM_SEASON)) {
  if (is.null(name) || !nzchar(name)) return(NULL)
  key <- paste("bat", name, paste(seasons, collapse = ","))
  hit <- .ps_env[[key]]
  if (!is.null(hit) && difftime(Sys.time(), hit$at, units = "mins") < 30) return(hit$rows)
  age <- ps_bio_age(name)
  out <- list()
  for (yr in seasons) {
    rr <- tryCatch(ps_tm_rows(name, yr, "bat"), error = function(e) NULL)
    if (is.null(rr) || !nrow(rr)) next
    g <- function(ab, i) { v <- tm_stat(rr[i, , drop = FALSE], ab); if (length(v) == 0) NA_real_ else .ps_num(v) }
    for (i in seq_len(nrow(rr))) {
      team <- as.character(rr$Team[i]); ncaa <- ps_team_is_ncaa(team) || identical(as.character(rr$Source[i]), "calendar")
      pa <- g("PA", i); ab <- g("AB", i); h <- g("H", i); d2 <- g("2B", i); d3 <- g("3B", i); hr <- g("HR", i)
      bb <- g("BB", i); hbp <- g("HBP", i); k <- g("K", i)
      sw <- g("SWING", i); con <- g("CONTACT", i); miss <- g("MISS", i); ch <- g("CHASE", i); ooz <- g("OUTOFZONE", i)
      avg <- g("BA", i); obp <- g("OBP", i); slg <- g("SLG", i)
      h1 <- h - d2 - d3 - hr
      row <- data.frame(
        Year = yr, Season = if (ncaa) "Spring" else "Summer", Team = team,
        LG = if (ncaa) ps_team_conf(team) %||% NA_character_ else "Summer",
        Age = if (is.finite(age)) age - (TM_SEASON - yr) else NA_real_,
        G = g("G", i), PA = pa, AB = ab, H = h, `2B` = d2, `3B` = d3, HR = hr, BB = bb, HBP = hbp, K = k, SB = g("SB", i),
        AVG = round(avg, 3), OBP = round(obp, 3), SLG = round(slg, 3), OPS = round(obp + slg, 3),
        `BB%` = ps_rate(bb, pa), `K%` = ps_rate(k, pa),
        BABIP = round(ifelse(is.finite(ab) & (ab - k - hr) > 0, (h - hr) / (ab - k - hr), NA_real_), 3),
        ISO = round(slg - avg, 3),
        wOBA = round(ifelse(is.finite(pa) & pa > 0,
                            (LB_WOBA_W[["bb"]] * bb + LB_WOBA_W[["hbp"]] * hbp + 0.899 * h1 + 1.21 * d2 + 1.436 * d3 + 1.781 * hr) / pa,
                            NA_real_), 3),
        `Whiff%` = ps_rate(miss, sw), `Chase%` = ps_rate(ch, ooz), `Contact%` = ps_rate(con, sw),
        xwOBA = round(g("XWOBA", i), 3),
        EV90 = NA_real_, `HardHit%` = NA_real_, xwOBAcon = NA_real_, `Z-Whiff%` = NA_real_,
        Source = as.character(rr$Source[i]), check.names = FALSE, stringsAsFactors = FALSE)
      out[[length(out) + 1]] <- row
    }
  }
  rows <- if (length(out)) dplyr::bind_rows(out) else NULL
  pre <- tryCatch(lb_precomputed(TM_SEASON), error = function(e) list())
  tk <- pre$hitters
  if (is.data.frame(tk) && nrow(tk)) {
    j <- which(.tm_norm(tk$Batter) == .tm_norm(name))
    if (length(j) > 1) {
      teams <- if (!is.null(rows)) .conf_norm(rows$Team) else character(0)
      pref <- j[.conf_norm(tk$School[j]) %in% teams]; if (length(pref)) j <- pref
    }
    if (length(j)) {
      j <- j[1]; tk_team <- as.character(tk$School[j])
      tk_conf <- ps_team_conf(tk_team); if (is.na(tk_conf) && "Conf" %in% names(tk)) tk_conf <- as.character(tk$Conf[j])
      cols <- c("EV90", "HardHit%", "xwOBAcon", "Z-Whiff%", "Whiff%", "Chase%", "Contact%")
      vals <- lapply(cols, function(cc) .ps_num(tk[[cc]][j])); names(vals) <- cols
      target <- if (!is.null(rows)) which(rows$Year == TM_SEASON & rows$Season == "Spring" &
                                           (.conf_norm(rows$Team) == .conf_norm(tk_team) | rows$Source == "calendar")) else integer(0)
      if (length(target)) {
        t <- target[1]
        for (cc in cols) if (is.finite(vals[[cc]]) && (!cc %in% c("Whiff%", "Chase%", "Contact%") || !is.finite(rows[[cc]][t])))
          rows[[cc]][t] <- vals[[cc]]
        if (rows$Source[t] == "calendar" || is.na(rows$LG[t])) { rows$Team[t] <- tk_team; rows$LG[t] <- tk_conf }
      } else {
        tkrow <- data.frame(Year = TM_SEASON, Season = "Spring", Team = tk_team, LG = tk_conf,
                            Age = if (is.finite(age)) age else NA_real_, Source = "trackman",
                            check.names = FALSE, stringsAsFactors = FALSE)
        for (cc in cols) tkrow[[cc]] <- vals[[cc]]
        rows <- dplyr::bind_rows(rows, tkrow)
      }
    }
  }
  if (!is.null(rows) && nrow(rows)) {
    if (!"PA" %in% names(rows)) rows$PA <- NA_real_
    rows <- rows[order(rows$Year, PS_SEASON_ORDER[rows$Season], -ifelse(is.finite(rows$PA), rows$PA, 0)), , drop = FALSE]
    rows$LG[is.na(rows$LG) | !nzchar(rows$LG)] <- "—"
  }
  .ps_env[[key]] <- list(rows = rows, at = Sys.time())
  rows
}

# ---- column sets -----------------------------------------------------------------------
# dir: +1 higher is better, -1 lower is better, 0 uncoloured; dig: decimals
PS_COLS <- list(
  pit = list(
    standard = list(G = c(0, 0), GS = c(0, 0), W = c(0, 0), L = c(0, 0), SV = c(0, 0), IP = c(0, 1), H = c(0, 0), R = c(0, 0),
                    ER = c(0, 0), BB = c(0, 0), K = c(0, 0), HBP = c(0, 0), HR = c(0, 0),
                    ERA = c(-1, 2), WHIP = c(-1, 2), BAA = c(-1, 3)),
    advanced = list(G = c(0, 0), IP = c(0, 1), P = c(0, 0), `K%` = c(1, 1), `BB%` = c(-1, 1), `Whiff%` = c(1, 1), `Chase%` = c(1, 1),
                    `Zone%` = c(1, 1), wOBA = c(-1, 3), ERA = c(-1, 2), FIP = c(-1, 2), RV = c(-1, 2),
                    `FB Velo` = c(1, 1), `Stuff+` = c(1, 0), `Pitching+` = c(1, 0), `Location+` = c(1, 0)),
    expected = list(xBA = c(-1, 3), xSLG = c(-1, 3), xwOBA = c(-1, 3))
  ),
  bat = list(
    standard = list(G = c(0, 0), PA = c(0, 0), AB = c(0, 0), H = c(0, 0), `2B` = c(0, 0), `3B` = c(0, 0), HR = c(0, 0), BB = c(0, 0),
                    HBP = c(0, 0), K = c(0, 0), SB = c(0, 0), AVG = c(1, 3), OBP = c(1, 3), SLG = c(1, 3), OPS = c(1, 3)),
    advanced = list(G = c(0, 0), PA = c(0, 0), AVG = c(1, 3), OBP = c(1, 3), SLG = c(1, 3), OPS = c(1, 3), `BB%` = c(1, 1), `K%` = c(-1, 1),
                    BABIP = c(1, 3), ISO = c(1, 3), wOBA = c(1, 3), `Whiff%` = c(-1, 1), `Chase%` = c(-1, 1), `Contact%` = c(1, 1),
                    EV90 = c(1, 1), `HardHit%` = c(1, 1)),
    expected = list(xwOBA = c(1, 3), xwOBAcon = c(1, 3))
  )
)
# board column that holds the D1 distribution for a table column
.PS_REF_COL <- c(BAA = "xBA", AVG = "BA", `FB Velo` = "FB Velo")

# percentile of a value against the qualified D1 population of the board
ps_pct <- function(kind, col, v) {
  if (!is.finite(v)) return(NA_real_)
  ck <- paste(kind, col)
  f <- .ps_env$ecdf[[ck]]
  if (is.null(f)) {
    pre <- tryCatch(lb_precomputed(TM_SEASON), error = function(e) list())
    ref <- if (kind == "pit") pre$pitchers else pre$hitters
    rc <- if (col %in% names(.PS_REF_COL)) .PS_REF_COL[[col]] else col
    if (!is.data.frame(ref) || !rc %in% names(ref)) return(NA_real_)
    qual <- if (kind == "pit") .ps_num(ref$IP) >= 10 else .ps_num(ref$PA) >= 30
    lv <- if ("Level" %in% names(ref)) ref$Level %in% "D1" else TRUE
    x <- .ps_num(ref[[rc]])[qual %in% TRUE & lv]; x <- x[is.finite(x)]
    if (length(x) < 50) return(NA_real_)
    f <- slim_ecdf(stats::ecdf(x))
    if (is.null(.ps_env$ecdf)) .ps_env$ecdf <- list()
    .ps_env$ecdf[[ck]] <- f
  }
  f(v)
}
# green (good) / white / red (bad) fill for a percentile in the stat's direction
ps_fill <- function(p, dir) {
  if (!is.finite(p) || dir == 0) return("transparent")
  if (dir < 0) p <- 1 - p
  pal <- grDevices::colorRampPalette(c("#E1463E", "#FFFFFF", "#1E9E52"))(21)
  pal[1 + round(20 * max(0, min(1, p)))]
}

# ---- UI ------------------------------------------------------------------------------------
PS_CSS <- HTML("
  .ps-card { background:#132A3A; border-radius:14px; padding:14px 16px 10px; margin:6px 0 14px; color:#fff;
             box-shadow:0 1px 3px rgba(0,0,0,.18); overflow-x:auto; }
  .ps-head { display:flex; align-items:center; gap:14px; flex-wrap:wrap; margin-bottom:10px; }
  .ps-title { font-size:26px; font-weight:800; letter-spacing:-.01em; margin-right:6px; }
  .ps-head .btn-group .btn { background:#1E3A4C; border:1px solid #3B5A6E; color:#D6E0E6; font-weight:600; font-size:13px;
                             padding:6px 14px; box-shadow:none; }
  .ps-head .btn-group .btn.active { background:#3C8D8E; color:#fff; border-color:#3C8D8E; }
  .ps-head .btn-group .btn:focus { outline:none; }
  .ps-head .form-group { margin:0; }
  .ps-note { color:#9FB3BF; font-size:12px; margin-left:auto; }
  table.ps-table { border-collapse:separate; border-spacing:0; width:100%; font-size:14px; font-variant-numeric:tabular-nums; }
  table.ps-table th { color:#fff; font-weight:700; text-align:center; padding:9px 8px; border-bottom:1px solid #3B5A6E; white-space:nowrap; }
  table.ps-table th.l, table.ps-table td.l { text-align:left; }
  table.ps-table td { padding:9px 8px; text-align:center; border-bottom:1px solid #22405280; white-space:nowrap; color:#fff; }
  table.ps-table td.stat { color:#111827; font-weight:600; background:#fff; }
  table.ps-table td.stat.blank { background:#1E3A4C; color:#7C8F9A; font-weight:400; }
  table.ps-table th.grp, table.ps-table td.grp { border-left:2px solid #8FA8B6; }
  table.ps-table tr.cur td { font-weight:800; }
  table.ps-table td .ps-lg { display:inline-block; font-size:12px; font-weight:700; letter-spacing:.04em; }
  table.ps-table td .ps-src { color:#9FB3BF; font-size:11px; margin-left:6px; }
  .ps-empty { color:#9FB3BF; font-size:13px; padding:8px 2px; }
")

season_table_ui <- function(prefix, title) {
  div(class = "ps-card",
    tags$style(PS_CSS),
    div(class = "ps-head",
        span(class = "ps-title", title),
        shinyWidgets::radioGroupButtons(paste0(prefix, "_mode"), NULL, choices = c("Advanced", "Standard"),
                                        selected = "Advanced", size = "sm"),
        shinyWidgets::checkboxGroupButtons(paste0(prefix, "_exp"), NULL, choices = c("Show Expected"), size = "sm"),
        uiOutput(paste0(prefix, "_note"), inline = TRUE)),
    uiOutput(paste0(prefix, "_table")))
}

# ---- render --------------------------------------------------------------------------------
ps_fmt <- function(v, dig) {
  if (is.null(v) || length(v) == 0 || !is.finite(v)) return(NULL)
  if (dig == 3) return(sub("^(-?)0\\.", "\\1.", sprintf("%.3f", v)))
  formatC(v, format = "f", digits = dig, big.mark = ",")
}

season_table_html <- function(rows, kind = c("pit", "bat"), mode = "Advanced", expected = FALSE) {
  kind <- match.arg(kind)
  if (is.null(rows) || !is.data.frame(rows) || !nrow(rows))
    return(div(class = "ps-empty", "No season lines for this player yet."))
  spec <- PS_COLS[[kind]][[if (identical(mode, "Standard")) "standard" else "advanced"]]
  if (isTRUE(expected)) spec <- c(spec, PS_COLS[[kind]]$expected)
  spec <- spec[vapply(names(spec), function(cc) cc %in% names(rows), logical(1))]
  # first stat column after the counting block gets the group divider
  first_rate <- which(vapply(spec, function(s) s[1] != 0, logical(1)))[1]
  first_exp  <- if (isTRUE(expected)) match(names(PS_COLS[[kind]]$expected)[1], names(spec)) else NA
  cur <- max(which(rows$Year == max(rows$Year)))
  hdr <- tags$tr(
    tags$th("Year", class = "l"), tags$th("Age"), tags$th("Team", class = "l"), tags$th("LG"),
    lapply(seq_along(spec), function(i)
      tags$th(names(spec)[i], class = paste(if (i %in% c(first_rate, first_exp)) "grp" else ""))))
  body <- lapply(seq_len(nrow(rows)), function(r) {
    lab <- rows$Season[r]
    yr_txt <- if (identical(lab, "Spring")) as.character(rows$Year[r]) else paste0(rows$Year[r], " ", lab)
    src <- rows$Source[r]
    src_txt <- if (identical(src, "calendar")) "calendar year" else if (identical(src, "trackman")) "TrackMan" else NULL
    tags$tr(class = if (r == cur) "cur" else "",
      tags$td(yr_txt, class = "l"),
      tags$td(if (is.finite(rows$Age[r])) as.character(rows$Age[r]) else "—"),
      tags$td(class = "l", rows$Team[r], if (!is.null(src_txt)) span(class = "ps-src", src_txt)),
      tags$td(span(class = "ps-lg", rows$LG[r])),
      lapply(seq_along(spec), function(i) {
        cc <- names(spec)[i]; dir <- spec[[i]][1]; dig <- spec[[i]][2]
        v <- .ps_num(rows[[cc]][r]); txt <- ps_fmt(v, dig)
        cls <- paste("stat", if (i %in% c(first_rate, first_exp)) "grp" else "", if (is.null(txt)) "blank" else "")
        fill <- if (!is.null(txt) && dir != 0) ps_fill(ps_pct(kind, cc, v), dir) else "transparent"
        tags$td(class = cls, style = if (!is.null(txt) && fill != "transparent") paste0("background:", fill, ";") else NULL,
                if (is.null(txt)) "" else txt)
      }))
  })
  tags$table(class = "ps-table", tags$thead(hdr), tags$tbody(body))
}
