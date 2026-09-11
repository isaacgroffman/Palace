# ===================  NCAA DATA LAYER  ======================
# Every 2026 NCAA pitch lives in Supabase (pitchprofiler.pitches), already
# reclassified and scored. Nothing is pulled at startup beyond a tiny
# Pitcher/Team directory; a pitcher's rows are fetched (and cached) only
# when that pitcher is selected.
# ============================================================

# Per-row feet repair for plate locations (same logic as advance app)
normalize_plate_location <- function(df) {
  if (!all(c("PlateLocSide", "PlateLocHeight") %in% names(df))) return(df)
  df %>%
    mutate(
      .is_inches = !is.na(PlateLocHeight) & abs(PlateLocHeight) > 8,
      PlateLocSide   = ifelse(.is_inches, PlateLocSide / 12,   PlateLocSide),
      PlateLocHeight = ifelse(.is_inches, PlateLocHeight / 12, PlateLocHeight)
    ) %>%
    select(-.is_inches)
}

# LEGACY coastal set: players with CCU game data / the old bio file. This is
# the spring-era identity — a 2027 transfer is NOT in it, so in spring
# contexts he still resolves to his old team (scouting view, his real data).
coastal_pitcher_set <- unique(c(
  bio$Pitcher_FL, bio$Pitcher,
  if (exists("data")      && is.data.frame(data))      unique(data$Pitcher),
  if (exists("fall25")    && is.data.frame(fall25))    unique(fall25$Pitcher),
  if (exists("prespring") && is.data.frame(prespring)) unique(prespring$Pitcher),
  if (exists("spring26")  && is.data.frame(spring26))  unique(spring26$Pitcher)
))
coastal_pitcher_set <- coastal_pitcher_set[!is.na(coastal_pitcher_set) & nzchar(coastal_pitcher_set)]
coastal_pitcher_norms <- unique(bp_norm_name(coastal_pitcher_set))

is_coastal_pitcher <- function(nm) {
  if (is.null(nm) || !nzchar(nm)) return(FALSE)
  nm %in% coastal_pitcher_set || bp_norm_name(nm) %in% coastal_pitcher_norms
}

# FALL-2026 coastal set: the 2027 roster + anyone with a Supabase pen. These
# players are Coastal ONLY when Fall 2026 is among the selected seasons.
fall26_pitcher_norms <- unique(bp_norm_name(c(
  if (!is.null(roster27)) roster27$Name[roster27$IsPitcher],
  bullpen_pitchers_fl
)))

is_fall26_pitcher <- function(nm) {
  !is.null(nm) && nzchar(nm) && bp_norm_name(nm) %in% fall26_pitcher_norms
}

# Season-aware coastal test: legacy always counts; roster/bullpen membership
# counts only while Fall 2026 is selected.
is_coastal_ctx <- function(nm, seasons) {
  is_coastal_pitcher(nm) ||
    ("Fall26" %in% (seasons %||% character(0)) && is_fall26_pitcher(nm))
}

# ---- Pitcher/Team directory (2 columns only — fast) ----
# Raw Pitcher/Team pairs — kept around so the Team Report can enumerate
# a full opposing staff by team code.
ncaa_dir_pairs <- tryCatch({
  sb <- pp_directory()
  if (is.null(sb) || nrow(sb) == 0) stop("Supabase directory unavailable")
  if (!"PitcherId" %in% names(sb)) sb$PitcherId <- NA_character_
  out <- sb %>%
    filter(!is.na(Pitcher), nzchar(Pitcher)) %>%
    mutate(
      display   = ifelse(grepl(",", Pitcher), normalize_lastfirst(Pitcher), Pitcher),
      team_disp = prettify_team(PitcherTeam),
      PitcherId = as.character(PitcherId)
    ) %>%
    distinct(Pitcher, PitcherTeam, PitcherId, display, team_disp, src)
  cat("[pitchprofiler] directory:", nrow(out), "pitcher-team pairs\n")
  out
}, error = function(e) {
  cat("WARNING: NCAA directory build failed -", conditionMessage(e), "\n")
  data.frame(Pitcher = character(0), PitcherTeam = character(0),
             display = character(0), team_disp = character(0), src = character(0))
})

# TrackMan id -> directory display name (leaderboard / matrix links carry the
# id, which survives every spelling difference between TruMedia and TrackMan)
ncaa_display_for_id <- function(id) {
  id <- as.character(id %||% "")
  if (!nzchar(id) || !"PitcherId" %in% names(ncaa_dir_pairs)) return(NULL)
  hit <- ncaa_dir_pairs$display[!is.na(ncaa_dir_pairs$PitcherId) & ncaa_dir_pairs$PitcherId == id]
  hit <- hit[!is.na(hit) & nzchar(hit)]
  if (length(hit)) hit[1] else NULL
}

ncaa_directory <- tryCatch({
  if (nrow(ncaa_dir_pairs) == 0) stop("empty NCAA directory")
  ncaa_dir_pairs %>%
    filter(is.na(PitcherTeam) | PitcherTeam != "COA_CHA") %>%
    filter(!display %in% coastal_pitcher_set) %>%
    group_by(display) %>%
    summarise(
      raw_names = list(unique(Pitcher)),
      teams     = paste(sort(unique(team_disp[!is.na(team_disp)])), collapse = " / "),
      # Which source seasons this arm actually appears in ("2025"/"2026").
      srcs      = list(unique(as.character(src[!is.na(src)]))),
      .groups   = "drop"
    ) %>%
    arrange(display)
}, error = function(e) {
  cat("WARNING: NCAA directory build failed -", conditionMessage(e), "\n")
  data.frame(display = character(0), teams = character(0)) %>%
    mutate(raw_names = list(), srcs = list())
})

# ---- Team-level directory for the Team Report ----
ncaa_team_directory <- tryCatch({
  ncaa_dir_pairs %>%
    filter(!is.na(PitcherTeam), nzchar(PitcherTeam),
           !is.na(team_disp), nzchar(team_disp)) %>%
    group_by(team_disp) %>%
    summarise(codes = list(unique(PitcherTeam)),
              n_arms = dplyr::n_distinct(display), .groups = "drop") %>%
    arrange(team_disp)
}, error = function(e) {
  data.frame(team_disp = character(0), n_arms = integer(0)) %>%
    mutate(codes = list())
})
ncaa_team_codes <- setNames(ncaa_team_directory$codes, ncaa_team_directory$team_disp)

# Lean per-team loader (only the columns the count report reads), lazy
# filter + collect, cached per team for the session.
# ============================================================
