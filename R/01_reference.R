# ============================================================================
# REFERENCE LOOKUPS
# Team / league / conference lookups (reference/merged_teams.csv) and the helpers built on them.
# ============================================================================

team_name_map <- read_csv("reference/merged_teams.csv", show_col_types = FALSE) %>%
  select(any_of(c("trackman_abbr", "team_name", "conference", "logo_url"))) %>%
  filter(!is.na(team_name) & team_name != "")
# Older copies of the file predate the conference join; tolerate them.
if (!"conference" %in% names(team_name_map))
  team_name_map$conference <- NA_character_

# conference -> the school names that belong to it. Used by the league
# leaderboard so "SBELT" can fill the team picker in one click.
# "Team" is a placeholder in the source league file, not a conference.
CONF_TEAMS <- team_name_map %>%
  filter(!is.na(conference), conference != "", conference != "Team",
         !is.na(team_name),  team_name  != "") %>%
  distinct(conference, team_name)
LB_CONFERENCES <- sort(unique(CONF_TEAMS$conference))
cat("[Conf] conferences loaded:", length(LB_CONFERENCES),
    "covering", nrow(CONF_TEAMS), "teams\n")

# Loose name match: TruMedia spells schools slightly differently than the
# NCAA logo file ("St." vs "State", punctuation), so compare on letters only.
.conf_norm <- function(x)
  gsub("[^a-z0-9]", "", tolower(as.character(x)))

conf_team_names <- function(confs) {
  if (is.null(confs) || length(confs) == 0) return(character(0))
  unique(CONF_TEAMS$team_name[CONF_TEAMS$conference %in% confs])
}

prettify_team <- function(x) {
  lookup <- setNames(team_name_map$team_name, team_name_map$trackman_abbr)
  out <- lookup[as.character(x)]
  # unname the whole result: lookup[] attaches names (NA names for codes
  # not in the map) and ifelse() propagates them onto the output, which
  # later crashes updateSelectizeInput(server = TRUE) with
  # "NAs are not allowed in subscripted assignments".
  unname(ifelse(is.na(out), as.character(x), unname(out)))
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
