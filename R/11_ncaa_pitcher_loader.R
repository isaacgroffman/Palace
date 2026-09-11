# Per-pitcher NCAA loader (lazy collect + in-memory cache)
# ============================================================
ensure_ncaa_cols <- function(df) {
  num_cols <- c("ax0","ay0","az0","vx0","vy0","vz0","x0","y0","z0",
                "RelSpeed","SpinRate","SpinAxis","HorzBreak","InducedVertBreak","VertBreak",
                "RelSide","RelHeight","Extension","PlateLocSide","PlateLocHeight",
                "ExitSpeed","Angle","Direction","Distance","Bearing","HangTime",
                "Balls","Strikes","Outs","PitchofPA","PAofInning","Inning","PitchNo",
                "OutsOnPlay","RunsScored","VertApprAngle","HorzApprAngle")
  chr_cols <- c("PitchCall","PlayResult","KorBB","TaggedPitchType","TaggedHitType",
                "PitcherThrows","BatterSide","Pitcher","Batter","PitcherTeam","BatterTeam",
                "HomeTeam","AwayTeam","Catcher","PitcherId","BatterId",
                "GameID","GameUID","PitchUID","Count","Notes","AutoPitchType")
  for (cc in num_cols) {
    if (!cc %in% names(df)) df[[cc]] <- NA_real_
    else df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
  }
  for (cc in chr_cols) {
    if (!cc %in% names(df)) df[[cc]] <- NA_character_
    else df[[cc]] <- as.character(df[[cc]])
  }
  if (!"Date" %in% names(df)) df$Date <- as.Date(NA)
  df$Date <- safe_as_date(df$Date)
  # GameID fallbacks so grouping never dies
  df$GameID <- ifelse(is.na(df$GameID) | df$GameID == "",
                      ifelse(!is.na(df$GameUID) & df$GameUID != "",
                             df$GameUID,
                             paste(df$Date, df$Pitcher, sep = "_")),
                      df$GameID)
  df$PlayResult <- ifelse(is.na(df$PlayResult), "Undefined", df$PlayResult)
  df$KorBB      <- ifelse(is.na(df$KorBB), "Undefined", df$KorBB)
  df$PitchCall  <- ifelse(is.na(df$PitchCall), "Undefined", df$PitchCall)
  df
}

# Light prep fallback: enough columns for every tab to render even when
# the full model pipeline can't run on this pitcher's data.
# Per-pitch is_* / *Indicator columns from PitchCall / PlayResult / KorBB /
# plate location. Shared by the pitcher light prep and the hitter loaders
# (hitter frames come straight from Supabase without process_pitcher_data).
add_pitch_indicators <- function(df) {
  # is_* backfill (same rules as process_pitcher_data) so the indicator
  # block below always has its inputs
  if (!all(c("is_hit","slg","on_base","is_plate_appearance","is_at_bat","is_walk","is_k") %in% names(df))) {
    df <- df %>% mutate(
      is_hit = ifelse(PlayResult %in% c("Single","Double","Triple","Homerun","HomeRun"), 1,
                      ifelse(PitchCall == "InPlay" | KorBB == "Strikeout", 0, NA_real_)),
      slg = case_when(
        PlayResult == "Single" ~ 1, PlayResult == "Double" ~ 2,
        PlayResult == "Triple" ~ 3, PlayResult %in% c("Homerun","HomeRun") ~ 4,
        PitchCall == "InPlay" | KorBB == "Strikeout" ~ 0, TRUE ~ NA_real_),
      on_base = case_when(
        PlayResult %in% c("Single","Double","Triple","Homerun","HomeRun") ~ 1,
        KorBB == "Walk" | PitchCall == "HitByPitch" ~ 1,
        PitchCall == "InPlay" | KorBB == "Strikeout" ~ 0, TRUE ~ NA_real_),
      is_plate_appearance = ifelse(
        PitchCall %in% c("InPlay","HitByPitch") | KorBB %in% c("Strikeout","Walk"), 1, 0),
      is_at_bat = case_when(
        PitchCall == "InPlay" & !PlayResult %in% c("StolenBase","Sacrifice","CaughtStealing","Undefined") ~ 1,
        KorBB == "Strikeout" ~ 1, TRUE ~ 0),
      is_walk = ifelse(KorBB == "Walk", 1, ifelse(is_plate_appearance == 1, 0, NA_real_)),
      is_k = ifelse(KorBB == "Strikeout", 1, ifelse(is_at_bat == 1, 0, NA_real_))
    )
  }

  df <- df %>%
    mutate(
      FBindicator = ifelse(TaggedPitchType %in% c("Fastball","Four-Seam","FourSeamFastBall","4-Seam Fastball","FF",
                                                  "Sinker","Two-Seam","TwoSeamFastBall","2-Seam Fastball","SI"), 1, 0),
      OSindicator = ifelse(TaggedPitchType %in% c("Slider","Cutter","Curveball","CurveBall","ChangeUp","Changeup",
                                                  "Splitter","Sweeper","Knuckleball","Slurve","Knuckle Curve"), 1, 0),
      StrikeZoneIndicator = ifelse(
        !is.na(PlateLocSide) & !is.na(PlateLocHeight) &
          PlateLocSide >= -0.83 & PlateLocSide <= 0.83 &
          PlateLocHeight >= 1.5 & PlateLocHeight <= 3.38, 1, 0),
      StrikeIndicator = ifelse(
        PitchCall %in% c("StrikeSwinging","StrikeCalled","FoulBallNotFieldable","FoulBall","InPlay"), 1, 0),
      WhiffIndicator  = ifelse(PitchCall == "StrikeSwinging", 1, 0),
      SwingIndicator  = ifelse(PitchCall %in% c("StrikeSwinging","FoulBallNotFieldable","FoulBall","InPlay"), 1, 0),
      HBPIndicator  = ifelse(PitchCall == "HitByPitch", 1, 0),
      WalkIndicator = ifelse(KorBB == "Walk", 1, 0),
      FPindicator   = ifelse(Balls == 0 & Strikes == 0, 1, 0),
      FPSindicator  = ifelse(StrikeIndicator == 1 & FPindicator == 1, 1, 0),
      FBstrikeind   = ifelse(StrikeIndicator == 1 & FBindicator == 1, 1, 0),
      OSstrikeind   = ifelse(StrikeIndicator == 1 & OSindicator == 1, 1, 0),
      ABindicator   = ifelse(!is.na(is_at_bat), is_at_bat, 0),
      HitIndicator  = ifelse(!is.na(is_hit) & is_hit == 1, 1, 0),
      PAindicator   = ifelse(!is.na(is_plate_appearance), is_plate_appearance, 0),
      OnBaseindicator = ifelse(!is.na(on_base) & on_base == 1, 1, 0),
      BIPind        = ifelse(PitchCall == "InPlay", 1, 0),
      HHindicator   = ifelse(PitchCall == "InPlay" & !is.na(ExitSpeed) & ExitSpeed > 95, 1, 0),
      biphh         = ifelse(PitchCall == "InPlay" & !is.na(ExitSpeed) & ExitSpeed > 15, 1, 0),
      SolidContact  = ifelse(PitchCall == "InPlay" & !is.na(ExitSpeed) & !is.na(Angle) &
                             ((ExitSpeed > 95 & Angle >= 0 & Angle <= 40) |
                              (ExitSpeed > 92 & Angle >= 8 & Angle <= 40)), 1, 0),
      BarrelIndicator = ifelse(PitchCall == "InPlay" & !is.na(ExitSpeed) & !is.na(Angle) &
                               ExitSpeed >= 95 & Angle >= 10 & Angle <= 35, 1, 0),
      PlateIndicator  = ifelse(!is.na(PlateLocSide) & PlateLocSide >= -0.95 & PlateLocSide <= 0.95, 1, 0),
      IZBarrelIndicator = ifelse(BarrelIndicator == 1 & StrikeZoneIndicator == 1, 1, 0),
      Zwhiffind     = ifelse(WhiffIndicator == 1 & StrikeZoneIndicator == 1, 1, 0),
      Zswing        = ifelse(StrikeZoneIndicator == 1 & SwingIndicator == 1, 1, 0),
      GBindicator   = ifelse(!is.na(TaggedHitType) & TaggedHitType == "GroundBall", 1, 0),
      Chaseindicator = ifelse(SwingIndicator == 1 & StrikeZoneIndicator == 0, 1, 0),
      OutofZone     = ifelse(StrikeZoneIndicator == 0, 1, 0),
      OutIndicator  = ifelse((PlayResult %in% c("Out","FieldersChoice") | KorBB == "Strikeout") & HBPIndicator == 0, 1, 0),
      totalbases    = ifelse(!is.na(slg), slg, 0)
    )
  df
}

scout_prep_light <- function(df) {
  df <- add_pitch_indicators(df)
  # height + arm angle
  hkey <- norm_player_key(df$Pitcher[1])
  h_in <- 74
  if (!is.null(drs_bio)) {
    r <- drs_bio[!is.na(drs_bio$bio_key) & drs_bio$bio_key == hkey, , drop = FALSE]
    if (nrow(r) > 0 && !is.na(r$height_in[1])) h_in <- r$height_in[1]
  }
  df <- df %>%
    mutate(
      pitcher_height_inches = h_in,
      height_inches = h_in,
      RelSide_in    = 12 * as.numeric(RelSide),
      RelHeight_in  = 12 * as.numeric(RelHeight),
      shoulder_pos  = 0.70 * pitcher_height_inches,
      Adj           = pmax(RelHeight_in - shoulder_pos, 1e-6),
      Opp           = abs(RelSide_in),
      arm_angle_deg = atan2(Opp, Adj) * 180 / pi,
      arm_angle_savant = pmin(pmax(90 - arm_angle_deg, 0), 90),
      sz_top = 38, sz_bot = 19
      # NOTE: plate locations stay in FEET — that's the app-wide convention
      # (the coastal datasets get /12 after pooling).
    ) %>%
    select(-RelSide_in, -RelHeight_in, -shoulder_pos, -Adj, -Opp, -arm_angle_deg)
  df$ncaa_ind <- 1
  df$Roster_2026 <- "No"
  df
}

# Guarantee the plus-score columns every tab reads. Uses raw model
# scores + the pooled calibration when available; otherwise keeps any
# precomputed stuff_plus from the NCAA parquet and NA-fills the rest
# so summaries render "-" instead of erroring.
# Final unit guard: whatever path produced this frame, plate locations
# must leave in FEET to match the coastal datasets and every location plot.
ensure_feet <- function(df) {
  if (!all(c("PlateLocSide", "PlateLocHeight") %in% names(df))) return(df)
  med <- suppressWarnings(median(abs(df$PlateLocHeight), na.rm = TRUE))
  if (is.finite(med) && med > 8) {
    df$PlateLocSide   <- df$PlateLocSide / 12
    df$PlateLocHeight <- df$PlateLocHeight / 12
  }
  df
}

ensure_plus_scores <- function(df) {
  # Guarantee the plus columns every tab reads. Recomputes from the
  # per-pitch xrv columns against the FIXED Pitch Profiler reference
  # when possible; otherwise NA-fills so summaries render "-".
  fill_one <- function(df, plus_col, xrv_col, ref) {
    if (!plus_col %in% names(df) || all(is.na(df[[plus_col]]))) {
      df[[plus_col]] <- if (xrv_col %in% names(df)) {
        xrv_to_plus(df[[xrv_col]], ref)
      } else NA_real_
    }
    df
  }
  df <- fill_one(df, "stuff_plus",    "stuff_xrv",    PP_REF$stuff)
  df <- fill_one(df, "pitching_plus", "pitching_xrv", PP_REF$pitching)
  df <- fill_one(df, "location_plus", "location_xrv", PP_REF$location)
  df
}

# fast = TRUE skips the full process_pitcher_data pipeline (proModel
# Python scoring, reference pools) and lands directly on scout_prep_light:
# every chart-facing column (indicators, arm angle) is present, plus
# scores are NA until the full pass runs. Used for first-paint frames.
prep_ncaa_pitcher <- function(df, fast = FALSE) {
  df <- ensure_ncaa_cols(df)
  df <- normalize_plate_location(df)  # feet in, always

  # Normalize handedness to the levels the recipes were trained on.
  # Stray levels ("Undefined", "", "S") become extra dummy columns at
  # bake time and crash xgboost with a feature-count mismatch.
  first_letter <- function(x) toupper(substr(trimws(as.character(x)), 1, 1))
  df$BatterSide <- dplyr::case_when(
    first_letter(df$BatterSide) == "L" ~ "Left",
    first_letter(df$BatterSide) == "R" ~ "Right",
    TRUE ~ NA_character_
  )
  df <- df[!is.na(df$BatterSide), , drop = FALSE]
  df$PitcherThrows <- dplyr::case_when(
    first_letter(df$PitcherThrows) == "L" ~ "Left",
    first_letter(df$PitcherThrows) == "R" ~ "Right",
    TRUE ~ "Right"
  )

  out <- if (fast) NULL else tryCatch({
    res <- process_pitcher_data(df, bio_heights_ncaa, "ncaa_ind")
    d <- res$data
    d$Roster_2026 <- "No"
    # process_pitcher_data works in inches internally; the coastal datasets
    # get divided back to FEET after pooling. Mirror that here so NCAA
    # locations land on the same scale as every coastal pitcher.
    d$PlateLocHeight <- d$PlateLocHeight / 12
    d$PlateLocSide   <- d$PlateLocSide / 12
    cat("  [NCAA prep] full pipeline OK:", nrow(d), "rows\n")
    d
  }, error = function(e) {
    cat("  [NCAA prep] pipeline failed (", conditionMessage(e),
        ") - using light prep\n")
    NULL
  })
  if (is.null(out)) out <- scout_prep_light(df)
  ensure_feet(ensure_plus_scores(out))
}

# ============================================================================
