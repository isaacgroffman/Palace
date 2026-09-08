# ============================================================================
# PITCH PROCESSING & PERCENTILE MAPS
# process_pitcher_data(), indicator backfill, P5 ecdf maps, movement plot helpers. Functions only - no data is loaded here.
# ============================================================================

numeric_columns <- c("PitchNo", "PAofInning", "PitchofPA", "PitcherId", "BatterId", "Inning", "Outs", "Balls",
                     "Strikes", "OutsOnPlay", "RunsScored", "RelSpeed", "VertRelAngle", "HorzRelAngle", "SpinRate", 
                     "SpinAxis", "RelHeight", "RelSide", "Extension", "VertBreak", "InducedVertBreak", "HorzBreak", 
                     "PlateLocHeight", "PlateLocSide", "ZoneSpeed", "VertApprAngle", "HorzApprAngle", "ZoneTime", 
                     "ExitSpeed", "Angle", "Direction", "Distance", "Bearing", "HangTime", "pfxx", "pfxz", "x0", "y0", "z0", "vx0", "vz0", "vy0", "ax0", "ay0", 
                     "az0",  "EffectiveVelo", "SpeedDrop", "ContactPositionX", "ContactPositionY",
                     "ContactPositionZ", "CatcherId", "ThrowSpeed",
                     "PopTime", "ExchangeTime", "TimeToBase")






# Function to process pitcher data (reusable for all datasets)
process_pitcher_data <- function(df, bio_heights, season_indicator = "data_ind") {
  
  # ============================================================
  # Backfill is_* indicators for older datasets that don't have them
  # ============================================================
  needed_cols <- c("is_hit", "slg", "on_base", "is_plate_appearance", 
                   "is_at_bat", "is_walk", "is_k")
  
  if (!all(needed_cols %in% names(df))) {
    cat("  [Backfill] Source data missing is_* indicators. Building from PlayResult/KorBB/PitchCall.\n")
    
    df <- df %>% mutate(
      is_hit = case_when(
        PlayResult %in% c("Single", "Double", "Triple", "Homerun", "HomeRun") & PitchCall == "InPlay" ~ 1,
        !PlayResult %in% c("Single", "Double", "Triple", "Homerun", "HomeRun") & PitchCall == "InPlay" ~ 0,
        KorBB == "Strikeout" ~ 0,
        PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking") ~ 0,
        TRUE ~ NA_real_
      ),
      slg = case_when(
        PitchCall == "InPlay" & PlayResult == "Single" ~ 1,
        PitchCall == "InPlay" & PlayResult == "Double" ~ 2,
        PitchCall == "InPlay" & PlayResult == "Triple" ~ 3,
        PitchCall == "InPlay" & PlayResult %in% c("Homerun", "HomeRun") ~ 4,
        !PlayResult %in% c("Single", "Double", "Triple", "Homerun", "HomeRun") & PitchCall == "InPlay" ~ 0,
        KorBB == "Strikeout" ~ 0,
        PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking") ~ 0,
        TRUE ~ NA_real_
      ),
      on_base = case_when(
        PitchCall == "InPlay" & PlayResult %in% c("Single", "Double", "Triple", "Homerun", "HomeRun") ~ 1,
        PitchCall %in% c("HitByPitch") | KorBB == "Walk" ~ 1,
        PitchCall == "InPlay" & PlayResult %in% c("Out", "Error", "FieldersChoice") & PlayResult != "Sacrifice" ~ 0,
        KorBB == "Strikeout" ~ 0,
        PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking") ~ 0,
        TRUE ~ NA_real_
      ),
      is_plate_appearance = ifelse(
        PitchCall %in% c("InPlay", "HitByPitch") | 
          KorBB %in% c("Strikeout", "Walk") | 
          PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking"), 1, 0
      ),
      is_at_bat = case_when(
        PitchCall == "InPlay" & !PlayResult %in% c("StolenBase", "Sacrifice", "CaughtStealing", "Undefined") ~ 1,
        KorBB == "Strikeout" ~ 1,
        PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking") ~ 1,
        TRUE ~ 0
      ),
      is_walk = case_when(
        is_plate_appearance == 1 & KorBB == "Walk" ~ 1,
        is_plate_appearance == 1 & KorBB != "Walk" ~ 0,
        TRUE ~ NA_real_
      ),
      is_k = case_when(
        is_at_bat == 1 & KorBB == "Strikeout" ~ 1,
        is_at_bat == 1 & KorBB != "Strikeout" ~ 0,
        PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking") ~ 1,
        TRUE ~ NA_real_
      )
    )
  } else {
    cat("  [Backfill] Source data already has is_* indicators. Skipping backfill.\n")
  }
  
  # ============================================================
  # CORRECTION PASS: Enforce hit/AB/TB/on_base/etc. based on PlayResult/KorBB/PitchCall
  # Runs ALWAYS (whether backfilled or source-provided) to catch
  # data quality issues where indicators may be NA or 0 for rows
  # that clearly had hit outcomes upstream.
  # Also adds HBP/walk/strikeout corrections for on_base, is_walk, is_k.
  # ============================================================
  before_hits <- sum(df$is_hit == 1, na.rm = TRUE)
  before_tb   <- sum(df$slg, na.rm = TRUE)
  before_ab   <- sum(df$is_at_bat == 1, na.rm = TRUE)
  before_ob   <- sum(df$on_base == 1, na.rm = TRUE)
  
  df <- df %>% mutate(
    is_hit = case_when(
      PlayResult %in% c("Single", "Double", "Triple", "Homerun", "HomeRun") ~ 1,
      TRUE ~ is_hit
    ),
    slg = case_when(
      PlayResult == "Single" ~ 1,
      PlayResult == "Double" ~ 2,
      PlayResult == "Triple" ~ 3,
      PlayResult %in% c("Homerun", "HomeRun") ~ 4,
      TRUE ~ slg
    ),
    is_at_bat = case_when(
      PlayResult %in% c("Single", "Double", "Triple", "Homerun", "HomeRun") ~ 1,
      PlayResult %in% c("Out", "Error", "FieldersChoice") ~ 1,
      KorBB == "Strikeout" ~ 1,
      PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking") ~ 1,
      TRUE ~ is_at_bat
    ),
    is_k = case_when(
      KorBB == "Strikeout" ~ 1,
      PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking") ~ 1,
      TRUE ~ is_k
    ),
    is_walk = case_when(
      KorBB == "Walk" ~ 1,
      TRUE ~ is_walk
    ),
    on_base = case_when(
      PlayResult %in% c("Single", "Double", "Triple", "Homerun", "HomeRun") ~ 1,
      KorBB == "Walk" ~ 1,
      PitchCall == "HitByPitch" ~ 1,
      TRUE ~ on_base
    ),
    is_plate_appearance = case_when(
      PlayResult %in% c("Single", "Double", "Triple", "Homerun", "HomeRun",
                        "Out", "Error", "FieldersChoice", "Sacrifice") ~ 1,
      KorBB %in% c("Strikeout", "Walk") ~ 1,
      PitchCall == "HitByPitch" ~ 1,
      PlayResult %in% c("StrikeoutSwinging", "StrikeoutLooking") ~ 1,
      TRUE ~ is_plate_appearance
    )
  )
  
  after_hits <- sum(df$is_hit == 1, na.rm = TRUE)
  after_tb   <- sum(df$slg, na.rm = TRUE)
  after_ab   <- sum(df$is_at_bat == 1, na.rm = TRUE)
  after_ob   <- sum(df$on_base == 1, na.rm = TRUE)
  
  cat(sprintf("  [Correction] Hits: %d -> %d  |  TB: %.0f -> %.0f  |  AB: %d -> %d  |  OB: %d -> %d\n",
              before_hits, after_hits, before_tb, after_tb,
              before_ab, after_ab, before_ob, after_ob))
  
  # Normalize names
  df <- df %>%
    dplyr::mutate(
      Pitcher = ifelse(grepl(",", Pitcher), normalize_lastfirst(Pitcher), Pitcher),
      Batter  = ifelse(grepl(",", Batter),  normalize_lastfirst(Batter),  Batter)
    )
  
  # Remove duplicates
  df <- df %>% dplyr::distinct()
  df <- dedupe_safely_by_uid(df)
  
  # Coerce numeric columns that may arrive as character in some datasets
  for (col in numeric_columns) {
    if (col %in% names(df) && !is.numeric(df[[col]])) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }
  
  df$TaggedPitchType <- gsub("^Changeup$", "ChangeUp", df$TaggedPitchType)
  
  # Add arm angle calculations if not present
  if (!"arm_angle_savant" %in% names(df)) {
    df <- df %>%
      left_join(bio_heights, by = c("Pitcher" = "Pitcher_FL")) %>%
      mutate(
        pitcher_height_inches = ifelse(is.na(pitcher_height_inches),
                                       median(pitcher_height_inches, na.rm = TRUE),
                                       pitcher_height_inches),
        RelSide_in    = 12 * as.numeric(RelSide),
        RelHeight_in  = 12 * as.numeric(RelHeight),
        shoulder_pos  = 0.70 * pitcher_height_inches,
        Adj           = pmax(RelHeight_in - shoulder_pos, 1e-6),
        Opp           = abs(RelSide_in),
        arm_angle_deg = atan2(Opp, Adj) * 180 / pi,
        arm_angle_savant = pmin(pmax(90 - arm_angle_deg, 0), 90)
      ) %>%
      select(-RelSide_in, -RelHeight_in, -shoulder_pos, -Adj, -Opp, -arm_angle_deg)
  }
  
  # Create Outing column
  df <- df %>%
    mutate(Outing = ifelse(is.na(Date), 
                           as.character(GameID), 
                           as.character(Date)))
  
  # Fix NA GameIDs — synthesize from Date + PitcherTeam + AwayTeam/HomeTeam
  df <- df %>%
    mutate(
      GameID = ifelse(
        is.na(GameID) | GameID == "" | GameID == "NA",
        paste0(as.character(Date), "-", PitcherTeam, "-", 
               ifelse(!is.na(AwayTeam), AwayTeam, "UNK")),
        GameID
      )
    )
  
  # Parse dates — try multiple formats
  df$Date <- as.character(safe_as_date(df$Date))
  parsed_mdy <- suppressWarnings(as.Date(df$Date, format = "%m/%d/%y"))
  parsed_ymd <- suppressWarnings(as.Date(df$Date, format = "%Y-%m-%d"))
  df$Date <- dplyr::coalesce(parsed_mdy, parsed_ymd)
  df$Date <- parse_date_fallback(df$Date, alt = if ("UTCDate" %in% names(df)) df$UTCDate else NULL)
  
  normalize_time_col <- function(x) {
    if (is.null(x) || length(x) == 0) return(x)
    if (inherits(x, c("hms", "difftime", "Period", "Duration"))) {
      return(as.character(x))
    }
    if (is.character(x)) {
      x <- gsub("(:\\d{2})\\.(\\d)$", "\\1.\\20", x)
      return(x)
    }
    if (is.numeric(x)) {
      return(as.character(x))
    }
    return(as.character(x))
  }
  
  time_related_cols <- c("Time", "Tilt", "UTCTime", "HangTime", 
                         "PopTime", "ExchangeTime", "TimeToBase", "MeasuredDuration")
  
  for (col in time_related_cols) {
    if (col %in% names(df)) {
      df[[col]] <- normalize_time_col(df[[col]])
    }
  }
  
  normalize_datetime_col <- function(x) {
    if (is.null(x) || length(x) == 0) return(x)
    if (inherits(x, c("POSIXct", "POSIXlt", "POSIXt"))) {
      return(format(x, "%Y-%m-%d %H:%M:%S"))
    }
    if (inherits(x, "Date")) {
      return(as.character(x))
    }
    if (is.character(x)) {
      x <- trimws(x)
      return(x)
    }
    return(as.character(x))
  }
  
  datetime_cols <- c("LocalDateTime", "UTCDateTime")
  for (col in datetime_cols) {
    if (col %in% names(df)) {
      df[[col]] <- normalize_datetime_col(df[[col]])
    }
  }
  
  if ("UTCDate" %in% names(df)) {
    df$UTCDate <- tryCatch({
      if (inherits(df$UTCDate, "Date")) {
        as.character(df$UTCDate)
      } else if (is.numeric(df$UTCDate)) {
        as.character(df$UTCDate)
      } else if (is.character(df$UTCDate)) {
        parsed <- suppressWarnings(as.Date(df$UTCDate, tryFormats = c("%Y-%m-%d", "%m/%d/%y", "%m/%d/%Y")))
        ifelse(is.na(parsed), df$UTCDate, as.character(parsed))
      } else {
        as.character(df$UTCDate)
      }
    }, error = function(e) {
      as.character(df$UTCDate)
    })
  }
  
  df <- df %>%
    mutate(across(where(~inherits(., c("hms", "difftime", "Period", "Duration", "POSIXct", "POSIXlt"))), 
                  as.character))
  
  # ============================================================
  # PITCH PROFILER SCORING (must run HERE, not later):
  #  - BEFORE the Date/PitchCall filters: vra_diff, times_through_order
  #    and times_seen_pitch_type are computed on the full stream, and a
  #    dropped pitch corrupts its neighbors (guide §6).
  #  - BEFORE the RelSide/ax0 LHP flips and the PlateLoc x12 conversion:
  #    the bundle's cleaning.py does its own normalization and must see
  #    the raw-convention frame, or LHP release side gets double-flipped.
  # Attaches: stuff_xrv/pitching_xrv/location_xrv + stuff_plus/
  # pitching_plus/location_plus (fixed college reference, 100 = avg).
  # ============================================================
  df <- score_promodel(df, label = season_indicator)

df <- df %>%
  filter(!is.na(Date)) %>%
  filter(!is.na(RelSpeed) | !is.na(PitchCall))
                                 
  plh_median <- median(abs(df$PlateLocHeight), na.rm = TRUE)
  plate_loc_already_inches <- !is.na(plh_median) && plh_median > 10
  
  if (plate_loc_already_inches) {
    cat("  [Unit Detection] PlateLocHeight median abs =", round(plh_median, 1), 
        "-> already in INCHES, converting to feet first\n")
    df <- df %>%
      mutate(
        PlateLocHeight = PlateLocHeight / 12,
        PlateLocSide   = PlateLocSide / 12
      )
  } else {
    cat("  [Unit Detection] PlateLocHeight median abs =", round(plh_median, 1), 
        "-> in FEET (standard TrackMan)\n")
  }
  
  spray_df <- df %>%
    filter(PitchCall == 'InPlay') %>%
    mutate(Bearing = as.numeric(Bearing),
           Distance = as.numeric(Distance),
           hc_bearing = Bearing * (3.14159/180),
           hc_x = Distance * sin(hc_bearing),
           hc_y = Distance * cos(hc_bearing))
  
  df <- df %>%
    filter(PitchCall != 'Undefined' | TaggedPitchType != 'Undefined') %>%
    mutate(
     FBindicator = ifelse(TaggedPitchType %in% c(
  "Fastball", "Four-Seam", "FourSeamFastBall", "4-Seam Fastball", "FF",
  "Sinker", "Two-Seam", "TwoSeamFastBall", "2-Seam Fastball", "SI"
), 1, 0),
      OSindicator = ifelse(TaggedPitchType %in% c("Slider", "Cutter", "Curveball", "CurveBall", "ChangeUp", "Changeup", "Splitter", "Sweeper", "Knuckleball", "Slurve", "Knuckle Curve"), 1, 0),
      EarlyIndicator = ifelse(
        ((Balls == 0 & Strikes == 0 & PitchCall == "InPlay") |
           (Balls == 1 & Strikes == 0 & PitchCall == "InPlay") |
           (Balls == 0 & Strikes == 1 & PitchCall == "InPlay") |
           (Balls == 1 & Strikes == 1 & PitchCall == "InPlay")), 1, 0),
      AheadIndicator = ifelse(
        ((Balls == 0 & Strikes == 1) & (PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable",'FoulBall'))) |
          ((Balls == 1 & Strikes == 1) & (PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable",'FoulBall'))), 1, 0),
      StrikeZoneIndicator = ifelse(
        PlateLocSide >= -0.83 & PlateLocSide <= 0.83 &
          PlateLocHeight >= 1.5 & PlateLocHeight <= 3.38, 1, 0),
      EdgeHeightIndicator = ifelse(
        ((PlateLocHeight > 14/12 & PlateLocHeight < 22/12) |
           (PlateLocHeight > 38/12 & PlateLocHeight < 46/12)), 1, 0),
      EdgeZoneHtIndicator = ifelse(PlateLocHeight > 16/12 & PlateLocHeight < 45.2/12, 1, 0),
      EdgeZoneWIndicator = ifelse(PlateLocSide > -13.4/12 & PlateLocSide < 13.4/12, 1, 0),
      EdgeWidthIndicator = ifelse(
        ((PlateLocSide > -13.3/12 & PlateLocSide < -6.7/12) |
           (PlateLocSide < 13.3/12 & PlateLocSide > 6.7/12)), 1, 0),
      HeartIndicator = ifelse(
        PlateLocSide >= -0.5583 & PlateLocSide <= 0.5583 &
          PlateLocHeight >= 1.83 & PlateLocHeight <= 3.5, 1, 0),
      StrikeIndicator = ifelse(
        PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBallNotFieldable",'FoulBall', "InPlay"), 1, 0),
      WhiffIndicator = ifelse(PitchCall == 'StrikeSwinging',1,0),
      SwingIndicator = ifelse(PitchCall %in% c("StrikeSwinging", "FoulBallNotFieldable", 'FoulBall',"InPlay"), 1, 0),
      LHHindicator = ifelse(BatterSide == 'Left', 1,0),
      RHHindicator = ifelse(BatterSide == 'Right', 1,0),
      
      # --- Sacrifice + CI helpers ---
      SacFlyIndicator = ifelse(
        PlayResult == "Sacrifice" & TaggedHitType %in% c("FlyBall", "LineDrive", "PopUp"), 1, 0),
      SacBuntIndicator = ifelse(
        PlayResult == "Sacrifice" & TaggedHitType == "Bunt", 1, 0),
      SacIndicator = ifelse(PlayResult == "Sacrifice", 1, 0),
      CIIndicator = ifelse(PitchCall == "CatchersInterference", 1, 0),
      
      HBPIndicator = ifelse(PitchCall == 'HitByPitch', 1, 0),
      WalkIndicator = ifelse(KorBB == 'Walk', 1, 0),
      FPindicator = ifelse(Balls == 0 & Strikes == 0, 1, 0),
      LeadOffIndicator = ifelse(
        (PAofInning == 1 & (PlayResult != "Undefined" | KorBB != "Undefined")) | PitchCall == "HitByPitch", 1, 0),
      
      # OBP denominator: AB + BB + HBP + SF (Option A — strict MLB rulebook)
      OBPdenominator = ifelse(!is.na(is_at_bat), is_at_bat, 0) + 
        ifelse(!is.na(is_walk) & is_walk == 1, 1, 0) + 
        HBPIndicator + 
        SacFlyIndicator,
      
      # --- Legacy aliases so existing code keeps working ---
      ABindicator     = ifelse(!is.na(is_at_bat), is_at_bat, 0),
      HitIndicator    = ifelse(!is.na(is_hit) & is_hit == 1, 1, 0),
      PAindicator     = ifelse(!is.na(is_plate_appearance), is_plate_appearance, 0),
      OnBaseindicator = ifelse(!is.na(on_base) & on_base == 1, 1, 0),
      
      BIPind = ifelse(PitchCall == 'InPlay', 1, 0),
      SolidContact = ifelse(
        (PitchCall == "InPlay" & ((ExitSpeed > 95 & Angle >= 0 & Angle <= 40) | (ExitSpeed > 92 & Angle >= 8 & Angle <= 40))), 1, 0),
      HHindicator = ifelse(PitchCall=='InPlay' & ExitSpeed > 95,1,0),
      biphh = ifelse(PitchCall == 'InPlay' & ExitSpeed > 15,1,0),
      BarrelIndicator = ifelse(PitchCall == 'InPlay' & !is.na(ExitSpeed) & !is.na(Angle) & 
                                 ExitSpeed >= 95 & Angle >= 10 & Angle <= 35, 1, 0),
      PlateIndicator = ifelse(PlateLocSide >= -0.95 & PlateLocSide <= 0.95, 1, 0),
      IZBarrelIndicator = ifelse(BarrelIndicator == 1 & StrikeZoneIndicator == 1, 1, 0)
    )
  
  df <- df %>%
    mutate(
      FBstrikeind = ifelse(
        (PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBall",'FoulBallNotFieldable', "InPlay")) & (FBindicator == 1), 1, 0),
      OSstrikeind = ifelse(
        (PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBall",'FoulBallNotFieldable', "InPlay")) & (OSindicator == 1), 1, 0),
      EdgeIndicator = ifelse(
        (EdgeHeightIndicator == 1 & EdgeZoneWIndicator == 1) | (EdgeWidthIndicator == 1 & EdgeZoneHtIndicator == 1), 1, 0),
      QualityPitchIndicator = ifelse(StrikeZoneIndicator == 1 | EdgeIndicator == 1, 1, 0),
      FPSindicator = ifelse(
        PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable",'FoulBall', "InPlay") & (FPindicator == 1), 1, 0),
      OutIndicator = ifelse(
        (PlayResult %in% c("Out", "FieldersChoice") | KorBB == "Strikeout") & HBPIndicator == 0, 1, 0),
      LOOindicator = ifelse(LeadOffIndicator == 1 & OutIndicator == 1, 1, 0),
      Zwhiffind = ifelse(WhiffIndicator == 1 & StrikeZoneIndicator == 1, 1,0),
      Zswing = ifelse(StrikeZoneIndicator == 1 & SwingIndicator == 1, 1,0),
      GBindicator = ifelse(TaggedHitType=='GroundBall',1,0),
      Chaseindicator = ifelse(SwingIndicator==1 & StrikeZoneIndicator == 0,1,0),
      OutofZone = ifelse(StrikeZoneIndicator == 0,1,0),
      totalbases = ifelse(!is.na(slg), slg, 0)
    )
  
  df <- df %>%
    mutate(PlayResult = ifelse(is.na(PlayResult) | PlayResult == "Undefined", PitchCall, PlayResult))
  
  df$TaggedPitchType <- factor(df$TaggedPitchType,
                               levels = c("Fastball", "Sinker", "Cutter","Curveball", "Slider", "ChangeUp", "Splitter", "Sweeper", 'Knuckleball', 'Other'))
  
  df <- df %>% 
    mutate(RelSide = case_when(
      PitcherThrows == "Right" ~ RelSide,
      PitcherThrows == "Left" ~ -RelSide,
      PitcherThrows %in% c("Both", "Undefined") & RelSide > 0 ~ RelSide,
      PitcherThrows %in% c("Both", "Undefined") & RelSide < 0 ~ -RelSide),
      ax0 = case_when(
        PitcherThrows == "Right" ~ ax0,
        PitcherThrows == "Left" ~ -ax0,
        PitcherThrows %in% c("Both", "Undefined") & ax0 > 0 ~ ax0,
        PitcherThrows %in% c("Both", "Undefined") & ax0 < 0 ~ -ax0),
      PlateLocHeight = PlateLocHeight*12,
      PlateLocSide = PlateLocSide*12,
      ax0 = -ax0) %>%
    group_by(Pitcher, GameID) %>%
    mutate(
      primary_pitch = case_when(
        any(TaggedPitchType == "Fastball") ~ "Fastball",
        any(TaggedPitchType == "Sinker") ~ "Sinker",
        TRUE ~ names(sort(table(TaggedPitchType), decreasing = TRUE))[1]
      )
    ) %>%
    group_by(Pitcher, GameID, primary_pitch) %>%
    mutate(
      primary_az0 = mean(az0[TaggedPitchType == primary_pitch], na.rm = TRUE),
      primary_velo = mean(RelSpeed[TaggedPitchType == primary_pitch], na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    mutate(az0_diff = az0 - primary_az0, 
           velo_diff = RelSpeed - primary_velo)
  
  # Add season indicator
  df[[season_indicator]] <- 1
  
  # Add batter info
  df <- df %>% 
    left_join(batter_info, by = c("Batter" = "Name")) %>%
    mutate(
      height_inches = as.numeric(str_extract(Ht, "^\\d+")) * 12 +
        as.numeric(str_extract(Ht, "(?<=\\s)\\d+")),
      sz_top = if_else(
        is.na(height_inches), 38,
        round(height_inches * 0.535, 1)
      ),
      sz_bot = if_else(
        is.na(height_inches), 19,
        round(height_inches * 0.27, 1)
      )) 
  
  # Add team/league info
  # Thin TruMedia payloads can omit these; NA-fill so the joins survive
  # instead of killing the whole pipeline into light prep (no grades).
  for (cc in c("HomeTeam", "AwayTeam", "League")) {
    if (!cc %in% names(df)) df[[cc]] <- NA_character_
  }
  df <- df %>% 
    left_join(team_leagues, by = "HomeTeam") %>% 
    left_join(team_leagues %>% rename(AwayLeague = HomeLeague, AwayTeam = HomeTeam), by = "AwayTeam") %>% 
    left_join(league_run_environments, by = "League") 
  
  # ============================================================
  # SIGN-CONVENTION RESTORE (was: model split + in-R xgboost predict)
  # ------------------------------------------------------------
  # Plus scores are already attached by score_promodel() earlier in
  # this function (Pitch Profiler bundle via reticulate). The old
  # valid/invalid split existed only to feed the R recipes; what
  # remains is restoring the app-wide sign convention the old
  # recombine step produced, applied uniformly to ALL rows.
  # ============================================================
  unflip_relside_ax0 <- function(d) {
    d %>% mutate(RelSide = case_when(
      PitcherThrows == "Right" ~ RelSide,
      PitcherThrows == "Left" ~ -RelSide,
      PitcherThrows %in% c("Both", "Undefined") & RelSide > 0 ~ RelSide,
      PitcherThrows %in% c("Both", "Undefined") & RelSide < 0 ~ -RelSide),
      ax0 = case_when(
        PitcherThrows == "Right" ~ ax0,
        PitcherThrows == "Left" ~ -ax0,
        PitcherThrows %in% c("Both", "Undefined") & ax0 > 0 ~ ax0,
        PitcherThrows %in% c("Both", "Undefined") & ax0 < 0 ~ -ax0),
      ax0 = -ax0)
  }
  df <- unflip_relside_ax0(df)

  cat("  [proModel] Frame ready:", nrow(df), "rows |",
      sum(is.finite(df$stuff_xrv)), "with plus scores\n")
  
  return(list(data = df, spray = spray_df))
}
                                 
# ============================================================
# PROCESS ALL DATASETS
# ============================================================

add_roster_flag <- function(df) {
  df %>%
    left_join(bio %>% select(Pitcher_FL, Roster_2026),
              by = c("Pitcher" = "Pitcher_FL")) %>%
    mutate(Roster_2026 = ifelse(is.na(Roster_2026), "No", Roster_2026))
}



pdiv <- function(num, den) ifelse(den > 0, num/den, NA_real_)



# Normalize BatterSide to "L"/"R"
norm_side <- function(x) toupper(substr(x, 1, 1))


# Safe colored-cell renderer (v is a label string; pct is numeric in [0,1])
shade_cell_html <- function(label, pct) {
  pal <- scales::gradient_n_pal(c("#E1463E","white","#00840D"), values = c(0,0.5,1))
  col <- pal(ifelse(is.finite(pct), pct, 0.5))
  tip <- if (is.finite(pct)) sprintf("D1 percentile: %d", round(pct * 100)) else "D1 percentile: n/a"
  gt::html(sprintf(
    "<div title='%s' style='background-color:%s;padding:2px 6px;border-radius:4px;display:inline-block;cursor:help;'>%s</div>",
    tip,
    col,
    htmltools::htmlEscape(ifelse(is.na(label), "-", as.character(label)))
  ))
}



build_p5_ecdf_maps <- function(p5_ref) {
  ref <- p5_ref %>% dplyr::mutate(BatterSide = norm_side(BatterSide))
  
  ecdf_safe <- function(v) {
    v <- stats::na.omit(v)
    if (length(v) > 1) stats::ecdf(v) else function(z) rep(0.5, length(z))
  }

  # Stuff+ / Pitching+ / Location+ need their own fallback. The P5
  # reference pool is legacy data that the model never scored (the
  # startup log says "P5_ind skipped (legacy pool, not graded)" and
  # "0 with plus scores"), so stuff_plus is 100% NA here and ecdf_safe
  # was handing back the constant-0.5 function -- which is why every
  # plus cell rendered white at the 50th percentile no matter the value.
  # These models are BUILT on a 100-mean / 10-SD scale, so when there is
  # no empirical reference we grade analytically instead of flat.
  plus_ecdf <- function(v) {
    v <- stats::na.omit(v)
    if (length(v) > 1) return(stats::ecdf(v))
    function(z) stats::pnorm((z - 100) / 10)
  }
  
  mov_by_pitch <- ref %>%
    dplyr::group_by(TaggedPitchType) %>%
    dplyr::summarise(
      ecdf_speed     = list(ecdf_safe(RelSpeed)),
      ecdf_spin      = list(ecdf_safe(SpinRate)),
      ecdf_ivb       = list(ecdf_safe(InducedVertBreak)),
      ecdf_hb        = list(ecdf_safe(HorzBreak)),
      ecdf_ext       = list(ecdf_safe(Extension)),
      ecdf_relh      = list(ecdf_safe(RelHeight)),
      ecdf_rels      = list(ecdf_safe(RelSide)),
      ecdf_vaa       = list(ecdf_safe(if ("VertApprAngle" %in% names(ref)) VertApprAngle else NA_real_)),  # NEW
      ecdf_haa       = list(ecdf_safe(if ("HorzApprAngle" %in% names(ref)) HorzApprAngle else NA_real_)),  # NEW
      ecdf_stuffplus    = list(plus_ecdf(if ("stuff_plus"    %in% names(ref)) stuff_plus    else NA_real_)),
      ecdf_pitchingplus = list(plus_ecdf(if ("pitching_plus" %in% names(ref)) pitching_plus else NA_real_)),
      ecdf_locationplus = list(plus_ecdf(if ("location_plus" %in% names(ref)) location_plus else NA_real_)),
      .groups = "drop"
    )
  
  # Result rates ECDFs by pitch type + split, aggregated at (Pitcher, pitch, split)
  rate_agg <- ref %>%
    dplyr::group_by(Pitcher, TaggedPitchType, BatterSide) %>%
    dplyr::summarise(
      strike = pdiv(sum(StrikeIndicator,    na.rm = TRUE), dplyr::n()),
      whiff  = pdiv(sum(WhiffIndicator,     na.rm = TRUE), sum(SwingIndicator, na.rm = TRUE)),
      zwhiff = pdiv(sum(Zwhiffind,          na.rm = TRUE), sum(Zswing,         na.rm = TRUE)),
      chase  = pdiv(sum(Chaseindicator,     na.rm = TRUE), sum(OutofZone,      na.rm = TRUE)),
      hh     = pdiv(sum(HHindicator,        na.rm = TRUE), sum(biphh,          na.rm = TRUE)),
      gb     = pdiv(sum(GBindicator,        na.rm = TRUE), sum(BIPind,         na.rm = TRUE)),
      avg    = pdiv(sum(HitIndicator,       na.rm = TRUE), sum(ABindicator,    na.rm = TRUE)),
      slg    = pdiv(sum(totalbases,         na.rm = TRUE), sum(ABindicator,    na.rm = TRUE)),
      zone00 = pdiv(sum(StrikeZoneIndicator[Balls == 0 & Strikes == 0], na.rm = TRUE),
                    sum(Balls == 0 & Strikes == 0, na.rm = TRUE)),
      swing  = pdiv(sum(SwingIndicator,     na.rm = TRUE), dplyr::n()),
      whiff2k = pdiv(sum(WhiffIndicator[Strikes == 2], na.rm = TRUE),
                     sum(SwingIndicator[Strikes == 2], na.rm = TRUE)),
      .groups = "drop"
    )
  
  
  res_by_pitch_split <- rate_agg %>%
    tidyr::nest(data = -c(TaggedPitchType, BatterSide)) %>%
    dplyr::mutate(
      # high-is-better
      ecdf_strike = purrr::map(data, ~ ecdf_safe(.x$strike)),
      ecdf_whiff  = purrr::map(data, ~ ecdf_safe(.x$whiff)),
      ecdf_zwhiff = purrr::map(data, ~ ecdf_safe(.x$zwhiff)),
      ecdf_chase  = purrr::map(data, ~ ecdf_safe(.x$chase)),
      ecdf_gb     = purrr::map(data, ~ ecdf_safe(.x$gb)),
      ecdf_zone00 = purrr::map(data, ~ ecdf_safe(.x$zone00)),
      ecdf_swing  = purrr::map(data, ~ ecdf_safe(.x$swing)),
      ecdf_whiff2k = purrr::map(data, ~ ecdf_safe(.x$whiff2k)),
      # neutral versions (kept for reference)
      ecdf_hh     = purrr::map(data, ~ ecdf_safe(.x$hh)),
      ecdf_avg    = purrr::map(data, ~ ecdf_safe(.x$avg)),
      ecdf_slg    = purrr::map(data, ~ ecdf_safe(.x$slg)),
      # LOW-is-better variants (use -x so ties at 0 become GREEN)
      ecdf_hh_low  = purrr::map(data, ~ ecdf_safe(-.x$hh)),
      ecdf_avg_low = purrr::map(data, ~ ecdf_safe(-.x$avg)),
      ecdf_slg_low = purrr::map(data, ~ ecdf_safe(-.x$slg))
    ) %>%
    dplyr::select(-data)
  
  list(mov = mov_by_pitch, res = res_by_pitch_split)
}

# tiny getters
get_mov_ecdf <- function(maps, pitch, metric) {
  r <- dplyr::filter(maps$mov, TaggedPitchType == pitch)
  if (!nrow(r)) return(function(z) rep(0.5, length(z)))
  r[[paste0("ecdf_", metric)]][[1]]
}
get_res_ecdf <- function(maps, pitch, side, metric) {
  r <- dplyr::filter(maps$res, TaggedPitchType == pitch, BatterSide == norm_side(side))
  if (!nrow(r)) return(function(z) rep(0.5, length(z)))
  r[[paste0("ecdf_", metric)]][[1]]
}



get_pitcher_image_url <- function(arm_angle, is_lefty) {
  if (is_lefty) {
    # Left-handed pitchers
    if (arm_angle >= 40) {
      return("https://i.imgur.com/WYChFiq.png")  # LHP_HIGH_FRONT
    } else if (arm_angle >= 10) {
      return("https://i.imgur.com/KeeUG6z.png")  # LHP_MID_FRONT
    } else {
      return("https://i.imgur.com/HewvuXT.png")  # LHP_LOW_FRONT
    }
  } else {
    # Right-handed pitchers
    if (arm_angle >= 40) {
      return("https://i.imgur.com/84v0KDV.png")  # RHP_HIGH_FRONT
    } else if (arm_angle >= 10) {
      return("https://i.imgur.com/RLlIVrc.png")  # RHP_MID_FRONT
    } else {
      return("https://i.imgur.com/cjpIEME.png")  # RHP_LOW_FRONT
    }
  }
}

# Main arm angle plotting function for the pitcher app
plot_arm_angles_app <- function(
    df, 
    pitcher_name, 
    pitch_colors,
    shoulder_scale = 0.70,
    view = c("mound", "plate"),
    angle_from_vertical = FALSE,
    show_points = TRUE,
    ray_len_ft = NULL,
    mirror_lefties = FALSE,
    show_pitcher_figure = TRUE,
    figure_size = 0.6,
    show_legend = TRUE,
    title_text = NULL
) {
  
  view <- match.arg(view)
  
  # Auto-detect pitcher column
  pcol <- intersect(c("Pitcher", "PitcherName", "player_name", "Name"), names(df))[1]
  if (is.na(pcol)) stop("No pitcher column found.")
  
  # Filter data for the specific pitcher
  dat <- df %>% filter(.data[[pcol]] == pitcher_name)
  if (nrow(dat) == 0) {
    stop(paste("No data for pitcher:", pitcher_name))
  }
  
  # Detect handedness from throw column or RelSide
  tcol_idx <- which(grepl("throw", names(dat), ignore.case = TRUE))
  throws_raw <- if (length(tcol_idx)) dat[[tcol_idx[1]]] else NA
  throws_val <- na.omit(unique(as.character(throws_raw)))[1]
  is_lefty <- !is.na(throws_val) && toupper(substr(throws_val, 1, 1)) == "L"
  if (is.na(is_lefty)) is_lefty <- median(dat$RelSide, na.rm = TRUE) < 0
  
  # Get height data (you already have height_inches column)
  h_in <- dat$height_inches[which(!is.na(dat$height_inches))[1]]
  if (is.na(h_in)) {
    h_in <- 72  # Default 6 feet if height data missing
  }
  shoulder_y <- (h_in * shoulder_scale) / 12
  
  # Calculate averages per pitch type
  avg <- dat %>%
    filter(!is.na(RelSide), !is.na(RelHeight),
           !is.na(TaggedPitchType), !is.na(arm_angle_savant)) %>%
    group_by(TaggedPitchType) %>%
    summarise(
      RelSide = mean(RelSide, na.rm = TRUE),
      RelHeight = mean(RelHeight, na.rm = TRUE),
      arm_angle = mean(arm_angle_savant, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (nrow(avg) == 0) {
    stop(paste("No usable arm angle data for pitcher:", pitcher_name))
  }
  
  # Get overall arm angle for the pitcher figure
  overall_arm_angle <- mean(dat$arm_angle_savant, na.rm = TRUE)
  
  avg <- avg %>%
    mutate(
      # Plate view coordinates: RHP left of center, LHP right of center
      x_rel = if (view == "plate") {
        if (is_lefty)  abs(RelSide) else -abs(RelSide)
      } else {
        if (is_lefty) -abs(RelSide) else  abs(RelSide)
      },
      y_rel = RelHeight,
      theta_deg = if (angle_from_vertical) 90 - arm_angle else arm_angle,
      theta_deg = pmin(pmax(theta_deg, 0.1), 89.9),
      theta_rad = theta_deg * pi / 180
    )
  
  seg <- avg %>%
    mutate(
      dy     = y_rel - shoulder_y,
      tan_th = tan(theta_rad),
      dx     = ifelse(abs(tan_th) < 1e-6, 0, dy / tan_th),
      
      # Shoulder sits closer to center than the release point
      xs     = x_rel - sign(x_rel) * dx,  # RHP: x_rel<0 -> xs = x_rel + dx; LHP: xs = x_rel - dx
      ys     = shoulder_y,
      xe     = x_rel,
      ye     = y_rel
    )
  
  # Optional fixed tail length that ends at the dot
  if (!is.null(ray_len_ft)) {
    seg <- seg %>%
      mutate(
        L = sqrt((xe - xs)^2 + (ye - ys)^2),
        ux = (xe - xs) / L, 
        uy = (ye - ys) / L,
        xs = xe - ray_len_ft * ux,
        ys = ye - ray_len_ft * uy
      )
  }
  
  # Axis labels
  left_lab <- if (view == "plate") "3B ←" else "← 1B"
  right_lab <- if (view == "plate") "→ 1B" else "3B →"
  
  # Legend labels
  label_map <- setNames(
    paste0(avg$TaggedPitchType, " (", round(avg$arm_angle, 1), "°)"),
    avg$TaggedPitchType
  )
  
  # Build custom title
  if (is.null(title_text)) {
    title_text <- paste("Arm Angles (Batter View):", pitcher_name)
  }
  
  # Create base plot
  p <- ggplot(seg) +
    geom_segment(
      aes(x = xs, y = ys, xend = xe, yend = ye, color = TaggedPitchType),
      linewidth = 1.2, alpha = 0.95
    )
  
  # Add release points if requested
  if (show_points) {
    p <- p + geom_point(
      aes(x = xe, y = ye, fill = TaggedPitchType),
      shape = 21, size = 5, stroke = 0.25,
      color = "black", alpha = 0.95
    )
  }
  
  # Add pitcher figure if requested - using your custom URLs
  if (show_pitcher_figure) {
    img_url <- get_pitcher_image_url(overall_arm_angle, is_lefty)
    p <- p + ggimage::geom_image(
      image = img_url, 
      x = 0,  # Center on rubber
      y = 2.5,  # Position on mound
      size = figure_size,
      inherit.aes = FALSE, 
      asp = 1
    )
  }
  
  # Complete the plot formatting
  p <- p +
    xlim(-5, 5) + ylim(0, 8) +
    annotate("text", x = -5, y = 8, label = left_lab, size = 3, hjust = 0) +
    annotate("text", x = 5, y = 8, label = right_lab, size = 3, hjust = 1) +
    theme_minimal() +
    xlab("Horizontal Release (ft)") + 
    ylab("Vertical Release (ft)") +
    ggtitle(title_text) +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      panel.background = element_rect(fill = "#ffffff"),
      legend.position = if (show_legend) "bottom" else "none",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    ) +
    geom_rect(
      aes(xmin = -5, xmax = 5, ymin = 0, ymax = 0.83),
      inherit.aes = FALSE, fill = "#632b11", linewidth = 1
    ) +
    geom_rect(
      aes(xmin = -0.5, xmax = 0.5, ymin = 0.8, ymax = 0.95),
      inherit.aes = FALSE, fill = "white", color = "black", linewidth = 0.5
    ) +
    scale_color_manual(
      values = pitch_colors,
      breaks = avg$TaggedPitchType,
      labels = label_map,
      name = "Pitch (avg arm angle)"
    ) +
    scale_fill_manual(values = pitch_colors, guide = "none")
  
  return(p)
}

compute_count_usage_table <- function(df, pitchers) {
  hard_types <- c("Fastball", "Sinker", "FourSeamFastBall", "TwoSeamFastBall")
  soft_types <- c("Slider", "Curveball", "ChangeUp", "Splitter", "Sweeper", "Cutter",
                  "Knuckleball", "Slurve")
  
  df <- df %>%
    dplyr::filter(Pitcher %in% pitchers,
                  !is.na(TaggedPitchType),
                  TaggedPitchType != "Other",
                  !is.na(Balls), !is.na(Strikes)) %>%
    dplyr::mutate(
      pitch_group = dplyr::case_when(
        TaggedPitchType %in% hard_types ~ "Hard",
        TaggedPitchType %in% soft_types ~ "Soft",
        TRUE ~ NA_character_
      ),
      count_label = paste0(Balls, "-", Strikes),
      side = dplyr::case_when(
        BatterSide == "Left"  ~ "L",
        BatterSide == "Right" ~ "R",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(pitch_group), !is.na(side))
  
  # All possible counts in order
  all_counts <- c("0-0","0-1","0-2","1-0","1-1","1-2","2-0","2-1","2-2","3-0","3-1","3-2")
  
  # Per-count percentages
  usage_by_count <- df %>%
    dplyr::group_by(Pitcher, side, count_label) %>%
    dplyr::mutate(bucket_n = dplyr::n()) %>%
    dplyr::group_by(Pitcher, side, count_label, pitch_group) %>%
    dplyr::summarise(
      pct = round(dplyr::n() / dplyr::first(bucket_n) * 100, 1),
      .groups = "drop"
    )
  
  # Totals (all counts combined)
  usage_total <- df %>%
    dplyr::group_by(Pitcher, side) %>%
    dplyr::mutate(bucket_n = dplyr::n()) %>%
    dplyr::group_by(Pitcher, side, pitch_group) %>%
    dplyr::summarise(
      pct = round(dplyr::n() / dplyr::first(bucket_n) * 100, 1),
      .groups = "drop"
    ) %>%
    dplyr::mutate(count_label = "Totals")
  
  combined <- dplyr::bind_rows(usage_by_count, usage_total)
  
  wide <- combined %>%
    tidyr::pivot_wider(
      names_from  = count_label,
      values_from = pct,
      values_fill = 0
    )
  
  # Ensure all columns exist
  for (col in c("Totals", all_counts)) {
    if (!col %in% names(wide)) wide[[col]] <- 0
  }
  
  wide %>%
    dplyr::select(Pitcher, side, pitch_group, Totals, dplyr::all_of(all_counts)) %>%
    dplyr::arrange(Pitcher, side, dplyr::desc(pitch_group))
}

draw_count_usage_report <- function(usage_data, pitchers,
                                    title = "Team Count Usage Report",
                                    side_filter = "L") {
  
  n_pitchers <- length(pitchers)
  
  margin_top    <- 0.07
  margin_bottom <- 0.02
  margin_left   <- 0.01
  margin_right  <- 0.01
  title_height  <- 0.05
  
  body_top    <- 1 - margin_top - title_height
  body_height <- body_top - margin_bottom
  row_height  <- min(body_height / max(n_pitchers, 1), 0.042)
  
  count_labels <- c("Totals","0-0","0-1","0-2","1-0","1-1","1-2","2-0","2-1","2-2","3-0","3-1","3-2")
  n_cols       <- length(count_labels)
  
  name_left  <- margin_left
  name_width <- 0.11
  label_width <- 0.04
  table_left  <- name_left + name_width + label_width + 0.005
  table_right <- 1 - margin_right
  table_width <- table_right - table_left
  col_width   <- table_width / n_cols
  
  col_teal   <- "#006F71"
  col_peru   <- "#A27752"
  col_header <- "#2C3E50"
  col_hard   <- "#3465cb"
  col_soft   <- "#65aa02"
  col_grid   <- "grey75"
  col_bg_alt <- "#f7fafa"
  
  pct_fill <- function(pct) {
    if (is.na(pct) || pct == 0) return("white")
    pal <- colorRampPalette(c("#E1463E", "white", "#00840D"))(101)
    idx <- pmax(1, pmin(101, round(pct) + 1))
    pal[idx]
  }
  
  text_col_for <- function(pct) {
    if (is.na(pct) || (pct > 25 & pct < 75)) "black" else if (pct >= 90 | pct <= 10) "white" else "black"
  }
  
  side_label <- if (side_filter == "L") "vs Left-Handed Hitters" else "vs Right-Handed Hitters"
  
  grid::grid.text(title, x = 0.5, y = 1 - margin_top + 0.015,
                  gp = grid::gpar(fontsize = 14, fontface = "bold", col = col_teal),
                  just = "centre")
  grid::grid.text(side_label, x = 0.5, y = 1 - margin_top - 0.015,
                  gp = grid::gpar(fontsize = 11, fontface = "bold", col = col_peru),
                  just = "centre")
  
  header_y <- body_top + 0.008
  
  for (j in seq_along(count_labels)) {
    cx <- table_left + (j - 0.5) * col_width
    grid::grid.text(count_labels[j], x = cx, y = header_y,
                    gp = grid::gpar(fontsize = 7, fontface = "bold", col = col_header))
  }
  
  grid::grid.lines(x = c(margin_left, 1 - margin_right),
                   y = c(body_top - 0.002, body_top - 0.002),
                   gp = grid::gpar(col = col_header, lwd = 0.8))
  
  for (i in seq_along(pitchers)) {
    p        <- pitchers[i]
    y_center <- body_top - (i - 0.5) * row_height - 0.004
    y_bot    <- y_center - row_height / 2
    
    if (i %% 2 == 0) {
      grid::grid.rect(x = 0.5, y = y_center,
                      width = 1 - margin_left - margin_right,
                      height = row_height,
                      gp = grid::gpar(fill = col_bg_alt, col = NA))
    }
    
    grid::grid.text(p, x = name_left + 0.005, y = y_center, just = "left",
                    gp = grid::gpar(fontsize = 7.5, fontface = "bold", col = col_header))
    
    p_data <- usage_data %>% dplyr::filter(Pitcher == p, side == side_filter)
    hard_data <- p_data %>% dplyr::filter(pitch_group == "Hard")
    soft_data <- p_data %>% dplyr::filter(pitch_group == "Soft")
    
    hard_y <- y_center + row_height * 0.22
    soft_y <- y_center - row_height * 0.22
    
    grid::grid.text("Hard%", x = name_left + name_width + 0.002, y = hard_y, just = "left",
                    gp = grid::gpar(fontsize = 5.5, col = col_hard, fontface = "bold"))
    grid::grid.text("Soft%", x = name_left + name_width + 0.002, y = soft_y, just = "left",
                    gp = grid::gpar(fontsize = 5.5, col = col_soft, fontface = "bold"))
    
    for (j in seq_along(count_labels)) {
      bucket <- count_labels[j]
      cx     <- table_left + (j - 0.5) * col_width
      
      hval <- if (nrow(hard_data) > 0 && bucket %in% names(hard_data)) hard_data[[bucket]][1] else 0
      sval <- if (nrow(soft_data) > 0 && bucket %in% names(soft_data)) soft_data[[bucket]][1] else 0
      
      is_totals <- (j == 1)
      cell_font <- if (is_totals) 7 else 6.5
      cell_face <- if (is_totals) "bold" else "plain"
      
      hard_fill <- pct_fill(hval)
      soft_fill <- pct_fill(sval)
      hard_text_col <- text_col_for(hval)
      soft_text_col <- text_col_for(sval)
      
      grid::grid.rect(x = cx, y = hard_y,
                      width = col_width * 0.92, height = row_height * 0.38,
                      gp = grid::gpar(fill = hard_fill, col = col_grid, lwd = 0.3))
      grid::grid.rect(x = cx, y = soft_y,
                      width = col_width * 0.92, height = row_height * 0.38,
                      gp = grid::gpar(fill = soft_fill, col = col_grid, lwd = 0.3))
      
      grid::grid.text(ifelse(hval == 0, "-", paste0(hval, "%")),
                      x = cx, y = hard_y,
                      gp = grid::gpar(fontsize = cell_font, fontface = cell_face, col = hard_text_col))
      grid::grid.text(ifelse(sval == 0, "-", paste0(sval, "%")),
                      x = cx, y = soft_y,
                      gp = grid::gpar(fontsize = cell_font, fontface = cell_face, col = soft_text_col))
    }
    
    grid::grid.lines(x = c(margin_left, 1 - margin_right),
                     y = c(y_bot, y_bot),
                     gp = grid::gpar(col = col_grid, lwd = 0.25))
  }
  
  totals_right_x <- table_left + col_width
  grid::grid.lines(x = c(totals_right_x, totals_right_x),
                   y = c(body_top - 0.002, body_top - n_pitchers * row_height - 0.004),
                   gp = grid::gpar(col = col_peru, lwd = 1.2))
  
  grid::grid.text(
    paste("Generated:", Sys.Date(), " | Coastal Carolina Baseball Analytics"),
    x = 0.5, y = 0.006,
    gp = grid::gpar(fontsize = 6, col = "grey60"), just = "centre")
}

# Build expected movement grid by release height
build_expected_movement_grid <- function(data, bin_size = 0.10) {
  if (is.null(data)) return(NULL)
  
  data %>%
    filter(!is.na(TaggedPitchType), !is.na(RelHeight),
           !is.na(HorzBreak), !is.na(InducedVertBreak),
           TaggedPitchType != "Other") %>%
    mutate(RH_bin = round(RelHeight / bin_size) * bin_size) %>%
    group_by(TaggedPitchType, RH_bin) %>%
    summarise(
      exp_hb = median(HorzBreak, na.rm = TRUE),
      exp_ivb = median(InducedVertBreak, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    filter(n >= 10)
}

# exp_movement_grid is built in R/07_pools.R (build mode) or read as an artifact (serve mode).

# Report version of the Viz tab's square (traditional) movement chart:
# same grid, limits, background, and arm-angle methodology — but with a
# thicker arm-angle line, filled expected-movement diamonds, and only
# big average points per pitch type (no individual pitches).
create_square_movement_plot <- function(df) {
  df <- df %>% dplyr::filter(!is.na(HorzBreak), !is.na(InducedVertBreak),
                             TaggedPitchType != "Other")
  # show-me pitches clutter the chart: drop types under 8% usage
  if (nrow(df) > 0) {
    usage <- prop.table(table(df$TaggedPitchType))
    keep_pt <- names(usage)[usage >= 0.08]
    if (length(keep_pt) > 0)
      df <- df[df$TaggedPitchType %in% keep_pt, , drop = FALSE]
  }
  if (nrow(df) < 5) {
    return(ggplot() + theme_void() +
             annotate("text", x = 0, y = 0, label = "Insufficient data"))
  }
  is_lefty <- isTRUE(toupper(substr(df$PitcherThrows[1], 1, 1)) == "L")
  avg <- df %>% dplyr::group_by(TaggedPitchType) %>%
    dplyr::summarise(HB = mean(HorzBreak, na.rm = TRUE),
                     IVB = mean(InducedVertBreak, na.rm = TRUE),
                     .groups = "drop")
  aa <- suppressWarnings(mean(df$arm_angle_savant, na.rm = TRUE))
  rel_side_sign <- suppressWarnings(sign(mean(df$RelSide, na.rm = TRUE)))
  if (is.na(rel_side_sign) || rel_side_sign == 0)
    rel_side_sign <- if (is_lefty) -1 else 1
  lim <- 25
  p <- ggplot() +
    geom_hline(yintercept = 0, color = "gray55", linewidth = 0.4) +
    geom_vline(xintercept = 0, color = "gray55", linewidth = 0.4)
  if (is.finite(aa)) {
    slope <- tan(aa * pi / 180) * rel_side_sign
    p <- p + geom_abline(slope = slope, intercept = 0,
                         linetype = "dashed", color = "gray30",
                         linewidth = 1.2)
  }
  if (exists("exp_movement_grid")) {
    exp_pts <- df %>%
      dplyr::mutate(RH_bin = round(RelHeight / 0.10) * 0.10) %>%
      dplyr::left_join(exp_movement_grid,
                       by = c("TaggedPitchType", "RH_bin")) %>%
      dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(HB = mean(exp_hb, na.rm = TRUE),
                       IVB = mean(exp_ivb, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(!is.na(HB), !is.na(IVB))
    # grid is RHP-normalized; mirror expected HB for lefties
    if (is_lefty && nrow(exp_pts) > 0) exp_pts$HB <- -exp_pts$HB
    if (nrow(exp_pts) > 0)
      p <- p + geom_point(data = exp_pts,
                          aes(HB, IVB, fill = TaggedPitchType),
                          shape = 23, size = 6.5, stroke = 1, color = "black")
  }
  p <- p +
    geom_point(data = avg, aes(HB, IVB, fill = TaggedPitchType),
               shape = 21, size = 12, color = "black", stroke = 1.1) +
    scale_fill_manual(values = pitch_colors, name = NULL) +
    coord_fixed(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
    labs(x = "Horizontal Break (in)", y = "Induced Vertical Break (in)") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "none")
  if (is.finite(aa))
    p <- p + annotate("label", x = -lim + 1, y = lim - 1,
                      label = sprintf("Arm angle: %.0f\u00b0", aa),
                      hjust = 0, vjust = 1, size = 4.2, fontface = "italic",
                      color = "gray25", fill = "white", label.size = 0)
  p
}

# New customizable movement chart
create_pitcher_movement_plot <- function(data, pitcher_name, expected_grid = NULL,
                                         show_arm_angle_rays = TRUE,
                                         show_arm_angle_annotation = TRUE,
                                         marker_style = "scaled_usage",
                                         show_individual_pitches = TRUE,
                                         show_expected_diamonds = TRUE,
                                         selected_pitch_types = NULL) {
  if (is.null(data)) {
    return(ggplot() + theme_void() + 
             annotate("text", x = 0, y = 0, label = "No data available"))
  }
  
  pitcher_data <- data %>%
    filter(Pitcher == pitcher_name,
           !is.na(HorzBreak), !is.na(InducedVertBreak),
           !is.na(TaggedPitchType), TaggedPitchType != "Other",
           !is.na(RelSpeed))
  
  if (!is.null(selected_pitch_types) && length(selected_pitch_types) > 0) {
    pitcher_data <- pitcher_data %>%
      filter(TaggedPitchType %in% selected_pitch_types)
  }
  
  if (nrow(pitcher_data) < 5) {
    return(ggplot() + theme_void() + 
             annotate("text", x = 0, y = 0, label = "Insufficient data"))
  }
  
  pitcher_throws <- pitcher_data$PitcherThrows[1]
  if (is.na(pitcher_throws)) pitcher_throws <- "Right"
  is_lefty <- pitcher_throws == "Left"
  
  h_in <- if ("height_inches" %in% names(pitcher_data)) {
    med_h <- median(pitcher_data$height_inches, na.rm = TRUE)
    if (is.na(med_h)) 72 else med_h
  } else if ("pitcher_height_inches" %in% names(pitcher_data)) {
    med_h <- median(pitcher_data$pitcher_height_inches, na.rm = TRUE)
    if (is.na(med_h)) 72 else med_h
  } else 72
  shoulder_pos <- h_in * 0.70
  
  p_mean <- pitcher_data %>%
    group_by(TaggedPitchType) %>%
    summarize(
      HorzBreak = mean(HorzBreak, na.rm = TRUE),
      InducedVertBreak = mean(InducedVertBreak, na.rm = TRUE),
      mean_velo = round(mean(RelSpeed, na.rm = TRUE), 1),
      mean_rel_height = mean(RelHeight, na.rm = TRUE),
      mean_rel_side = mean(RelSide, na.rm = TRUE),
      mean_arm_angle = mean(arm_angle_savant, na.rm = TRUE),
      mean_stuff = round(mean(stuff_plus, na.rm = TRUE), 0),
      usage = n(),
      .groups = "drop"
    ) %>%
    mutate(
      usage_pct = round(usage / sum(usage) * 100, 1),
      scaled_usage = (usage - min(usage)) / max((max(usage) - min(usage)), 1) * (14 - 10) + 10,
      RH_bin = round(mean_rel_height / 0.10) * 0.10
    ) %>%
    filter(usage >= 3)
  
  p_mean$exp_hb <- NA_real_
  p_mean$exp_ivb <- NA_real_
  p_mean$has_expected <- FALSE
  
  if (!is.null(expected_grid) && nrow(expected_grid) > 0) {
    for (i in seq_len(nrow(p_mean))) {
      pt <- p_mean$TaggedPitchType[i]
      rh <- p_mean$RH_bin[i]
      
      match_row <- expected_grid %>%
        filter(TaggedPitchType == pt, abs(RH_bin - rh) < 0.15)
      
      if (nrow(match_row) > 0) {
        match_row <- match_row %>%
          mutate(dist = abs(RH_bin - rh)) %>%
          arrange(dist) %>%
          slice(1)
        p_mean$exp_hb[i] <- if (is_lefty) -match_row$exp_hb[1] else match_row$exp_hb[1]
        p_mean$exp_ivb[i] <- match_row$exp_ivb[1]
        p_mean$has_expected[i] <- TRUE
      }
    }
  }
  
  arm_angle_savant <- median(pitcher_data$arm_angle_savant, na.rm = TRUE)
  if (is.na(arm_angle_savant)) arm_angle_savant <- 45
  
  
  if (show_arm_angle_rays && nrow(p_mean) > 0) {
    ray_length <- 18
    
    arm_rays <- p_mean %>%
      mutate(
        rel_x_raw = if (is_lefty) -abs(mean_rel_side) else abs(mean_rel_side),
        rel_y_raw = mean_rel_height - (shoulder_pos / 12),
        rel_mag = sqrt(rel_x_raw^2 + rel_y_raw^2),
        rel_mag = ifelse(rel_mag == 0 | is.na(rel_mag), 1, rel_mag),
        dir_x = rel_x_raw / rel_mag,
        dir_y = rel_y_raw / rel_mag,
        ray_xs = 0,
        ray_ys = 0,
        ray_xe = dir_x * ray_length,
        ray_ye = dir_y * ray_length
      )
  }
  
  circle_df <- data.frame(
    x = 38 * cos(seq(0, 2*pi, length.out = 100)),
    y = 38 * sin(seq(0, 2*pi, length.out = 100))
  )
  
  p <- ggplot(pitcher_data, aes(x = HorzBreak, y = InducedVertBreak)) +
    geom_polygon(data = circle_df, aes(x = x, y = y), fill = "#e5f3f3", color = "#e5f3f3", inherit.aes = FALSE) +
    geom_path(data = data.frame(x = 12 * cos(seq(0, 2*pi, length.out = 100)), 
                                y = 12 * sin(seq(0, 2*pi, length.out = 100))),
              aes(x = x, y = y), linetype = "dashed", color = "gray60", linewidth = 0.3, inherit.aes = FALSE) +
    geom_path(data = data.frame(x = 24 * cos(seq(0, 2*pi, length.out = 100)), 
                                y = 24 * sin(seq(0, 2*pi, length.out = 100))),
              aes(x = x, y = y), linetype = "dashed", color = "gray60", linewidth = 0.3, inherit.aes = FALSE) +
    geom_path(data = data.frame(x = 32 * cos(seq(0, 2*pi, length.out = 100)), 
                                y = 32 * sin(seq(0, 2*pi, length.out = 100))),
              aes(x = x, y = y), linetype = "solid", color = "gray50", linewidth = 0.5, inherit.aes = FALSE) +
    geom_segment(x = 0, y = -34, xend = 0, yend = 34, linewidth = 0.5, color = "grey55") +
    geom_segment(x = -34, y = 0, xend = 34, yend = 0, linewidth = 0.5, color = "grey55") +
    annotate('text', x = 0, y = 42, label = '12', size = 5, fontface = "bold") +
    annotate('text', x = 21, y = 37, label = '1', size = 4.5, fontface = "bold") +
    annotate('text', x = 37, y = 21, label = '2', size = 4.5, fontface = "bold") +
    annotate('text', x = 42, y = 0, label = '3', size = 5, fontface = "bold") +
    annotate('text', x = 37, y = -21, label = '4', size = 4.5, fontface = "bold") +
    annotate('text', x = 21, y = -37, label = '5', size = 4.5, fontface = "bold") +
    annotate('text', x = 0, y = -42, label = '6', size = 5, fontface = "bold") +
    annotate('text', x = -21, y = -37, label = '7', size = 4.5, fontface = "bold") +
    annotate('text', x = -37, y = -21, label = '8', size = 4.5, fontface = "bold") +
    annotate('text', x = -42, y = 0, label = '9', size = 5, fontface = "bold") +
    annotate('text', x = -37, y = 21, label = '10', size = 4.5, fontface = "bold") +
    annotate('text', x = -21, y = 37, label = '11', size = 4.5, fontface = "bold") +
    annotate('text', x = 12, y = -2, label = '12"', size = 3.5, color = "gray40") +
    annotate('text', x = 24, y = -2, label = '24"', size = 3.5, color = "gray40") +
    annotate('text', x = 36, y = -2, label = '36"', size = 3.5, color = "gray40") +
    annotate('text', y = 12, x = -2, label = '12"', size = 3.5, color = "gray40") +
    annotate('text', y = 24, x = -2, label = '24"', size = 3.5, color = "gray40") +
    annotate('text', y = 36, x = -2, label = '36"', size = 3.5, color = "gray40")
  
  if (show_arm_angle_rays && exists("arm_rays") && nrow(arm_rays) > 0) {
    p <- p +
      geom_segment(
        data = arm_rays,
        aes(x = ray_xs, y = ray_ys, xend = ray_xe, yend = ray_ye, color = TaggedPitchType),
        linewidth = 3.2, alpha = 0.9, inherit.aes = FALSE
      ) +
      geom_point(
        data = arm_rays,
        aes(x = ray_xe, y = ray_ye, fill = TaggedPitchType),
        shape = 21, size = 3, stroke = 0.5, color = "black", alpha = 0.9, inherit.aes = FALSE
      )
  }
  
  if (show_individual_pitches) {
    p <- p + geom_point(aes(fill = TaggedPitchType), shape = 21, size = 3.5, 
                        color = "black", stroke = 0.3, alpha = 1)
  }
  
  if (show_expected_diamonds) {
    exp_data <- p_mean %>% filter(has_expected == TRUE)
    if (nrow(exp_data) > 0) {
      p <- p +
        geom_point(data = exp_data,
                   aes(x = exp_hb, y = exp_ivb, fill = TaggedPitchType),
                   shape = 23, size = 8, stroke = 1.2, color = "black", 
                   alpha = 1, inherit.aes = FALSE)
    }
  }
  
  if (marker_style == "scaled_usage") {
    p <- p +
      geom_point(data = p_mean,
                 aes(x = HorzBreak, y = InducedVertBreak, fill = TaggedPitchType, size = scaled_usage),
                 shape = 21, color = "black", stroke = 2, alpha = 0.9, inherit.aes = FALSE) +
      geom_label(data = p_mean,
                 aes(x = HorzBreak, y = InducedVertBreak, label = mean_velo),
                 color = "black", size = 3.5, fontface = "bold", fill = "white", 
                 label.size = 0.3, inherit.aes = FALSE) +
      scale_size_identity()
  } else if (marker_style == "average") {
    p <- p +
      geom_point(data = p_mean,
                 aes(x = HorzBreak, y = InducedVertBreak, fill = TaggedPitchType),
                 shape = 21, size = 14, color = "black", stroke = 2, alpha = 0.95, inherit.aes = FALSE) +
      geom_label(data = p_mean,
                 aes(x = HorzBreak, y = InducedVertBreak, label = mean_velo),
                 color = "black", size = 4, fontface = "bold", fill = "white", 
                 label.size = 0.3, inherit.aes = FALSE)
  }
  
  p <- p +
    coord_fixed(xlim = c(-45, 45), ylim = c(-45, 45), clip = "off") + 
    scale_fill_manual(values = pitch_colors, drop = FALSE) +
    scale_color_manual(values = pitch_colors, drop = FALSE) +
    labs(title = NULL, x = "", y = "") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.margin = margin(20, 5, 5, 5)
    )
  
  return(p)
}

# ============================================================
