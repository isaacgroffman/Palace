# ============================================================
# POOLS — the pitch frames the app reads. Two modes:
#
#   serve (target): read pools + artifacts precomputed by
#                   scripts/build_serving_data.R. Boot = a few parquet reads.
#   build (legacy): download raw parquet/CSV from Hugging Face, run
#                   process_pitcher_data() + proModel scoring on every pool,
#                   bind, filter back. This is the original startup path,
#                   kept verbatim so the build script and the app can never
#                   disagree about how a pool is made.
#
# Objects produced in BOTH modes (everything downstream depends on them):
#   data, spraydata            2025 spring (Coastal)
#   fall25, sprayfall25        2025 fall
#   prespring, sprayprespring  2026 pre-spring
#   spring26, sprayspring26, spring26_game_choices, manual_data_loaded, odu_data_loaded
#   P5_2025, SBC_2025          legacy reference pools (LAZY in serve mode)
#   full_data_p5_compare       everything bound together (LAZY in serve mode)
#   p5_maps, exp_movement_grid, p5_slot_grid   derived from P5 (artifacts in serve mode)
# ============================================================

align_manual_data <- function(manual_df, reference_df) {
  if (is.null(manual_df) || nrow(manual_df) == 0) return(NULL)
  
  # Add missing columns from reference as NA
  missing_cols <- setdiff(names(reference_df), names(manual_df))
  for (col in missing_cols) {
    manual_df[[col]] <- NA
  }
  
  # Force key outcome columns to character NA (not logical)
  if (all(is.na(manual_df$PlayResult)))    manual_df$PlayResult    <- NA_character_
  if (all(is.na(manual_df$KorBB)))         manual_df$KorBB         <- NA_character_
  if (all(is.na(manual_df$TaggedHitType))) manual_df$TaggedHitType <- NA_character_
  if (all(is.na(manual_df$AutoHitType)))   manual_df$AutoHitType   <- NA_character_
  
  # "NULL" strings → real NA
  manual_df <- manual_df %>%
    mutate(across(where(is.character), ~ifelse(. == "NULL", NA_character_, .)))
  
  # ── Force numeric columns ──
  # readr guesses columns with many NAs as character or logical.
  # Coerce everything that should be numeric to numeric.
  numeric_force_cols <- c(
    "PitchNo", "PAofInning", "PitchofPA", "PitcherId", "BatterId",
    "Inning", "Outs", "Balls", "Strikes", "OutsOnPlay", "RunsScored",
    "RelSpeed", "VertRelAngle", "HorzRelAngle", "SpinRate", "SpinAxis",
    "RelHeight", "RelSide", "Extension", "VertBreak", "InducedVertBreak",
    "HorzBreak", "PlateLocHeight", "PlateLocSide", "ZoneSpeed",
    "VertApprAngle", "HorzApprAngle", "ZoneTime",
    "ExitSpeed", "Angle", "Direction", "HitSpinRate",
    "Distance", "LastTrackedDistance", "Bearing", "HangTime",
    "pfxx", "pfxz", "x0", "y0", "z0", "vx0", "vy0", "vz0",
    "ax0", "ay0", "az0",
    "EffectiveVelo", "MaxHeight", "SpeedDrop",
    "ContactPositionX", "ContactPositionY", "ContactPositionZ",
    "ThrowSpeed", "PopTime", "ExchangeTime", "TimeToBase",
    "CatchPositionX", "CatchPositionY", "CatchPositionZ",
    "ThrowPositionX", "ThrowPositionY", "ThrowPositionZ",
    "BasePositionX", "BasePositionY", "BasePositionZ",
    "ThrowTrajectoryXc0", "ThrowTrajectoryXc1", "ThrowTrajectoryXc2",
    "ThrowTrajectoryYc0", "ThrowTrajectoryYc1", "ThrowTrajectoryYc2",
    "ThrowTrajectoryZc0", "ThrowTrajectoryZc1", "ThrowTrajectoryZc2",
    "Number"
  )
  
  for (col in numeric_force_cols) {
    if (col %in% names(manual_df)) {
      manual_df[[col]] <- suppressWarnings(as.numeric(as.character(manual_df[[col]])))
    }
  }
  
  # ── Force character columns that vary in type between datasets ──
  char_force_cols <- c(
    "Date", "UTCDate", "Time", "UTCTime",
    "LocalDateTime", "UTCDateTime", "Tilt",
    "HomeTeamForeignID", "AwayTeamForeignID", "GameForeignID",
    "MeasuredDuration",
    "PitchReleaseConfidence", "PitchLocationConfidence",
    "PitchMovementConfidence", "HitLaunchConfidence",
    "HitLandingConfidence", "CatcherThrowCatchConfidence",
    "CatcherThrowReleaseConfidence", "CatcherThrowLocationConfidence",
    "BatSpeed", "VerticalAttackAngle", "HorizontalAttackAngle",
    "HitSpinAxis", "Notes", "PitcherSet", "Stadium", "Level", "League",
    "System", "PlayID", "GameUID"
  )
  
  for (col in char_force_cols) {
    if (col %in% names(manual_df))    manual_df[[col]]    <- as.character(manual_df[[col]])
    if (col %in% names(reference_df)) reference_df[[col]]  <- as.character(reference_df[[col]])
  }
  
  # Keep only columns that exist in reference
  common_cols <- intersect(names(reference_df), names(manual_df))
  manual_df <- manual_df %>% select(all_of(common_cols))
  
  cat("[Manual Data] Aligned:", ncol(manual_df), "columns,", nrow(manual_df), "rows\n")
  return(manual_df)
}

# ============================================================
# Bind hand-tracked manual games into spring26_raw
# (Houston Hawkeye manual CSV + Saturday vs ODU game)
# ============================================================
bind_manual_rows <- function(reference, manual_raw, label) {
  if (is.null(manual_raw) || nrow(manual_raw) == 0) {
    cat("[", label, "] No manual data available.\n", sep = "")
    return(reference)
  }
  aligned <- align_manual_data(manual_raw, reference)
  if (is.null(aligned) || nrow(aligned) == 0) return(reference)

  # Force known-problem columns to character in the reference for safe binding
  char_force_cols <- c(
    "Date", "UTCDate", "Time", "UTCTime",
    "LocalDateTime", "UTCDateTime", "Tilt",
    "HomeTeamForeignID", "AwayTeamForeignID", "GameForeignID",
    "MeasuredDuration",
    "PitchReleaseConfidence", "PitchLocationConfidence",
    "PitchMovementConfidence", "HitLaunchConfidence",
    "HitLandingConfidence", "CatcherThrowCatchConfidence",
    "CatcherThrowReleaseConfidence", "CatcherThrowLocationConfidence",
    "BatSpeed", "VerticalAttackAngle", "HorizontalAttackAngle",
    "HitSpinAxis", "Notes", "PitcherSet", "Stadium", "Level", "League",
    "System", "PlayID", "GameUID"
  )
  for (col in intersect(char_force_cols, names(reference))) {
    reference[[col]] <- as.character(reference[[col]])
  }

  # Final safety: match every column type between the two data frames
  for (col in intersect(names(aligned), names(reference))) {
    ref_class <- class(reference[[col]])[1]
    man_class <- class(aligned[[col]])[1]
    if (ref_class != man_class) {
      aligned[[col]] <- tryCatch({
        switch(ref_class,
               "numeric"   = suppressWarnings(as.numeric(as.character(aligned[[col]]))),
               "integer"   = suppressWarnings(as.integer(as.character(aligned[[col]]))),
               "character" = as.character(aligned[[col]]),
               "logical"   = as.logical(aligned[[col]]),
               "double"    = suppressWarnings(as.numeric(as.character(aligned[[col]]))),
               as.character(aligned[[col]])  # fallback
        )
      }, error = function(e) {
        cat("[", label, "] Type coercion failed for column: ", col,
            " (ref: ", ref_class, " man: ", man_class, ")\n", sep = "")
        as.character(aligned[[col]])
      })
    }
  }

  cat("[", label, "] Binding ", nrow(aligned), " manual rows\n", sep = "")
  out <- bind_rows(reference, aligned)
  cat("[", label, "] Combined: ", nrow(out), " total rows\n", sep = "")
  out
}


# Median slot (HB, IVB) per pitch type x release-height bin, from the P5 pool.
build_p5_slot_grid <- function(p5, bin = 0.10) {
  req_cols <- c("TaggedPitchType","RelHeight","HorzBreak","InducedVertBreak")
  if (!all(req_cols %in% names(p5))) return(tibble())
  p5 %>%
    filter(!is.na(TaggedPitchType), !is.na(RelHeight),
           !is.na(HorzBreak), !is.na(InducedVertBreak)) %>%
    mutate(RH_bin = round(RelHeight / bin) * bin) %>%
    group_by(TaggedPitchType, RH_bin) %>%
    summarise(
      slot_hb  = median(HorzBreak, na.rm = TRUE),
      slot_ivb = median(InducedVertBreak, na.rm = TRUE),
      n = n(), .groups = "drop"
    )
}

if (palace_serve_mode()) {

  data           <- pool_read("spring25")
  spraydata      <- pool_read("spray_spring25")
  fall25         <- pool_read("fall25")
  sprayfall25    <- pool_read("spray_fall25")
  prespring      <- pool_read("prespring26")
  sprayprespring <- pool_read("spray_prespring26")
  spring26       <- pool_read("spring26")
  sprayspring26  <- pool_read("spray_spring26")

  spring26_game_choices <- if ("game_label" %in% names(spring26))
    sort(unique(spring26$game_label)) else character(0)
  cat("Spring 2026 games available:", length(spring26_game_choices), "\n")

  # Hand-tracked games (no TrackMan physics) are baked into spring26 at build
  # time; the "include manual data" toggle still needs to know they exist.
  manual_dates <- as.Date(c("2026-02-27", "2026-02-28", "2026-03-01"))
  manual_data_loaded <- all(c("Date", "ax0") %in% names(spring26)) &&
    any(as.Date(spring26$Date) %in% manual_dates & is.na(spring26$ax0), na.rm = TRUE)
  odu_data_loaded <- FALSE

  # Heavy legacy pools and everything derived from them are promises:
  # nothing is downloaded until a session actually touches them.
  delayedAssign("P5_2025",              pool_read("P5_2025"))
  delayedAssign("SBC_2025",             pool_read("SBC_2025"))
  delayedAssign("full_data_p5_compare", pool_all())
  delayedAssign("p5_maps",              artifact_read("p5_maps"))
  delayedAssign("exp_movement_grid",    artifact_read("exp_movement_grid"))
  delayedAssign("p5_slot_grid",         artifact_read("p5_slot_grid"))

  if (PALACE_PREWARM) {
    cat("[serve] PALACE_PREWARM=TRUE: forcing lazy pools at boot\n")
    invisible(list(p5_maps, exp_movement_grid, p5_slot_grid, P5_2025, SBC_2025, full_data_p5_compare))
  }

} else {

  # Lookups only process_pitcher_data() needs (heights, leagues, run environments)
  batter_info <- read_csv("reference/batter_heights.csv", show_col_types = FALSE)
  team_leagues <- read_csv("reference/team_leagues.csv", show_col_types = FALSE)
  league_run_environments <- read_csv("reference/old_league_run_environments.csv", show_col_types = FALSE)

  data <- download_private_parquet("CoastalBaseball/PitcherAppFiles", "CCUPitcher25.parquet")
  SBC_2025 <- download_private_parquet("CoastalBaseball/PitcherAppFiles", "SBC_2025.parquet")
  P5_2025 <- download_private_parquet("CoastalBaseball/PitcherAppFiles", "P5_2025.parquet")
  fall25_raw <- read_parquet("raw/fall_data_pitcher.parquet")
  prespring_raw <- read_parquet("raw/prespring_data_pitcher.parquet")
  spring26_raw <- download_master_dataset("CoastalBaseball/2026MasterDataset", "coastal_pitchers")

  manual_spring26_raw <- tryCatch({
    read_csv("raw/3_1, 2_27, 2_28 - values_times_12_set2.csv", show_col_types = FALSE)
  }, error = function(e) {
    cat("Manual spring26 CSV not found, skipping.\n")
    NULL
  })

  # Saturday vs ODU game (hand-tracked, no TrackMan)
  manual_odu_raw <- tryCatch({
    read_csv("raw/Sat_Vs_ODU_Sheet1_.csv", show_col_types = FALSE)
  }, error = function(e) {
    cat("Sat vs ODU CSV not found, skipping.\n")
    NULL
  })

  spring26_raw <- bind_manual_rows(spring26_raw, manual_spring26_raw, "Manual Data")
  spring26_raw <- bind_manual_rows(spring26_raw, manual_odu_raw, "ODU Data")

  # Manual data status flags (used by UI badge)
  manual_data_loaded <- exists("manual_spring26_raw") && 
    !is.null(manual_spring26_raw) && 
    is.data.frame(manual_spring26_raw) && 
    nrow(manual_spring26_raw) > 0

  if (manual_data_loaded) {
    manual_data_n <- nrow(manual_spring26_raw)
    manual_data_pitchers <- length(unique(manual_spring26_raw$Pitcher))
    manual_data_dates <- paste(sort(unique(manual_spring26_raw$Date)), collapse = ", ")
    cat("[Manual Data] Flags set: loaded =", manual_data_loaded, 
        "| n =", manual_data_n, 
        "| pitchers =", manual_data_pitchers, "\n")
  } else {
    manual_data_n <- 0
    manual_data_pitchers <- 0
    manual_data_dates <- ""
  }

  # ODU game status flags
  odu_data_loaded <- exists("manual_odu_raw") && 
    !is.null(manual_odu_raw) && 
    is.data.frame(manual_odu_raw) && 
    nrow(manual_odu_raw) > 0

  if (odu_data_loaded) {
    odu_data_n <- nrow(manual_odu_raw)
    odu_data_pitchers <- length(unique(manual_odu_raw$Pitcher))
    cat("[ODU Data] Flags set: loaded =", odu_data_loaded,
        "| n =", odu_data_n,
        "| pitchers =", odu_data_pitchers, "\n")
  } else {
    odu_data_n <- 0
    odu_data_pitchers <- 0
  }

  # Early non-destructive name normalize
  data <- data %>%
    dplyr::mutate(
      Pitcher = ifelse(grepl(",", Pitcher), normalize_lastfirst(Pitcher), Pitcher),
      Batter  = ifelse(grepl(",", Batter),  normalize_lastfirst(Batter),  Batter)
    )

  data <- data %>%
    dplyr::mutate(Pitcher = stringr::str_replace_all(Pitcher, "(\\w+), (\\w+)", "\\2 \\1"))

  data <- data %>% dplyr::distinct()

  # Safer dedupe (handles mostly-NA PitchUID)
  if ("PitchUID" %in% names(data) && any(!is.na(data$PitchUID))) {
    data <- data %>% dplyr::distinct(PitchUID, .keep_all = TRUE)
  } else {
    data <- data %>% dplyr::distinct(
      GameID, PitcherId, Inning, PAofInning, PitchofPA, PitchNo,
      .keep_all = TRUE
    )
  }

  # Process Spring 2025 data
  spring_processed <- process_pitcher_data(data, bio_heights, "data_ind")
  data <- spring_processed$data
  spraydata <- spring_processed$spray

  # Process Fall 2025 data
  fall_processed <- process_pitcher_data(fall25_raw, bio_heights, "fall_ind")
  fall25 <- add_roster_flag(fall_processed$data)
  sprayfall25 <- fall_processed$spray

  # Process Pre-Spring 2026 data
  prespring_processed <- process_pitcher_data(prespring_raw, bio_heights, "prespring_ind")
  prespring <- add_roster_flag(prespring_processed$data)
  sprayprespring <- prespring_processed$spray

  # Process Spring 2026 data
  spring26_processed <- process_pitcher_data(spring26_raw, bio_heights, "spring26_ind")
  spring26 <- add_roster_flag(spring26_processed$data)
  sprayspring26 <- spring26_processed$spray

  # Process SBC 2025
  sbc_processed <- process_pitcher_data(SBC_2025, bio_heights, "SBC_ind")
  SBC_2025 <- add_roster_flag(sbc_processed$data)

  # Process P5 2025
  p5_processed <- process_pitcher_data(P5_2025, bio_heights, "P5_ind")
  P5_2025 <- add_roster_flag(p5_processed$data)


  p5_ref <- P5_2025

  # ---- normalize expected columns for build_p5_ecdf_maps ----
  if (!"Pitcher" %in% names(p5_ref)) {
    if ("PitcherName" %in% names(p5_ref)) {
      p5_ref <- dplyr::rename(p5_ref, Pitcher = PitcherName)
    } else if ("PitcherId" %in% names(p5_ref)) {
      p5_ref <- dplyr::mutate(p5_ref, Pitcher = as.character(PitcherId))
    } else {
      p5_ref <- dplyr::mutate(p5_ref, Pitcher = NA_character_)
    }
  }

  cat("fall25 columns before bind_rows:", names(fall25), "\n")
  cat("fall_ind in fall25:", "fall_ind" %in% names(fall25), "\n")
  cat("fall25 rows:", nrow(fall25), "\n")
  cat("spring26 rows before bind_rows:", nrow(spring26), "\n")

  full_data_p5_compare <- bind_rows(
    harmonize_types(list(data, fall25, prespring, spring26, P5_2025, SBC_2025))
  )
  # stuff_plus / pitching_plus / location_plus are attached per pitch by
  # score_promodel() inside process_pitcher_data(), scaled against the FIXED
  # Pitch Profiler college reference (100 = average college pitcher-season,
  # 10 pts = 1 SD). No pooled re-scaling: Coastal, P5, SBC and on-demand
  # NCAA frames all sit on one absolute scale, stable across restarts.
  # (The old plus_calib pooled-constant machinery is gone with it.)

  # ============================================================
  # FILTER BACK INTO INDIVIDUAL DATASETS WITH STANDARDIZED PLUS SCORES
  # ============================================================
  # All datasets now get the same /12 treatment to convert inches back to feet

  data <- full_data_p5_compare %>%
    filter(data_ind == 1) %>%
    mutate(Date = as.Date(Date),
           UTCDate = as.Date(UTCDate),
           PlateLocHeight = PlateLocHeight/12,
           PlateLocSide = PlateLocSide/12)

  fall25 <- full_data_p5_compare %>% 
    filter(fall_ind == 1) %>%
    mutate(Date = as.Date(Date),
           UTCDate = as.Date(UTCDate),
           PlateLocHeight = PlateLocHeight/12,
           PlateLocSide = PlateLocSide/12)

  if (nrow(fall25) == 0) {
    cat("WARNING: fall25 has 0 rows after filtering!\n")
  }

  prespring <- full_data_p5_compare %>% 
    filter(prespring_ind == 1) %>%
    mutate(Date = as.Date(Date),
           UTCDate = as.Date(UTCDate),
           PlateLocHeight = PlateLocHeight/12,
           PlateLocSide = PlateLocSide/12)

  if (nrow(prespring) == 0) {
    cat("WARNING: prespring has 0 rows after filtering!\n")
  }

  spring26 <- full_data_p5_compare %>% 
    filter(spring26_ind == 1) %>%
    mutate(Date = as.Date(Date),
           UTCDate = as.Date(UTCDate),
           PlateLocHeight = PlateLocHeight/12,
           PlateLocSide = PlateLocSide/12)

  if (nrow(spring26) == 0) {
    cat("WARNING: spring26 has 0 rows after filtering!\n")
  }

  if (nrow(spring26) > 0) {
    spring26 <- spring26 %>%
      mutate(
        opponent_code = ifelse(!is.na(BatterTeam), BatterTeam,
                               ifelse(PitcherTeam == HomeTeam, AwayTeam, "Unknown")),
        opponent_name = prettify_team(opponent_code),
        game_label = paste0(as.character(Date), " vs ", opponent_name)
      )

    if (exists("sprayspring26") && is.data.frame(sprayspring26) && nrow(sprayspring26) > 0) {
      sprayspring26 <- sprayspring26 %>%
        mutate(
          opponent_code = ifelse(!is.na(BatterTeam), BatterTeam,
                                 ifelse(PitcherTeam == HomeTeam, AwayTeam, "Unknown")),
          opponent_name = prettify_team(opponent_code),
          game_label = paste0(as.character(Date), " vs ", opponent_name)
        )
    }

    spring26_game_choices <- sort(unique(spring26$game_label))
    cat("Spring 2026 games available:", length(spring26_game_choices), "\n")
  } else {
    spring26_game_choices <- character(0)
  }


  P5_2025 <- full_data_p5_compare %>%
    filter(P5_ind == 1) %>%
    mutate(Date = as.Date(Date),
           UTCDate = as.Date(UTCDate),
           PlateLocHeight = PlateLocHeight/12,
           PlateLocSide = PlateLocSide/12)

  SBC_2025 <- full_data_p5_compare %>%
    filter(SBC_ind == 1) %>%
    mutate(Date = as.Date(Date),
           UTCDate = as.Date(UTCDate),
           PlateLocHeight = PlateLocHeight/12,
           PlateLocSide = PlateLocSide/12)

  p5_maps <- build_p5_ecdf_maps(p5_ref)

  exp_movement_grid <- build_expected_movement_grid(P5_2025, bin_size = 0.10)

  # Slot grid (was rebuilt inside server() on every session).
  p5_slot_grid <- build_p5_slot_grid(P5_2025, bin = 0.10)

}
