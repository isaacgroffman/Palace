# ===============  NCAA SCOUTING REPORT MODULE  ==============
# Ported from the Advance Pitcher app (app 37). Powers the
# Scouting tab + full PDF scouting reports for NCAA pitchers.
# ============================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# ggimage is loaded at the top of this app; keep the advance app's
# feature flag so the ported plot code works unchanged.
has_ggimage <- requireNamespace("ggimage", quietly = TRUE)


# Calculate ray end point for arm angle visualization
ray_end_for_type <- function(mean_rel_side, mean_rel_height, length_units, shoulder_y) {
  dx <- abs(mean_rel_side)
  dy <- max(mean_rel_height - shoulder_y, 0)
  mag <- sqrt(dx^2 + dy^2)
  if (!is.finite(mag) || mag == 0) return(c(0, 0))
  ux <- dx / mag
  uy <- dy / mag
  x_sign <- ifelse(mean_rel_side < 0, -1, 1)
  c(x_sign * ux * length_units, uy * length_units)
}
# ============================================================================
# VISUALIZATION FUNCTIONS
# ============================================================================

# 1. ARM SLOT PLOT - Compact with pitcher figure and arm angle rays
create_pitcher_arm_angle_plot <- function(data, pitcher_name) {
  if (is.null(data)) {
    return(ggplot() + theme_void() + 
             annotate("text", x = 0.5, y = 0.5, label = "No data available"))
  }
  
  pitcher_data <- data %>%
    filter(Pitcher == pitcher_name, 
           !is.na(RelSide), !is.na(RelHeight),
           !is.na(TaggedPitchType), TaggedPitchType != "Other")
  
  if (nrow(pitcher_data) < 10) {
    return(ggplot() + theme_void() + 
             annotate("text", x = 0.5, y = 0.5, label = "Insufficient data"))
  }
  
  # Detect handedness
  pitcher_hand <- pitcher_data$PitcherThrows[1]
  is_lefty <- !is.na(pitcher_hand) && pitcher_hand == "Left"
  
  # Get overall arm angle
  overall_arm_angle <- mean(pitcher_data$arm_angle_savant, na.rm = TRUE)
  if (is.na(overall_arm_angle)) overall_arm_angle <- 45
  
  # Categorize arm slot type - added High 3/4
  arm_slot_category <- if (overall_arm_angle < 10) "Sidearm"
  else if (overall_arm_angle < 40) "Low 3/4"
  else if (overall_arm_angle < 47) "3/4"
  else if (overall_arm_angle < 60) "High 3/4"
  else "Overhand"
  
  # Calculate averages per pitch type for arm angle rays
  avg <- pitcher_data %>%
    filter(!is.na(arm_angle_savant)) %>%
    group_by(TaggedPitchType) %>%
    summarise(
      RelSide = mean(RelSide, na.rm = TRUE),
      RelHeight = mean(RelHeight, na.rm = TRUE),
      arm_angle = mean(arm_angle_savant, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    filter(n >= 5)
  
  if (nrow(avg) == 0) {
    return(ggplot() + theme_void() + 
             annotate("text", x = 0.5, y = 0.5, label = "No arm angle data"))
  }
  
  # Get pitcher image URL
  image_url <- get_pitcher_image_url(overall_arm_angle, is_lefty)
  
  # Figure positioning depends on arm angle type
  # Different figures have different poses - need to adjust x and y for each
  # Key: figures need to be LOW enough to touch the mound
  
  if (overall_arm_angle >= 47) {
    # Overhand / High 3/4 - upright figure, shift for handedness
    img_center_x <- if (is_lefty) -0.35 else 0.35
    img_center_y <- 2.20
    shoulder_y_fixed <- 4.0
    fig_size <- 0.285
  } else if (overall_arm_angle >= 40) {
    # 3/4 - slightly more shift than overhand
    img_center_x <- if (is_lefty) -0.40 else 0.40
    img_center_y <- 2.20
    shoulder_y_fixed <- 4.0
    fig_size <- 0.285
  } else if (overall_arm_angle >= 10) {
    # Low 3/4 - significant adjustment for proper alignment
    img_center_x <- if (is_lefty) -1.07 else 1.07  # More shifted for better alignment
    img_center_y <- 2.55  # Higher up
    shoulder_y_fixed <- 4.0
    fig_size <- 0.264  # Slightly taller
  } else {
    # Sidearm - dramatic lean
    img_center_x <- if (is_lefty) -0.9 else 0.9
    img_center_y <- 3.0
    shoulder_y_fixed <- 4.0
    fig_size <- 0.34
  }
  
  # Create image data frame
  img_df <- data.frame(x = img_center_x, y = img_center_y, image = image_url)
  
  
  seg <- avg %>%
    mutate(
      x_rel = if (is_lefty) abs(RelSide) else -abs(RelSide),
      y_rel = RelHeight,
      xs = 0,
      ys = shoulder_y_fixed,
      xe = x_rel,
      ye = y_rel
    )
  
  p <- ggplot(seg) +
    # Mound/dirt rectangle - extend wider for ±4.5ft
    annotate("rect", xmin = -4.5, xmax = 4.5, ymin = 0, ymax = 0.6,
             fill = "#8B4513", alpha = 0.7) +
    # Teal background behind pitcher
    annotate("rect", xmin = -4.5, xmax = 4.5, ymin = 0.6, ymax = 4.3,
             fill = "#006F71", alpha = 0.9) +
    # Yellow stripe at top of background
    annotate("rect", xmin = -4.5, xmax = 4.5, ymin = 4.3, ymax = 4.5,
             fill = "yellow", alpha = 0.9) +
    # Rubber
    annotate("rect", xmin = -0.5, xmax = 0.5, ymin = 0.5, ymax = 0.65,
             fill = "white", color = "black", linewidth = 0.5)
  
  # Add pitcher figure - use ggimage if available, otherwise draw simple shape
  if (has_ggimage) {
    tryCatch({
      p <- p + ggimage::geom_image(
        data = img_df,
        aes(x = x, y = y, image = image),
        size = fig_size
      )
    }, error = function(e) {
      message("Could not load pitcher image: ", e$message)
      # Fallback: draw simple pitcher silhouette
      p <- p + 
        annotate("point", x = img_center_x, y = img_center_y + 1.2, size = 8, color = "gray30") +  # Head
        annotate("segment", x = img_center_x, xend = img_center_x, 
                 y = img_center_y + 0.9, yend = img_center_y - 0.3, 
                 linewidth = 3, color = "gray30")  # Body
    })
  } else {
    # Draw simple pitcher silhouette without ggimage
    p <- p + 
      annotate("point", x = img_center_x, y = img_center_y + 1.2, size = 8, color = "gray30") +
      annotate("segment", x = img_center_x, xend = img_center_x, 
               y = img_center_y + 0.9, yend = img_center_y - 0.3, 
               linewidth = 3, color = "gray30")
  }
  
  # Add shoulder point (small gray dot at fixed shoulder position)
  p <- p + annotate("point", x = 0, y = shoulder_y_fixed, color = "gray40", size = 2, shape = 16)
  
  # Add arm angle rays - THIN LINES colored by pitch type
  if (nrow(seg) > 0) {
    p <- p +
      geom_segment(
        data = seg,
        aes(x = xs, y = ys, xend = xe, yend = ye, color = TaggedPitchType),
        linewidth = 1.5, alpha = 0.9
      ) +
      # Release points - small dots with black outline
      geom_point(
        data = seg,
        aes(x = xe, y = ye, fill = TaggedPitchType),
        shape = 21, size = 3, stroke = 0.5, color = "black", alpha = 0.9
      )
  }
  
  p <- p +
    # Labels
    annotate("text", x = -3.5, y = 6.8, label = "3B", size = 2.5, fontface = "bold") +
    annotate("text", x = 3.5, y = 6.8, label = "1B", size = 2.5, fontface = "bold") +
    # Arm angle label at bottom
    annotate("label", x = 0, y = 0.15,
             label = paste0(round(overall_arm_angle, 1), "° - ", arm_slot_category),
             fill = "#e65100", color = "white", fontface = "bold", size = 2.5,
             label.padding = unit(0.15, "lines")) +
    # Scales - extended to ±4.5 for wider release points
    xlim(-4.5, 4.5) + ylim(0, 7) +
    scale_color_manual(values = pitch_colors, na.value = "gray50") +
    scale_fill_manual(values = pitch_colors, na.value = "gray50") +
    labs(title = ("Arm Angles"),
         x = "Horizontal (ft)", y = "Vertical (ft)") +
    theme_void() +
    theme(
      plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
      axis.text = element_text(size = 6),
      axis.title = element_text(size = 7),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "none",
      plot.margin = margin(2, 2, 2, 2)
    )
  
  return(p)
}



# 4. COUNT USAGE PIE CHARTS
create_count_usage_pies <- function(data, pitcher_name) {
  if (is.null(data)) {
    return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "No data"))
  }
  
  df <- data %>%
    filter(Pitcher == pitcher_name, TaggedPitchType != "Other") %>%
    group_by(TaggedPitchType) %>%
    mutate(n = 100 * n() / nrow(data %>% filter(Pitcher == pitcher_name))) %>%
    filter(n >= 2) %>%
    ungroup()
  
  if (nrow(df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No count data\navailable for this pitcher", size = 4) +
        theme_void()
    )
  }
  
  plot_df <- df %>%
    mutate(
      count = case_when(
        Balls == 0 & Strikes == 0 ~ "1P",
        Strikes == 2 ~ "2K",
        Balls == 3 & Strikes == 2 ~ "Full",
        Balls > Strikes ~ "Behind",
        TRUE ~ "Even"
      ),
      BatterSide = ifelse(BatterSide == "Right", "Vs Right", "Vs Left")
    ) %>%
    filter(count %in% c("1P", "Even", "Behind", "2K", "Full")) %>%
    group_by(count, BatterSide) %>%
    mutate(total_count = n()) %>%
    group_by(count, TaggedPitchType, BatterSide) %>%
    summarise(n = n(), total = first(total_count), .groups = "drop") %>%
    mutate(
      percentage = n / total * 100,
      pct_label = ifelse(percentage >= 5, scales::percent(percentage / 100, accuracy = 1), "")
    ) %>%
    arrange(desc(percentage))
  
  # Set factor order for counts
  plot_df$count <- factor(plot_df$count, levels = c("1P", "Even", "Behind", "2K", "Full"))
  
  p <- ggplot(plot_df, aes(x = "", y = percentage, fill = TaggedPitchType)) +
    geom_bar(width = 1, stat = "identity", color = "white", linewidth = 0.3) +
    geom_text(aes(label = pct_label),
              position = position_stack(vjust = 0.5),
              size = 2.5, fontface = "bold") +
    coord_polar("y", start = 0) +
    facet_grid(BatterSide ~ count, labeller = label_value) +
    scale_fill_manual(values = pitch_colors, drop = FALSE) +
    labs(title = ("Pitch Usage by Count"), fill = "Pitch Type") +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
      strip.text = element_text(size = 9, face = "bold"),
      legend.position = "none",  # Remove legend to give more room for notes
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  return(p)
}


# SEC Benchmarks for coloring
sec_benchmarks <- list(
  era = 4.75,
  fip = 4.22,
  whip = 1.37,
  k_pct = 26,
  bb_pct = 10,
  ba_against = 0.244,
  slg_against = 0.396,
  fb_strike = 64,
  bb_strike = 60,
  os_strike = 59,
  zone_pct = 45.9,
  whiff_pct = 28.9,
  strike_pct = 62.6
)

# Color function for stats (green = good, red = bad)
get_stat_color <- function(value, benchmark, higher_is_better = TRUE, range_pct = 0.25) {
  if (is.na(value) || is.na(benchmark)) return("#FFFFFF")
  if (is.nan(value) || is.infinite(value)) return("#FFFFFF")
  
  pal <- scales::gradient_n_pal(c("#E1463E", "white", "#00840D"))
  range_val <- benchmark * range_pct
  min_val <- benchmark - range_val
  max_val <- benchmark + range_val
  
  if (higher_is_better) {
    normalized <- (value - min_val) / (max_val - min_val)
  } else {
    normalized <- (max_val - value) / (max_val - min_val)
  }
  normalized <- pmax(0, pmin(1, normalized))
  pal(normalized)
}

# Process pitcher indicators for advanced stats
process_pitcher_indicators <- function(df) {
  df %>%
    mutate(
      OutsOnPlay = case_when(
        PlayResult == "Out" ~ 1,
        PlayResult == "FieldersChoice" ~ 1,
        PlayResult == "Sacrifice" ~ 1,
        PlayResult == "SacrificeFly" ~ 1,
        KorBB == "Strikeout" ~ 1,
        grepl("DoublePlay", PlayResult, ignore.case = TRUE) ~ 2,
        TRUE ~ 0
      ),
      # Note: is_hit and is_ab already exist in the dataset
      total_bases = case_when(
        PlayResult == "Single" ~ 1,
        PlayResult == "Double" ~ 2,
        PlayResult == "Triple" ~ 3,
        PlayResult == "HomeRun" ~ 4,
        TRUE ~ 0
      ),
      is_k = as.integer(KorBB == "Strikeout"),
      is_walk = as.integer(KorBB == "Walk"),
      is_hbp = as.integer(PitchCall == "HitByPitch"),
      is_put_away = as.integer(KorBB == "Strikeout" & Strikes == 2),
      in_zone = as.integer(PlateLocSide >= -0.83 & PlateLocSide <= 0.83 &
                             PlateLocHeight >= 1.5 & PlateLocHeight <= 3.5),
      is_swing = as.integer(PitchCall %in% c("StrikeSwinging", "FoulBall", 
                                             "FoulBallNotFieldable", "FoulBallFieldable", "InPlay")),
      is_whiff = as.integer(PitchCall == "StrikeSwinging"),
      is_strike = as.integer(!PitchCall %in% c("BallCalled", "BallinDirt", "BallIntentional")),
      pitch_family = case_when(
        TaggedPitchType %in% c("Fastball", "Four-Seam", "FourSeamFastBall", "Sinker", "TwoSeamFastBall", "Two-Seam") ~ "FB",
        TaggedPitchType %in% c("Slider", "Curveball", "Cutter", "Sweeper", "Slurve", "Knuckle Curve") ~ "BB",
        TaggedPitchType %in% c("ChangeUp", "Changeup", "Splitter") ~ "OS",
        TRUE ~ "Other"
      )
    )
}

# Calculate comprehensive pitcher stats
calculate_advanced_pitcher_stats <- function(data, pitcher_name) {
  df <- data %>% 
    filter(Pitcher == pitcher_name) %>%
    process_pitcher_indicators()
  
  if (nrow(df) < 20) return(NULL)
  
  # Get PA-level data
  pa_data <- df %>%
    filter(!is.na(KorBB) | PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", 
                                             "FieldersChoice", "Error") | PitchCall == "HitByPitch") %>%
    group_by(GameID, Inning, PAofInning) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  # Basic counting stats - FIXED IP calculation
  total_outs <- sum(df$OutsOnPlay, na.rm = TRUE)
  full_innings <- floor(total_outs / 3)
  partial_outs <- total_outs %% 3
  # IP format: X.0, X.1, or X.2 only (not X.3+)
  ip_display <- full_innings + partial_outs / 10
  # Actual innings for rate calculations
  ip_actual <- total_outs / 3
  
  n_games <- n_distinct(df$GameID)
  n_pitches <- nrow(df)
  
  # Most recent outing
  recent_game <- df %>% 
    filter(GameID == max(GameID, na.rm = TRUE))
  recent_pitches <- nrow(recent_game)
  
  # Get last 3 outings with dates and pitch counts
  game_summary <- df %>%
    group_by(GameID, Date) %>%
    summarise(pitches = n(), .groups = "drop") %>%
    arrange(desc(Date)) %>%
    head(3)
  
  last_3_outings <- if (nrow(game_summary) > 0) {
    paste(sapply(1:nrow(game_summary), function(i) {
      date_val <- game_summary$Date[i]
      date_str <- tryCatch({
        if (inherits(date_val, "Date") || inherits(date_val, "POSIXt")) {
          format(date_val, "%m/%d")
        } else if (is.character(date_val) || is.numeric(date_val)) {
          format(as.Date(date_val), "%m/%d")
        } else {
          "?"
        }
      }, error = function(e) "?")
      paste0(game_summary$pitches[i], " (", date_str, ")")
    }), collapse = ", ")
  } else {
    "-"
  }
  
  # PA-based stats
  n_pa <- nrow(pa_data)
  n_k <- sum(pa_data$is_k, na.rm = TRUE)
  n_bb <- sum(pa_data$is_walk, na.rm = TRUE)
  n_hbp <- sum(pa_data$is_hbp, na.rm = TRUE)
  n_hr <- sum(pa_data$PlayResult == "HomeRun", na.rm = TRUE)
  
  # Runs (from RunsScored column if available)
  n_runs <- if ("RunsScored" %in% names(df)) sum(df$RunsScored, na.rm = TRUE) else n_hr
  
  # Split stats - calculate BA and SLG from PlayResult
  vs_lhh <- pa_data %>% filter(BatterSide == "Left")
  vs_rhh <- pa_data %>% filter(BatterSide == "Right")
  
  # Calculate hits and ABs from PlayResult (don't rely on pre-existing columns)
  calculate_ba_slg <- function(pa_df) {
    if (nrow(pa_df) == 0) return(list(ba = NA, slg = NA))
    
    # Hits: Single, Double, Triple, HomeRun
    hits <- sum(pa_df$PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), na.rm = TRUE)
    
    # At-bats: exclude walks and HBP
    ab <- sum(pa_df$PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out", "FieldersChoice", "Error") |
                pa_df$KorBB == "Strikeout", na.rm = TRUE)
    
    # Total bases
    tb <- sum(pa_df$PlayResult == "Single", na.rm = TRUE) +
      2 * sum(pa_df$PlayResult == "Double", na.rm = TRUE) +
      3 * sum(pa_df$PlayResult == "Triple", na.rm = TRUE) +
      4 * sum(pa_df$PlayResult == "HomeRun", na.rm = TRUE)
    
    ba <- if (ab > 0) hits / ab else NA
    slg <- if (ab > 0) tb / ab else NA
    
    list(ba = ba, slg = slg, hits = hits, ab = ab, tb = tb)
  }
  
  lhh_stats <- calculate_ba_slg(vs_lhh)
  rhh_stats <- calculate_ba_slg(vs_rhh)
  
  ba_vs_l <- lhh_stats$ba
  ba_vs_r <- rhh_stats$ba
  slg_vs_l <- lhh_stats$slg
  slg_vs_r <- rhh_stats$slg
  
  # Total hits for WHIP
  all_stats <- calculate_ba_slg(pa_data)
  n_hits <- all_stats$hits
  
  # Pitch-level stats
  n_swings <- sum(df$is_swing, na.rm = TRUE)
  n_whiffs <- sum(df$is_whiff, na.rm = TRUE)
  n_strikes <- sum(df$is_strike, na.rm = TRUE)
  n_in_zone <- sum(df$in_zone, na.rm = TRUE)
  
  # By pitch family
  fb_data <- df %>% filter(pitch_family == "FB")
  bb_data <- df %>% filter(pitch_family == "BB")
  os_data <- df %>% filter(pitch_family == "OS")
  
  fb_strike_pct <- if (nrow(fb_data) > 0) 100 * sum(fb_data$is_strike, na.rm = TRUE) / nrow(fb_data) else NA
  bb_strike_pct <- if (nrow(bb_data) > 0) 100 * sum(bb_data$is_strike, na.rm = TRUE) / nrow(bb_data) else NA
  os_strike_pct <- if (nrow(os_data) > 0) 100 * sum(os_data$is_strike, na.rm = TRUE) / nrow(os_data) else NA
  
  # Calculate rates
  k_pct <- if (n_pa > 0) 100 * n_k / n_pa else 0
  bb_pct <- if (n_pa > 0) 100 * n_bb / n_pa else 0
  whiff_pct <- if (n_swings > 0) 100 * n_whiffs / n_swings else 0
  zone_pct <- if (n_pitches > 0) 100 * n_in_zone / n_pitches else 0
  strike_pct <- if (n_pitches > 0) 100 * n_strikes / n_pitches else 0
  
  # RA9 (not ERA - can't determine earned runs from TrackMan), WHIP, FIP
  ra9 <- if (ip_actual > 0) 9 * n_runs / ip_actual else NA
  whip <- if (ip_actual > 0) (n_bb + n_hits) / ip_actual else NA
  
  # FIP with correct constant (4.08) and formula
  fip_constant <- 4.08
  fip <- if (ip_actual > 0) ((13 * n_hr + 3 * (n_bb + n_hbp) - 2 * n_k) / ip_actual) + fip_constant else NA
  
  # Avg IP per outing (using display format)
  avg_outs_per_game <- if (n_games > 0) total_outs / n_games else 0
  avg_ip_full <- floor(avg_outs_per_game / 3)
  avg_ip_partial <- round(avg_outs_per_game %% 3) / 10
  avg_ip_display <- avg_ip_full + avg_ip_partial
  
  avg_pitches <- if (n_games > 0) n_pitches / n_games else 0
  
  list(
    ip = ip_display,
    n_games = n_games,
    avg_ip = avg_ip_display,
    avg_pitches = round(avg_pitches, 0),
    recent_pitches = recent_pitches,
    last_3_outings = last_3_outings,
    ra9 = round(ra9, 2),
    whip = round(whip, 2),
    fip = round(fip, 2),
    k_pct = round(k_pct, 1),
    bb_pct = round(bb_pct, 1),
    ba_vs_l = round(ba_vs_l, 3),
    ba_vs_r = round(ba_vs_r, 3),
    slg_vs_l = round(slg_vs_l, 3),
    slg_vs_r = round(slg_vs_r, 3),
    whiff_pct = round(whiff_pct, 1),
    zone_pct = round(zone_pct, 1),
    strike_pct = round(strike_pct, 1),
    fb_strike_pct = round(fb_strike_pct, 1),
    bb_strike_pct = round(bb_strike_pct, 1),
    os_strike_pct = round(os_strike_pct, 1),
    pitcher_hand = df$PitcherThrows[1]
  )
}

# Create sequencing matrix
create_sequencing_matrix <- function(data, pitcher_name, batter_side = NULL) {
  df <- data %>%
    filter(Pitcher == pitcher_name, !is.na(TaggedPitchType), TaggedPitchType != "Other")
  
  if (!is.null(batter_side)) {
    df <- df %>% filter(BatterSide == batter_side)
  }
  
  if (nrow(df) < 50) {
    return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "Low n"))
  }
  
  # Create previous pitch column
  df <- df %>%
    arrange(GameID, Inning, PAofInning, PitchofPA) %>%
    group_by(GameID, Inning, PAofInning) %>%
    mutate(prev_pitch = lag(TaggedPitchType)) %>%
    ungroup() %>%
    filter(!is.na(prev_pitch))
  
  # Calculate transition matrix
  trans_matrix <- df %>%
    count(prev_pitch, TaggedPitchType) %>%
    group_by(prev_pitch) %>%
    mutate(pct = round(100 * n / sum(n), 0)) %>%
    ungroup()
  
  title_suffix <- if (!is.null(batter_side)) paste0(" vs ", batter_side, "HH") else ""
  
  ggplot(trans_matrix, aes(x = TaggedPitchType, y = prev_pitch, fill = pct)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(pct, "%")), size = 2.5) +
    scale_fill_gradient(low = "white", high = "#2c7bb6", name = "%") +
    labs(title = paste0("Pitch Sequencing", title_suffix),
         x = "Current Pitch", y = "Previous Pitch") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y = element_text(size = 7),
      legend.position = "none"
    )
}


# Create spray chart for hits against - SECTOR VERSION
create_hits_spray_chart <- function(data, pitcher_name, batter_side) {
  df <- data %>%
    filter(Pitcher == pitcher_name,
           BatterSide == batter_side,
           PitchCall == "InPlay",
           !is.na(Bearing), !is.na(Distance)) %>%
    mutate(
      Bearing2 = Bearing * pi / 180,
      x = Distance * sin(Bearing2),
      y = Distance * cos(Bearing2)
    )
  
  if (nrow(df) < 5) {
    return(ggplot() + theme_void() + 
             annotate("text", x = 0, y = 200, label = paste0("Low n vs ", batter_side, "HH"), size = 3))
  }
  
  # 5 angle sectors from -45 to +45
  angle_breaks_deg <- seq(-45, 45, length.out = 6)
  angle_breaks_rad <- angle_breaks_deg * pi / 180
  
  df <- df %>%
    mutate(
      angle_sector = case_when(
        Bearing >= angle_breaks_deg[1] & Bearing < angle_breaks_deg[2] ~ 1L,
        Bearing >= angle_breaks_deg[2] & Bearing < angle_breaks_deg[3] ~ 2L,
        Bearing >= angle_breaks_deg[3] & Bearing < angle_breaks_deg[4] ~ 3L,
        Bearing >= angle_breaks_deg[4] & Bearing < angle_breaks_deg[5] ~ 4L,
        Bearing >= angle_breaks_deg[5] & Bearing <= angle_breaks_deg[6] ~ 5L,
        TRUE ~ NA_integer_
      ),
      dist_sector = case_when(
        Distance < 150 ~ 1L,
        Distance >= 150 ~ 2L,
        TRUE ~ NA_integer_
      )
    ) %>%
    filter(!is.na(angle_sector), !is.na(dist_sector))
  
  if (nrow(df) == 0) {
    return(ggplot() + theme_void() + 
             annotate("text", x = 0, y = 200, label = paste0("No BIP vs ", batter_side, "HH"), size = 3))
  }
  
  total_bip <- nrow(df)
  
  # Sector stats
  sector_stats <- df %>%
    group_by(angle_sector, dist_sector) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(pct = round(100 * count / total_bip))
  
  # Create sector polygons
  create_sector <- function(r_inner, r_outer, theta_start, theta_end, n = 40) {
    theta_seq <- seq(theta_start, theta_end, length.out = n)
    inner_x <- r_inner * sin(theta_seq)
    inner_y <- r_inner * cos(theta_seq)
    outer_x <- r_outer * sin(rev(theta_seq))
    outer_y <- r_outer * cos(rev(theta_seq))
    data.frame(x = c(inner_x, outer_x), y = c(inner_y, outer_y))
  }
  
  dist_breaks <- c(0, 150, 400)
  
  sectors <- list()
  sector_id <- 1
  for (i in 1:5) {
    for (j in 1:2) {
      poly <- create_sector(
        dist_breaks[j], dist_breaks[j + 1],
        angle_breaks_rad[i], angle_breaks_rad[i + 1]
      )
      poly$sector_id <- sector_id
      poly$angle_idx <- i
      poly$dist_idx <- j
      sectors[[sector_id]] <- poly
      sector_id <- sector_id + 1
    }
  }
  
  sector_df <- bind_rows(sectors)
  
  # Sector centers & merge stats
  sector_centers <- sector_df %>%
    group_by(sector_id, angle_idx, dist_idx) %>%
    summarise(cx = mean(x), cy = mean(y), .groups = "drop") %>%
    left_join(
      sector_stats %>% rename(angle_idx = angle_sector, dist_idx = dist_sector),
      by = c("angle_idx", "dist_idx")
    )
  
  sector_df <- sector_df %>%
    left_join(sector_centers %>% select(sector_id, pct, count), by = "sector_id")
  
  # Plot
  ggplot() +
    geom_polygon(
      data = sector_df,
      aes(x = x, y = y, group = sector_id, fill = pct),
      color = "black", linewidth = 0.3
    ) +
    geom_text(
      data = sector_centers %>% filter(!is.na(pct) & pct > 0),
      aes(x = cx, y = cy, label = paste0(pct, "%")),
      size = 2.5, fontface = "bold"
    ) +
    # Foul lines
    annotate("segment", x = 0, y = 0, xend = 247.487, yend = 247.487, color = "black", linewidth = 0.5) +
    annotate("segment", x = 0, y = 0, xend = -247.487, yend = 247.487, color = "black", linewidth = 0.5) +
    annotate("text", x = 200, y = 350, label = paste("BIP:", total_bip), size = 2.5, hjust = 0, fontface = "bold") +
    scale_fill_gradientn(
      colors = c("white", "#e5fbe5", "#90EE90", "#66c266", "#228B22", "#145a14", "#006400"),
      values = scales::rescale(c(0, 3, 5, 10, 15, 20, 25)),
      limits = c(0, NA),
      na.value = "white",
      name = "% BIP"
    ) +
    coord_fixed(xlim = c(-280, 280), ylim = c(-20, 380)) +
    labs(title = paste0("vs ", batter_side, "HH")) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 8, face = "bold")
    )
}


create_pitch_group_heatmap <- function(data, pitcher_name, pitch_group, metric_type, batter_side = NULL) {
  
  # ---- pitch groups ----
  fb_pitches <- c("Fastball", "Sinker", "FourSeamFastBall", "TwoSeamFastBall", "Four-Seam", "Two-Seam")
  bb_pitches <- c("Slider", "Curveball", "Cutter", "Sweeper", "Slurve", "Knuckle Curve")
  os_pitches <- c("ChangeUp", "Changeup", "Splitter")
  all_pitches <- c(fb_pitches, bb_pitches, os_pitches)
  
  selected_pitches <- switch(
    pitch_group,
    "FB"  = fb_pitches,
    "BB"  = bb_pitches,
    "OS"  = os_pitches,
    "All" = all_pitches,
    fb_pitches
  )
  
  df <- data %>%
    dplyr::filter(
      Pitcher == pitcher_name,
      TaggedPitchType %in% selected_pitches,
      !is.na(PlateLocSide), !is.na(PlateLocHeight)
    )
  
  if (!is.null(batter_side) && batter_side != "All") {
    df <- df %>% dplyr::filter(BatterSide == batter_side)
  }
  
  # ---- filter by metric ----
  if (metric_type %in% c("Whiff", "Whf")) {
    df <- df %>% dplyr::filter(PitchCall == "StrikeSwinging")
  } else if (metric_type %in% c("Damage", "Dmg")) {
    df <- df %>% dplyr::filter(!is.na(ExitSpeed), ExitSpeed >= 95)
  }
  
  # ---- base strike zone (ALWAYS drawn) ----
  base_zone <- ggplot() +
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
             fill = NA, color = "black", linewidth = 0.8) +
    annotate("path",
             x = c(-0.708, 0.708, 0.708, 0, -0.708, -0.708),
             y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15),
             color = "black", linewidth = 0.6) +
    coord_fixed(xlim = c(-2, 2), ylim = c(0, 4.5)) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_blank(),
      plot.margin = margin(2, 2, 2, 2)
    )
  
  # ---- if NO pitches: blank zone only ----
  if (nrow(df) == 0) return(base_zone)
  
  # ---- small N: show pitch locations (no density) ----
  if (nrow(df) < 5) {
    return(
      base_zone +
        geom_point(data = df, aes(x = PlateLocSide, y = PlateLocHeight), alpha = 0.85, size = 0.9)
    )
  }
  
  # ---- enough N: density + (optional) points ----
  base_zone +
    stat_density_2d(
      data = df,
      aes(x = PlateLocSide, y = PlateLocHeight, fill = after_stat(density)),
      geom = "raster", contour = FALSE
    ) +
    scale_fill_gradientn(colours = c("white", "blue", "#FF9999", "red", "darkred"), guide = "none") +
    geom_point(data = df, aes(x = PlateLocSide, y = PlateLocHeight), alpha = 0.25, size = 0.6)
}


# Enhanced pitch characteristics with velocity range, putaway%, release point
create_enhanced_pitch_characteristics <- function(data, pitcher_name) {
  df <- data %>%
    dplyr::filter(Pitcher == pitcher_name, !is.na(TaggedPitchType), TaggedPitchType != "Other") %>%
    process_pitcher_indicators()
  
  if (nrow(df) < 10) return(NULL)
  
  total_pitches <- nrow(df)
  pitcher_hand  <- df$PitcherThrows[1]
  
  has_stuff_plus <- "stuff_plus" %in% names(df)
  has_vaa        <- "VertApprAngle" %in% names(df)
  
  # wobacon with any case
  wobacon_col <- names(df)[tolower(names(df)) == "wobacon"]
  has_wobacon <- length(wobacon_col) > 0
  if (has_wobacon) wobacon_col <- wobacon_col[1]
  
  # woba column (overall wOBA, not just on contact)
  woba_col <- names(df)[tolower(names(df)) == "woba"]
  has_woba <- length(woba_col) > 0
  if (has_woba) woba_col <- woba_col[1]
  
  # safe helper for whiff% with denominator
  whiff_pct_safe <- function(swing, whiff) {
    ifelse(swing > 0, round(100 * whiff / swing, 1), 0)
  }
  
  pitch_stats <- df %>%
    dplyr::group_by(Pitch = TaggedPitchType) %>%
    dplyr::summarise(
      Count = dplyr::n(),
      `Usage%` = round(100 * dplyr::n() / total_pitches, 1),
      
      AvgVelo = round(mean(RelSpeed, na.rm = TRUE), 1),
      MaxVelo = round(max(RelSpeed, na.rm = TRUE), 1),
      
      # Velo range (10th-90th percentile)
      VeloRng = paste0(round(stats::quantile(RelSpeed, 0.10, na.rm = TRUE), 0), "-",
                       round(stats::quantile(RelSpeed, 0.90, na.rm = TRUE), 0)),
      
      Spin = round(mean(SpinRate, na.rm = TRUE), 0),
      IVB  = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      HB   = round(mean(HorzBreak, na.rm = TRUE), 1),
      VAA  = if (has_vaa) round(mean(VertApprAngle, na.rm = TRUE), 2) else NA_real_,
      Ext  = round(mean(Extension, na.rm = TRUE), 2),
      
      hRel = round(mean(RelSide, na.rm = TRUE), 2),
      vRel = round(mean(RelHeight, na.rm = TRUE), 2),
      
      `Strike%`  = round(100 * sum(is_strike, na.rm = TRUE) / dplyr::n(), 1),
      `Whiff%`   = whiff_pct_safe(sum(is_swing, na.rm = TRUE), sum(is_whiff, na.rm = TRUE)),
      `Zone%`    = round(100 * sum(in_zone, na.rm = TRUE) / dplyr::n(), 1),
      `Putaway%` = ifelse(sum(Strikes == 2, na.rm = TRUE) > 0,
                          round(100 * sum(is_put_away, na.rm = TRUE) / sum(Strikes == 2, na.rm = TRUE), 1), 0),
      
      # Whiff% split by batter side (uses swings as denominator within side)
      `Whiff% vL` = whiff_pct_safe(
        sum(is_swing[BatterSide == "Left"],  na.rm = TRUE),
        sum(is_whiff[BatterSide == "Left"],  na.rm = TRUE)
      ),
      `Whiff% vR` = whiff_pct_safe(
        sum(is_swing[BatterSide == "Right"], na.rm = TRUE),
        sum(is_whiff[BatterSide == "Right"], na.rm = TRUE)
      ),
      
      AvgEV = round(mean(ExitSpeed[PitchCall == "InPlay"], na.rm = TRUE), 1),
      
      # overall wOBAcon (in-play only)
      wOBAcon_val = if (has_wobacon) {
        round(mean(.data[[wobacon_col]][PitchCall == "InPlay"], na.rm = TRUE), 3)
      } else NA_real_,
      
      # wOBA overall and splits (all pitches, not just in-play)
      wOBA_val = if("woba" %in% names(.data)) round(mean(woba, na.rm = TRUE), 3) else NA_real_,
      wOBA_vL_val = if (has_woba) {
        round(mean(.data[[woba_col]][BatterSide == "Left"], na.rm = TRUE), 3)
      } else NA_real_,
      wOBA_vR_val = if (has_woba) {
        round(mean(.data[[woba_col]][BatterSide == "Right"], na.rm = TRUE), 3)
      } else NA_real_,
      
      `Stuff+` = if (has_stuff_plus) round(mean(stuff_plus, na.rm = TRUE), 0) else NA_real_,
      
      Notes = "",
      .groups = "drop"
    ) %>%
    dplyr::rename(
      wOBAcon     = wOBAcon_val,
      wOBA        = wOBA_val,
      `wOBA vL`   = wOBA_vL_val,
      `wOBA vR`   = wOBA_vR_val
    ) %>%
    dplyr::filter(Count >= 5) %>%
    dplyr::arrange(dplyr::desc(`Usage%`))
  
  # Totals row
  totals <- df %>%
    dplyr::summarise(
      Pitch = "TOTAL",
      Count = dplyr::n(),
      `Usage%` = 100,
      
      AvgVelo = round(mean(RelSpeed, na.rm = TRUE), 1),
      MaxVelo = round(max(RelSpeed, na.rm = TRUE), 1),
      VeloRng = "-",
      
      Spin = round(mean(SpinRate, na.rm = TRUE), 0),
      IVB  = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      HB   = round(mean(HorzBreak, na.rm = TRUE), 1),
      VAA  = if (has_vaa) round(mean(VertApprAngle, na.rm = TRUE), 2) else NA_real_,
      Ext  = round(mean(Extension, na.rm = TRUE), 2),
      
      hRel = round(mean(RelSide, na.rm = TRUE), 2),
      vRel = round(mean(RelHeight, na.rm = TRUE), 2),
      
      `Strike%`  = round(100 * sum(is_strike, na.rm = TRUE) / dplyr::n(), 1),
      `Whiff%`   = whiff_pct_safe(sum(is_swing, na.rm = TRUE), sum(is_whiff, na.rm = TRUE)),
      `Zone%`    = round(100 * sum(in_zone, na.rm = TRUE) / dplyr::n(), 1),
      `Putaway%` = ifelse(sum(Strikes == 2, na.rm = TRUE) > 0,
                          round(100 * sum(is_put_away, na.rm = TRUE) / sum(Strikes == 2, na.rm = TRUE), 1), 0),
      
      # totals splits
      `Whiff% vL` = whiff_pct_safe(
        sum(is_swing[BatterSide == "Left"], na.rm = TRUE),
        sum(is_whiff[BatterSide == "Left"], na.rm = TRUE)
      ),
      `Whiff% vR` = whiff_pct_safe(
        sum(is_swing[BatterSide == "Right"], na.rm = TRUE),
        sum(is_whiff[BatterSide == "Right"], na.rm = TRUE)
      ),
      
      AvgEV = round(mean(ExitSpeed[PitchCall == "InPlay"], na.rm = TRUE), 1),
      
      wOBAcon = if (has_wobacon) round(mean(.data[[wobacon_col]][PitchCall == "InPlay"], na.rm = TRUE), 3) else NA_real_,
      
      # wOBA overall and splits for totals
      wOBA = if (has_woba) round(mean(.data[[woba_col]], na.rm = TRUE), 3) else NA_real_,
      `wOBA vL` = if (has_woba) round(mean(.data[[woba_col]][BatterSide == "Left"], na.rm = TRUE), 3) else NA_real_,
      `wOBA vR` = if (has_woba) round(mean(.data[[woba_col]][BatterSide == "Right"], na.rm = TRUE), 3) else NA_real_,
      
      `Stuff+` = if (has_stuff_plus) round(mean(stuff_plus, na.rm = TRUE), 0) else NA_real_,
      Notes = ""
    )
  
  pitch_stats <- dplyr::bind_rows(pitch_stats, totals)
  
  list(stats = pitch_stats, pitcher_hand = pitcher_hand)
}

# ============================================================================
# NEW: ROLLING DATA HELPER (for individual pitcher PDF charts)
# ============================================================================
create_rolling_data_single <- function(data, pitcher_name, stat_type) {
  fb_pitches <- c("Fastball", "Sinker", "FourSeamFastBall", "TwoSeamFastBall", "Four-Seam", "Two-Seam")
  
  rolling_data <- data %>%
    filter(Pitcher == pitcher_name) %>%
    arrange(Date, GameUID, PitchofPA) %>%
    group_by(GameUID) %>%
    mutate(pitch_in_game = row_number()) %>%
    ungroup()
  
  if (stat_type == "velo") {
    raw_data <- rolling_data %>%
      filter(TaggedPitchType %in% fb_pitches, !is.na(RelSpeed)) %>%
      select(pitch_in_game, value = RelSpeed)
  } else {
    return(data.frame(pitch_num = integer(), value = numeric()))
  }
  
  if (nrow(raw_data) == 0) return(data.frame(pitch_num = integer(), value = numeric()))
  
  # Bucket by 10s: center of bucket (5, 15, 25, etc.)
  result <- raw_data %>%
    mutate(pitch_bucket = floor((pitch_in_game - 1) / 10) * 10 + 5) %>%
    group_by(pitch_num = pitch_bucket) %>%
    summarise(value = mean(value, na.rm = TRUE), n = n(), .groups = "drop") %>%
    filter(n >= 5)
  
  result
}

# NEW: Monthly FB Velo helper
create_monthly_fb_velo <- function(data, pitcher_name) {
  fb_pitches <- c("Fastball", "Sinker", "FourSeamFastBall", "TwoSeamFastBall", "Four-Seam", "Two-Seam")
  
  monthly_data <- data %>%
    filter(Pitcher == pitcher_name, TaggedPitchType %in% fb_pitches, !is.na(RelSpeed), !is.na(Date)) %>%
    mutate(
      Date = as.Date(Date),
      month_label = format(Date, "%b %Y"),
      month_date = as.Date(paste0(format(Date, "%Y-%m"), "-01"))
    ) %>%
    group_by(month_date, month_label) %>%
    summarise(avg_velo = mean(RelSpeed, na.rm = TRUE), n = n(), .groups = "drop") %>%
    filter(n >= 5) %>%
    arrange(month_date)
  
  monthly_data
}

# NEW: Time Through Order (TTO) wOBA chart
create_tto_chart <- function(data, pitcher_name) {
  has_woba <- "woba" %in% names(data)
  
  df <- data %>%
    filter(Pitcher == pitcher_name, !is.na(Batter), !is.na(Date)) %>%
    arrange(Date, GameUID, Inning, PAofInning)
  
  if (nrow(df) < 50) {
    return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "Insufficient data", size = 3))
  }
  
  # For each game, track how many times each batter has been faced
  tto_data <- df %>%
    group_by(GameUID, Batter) %>%
    mutate(
      # Each unique PA for this batter in this game
      pa_num_in_game = cumsum(!duplicated(paste(Inning, PAofInning)))
    ) %>%
    ungroup() %>%
    # Keep only last pitch of each PA (the PA result)
    filter(
      KorBB %in% c("Strikeout", "Walk") |
        PitchCall == "HitByPitch" |
        PitchCall == "InPlay"
    ) %>%
    group_by(GameUID, Batter) %>%
    mutate(times_faced = row_number()) %>%
    ungroup()
  
  # Calculate wOBA or a proxy by times faced
  if (has_woba) {
    tto_summary <- tto_data %>%
      group_by(times_faced) %>%
      summarise(
        avg_woba = mean(woba, na.rm = TRUE),
        n_pa = n(),
        .groups = "drop"
      ) %>%
      filter(n_pa >= 5, times_faced <= 5)
  } else {
    # Use OBP-like proxy: hits + walks + HBP / PA
    tto_summary <- tto_data %>%
      mutate(
        reached = as.integer(
          PlayResult %in% c("Single", "Double", "Triple", "HomeRun") |
            KorBB == "Walk" | PitchCall == "HitByPitch"
        )
      ) %>%
      group_by(times_faced) %>%
      summarise(
        avg_woba = mean(reached, na.rm = TRUE),
        n_pa = n(),
        .groups = "drop"
      ) %>%
      filter(n_pa >= 5, times_faced <= 5)
  }
  
  if (nrow(tto_summary) < 2) {
    return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "Insufficient TTO data", size = 3))
  }
  
  y_label <- if (has_woba) "wOBA" else "OBP"
  
  ggplot(tto_summary, aes(x = factor(times_faced), y = avg_woba)) +
    geom_col(fill = "#006F71", width = 0.6, alpha = 0.85) +
    geom_text(aes(label = sprintf("%.3f", avg_woba)), vjust = -0.5, size = 2.5, fontface = "bold") +
    geom_text(aes(label = paste0("n=", n_pa), y = 0), vjust = 1.5, size = 2, color = "gray50") +
    labs(
      title = paste0(y_label, " by Times Through Order"),
      x = "Times Faced", y = y_label
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.15, 0.15))) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 9, color = "#006F71"),
      axis.text = element_text(size = 7),
      axis.title = element_text(size = 7),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(2, 2, 2, 2)
    )
}

create_pitcher_inning_row <- function(data, pitcher_name) {
  inning_data <- data %>%
    filter(Pitcher == pitcher_name, !is.na(Inning)) %>%
    mutate(Inning = ifelse(Inning > 9, "X", as.character(Inning))) %>%
    group_by(Inning) %>%
    summarise(n = n(), .groups = "drop")
  
  all_innings <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "X")
  inning_data <- data.frame(Inning = all_innings) %>%
    left_join(inning_data, by = "Inning") %>%
    mutate(n = ifelse(is.na(n), 0, n))
  inning_data$Inning <- factor(inning_data$Inning, levels = all_innings)
  
  ggplot(inning_data, aes(x = Inning, y = 1, fill = n)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = ifelse(n > 0, n, "")), size = 2.5, fontface = "bold") +
    scale_fill_gradient2(low = "#E1463E", mid = "#FFFFCC", high = "#00840D",
                         midpoint = max(inning_data$n[inning_data$n > 0], na.rm = TRUE) / 2,
                         guide = "none") +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL, title = "Pitch Count by Inning") +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 8, color = "#006F71"),
      axis.text.x = element_text(size = 6, face = "bold"),
      plot.margin = margin(2, 2, 2, 2)
    )
}

# Main PDF creation function - WITH NOTES SUPPORT
create_comprehensive_pitcher_pdf <- function(data, pitcher_name, output_file,
                                             delivery_notes = "", count_usage_notes = "",
                                             pitch_notes = list(), overall_notes = "",
                                             vs_lhh_notes = "", vs_rhh_notes = "",
                                             series_title = "", pitcher_role = "",
                                             matchup_matrix = NULL) {
  if (length(dev.list()) > 0) try(dev.off(), silent = TRUE)
  
  # Filter pitcher data
  pitcher_df <- data %>% filter(Pitcher == pitcher_name)
  if (nrow(pitcher_df) < 20) {
    pdf(output_file, width = 22, height = 28)
    grid::grid.newpage()
    grid::grid.text(paste("Insufficient data for", pitcher_name),
                    gp = grid::gpar(fontsize = 20, fontface = "bold"))
    dev.off()
    return(output_file)
  }
  
  # Get advanced stats
  stats <- calculate_advanced_pitcher_stats(data, pitcher_name)
  if (is.null(stats)) {
    pdf(output_file, width = 22, height = 28)
    grid::grid.newpage()
    grid::grid.text(paste("Could not calculate stats for", pitcher_name),
                    gp = grid::gpar(fontsize = 20, fontface = "bold"))
    dev.off()
    return(output_file)
  }
  
  # Get pitch characteristics
  pitch_result <- create_enhanced_pitch_characteristics(data, pitcher_name)
  pitch_char <- if (!is.null(pitch_result)) pitch_result$stats else NULL
  
  # Create plots
  arm_angle_plot <- tryCatch(
    create_pitcher_arm_angle_plot(data, pitcher_name),
    error = function(e) ggplot() + theme_void() + ggtitle("Arm Angle Error")
  )
  
  movement_plot <- tryCatch(
    create_pitcher_movement_plot(data, pitcher_name, exp_movement_grid),
    error = function(e) ggplot() + theme_void() + ggtitle("Movement Error")
  )
  
  count_plot <- tryCatch(
    create_count_usage_pies(data, pitcher_name),
    error = function(e) ggplot() + theme_void() + ggtitle("Count Usage Error")
  )
  
  seq_lhh <- tryCatch(
    create_sequencing_matrix(data, pitcher_name, "Left"),
    error = function(e) ggplot() + theme_void() + ggtitle("Seq vs LHH Error")
  )
  
  seq_rhh <- tryCatch(
    create_sequencing_matrix(data, pitcher_name, "Right"),
    error = function(e) ggplot() + theme_void() + ggtitle("Seq vs RHH Error")
  )
  
  spray_lhh <- tryCatch(
    create_hits_spray_chart(data, pitcher_name, "Left"),
    error = function(e) ggplot() + theme_void() + ggtitle("Spray LHH Error")
  )
  
  spray_rhh <- tryCatch(
    create_hits_spray_chart(data, pitcher_name, "Right"),
    error = function(e) ggplot() + theme_void() + ggtitle("Spray RHH Error")
  )
  
  # Create heatmaps
  heatmaps <- list()
  for (pg in c("FB", "BB", "OS")) {
    for (mt in c("All", "Whf", "Dmg")) {
      for (side in c("Left", "Right")) {
        key <- paste0(pg, "_", mt, "_", side)
        heatmaps[[key]] <- tryCatch(
          create_pitch_group_heatmap(data, pitcher_name, pg, mt, side),
          error = function(e) ggplot() + theme_void()
        )
      }
    }
  }
  
  # Start PDF
  pdf(output_file, width = 22, height = 28)
  on.exit(try(dev.off(), silent = TRUE), add = TRUE)
  grid::grid.newpage()
  
  # ===== HEADER =====
  # Add series title if provided
  if (nchar(series_title) > 0) {
    grid::grid.rect(x = 0.5, y = 0.995, width = 1, height = 0.012,
                    gp = grid::gpar(fill = "#1976D2", col = NA), just = c("center", "top"))
    grid::grid.text(series_title, x = 0.5, y = 0.989,
                    gp = grid::gpar(fontsize = 14, fontface = "bold", col = "white"))
  }
  
  # Pitcher name header
  header_y <- if (nchar(series_title) > 0) 0.982 else 0.985
  grid::grid.rect(x = 0.5, y = header_y, width = 1, height = 0.025,
                  gp = grid::gpar(fill = "#006F71", col = NA), just = c("center", "top"))
  
  # Get pitcher info
  pitcher_df <- data %>% filter(Pitcher == pitcher_name)
  pitcher_throws <- if (nrow(pitcher_df) > 0 && "PitcherThrows" %in% names(pitcher_df)) {
    pitcher_df$PitcherThrows[1]
  } else {
    "Right"
  }
  is_lefty <- !is.na(pitcher_throws) && pitcher_throws == "Left"
  hand_label <- if (is_lefty) "LHP" else "RHP"
  
  # Get pitcher height
  pitcher_height_str <- get_pitcher_height_display(pitcher_name)
  height_label <- if (nchar(pitcher_height_str) > 0) paste0(" | ", pitcher_height_str) else ""
  
  # Jersey number + class from the DRS bio (team-aware when the pitch
  # data carries a PitcherTeam)
  team_hint <- if (nrow(pitcher_df) > 0 && "PitcherTeam" %in% names(pitcher_df)) {
    tt <- pitcher_df$PitcherTeam[!is.na(pitcher_df$PitcherTeam)]
    if (length(tt) > 0 && exists("prettify_team")) prettify_team(tt[1]) else NULL
  } else NULL
  drs_hdr <- tryCatch(drs_lookup(pitcher_name, team_hint), error = function(e) NULL)
  num_prefix <- if (!is.null(drs_hdr) && "Jersey" %in% names(drs_hdr) &&
                    !is.na(drs_hdr$Jersey) && nzchar(as.character(drs_hdr$Jersey)))
    paste0("#", drs_hdr$Jersey, " ") else ""
  class_label <- if (!is.null(drs_hdr) && !is.na(drs_hdr$Class) &&
                     nzchar(as.character(drs_hdr$Class)))
    paste0(" | ", drs_hdr$Class) else ""
  
  # Combine pitcher name with role if provided
  display_name <- if (nchar(pitcher_role) > 0) {
    paste0(pitcher_role, " - ", num_prefix, pitcher_name)
  } else {
    paste0(num_prefix, pitcher_name)
  }
  
  # Name color - red for lefties
  name_color <- if (is_lefty) "#FF4444" else "white"
  
  # Draw pitcher name
  grid::grid.text(display_name, x = 0.5, y = header_y - 0.010,
                  gp = grid::gpar(fontsize = 24, fontface = "bold", col = name_color))
  
  # Draw handedness, height, and class on right side of header
  grid::grid.text(paste0(hand_label, height_label, class_label), x = 0.95, y = header_y - 0.012,
                  gp = grid::gpar(fontsize = 12, fontface = "bold", col = "white"), just = "right")
  
  # ===== ROW 1: Basic Stats =====
  row1_y <- 0.955
  row1_labels <- c("IP", "G", "IP/G", "P/G", "Last 3 Outings")
  row1_values <- c(sprintf("%.1f", stats$ip), stats$n_games, sprintf("%.1f", stats$avg_ip),
                   stats$avg_pitches, stats$last_3_outings)
  
  n_cols1 <- length(row1_labels)
  total_width1 <- 0.70  # Wider to fit last 3 outings
  col_widths1 <- c(0.08, 0.08, 0.08, 0.08, 0.38)  # Custom widths
  start_x1 <- 0.15
  row_h <- 0.012
  
  x_pos <- start_x1
  for (i in seq_along(row1_labels)) {
    x_center <- x_pos + col_widths1[i] / 2
    grid::grid.rect(x = x_center, y = row1_y, width = col_widths1[i], height = row_h,
                    gp = grid::gpar(fill = "#006F71", col = "black", lwd = 0.5), just = c("center", "top"))
    grid::grid.text(row1_labels[i], x = x_center, y = row1_y - row_h * 0.5,
                    gp = grid::gpar(fontsize = if(i == 5) 8 else 10, fontface = "bold", col = "white"))
    grid::grid.rect(x = x_center, y = row1_y - row_h, width = col_widths1[i], height = row_h,
                    gp = grid::gpar(fill = "white", col = "black", lwd = 0.5), just = c("center", "top"))
    grid::grid.text(as.character(row1_values[i]), x = x_center, y = row1_y - row_h * 1.5,
                    gp = grid::gpar(fontsize = if(i == 5) 8 else 10))
    x_pos <- x_pos + col_widths1[i]
  }
  
  # ===== ROW 2: Advanced Stats =====
  row2_y <- 0.925
  row2_labels <- c("RA9", "FIP", "WHIP", "K%", "BB%", "BA L", "BA R", "SLG L", "SLG R")
  row2_values <- c(stats$ra9, stats$fip, stats$whip, stats$k_pct, stats$bb_pct,
                   stats$ba_vs_l, stats$ba_vs_r, stats$slg_vs_l, stats$slg_vs_r)
  row2_benchmarks <- c(sec_benchmarks$era, sec_benchmarks$fip, sec_benchmarks$whip,
                       sec_benchmarks$k_pct, sec_benchmarks$bb_pct,
                       sec_benchmarks$ba_against, sec_benchmarks$ba_against,
                       sec_benchmarks$slg_against, sec_benchmarks$slg_against)
  row2_higher_better <- c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  
  n_cols2 <- length(row2_labels)
  total_width2 <- 0.80
  col_w2 <- total_width2 / n_cols2
  start_x2 <- 0.10
  
  for (i in seq_along(row2_labels)) {
    x_pos <- start_x2 + (i - 0.5) * col_w2
    fill_col <- get_stat_color(row2_values[i], row2_benchmarks[i], row2_higher_better[i], 0.30)
    grid::grid.rect(x = x_pos, y = row2_y, width = col_w2, height = 0.012,
                    gp = grid::gpar(fill = "#006F71", col = "black", lwd = 0.5), just = c("center", "top"))
    grid::grid.text(row2_labels[i], x = x_pos, y = row2_y - 0.006,
                    gp = grid::gpar(fontsize = 9, fontface = "bold", col = "white"))
    grid::grid.rect(x = x_pos, y = row2_y - 0.012, width = col_w2, height = 0.014,
                    gp = grid::gpar(fill = fill_col, col = "black", lwd = 0.5), just = c("center", "top"))
    val_text <- if (is.na(row2_values[i])) "-" else if (i <= 3) sprintf("%.2f", row2_values[i]) else if (i <= 5) paste0(row2_values[i], "%") else sprintf(".%03d", round(row2_values[i] * 1000))
    grid::grid.text(val_text, x = x_pos, y = row2_y - 0.019,
                    gp = grid::gpar(fontsize = 9))
  }
  
  # ===== ROW 3: Pitch-level Stats =====
  row3_y <- 0.895
  row3_labels <- c("Whiff%", "Zone%", "Strike%", "FB Strike%", "BB Strike%", "OS Strike%")
  row3_values <- c(stats$whiff_pct, stats$zone_pct, stats$strike_pct,
                   stats$fb_strike_pct, stats$bb_strike_pct, stats$os_strike_pct)
  row3_benchmarks <- c(sec_benchmarks$whiff_pct, sec_benchmarks$zone_pct, sec_benchmarks$strike_pct,
                       sec_benchmarks$fb_strike, sec_benchmarks$bb_strike, sec_benchmarks$os_strike)
  
  n_cols3 <- length(row3_labels)
  total_width3 <- 0.70
  col_w3 <- total_width3 / n_cols3
  start_x3 <- 0.15
  
  for (i in seq_along(row3_labels)) {
    x_pos <- start_x3 + (i - 0.5) * col_w3
    fill_col <- get_stat_color(row3_values[i], row3_benchmarks[i], TRUE, 0.20)
    grid::grid.rect(x = x_pos, y = row3_y, width = col_w3, height = 0.012,
                    gp = grid::gpar(fill = "#006F71", col = "black", lwd = 0.5), just = c("center", "top"))
    grid::grid.text(row3_labels[i], x = x_pos, y = row3_y - 0.006,
                    gp = grid::gpar(fontsize = 9, fontface = "bold", col = "white"))
    grid::grid.rect(x = x_pos, y = row3_y - 0.012, width = col_w3, height = 0.014,
                    gp = grid::gpar(fill = fill_col, col = "black", lwd = 0.5), just = c("center", "top"))
    val_text <- if (is.na(row3_values[i])) "-" else paste0(row3_values[i], "%")
    grid::grid.text(val_text, x = x_pos, y = row3_y - 0.019,
                    gp = grid::gpar(fontsize = 9))
  }
  
  # ===== CHARTS ROW 1: Arm Angle, Movement, Count Usage =====
  chart_row1_y <- 0.865
  chart_height1 <- 0.20
  
  # Arm angle
  grid::pushViewport(grid::viewport(x = 0.11, y = chart_row1_y, width = 0.19, height = chart_height1 * 0.80, just = c("center", "top")))
  tryCatch(print(arm_angle_plot, newpage = FALSE), error = function(e) NULL)
  grid::popViewport()
  
  # Delivery Notes box under arm angle - ALWAYS show the box
  delivery_notes_y <- chart_row1_y - chart_height1 * 0.80 - 0.005
  notes_height <- 0.045
  grid::grid.rect(x = 0.12, y = delivery_notes_y, width = 0.18, height = notes_height,
                  gp = grid::gpar(fill = "white", col = "#006F71", lwd = 1), just = c("center", "top"))
  grid::grid.text("Delivery Notes", x = 0.12, y = delivery_notes_y - 0.005,
                  gp = grid::gpar(fontsize = 8, fontface = "bold", col = "#006F71"))
  if (nchar(delivery_notes) > 0) {
    # Wrap text for notes
    wrapped_notes <- strwrap(delivery_notes, width = 30)
    for (j in seq_along(wrapped_notes)) {
      grid::grid.text(wrapped_notes[j], x = 0.11, y = delivery_notes_y - 0.015 - (j - 1) * 0.009,
                      gp = grid::gpar(fontsize = 7))
    }
  }
  
  # Movement - HUGE
  grid::pushViewport(grid::viewport(x = 0.46, y = chart_row1_y, width = 0.58, height = chart_height1 + 0.03, just = c("center", "top")))
  tryCatch(print(movement_plot, newpage = FALSE), error = function(e) NULL)
  grid::popViewport()
  
  # Count usage pies - lowered position
  grid::pushViewport(grid::viewport(x = 0.85, y = chart_row1_y - 0.02, width = 0.28, height = chart_height1 * 0.55, just = c("center", "top")))
  tryCatch(print(count_plot, newpage = FALSE), error = function(e) NULL)
  grid::popViewport()
  
  # ===== TIME THROUGH ORDER CHART (replaces pitch usage table) =====
  tto_chart <- tryCatch(
    create_tto_chart(data, pitcher_name),
    error = function(e) ggplot() + theme_void() + ggtitle("TTO Error")
  )
  
  usage_table_y <- 0.735
  usage_table_center <- 0.85
  
  pitch_df <- data %>%
    filter(Pitcher == pitcher_name, !is.na(TaggedPitchType), TaggedPitchType != "Other")
  
  main_pitches <- pitch_df %>%
    count(TaggedPitchType) %>%
    mutate(pct = n / sum(n) * 100) %>%
    filter(pct >= 3) %>%
    arrange(desc(pct)) %>%
    head(4) %>%
    pull(TaggedPitchType)
  
  # ===== TTO CHART (replaces old pitch usage table) =====
  grid::pushViewport(grid::viewport(x = usage_table_center, y = usage_table_y + 0.01, 
                                    width = 0.26, height = 0.10, just = c("center", "top")))
  tryCatch(print(tto_chart, newpage = FALSE), error = function(e) NULL)
  grid::popViewport()
  
  # ===== CHARTS ROW 2: Sequencing and Spray Charts =====
  chart_row2_y <- 0.645
  chart_height2 <- 0.11
  
  grid::pushViewport(grid::viewport(x = 0.17, y = chart_row2_y, width = 0.26, height = chart_height2, just = c("center", "top")))
  tryCatch(print(seq_lhh, newpage = FALSE), error = function(e) NULL)
  grid::popViewport()
  
  grid::pushViewport(grid::viewport(x = 0.43, y = chart_row2_y, width = 0.26, height = chart_height2, just = c("center", "top")))
  tryCatch(print(seq_rhh, newpage = FALSE), error = function(e) NULL)
  grid::popViewport()
  
  grid::pushViewport(grid::viewport(x = 0.67, y = chart_row2_y, width = 0.18, height = chart_height2, just = c("center", "top")))
  tryCatch(print(spray_lhh, newpage = FALSE), error = function(e) NULL)
  grid::popViewport()
  
  grid::pushViewport(grid::viewport(x = 0.85, y = chart_row2_y, width = 0.18, height = chart_height2, just = c("center", "top")))
  tryCatch(print(spray_rhh, newpage = FALSE), error = function(e) NULL)
  grid::popViewport()
  
  # ===== PITCH CHARACTERISTICS TABLE =====
  table_y <- 0.525
  
  if (!is.null(pitch_char) && nrow(pitch_char) > 0) {
    headers <- names(pitch_char)
    num_cols <- length(headers)
    num_rows <- min(nrow(pitch_char), 7)
    
    base_width <- 0.92 / (num_cols - 1 + 2.5)
    col_widths <- sapply(headers, function(h) {
      if (h == "Notes") base_width * 2.5 else base_width
    })
    x_starts_tbl <- 0.04 + cumsum(c(0, col_widths[-length(col_widths)]))
    row_h <- 0.016
    
    for (j in seq_along(headers)) {
      grid::grid.rect(x = x_starts_tbl[j], y = table_y, width = col_widths[j] * 0.98, height = row_h,
                      gp = grid::gpar(fill = "#006F71", col = "black", lwd = 0.5), just = c("left", "top"))
      grid::grid.text(headers[j], x = x_starts_tbl[j] + col_widths[j] * 0.49, y = table_y - row_h * 0.5,
                      gp = grid::gpar(fontsize = 8, fontface = "bold", col = "white"))
    }
    
    rate_cols <- c("Strike%", "Whiff%", "Zone%", "Putaway%")
    velo_cols <- c("AvgVelo", "MaxVelo")
    
    # SEC benchmarks by pitch type for AvgEV and wOBAcon (lower is better for pitcher)
    ev_benchmarks <- list(
      "Fastball" = 88.6, "Four-Seam" = 88.6, "FourSeamFastBall" = 88.6,
      "Sinker" = 87.9, "TwoSeamFastBall" = 87.9,
      "Slider" = 85.3, "Sweeper" = 85.5,
      "Changeup" = 85.6, "ChangeUp" = 85.6,
      "Curveball" = 85.4, "Splitter" = 85.2, "Cutter" = 86.5
    )
    wobacon_benchmarks <- list(
      "Fastball" = 0.402, "Four-Seam" = 0.402, "FourSeamFastBall" = 0.402,
      "Sinker" = 0.377, "TwoSeamFastBall" = 0.377,
      "Slider" = 0.393, "Sweeper" = 0.395,
      "Changeup" = 0.389, "ChangeUp" = 0.389,
      "Curveball" = 0.372, "Splitter" = 0.358, "Cutter" = 0.390
    )
    
    for (i in seq_len(num_rows)) {
      y_row <- table_y - i * row_h
      pitch_type <- pitch_char$Pitch[i]
      
      velo_bench <- if (pitch_type %in% c("Fastball", "Sinker", "Four-Seam", "Two-Seam")) 93 
      else if (pitch_type == "Slider") 83
      else if (pitch_type == "Curveball") 79
      else if (pitch_type %in% c("ChangeUp", "Changeup")) 84
      else if (pitch_type == "Cutter") 87
      else 85
      
      spin_bench <- if (pitch_type %in% c("Fastball", "Sinker", "Four-Seam", "Two-Seam")) 2267
      else if (pitch_type == "Slider") 2440
      else if (pitch_type == "Curveball") 2442
      else if (pitch_type %in% c("ChangeUp", "Changeup")) 1708
      else if (pitch_type == "Cutter") 2387
      else 2200
      
      ext_bench <- if (pitch_type %in% c("Fastball", "Sinker", "Four-Seam", "Two-Seam")) 5.83
      else if (pitch_type == "Slider") 5.54
      else if (pitch_type == "Curveball") 5.47
      else if (pitch_type %in% c("ChangeUp", "Changeup")) 5.98
      else if (pitch_type == "Cutter") 5.70
      else 5.60
      
      # Get SEC benchmarks for this pitch type
      ev_bench <- ev_benchmarks[[pitch_type]] %||% 86.5
      wobacon_bench <- wobacon_benchmarks[[pitch_type]] %||% 0.390
      
      for (j in seq_along(headers)) {
        val <- pitch_char[[headers[j]]][i]
        bg <- "#FFFFFF"
        
        if (headers[j] == "Pitch" && as.character(val) %in% names(pitch_colors)) {
          bg <- pitch_colors[[as.character(val)]]
        }
        if (headers[j] %in% velo_cols && is.numeric(val) && !is.na(val)) {
          bg <- get_stat_color(val, velo_bench, TRUE, 0.08)
        }
        if (headers[j] == "Spin" && is.numeric(val) && !is.na(val)) {
          bg <- get_stat_color(val, spin_bench, TRUE, 0.15)
        }
        if (headers[j] == "Ext" && is.numeric(val) && !is.na(val)) {
          bg <- get_stat_color(val, ext_bench, TRUE, 0.10)
        }
        if (headers[j] %in% rate_cols && is.numeric(val) && !is.na(val)) {
          benchmark <- switch(headers[j],
                              "Strike%" = 62,
                              "Whiff%" = 29,
                              "Zone%" = 46,
                              "Putaway%" = 25,
                              NA)
          if (!is.na(benchmark)) {
            bg <- get_stat_color(val, benchmark, TRUE, 0.25)
          }
        }
        # Whiff% vL and vR coloring - higher is better for pitcher
        if (headers[j] %in% c("Whiff% vL", "Whiff% vR") && is.numeric(val) && !is.na(val)) {
          bg <- get_stat_color(val, 29, TRUE, 0.25)  # TRUE = higher is better
        }
        # AvgEV coloring - lower is better for pitcher (inverse)
        if (headers[j] == "AvgEV" && is.numeric(val) && !is.na(val) && pitch_type != "TOTAL") {
          bg <- get_stat_color(val, ev_bench, FALSE, 0.06)  # FALSE = lower is better
        }
        # wOBAcon coloring - lower is better for pitcher (inverse)
        if (headers[j] == "wOBAcon" && is.numeric(val) && !is.na(val) && pitch_type != "TOTAL") {
          bg <- get_stat_color(val, wobacon_bench, FALSE, 0.15)  # FALSE = lower is better
        }
        # wOBA overall and splits coloring - lower is better for pitcher (inverse)
        if (headers[j] %in% c("wOBA", "wOBA vL", "wOBA vR") && is.numeric(val) && !is.na(val) && pitch_type != "TOTAL") {
          bg <- get_stat_color(val, 0.320, FALSE, 0.15)  # FALSE = lower is better, SEC avg wOBA ~0.320
        }
        if (headers[j] == "Stuff+" && is.numeric(val) && !is.na(val)) {
          bg <- get_stat_color(val, 100, TRUE, 0.15)
        }
        if (headers[j] == "Notes") {
          bg <- "#f5f5f5"
        }
        
        grid::grid.rect(x = x_starts_tbl[j], y = y_row, width = col_widths[j] * 0.98, height = row_h,
                        gp = grid::gpar(fill = bg, col = "gray70", lwd = 0.3), just = c("left", "top"))
        
        # Check for pitch-specific notes
        display_val <- if (headers[j] == "Notes" && pitch_type %in% names(pitch_notes)) {
          pitch_notes[[pitch_type]]
        } else if (headers[j] %in% c("wOBAcon", "wOBA", "wOBA vL", "wOBA vR") && is.numeric(val) && !is.na(val)) {
          # Format wOBA/wOBAcon as .XXX (e.g., .402 not 0.40)
          if (val < 1) {
            sub("^0", "", sprintf("%.3f", val))  # Remove leading zero: 0.402 -> .402
          } else {
            sprintf("%.3f", val)
          }
        } else if (is.numeric(val)) {
          if (is.na(val)) "-" else if (abs(val) < 10 && val != round(val)) sprintf("%.2f", val) else sprintf("%.1f", val)
        } else as.character(val)
        
        if (headers[j] != "Notes" || (headers[j] == "Notes" && pitch_type %in% names(pitch_notes))) {
          grid::grid.text(display_val, x = x_starts_tbl[j] + col_widths[j] * 0.49, y = y_row - row_h * 0.5,
                          gp = grid::gpar(fontsize = 7.5))
        }
      }
    }
  }
  
  hm_section_y <- 0.34
  
  grid::grid.rect(x = 0.5, y = hm_section_y + 0.032, width = 0.98, height = 0.015,
                  gp = grid::gpar(fill = "#006F71", col = NA))
  grid::grid.text("Location Analysis", x = 0.5, y = hm_section_y + 0.032,
                  gp = grid::gpar(fontsize = 10, fontface = "bold", col = "white"))
  
  hm_colors <- c("white", "blue", "#FF9999", "red", "darkred")
  fb_pill <- "#FA8072"
  bb_pill <- "#A020F0"
  os_pill <- "#228B22"
  
  hm_height   <- 0.048
  hm_width    <- 0.052
  hm_gap      <- 0.003
  pill_w      <- 0.022
  pill_h      <- 0.009
  row_pill_w  <- 0.018
  row_pill_h  <- 0.008
  
  section_centers <- seq(0.16, 0.78, length.out = 4)
  sec1_center <- section_centers[1]   # vs LHH
  sec2_center <- section_centers[2]   # vs RHH
  sec3_center <- section_centers[3]   # LHH by Count
  sec4_center <- section_centers[4]   # RHH by Count
  nitro_center <- 0.93
  
  row_y <- c(
    hm_section_y - 0.020,
    hm_section_y - 0.020 - hm_height - hm_gap,
    hm_section_y - 0.020 - 2 * (hm_height + hm_gap)
  )
  
  row_labels_1 <- c("All", "Whf", "Dmg")
  row_colors_1 <- c("#006F71", "#00840D", "#E1463E")
  
  row_labels_count <- c("1st", "Bhd", "2K")
  row_keys_count   <- c("1st", "Behind", "2K")
  row_colors_count <- c("#4169E1", "#FF8C00", "#DC143C")
  
  pitch_groups <- list(
    FB = c("Fastball", "Sinker", "Four-Seam", "FourSeamFastBall", "TwoSeamFastBall"),
    BB = c("Slider", "Curveball", "Sweeper", "Slurve", "Cutter"),
    OS = c("Changeup", "ChangeUp", "Splitter")
  )
  
  
  get_pt_short <- function(pt) {
    dplyr::case_when(
      pt %in% c("Fastball", "Four-Seam", "FourSeamFastBall") ~ "FB",
      pt %in% c("Sinker", "TwoSeamFastBall") ~ "SI",
      pt %in% c("Slider", "Sweeper") ~ "SL",
      pt == "Curveball" ~ "CB",
      pt %in% c("Changeup", "ChangeUp") ~ "CH",
      pt == "Splitter" ~ "SP",
      pt == "Cutter" ~ "CT",
      TRUE ~ substr(as.character(pt), 1, 2)
    )
  }
  
  # ------------------------
  # UNIFIED HEATMAP FUNCTION - Uses stat_density_2d
  # ------------------------
  create_density_heatmap <- function(data, min_pitches = 5) {
    # Base plot with zone
    base_plot <- ggplot() +
      annotate("rect", xmin = -0.8303, xmax = 0.8303, ymin = 1.5, ymax = 3.5,
               fill = NA, color = "black", linewidth = 0.5) +
      annotate("path",
               x = c(-0.708, 0.708, 0.708, 0, -0.708, -0.708),
               y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15),
               color = "black", linewidth = 0.4) +
      coord_fixed(xlim = c(-2, 2), ylim = c(0, 4.5)) +
      theme_void() +
      theme(
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0)
      )
    
    # If no data or too few pitches, return blank zone
    if (is.null(data) || nrow(data) < min_pitches) {
      return(base_plot)
    }
    
    # Filter to valid location data
    plot_data <- data %>%
      dplyr::filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))
    
    if (nrow(plot_data) < min_pitches) {
      return(base_plot)
    }
    
    # If small sample, use points instead of density
    if (nrow(plot_data) < 15) {
      p <- base_plot +
        geom_point(data = plot_data, 
                   aes(x = PlateLocSide, y = PlateLocHeight),
                   color = "red", alpha = 0.7, size = 1.5)
      return(p)
    }
    
    # Full density heatmap for larger samples
    p <- ggplot(plot_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(aes(fill = after_stat(density)), 
                      geom = "raster", 
                      contour = FALSE,
                      n = 100) +
      scale_fill_gradientn(
        colours = c("white", "blue", "#FF9999", "red", "darkred"),
        name = "Density"
      ) +
      annotate("rect", xmin = -0.8303, xmax = 0.8303, ymin = 1.5, ymax = 3.5,
               fill = NA, color = "black", linewidth = 0.5) +
      annotate("path",
               x = c(-0.708, 0.708, 0.708, 0, -0.708, -0.708),
               y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15),
               color = "black", linewidth = 0.4) +
      coord_fixed(xlim = c(-2, 2), ylim = c(0, 4.5)) +
      theme_void() +
      theme(
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0)
      )
    
    return(p)
  }
  
  # ------------------------
  # PILL STAT INDICATORS (vL / vR)
  # ------------------------
  pill_y <- hm_section_y + 0.015
  
  # 2K putaway by side
  putaway_lhh <- pitch_df %>%
    dplyr::filter(Strikes == 2, BatterSide == "Left") %>%
    dplyr::group_by(TaggedPitchType) %>%
    dplyr::summarise(
      n_2k = dplyr::n(),
      n_putaway = sum(KorBB == "Strikeout", na.rm = TRUE),
      putaway_pct = 100 * n_putaway / n_2k,
      .groups = "drop"
    ) %>%
    dplyr::filter(n_2k >= 5) %>%
    dplyr::arrange(dplyr::desc(putaway_pct))
  
  putaway_rhh <- pitch_df %>%
    dplyr::filter(Strikes == 2, BatterSide == "Right") %>%
    dplyr::group_by(TaggedPitchType) %>%
    dplyr::summarise(
      n_2k = dplyr::n(),
      n_putaway = sum(KorBB == "Strikeout", na.rm = TRUE),
      putaway_pct = 100 * n_putaway / n_2k,
      .groups = "drop"
    ) %>%
    dplyr::filter(n_2k >= 5) %>%
    dplyr::arrange(dplyr::desc(putaway_pct))
  
  grid::grid.text("2K Out:", x = 0.10, y = pill_y, just = "left",
                  gp = grid::gpar(fontsize = 6, fontface = "bold", col = "#006F71"))
  
  if (nrow(putaway_lhh) > 0) {
    pt_short <- get_pt_short(putaway_lhh$TaggedPitchType[1])
    grid::grid.roundrect(x = 0.155, y = pill_y, width = 0.055, height = 0.010,
                         r = unit(0.3, "snpc"), gp = grid::gpar(fill = "#CD853F", col = NA))
    grid::grid.text(paste0("vL ", pt_short, " ", sprintf("%.0f%%", putaway_lhh$putaway_pct[1])),
                    x = 0.155, y = pill_y,
                    gp = grid::gpar(fontsize = 5, fontface = "bold", col = "white"))
  }
  if (nrow(putaway_rhh) > 0) {
    pt_short <- get_pt_short(putaway_rhh$TaggedPitchType[1])
    grid::grid.roundrect(x = 0.215, y = pill_y, width = 0.055, height = 0.010,
                         r = unit(0.3, "snpc"), gp = grid::gpar(fill = "#CD853F", col = NA))
    grid::grid.text(paste0("vR ", pt_short, " ", sprintf("%.0f%%", putaway_rhh$putaway_pct[1])),
                    x = 0.215, y = pill_y,
                    gp = grid::gpar(fontsize = 5, fontface = "bold", col = "white"))
  }
  
  # 1st pitch usage by side
  first_lhh <- pitch_df %>%
    dplyr::filter(PitchofPA == 1, BatterSide == "Left") %>%
    dplyr::count(TaggedPitchType, name = "n") %>%
    dplyr::mutate(pct = 100 * n / sum(n)) %>%
    dplyr::arrange(dplyr::desc(pct))
  
  first_rhh <- pitch_df %>%
    dplyr::filter(PitchofPA == 1, BatterSide == "Right") %>%
    dplyr::count(TaggedPitchType, name = "n") %>%
    dplyr::mutate(pct = 100 * n / sum(n)) %>%
    dplyr::arrange(dplyr::desc(pct))
  
  grid::grid.text("1st:", x = 0.27, y = pill_y, just = "left",
                  gp = grid::gpar(fontsize = 6, fontface = "bold", col = "#006F71"))
  
  if (nrow(first_lhh) > 0) {
    pt_short <- get_pt_short(first_lhh$TaggedPitchType[1])
    grid::grid.roundrect(x = 0.315, y = pill_y, width = 0.055, height = 0.010,
                         r = unit(0.3, "snpc"), gp = grid::gpar(fill = "#4169E1", col = NA))
    grid::grid.text(paste0("vL ", pt_short, " ", sprintf("%.0f%%", first_lhh$pct[1])),
                    x = 0.315, y = pill_y,
                    gp = grid::gpar(fontsize = 5, fontface = "bold", col = "white"))
  }
  if (nrow(first_rhh) > 0) {
    pt_short <- get_pt_short(first_rhh$TaggedPitchType[1])
    grid::grid.roundrect(x = 0.375, y = pill_y, width = 0.055, height = 0.010,
                         r = unit(0.3, "snpc"), gp = grid::gpar(fill = "#4169E1", col = NA))
    grid::grid.text(paste0("vR ", pt_short, " ", sprintf("%.0f%%", first_rhh$pct[1])),
                    x = 0.375, y = pill_y,
                    gp = grid::gpar(fontsize = 5, fontface = "bold", col = "white"))
  }
  
  # Hardest hit (avg EV) by side
  hardest_lhh <- pitch_df %>%
    dplyr::filter(PitchCall == "InPlay", !is.na(ExitSpeed), BatterSide == "Left") %>%
    dplyr::group_by(TaggedPitchType) %>%
    dplyr::summarise(avg_ev = mean(ExitSpeed, na.rm = TRUE), n_bip = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n_bip >= 3) %>%
    dplyr::arrange(dplyr::desc(avg_ev))
  
  hardest_rhh <- pitch_df %>%
    dplyr::filter(PitchCall == "InPlay", !is.na(ExitSpeed), BatterSide == "Right") %>%
    dplyr::group_by(TaggedPitchType) %>%
    dplyr::summarise(avg_ev = mean(ExitSpeed, na.rm = TRUE), n_bip = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n_bip >= 3) %>%
    dplyr::arrange(dplyr::desc(avg_ev))
  
  grid::grid.text("Hardest:", x = 0.43, y = pill_y, just = "left",
                  gp = grid::gpar(fontsize = 6, fontface = "bold", col = "#006F71"))
  
  if (nrow(hardest_lhh) > 0) {
    pt_short <- get_pt_short(hardest_lhh$TaggedPitchType[1])
    grid::grid.roundrect(x = 0.49, y = pill_y, width = 0.055, height = 0.010,
                         r = unit(0.3, "snpc"), gp = grid::gpar(fill = "#E1463E", col = NA))
    grid::grid.text(paste0("vL ", pt_short, " ", sprintf("%.0f", hardest_lhh$avg_ev[1])),
                    x = 0.49, y = pill_y,
                    gp = grid::gpar(fontsize = 5, fontface = "bold", col = "white"))
  }
  if (nrow(hardest_rhh) > 0) {
    pt_short <- get_pt_short(hardest_rhh$TaggedPitchType[1])
    grid::grid.roundrect(x = 0.55, y = pill_y, width = 0.055, height = 0.010,
                         r = unit(0.3, "snpc"), gp = grid::gpar(fill = "#E1463E", col = NA))
    grid::grid.text(paste0("vR ", pt_short, " ", sprintf("%.0f", hardest_rhh$avg_ev[1])),
                    x = 0.55, y = pill_y,
                    gp = grid::gpar(fontsize = 5, fontface = "bold", col = "white"))
  }
  
  # ------------------------
  # Helpers: section header + row pills
  # ------------------------
  draw_section_header <- function(x_center, label, label_col) {
    col_x <- c(x_center - hm_width - hm_gap, x_center, x_center + hm_width + hm_gap)
    
    grid::grid.text(label, x = x_center, y = hm_section_y - 0.002,
                    gp = grid::gpar(fontsize = 7, fontface = "bold", col = label_col))
    
    pills <- list(list(fb_pill, "FB"), list(bb_pill, "BB"), list(os_pill, "OS"))
    for (i in 1:3) {
      grid::grid.roundrect(x = col_x[i], y = hm_section_y - 0.011, width = pill_w, height = pill_h,
                           r = unit(0.3, "snpc"), gp = grid::gpar(fill = pills[[i]][[1]], col = NA))
      grid::grid.text(pills[[i]][[2]], x = col_x[i], y = hm_section_y - 0.011,
                      gp = grid::gpar(fontsize = 5.5, fontface = "bold", col = "white"))
    }
    
    col_x
  }
  
  draw_row_pills <- function(first_col_x, labels, fills) {
    for (r in 1:3) {
      x_pill <- first_col_x - hm_width/2 - 0.014
      y_pill <- row_y[r] - hm_height/2
      grid::grid.roundrect(x = x_pill, y = y_pill, width = row_pill_w, height = row_pill_h,
                           r = unit(0.3, "snpc"), gp = grid::gpar(fill = fills[r], col = NA))
      grid::grid.text(labels[r], x = x_pill, y = y_pill,
                      gp = grid::gpar(fontsize = 5, fontface = "bold", col = "white"))
    }
  }
  
  # Define all section column positions
  sec1_col_x <- draw_section_header(sec1_center, "vs LHH", "#E53935")
  sec2_col_x <- draw_section_header(sec2_center, "vs RHH", "#1E88E5")
  sec3_col_x <- draw_section_header(sec3_center, "LHH by Count", "#E53935")
  sec4_col_x <- draw_section_header(sec4_center, "RHH by Count", "#1E88E5")
  
  # Draw row pills for each section
  draw_row_pills(sec1_col_x[1], row_labels_1, row_colors_1)
  draw_row_pills(sec2_col_x[1], row_labels_1, row_colors_1)
  draw_row_pills(sec3_col_x[1], row_labels_count, row_colors_count)
  draw_row_pills(sec4_col_x[1], row_labels_count, row_colors_count)
  
  # ------------------------
  # Count filters
  # ------------------------
  count_filters <- list(
    "1st"    = function(d) dplyr::filter(d, PitchofPA == 1),
    "Behind" = function(d) dplyr::filter(d, Balls > Strikes),
    "2K"     = function(d) dplyr::filter(d, Strikes == 2)
  )
  
  # Metric filters for whiff/damage
  metric_filters <- list(
    "All"    = function(d) d,
    "Whiff"  = function(d) dplyr::filter(d, PitchCall %in% c("StrikeSwinging", "SwingingStrike")),
    "Damage" = function(d) dplyr::filter(d, PitchCall == "InPlay", ExitSpeed >= 95)
  )
  
  # ------------------------
  # SECTION 1: vs LHH (All / Whiff / Damage)
  # ------------------------
  hm_metrics <- c("All", "Whiff", "Damage")
  hm_groups  <- c("FB", "BB", "OS")
  
  for (row_idx in 1:3) {
    for (col_idx in 1:3) {
      # Filter data for this cell
      cell_data <- pitch_df %>%
        dplyr::filter(
          TaggedPitchType %in% pitch_groups[[col_idx]],
          BatterSide == "Left",
          !is.na(PlateLocSide), 
          !is.na(PlateLocHeight)
        )
      
      # Apply metric filter
      cell_data <- metric_filters[[hm_metrics[row_idx]]](cell_data)
      
      # Create heatmap
      p <- create_density_heatmap(cell_data)
      
      grid::pushViewport(grid::viewport(
        x = sec1_col_x[col_idx], y = row_y[row_idx],
        width = hm_width, height = hm_height, just = c("center", "top")
      ))
      tryCatch(print(p, newpage = FALSE), error = function(e) NULL)
      grid::popViewport()
    }
  }
  
  # ------------------------
  # SECTION 2: vs RHH (All / Whiff / Damage)
  # ------------------------
  for (row_idx in 1:3) {
    for (col_idx in 1:3) {
      # Filter data for this cell
      cell_data <- pitch_df %>%
        dplyr::filter(
          TaggedPitchType %in% pitch_groups[[col_idx]],
          BatterSide == "Right",
          !is.na(PlateLocSide), 
          !is.na(PlateLocHeight)
        )
      
      # Apply metric filter
      cell_data <- metric_filters[[hm_metrics[row_idx]]](cell_data)
      
      # Create heatmap
      p <- create_density_heatmap(cell_data)
      
      grid::pushViewport(grid::viewport(
        x = sec2_col_x[col_idx], y = row_y[row_idx],
        width = hm_width, height = hm_height, just = c("center", "top")
      ))
      tryCatch(print(p, newpage = FALSE), error = function(e) NULL)
      grid::popViewport()
    }
  }
  
  # ------------------------
  # SECTION 3: LHH by Count (1st / Behind / 2K)
  # ------------------------
  for (row_idx in 1:3) {
    for (col_idx in 1:3) {
      # Filter data for this cell
      cell_data <- pitch_df %>%
        dplyr::filter(
          TaggedPitchType %in% pitch_groups[[col_idx]],
          BatterSide == "Left",
          !is.na(PlateLocSide), 
          !is.na(PlateLocHeight)
        )
      
      # Apply count filter
      cell_data <- count_filters[[row_keys_count[row_idx]]](cell_data)
      
      # Create heatmap
      p <- create_density_heatmap(cell_data)
      
      grid::pushViewport(grid::viewport(
        x = sec3_col_x[col_idx], y = row_y[row_idx],
        width = hm_width, height = hm_height, just = c("center", "top")
      ))
      tryCatch(print(p, newpage = FALSE), error = function(e) NULL)
      grid::popViewport()
    }
  }
  
  # ------------------------
  # SECTION 4: RHH by Count (1st / Behind / 2K)
  # ------------------------
  for (row_idx in 1:3) {
    for (col_idx in 1:3) {
      # Filter data for this cell
      cell_data <- pitch_df %>%
        dplyr::filter(
          TaggedPitchType %in% pitch_groups[[col_idx]],
          BatterSide == "Right",
          !is.na(PlateLocSide), 
          !is.na(PlateLocHeight)
        )
      
      # Apply count filter
      cell_data <- count_filters[[row_keys_count[row_idx]]](cell_data)
      
      # Create heatmap
      p <- create_density_heatmap(cell_data)
      
      grid::pushViewport(grid::viewport(
        x = sec4_col_x[col_idx], y = row_y[row_idx],
        width = hm_width, height = hm_height, just = c("center", "top")
      ))
      tryCatch(print(p, newpage = FALSE), error = function(e) NULL)
      grid::popViewport()
    }
  }
  
  # ===== SECTION 5: NITRO ZONES (smaller, far right) =====
  nitro_width <- 0.055
  nitro_height <- 0.060
  
  grid::grid.text("Nitro Zones", x = nitro_center, y = hm_section_y - 0.002,
                  gp = grid::gpar(fontsize = 8, fontface = "bold", col = "#E1463E"))
  grid::grid.text("(90th %ile EV)", x = nitro_center, y = hm_section_y - 0.010,
                  gp = grid::gpar(fontsize = 7, col = "#E1463E"))
  
  nitro_y_lhh <- hm_section_y - 0.025
  nitro_y_rhh <- hm_section_y - 0.095
  
  # Labels
  grid::grid.roundrect(x = nitro_center - nitro_width/2 - 0.012, y = nitro_y_lhh - nitro_height/2,
                       width = 0.020, height = 0.008,
                       r = unit(0.3, "snpc"), gp = grid::gpar(fill = "#E53935", col = NA))
  grid::grid.text("LHH", x = nitro_center - nitro_width/2 - 0.012, y = nitro_y_lhh - nitro_height/2,
                  gp = grid::gpar(fontsize = 5, fontface = "bold", col = "white"))
  
  grid::grid.roundrect(x = nitro_center - nitro_width/2 - 0.012, y = nitro_y_rhh - nitro_height/2,
                       width = 0.020, height = 0.008,
                       r = unit(0.3, "snpc"), gp = grid::gpar(fill = "#1E88E5", col = NA))
  grid::grid.text("RHH", x = nitro_center - nitro_width/2 - 0.012, y = nitro_y_rhh - nitro_height/2,
                  gp = grid::gpar(fontsize = 5, fontface = "bold", col = "white"))
  
  for (side in c("Left", "Right")) {
    nitro_data <- pitch_df %>%
      dplyr::filter(
        BatterSide == side, 
        PitchCall == "InPlay", 
        !is.na(ExitSpeed),
        !is.na(PlateLocSide), 
        !is.na(PlateLocHeight)
      )
    
    if (nrow(nitro_data) >= 8) {
      ev_90 <- quantile(nitro_data$ExitSpeed, 0.90, na.rm = TRUE)
      nitro_pts <- nitro_data %>% dplyr::filter(ExitSpeed >= ev_90)
      
      y_pos <- if (side == "Left") nitro_y_lhh else nitro_y_rhh
      
      p <- ggplot(nitro_pts, aes(x = PlateLocSide, y = PlateLocHeight)) +
        annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, 
                 fill = "#FFF5F5", color = "black", linewidth = 0.5) +
        annotate("path", x = c(-0.708, 0.708, 0.708, 0, -0.708, -0.708),
                 y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15), color = "black", linewidth = 0.4) +
        geom_point(aes(size = ExitSpeed), color = "#DC143C", alpha = 0.8) +
        scale_size_continuous(range = c(1.2, 3.5), guide = "none") +
        coord_fixed(xlim = c(-2, 2), ylim = c(0, 4.5)) +
        theme_void() + theme(plot.margin = margin(0,0,0,0))
      
      grid::pushViewport(grid::viewport(x = nitro_center, y = y_pos, 
                                        width = nitro_width, height = nitro_height, 
                                        just = c("center", "top")))
      tryCatch(print(p, newpage = FALSE), error = function(e) NULL)
      grid::popViewport()
    }
  }
  
  
  
  # ===== SECTION 5: NITRO ZONES (smaller, far right) =====
  nitro_width <- 0.055
  nitro_height <- 0.060
  
  grid::grid.text("Nitro Zones", x = nitro_center, y = hm_section_y - 0.002,
                  gp = grid::gpar(fontsize = 8, fontface = "bold", col = "#E1463E"))
  grid::grid.text("(90th %ile EV)", x = nitro_center, y = hm_section_y - 0.010,
                  gp = grid::gpar(fontsize = 7, col = "#E1463E"))
  
  nitro_y_lhh <- hm_section_y - 0.025
  nitro_y_rhh <- hm_section_y - 0.095
  
  # Labels
  grid::grid.roundrect(x = nitro_center - nitro_width/2 - 0.012, y = nitro_y_lhh - nitro_height/2,
                       width = 0.020, height = 0.008,
                       r = unit(0.3, "snpc"), gp = grid::gpar(fill = "#E53935", col = NA))
  grid::grid.text("LHH", x = nitro_center - nitro_width/2 - 0.012, y = nitro_y_lhh - nitro_height/2,
                  gp = grid::gpar(fontsize = 5, fontface = "bold", col = "white"))
  
  grid::grid.roundrect(x = nitro_center - nitro_width/2 - 0.012, y = nitro_y_rhh - nitro_height/2,
                       width = 0.020, height = 0.008,
                       r = unit(0.3, "snpc"), gp = grid::gpar(fill = "#1E88E5", col = NA))
  grid::grid.text("RHH", x = nitro_center - nitro_width/2 - 0.012, y = nitro_y_rhh - nitro_height/2,
                  gp = grid::gpar(fontsize = 5, fontface = "bold", col = "white"))
  
  for (side in c("Left", "Right")) {
    nitro_data <- pitch_df %>%
      filter(BatterSide == side, PitchCall == "InPlay", !is.na(ExitSpeed),
             !is.na(PlateLocSide), !is.na(PlateLocHeight))
    
    if (nrow(nitro_data) >= 8) {
      ev_90 <- quantile(nitro_data$ExitSpeed, 0.90, na.rm = TRUE)
      nitro_pts <- nitro_data %>% filter(ExitSpeed >= ev_90)
      
      y_pos <- if (side == "Left") nitro_y_lhh else nitro_y_rhh
      
      p <- ggplot(nitro_pts, aes(x = PlateLocSide, y = PlateLocHeight)) +
        annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, 
                 fill = "#FFF5F5", color = "black", linewidth = 0.5) +
        annotate("path", x = c(-0.708, 0.708, 0.708, 0, -0.708, -0.708),
                 y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15), color = "black", linewidth = 0.4) +
        geom_point(aes(size = ExitSpeed), color = "#DC143C", alpha = 0.8) +
        scale_size_continuous(range = c(1.2, 3.5), guide = "none") +
        coord_fixed(xlim = c(-2, 2), ylim = c(0, 4.5)) +
        theme_void() + theme(plot.margin = margin(0,0,0,0))
      
      grid::pushViewport(grid::viewport(x = nitro_center, y = y_pos, 
                                        width = nitro_width, height = nitro_height, 
                                        just = c("center", "top")))
      tryCatch(print(p, newpage = FALSE), error = function(e) NULL)
      grid::popViewport()
    }
  }
  
  
  # Horizontal divider before charts
  grid::grid.lines(x = c(0.02, 0.98), y = c(hm_section_y - 0.185, hm_section_y - 0.185),
                   gp = grid::gpar(col = "#006F71", lwd = 1.5))
  
  
  # ===== CHARTS SECTION: Inning Row + Stacked Velo Charts =====
  charts_y <- hm_section_y - 0.20
  chart_width <- 0.45
  
  fb_pitches <- c("Fastball", "Sinker", "FourSeamFastBall", "TwoSeamFastBall", "Four-Seam", "Two-Seam")
  
  # ===== PITCH COUNT BY INNING ROW (Left, top) =====
  inning_row_plot <- tryCatch(
    create_pitcher_inning_row(data, pitcher_name),
    error = function(e) ggplot() + theme_void()
  )
  
  grid::pushViewport(grid::viewport(x = 0.27, y = charts_y, width = chart_width, height = 0.025, just = c("center", "top")))
  tryCatch(print(inning_row_plot, newpage = FALSE), error = function(e) NULL)
  grid::popViewport()
  
  # ===== PITCH USAGE BY INNING STACKED BAR (Left, below inning row) =====
  inning_data <- pitcher_df %>%
    dplyr::filter(!is.na(Inning), !is.na(TaggedPitchType), TaggedPitchType != "Other") %>%
    dplyr::mutate(Inning = ifelse(Inning > 9, "X", as.character(Inning))) %>%
    dplyr::group_by(Inning, TaggedPitchType) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(Inning) %>%
    dplyr::mutate(pct = 100 * n / sum(n)) %>%
    dplyr::ungroup()
  
  if (nrow(inning_data) > 0) {
    all_innings <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "X")
    inning_data$Inning <- factor(inning_data$Inning, levels = all_innings)
    
    inning_plot <- ggplot(inning_data, aes(x = Inning, y = pct, fill = TaggedPitchType)) +
      geom_col(position = "stack", width = 0.8) +
      scale_fill_manual(values = pitch_colors, name = NULL) +
      labs(x = NULL, y = "Usage %", title = "Pitch Usage by Inning") +
      scale_y_continuous(expan= expansion(mult = c(0, 0.02))) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.key.size = unit(0.3, "cm"),
        legend.text = element_text(size = 5),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 9, color = "#006F71"),
        axis.text = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        panel.grid.minor = element_blank(),
        plot.margin = margin(2, 2, 2, 2)
      ) +
      guides(fill = guide_legend(nrow = 1))
    
    grid::pushViewport(grid::viewport(x = 0.27, y = charts_y - 0.028, width = chart_width, height = 0.065, just = c("center", "top")))
    tryCatch(print(inning_plot, newpage = FALSE), error = function(e) NULL)
    grid::popViewport()
  }
  
  # ===== FB VELO BY PITCH # IN GAME (Right, top) =====
  velo_by_pitch <- tryCatch(
    create_rolling_data_single(data, pitcher_name, "velo"),
    error = function(e) data.frame(pitch_num = integer(), value = numeric())
  )
  
  if (nrow(velo_by_pitch) > 0) {
    velo_pitch_plot <- ggplot(velo_by_pitch, aes(x = pitch_num, y = value)) +
      geom_line(color = "#006F71", linewidth = 1.2) +
      geom_point(color = "#006F71", size = 2) +
      geom_smooth(method = "loess", se = FALSE, color = "#E53935", linewidth = 0.8, linetype = "dashed", span = 0.75) +
      labs(x = "Pitch # in Game", y = "FB Velo (mph)", title = "FB Velo by Pitch Count") +
      scale_x_continuous(breaks = seq(0, 120, 20)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 9, color = "#006F71"),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        panel.grid.minor = element_blank(),
        plot.margin = margin(2, 2, 2, 2)
      )
    
    grid::pushViewport(grid::viewport(x = 0.73, y = charts_y, width = chart_width, height = 0.045, just = c("center", "top")))
    tryCatch(print(velo_pitch_plot, newpage = FALSE), error = function(e) NULL)
    grid::popViewport()
  }
  
  # ===== FB VELO BY MONTH (Right, bottom - stacked below pitch count velo) =====
  monthly_velo <- tryCatch(
    create_monthly_fb_velo(data, pitcher_name),
    error = function(e) data.frame(month_date = as.Date(character()), avg_velo = numeric())
  )
  
  if (nrow(monthly_velo) > 0) {
    monthly_velo_plot <- ggplot(monthly_velo, aes(x = month_date, y = avg_velo)) +
      geom_line(color = "#CD853F", linewidth = 1.2) +
      geom_point(color = "#CD853F", size = 2.5) +
      geom_text(aes(label = sprintf("%.1f", avg_velo)), vjust = -1, size = 2, fontface = "bold") +
      labs(x = NULL, y = "FB Velo (mph)", title = "Avg FB Velo by Month") +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 9, color = "#CD853F"),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        panel.grid.minor = element_blank(),
        plot.margin = margin(2, 2, 2, 2)
      )
    
    grid::pushViewport(grid::viewport(x = 0.73, y = charts_y - 0.048, width = chart_width, height = 0.045, just = c("center", "top")))
    tryCatch(print(monthly_velo_plot, newpage = FALSE), error = function(e) NULL)
    grid::popViewport()
  }
  
  # ===== MATCHUP MATRIX (if provided) =====
  if (!is.null(matchup_matrix) && !is.null(matchup_matrix$data) && nrow(matchup_matrix$data) > 0) {
    # Start new page for matchup matrix
    grid::grid.newpage()
    
    # Header
    grid::grid.rect(x = 0.5, y = 0.97, width = 1, height = 0.04,
                    gp = grid::gpar(fill = "#006F71", col = NA))
    grid::grid.text(paste0("Hitter Matchup Analysis: ", pitcher_name), x = 0.5, y = 0.97,
                    gp = grid::gpar(fontsize = 16, fontface = "bold", col = "white"))
    
    # Draw matrix table
    df <- matchup_matrix$data
    pitch_types <- matchup_matrix$pitch_types
    
    # Add notes if available
    if (!is.null(matchup_matrix$notes)) {
      for (h_name in names(matchup_matrix$notes)) {
        if (h_name %in% df$Hitter) {
          df$Notes[df$Hitter == h_name] <- matchup_matrix$notes[[h_name]]
        }
      }
    }
    
    # Table layout
    n_rows <- nrow(df)
    n_cols <- 4 + length(pitch_types) + 1  # Hitter, Hand, BA, SLG, pitch types, Notes
    
    table_top <- 0.92
    row_height <- min(0.025, 0.6 / (n_rows + 1))  # Header + data rows
    col_width <- 0.9 / n_cols
    start_x <- 0.05
    
    # Column headers
    headers <- c("Hitter", "H", "BA", "SLG", pitch_types, "Notes")
    for (j in seq_along(headers)) {
      x_pos <- start_x + (j - 0.5) * col_width
      grid::grid.rect(x = x_pos, y = table_top, width = col_width * 0.95, height = row_height,
                      gp = grid::gpar(fill = "#006F71", col = "white"))
      grid::grid.text(headers[j], x = x_pos, y = table_top,
                      gp = grid::gpar(fontsize = 8, fontface = "bold", col = "white"))
    }
    
    # Data rows
    for (i in 1:n_rows) {
      y_pos <- table_top - i * row_height
      row_bg <- if (i %% 2 == 0) "#f5f5f5" else "white"
      
      # Get row data
      hitter <- df$Hitter[i]
      hand <- df$Hand[i]
      ba <- if ("BA" %in% names(df)) df$BA[i] else "-"
      slg <- if ("SLG" %in% names(df)) df$SLG[i] else "-"
      notes <- if ("Notes" %in% names(df)) df$Notes[i] else ""
      
      row_values <- c(hitter, hand, ba, slg)
      for (pt in pitch_types) {
        rv_val <- if (pt %in% names(df)) df[[pt]][i] else "-"
        row_values <- c(row_values, rv_val)
      }
      row_values <- c(row_values, notes)
      
      for (j in seq_along(row_values)) {
        x_pos <- start_x + (j - 0.5) * col_width
        
        # Color coding for RV/100 columns
        cell_bg <- row_bg
        if (j > 4 && j <= (4 + length(pitch_types))) {
          val <- row_values[j]
          if (!is.na(val) && val != "-") {
            if (grepl("^\\+", val)) cell_bg <- "#D9EF8B"  # Positive = hitter advantage
            else if (grepl("^-", val)) cell_bg <- "#FDAE61"  # Negative = pitcher advantage
          }
        }
        
        grid::grid.rect(x = x_pos, y = y_pos, width = col_width * 0.95, height = row_height,
                        gp = grid::gpar(fill = cell_bg, col = "#ddd"))
        
        # Truncate long text
        display_text <- if (nchar(row_values[j]) > 20) paste0(substr(row_values[j], 1, 18), "...") else row_values[j]
        grid::grid.text(display_text, x = x_pos, y = y_pos,
                        gp = grid::gpar(fontsize = 7))
      }
    }
    
    # Legend
    grid::grid.text("RV/100: Green = Hitter Advantage | Orange = Pitcher Advantage | Based on similar pitches (velo/movement/spin)",
                    x = 0.5, y = table_top - (n_rows + 1.5) * row_height,
                    gp = grid::gpar(fontsize = 7, col = "gray50"))
  }
  
  # ===== FOOTER =====
  grid::grid.text("Data: TrackMan | Coastal Carolina Baseball", x = 0.5, y = 0.01,
                  gp = grid::gpar(fontsize = 8, col = "gray50"))
  
  invisible(output_file)
}



# ============================================================
# ============  TEAM COUNT REPORT (multi-pitcher)  ===========
# Ported from the Advance Pitcher app: one row per pitcher,
# one column per ball-strike count, LHH/RHH heatmaps + usage
# pies, notes box per pitcher. Drives the Team Report tab.
# ============================================================





# ============================================================================
