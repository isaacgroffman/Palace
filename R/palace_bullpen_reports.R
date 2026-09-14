# =============================================================================
# R/palace_bullpen_reports.R — bullpen PDF report (landscape one-pager) and
# the Practice tab's multi-report ZIP downloads for bullpens and BP.
#
# Report code below is the user's; calculate_bullpen_summary was renamed
# calculate_bullpen_report_summary because R/palace_bullpen.R already owns
# that name for the in-app gt table. bpr_frame() adapts the Supabase bullpen
# frame (sb_load_practice_pitches) to what the report expects.
# =============================================================================
if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

parse_flexible_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  x <- as.character(x); out <- as.Date(rep(NA_integer_, length(x)))
  for (fmt in c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y", "%Y/%m/%d", "%d-%m-%Y")) {
    miss <- is.na(out); if (!any(miss)) break
    out[miss] <- suppressWarnings(as.Date(substr(x[miss], 1, 10), format = fmt))
  }
  out
}
# Supabase bullpen frame -> report frame. Adds the columns the report reads
# (PitchUID, spin-axis 3d fields; the measured tilt falls back to SpinAxis
# when the 3d transverse angle was not stored) and keeps Pitcher as
# "Last, First" for matching; process_bullpen_dataset flips it for display.
bpr_frame <- function(df) {
  if (is.null(df) || !nrow(df)) return(NULL)
  if (!"PitchUID" %in% names(df)) df$PitchUID <- if ("pitch_uid" %in% names(df)) df$pitch_uid else df$PlayID
  if (!"SpinAxis3dTransverseAngle" %in% names(df)) df$SpinAxis3dTransverseAngle <- NA_real_
  if ("SpinAxis" %in% names(df)) df$SpinAxis3dTransverseAngle <- ifelse(is.na(df$SpinAxis3dTransverseAngle), df$SpinAxis, df$SpinAxis3dTransverseAngle)
  df$Date <- as.Date(df$Date)
  process_bullpen_dataset(df)
}
bpr_display_name <- function(x) sub("^\\s*(\\w+)\\s*,\\s*(\\w+)\\s*$", "\\2 \\1", as.character(x))

# One ZIP, one PDF per player. `builders` is a named list: file name -> function(path).
palace_zip_reports <- function(zipfile, builders) {
  dir <- tempfile("reports_"); dir.create(dir)
  made <- character(0)
  for (nm in names(builders)) {
    f <- file.path(dir, nm)
    ok <- tryCatch({ builders[[nm]](f); file.exists(f) }, error = function(e) { cat("[reports]", nm, "failed:", conditionMessage(e), "\n"); FALSE })
    if (isTRUE(ok)) made <- c(made, nm)
  }
  if (!length(made)) stop("no reports could be built")
  if (requireNamespace("zip", quietly = TRUE)) zip::zip(zipfile, files = made, root = dir, mode = "cherry-pick")
  else { old <- setwd(dir); on.exit(setwd(old), add = TRUE); utils::zip(zipfile, files = made, flags = "-q") }
  invisible(zipfile)
}
safe_file_name <- function(x) gsub("[^A-Za-z0-9]+", "_", x)

# ---- report code ----------------------------------------------------------------------
angle_to_clock <- function(angle) {
  if (is.na(angle) || !is.finite(angle)) return("-")
  angle <- angle %% 360
  total_minutes <- (angle / 360) * 720
  hours <- floor(total_minutes / 60)
  minutes <- round(total_minutes %% 60)
  if (minutes == 60) { hours <- hours + 1; minutes <- 0 }
  hours <- hours %% 12
  if (hours == 0) hours <- 12
  sprintf("%d:%02d", hours, minutes)
}
# Process bullpen CSV data
process_bullpen_dataset <- function(df) {
  if ("Pitcher" %in% names(df)) {
    df <- df %>% mutate(Pitcher = stringr::str_replace(
      Pitcher, "^\\s*(\\w+)\\s*,\\s*(\\w+)\\s*$", "\\2 \\1"))
  }
  
  df <- df %>% distinct()
  if ("PitchUID" %in% names(df)) df <- df %>% distinct(PitchUID, .keep_all = TRUE)
  
  if ("Date" %in% names(df)) df$Date <- parse_flexible_date(df$Date)
  
  num_cols <- c("PlateLocSide", "PlateLocHeight", "RelSpeed", "SpinRate", 
                "InducedVertBreak", "HorzBreak", "RelHeight", "RelSide", 
                "Extension", "VertApprAngle", "HorzApprAngle",
                "SpinAxis", "SpinAxis3dTransverseAngle", 
                "SpinAxis3dLongitudinalAngle", "SpinAxis3dActiveSpinRate",
                "SpinAxis3dSpinEfficiency")
  for (col in num_cols) {
    if (col %in% names(df)) df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  
  if ("RelSpeed" %in% names(df)) df <- df %>% filter(!is.na(RelSpeed))
  
  if (!"TaggedPitchType" %in% names(df)) {
    alt <- intersect(c("pitch_type", "PitchType", "TaggedPitch", "AutoPitchType"), names(df))
    if (length(alt)) df$TaggedPitchType <- df[[alt[1]]] else df$TaggedPitchType <- NA_character_
  }
  
  df <- df %>%
    mutate(TaggedPitchType = case_when(
      TaggedPitchType %in% c("FourSeamFastball", "FourSeamFastBall", "4-Seam",
                             "Four-Seam Fastball", "4-Seam Fastball") ~ "Four-Seam",
      TRUE ~ TaggedPitchType
    ))
  
  # Ensure PitchCall exists (bullpen CSVs often have it empty or missing entirely)
  if (!"PitchCall" %in% names(df)) df$PitchCall <- NA_character_
  df$PitchCall[is.na(df$PitchCall) | df$PitchCall == ""] <- "Unknown"
  
  # Ensure SpinAxis columns exist (create as NA if missing)
  if (!"SpinAxis" %in% names(df)) df$SpinAxis <- NA_real_
  if (!"SpinAxis3dTransverseAngle" %in% names(df)) df$SpinAxis3dTransverseAngle <- NA_real_
  if (!"SpinAxis3dLongitudinalAngle" %in% names(df)) df$SpinAxis3dLongitudinalAngle <- NA_real_
  if (!"SpinAxis3dActiveSpinRate" %in% names(df)) df$SpinAxis3dActiveSpinRate <- NA_real_
  if (!"SpinAxis3dSpinEfficiency" %in% names(df)) df$SpinAxis3dSpinEfficiency <- NA_real_
  
  # Zone indicator only — no game-level stats needed for bullpen
  df <- df %>%
    mutate(
      in_zone = as.integer(
        !is.na(PlateLocSide) & !is.na(PlateLocHeight) &
          PlateLocSide >= -0.83 & PlateLocSide <= 0.83 &
          PlateLocHeight >= 1.5 & PlateLocHeight <= 3.38
      )
    )
  
  df
}
# Calculate bullpen summary table
calculate_bullpen_report_summary <- function(pitcher_df) {
  if (!"VertApprAngle" %in% names(pitcher_df)) pitcher_df$VertApprAngle <- NA_real_
  if (!"HorzApprAngle" %in% names(pitcher_df)) pitcher_df$HorzApprAngle <- NA_real_
  types <- pitcher_df %>%
    filter(!is.na(TaggedPitchType), TaggedPitchType != "", 
           TaggedPitchType != "Undefined", TaggedPitchType != "Other") %>%
    pull(TaggedPitchType) %>% unique()
  
  if (length(types) == 0) types <- unique(pitcher_df$TaggedPitchType)
  
  measured_tilt <- pitcher_df %>%
    mutate(
      SpinAxis3dTransverseAngle = SpinAxis3dTransverseAngle + 180,
      SpinAxis3dTransverseAngle = ifelse(SpinAxis3dTransverseAngle > 360, 
                                         SpinAxis3dTransverseAngle - 360, 
                                         SpinAxis3dTransverseAngle)
    ) %>%
    filter(TaggedPitchType %in% types) %>%
    group_by(TaggedPitchType) %>%
    summarise(meas_axis = round(median(SpinAxis3dTransverseAngle, na.rm = TRUE), 1), .groups = "drop") %>%
    mutate(`Meas Tilt` = sapply(meas_axis, angle_to_clock)) %>%
    select(TaggedPitchType, `Meas Tilt`)
  
  summary_df <- pitcher_df %>%
    filter(TaggedPitchType %in% types) %>%
    group_by(TaggedPitchType) %>%
    summarise(
      `#` = n(),
      Velo = round(mean(RelSpeed, na.rm = TRUE), 1),
      `Max Velo` = round(max(RelSpeed, na.rm = TRUE), 1),
      IVB = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      HB = round(mean(HorzBreak, na.rm = TRUE), 1),
      `Total Spin` = round(mean(SpinRate, na.rm = TRUE), 0),
      `Spin Eff%` = round(100 * mean(SpinAxis3dSpinEfficiency, na.rm = TRUE), 0),
      `VAA` = round(mean(VertApprAngle, na.rm = TRUE), 1),
      `HAA` = round(mean(HorzApprAngle, na.rm = TRUE), 1),
      RelH = round(mean(RelHeight, na.rm = TRUE), 2),
      RelS = round(mean(RelSide, na.rm = TRUE), 2),
      Ext = round(mean(Extension, na.rm = TRUE), 2),
      `Zone%` = round(100 * mean(in_zone, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    rename(Pitch = TaggedPitchType) %>%
    arrange(desc(`#`))
  
  summary_df <- summary_df %>%
    left_join(measured_tilt, by = c("Pitch" = "TaggedPitchType"))
  
  summary_df <- summary_df %>%
    select(Pitch, `#`, Velo, `Max Velo`, IVB, HB, `Total Spin`,
           `Spin Eff%`, `VAA`, `HAA`, `Meas Tilt`, RelH, RelS, Ext, `Zone%`)
  
  # Add totals row
  all_data <- pitcher_df %>% filter(TaggedPitchType %in% types)
  
  meas_raw <- all_data$SpinAxis3dTransverseAngle + 180
  meas_raw <- ifelse(meas_raw > 360, meas_raw - 360, meas_raw)
  meas_axis_all <- round(median(meas_raw, na.rm = TRUE), 1)
  
  totals_row <- data.frame(
    Pitch = "Total",
    `#` = sum(summary_df$`#`, na.rm = TRUE),
    Velo = round(mean(all_data$RelSpeed, na.rm = TRUE), 1),
    `Max Velo` = round(max(all_data$RelSpeed, na.rm = TRUE), 1),
    IVB = round(mean(all_data$InducedVertBreak, na.rm = TRUE), 1),
    HB = round(mean(all_data$HorzBreak, na.rm = TRUE), 1),
    `Total Spin` = round(mean(all_data$SpinRate, na.rm = TRUE), 0),
    `Spin Eff%` = round(100 * mean(all_data$SpinAxis3dSpinEfficiency, na.rm = TRUE), 0),
    `VAA` = round(mean(all_data$VertApprAngle, na.rm = TRUE), 1),
    `HAA` = round(mean(all_data$HorzApprAngle, na.rm = TRUE), 1),
    `Meas Tilt` = angle_to_clock(meas_axis_all),
    RelH = round(mean(all_data$RelHeight, na.rm = TRUE), 2),
    RelS = round(mean(all_data$RelSide, na.rm = TRUE), 2),
    Ext = round(mean(all_data$Extension, na.rm = TRUE), 2),
    `Zone%` = round(100 * mean(all_data$in_zone, na.rm = TRUE), 1),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  summary_df <- dplyr::bind_rows(summary_df, totals_row)
  
  summary_df
}
# Bullpen location plot (no shape manual - simplified)
create_bullpen_location_plot <- function(pitcher_df, pitch_colors) {
  filter_types <- pitcher_df %>%
    filter(!is.na(TaggedPitchType), TaggedPitchType != "", 
           TaggedPitchType != "Undefined", TaggedPitchType != "Other") %>%
    pull(TaggedPitchType) %>% unique()
  
  df <- pitcher_df %>%
    filter(TaggedPitchType %in% filter_types,
           !is.na(PlateLocSide), !is.na(PlateLocHeight))
  
  if (nrow(df) == 0) {
    return(ggplot() + theme_void() + labs(title = "Pitch Locations") +
             theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5)))
  }
  
  zone_left <- -0.8333; zone_right <- 0.8333
  zone_bottom <- 1.5; zone_top <- 3.5
  shadow_left <- -1.1; shadow_right <- 1.1
  shadow_bottom <- 1.2; shadow_top <- 3.8
  zone_width <- (zone_right - zone_left) / 3
  zone_height <- (zone_top - zone_bottom) / 3
  
  ggplot(df, aes(x = PlateLocSide, y = PlateLocHeight)) +
    annotate("rect", xmin = shadow_left, xmax = shadow_right,
             ymin = shadow_bottom, ymax = shadow_top,
             fill = NA, color = "gray30", linetype = "dashed", linewidth = 0.5) +
    annotate("rect", xmin = zone_left, xmax = zone_right,
             ymin = zone_bottom, ymax = zone_top,
             fill = NA, color = "#E74C3C", linewidth = 1) +
    annotate("segment", x = zone_left + zone_width, xend = zone_left + zone_width,
             y = zone_bottom, yend = zone_top, color = "gray50", linetype = "dashed", linewidth = 0.3) +
    annotate("segment", x = zone_left + 2*zone_width, xend = zone_left + 2*zone_width,
             y = zone_bottom, yend = zone_top, color = "gray50", linetype = "dashed", linewidth = 0.3) +
    annotate("segment", x = zone_left, xend = zone_right,
             y = zone_bottom + zone_height, yend = zone_bottom + zone_height,
             color = "gray50", linetype = "dashed", linewidth = 0.3) +
    annotate("segment", x = zone_left, xend = zone_right,
             y = zone_bottom + 2*zone_height, yend = zone_bottom + 2*zone_height,
             color = "gray50", linetype = "dashed", linewidth = 0.3) +
    annotate("polygon", x = c(-0.708, 0.708, 0.708, 0, -0.708),
             y = c(0.15, 0.15, 0.30, 0.50, 0.30), fill = NA, color = "black", linewidth = 0.5) +
    geom_point(aes(fill = TaggedPitchType), size = 3, shape = 21, 
               color = "black", stroke = 0.5, alpha = 0.85) +
    scale_fill_manual(values = pitch_colors, name = "Pitch") +
    coord_fixed(xlim = c(-2.2, 2.2), ylim = c(0, 4.2)) +
    labs(title = "Pitch Locations") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 7),
      axis.text = element_blank(), axis.title = element_blank(),
      axis.ticks = element_blank(), panel.grid = element_blank(),
      plot.margin = margin(2, 2, 2, 2)
    )
}

# Bullpen movement plot
create_bullpen_movement_plot <- function(pitcher_df, pitcher_name, pitch_colors) {
  df <- pitcher_df %>% filter(!is.na(TaggedPitchType), 
                              TaggedPitchType != "Other",
                              TaggedPitchType != "Undefined")
  if (nrow(df) == 0) return(ggplot() + theme_void() + ggtitle("Pitch Movement"))
  
  centers <- df %>% group_by(TaggedPitchType) %>%
    summarise(
      mean_velo = round(mean(RelSpeed, na.rm = TRUE)),
      mean_hb = median(HorzBreak, na.rm = TRUE),
      mean_ivb = median(InducedVertBreak, na.rm = TRUE), 
      .groups = "drop"
    )
  
  ggplot(df, aes(x = HorzBreak, y = InducedVertBreak)) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_point(aes(fill = TaggedPitchType), alpha = 0.85, shape = 21, 
               color = "black", stroke = 0.4, size = 4.5) +
    geom_point(data = centers, aes(x = mean_hb, y = mean_ivb, fill = TaggedPitchType),
               alpha = 1, shape = 21, color = "black", stroke = 0.5, size = 4) +
    geom_text(data = centers, aes(x = mean_hb, y = mean_ivb, label = mean_velo),
              color = "black", size = 2, vjust = 0.5, fontface = "bold") +
    scale_fill_manual(values = pitch_colors) +
    coord_fixed(ratio = 1, xlim = c(-27.5, 27.5), ylim = c(-27.5, 27.5)) +
    labs(title = "Movement Profile", x = "Horizontal Break (in)", y = "Induced Vertical Break (in)") +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      plot.margin = margin(4, 4, 4, 4)
    )
}

# Bullpen release point plot
create_bullpen_release_plot <- function(pitcher_df, pitcher_name, pitch_colors) {
  df <- pitcher_df %>%
    filter(!is.na(RelSide), !is.na(RelHeight),
           !is.na(TaggedPitchType), TaggedPitchType != "Other",
           TaggedPitchType != "Undefined")
  
  if (nrow(df) == 0) {
    return(ggplot() + theme_void() + ggtitle("Release Points") +
             theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5)))
  }
  
  avg_release <- df %>%
    group_by(TaggedPitchType) %>%
    summarise(RelSide = mean(RelSide, na.rm = TRUE),
              RelHeight = mean(RelHeight, na.rm = TRUE), .groups = "drop")
  
  ggplot() +
    geom_point(data = df, aes(RelSide, RelHeight, fill = TaggedPitchType),
               size = 2, shape = 21, color = "black", alpha = 0.85, stroke = 0.25) +
    geom_point(data = avg_release, aes(RelSide, RelHeight, fill = TaggedPitchType),
               size = 3, shape = 21, color = "black", stroke = 0.3, alpha = 1) +
    annotate("text", x = -5, y = 8, label = "← 1B", size = 3, hjust = 0) +
    annotate("text", x = 5, y = 8, label = "3B →", size = 3, hjust = 1) +
    geom_rect(aes(xmin = -5, xmax = 5, ymin = 0, ymax = 0.83), 
              fill = "#632b11", inherit.aes = FALSE) +
    geom_rect(aes(xmin = -0.5, xmax = 0.5, ymin = 0.8, ymax = 0.95),
              fill = "white", color = "black", linewidth = 0.4, inherit.aes = FALSE) +
    scale_fill_manual(values = pitch_colors, name = "Pitch Type") +
    coord_fixed(ratio = 1, xlim = c(-4, 4), ylim = c(0, 8), clip = "off") +
    labs(title = "Release Points", x = "Release Side (ft)", y = "Release Height (ft)") +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.position = "none",
      plot.margin = margin(4, 4, 4, 4)
    )
}

# Main bullpen PDF generator (LANDSCAPE)
create_bullpen_pdf_report <- function(bp_data, pitcher_name, output_file) {
  if (length(dev.list()) > 0) try(dev.off(), silent = TRUE)
  
  pitch_colors <- c(
    "Fastball" = "#3465cb", "Four-Seam" = "#3465cb", "FourSeamFastBall" = "#3465cb",
    "4-Seam Fastball" = "#3465cb", "FF" = "#3465cb",
    "Sinker" = "#e5e501", "TwoSeamFastBall" = "#e5e501", "Two-Seam" = "#e5e501",
    "2-Seam Fastball" = "#e5e501", "SI" = "#e5e501",
    "Slider" = "#65aa02", "SL" = "#65aa02",
    "Sweeper" = "#dc4476", "SW" = "#dc4476",
    "Curveball" = "#d73813", "CB" = "#d73813", "Knuckle Curve" = "#d73813", "KC" = "#d73813",
    "ChangeUp" = "#980099", "Changeup" = "#980099", "CH" = "#980099",
    "Splitter" = "#23a999", "FS" = "#23a999", "SP" = "#23a999",
    "Cutter" = "#ff9903", "FC" = "#ff9903",
    "Slurve" = "#9370DB",
    "Other" = "gray50"
  )
  
  pitcher_df <- bp_data %>% filter(Pitcher == pitcher_name)
  
  if (nrow(pitcher_df) == 0) {
    pdf(output_file, width = 11, height = 7)
    grid::grid.newpage()
    grid::grid.text(paste("No data found for", pitcher_name),
                    gp = grid::gpar(fontsize = 16, fontface = "bold"))
    dev.off()
    return(output_file)
  }
  
  game_date <- tryCatch({
    d <- unique(pitcher_df$Date)[1]
    parsed <- parse_flexible_date(d)
    if (!is.na(parsed)) format(parsed, "%m/%d/%Y") else "N/A"
  }, error = function(e) "N/A")
  
  summary_table <- calculate_bullpen_report_summary(pitcher_df)
  loc_plot <- create_bullpen_location_plot(pitcher_df, pitch_colors)
  mov_plot <- create_bullpen_movement_plot(pitcher_df, pitcher_name, pitch_colors)
  rel_plot <- create_bullpen_release_plot(pitcher_df, pitcher_name, pitch_colors)
  
  # LANDSCAPE PDF - compact height
  pdf(output_file, width = 11, height = 7)
  on.exit(try(dev.off(), silent = TRUE), add = TRUE)
  grid::grid.newpage()
  
  # ===== HEADER BAR =====
  grid::grid.rect(x = 0, y = 0.945, width = 1, height = 0.055,
                  just = c("left", "bottom"),
                  gp = grid::gpar(fill = "#006F71", col = NA))
  
  grid::grid.text("Bullpen Report", x = 0.02, y = 0.972, just = "left",
                  gp = grid::gpar(col = "white", fontface = "bold", cex = 1.2))
  
  # Logo
  try({
    logo_img <- magick::image_read("https://i.imgur.com/zjTu3JS.png")
    logo_img <- magick::image_resize(logo_img, "x140")
    logo_grob <- grid::rasterGrob(as.raster(logo_img), interpolate = TRUE)
    grid::pushViewport(grid::viewport(x = 0.988, y = 0.972,
                                      width = 0.10, height = 0.048,
                                      just = c("right", "center")))
    grid::grid.draw(logo_grob)
    grid::popViewport()
  }, silent = TRUE)
  
  # ===== INFO LINE =====
  grid::grid.text(paste0(game_date, "   |   ", pitcher_name), 
                  x = 0.02, y = 0.92, just = "left",
                  gp = grid::gpar(cex = 0.85, fontface = "bold", col = "black"))
  
  # ===== SUMMARY TABLE =====
  if (nrow(summary_table) > 0) {
    headers <- names(summary_table)
    num_cols <- length(headers)
    
    pitch_w <- 0.08
    remaining_w <- (0.96 - pitch_w) / (num_cols - 1)
    col_widths <- c(pitch_w, rep(remaining_w, num_cols - 1))
    
    x_start <- 0.5 - sum(col_widths) / 2
    x_pos <- c(x_start, x_start + cumsum(col_widths[-length(col_widths)]))
    
    row_h <- 0.038
    y_top <- 0.89
    header_cex <- 0.65
    cell_cex <- 0.65
    
    # Draw headers
    for (i in seq_along(headers)) {
      grid::grid.rect(x = x_pos[i], y = y_top, width = col_widths[i] * 0.985, height = row_h,
                      just = c("left", "top"),
                      gp = grid::gpar(fill = "#006F71", col = "black", lwd = 0.5))
      grid::grid.text(headers[i],
                      x = x_pos[i] + col_widths[i] * 0.49, y = y_top - row_h * 0.5,
                      gp = grid::gpar(col = "white", cex = header_cex, fontface = "bold"))
    }
    
    # Draw data rows
    for (r in seq_len(nrow(summary_table))) {
      y_row <- y_top - r * row_h
      is_totals <- (r == nrow(summary_table) && summary_table$Pitch[r] == "Total")
      
      for (i in seq_along(headers)) {
        val <- as.character(summary_table[[i]][r])
        if (is.na(val) || val == "NaN" || val == "Inf" || val == "-Inf") val <- "-"
        
        bg <- ifelse(r %% 2 == 0, "#f7f7f7", "white")
        txt_col <- "black"
        font_face <- "plain"
        
        # Totals row styling
        if (is_totals) {
          bg <- "#e0e0e0"
          font_face <- "bold"
        }
        
        # Pitch column coloring
        if (i == 1 && !is_totals) {
          pitch_name <- summary_table$Pitch[r]
          if (!is.na(pitch_name) && pitch_name %in% names(pitch_colors)) {
            bg <- pitch_colors[[pitch_name]]
            rgb_vals <- grDevices::col2rgb(bg) / 255
            luminance <- 0.2126 * rgb_vals[1] + 0.7152 * rgb_vals[2] + 0.0722 * rgb_vals[3]
            txt_col <- ifelse(luminance < 0.5, "white", "black")
          }
          font_face <- "bold"
        } else if (i == 1 && is_totals) {
          font_face <- "bold"
        }
        
        grid::grid.rect(x = x_pos[i], y = y_row, width = col_widths[i] * 0.985, height = row_h,
                        just = c("left", "top"),
                        gp = grid::gpar(fill = bg, col = "grey80", lwd = 0.3))
        grid::grid.text(val,
                        x = x_pos[i] + col_widths[i] * 0.49, y = y_row - row_h * 0.5,
                        gp = grid::gpar(cex = cell_cex, col = txt_col, fontface = font_face))
      }
    }
    
    table_bottom <- y_top - (nrow(summary_table) + 1) * row_h
  } else {
    table_bottom <- 0.85
  }
  
  # ===== THREE CHARTS =====
  chart_top <- table_bottom - 0.025
  chart_h <- min(0.42, chart_top - 0.05)  # cap height so charts don't stretch
  chart_w <- 0.32
  
  grid::pushViewport(grid::viewport(x = 0.18, y = chart_top, width = chart_w, height = chart_h, 
                                    just = c("center", "top")))
  tryCatch(print(loc_plot, newpage = FALSE), error = function(e) NULL)
  grid::popViewport()
  
  grid::pushViewport(grid::viewport(x = 0.50, y = chart_top, width = chart_w, height = chart_h, 
                                    just = c("center", "top")))
  tryCatch(print(mov_plot, newpage = FALSE), error = function(e) NULL)
  grid::popViewport()
  
  grid::pushViewport(grid::viewport(x = 0.82, y = chart_top, width = chart_w, height = chart_h, 
                                    just = c("center", "top")))
  tryCatch(print(rel_plot, newpage = FALSE), error = function(e) NULL)
  grid::popViewport()
  
  grid::grid.text("Data: TrackMan | Report Generated: Coastal Carolina Baseball Analytics",
                  x = 0.5, y = 0.02, gp = grid::gpar(cex = 0.65, col = "grey50"))
  
  invisible(output_file)
}

# ---- Practice tab: multi-report downloads -------------------------------------------------
# Shared window controls: date range + presets + player picker; `data()` is the
# full frame with Date and a `who` column ("Last, First").
.prep_window_ui <- function(p, dates, who_label, who_choices, sel_range = NULL) {
  last <- max(dates); r <- sel_range %||% c(max(min(dates), last - 6), last)
  tagList(
    fluidRow(
      column(3, dateRangeInput(p("range"), "Window", start = r[1], end = r[2], min = min(dates), max = last)),
      column(4, div(style = "padding-top:25px; display:flex; gap:6px; flex-wrap:wrap;",
                    actionButton(p("last_session"), "Last session"), actionButton(p("last_week"), "Last 7 days"), actionButton(p("all"), "All"))),
      column(5, selectizeInput(p("who"), who_label, choices = who_choices, multiple = TRUE, options = list(placeholder = "Everyone in the window")))),
    uiOutput(p("summary")))
}
.prep_presets <- function(input, session, p, data) {
  observeEvent(input[[p("last_session")]], { d <- data(); req(d); updateDateRangeInput(session, p("range"), start = max(d$Date), end = max(d$Date)) })
  observeEvent(input[[p("last_week")]], { d <- data(); req(d); updateDateRangeInput(session, p("range"), start = max(min(d$Date), max(d$Date) - 6), end = max(d$Date)) })
  observeEvent(input[[p("all")]], { d <- data(); req(d); updateDateRangeInput(session, p("range"), start = min(d$Date), end = max(d$Date)) })
}

palace_bullpen_reports_ui <- function(prefix = "bprep") {
  p <- function(id) paste0(prefix, "_", id)
  tagList(div(style = "margin-top:12px;"), uiOutput(p("controls")),
          div(style = "font-size:12px; color:#666; margin:4px 0 8px;", "One landscape bullpen report per pitcher (summary table, locations, movement, release points), zipped. Pick a window; leave the pitcher list empty for everyone who threw."),
          DT::DTOutput(p("table")),
          div(style = "margin-top:10px;", downloadButton(p("dl_zip"), "Download reports (ZIP, one PDF per pitcher)")))
}
palace_bullpen_reports_server <- function(input, output, session, prefix = "bprep") {
  p <- function(id) paste0(prefix, "_", id)
  staff <- bp_staff_data(session)
  data <- reactive({ d <- staff(); if (is.null(d) || !nrow(d)) NULL else { d$who <- d$Pitcher; d } })
  output[[p("controls")]] <- renderUI({
    d <- data(); if (is.null(d)) return(div(class = "lb-empty", "No bullpen sessions in the database yet."))
    .prep_window_ui(p, d$Date, "Pitchers", sort(unique(d$who)), isolate(input[[p("range")]]))
  })
  .prep_presets(input, session, p, data)
  wd <- reactive({ d <- data(); r <- input[[p("range")]]; req(d, length(r) == 2)
    d <- d[d$Date >= r[1] & d$Date <= r[2], , drop = FALSE]
    w <- input[[p("who")]]; if (length(w)) d <- d[d$who %in% w, , drop = FALSE]; d })
  rows <- reactive({ d <- wd(); if (!nrow(d)) return(data.frame())
    d %>% dplyr::group_by(Pitcher) %>% dplyr::summarise(Sessions = dplyr::n_distinct(session_id), Pitches = dplyr::n(),
      `First` = format(min(Date), "%b %d"), `Last` = format(max(Date), "%b %d"), `Avg Velo` = round(mean(RelSpeed, na.rm = TRUE), 1),
      `Max Velo` = round(suppressWarnings(max(RelSpeed, na.rm = TRUE)), 1), .groups = "drop") %>% dplyr::arrange(Pitcher) })
  output[[p("summary")]] <- renderUI({ r <- rows(); div(style = "font-size:12.5px; color:#374151;", sprintf("%d pitcher(s), %s pitches in the window.", nrow(r), format(sum(r$Pitches), big.mark = ","))) })
  output[[p("table")]] <- DT::renderDT({ r <- rows(); validate(need(nrow(r) > 0, "No bullpens in that window."))
    DT::datatable(r, rownames = FALSE, selection = "none", class = "compact stripe", options = list(dom = "tp", pageLength = 25)) })
  output[[p("dl_zip")]] <- downloadHandler(
    filename = function() sprintf("bullpen_reports_%s_to_%s.zip", input[[p("range")]][1], input[[p("range")]][2]),
    content = function(file) {
      d <- wd(); validate(need(nrow(d) > 0, "No bullpens in that window"))
      rep <- bpr_frame(d); pitchers <- sort(unique(rep$Pitcher))
      builders <- stats::setNames(lapply(pitchers, function(nm) function(path) create_bullpen_pdf_report(rep, nm, path)),
                                  sprintf("Bullpen_%s_%s.pdf", safe_file_name(pitchers), format(max(d$Date))))
      palace_zip_reports(file, builders)
    })
}

palace_bp_reports_ui <- function(prefix = "bphrep") {
  p <- function(id) paste0(prefix, "_", id)
  tagList(div(style = "margin-top:12px;"), uiOutput(p("controls")),
          div(style = "font-size:12px; color:#666; margin:4px 0 8px;", "One BP report per hitter (switch hitters get a page per side; windows spanning several dates add the exit-velo trends pages), zipped. Leave the hitter list empty for everyone in the window."),
          DT::DTOutput(p("table")),
          div(style = "margin-top:10px; display:flex; gap:8px; flex-wrap:wrap;",
              downloadButton(p("dl_zip"), "Download reports (ZIP, one PDF per hitter)"),
              downloadButton(p("dl_lb"), "Leaderboard (PDF)")))
}
palace_bp_reports_server <- function(input, output, session, prefix = "bphrep") {
  p <- function(id) paste0(prefix, "_", id)
  all_data <- bph_staff_data(session)
  data <- reactive({ d <- all_data(); if (is.null(d) || !nrow(d)) NULL else { d$who <- d$Batter; d } })
  output[[p("controls")]] <- renderUI({
    d <- data(); if (is.null(d)) return(bph_empty())
    .prep_window_ui(p, d$Date, "Hitters", sort(unique(d$who)), isolate(input[[p("range")]]))
  })
  .prep_presets(input, session, p, data)
  wd <- reactive({ d <- data(); r <- input[[p("range")]]; req(d, length(r) == 2)
    d <- d[d$Date >= r[1] & d$Date <= r[2], , drop = FALSE]
    w <- input[[p("who")]]; if (length(w)) d <- d[d$who %in% w, , drop = FALSE]; d })
  rows <- reactive({ d <- wd(); if (!nrow(d)) return(data.frame())
    d %>% dplyr::group_by(Batter) %>% dplyr::group_modify(~ bph_stats(.x)) %>% dplyr::ungroup() %>%
      dplyr::mutate(Sessions = vapply(Batter, function(b) dplyr::n_distinct(d$session_id[d$Batter == b]), integer(1)), .after = "Batter") %>% dplyr::arrange(Batter) })
  output[[p("summary")]] <- renderUI({ r <- rows(); div(style = "font-size:12.5px; color:#374151;", sprintf("%d hitter(s), %s balls in play in the window.", nrow(r), format(sum(r$BBE), big.mark = ","))) })
  output[[p("table")]] <- DT::renderDT({ r <- rows(); validate(need(nrow(r) > 0, "No batting practice in that window."))
    DT::datatable(r, rownames = FALSE, selection = "none", class = "compact stripe", options = list(dom = "tp", pageLength = 25, scrollX = TRUE)) })
  output[[p("dl_zip")]] <- downloadHandler(
    filename = function() sprintf("BP_reports_%s_to_%s.zip", input[[p("range")]][1], input[[p("range")]][2]),
    content = function(file) {
      d <- wd(); validate(need(nrow(d) > 0, "No batting practice in that window"))
      weekly <- bp_is_weekly(d); lab <- bp_date_label(d$Date); hitters <- sort(unique(d$Batter))
      builders <- stats::setNames(lapply(hitters, function(nm) function(path)
        create_bp_pdf(d, nm, path, report_title = if (weekly) "Weekly BP Report" else "BP Report", date_label = lab, trends = weekly)),
        sprintf("BP_%s_%s.pdf", safe_file_name(hitters), format(max(d$Date))))
      palace_zip_reports(file, builders)
    })
  output[[p("dl_lb")]] <- downloadHandler(
    filename = function() sprintf("BP_leaderboard_%s_to_%s.pdf", input[[p("range")]][1], input[[p("range")]][2]),
    content = function(file) { d <- wd(); validate(need(nrow(d) > 0, "No batting practice in that window"))
      create_bp_leaderboard_pdf(calculate_bp_leaderboard(d), bp_date_label(d$Date), file, bp_data = d) })
}
