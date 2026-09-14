# =============================================================================
# R/palace_bp_reports.R — batting practice visualizations, leaderboard, PDF
# reports and exit-velo trends (the BP counterpart of the bullpen reports).
#
# Input frame convention (bp_legacy_frame() builds it from the Storage BP
# rows in R/palace_batting_practice.R): Batter ("Last, First"), BatterSide,
# Date, session_id, ExitSpeed, Angle, Distance, Bearing, PlateLocSide,
# PlateLocHeight, ContactPositionX/Y/Z (feet), BIPind, LA1030ind, HHind,
# Barrelind.
# =============================================================================
if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# MLB-style barrel: 98 mph at 26-30 deg, the window widening one degree per
# mph on each side up to 8-50 deg.
bp_is_barrel <- function(ev, la) {
  ok <- !is.na(ev) & !is.na(la) & ev >= 98
  lo <- pmax(8, 26 - (ev - 98)); hi <- pmin(50, 30 + (ev - 98))
  ok & la >= lo & la <= hi
}
# Storage BP rows (snake_case, R/palace_upload.R UP_BP_COLS) -> report frame
bp_legacy_frame <- function(d) {
  if (is.null(d) || !nrow(d)) return(NULL)
  g <- function(cn) if (cn %in% names(d)) d[[cn]] else rep(NA, nrow(d))
  out <- data.frame(
    Batter = as.character(g("batter")), BatterSide = as.character(g("batter_side")),
    Date = as.Date(as.character(g("session_date"))), session_id = as.character(g("session_id")),
    PitchNo = as.integer(g("pitch_no")), Pitcher = as.character(g("pitcher")),
    ExitSpeed = as.numeric(g("exit_speed")), Angle = as.numeric(g("launch_angle")),
    Direction = as.numeric(g("direction")), Distance = as.numeric(g("distance")), Bearing = as.numeric(g("bearing")),
    HangTime = as.numeric(g("hang_time")), PlateLocSide = as.numeric(g("plate_loc_side")), PlateLocHeight = as.numeric(g("plate_loc_height")),
    ContactPositionX = as.numeric(g("contact_position_x")), ContactPositionY = as.numeric(g("contact_position_y")),
    ContactPositionZ = as.numeric(g("contact_position_z")), RelSpeed = as.numeric(g("rel_speed")),
    stringsAsFactors = FALSE)
  out$Batter[is.na(out$Batter) | !nzchar(out$Batter)] <- "Unknown"
  out$BIPind    <- as.integer(!is.na(out$ExitSpeed))
  out$LA1030ind <- as.integer(out$BIPind == 1 & !is.na(out$Angle) & out$Angle >= 10 & out$Angle <= 30)
  out$HHind     <- as.integer(out$BIPind == 1 & out$ExitSpeed >= 95)
  out$Barrelind <- as.integer(bp_is_barrel(out$ExitSpeed, out$Angle))
  out
}
parse_game_day <- function(df) { d <- unique(na.omit(as.Date(df$Date))); if (length(d) == 1) d else as.Date(NA) }

bp_date_label <- function(dates) {
  d <- sort(unique(na.omit(as.Date(dates)))); if (!length(d)) return(NULL)
  if (length(d) == 1) format(d, "%B %e, %Y") else paste(format(d[1], "%b %e"), "-", format(d[length(d)], "%b %e, %Y"))
}

# ---- in-app extras: exit-velo bucket heatmaps ---------------------------------------
BP_EV_BUCKETS <- list(under85 = list(label = "EV < 85", lo = -Inf, hi = 85), mid = list(label = "EV 85–95", lo = 85, hi = 95), plus95 = list(label = "EV 95+", lo = 95, hi = Inf))
create_bp_ev_heatmap <- function(bp_data, batter_name, bucket = "plus95", min_n = 5) {
  b <- BP_EV_BUCKETS[[bucket]]
  d <- bp_data %>% filter(Batter == batter_name, BIPind == 1, !is.na(ExitSpeed), !is.na(PlateLocSide), !is.na(PlateLocHeight),
                          ExitSpeed >= b$lo, ExitSpeed < b$hi)
  zone <- list(
    annotate("rect", xmin = -0.8303, xmax = 0.8303, ymin = 1.5, ymax = 3.5, fill = NA, color = "black", linewidth = 0.5),
    annotate("path", x = c(-0.708, 0.708, 0.708, 0, -0.708, -0.708), y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15), color = "black", linewidth = 0.4),
    coord_fixed(xlim = c(-2, 2), ylim = c(0, 4.5)), theme_void(),
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 11, face = "bold"), plot.margin = margin(2, 2, 2, 2)))
  ttl <- sprintf("%s (%d BBE)", b$label, nrow(d))
  if (nrow(d) < min_n)
    return(ggplot() + zone + ggtitle(ttl) + annotate("text", x = 0, y = 2.5, label = if (nrow(d)) "Too few balls in play" else "No balls in play", size = 4, color = "gray50"))
  if (nrow(d) < 15)
    return(ggplot(d, aes(PlateLocSide, PlateLocHeight)) + geom_point(aes(fill = ExitSpeed), shape = 21, size = 3, color = "black", alpha = 0.85) +
             scale_fill_gradient(low = "blue", high = "red") + zone + ggtitle(ttl))
  ggplot(d, aes(PlateLocSide, PlateLocHeight)) +
    stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE, n = 100) +
    scale_fill_gradientn(colours = c("white", "blue", "#FF9999", "red", "darkred")) +
    zone + ggtitle(ttl)
}

# =============================================================================
# Report code (spray chart, zone plot, contact maps, leaderboard, PDFs, trends)
# =============================================================================
create_bp_spray_chart <- function(batter_name, bp_data) {
  chart_data <- bp_data %>%
    filter(Batter == batter_name, BIPind == 1, !is.na(Distance), !is.na(Bearing)) %>%
    mutate(
      Bearing2 = Bearing * pi/180,
      x = Distance * sin(Bearing2),
      y = Distance * cos(Bearing2)
    )
  
  if (!nrow(chart_data)) {
    return(
      ggplot() + theme_void() + 
        coord_fixed(xlim = c(-360, 360), ylim = c(-20, 410), expand = FALSE) +
        annotate("segment", x = 0, y = 0, xend = 247.487, yend = 247.487, color = "gray70") +
        annotate("segment", x = 0, y = 0, xend = -247.487, yend = 247.487, color = "gray70") +
        ggtitle(paste("BP Spray Chart:", batter_name)) +
        annotate("text", x = 0, y = 200, label = "No spray data available", size = 5, color = "gray50") +
        theme(plot.title = element_text(hjust=0.5, size=10, face="bold"))
    )
  }
  
  ggplot(chart_data, aes(x, y)) +
    coord_fixed(xlim = c(-360, 360), ylim = c(-20, 410), expand = FALSE) +
    annotate("segment", x = 0, y = 0, xend = 247.487, yend = 247.487, color = "black") +
    annotate("segment", x = 0, y = 0, xend = -247.487, yend = 247.487, color = "black") +
    annotate("segment", x = 63.6396, y = 63.6396, xend = 0, yend = 127.279, color = "black") +
    annotate("segment", x = -63.6396, y = 63.6396, xend = 0, yend = 127.279, color = "black") +
    annotate("curve", x = 89.095, y = 89.095, xend = 0, yend = 160, curvature = 0.36, linewidth = 0.5, color = "black") +
    annotate("curve", x = -89.095, y = 89.095, xend = 0, yend = 160, curvature = -0.36, linewidth = 0.5, color = "black") +
    annotate("curve", x = -247.487, y = 247.487, xend = 247.487, yend = 247.487, curvature = -0.65, linewidth = 0.5, color = "black") +
    geom_point(aes(fill = ExitSpeed), size = 3, shape = 21, color = "black", stroke = 0.4, alpha = 0.85) +
    scale_fill_gradient(low = "blue", high = "red", name = "Exit Velo", na.value = "grey50") +
    theme_void() +
    ggtitle(paste("BP Spray Chart:", batter_name)) +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
      plot.margin = margin(3, 3, 3, 3),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.key.height = unit(0.5, "cm"),
      legend.key.width = unit(0.3, "cm")
    )
}


create_bp_zone_plot <- function(batter_name, bp_data) {
  zone_data <- bp_data %>%
    filter(Batter == batter_name, BIPind == 1, !is.na(PlateLocSide), !is.na(PlateLocHeight))
  
  if (!nrow(zone_data)) {
    return(
      ggplot() +
        annotate("rect", xmin = -0.8303, xmax = 0.8303, ymin = 1.5, ymax = 3.38,
                 alpha = 0, size = .5, color = "gray70") +
        annotate("path", 
                 x = c(-0.708, 0.708, 0.708, 0, -0.708, -0.708),
                 y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15), 
                 color = "gray70", linewidth = 0.5) +
        coord_fixed(ratio = 1, xlim = c(-2, 2), ylim = c(0, 4.5)) +
        theme_void() +
        ggtitle(paste("BP Zone Plot:", batter_name)) +
        annotate("text", x = 0, y = 2.5, label = "No zone data available", size = 5, color = "gray50") +
        theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
    )
  }
  
  ggplot(zone_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
    geom_point(aes(fill = ExitSpeed), size = 3, shape = 21, color = "black", stroke = 0.4, alpha = 0.8) +
    scale_fill_gradient(low = "blue", high = "red", name = "Exit Velo", na.value = "grey50") +
    annotate("rect", xmin = -0.8303, xmax = 0.8303, ymin = 1.5, ymax = 3.38, 
             fill = NA, color = "black", linewidth = 0.8) +
    annotate("path", 
             x = c(-0.708, 0.708, 0.708, 0, -0.708, -0.708),
             y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15), 
             color = "black", linewidth = 0.6) +
    coord_fixed(ratio = 1, xlim = c(-2, 2), ylim = c(0, 4.5)) +
    ggtitle(paste("BP Zone Plot:", batter_name)) +
    theme_void() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
      plot.margin = margin(3, 3, 3, 3),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.key.height = unit(0.5, "cm"),
      legend.key.width = unit(0.3, "cm")
    )
}

create_bp_contact_map <- function(batter_name, bp_data) {
  contact_data <- bp_data %>%
    filter(Batter == batter_name, BIPind == 1, !is.na(ExitSpeed),
           !is.na(ContactPositionZ), !is.na(ContactPositionX), !is.na(ContactPositionY)) %>%
    mutate(
      ContactPositionX = ContactPositionX * 12,
      ContactPositionY = ContactPositionY * 12,
      ContactPositionZ = ContactPositionZ * 12
    )
  
  if (!nrow(contact_data)) {
    return(
      ggplot() +
        annotate("segment", x = -8.5, y = 17, xend = 8.5, yend = 17, color = "gray70", linewidth = 0.5) +
        annotate("segment", x = 8.5, y = 8.5, xend = 8.5, yend = 17, color = "gray70", linewidth = 0.5) +
        annotate("segment", x = -8.5, y = 8.5, xend = -8.5, yend = 17, color = "gray70", linewidth = 0.5) +
        annotate("rect", xmin = 20, xmax = 48, ymin = -20, ymax = 40, fill = NA, color = "gray70", linewidth = 0.5) +
        annotate("rect", xmin = -48, xmax = -20, ymin = -20, ymax = 40, fill = NA, color = "gray70", linewidth = 0.5) +
        xlim(-50, 50) + ylim(-20, 50) +
        coord_fixed() +
        theme_void() +
        ggtitle(paste("BP Contact Points:", batter_name)) +
        annotate("text", x = 0, y = 20, label = "No contact data available", size = 5, color = "gray50") +
        theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
    )
  }
  
  batter_side <- unique(contact_data$BatterSide)[1]
  if (is.na(batter_side)) batter_side <- "Right"
  
  ggplot(contact_data, aes(x = ContactPositionZ, y = ContactPositionX)) +
    annotate("segment", x = -8.5, y = 17, xend = 8.5, yend = 17, color = "black", linewidth = 0.5) +
    annotate("segment", x = 8.5, y = 8.5, xend = 8.5, yend = 17, color = "black", linewidth = 0.5) +
    annotate("segment", x = -8.5, y = 8.5, xend = -8.5, yend = 17, color = "black", linewidth = 0.5) +
    annotate("segment", x = -8.5, y = 8.5, xend = 0, yend = 0, color = "black", linewidth = 0.5) +
    annotate("segment", x = 8.5, y = 8.5, xend = 0, yend = 0, color = "black", linewidth = 0.5) +
    annotate("rect", xmin = 20, xmax = 48, ymin = -20, ymax = 40, fill = NA, color = "black", linewidth = 0.5) +
    annotate("rect", xmin = -48, xmax = -20, ymin = -20, ymax = 40, fill = NA, color = "black", linewidth = 0.5) +
    annotate("text", x = ifelse(batter_side == "Right", -34, 34), y = 10,
             label = ifelse(batter_side == "Right", "R", "L"), size = 7, fontface = "bold") +
    xlim(-50, 50) + ylim(-20, 50) +
    geom_point(aes(fill = ExitSpeed), color = "black", stroke = 0.4, shape = 21, alpha = 0.85, size = 2.5) +
    scale_fill_gradient(name = "Exit Velo", low = "blue", high = "red") +
    coord_fixed() +
    ggtitle(paste("BP Contact Points:", batter_name)) +
    theme_void() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
      plot.margin = margin(3, 3, 3, 3),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.key.height = unit(0.5, "cm"),
      legend.key.width = unit(0.3, "cm")
    )
}

create_bp_contact_side <- function(batter_name, bp_data) {
  # TrackMan contact coords: X = depth (toward pitcher), Y = height, Z = side
  contact_data <- bp_data %>%
    filter(Batter == batter_name, BIPind == 1, !is.na(ExitSpeed),
           !is.na(ContactPositionX), !is.na(ContactPositionY))
  
  # Home plate side illustration (tip toward catcher at depth 0, front edge at 1.417 ft)
  plate_df <- data.frame(
    x = c(0, 0.708, 1.417, 1.417, 0.708),
    y = c(0.25, 0.35, 0.35, 0.15, 0.15)
  )
  
  base_theme <- theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
      axis.title = element_text(size = 9, face = "bold"),
      axis.text = element_text(size = 7, color = "black"),
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
      legend.position = "right",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.key.height = unit(0.5, "cm"),
      legend.key.width = unit(0.3, "cm"),
      plot.margin = margin(3, 3, 3, 3)
    )
  
  if (!nrow(contact_data)) {
    return(
      ggplot() +
        geom_polygon(data = plate_df, aes(x, y), fill = NA, color = "gray70", linewidth = 0.5) +
        coord_fixed(xlim = c(-1, 4), ylim = c(0, 5), expand = FALSE) +
        labs(x = "Contact Depth (ft in front of plate)", y = "Contact Height (ft)") +
        ggtitle(paste("Contact Depth vs Height:", batter_name)) +
        annotate("text", x = 1.5, y = 2.5, label = "No contact data available", size = 5, color = "gray50") +
        base_theme
    )
  }
  
  ggplot(contact_data, aes(x = ContactPositionX, y = ContactPositionY)) +
    geom_polygon(data = plate_df, aes(x, y), fill = "white", color = "black",
                 linewidth = 0.6, inherit.aes = FALSE) +
    geom_point(aes(fill = ExitSpeed), size = 3, shape = 21, color = "black", stroke = 0.4, alpha = 0.85) +
    scale_fill_gradient(low = "blue", high = "red", name = "Exit Velo", na.value = "grey50") +
    coord_fixed(xlim = c(-1, 4), ylim = c(0, 5), expand = FALSE) +
    labs(x = "Contact Depth (ft in front of plate)", y = "Contact Height (ft)") +
    ggtitle(paste("Contact Depth vs Height:", batter_name)) +
    base_theme
}

calculate_bp_leaderboard <- function(bp_data) {
  df <- bp_data %>% filter(!is.na(Batter))
  
  # Switch hitters (tagged on both sides) get one row per side, labeled
  if ("BatterSide" %in% names(df)) {
    switch_batters <- df %>%
      filter(BatterSide %in% c("Left", "Right")) %>%
      distinct(Batter, BatterSide) %>%
      count(Batter) %>%
      filter(n == 2) %>%
      pull(Batter)
    
    if (length(switch_batters)) {
      # Drop side-untagged rows for switch hitters (consistent with the
      # per-side report split, which can't assign them to either page)
      df <- df %>% filter(!(Batter %in% switch_batters & !BatterSide %in% c("Left", "Right")))
      df <- df %>%
        mutate(Batter = ifelse(
          Batter %in% switch_batters,
          paste0(Batter, " (", ifelse(BatterSide == "Left", "LHH", "RHH"), ")"),
          Batter))
    }
  }
  
  df %>%
    group_by(Batter) %>%
    summarise(
      BBE = sum(BIPind, na.rm = TRUE),
      `Avg EV` = round(mean(ExitSpeed[BIPind == 1], na.rm = TRUE), 1),
      `Avg LA` = round(mean(Angle[BIPind == 1], na.rm = TRUE), 1),
      `Max EV` = round(suppressWarnings(max(ExitSpeed[BIPind == 1], na.rm = TRUE)), 1),
      `Max Dist` = round(suppressWarnings(max(Distance[BIPind == 1], na.rm = TRUE)), 0),
      `10-30%` = round(sum(LA1030ind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE) * 100, 1),
      `HH%` = round(sum(HHind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE) * 100, 1),
      `Barrel%` = round(sum(Barrelind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    mutate(across(where(is.numeric), ~ifelse(is.finite(.x), .x, NA_real_))) %>%
    arrange(desc(`Avg EV`))
}
bp_leaderboard_colors <- function(x, target = NULL) {
  # Red (bad) -> white (group average) -> green (good), scaled within the leaderboard.
  # If target is given (e.g. Avg LA), "good" = closest to the target value.
  score <- if (!is.null(target)) -abs(x - target) else x
  ok <- is.finite(score)
  cols <- rep("#FFFFFF", length(x))
  if (sum(ok) >= 2) {
    mu <- mean(score[ok])
    dev <- max(abs(score[ok] - mu))
    scaled <- if (dev > 0) 0.5 + 0.5 * (score[ok] - mu) / dev else rep(0.5, sum(ok))
    ramp <- grDevices::colorRamp(c("#F8696B", "#FFFFFF", "#63BE7B"))
    rgb_vals <- ramp(scaled)
    cols[ok] <- grDevices::rgb(rgb_vals[, 1], rgb_vals[, 2], rgb_vals[, 3], maxColorValue = 255)
  }
  cols
}
create_bp_leaderboard_pdf <- function(lb, date_label, output_file, bp_data = NULL) {
  if (length(dev.list()) > 0) try(dev.off(), silent = TRUE)
  
  stat_cols <- c("BBE", "Avg EV", "Avg LA", "Max EV", "Max Dist", "10-30%", "HH%", "Barrel%")
  color_cols <- setdiff(stat_cols, c("BBE", "Avg LA"))  # BBE = volume; Avg LA = direction, not quality
  
  cell_fill <- matrix("white", nrow = nrow(lb), ncol = length(stat_cols),
                      dimnames = list(NULL, stat_cols))
  for (cn in color_cols) {
    cell_fill[, cn] <- bp_leaderboard_colors(lb[[cn]])
  }
  
  pdf(output_file, width = 11, height = 8.5)
  on.exit(try(dev.off(), silent = TRUE), add = TRUE)
  
  name_w <- 0.20
  col_w <- 0.082
  row_h <- 0.028
  x0 <- 0.5 - (name_w + length(stat_cols) * col_w) / 2
  rows_per_page <- 25
  n_pages <- max(1, ceiling(nrow(lb) / rows_per_page))
  
  # Weekly leaderboards (loaded BP data spans >1 date) get a team EV trends
  # page after the table; single-date leaderboards don't (nothing to trend)
  has_trends <- bp_is_weekly(bp_data)
  total_pages <- n_pages + as.integer(has_trends)
  
  draw_cell <- function(x, y, w, fill, label, bold = FALSE, txt_col = "black") {
    grid::grid.rect(x = x, y = y, width = w * 0.99, height = row_h,
                    just = c("left", "top"),
                    gp = grid::gpar(fill = fill, col = "black", lwd = 0.4))
    grid::grid.text(label, x = x + w * 0.5, y = y - row_h / 2,
                    gp = grid::gpar(cex = 0.85, col = txt_col,
                                    fontface = if (bold) "bold" else "plain"))
  }
  
  for (pg in seq_len(n_pages)) {
    grid::grid.newpage()
    grid::grid.text("BP Leaderboard", y = 0.955,
                    gp = grid::gpar(fontface = "bold", cex = 1.5, col = "#006F71"))
    if (!is.null(date_label)) {
      grid::grid.text(date_label, y = 0.915, gp = grid::gpar(cex = 1.1, col = "grey30"))
    }
    if (total_pages > 1) {
      grid::grid.text(paste0("Page ", pg, " of ", total_pages), x = 0.95, y = 0.03,
                      gp = grid::gpar(cex = 0.7, col = "grey50"))
    }
    
    # Footer
    grid::grid.text("Coastal Carolina Baseball Analytics", x = 0.5, y = 0.018,
                    gp = grid::gpar(cex = 0.65, col = "grey55"))
    
    y_top <- 0.86
    
    # Header row
    draw_cell(x0, y_top, name_w, "#006F71", "Player", bold = TRUE, txt_col = "white")
    for (j in seq_along(stat_cols)) {
      draw_cell(x0 + name_w + (j - 1) * col_w, y_top, col_w, "#006F71",
                stat_cols[j], bold = TRUE, txt_col = "white")
    }
    
    idx <- (((pg - 1) * rows_per_page) + 1):min(pg * rows_per_page, nrow(lb))
    for (r in seq_along(idx)) {
      i <- idx[r]
      y_r <- y_top - r * row_h
      draw_cell(x0, y_r, name_w, "white", lb$Batter[i], bold = TRUE)
      for (j in seq_along(stat_cols)) {
        val <- lb[[stat_cols[j]]][i]
        draw_cell(x0 + name_w + (j - 1) * col_w, y_r, col_w,
                  cell_fill[i, stat_cols[j]],
                  ifelse(is.finite(val), as.character(val), "-"))
      }
    }
  }
  
  # Team exit velo trends page (weekly only)
  if (has_trends) {
    draw_bp_trends_page(bp_data, batter_name = NULL,
                        report_title = "BP Leaderboard", date_label = date_label,
                        page_label = paste0("Page ", total_pages, " of ", total_pages))
  }
  
  invisible(output_file)
}

# ---- BP exit velo trends (weekly reports) ----------------------------------

# Monday of the week containing each date (%u: 1 = Monday ... 7 = Sunday)
bp_week_start <- function(d) {
  d <- as.Date(d)
  d - (as.integer(format(d, "%u")) - 1L)
}

# TRUE when the loaded BP data spans more than one date (the app's definition
# of a "weekly" report scope)
bp_is_weekly <- function(bp_data) {
  if (is.null(bp_data) || !"Date" %in% names(bp_data)) return(FALSE)
  length(unique(na.omit(as.Date(bp_data$Date)))) > 1
}

# TRUE when the loaded BP data spans more than one Mon-Sun week
bp_is_multiweek <- function(bp_data) {
  if (is.null(bp_data) || !"Date" %in% names(bp_data)) return(FALSE)
  length(unique(bp_week_start(na.omit(as.Date(bp_data$Date))))) > 1
}

# Per-day or per-week EV summary. BBE matches the leaderboard definition
# (sum of BIPind); EV stats use only rows with a measured ExitSpeed.
bp_ev_trend_summary <- function(bp_data, by = c("day", "week"), batter_name = NULL) {
  by <- match.arg(by)
  if (is.null(bp_data) || !all(c("Date", "ExitSpeed", "BIPind") %in% names(bp_data))) return(NULL)
  
  df <- bp_data %>% filter(!is.na(Date), BIPind == 1)
  if (!is.null(batter_name)) df <- df %>% filter(Batter == batter_name)
  if (!nrow(df)) return(NULL)
  
  df <- df %>% mutate(Date = as.Date(Date))
  df <- if (by == "week") df %>% mutate(period = bp_week_start(Date)) else df %>% mutate(period = Date)
  
  df %>%
    group_by(period) %>%
    summarise(
      BBE = n(),
      `Avg EV` = mean(ExitSpeed, na.rm = TRUE),
      `Max EV` = suppressWarnings(max(ExitSpeed, na.rm = TRUE)),
      `HH%` = sum(ExitSpeed >= 95, na.rm = TRUE) / n() * 100,
      .groups = "drop"
    ) %>%
    mutate(across(c(`Avg EV`, `Max EV`, `HH%`), ~ifelse(is.finite(.x), .x, NA_real_))) %>%
    filter(!is.na(`Avg EV`)) %>%
    arrange(period)
}

bp_trend_period_label <- function(period, by) {
  if (by == "week") paste0("Wk of ", format(period, "%b %e")) else format(period, "%b %e")
}

# Avg EV (teal) + Max EV (bronze) by day or week. When batter_name is given the
# series are that player's, and team_data (if supplied) adds the team Avg EV as
# a dashed grey reference line. Sample size (BBE) is shown under each x label.
create_bp_ev_trend_chart <- function(bp_data, by = c("day", "week"),
                                     batter_name = NULL, team_data = NULL) {
  by <- match.arg(by)
  who <- if (is.null(batter_name)) "Team" else batter_name
  ttl <- paste0(who, " Exit Velo by ", if (by == "week") "Week" else "Day")
  
  main <- bp_ev_trend_summary(bp_data, by, batter_name)
  ref  <- if (!is.null(batter_name) && !is.null(team_data)) bp_ev_trend_summary(team_data, by) else NULL
  
  base_theme <- theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      axis.text.x = element_text(lineheight = 0.9),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      legend.key.width = unit(1.2, "cm"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(4, 8, 2, 4)
    )
  
  if (is.null(main) || !nrow(main)) {
    return(
      ggplot() + theme_void() + ggtitle(ttl) +
        annotate("text", x = 0.5, y = 0.5, label = "No exit velo data available",
                 size = 5, color = "gray50") +
        theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"))
    )
  }
  
  # Discrete x: union of periods so a player's series lines up with the team line
  periods <- sort(unique(c(main$period, if (!is.null(ref)) ref$period)))
  x_labels <- bp_trend_period_label(periods, by)
  n_lab <- main$BBE[match(periods, main$period)]
  x_labels <- ifelse(is.na(n_lab), x_labels, paste0(x_labels, "\n", n_lab, " BBE"))
  
  long <- bind_rows(
    main %>% transmute(period, Metric = "Avg EV", EV = `Avg EV`),
    main %>% transmute(period, Metric = "Max EV", EV = `Max EV`)
  ) %>%
    filter(!is.na(EV)) %>%
    mutate(x = factor(period, levels = periods, labels = x_labels),
           Metric = factor(Metric, levels = c("Avg EV", "Max EV", "Team Avg EV")))
  
  if (!is.null(ref) && nrow(ref)) {
    ref_long <- ref %>%
      transmute(period, Metric = "Team Avg EV", EV = `Avg EV`) %>%
      mutate(x = factor(period, levels = periods, labels = x_labels),
             Metric = factor(Metric, levels = c("Avg EV", "Max EV", "Team Avg EV")))
  } else ref_long <- NULL
  
  y_vals <- c(long$EV, if (!is.null(ref_long)) ref_long$EV)
  y_rng <- range(y_vals, na.rm = TRUE)
  y_pad <- max(3, diff(y_rng) * 0.15)
  
  p <- ggplot(long, aes(x = x, y = EV, group = Metric))
  
  if (!is.null(ref_long)) {
    p <- p +
      geom_line(data = ref_long, aes(color = Metric, linetype = Metric), linewidth = 0.7) +
      geom_point(data = ref_long, aes(color = Metric), size = 1.8, shape = 1)
  }
  
  p <- p +
    geom_line(aes(color = Metric, linetype = Metric), linewidth = 1) +
    geom_point(aes(color = Metric), size = 2.6) +
    geom_text(aes(label = sprintf("%.1f", EV), color = Metric),
              vjust = -0.9, size = 2.8, show.legend = FALSE) +
    scale_color_manual(values = c("Avg EV" = "#006F71", "Max EV" = "#A27752", "Team Avg EV" = "grey55"),
                       breaks = c("Avg EV", "Max EV", "Team Avg EV"), drop = TRUE) +
    scale_linetype_manual(values = c("Avg EV" = "solid", "Max EV" = "solid", "Team Avg EV" = "dashed"),
                          breaks = c("Avg EV", "Max EV", "Team Avg EV"), drop = TRUE) +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(limits = c(y_rng[1] - y_pad, y_rng[2] + y_pad)) +
    labs(x = NULL, y = "Exit Velocity (mph)") +
    ggtitle(ttl) +
    base_theme
  
  p
}

# One landscape page: EV-by-day chart, plus EV-by-week chart when the loaded
# data spans multiple weeks. batter_name = NULL draws the team version.
draw_bp_trends_page <- function(bp_data, batter_name = NULL, side_label = NULL,
                                report_title = "Weekly BP Report", date_label = NULL,
                                team_data = NULL, page_label = NULL) {
  scope_data <- if (!is.null(team_data)) team_data else bp_data
  multiweek <- bp_is_multiweek(scope_data)
  
  # Whole-scope summary line (same definitions as the leaderboard columns)
  tot <- bp_data %>% filter(!is.na(Date), BIPind == 1)
  if (!is.null(batter_name)) tot <- tot %>% filter(Batter == batter_name)
  ev <- tot$ExitSpeed[!is.na(tot$ExitSpeed)]
  summary_line <- if (length(ev)) {
    sprintf("%d BBE   |   Avg EV %.1f   |   Max EV %.1f   |   HH%% %.1f",
            nrow(tot), mean(ev), max(ev), sum(ev >= 95) / nrow(tot) * 100)
  } else NULL
  
  day_plot  <- create_bp_ev_trend_chart(bp_data, "day", batter_name, team_data)
  week_plot <- if (multiweek) create_bp_ev_trend_chart(bp_data, "week", batter_name, team_data) else NULL
  
  grid::grid.newpage()
  
  grid::grid.text(report_title, y = 0.955,
                  gp = grid::gpar(fontface = "bold", cex = 1.3, col = "#006F71"))
  
  heading <- if (is.null(batter_name)) "Team Exit Velo Trends" else {
    paste0(if (!is.null(side_label)) paste(batter_name, side_label) else batter_name,
           ": Exit Velo Trends")
  }
  grid::grid.text(heading, y = 0.915, gp = grid::gpar(fontface = "bold", cex = 1.4, col = "black"))
  
  y_sub <- 0.878
  if (!is.null(date_label)) {
    grid::grid.text(date_label, y = y_sub, gp = grid::gpar(cex = 1.0, col = "grey30"))
    y_sub <- y_sub - 0.03
  }
  if (!is.null(summary_line)) {
    grid::grid.text(summary_line, y = y_sub, gp = grid::gpar(cex = 0.85, col = "grey40"))
  }
  
  if (multiweek) {
    grid::pushViewport(grid::viewport(x = 0.5, y = 0.835, width = 0.92, height = 0.40, just = c("center", "top")))
    print(day_plot, newpage = FALSE)
    grid::popViewport()
    grid::pushViewport(grid::viewport(x = 0.5, y = 0.43, width = 0.92, height = 0.39, just = c("center", "top")))
    print(week_plot, newpage = FALSE)
    grid::popViewport()
  } else {
    grid::pushViewport(grid::viewport(x = 0.5, y = 0.80, width = 0.86, height = 0.66, just = c("center", "top")))
    print(day_plot, newpage = FALSE)
    grid::popViewport()
  }
  
  if (!is.null(page_label)) {
    grid::grid.text(page_label, x = 0.95, y = 0.03, gp = grid::gpar(cex = 0.7, col = "grey50"))
  }
  grid::grid.text("Coastal Carolina Baseball Analytics", x = 0.5, y = 0.018,
                  gp = grid::gpar(cex = 0.65, col = "grey55"))
  
  invisible(NULL)
}

bp_batter_sides <- function(bp_data, batter_name) {
  s <- bp_data %>% filter(Batter == batter_name) %>% pull(BatterSide)
  intersect(c("Left", "Right"), unique(na.omit(s)))
}

draw_bp_report_page <- function(bp_data, batter_name, side_label = NULL,
                                report_title = "BP Report", date_label = NULL) {
  batter_df <- filter(bp_data, Batter == batter_name)
  
  # Calculate stats in the order: BBE, Avg EV, Avg LA, Max EV, 10-30%, HH%, Barrel%
  stats <- batter_df %>%
    summarise(
      BBE = sum(BIPind, na.rm = TRUE),
      `Avg EV` = round(mean(ExitSpeed[BIPind == 1], na.rm = TRUE), 1),
      `Avg LA` = round(mean(Angle[BIPind == 1], na.rm = TRUE), 1),
      `Max EV` = round(max(ExitSpeed[BIPind == 1], na.rm = TRUE), 1),
      `10-30%` = round(sum(LA1030ind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE) * 100, 1),
      `HH%` = round(sum(HHind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE) * 100, 1),
      `Barrel%` = round(sum(Barrelind, na.rm = TRUE) / sum(BIPind, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    )
  
  # Date line: explicit override (e.g. weekly range) or the session date
  bp_date_label <- date_label
  if (is.null(bp_date_label)) {
    bp_date <- suppressWarnings(tryCatch(parse_game_day(batter_df), error = function(e) as.Date(NA)))
    bp_date_label <- if (!is.na(bp_date)) format(bp_date, "%B %e, %Y") else NULL
  }
  
  # Create plots
  spray_plot <- create_bp_spray_chart(batter_name, bp_data)
  zone_plot <- create_bp_zone_plot(batter_name, bp_data)
  contact_plot <- create_bp_contact_map(batter_name, bp_data)
  side_plot <- create_bp_contact_side(batter_name, bp_data)
  
  grid::grid.newpage()
  
  # Title section
  grid::pushViewport(grid::viewport(x = 0.5, y = 0.97, width = 1, height = 0.05, just = c("center", "top")))
  grid::grid.text(report_title,
                  gp = grid::gpar(fontface = "bold", cex = 1.3, col = "#006F71"))
  grid::popViewport()
  
  grid::pushViewport(grid::viewport(x = 0.5, y = 0.935, width = 1, height = 0.04, just = c("center", "top")))
  grid::grid.text(if (!is.null(side_label)) paste(batter_name, side_label) else batter_name,
                  gp = grid::gpar(fontface = "bold", cex = 1.6, col = "black"))
  grid::popViewport()
  
  # BP date
  if (!is.null(bp_date_label)) {
    grid::pushViewport(grid::viewport(x = 0.5, y = 0.90, width = 1, height = 0.03, just = c("center", "top")))
    grid::grid.text(bp_date_label,
                    gp = grid::gpar(cex = 1.0, col = "grey30"))
    grid::popViewport()
  }
  
  # Stats table with NO color coding (all white) - BIGGER cells and text
  headers <- c("BBE", "Avg EV", "Avg LA", "Max EV", "10-30%", "HH%", "Barrel%")
  values <- c(stats$BBE, stats$`Avg EV`, stats$`Avg LA`, stats$`Max EV`,
              stats$`10-30%`, stats$`HH%`, stats$`Barrel%`)
  
  col_w <- 0.10
  cell_h <- 0.027
  x0 <- 0.5 - (length(headers) * col_w) / 2
  yh <- 0.865
  yv <- yh - cell_h
  
  for (i in seq_along(headers)) {
    xi <- x0 + (i - 1) * col_w
    
    # Header with teal background
    grid::grid.rect(x = xi, y = yh, width = col_w * 0.985, height = cell_h,
                    just = c("left", "top"),
                    gp = grid::gpar(fill = "#006F71", col = "black", lwd = 0.5))
    grid::grid.text(headers[i],
                    x = xi + col_w * 0.49, y = yh - cell_h / 2,
                    gp = grid::gpar(col = "white", cex = 1.0, fontface = "bold"))
    
    # Value cell - NO color coding, all white
    val <- values[i]
    
    grid::grid.rect(x = xi, y = yv, width = col_w * 0.985, height = cell_h,
                    just = c("left", "top"),
                    gp = grid::gpar(fill = "white", col = "black", lwd = 0.4))
    grid::grid.text(ifelse(is.finite(val), as.character(val), "-"),
                    x = xi + col_w * 0.49, y = yv - cell_h / 2,
                    gp = grid::gpar(cex = 1.05))
  }
  
  # Row 1: Spray chart (left) + Zone plot (right)
  grid::pushViewport(grid::viewport(x = 0.25, y = 0.795, width = 0.42, height = 0.37, just = c("center", "top")))
  print(spray_plot, newpage = FALSE)
  grid::popViewport()
  
  grid::pushViewport(grid::viewport(x = 0.75, y = 0.795, width = 0.42, height = 0.37, just = c("center", "top")))
  print(zone_plot, newpage = FALSE)
  grid::popViewport()
  
  # Row 2: Contact map (left) + Contact depth/height side view (right)
  grid::pushViewport(grid::viewport(x = 0.25, y = 0.415, width = 0.44, height = 0.38, just = c("center", "top")))
  print(contact_plot, newpage = FALSE)
  grid::popViewport()
  
  grid::pushViewport(grid::viewport(x = 0.75, y = 0.415, width = 0.44, height = 0.38, just = c("center", "top")))
  print(side_plot, newpage = FALSE)
  grid::popViewport()
  
  # Footer
  grid::grid.text("Coastal Carolina Baseball Analytics", x = 0.5, y = 0.018,
                  gp = grid::gpar(cex = 0.65, col = "grey55"))
  
  invisible(NULL)
}

# The pages one hitter gets (report page, then trends when weekly; one set per
# side for switch hitters). Used by create_bp_pdf and the team bundle.
bp_draw_hitter_pages <- function(bp_data, batter_name, side = NULL, report_title = "BP Report",
                                 date_label = NULL, trends = FALSE) {
  side_tag <- c(Left = "(LHH)", Right = "(RHH)")
  trends <- isTRUE(trends) && bp_is_weekly(bp_data)
  draw_pages <- function(df, side_label = NULL) {
    draw_bp_report_page(df, batter_name, side_label = side_label,
                        report_title = report_title, date_label = date_label)
    if (trends) {
      draw_bp_trends_page(df, batter_name, side_label = side_label,
                          report_title = report_title, date_label = date_label,
                          team_data = bp_data)
    }
  }
  if (!is.null(side)) {
    draw_pages(filter(bp_data, BatterSide == side), side_label = side_tag[[side]])
  } else {
    sides <- bp_batter_sides(bp_data, batter_name)
    if (length(sides) == 2) {
      for (sd in c("Left", "Right")) draw_pages(filter(bp_data, BatterSide == sd), side_label = side_tag[[sd]])
    } else draw_pages(bp_data)
  }
  invisible(NULL)
}

create_bp_pdf <- function(bp_data, batter_name, output_file, side = NULL,
                          report_title = "BP Report", date_label = NULL,
                          trends = FALSE) {
  if (length(dev.list()) > 0) try(dev.off(), silent = TRUE)
  pdf(output_file, width = 11, height = 8.5)
  on.exit(try(dev.off(), silent = TRUE), add = TRUE)
  bp_draw_hitter_pages(bp_data, batter_name, side = side, report_title = report_title,
                       date_label = date_label, trends = trends)
  invisible(output_file)
}

# Every hitter's report in one PDF (leaderboard order), for a session or a
# date range; weekly ranges add the trends pages.
create_bp_team_pdf <- function(bp_data, output_file, report_title = "BP Report", date_label = NULL, trends = FALSE) {
  if (length(dev.list()) > 0) try(dev.off(), silent = TRUE)
  lb <- calculate_bp_leaderboard(bp_data)
  hitters <- unique(sub(" \\((LHH|RHH)\\)$", "", lb$Batter))
  pdf(output_file, width = 11, height = 8.5)
  on.exit(try(dev.off(), silent = TRUE), add = TRUE)
  for (h in hitters) bp_draw_hitter_pages(bp_data, h, report_title = report_title, date_label = date_label, trends = trends)
  invisible(output_file)
}
