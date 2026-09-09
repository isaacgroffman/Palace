  # Heatmap data
  
  create_player_header <- function(df, player) {
    row <- df[df$Pitcher_FL == player | df$Pitcher == player, ]
    if (nrow(row) == 0) {
      return(textGrob(paste("No bio found for", player),
                      gp = gpar(fontface = "bold", cex = 1.2)))
    }
    row <- row[1, ]
    
    # ---------- Image: team logo (replaces headshot/state) ----------
    logo_g <- cached_image_grob(ccu_logo_url, blank_on_fail = FALSE)
    # ------------------------------------------------
    
    # Helpers
    to_first_last <- function(x) {
      if (is.na(x) || is.null(x)) return("Unknown Player")
      out <- sub("^\\s*([^,]+),\\s*(.+)\\s*$", "\\2 \\1", x)
      if (identical(out, x)) x else out
    }
    val_or_na <- function(x) ifelse(is.na(x) || x == "", "N/A", as.character(x))
    hascol <- function(nm) nm %in% names(row)
    
    player_name   <- to_first_last(row$Pitcher)
    position      <- val_or_na(ifelse(is.na(row$Position), "", row$Position))
    jersey_number <- ifelse(is.na(row$Number) || row$Number == "", "", paste0("# ", row$Number))
    
    class_txt     <- if (hascol("Class")) val_or_na(row$Class) else "N/A"
    hometown_txt  <- if (hascol("Hometown")) val_or_na(row$Hometown) else "N/A"
    
    bats_txt      <- if (hascol("Bats"))   val_or_na(row$Bats)   else "N/A"
    throws_txt    <- if (hascol("Throws")) val_or_na(row$Throws) else "N/A"
    
    height_txt    <- if (hascol("Height")) val_or_na(row$Height) else "N/A"
    weight_txt    <- if (hascol("Weight")) {
      ifelse(is.na(row$Weight) || row$Weight == "", "N/A", paste0(row$Weight, " lbs"))
    } else "N/A"
    
    birth_txt <- if (hascol("Birthday")) val_or_na(row$Birthday) else "N/A"
    
    # B-T like the roster pages ("R-R"); fall back to throws only
    bt_txt <- if (bats_txt != "N/A" && throws_txt != "N/A") {
      paste0(substr(bats_txt, 1, 1), "-", substr(throws_txt, 1, 1))
    } else if (throws_txt != "N/A") {
      paste0("T: ", substr(throws_txt, 1, 1))
    } else "N/A"
    
    # DRS26 Jersey is the primary number source for Coastal too;
    # CCU bio Number only backstops it
    num_clean <- get_jersey_number(
      player_name, "Coastal Carolina",
      fallback = if (jersey_number != "") sub("^#\\s*", "", jersey_number) else "")
    
    pro_player_header(
      name    = player_name,
      number  = num_clean,
      logo_grob = logo_g,
      fields  = list(
        list(label = "Position", value = position),
        list(label = "Weight",   value = weight_txt),
        list(label = "Height",   value = height_txt),
        list(label = "Year",     value = class_txt, w = 0.85),
        list(label = "Hometown", value = hometown_txt, w = 2.3),
        list(label = "Born",     value = birth_txt, w = 1.2),
        list(label = "B-T",      value = bt_txt, w = 0.7)
      )
    )
  }
  
  pdiv <- function(num, den) ifelse(den > 0, num/den, NA_real_)
  
  # green-white-red palette with controllable midpoint
  tri_pal <- function(lo, mid, hi) {
    pal <- scales::gradient_n_pal(c("#E1463E","white","#00840D"),
                                  values = c(0, 0.5, 1))
    function(x) pal(scales::rescale(x, to = c(0,1), from = c(lo, hi)))
  }
  
  # reverse (for "lower is better" like xwOBAcon if you add it)
  tri_pal_rev <- function(lo, mid, hi) {
    pal <- scales::gradient_n_pal(c("#00840D","white","#E1463E"),
                                  values = c(0, 0.5, 1))
    function(x) pal(scales::rescale(x, to = c(0,1), from = c(lo, hi)))
  }
  
  # convenient mid as sample median (fallback to midpoint)
  mid_of <- function(x, lo, hi) {
    m <- suppressWarnings(stats::median(x, na.rm = TRUE))
    if (is.finite(m)) m else (lo + hi)/2
  }
  
  
  make_sequence_data <- function(data, pitcher) {
    x <- data %>%
      dplyr::mutate(
        Pitcher = stringr::str_replace_all(Pitcher, "(\\w+), (\\w+)", "\\2 \\1"),
        zone_ind = ifelse(StrikeZoneIndicator == 1, 1, 0),
        strike_ind = ifelse(StrikeIndicator == 1, 1, 0),
        called_strike_ind = ifelse(PitchCall == "StrikeCalled", 1, 0)
      ) %>%
      dplyr::filter(Pitcher == pitcher, !is.na(TaggedPitchType), TaggedPitchType != "Other")
    
    x <- x %>%
      dplyr::arrange(GameID, Date, Inning, PAofInning, PitchofPA) %>%
      dplyr::group_by(GameID, Date, Inning, PAofInning) %>%
      dplyr::mutate(
        prev_type = dplyr::lag(TaggedPitchType, 1),
        seq_lab = paste0(prev_type, " → ", TaggedPitchType),
        is_seq = !is.na(prev_type)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(is_seq)
    
    seq_summary <- x %>%
      dplyr::group_by(seq_lab) %>%
      dplyr::summarise(
        Count = dplyr::n(),
        `Strike %` = round(mean(strike_ind) * 100, 1),
        `Called Strike %` = round(mean(called_strike_ind) * 100, 1),
        .groups = "drop"
      )
    
    seq_summary
  }
  
  
  create_pitcher_spray_chart_fixed <- function(pitcher_name, team_data, 
                                               include_outs = FALSE,
                                               facet_by = "none",
                                               min_ev = NULL) {
    
    # Filter data for the specific pitcher and balls in play
    chart_data <- team_data %>%
      dplyr::filter(Pitcher == pitcher_name, PitchCall == "InPlay") %>%
      dplyr::filter(!is.na(Distance), !is.na(Bearing))
    
    if (nrow(chart_data) == 0) {
      return(ggplot() +
               theme_void() +
               ggtitle(paste("No balls in play data for", pitcher_name)) +
               theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))
    }
    
    # Apply exit velocity filter if specified
    if (!is.null(min_ev) && "ExitSpeed" %in% names(chart_data)) {
      chart_data <- chart_data %>% dplyr::filter(ExitSpeed >= min_ev)
    }
    
    # Create event type classification
    chart_data <- chart_data %>%
      dplyr::mutate(
        event_type = dplyr::case_when(
          PlayResult == "Single"   ~ "Single",
          PlayResult == "Double"   ~ "Double", 
          PlayResult == "Triple"   ~ "Triple",
          PlayResult == "HomeRun"  ~ "Home Run",
          TRUE                     ~ "Field Out"
        )
      )
    
    # Filter out outs if not including them
    if (!include_outs) {
      chart_data <- chart_data %>% dplyr::filter(event_type != "Field Out")
    }
    
    # Calculate coordinates
    chart_data <- chart_data %>%
      dplyr::mutate(
        hc_bearing = Bearing * pi / 180,
        hc_x = Distance * sin(hc_bearing),
        hc_y = Distance * cos(hc_bearing)
      )
    
    if (nrow(chart_data) == 0) {
      return(ggplot() +
               theme_void() +
               ggtitle(paste("No hits to display for", pitcher_name)) +
               theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))
    }
    
    # Create the plot with basic field outline
    p <- chart_data %>%
      ggplot2::ggplot(ggplot2::aes(hc_x, hc_y, fill = event_type)) +
      ggplot2::scale_fill_manual(
        values = c(
          "Single"   = "orange",
          "Double"   = "gold", 
          "Triple"   = "peru",
          "Home Run" = "darkcyan",
          "Field Out"= "grey80"
        ),
        name = "Result"
      ) +
      
      geom_point(size = 3, alpha = 0.8, shape = 21, color="black") +
      geom_segment(data = filter(filtered_spray_data(), TaggedHitType == 'GroundBall'), 
                   aes(x = 0, y = 0,xend = hc_x / sqrt(hc_x^2 + hc_y^2) * line_length, 
                       yend = hc_y / sqrt(hc_x^2 + hc_y^2) * line_length), 
                   color = "red") +  # Extend lines for GB
      ggplot2::ggtitle(paste("Hits Allowed:", pitcher_name)) +
      geom_segment(aes(x = 0, y = 0, xend = 247.487, yend = 247.487), color = "black") +
      geom_segment(aes(x = 0, y = 0, xend = -247.487, yend = 247.487), color = "black") +
      geom_segment(aes(x = 63.6396, y = 63.6396, xend = 0, yend = 127.279), color = "black") +
      geom_segment(aes(x = -63.6396, y = 63.6396, xend = 0, yend = 127.279), color = "black") +
      geom_curve(aes(x = 85.095, y = 85.095, xend = 0, yend = 160), curvature = 0.36, size = .5,
                 color = "black") +
      geom_curve(aes(x = -85.095, y = 85.095, xend = 0, yend = 160), curvature = -0.36, size = .5,
                 color = "black") +
      geom_curve(aes(x = -247.487, y = 247.487, xend = 247.487, yend = 247.487),
                 curvature = -0.65, size = .5,
                 color = "black") +
      theme_void() + theme(legend.position = "bottom", axis.title = element_blank()) +
      theme(strip.text = element_text(size = 20, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(), 
            legend.text = element_text(size = 18),
            legend.title = element_blank()#remove y axis labels
      )  + facet_wrap(~Pitcher) + coord_cartesian()
    
    # Add faceting if requested
    if (facet_by == "BatterSide" && "BatterSide" %in% names(chart_data)) {
      p <- p + ggplot2::facet_wrap(~BatterSide, nrow = 1)
    } else if (facet_by == "TaggedPitchType" && "TaggedPitchType" %in% names(chart_data)) {
      p <- p + ggplot2::facet_wrap(~TaggedPitchType)
    }
    
    return(p)
  }
  
  filtered_spray_data <- reactive({
    df <- selected_data()
    
    spray_df <- df %>%
      dplyr::filter(PitchCall == "InPlay",
                    !is.na(Distance), !is.na(Bearing))
    
    if (!is.null(input$BatterSide_spray))
      spray_df <- dplyr::filter(spray_df, BatterSide %in% input$BatterSide_spray)
    
    if (!is.null(input$pitchtype_spray))
      spray_df <- dplyr::filter(spray_df, TaggedPitchType %in% input$pitchtype_spray)
    
    if (!is.null(input$dateRange_spray))
      spray_df <- dplyr::filter(spray_df,
                                Date >= as.Date(input$dateRange_spray[1]) &
                                  Date <= as.Date(input$dateRange_spray[2]))
    
    # Multi-select pitchers (if nothing selected, keep all)
    if (!is.null(input$pitcher_spray) && length(input$pitcher_spray) > 0)
      spray_df <- dplyr::filter(spray_df, Pitcher %in% input$pitcher_spray)
    
    # 2026 Roster filter (if present)
    if (!is.null(input$Roster_2026_spray) && exists("bio") && "Roster_2026" %in% names(bio)) {
      Roster_pitchers <- bio %>%
        dplyr::filter(`Roster_2026` %in% input$Roster_2026_spray) %>%
        dplyr::pull(Pitcher_FL)
      spray_df <- dplyr::filter(spray_df, Pitcher %in% Roster_pitchers)
    }
    
    # Create hc_x/hc_y if not present
    if (!all(c("hc_x","hc_y") %in% names(spray_df))) {
      spray_df <- spray_df %>%
        dplyr::mutate(
          hc_bearing = Bearing * pi / 180,
          hc_x = Distance * sin(hc_bearing),
          hc_y = Distance * cos(hc_bearing)
        )
    }
    
    spray_df
  })
  
  
  processed_data_leader <- reactive({
    df <- selected_data()
    req(input$dateRange_leader, input$BatterSide_leader, input$pitchtype_leader)
    
    filtered_data2 <- df %>% 
      filter(
        Date >= as.Date(input$dateRange_leader[1]) & Date <= as.Date(input$dateRange_leader[2]),
        BatterSide %in% input$BatterSide_leader,
        TaggedPitchType %in% input$pitchtype_leader
      )
    
    # Apply Roster filter if present
    if (!is.null(input$Roster_2026_leader) && "Roster_2026" %in% names(bio)) {
      Roster_pitchers <- bio %>%
        dplyr::filter(Roster_2026 %in% input$Roster_2026_leader) %>%
        dplyr::pull(Pitcher_FL)
      filtered_data2 <- filtered_data2 %>%
        dplyr::filter(Pitcher %in% Roster_pitchers)
    }
    
    # Apply pitcher filter if present
    if (!is.null(input$pitcher_leader) && length(input$pitcher_leader) > 0) {
      filtered_data2 <- filtered_data2 %>%
        dplyr::filter(Pitcher %in% input$pitcher_leader)
    }
    
    # Calculate statistics for each pitcher with NEW stats and NEW column order
    summary_data <- filtered_data2 %>%
      group_by(Pitcher) %>%
      summarize(
        'BF' = sum(PAindicator, na.rm = TRUE),
        'Avg FB Velo' = round(mean(RelSpeed[TaggedPitchType == "Fastball" | TaggedPitchType == 'Sinker'], na.rm = TRUE), 1),
        'Max FB Velo' = round(max(RelSpeed[TaggedPitchType == "Fastball" | TaggedPitchType == 'Sinker'], na.rm = TRUE), 1),
        'Strike %' = round(sum(StrikeIndicator, na.rm = TRUE) / n() * 100, 1),
        'FB Strike %' = round(sum(FBstrikeind, na.rm = TRUE) / sum(FBindicator, na.rm = TRUE) * 100, 1),
        'OS Strike %' = round(sum(OSstrikeind, na.rm = TRUE) / sum(OSindicator, na.rm = TRUE) * 100, 1),
        'E+A %' = round((sum(EarlyIndicator, na.rm = TRUE) + sum(AheadIndicator, na.rm = TRUE)) / sum(PAindicator, na.rm = TRUE) * 100, 1),
        '1PK %' = round(sum(FPSindicator, na.rm = TRUE) / sum(FPindicator, na.rm = TRUE) * 100, 1),
        'Whiff %' = round(sum(WhiffIndicator, na.rm = TRUE) / sum(SwingIndicator, na.rm = TRUE) * 100, 1),
        'IZ Whiff %' = round(sum(Zwhiffind, na.rm = TRUE) / sum(Zswing, na.rm = TRUE) * 100, 1),
        'Chase %' = round(sum(Chaseindicator, na.rm = TRUE) / sum(OutofZone, na.rm = TRUE) * 100, 1),
        # NEW: Plate % added after Zone % (horizontal zone only)
        'Plate %' = round(sum(PlateIndicator, na.rm = TRUE) / n() * 100, 1),
'K %' = round(sum(is_k, na.rm = TRUE) / sum(PAindicator, na.rm = TRUE) * 100, 1),
        'BB %' = round(sum(WalkIndicator, na.rm = TRUE) / sum(PAindicator, na.rm = TRUE) * 100, 1),
        'LOO %' = round(sum(LOOindicator, na.rm = TRUE) / sum(LeadOffIndicator, na.rm = TRUE) * 100, 1),
'AVG' = round(sum(HitIndicator, na.rm = TRUE) / sum(ABindicator, na.rm = TRUE), 3),
        'OPS' = round(
          (sum(OnBaseindicator, na.rm = TRUE) / sum(OBPdenominator, na.rm = TRUE)) +
          (sum(totalbases, na.rm = TRUE) / sum(ABindicator, na.rm = TRUE)), 3),
        # NEW ORDER: Barrel%, IZ Barrel%, HH%, GB%
        'Barrel %' = round(sum(BarrelIndicator, na.rm = TRUE) / sum(BIPind, na.rm = TRUE) * 100, 1),
        'IZ Barrel %' = round(sum(IZBarrelIndicator, na.rm = TRUE) / sum(BIPind[StrikeZoneIndicator == 1], na.rm = TRUE) * 100, 1),
        'HH %' = round(sum(HHindicator, na.rm = TRUE) / sum(biphh, na.rm = TRUE) * 100, 1),
        'GB %' = round(sum(GBindicator, na.rm = TRUE) / sum(BIPind, na.rm = TRUE) * 100, 1),
        .groups = 'drop'
      )

    # Expected stats off the RAW frame (Pitch Profiler xstats chain).
    # Placed after OPS so actual and expected damage read side by side.
    xs <- pp_expected_stats_cached(filtered_data2, group_cols = c("Pitcher"))
    if (!is.null(xs) && "Pitcher" %in% names(xs)) {
      summary_data <- summary_data %>%
        dplyr::left_join(xs, by = "Pitcher") %>%
        dplyr::relocate(dplyr::any_of(c("xBA", "xSLG", "xwOBA")),
                        .after = "OPS")
    }

    return(summary_data)
  })
  
  
  output$statistics_table <- renderDataTable({
    d <- processed_data_leader()
    # Pitcher names click through to that arm's Overview page.
    if ("Pitcher" %in% names(d)) {
      d$Pitcher <- vapply(as.character(d$Pitcher),
                          tp_player_link, character(1), side = "pitcher")
    }
    tbl <- DT::datatable(
      d,
      escape = FALSE,
      options = list(paging = FALSE, ordering = TRUE, searching = FALSE)
    )
    # Expected stats — LOWER better, same treatment as AVG/OPS. Guarded:
    # the columns exist only when the xstats python call succeeded.
    if ("xBA" %in% names(d))
      tbl <- tbl %>% formatStyle('xBA',
        backgroundColor = styleInterval(brksavg, clrsavg))
    if ("xSLG" %in% names(d))
      tbl <- tbl %>% formatStyle('xSLG',
        backgroundColor = styleInterval(seq(.250, .640, .001),
          colorRampPalette(c("#00840D","white","#E1463E"))(length(seq(.250, .640, .001)) + 1)))
    if ("xwOBA" %in% names(d))
      tbl <- tbl %>% formatStyle('xwOBA',
        backgroundColor = styleInterval(seq(.250, .450, .001),
          colorRampPalette(c("#00840D","white","#E1463E"))(length(seq(.250, .450, .001)) + 1)))
    tbl %>%
      formatStyle(
        'Avg FB Velo',
        backgroundColor = styleInterval(brks, clrs)
      ) %>%
      formatStyle(
        'Max FB Velo',
        backgroundColor = styleInterval(brks2,clrs2)
      ) %>%
      formatStyle(
        'Strike %',
        backgroundColor = styleInterval(brks3, clrs3)
      ) %>%
      formatStyle(
        'FB Strike %',
        backgroundColor = styleInterval(brks4,clrs4)
      ) %>%
      formatStyle(
        'OS Strike %',
        backgroundColor = styleInterval(brks5,clrs5)
      ) %>%
      formatStyle(
        'E+A %',
        backgroundColor = styleInterval(brksepa,clrsepa)
      ) %>%
      formatStyle(
        '1PK %',
        backgroundColor = styleInterval(brks6,clrs6)
      ) %>%
      formatStyle(
        'Whiff %',
        backgroundColor = styleInterval(brks7, clrs7)
      ) %>%
      formatStyle(
        'IZ Whiff %',
        backgroundColor = styleInterval(brks8,clrs8)
      ) %>%
      formatStyle(
        'Chase %',
        backgroundColor = styleInterval(brkschase,clrschase)
      ) %>%
      # NEW: Plate % formatting
      formatStyle(
        'Plate %',
        backgroundColor = styleInterval(brksplate, clrsplate)
      ) %>%
      formatStyle(
        'K %',
        backgroundColor = styleInterval(brks9,clrs9)
      ) %>%
      formatStyle(
        'BB %',
        backgroundColor = styleInterval(brksbb, clrsbb)
      ) %>%
      formatStyle(
        'LOO %',
        backgroundColor = styleInterval(brksloo, clrsloo)
      ) %>%
      formatStyle(
        'AVG',
        backgroundColor = styleInterval(brksavg, clrsavg)
      ) %>%
      formatStyle(
        'OPS',
        backgroundColor = styleInterval(brksops, clrsops)
      ) %>%
      # NEW: Barrel % formatting (lower is better - green to red)
      formatStyle(
        'Barrel %',
        backgroundColor = styleInterval(brksbarrel, clrsbarrel)
      ) %>%
      # NEW: IZ Barrel % formatting (lower is better - green to red)
      formatStyle(
        'IZ Barrel %',
        backgroundColor = styleInterval(brksizbarrel, clrsizbarrel)
      ) %>%
      formatStyle(
        'HH %',
        backgroundColor = styleInterval(brkshh, clrshh)
      ) %>%
      formatStyle(
        'GB %',
        backgroundColor = styleInterval(brksgb, clrsgb)
      )
  })
  
  output$spraychart_main <- renderPlot({
    sd <- filtered_spray_data()
    validate(need(nrow(sd) > 0, "No balls in play in current filters"))
    
    # ensure coords exist (safety)
    if (!all(c("hc_x","hc_y") %in% names(sd))) {
      sd <- sd %>%
        dplyr::mutate(
          hc_bearing = Bearing * pi/180,
          hc_x = Distance * sin(hc_bearing),
          hc_y = Distance * cos(hc_bearing)
        )
    }
    
    # length for GB rays
    line_length <- 150
    
    # Hit-type palette (this is what the spray chart actually uses)
    hittype_colors <- c(
      "GroundBall" = "#D95F02",
      "LineDrive"  = "#1B9E77",
      "FlyBall"    = "#7570B3",
      "PopUp"      = "#E6AB02"
    )
    
    ggplot(sd, aes(x = hc_x, y = hc_y, fill = TaggedHitType)) +
      geom_point(size = 3, alpha = 0.8, shape = 21, color = "black") +
      geom_segment(
        data = dplyr::filter(sd, TaggedHitType == "GroundBall"),
        aes(x = 0, y = 0,
            xend = hc_x / sqrt(hc_x^2 + hc_y^2) * line_length,
            yend = hc_y / sqrt(hc_x^2 + hc_y^2) * line_length),
        color = "red"
      ) +
      geom_segment(aes(x = 0, y = 0, xend = 247.487, yend = 247.487), color = "black") +
      geom_segment(aes(x = 0, y = 0, xend = -247.487, yend = 247.487), color = "black") +
      geom_segment(aes(x = 63.6396, y = 63.6396, xend = 0, yend = 127.279), color = "black") +
      geom_segment(aes(x = -63.6396, y = 63.6396, xend = 0, yend = 127.279), color = "black") +
      geom_curve(aes(x = 85.095, y = 85.095, xend = 0, yend = 160), curvature = 0.36, size = .5,
                 color = "black") +
      geom_curve(aes(x = -85.095, y = 85.095, xend = 0, yend = 160), curvature = -0.36, size = .5,
                 color = "black") +
      geom_curve(aes(x = -247.487, y = 247.487, xend = 247.487, yend = 247.487),
                 curvature = -0.65, size = .5,
                 color = "black") +
      scale_fill_manual(values = hittype_colors, drop = FALSE) +
      theme_void() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text  = element_text(size = 18),
        strip.text   = element_text(size = 20, face = "bold"),
        axis.text.x  = element_blank(),
        axis.text.y  = element_blank()
      ) +
      facet_wrap(~Pitcher) +
      coord_cartesian()
  })
  
  
  
  
  # =====================================================================
  # ============  NEW: GLOBAL POOL, GRADING CHART, VIZ OUTPUTS  ==========
  # =====================================================================

  # Resolve the selected grading pool to a data frame (D1 = full P5+SBC).
  # These are the SLIM pools (R/palace_artifacts.R): every column the grading
  # code reads, none of the 100 it does not, precomputed and ~5x smaller.
  .grade_pool_cache <- new.env(parent = emptyenv())
  grade_pool_df <- reactive({
    pool <- input$global_pool %||% "D1"
    hit <- .grade_pool_cache[[pool]]
    if (!is.null(hit)) return(hit)
    out <- switch(pool,
                  "Power 5"  = P5_slim,
                  "Sun Belt" = SBC_slim,
                  "D1"       = dplyr::bind_rows(harmonize_types(list(P5_slim, SBC_slim))),
                  P5_slim)
    if (is.null(out) || !is.data.frame(out) || nrow(out) == 0) out <- P5_slim
    .grade_pool_cache[[pool]] <- out
    out
  })

  output$grade_pool_label <- renderText({
    input$global_pool %||% "D1"
  })

  # ===== Old methodology: z-score x 15 vs P5/SBC pool distribution =====
  GRADE_PTS_PER_SD <- 15
  GRADE_CENTER     <- 100

  # Per-pitcher summary of the twelve grade-relevant rate stats.
  summarise_grade_stats <- function(df) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
    pdivl <- function(a, b) ifelse(b > 0, 100 * a / b, NA_real_)
    df %>%
      dplyr::summarise(
        n           = dplyr::n(),
        `Strike%`   = pdivl(sum(StrikeIndicator, na.rm = TRUE), dplyr::n()),
        `0-0 Strike%` = pdivl(sum(StrikeIndicator[Balls == 0 & Strikes == 0], na.rm = TRUE),
                              sum(Balls == 0 & Strikes == 0, na.rm = TRUE)),
        `0-0 Zone%` = pdivl(sum(StrikeZoneIndicator[Balls == 0 & Strikes == 0], na.rm = TRUE),
                            sum(Balls == 0 & Strikes == 0, na.rm = TRUE)),
        `Ahead Strike%` = pdivl(sum(StrikeIndicator[Strikes > Balls], na.rm = TRUE),
                                sum(Strikes > Balls, na.rm = TRUE)),
        `Kill%` = {
          n_k2k <- sum(KorBB == "Strikeout", na.rm = TRUE)
          n_pa2k <- dplyr::n_distinct(
            paste(GameID, Inning, PAofInning)[Strikes == 2])
          if (n_pa2k > 0) 100 * n_k2k / n_pa2k else NA_real_
        },
        `Walk%`     = pdivl(sum(WalkIndicator, na.rm = TRUE), sum(PAindicator, na.rm = TRUE)),
        `Miss%`     = pdivl(sum(WhiffIndicator, na.rm = TRUE), sum(SwingIndicator, na.rm = TRUE)),
        `IZ Miss%`  = pdivl(sum(Zwhiffind, na.rm = TRUE), sum(Zswing, na.rm = TRUE)),
        `Chase%`    = pdivl(sum(Chaseindicator, na.rm = TRUE), sum(OutofZone, na.rm = TRUE)),
        `2K Miss%`  = pdivl(sum(WhiffIndicator[Strikes == 2], na.rm = TRUE),
                            sum(SwingIndicator[Strikes == 2], na.rm = TRUE)),
        AVG         = ifelse(sum(ABindicator, na.rm = TRUE) > 0,
                             sum(HitIndicator, na.rm = TRUE) / sum(ABindicator, na.rm = TRUE), NA_real_),
        SLG         = ifelse(sum(ABindicator, na.rm = TRUE) > 0,
                             sum(totalbases, na.rm = TRUE) / sum(ABindicator, na.rm = TRUE), NA_real_),
        `GB%`       = pdivl(sum(GBindicator, na.rm = TRUE), sum(BIPind, na.rm = TRUE)),
        `HardHit%`  = pdivl(sum(HHindicator, na.rm = TRUE), sum(biphh, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  # Per-arm expected stats over the grading pool. ONE cached proModel
  # call for the whole pool (pp_expected_stats_cached already memoizes on
  # frame shape), reused by both the pool distribution and the selected
  # pitcher so the Damage bar reads the same xwOBA the vLHH / vRHH tables
  # print. Returns NULL if the model is unavailable, and every consumer
  # degrades to a three-stat Damage pillar rather than erroring.
  xs_pool_arm <- reactive({
    pool <- tryCatch(grade_pool_df(), error = function(e) NULL)
    if (is.null(pool) || !nrow(pool)) return(NULL)
    # from pitchprofiler.pitcher_season (same bundle rules, no Python at runtime)
    tryCatch(pp_pool_xstats(pool, by = "Pitcher"),
             error = function(e) {
               cat("[Grades] pool expected stats unavailable -",
                   conditionMessage(e), "\n"); NULL })
  })

  # Pool distribution (mean / sd per stat) at the pitcher level.
  grade_pool_dist <- reactive({
    pool_name <- input$global_pool %||% "D1"
    cached <- .grade_dist_cache[[pool_name]]
    if (!is.null(cached)) return(cached)

    pool <- grade_pool_df()
    req("Pitcher" %in% names(pool))
    per_arm <- pool %>%
      dplyr::group_by(Pitcher) %>%
      dplyr::group_modify(~ summarise_grade_stats(.x)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(n >= 100)

    # attach per-arm xwOBA so the Damage pillar has a distribution to
    # scale against, exactly like the other three stats
    xs <- xs_pool_arm()
    if (!is.null(xs) && all(c("Pitcher", "xwOBA") %in% names(xs))) {
      per_arm <- per_arm %>%
        dplyr::left_join(xs[, c("Pitcher", "xwOBA")], by = "Pitcher")
    } else {
      per_arm$xwOBA <- NA_real_
    }

    stat_cols <- setdiff(names(per_arm), c("Pitcher", "n"))
    out <- tibble::tibble(
      stat = stat_cols,
      mean = vapply(stat_cols, function(s) mean(per_arm[[s]], na.rm = TRUE), numeric(1)),
      sd   = vapply(stat_cols, function(s) stats::sd(per_arm[[s]], na.rm = TRUE), numeric(1))
    )
    .grade_dist_cache[[pool_name]] <- out
    out
  })

  # Direction: TRUE = higher is better. Walk/AVG/SLG/HardHit are inverted.
  grade_direction <- c(
    `Strike%` = 1, `0-0 Strike%` = 1, `0-0 Zone%` = 1, `Ahead Strike%` = 1, `Walk%` = -1,
    `Kill%` = 1,
    `Miss%` = 1, `IZ Miss%` = 1, `Chase%` = 1, `2K Miss%` = 1,
    AVG = -1, SLG = -1, xwOBA = -1, `HardHit%` = -1
  )

  grade_scale_to_100 <- function(value, stat, dist) {
    row <- dist[dist$stat == stat, ]
    if (nrow(row) == 0 || is.na(value) || is.na(row$sd) || row$sd == 0) return(NA_real_)
    z <- (value - row$mean) / row$sd
    dir <- grade_direction[[stat]]
    if (is.null(dir)) dir <- 1
    round(GRADE_CENTER + dir * z * GRADE_PTS_PER_SD)
  }

  # Pillar -> stats. Short pillar keys map to the display label used in the chart.
  grade_groups <- list(
    "Strikes" = c("Strike%", "0-0 Strike%", "Ahead Strike%", "Walk%"),
    "Miss"    = c("Miss%", "IZ Miss%", "Chase%", "2K Miss%"),
    "Damage"  = c("AVG", "SLG", "xwOBA", "HardHit%")
  )

  grade_breakdown <- reactive({
    req(input$global_pitcher)
    pdat0 <- filtered_data() %>% dplyr::filter(Pitcher == input$global_pitcher)
    validate(need(nrow(pdat0) > 0, "No data for this pitcher in the current filters."))
    dist <- grade_pool_dist()
    validate(need(nrow(dist) > 0, "Pool distribution unavailable."))

    side_data <- function(side_code) {
      if (side_code == "All") pdat0
      else pdat0 %>% dplyr::filter(norm_side(BatterSide) == side_code)
    }

    sides <- c("All", "R", "L")
    subs <- dplyr::bind_rows(lapply(sides, function(sc) {
      d <- side_data(sc)
      if (nrow(d) == 0) return(NULL)
      raw <- summarise_grade_stats(d)
      if (is.null(raw)) return(NULL)

      # xwOBA is not a pitch-level tally like the others -- it comes off
      # the proModel expected-stats chain. Computing it on THIS side's
      # frame is what makes the Damage bar agree with the vLHH / vRHH
      # tables, which run the same call on the same split.
      raw$xwOBA <- tryCatch({
        xs <- pp_expected_stats_cached(d, group_cols = c("Pitcher"))
        if (!is.null(xs) && "xwOBA" %in% names(xs) && nrow(xs) > 0)
          suppressWarnings(as.numeric(xs$xwOBA[1])) else NA_real_
      }, error = function(e) NA_real_)

      # NULL-safe: a stat the summary does not carry grades as NA rather
      # than blowing up vapply's type contract.
      gv <- function(s) {
        v <- raw[[s]]
        if (is.null(v) || length(v) == 0) NA_real_ else suppressWarnings(as.numeric(v[1]))
      }
      purrr::imap_dfr(grade_groups, function(stats_vec, gname) {
        tibble::tibble(
          group = gname, stat = stats_vec,
          val   = vapply(stats_vec, gv, numeric(1)),
          grade = vapply(stats_vec, function(s) grade_scale_to_100(gv(s), s, dist), numeric(1))
        )
      }) %>% dplyr::mutate(Side = sc)
    }))
    validate(need(nrow(subs) > 0, "No data to grade for this pitcher."))
    subs$group <- factor(subs$group, levels = names(grade_groups))
    subs$Side  <- factor(subs$Side,  levels = c("L","All","R"))

    # Composite pillar grade per side = mean of member stat grades.
    comps <- subs %>%
      dplyr::group_by(group, Side) %>%
      dplyr::summarise(grade = round(mean(grade, na.rm = TRUE)), .groups = "drop") %>%
      dplyr::mutate(above = grade >= 100, height = grade - 100)
    comps$group <- factor(comps$group, levels = names(grade_groups))
    comps$Side  <- factor(comps$Side,  levels = c("L","All","R"))

    list(subs = subs, comps = comps, sides = sides)
  })

  # --- Grading bar chart: vL / All / vR bars per pillar (reference style) ---
  output$grade_bar_chart <- renderPlot({
    gb <- grade_breakdown()
    grp_df <- gb$comps
    teal <- "#0E6E70"; gray_bar <- "#B9C2CC"

    # All bars colored by goodness; vL/vR shown as gray context bars.
    plus_col <- function(score) {
      z <- (score - 100) / (GRADE_PTS_PER_SD * 2)
      if (is.na(z) || z == 0) return(gray_bar)
      if (z > 0) {
        t <- pmin(z, 1); sprintf("#%02X%02X%02X", round(255 + t*(0x1E-255)), round(255 + t*(0x8E-255)), round(255 + t*(0x1E-255)))
      } else {
        t <- pmin(-z, 1); sprintf("#%02X%02X%02X", round(255 + t*(0xC0-255)), round(255 + t*(0x1E-255)), round(255 + t*(0x1E-255)))
      }
    }
    grp_df$fill <- ifelse(grp_df$Side == "All",
                          vapply(grp_df$grade, plus_col, character(1)),
                          gray_bar)

    ggplot(grp_df, aes(x = group, y = height, group = Side)) +
      geom_hline(yintercept = 0, color = "#9AA3AE", linewidth = 0.7) +
      geom_col(aes(fill = fill), position = position_dodge(width = 0.78),
               width = 0.72, color = "gray35", linewidth = 0.25) +
      scale_fill_identity() +
      geom_text(aes(label = grade,
                    y = height + ifelse(height >= 0, 5, -5),
                    vjust = ifelse(height >= 0, 0, 1)),
                position = position_dodge(width = 0.78),
                fontface = "bold", size = 4.6) +
      geom_text(aes(label = ifelse(Side == "All", "All", ifelse(Side == "L","vL","vR")),
                    y = ifelse(height >= 0, -6, 6)),
                position = position_dodge(width = 0.78),
                size = 3, color = "#6B7280") +
      scale_y_continuous(limits = function(l) c(min(-45, l[1]-8), max(45, l[2]+10))) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 16, color = teal)
      )
  })

  # --- Grade stats table: old 12-stat layout, rows All / vs RHH / vs LHH ---
  output$grade_stats_table <- gt::render_gt({
    gb <- grade_breakdown()
    tab <- gb$subs %>%
      dplyr::mutate(
        Display = ifelse(stat %in% c("AVG","SLG","xwOBA"),
                         formatC(val, format = "f", digits = 3),
                         paste0(formatC(val, format = "f", digits = 1), "%")),
        pct = pmin(pmax(stats::pnorm((grade - 100) / GRADE_PTS_PER_SD), 0), 1),
        cell = paste0(Display, ifelse(is.na(grade), "", paste0(" (", grade, ")")))
      )

    stat_order <- c(grade_groups[["Strikes"]], grade_groups[["Miss"]], grade_groups[["Damage"]])

    mk_row <- function(side_code, side_label) {
      r <- tibble::tibble(Side = side_label)
      sub <- tab %>% dplyr::filter(Side == side_code)
      for (s in stat_order) {
        v <- sub %>% dplyr::filter(stat == s)
        r[[s]] <- if (nrow(v) == 0) "—" else v$cell[1]
      }
      r
    }
    wide <- dplyr::bind_rows(mk_row("All", "All"),
                             mk_row("R", "vs RHH"),
                             mk_row("L", "vs LHH"))

    pct_lk <- tab %>% dplyr::select(Side, stat, pct)

    g <- wide %>%
      gt::gt(rowname_col = "Side") %>%
      pp_gt_modern() %>%
      gt::tab_spanner(label = "Throw Strikes",
                      columns = c("Strike%", "0-0 Strike%", "Ahead Strike%", "Walk%")) %>%
      gt::tab_spanner(label = "Miss Bats",
                      columns = c("Miss%", "IZ Miss%", "Chase%", "2K Miss%")) %>%
      gt::tab_spanner(label = "Limit Damage",
                      columns = c("AVG", "SLG", "xwOBA", "HardHit%")) %>%
      gt::tab_style(style = gt::cell_text(weight = "bold", size = gt::px(11),
                                          color = "#8A93A0", transform = "uppercase"),
                    locations = gt::cells_column_spanners()) %>%
      gt::tab_style(style = gt::cell_text(weight = "bold", color = "#243746"),
                    locations = gt::cells_stub())

    sides_ord <- c("All","R","L")
    for (s in stat_order) {
      local({
        col_s <- s
        pcts <- vapply(sides_ord, function(sc) {
          p <- pct_lk$pct[pct_lk$stat == col_s & pct_lk$Side == sc]
          if (length(p)) p[1] else NA_real_
        }, numeric(1))
        g <<- g %>% gt::text_transform(
          locations = gt::cells_body(columns = col_s),
          fn = function(v) lapply(seq_along(v), function(i) pp_pill(v[i], pcts[i]))
        )
      })
    }
    g
  })

  # --- Count leverage table: 0-0 Zone% / Ahead Strike% / Kill%, rows vs LHH/RHH.
  # Same colored-pill percentile style as the grade stats table (pool z-scores).
  output$count_leverage_table <- gt::render_gt({
    req(input$global_pitcher)
    pdat0 <- filtered_data() %>% dplyr::filter(Pitcher == input$global_pitcher)
    validate(need(nrow(pdat0) > 0, "No data for this pitcher in the current filters."))
    dist <- grade_pool_dist()
    validate(need(nrow(dist) > 0, "Pool distribution unavailable."))

    cl_stats <- c("0-0 Zone%", "Ahead Strike%", "Kill%")

    side_row <- function(side_code, side_label) {
      sub <- if (side_code == "R") pdat0 %>% dplyr::filter(norm_side(BatterSide) == "R")
             else                  pdat0 %>% dplyr::filter(norm_side(BatterSide) == "L")
      raw <- summarise_grade_stats(sub)
      r <- tibble::tibble(Side = side_label)
      pcts <- setNames(rep(NA_real_, length(cl_stats)), cl_stats)
      for (s in cl_stats) {
        val <- if (is.null(raw)) NA_real_ else raw[[s]]
        grd <- grade_scale_to_100(val, s, dist)
        r[[s]] <- if (is.na(val)) "\u2014" else paste0(formatC(val, format = "f", digits = 1), "%")
        pcts[s] <- if (is.na(grd)) NA_real_
                   else pmin(pmax(stats::pnorm((grd - 100) / GRADE_PTS_PER_SD), 0), 1)
      }
      list(row = r, pcts = pcts)
    }

    rL <- side_row("L", "vs LHH")
    rR <- side_row("R", "vs RHH")
    wide <- dplyr::bind_rows(rL$row, rR$row)
    pct_mat <- rbind(rL$pcts, rR$pcts)  # rows in same order as wide

    g <- wide %>%
      gt::gt(rowname_col = "Side") %>%
      gt_theme_guardian() %>%
      gt::cols_align(align = "center", columns = gt::everything()) %>%
      gt::tab_options(column_labels.font.weight = "bold",
                      table.font.size = gt::px(15), data_row.padding = gt::px(10)) %>%
      gt::tab_style(style = gt::cell_text(weight = "bold"),
                    locations = gt::cells_stub())

    for (s in cl_stats) {
      local({
        col_s <- s
        pcts <- pct_mat[, col_s]
        g <<- g %>% gt::text_transform(
          locations = gt::cells_body(columns = col_s),
          fn = function(v) lapply(seq_along(v), function(i) shade_cell_html(v[i], pcts[i]))
        )
      })
    }
    g
  })

  # =================== VIZ TAB DATA + PLOTS ===================

  viz_data <- reactive({
    req(input$global_pitcher)
    df <- selected_data()
    req(is.data.frame(df), nrow(df) > 0)
    req(input$g_dateRange, input$g_BatterSide, input$g_pitchtype)
    out <- df %>%
      dplyr::filter(
        Pitcher == input$global_pitcher,
        safe_as_date(Date) >= as.Date(input$g_dateRange[1]) &
          safe_as_date(Date) <= as.Date(input$g_dateRange[2]),
        BatterSide %in% input$g_BatterSide,
        TaggedPitchType %in% input$g_pitchtype,
        RelSpeed >= input$g_velo[1] & RelSpeed <= input$g_velo[2]
      )
    apply_global_filters(out)
  })

  # Keep viz filters in sync with the active dataset
  observeEvent(selected_data(), {
    df <- selected_data()
    if ("Date" %in% names(df)) {
      rng <- range(df$Date, na.rm = TRUE)
      if (is.finite(rng[1]) && is.finite(rng[2]))
        updateDateRangeInput(session, "dateRange_viz", start = rng[1], end = rng[2])
    }
    if ("BatterSide" %in% names(df)) {
      bs <- sort(unique(df$BatterSide))
      updatePickerInput(session, "BatterSide_viz", choices = bs, selected = bs)
    }
  })

  # ---- Square (traditional) movement chart w/ arm-angle line + expected ----
  output$square_mvmt_plot <- renderPlot({
    df <- viz_data()
    validate(need(nrow(df) > 0, "No pitches for this selection."))
    df <- df %>% dplyr::filter(!is.na(HorzBreak), !is.na(InducedVertBreak),
                               TaggedPitchType != "Other")

    avg <- df %>% dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(HB = mean(HorzBreak, na.rm = TRUE),
                       IVB = mean(InducedVertBreak, na.rm = TRUE),
                       n = dplyr::n(), .groups = "drop")

    aa <- suppressWarnings(mean(df$arm_angle_savant, na.rm = TRUE))
    # Side the arm comes from: RHP (positive RelSide) slot tilts to +HB side, etc.
    rel_side_sign <- suppressWarnings(sign(mean(df$RelSide, na.rm = TRUE)))
    if (is.na(rel_side_sign) || rel_side_sign == 0) rel_side_sign <- 1
    lim <- 25

    p <- ggplot() +
      geom_hline(yintercept = 0, color = "gray55", linewidth = 0.4) +
      geom_vline(xintercept = 0, color = "gray55", linewidth = 0.4)

    # arm-angle diagonal line through origin.
    # arm_angle_savant is measured from horizontal (0 = sidearm, 90 = overhead),
    # using the same methodology as the rest of the app. The line rises at that
    # angle, tilting toward the side the pitcher's arm releases from.
    if (isTRUE(input$show_arm_angle_line) && is.finite(aa)) {
      slope <- tan(aa * pi / 180) * rel_side_sign
      p <- p + geom_abline(slope = slope, intercept = 0,
                           linetype = "dashed", color = "gray40", linewidth = 0.6)
    }

    if (isTRUE(input$show_individual_pitches)) {
      p <- p + geom_point(data = df,
                          aes(HorzBreak, InducedVertBreak, color = TaggedPitchType),
                          alpha = 0.35, size = 1.8)
    }

    if (isTRUE(input$show_expected_mvmt) && exists("exp_movement_grid")) {
      # expected movement per pitch type from the prebuilt grid (RelHeight bin)
      exp_pts <- df %>%
        dplyr::mutate(RH_bin = round(RelHeight / 0.10) * 0.10) %>%
        dplyr::left_join(exp_movement_grid,
                         by = c("TaggedPitchType", "RH_bin")) %>%
        dplyr::group_by(TaggedPitchType) %>%
        dplyr::summarise(HB = mean(exp_hb, na.rm = TRUE),
                         IVB = mean(exp_ivb, na.rm = TRUE), .groups = "drop") %>%
        dplyr::filter(!is.na(HB), !is.na(IVB))
      # expected grid is RHP-normalized; mirror HB for lefties
      if (isTRUE(toupper(substr(df$PitcherThrows[1], 1, 1)) == "L") &&
          nrow(exp_pts) > 0) exp_pts$HB <- -exp_pts$HB
      if (nrow(exp_pts) > 0) {
        p <- p + geom_point(data = exp_pts,
                            aes(HB, IVB, color = TaggedPitchType),
                            shape = 5, size = 5, stroke = 1.1)
      }
    }

    arm_lab <- if (isTRUE(input$show_arm_angle_line) && is.finite(aa))
      sprintf("Arm angle: %.0f\u00b0", aa) else NULL

    p2 <- p +
      geom_point(data = avg, aes(HB, IVB, fill = TaggedPitchType),
                 shape = 21, size = 7, color = "black", stroke = 0.6) +
      scale_color_manual(values = pitch_colors, name = "Pitch") +
      scale_fill_manual(values = pitch_colors, guide = "none") +
      coord_fixed(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
      labs(x = "Horizontal Break (in)", y = "Induced Vertical Break (in)") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            panel.border = element_rect(color = "black", fill = NA),
            legend.position = "bottom")

    if (!is.null(arm_lab)) {
      p2 <- p2 + annotate("label", x = -lim + 1, y = lim - 1, label = arm_lab,
                          hjust = 0, vjust = 1, size = 4, fontface = "italic",
                          color = "gray25", fill = "white", label.size = 0)
    }
    p2
  })

  # ===== Angle gauges: faceted by pitch type, flipped for handedness =====
  # Detect handedness of the selected pitcher (TRUE = lefty).
  viz_is_lefty <- reactive({
    df <- viz_data()
    if (is.null(df) || nrow(df) == 0) return(FALSE)
    tcol <- intersect(c("PitcherThrows","Throws"), names(df))
    if (length(tcol)) {
      tv <- na.omit(unique(as.character(df[[tcol[1]]])))[1]
      if (!is.na(tv)) return(toupper(substr(tv, 1, 1)) == "L")
    }
    isTRUE(median(df$RelSide, na.rm = TRUE) < 0)
  })

  # Build a faceted gauge data set for a metric across all pitch types.
  # metric_col: column to average; pool filtered the same way.
  # left_label/right_label: arc-end labels; high_is_left controls orientation.
  # Build linear gauge facets for a metric across all pitch types.
  # orient = "v" (Rise top / Depth bottom) or "h" (Sweep <-> Run).
  # For each pitch type: marker sits at its pool percentile along the axis.
  # Build half-circle arc gauge facets for a metric across all pitch types.
  # The arc is a semicircle; the marker sits on it at the pitch's pool percentile.
  # low_end/high_end name the two ends of the arc.
  build_gauge_facets <- function(df, pool, metric_col,
                                  low_label, high_label,
                                  low_word, high_word,
                                  metric_disp, min_n = 5, pool_min = 30,
                                  disp_col = NULL) {
    if (is.null(disp_col)) disp_col <- metric_col
    types <- df %>%
      dplyr::filter(!is.na(.data[[metric_col]]),
                    !is.na(TaggedPitchType), TaggedPitchType != "Other") %>%
      dplyr::count(TaggedPitchType) %>% dplyr::filter(n >= min_n) %>%
      dplyr::arrange(dplyr::desc(n)) %>% dplyr::pull(TaggedPitchType)
    if (length(types) == 0) return(NULL)

    R <- 1
    # Arc sweeps from 180deg (left end) to 0deg (right end), top semicircle.
    arc_base <- seq(180, 0, length.out = 100) * pi / 180
    arc_list <- list(); mark_list <- list(); txt_list <- list()
    for (pt in types) {
      val  <- mean(df[[metric_col]][df$TaggedPitchType == pt], na.rm = TRUE)
      disp <- mean(df[[disp_col]][df$TaggedPitchType == pt], na.rm = TRUE)
      pv  <- pool[[metric_col]][pool$TaggedPitchType == pt]
      pv  <- pv[!is.na(pv)]
      if (length(pv) < pool_min || is.na(val)) next
      pct_high <- mean(pv < val, na.rm = TRUE)   # share of pool BELOW this value
      pct_low  <- 1 - pct_high

      # t=0 -> left end (low_label), t=1 -> right end (high_label)
      t <- pct_high
      ang <- (180 - t * 180) * pi / 180
      mx <- R * cos(ang); my <- R * sin(ang)

      # green at both extremes, gray in the middle (extreme in either sense = notable)
      ext <- abs(seq(0, 1, length.out = length(arc_base)) - 0.5) * 2
      cols <- scales::gradient_n_pal(c("#9aa0a6", "#cfd3d6", "#2e9b3f"))(ext)
      arc_list[[pt]] <- tibble::tibble(TaggedPitchType = pt,
        x = R * cos(arc_base), y = R * sin(arc_base), col = cols)
      mark_list[[pt]] <- tibble::tibble(TaggedPitchType = pt, mx = mx, my = my)

      lead <- if (pct_high >= pct_low)
        sprintf("more %s than %d%%", high_word, round(pct_high * 100))
      else
        sprintf("more %s than %d%%", low_word, round(pct_low * 100))
      txt_list[[pt]] <- tibble::tibble(TaggedPitchType = pt,
        v_lab = sprintf("%s: %.1f\u00b0", metric_disp, disp), lead_lab = lead)
    }
    if (length(arc_list) == 0) return(NULL)
    list(arc = dplyr::bind_rows(arc_list),
         mark = dplyr::bind_rows(mark_list),
         txt = dplyr::bind_rows(txt_list),
         types = names(arc_list),
         low_label = low_label, high_label = high_label)
  }

  render_gauge_facets <- function(g) {
    n <- length(g$types); ncol <- min(3, n)
    ggplot() +
      geom_path(data = g$arc, aes(x, y, color = col, group = TaggedPitchType),
                linewidth = 6, lineend = "round") +
      scale_color_identity() +
      # pointer from arc center up to the marker on the arc
      geom_segment(data = g$mark, aes(x = 0, y = 0, xend = mx, yend = my),
                   color = "#c0392b", linewidth = 1.1,
                   arrow = grid::arrow(length = unit(0.13, "inches"))) +
      geom_point(data = g$mark, aes(mx, my), size = 4.5, color = "#111",
                 fill = "white", shape = 21, stroke = 1.2) +
      geom_text(data = g$txt, aes(x = 0, y = 1.52, label = v_lab),
                hjust = 0.5, size = 4, fontface = "bold") +
      geom_text(data = g$txt, aes(x = 0, y = 1.34, label = lead_lab),
                hjust = 0.5, size = 3.3, color = "#2e9b3f", fontface = "bold") +
      annotate("text", x = -1.12, y = 0.02, label = g$low_label,
               size = 3.7, fontface = "bold", hjust = 1) +
      annotate("text", x =  1.12, y = 0.02, label = g$high_label,
               size = 3.7, fontface = "bold", hjust = 0) +
      coord_fixed(xlim = c(-1.5, 1.5), ylim = c(-0.1, 1.66), clip = "off") +
      facet_wrap(~ TaggedPitchType, ncol = ncol) +
      theme_void(base_size = 13) +
      theme(strip.text = element_text(face = "bold", size = 14, color = "#0E6E70"),
            panel.spacing = unit(14, "pt"),
            plot.margin = margin(6, 10, 6, 10))
  }

  # ---- VAA vertical gauge (Rise top / Depth bottom), faceted by pitch type ----
  output$vaa_percentile_plot_legacy <- renderPlot({
    df <- viz_data() %>% dplyr::filter(!is.na(VertApprAngle))
    validate(need(nrow(df) > 0, "No VAA data."))
    pool <- grade_pool_df() %>% dplyr::filter(!is.na(VertApprAngle))
    validate(need(nrow(pool) > 50, "Insufficient pool data for VAA."))
    # Higher (less negative) VAA = more rise = high (right) end of the arc.
    g <- build_gauge_facets(df, pool, "VertApprAngle",
      low_label = "Depth", high_label = "Rise",
      low_word = "depth", high_word = "rise",
      metric_disp = "VAA")
    validate(need(!is.null(g), "Not enough data per pitch type for VAA."))
    render_gauge_facets(g)
  }, height = function() {
    n <- tryCatch(length(build_gauge_facets(
      viz_data() %>% dplyr::filter(!is.na(VertApprAngle)),
      grade_pool_df() %>% dplyr::filter(!is.na(VertApprAngle)),
      "VertApprAngle","d","r","d","r","VAA")$types), error = function(e) 3)
    if (is.null(n) || n == 0) n <- 3
    300 * ceiling(n / 3)
  })

  # ---- HAA arc gauge (Sweep <-> Run), faceted; handedness-aware ----
  #   We orient the arc so glove-side Sweep is the LEFT end and arm-side Run is
  #   the RIGHT end for a RHP (mirrored for a LHP). Because the marker position
  #   is percentile-driven, we negate the value where needed so the marker and
  #   the end labels always agree.
  #   TrackMan convention: +HAA = ball crossing toward 3B side.
  #     RHP glove-side sweep -> +HAA ; arm-side run -> -HAA
  #   To put Sweep (left) / Run (right) for a RHP we negate HAA so that
  #   larger (right end) = more run; LHP keeps the raw sign.
  output$haa_percentile_plot <- renderPlot({
    lefty <- viz_is_lefty()
    df <- viz_data() %>% dplyr::filter(!is.na(HorzApprAngle))
    validate(need(nrow(df) > 0, "No HAA data."))
    pool <- grade_pool_df() %>% dplyr::filter(!is.na(HorzApprAngle))
    validate(need(nrow(pool) > 50, "Insufficient pool data for HAA."))

    # Value shown to the user stays the raw HAA; only the axis used to POSITION
    # the marker is oriented so the right end = Run for a RHP.
    df$.pos   <- if (lefty)  df$HorzApprAngle   else -df$HorzApprAngle
    pool$.pos <- if (lefty)  pool$HorzApprAngle else -pool$HorzApprAngle
    df$.disp   <- df$HorzApprAngle

    low_label <- "Sweep"; high_label <- "Run"
    low_word  <- "sweep"; high_word  <- "run"
    if (lefty) { low_label <- "Run"; high_label <- "Sweep"; low_word <- "run"; high_word <- "sweep" }

    g <- build_gauge_facets(df, pool, ".pos",
      low_label = low_label, high_label = high_label,
      low_word = low_word, high_word = high_word,
      metric_disp = "HAA", disp_col = ".disp")
    validate(need(!is.null(g), "Not enough data per pitch type for HAA."))
    render_gauge_facets(g)
  }, height = function() {
    n <- tryCatch({
      d <- viz_data() %>% dplyr::filter(!is.na(HorzApprAngle))
      p <- grade_pool_df() %>% dplyr::filter(!is.na(HorzApprAngle))
      d$.pos <- d$HorzApprAngle; p$.pos <- p$HorzApprAngle
      length(build_gauge_facets(d, p, ".pos","s","r","s","r","HAA")$types)
    }, error = function(e) 3)
    if (is.null(n) || n == 0) n <- 3
    300 * ceiling(n / 3)
  })

  # ---- Approach angles: VAA vs HAA scatter by pitch type ----
  output$approach_angle_plot_legacy <- plotly::renderPlotly({
    df <- viz_data() %>%
      dplyr::filter(!is.na(VertApprAngle), !is.na(HorzApprAngle),
                    !is.na(TaggedPitchType), TaggedPitchType != "Other")
    validate(need(nrow(df) > 0, "No approach-angle data."))
    centers <- df %>% dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(VAA = mean(VertApprAngle, na.rm = TRUE),
                       HAA = mean(HorzApprAngle, na.rm = TRUE), .groups = "drop")
    df <- df %>% dplyr::mutate(hover = paste0("<b>", TaggedPitchType, "</b>",
      "<br>VAA: ", sprintf("%.1f", VertApprAngle),
      "<br>HAA: ", sprintf("%.1f", HorzApprAngle)))
    p <- ggplot(df, aes(HorzApprAngle, VertApprAngle, fill = TaggedPitchType, text = hover)) +
      geom_vline(xintercept = 0, color = "gray70", linewidth = 0.4) +
      geom_point(alpha = 0.5, shape = 21, color = "black", stroke = 0.2, size = 2.6) +
      geom_point(data = centers, aes(HAA, VAA, fill = TaggedPitchType),
                 shape = 21, color = "black", stroke = 0.8, size = 6, inherit.aes = FALSE) +
      scale_fill_manual(values = pitch_colors, name = "Pitch") +
      labs(x = "Horizontal Approach Angle (\u00b0)", y = "Vertical Approach Angle (\u00b0)") +
      theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
    plotly::ggplotly(p, tooltip = "text")
  })

  # ---- Release angles: VRA vs HRA scatter by pitch type ----
  output$release_angle_plot_legacy <- plotly::renderPlotly({
    validate(need(all(c("VertRelAngle","HorzRelAngle") %in% names(viz_data())),
                  "Release-angle columns not available in this data."))
    df <- viz_data() %>%
      dplyr::filter(!is.na(VertRelAngle), !is.na(HorzRelAngle),
                    !is.na(TaggedPitchType), TaggedPitchType != "Other")
    validate(need(nrow(df) > 0, "No release-angle data."))
    centers <- df %>% dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(VRA = mean(VertRelAngle, na.rm = TRUE),
                       HRA = mean(HorzRelAngle, na.rm = TRUE), .groups = "drop")
    df <- df %>% dplyr::mutate(hover = paste0("<b>", TaggedPitchType, "</b>",
      "<br>VRA: ", sprintf("%.1f", VertRelAngle),
      "<br>HRA: ", sprintf("%.1f", HorzRelAngle)))
    p <- ggplot(df, aes(HorzRelAngle, VertRelAngle, fill = TaggedPitchType, text = hover)) +
      geom_vline(xintercept = 0, color = "gray70", linewidth = 0.4) +
      geom_point(alpha = 0.5, shape = 21, color = "black", stroke = 0.2, size = 2.6) +
      geom_point(data = centers, aes(HRA, VRA, fill = TaggedPitchType),
                 shape = 21, color = "black", stroke = 0.8, size = 6, inherit.aes = FALSE) +
      scale_fill_manual(values = pitch_colors, name = "Pitch") +
      labs(x = "Horizontal Release Angle (\u00b0)", y = "Vertical Release Angle (\u00b0)") +
      theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
    plotly::ggplotly(p, tooltip = "text")
  })

  # ---- Arm angle visualization (reuses plot_arm_angles_app) ----
  output$arm_angle_plot <- renderPlot({
    req(input$global_pitcher)
    df <- viz_data()
    validate(need(nrow(df) > 0, "No data for arm angle."))
    tryCatch(
      plot_arm_angles_app(df, input$global_pitcher, pitch_colors,
                          view = "mound", show_pitcher_figure = TRUE,
                          show_legend = TRUE),
      error = function(e) {
        ggplot() + theme_void() +
          annotate("text", x = 0, y = 0, label = "Arm angle unavailable", size = 5, color = "gray50")
      }
    )
  })

  # ---- Release height vs Extension (D1 anchors 5.8 ext / 5.7 height) ----
  output$rel_ext_plot <- renderPlot({
    df <- viz_data() %>%
      dplyr::filter(!is.na(Extension), !is.na(RelHeight), TaggedPitchType != "Other")
    validate(need(nrow(df) > 0, "No release/extension data."))
    avg <- df %>% dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(Ext = mean(Extension, na.rm = TRUE),
                       RelH = mean(RelHeight, na.rm = TRUE), .groups = "drop")

    ggplot() +
      geom_vline(xintercept = 5.8, linetype = "dashed", color = "gray50") +
      geom_hline(yintercept = 5.7, linetype = "dashed", color = "gray50") +
      annotate("text", x = 5.8, y = 7.4, label = "D1 ext (5.8)",
               size = 2.8, color = "gray45", hjust = -0.05) +
      annotate("text", x = 4.2, y = 5.7, label = "D1 height (5.7)",
               size = 2.8, color = "gray45", vjust = -0.6) +
      geom_point(data = df, aes(Extension, RelHeight, color = TaggedPitchType),
                 alpha = 0.3, size = 1.4) +
      geom_point(data = avg, aes(Ext, RelH, fill = TaggedPitchType),
                 shape = 21, size = 5.5, color = "black", stroke = 0.6) +
      scale_color_manual(values = pitch_colors, guide = "none") +
      scale_fill_manual(values = pitch_colors, name = "Pitch") +
      coord_fixed(xlim = c(4, 7.5), ylim = c(4, 7.5)) +
      labs(x = "Extension (ft)", y = "Release Height (ft)") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom",
            panel.grid.minor = element_blank())
  })

  # ---- Release Consistency: rings centered on avg fastball (or most-thrown) ----
  output$release_consistency_plot <- renderPlot({
    df <- viz_data() %>%
      dplyr::filter(!is.na(RelSide), !is.na(RelHeight), TaggedPitchType != "Other")
    validate(need(nrow(df) > 0, "No release point data."))

    # Anchor = average release of the fastball if present, else most-thrown pitch.
    counts <- df %>% dplyr::count(TaggedPitchType, sort = TRUE)
    anchor_type <- if (any(counts$TaggedPitchType == "Fastball")) "Fastball" else counts$TaggedPitchType[1]
    anchor <- df %>% dplyr::filter(TaggedPitchType == anchor_type) %>%
      dplyr::summarise(RelSide = mean(RelSide, na.rm = TRUE),
                       RelHeight = mean(RelHeight, na.rm = TRUE))

    # Per-pitch averages, expressed in INCHES relative to the anchor.
    avg <- df %>% dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(RelSide = mean(RelSide, na.rm = TRUE),
                       RelHeight = mean(RelHeight, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(dx = (RelSide - anchor$RelSide) * 12,
                    dy = (RelHeight - anchor$RelHeight) * 12)

    # Ring radii in inches
    rings <- tibble::tibble(r = c(3, 6, 9, 12))
    circ <- function(r) {
      th <- seq(0, 2*pi, length.out = 100)
      tibble::tibble(x = r*cos(th), y = r*sin(th), r = r)
    }
    ring_df <- purrr::map_dfr(rings$r, circ)
    lim <- 13

    ggplot() +
      geom_path(data = ring_df, aes(x, y, group = r),
                color = "gray75", linewidth = 0.4) +
      # 3-inch detectability ring highlighted
      geom_path(data = circ(3), aes(x, y), color = "#c0392b",
                linewidth = 0.9, linetype = "longdash") +
      annotate("text", x = 0, y = 3.6, label = "3\" = hitter-detectable",
               size = 3, color = "#c0392b") +
      geom_hline(yintercept = 0, color = "gray80", linewidth = 0.3) +
      geom_vline(xintercept = 0, color = "gray80", linewidth = 0.3) +
      geom_point(data = avg, aes(dx, dy, fill = TaggedPitchType),
                 shape = 21, size = 7, color = "black", stroke = 0.6) +
      scale_fill_manual(values = pitch_colors, name = "Pitch") +
      coord_fixed(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
      labs(x = NULL, y = NULL,
           caption = sprintf("Center = avg %s release", anchor_type)) +
      theme_void(base_size = 12) +
      theme(legend.position = "bottom",
            plot.caption = element_text(hjust = 0.5, color = "gray45"))
  })

  # ---- Locations heatmap (plotly): hover for pitch detail, split by hand ----
  # Two sections (vs LHH / vs RHH) side by side; within each, rows = count
  # split (0-0 / Pre-2K / 2 Strikes), columns = pitch type.
  # Map a pitch's outcome to one of the detail categories used for shapes.
  classify_detail <- function(PitchCall, PlayResult, KorBB) {
    dplyr::case_when(
      PlayResult == "Single"               ~ "1B",
      PlayResult == "Double"               ~ "2B",
      PlayResult == "Triple"               ~ "3B",
      PlayResult == "HomeRun"              ~ "HR",
      PlayResult == "Error"                ~ "Error",
      PlayResult %in% c("Sacrifice","SacBunt","SacFly") ~ "Sac",
      PlayResult == "Out" | PlayResult == "FieldersChoice" ~ "Out",
      PitchCall == "HitByPitch"            ~ "HBP",
      PitchCall == "StrikeSwinging"        ~ "Whiff",
      PitchCall %in% c("StrikeCalled")     ~ "Called",
      PitchCall %in% c("FoulBall","FoulBallNotFieldable","FoulBallFieldable") ~ "Foul",
      PitchCall %in% c("BallCalled","BallinDirt","BallIntentional") ~ "Ball",
      TRUE                                 ~ "Other"
    )
  }

  .detail_shape_map <- c(
    "Ball" = 4, "2B" = 18, "Foul" = 2, "HBP" = 10, "Sac" = 3,
    "1B" = 19, "Called" = 5, "Whiff" = 8, "3B" = 17, "HR" = 15,
    "Out" = 16, "Error" = 0, "Other" = 1
  )

  build_location_plotly <- function(df, side_label, layer = "both") {
    if (nrow(df) == 0) {
      return(plotly::plotly_empty(type = "scatter", mode = "markers") %>%
               plotly::layout(title = paste0("vs ", side_label, ": no data")))
    }
    zl <- -0.83083; zr <- 0.83083; zb <- 1.5; zt <- 3.3775

    df <- df %>%
      dplyr::mutate(
        TaggedPitchType = forcats::fct_infreq(TaggedPitchType),
        CountSplit = as.character(CountSplit),
        Detail = classify_detail(PitchCall, PlayResult,
                                 if ("KorBB" %in% names(df)) KorBB else NA),
        Detail = factor(Detail, levels = names(.detail_shape_map)),
        hover = paste0(
          "<b>", TaggedPitchType, "</b>",
          "<br>Result: ", Detail,
          "<br>Velo: ", ifelse(is.na(RelSpeed), "-", sprintf("%.1f", RelSpeed)),
          "<br>IVB/HB: ", ifelse(is.na(InducedVertBreak), "-", sprintf("%.1f", InducedVertBreak)),
          " / ", ifelse(is.na(HorzBreak), "-", sprintf("%.1f", HorzBreak)),
          "<br>Stuff+: ", ifelse(is.na(stuff_plus), "-", sprintf("%.0f", stuff_plus)),
          "<br>Date: ", ifelse(is.na(Date), "-", as.character(Date)),
          "<br>Opp: ", if ("BatterTeam" %in% names(df)) prettify_team(BatterTeam) else "-"
        )
      )

    # Overall (All-pitch) faceted row on top, then the per-count situation rows.
    df_all <- df %>% dplyr::mutate(CountSplit = "All")
    df <- dplyr::bind_rows(df_all, df) %>%
      dplyr::mutate(CountSplit = factor(CountSplit,
                     levels = c("All", "0-0", "Pre-2K", "2 Strikes")))

    p <- ggplot(df, aes(PlateLocSide, PlateLocHeight))
    if (layer %in% c("both", "heat")) {
      # Higher bin count + expanded fill scale keeps the map saturated further
      # from the core so it does not fade out quickly.
      p <- p +
        stat_density2d(aes(fill = after_stat(level)), geom = "polygon",
                       bins = 12, alpha = 0.9) +
        scale_fill_gradientn(
          colours = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#f46d43", "#d7191c", "#a50026"),
          values = scales::rescale(c(0, 0.15, 0.35, 0.55, 0.72, 0.88, 1)),
          guide = "none")
    }
    if (layer %in% c("both", "points")) {
      p <- p +
        geom_point(aes(text = hover, shape = Detail), alpha = 0.85,
                   size = 1.4, color = "black", stroke = 0.7) +
        scale_shape_manual(values = .detail_shape_map, name = "Result", drop = FALSE)
    }
    p <- p +
      annotate("rect", xmin = zl, xmax = zr, ymin = zb, ymax = zt,
               fill = NA, color = "black", linewidth = 0.6) +
      coord_fixed(xlim = c(-2, 2), ylim = c(0.5, 4.5)) +
      facet_grid(CountSplit ~ TaggedPitchType) +
      labs(title = paste("vs", side_label)) +
      theme_void(base_size = 11) +
      theme(strip.text = element_text(face = "bold", size = 11),
            panel.border = element_rect(color = "gray70", fill = NA),
            panel.spacing = unit(4, "pt"),
            plot.title = element_text(hjust = 0.5, face = "bold"))

    plotly::ggplotly(p, tooltip = "text")
  }

  location_base_data <- reactive({
    req(input$global_pitcher)
    df0 <- selected_data()
    req(is.data.frame(df0), nrow(df0) > 0)
    req(input$g_dateRange, input$g_pitchtype)
    out <- df0 %>%
      dplyr::filter(
        Pitcher == input$global_pitcher,
        safe_as_date(Date) >= as.Date(input$g_dateRange[1]) &
          safe_as_date(Date) <= as.Date(input$g_dateRange[2]),
        TaggedPitchType %in% input$g_pitchtype,
        RelSpeed >= input$g_velo[1] & RelSpeed <= input$g_velo[2],
        !is.na(PlateLocSide), !is.na(PlateLocHeight),
        !is.na(TaggedPitchType), TaggedPitchType != "Other"
      )
    out <- apply_global_filters(out)
    out %>%
      dplyr::mutate(
        CountSplit = dplyr::case_when(
          Balls == 0 & Strikes == 0 ~ "0-0",
          Strikes == 2              ~ "2 Strikes",
          TRUE                      ~ "Pre-2K"
        )
      )
  })

  output$locations_single <- plotly::renderPlotly({
    side <- input$heatmap_side %||% "R"
    side_label <- if (side == "L") "LHH" else "RHH"
    df <- location_base_data() %>% dplyr::filter(norm_side(BatterSide) == side)
    validate(need(nrow(df) > 0, paste0("No vs-", side_label, " location data.")))
    build_location_plotly(df, side_label, layer = input$heatmap_layer %||% "both")
  })
