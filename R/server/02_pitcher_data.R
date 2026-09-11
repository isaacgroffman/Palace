  coastal_selected_data <- reactive({
    req(input$season_type)
    
    season_map <- list(
      "Spring25" = function() {
        if (!exists("data") || !is.data.frame(data) || nrow(data) == 0) return(NULL)
        data
      },
      "Fall25" = function() {
        if (!exists("fall25") || !is.data.frame(fall25) || nrow(fall25) == 0) return(NULL)
        fall25
      },
      "PreSpring26" = function() {
        if (!exists("prespring") || !is.data.frame(prespring) || nrow(prespring) == 0) return(NULL)
        prespring
      },
      "Fall26" = function() {
        # Fall 2026 = bullpen season: no game data exists. Return a ZERO-ROW
        # frame with the game schema so every pill shows its normal empty
        # state. The Bullpens pill reads Supabase directly regardless.
        empty_game_frame()
      },
      "Spring26" = function() {
        if (!exists("spring26") || !is.data.frame(spring26) || nrow(spring26) == 0) return(NULL)
        df <- spring26
        
        # Exclude manual data rows if checkbox is unchecked
        if (!isTRUE(input$include_manual_data) && manual_data_loaded) {
          manual_dates <- as.Date(c("2026-02-27", "2026-02-28", "2026-03-01"))
          df <- df %>% 
            filter(!(Date %in% manual_dates & is.na(ax0) & is.na(az0) & is.na(vx0)))
        }
        
        # (Game filtering happens downstream in apply_global_filters so the
        # per-pitcher game list always builds from the FULL season data —
        # filtering here would shrink the picker's own choices.)
        df
      }
    )
    
    # Collect all selected seasons
    dfs <- lapply(input$season_type, function(s) {
      fn <- season_map[[s]]
      if (!is.null(fn)) fn() else NULL
    })
    dfs <- Filter(Negate(is.null), dfs)
    
    if (length(dfs) == 0) return(empty_game_frame())
    if (length(dfs) == 1) return(dfs[[1]])
    
    # Find common columns across all selected datasets
    common_cols <- Reduce(intersect, lapply(dfs, names))

    dfs_common <- lapply(dfs, function(df) df %>% select(all_of(common_cols)))
    combined <- bind_rows(harmonize_types(dfs_common))
    
    # Convert dates back
    if ("Date" %in% names(combined)) combined$Date <- as.Date(combined$Date)
    if ("UTCDate" %in% names(combined)) combined$UTCDate <- as.Date(combined$UTCDate)
    
    return(combined)
  })

  # ===== NCAA routing =====
  # When the global player is a non-Coastal (NCAA) pitcher, every tab
  # reads that pitcher's lazily-loaded NCAA data instead of the Coastal
  # season datasets. Season picker maps Spring25 -> 2025 rows and
  # Spring26 -> 2026 rows; everything else falls back to all rows.
  ncaa_selected_data <- reactive({
    gp <- input$global_pitcher
    req(gp, nzchar(gp))
    ncaa_full_ready()   # re-fires when the fully scored frame lands
    df <- ncaa_best_data(gp)
    ncaa_schedule_full_load(gp)
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(data.frame())

    yrs <- c(
      if ("Spring25" %in% input$season_type) 2025,
      if ("Spring26" %in% input$season_type) 2026
    )
    if (length(yrs) > 0 && "Date" %in% names(df)) {
      dfy <- df %>% filter(!is.na(Date), lubridate::year(Date) %in% yrs)
      if (nrow(dfy) > 0) df <- dfy
    }
    df
  })

  selected_data <- reactive({
    gp <- input$global_pitcher
    if (!is.null(gp) && nzchar(gp) && !is_coastal_ctx(gp, input$season_type) && is_ncaa_pitcher(gp)) {
      return(ncaa_selected_data())
    }
    coastal_selected_data()
  })


  # Helper functions for advanced movement chart
  pitcher_handedness <- function(df) {
    if ("P_Throws" %in% names(df) && any(!is.na(df$P_Throws))) {
      t <- toupper(substr(stats::na.omit(df$P_Throws)[1], 1, 1))
      ifelse(t %in% c("L","R"), t, ifelse(median(df$RelSide, na.rm = TRUE) < 0, "L", "R"))
    } else {
      ifelse(median(df$RelSide, na.rm = TRUE) < 0, "L", "R")
    }
  }
  
  p5_row_handed <- function(side) {
    ifelse(is.na(side), NA_character_, ifelse(side < 0, "L", "R"))
  }
  
  shoulder_y_from_df <- function(df) {
    h_in <- if ("height_inches" %in% names(df) && any(!is.na(df$height_inches))) median(df$height_inches, na.rm = TRUE) else 72
    (h_in * 0.70) / 12
  }
  
  ray_end_for_type <- function(mean_rel_side, mean_rel_height, length_units, shoulder_y) {
    dx <- abs(mean_rel_side)
    dy <- max(mean_rel_height - shoulder_y, 0)
    mag <- sqrt(dx^2 + dy^2)
    if (!is.finite(mag) || mag == 0) return(c(0,0))
    ux <- dx / mag; uy <- dy / mag
    x_sign <- ifelse(mean_rel_side < 0, -1, 1)
    c(x_sign * ux * length_units, uy * length_units)
  }
  
  circle_shape <- function(r){
    list(type="circle", xref="x", yref="y",
         x0=-r, x1=r, y0=-r, y1=r,
         line=list(color="rgba(128,128,128,0.3)", dash="dot", width=1),
         fillcolor="rgba(0,0,0,0)")
  }
  
  pitch_factor <- function(x) {
    factor(x, levels = names(pitch_colors))
  }
  
  # Global P5 slot grid
  P5_slot_grid <- p5_slot_grid   # built once in R/07_pools.R (or read as an artifact)
  
  # Copy the full create_advanced_movement_plot function from the second document
  create_advanced_movement_plot <- function(df, pitcher_name, show_points = TRUE,
                                            P5_slot_grid_override = NULL) {
    df <- df %>% filter(!is.na(TaggedPitchType), TaggedPitchType != "Other",
                        !is.na(HorzBreak), !is.na(InducedVertBreak), !is.na(RelSpeed))
    if (nrow(df) == 0) return(plotly::plotly_empty("No data available"))
    
    total <- nrow(df)
    shoulder_y <- shoulder_y_from_df(df)
    hand <- pitcher_handedness(df)  # "L" or "R"
    
    # choose P5 grid (same-handed override if provided)
    P5_slot_grid_local <- if (!is.null(P5_slot_grid_override)) P5_slot_grid_override else P5_slot_grid
    
    # per type means (movement + release)
    pitch_summary <- df %>%
      group_by(TaggedPitchType) %>%
      summarise(
        mean_hb   = median(HorzBreak, na.rm = TRUE),
        mean_ivb  = median(InducedVertBreak, na.rm = TRUE),
        mean_velo = round(mean(RelSpeed, na.rm = TRUE), 1),
        RelHeight = mean(RelHeight, na.rm = TRUE),
        RelSide   = mean(RelSide,   na.rm = TRUE),
        usage_pct = round(n()/total*100, 1),
        .groups = "drop"
      ) %>%
      filter(usage_pct >= 1) %>%
      mutate(RH_bin = round(RelHeight/0.10)*0.10) %>%
      left_join(P5_slot_grid_local, by = c("TaggedPitchType","RH_bin")) %>%
      mutate(
        dHB_slot  = round(mean_hb  - slot_hb,  1),
        dIVB_slot = round(mean_ivb - slot_ivb, 1),
        hover_mean = paste0(
          "<b>", TaggedPitchType, " Avg</b><br>",
          "Velo: ", mean_velo, " mph<br>",
          "HB: ", sprintf("%.1f", mean_hb), " in (vs P5@slot ", ifelse(is.na(dHB_slot),"—",sprintf("%+.1f", dHB_slot)), ")<br>",
          "IVB: ", sprintf("%.1f", mean_ivb), " in (vs P5@slot ", ifelse(is.na(dIVB_slot),"—",sprintf("%+.1f", dIVB_slot)), ")"
        )
      )
    
    ray_len <- 12
    arm_df <- pitch_summary %>%
      transmute(
        TaggedPitchType,
        x0 = 0, y0 = 0,
        x1y1 = pmap(list(RelSide, RelHeight), ~ray_end_for_type(..1, ..2, ray_len, shoulder_y)),
        x1 = map_dbl(x1y1, 1),
        y1 = map_dbl(x1y1, 2)
      ) %>% select(-x1y1)
    
    # shapes (rings + axes)
    shapes <- c(
      list(
        list(type="line", x0=0, x1=0, y0=-25, y1=25, line=list(color="rgba(128,128,128,0.6)", dash="dash", width=1)),
        list(type="line", y0=0, y1=0, x0=-25, x1=25, line=list(color="rgba(128,128,128,0.6)", dash="dash", width=1))
      ),
      lapply(c(6,12,18,24), circle_shape)
    )
    
    # base layout
    p <- plot_ly() %>%
      layout(
        xaxis = list(title = "Horizontal Break (in)", range = c(-27.5, 27.5),
                     zeroline = FALSE, showgrid = FALSE, scaleanchor="y", scaleratio=1),
        yaxis = list(title = "Induced Vertical Break (in)", range = c(-27.5, 27.5),
                     zeroline = FALSE, showgrid = FALSE),
        shapes = shapes,
        margin = list(l=40,r=20,b=50,t=50),
        title = list(text = paste("Interactive Pitch Movement:", pitcher_name),
                     font = list(size = 18, family = "Arial", color = "black")),
        plot_bgcolor = "white", paper_bgcolor = "white",
        font = list(family = "Arial", size = 15),
        legend = list(
          orientation = "v",
          x = 0.02, y = 0.98, xanchor = "left", yanchor = "top",
          bgcolor = "rgba(255,255,255,0.9)", bordercolor = "rgba(0,0,0,0.25)", borderwidth = 1,
          font = list(size = 15
          )
        )
      )
    
    # optional per-pitch points with rich hover (metrics, result, date)
    if (isTRUE(show_points)) {
      df <- df %>%
        mutate(
          Spin_txt   = if ("SpinRate"   %in% names(.)) ifelse(is.na(SpinRate),"", paste0("Spin: ", round(SpinRate,0), " rpm<br>")) else "",
          Result_txt = if ("PlayResult" %in% names(.)) ifelse(is.na(PlayResult),"", paste0("Result: ", PlayResult, "<br>")) else "",
          Call_txt   = if ("PitchCall"  %in% names(.)) ifelse(is.na(PitchCall), "", paste0("Call: ",   PitchCall,  "<br>")) else "",
          Date_txt   = if ("Date"       %in% names(.)) paste0("Date: ", format(Date)) else "",
          hover_pt = paste0(
            "<b>", TaggedPitchType, "</b><br>",
            "Velo: ", round(RelSpeed,1), " mph<br>",
            "Stuff+: ", round(stuff_plus,1), "<br>",
            "HB: ", round(HorzBreak,1), " in<br>",
            "IVB: ", round(InducedVertBreak,1), " in<br>",
            Spin_txt, Result_txt, Call_txt, Date_txt
          )
        )
      
      p <- p %>%
        add_markers(
          data  = df,
          x = ~HorzBreak, y = ~InducedVertBreak,
          color  = ~pitch_factor(TaggedPitchType),
          colors = pitch_colors,
          size = I(15), opacity = 1,                             # bigger + higher alpha
          marker = list(line = list(color = "black", width = 1)),   # black outline
          text = ~hover_pt, hovertemplate = "%{text}<extra></extra>",
          name = "Pitches", legendgroup = "pitches", showlegend = FALSE
        )
    }
    
    # arm-angle rays per type
    p <- p %>%
      add_segments(
        data = arm_df,
        x = ~x0, y = ~y0, xend = ~x1, yend = ~y1,
        color  = ~pitch_factor(TaggedPitchType),
        colors = pitch_colors,
        inherit = FALSE, showlegend = FALSE,
        line = list(width = 6, color = NULL),
        hoverinfo = "none"
      ) %>%
      add_markers(
        data = arm_df, x = ~x1, y = ~y1,
        color  = ~pitch_factor(TaggedPitchType),
        colors = pitch_colors,
        marker = list(size = 9, line = list(color = "black", width = 1)),
        showlegend = FALSE, hoverinfo = "none"
      )
    
    # expected P5 @ slot marker (SAME-HANDED; solid diamond) + mean dots
    p <- p %>%
      add_markers(
        data = pitch_summary %>% filter(!is.na(slot_hb), !is.na(slot_ivb)),
        x = ~slot_hb, y = ~slot_ivb,
        color  = ~pitch_factor(TaggedPitchType),
        colors = pitch_colors,
        marker = list(size = 17, symbol = "diamond", line = list(width = 2, color = "black")),
        opacity = 0.95, showlegend = FALSE,
        text = ~paste0("<b>P5 @ slot (", hand, ")</b><br>HB: ", sprintf("%.1f", slot_hb),
                       " in<br>IVB: ", sprintf("%.1f", slot_ivb), " in"),
        hovertemplate = "Expected Movement for Arm Slot"
      ) %>%
      add_markers(
        data  = pitch_summary,
        x = ~mean_hb, y = ~mean_ivb,
        color  = ~pitch_factor(TaggedPitchType),
        colors = pitch_colors,
        size = I(20), opacity = 1,
        marker = list(symbol = "circle", line = list(color = "black", width = 2)),
        text = ~hover_mean, hovertemplate = "%{text}<extra></extra>",
        name = ~TaggedPitchType, legendgroup = ~TaggedPitchType
      ) %>%
      add_annotations(
        data = pitch_summary, x = ~mean_hb, y = ~mean_ivb,
        text = ~as.character(mean_velo), showarrow = FALSE,
        font = list(color = "black", size = 12, family = "Arial Black"),
        bgcolor = "rgba(255,255,255,0.85)", bordercolor = "black", borderwidth = 1
      )
    
    # config bar
    p <- p %>% plotly::config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("pan2d","select2d","lasso2d","autoScale2d"),
      displaylogo = FALSE
    )
    
    p
  }
  
  
  mound_curve <- data.frame(
    x = c(seq(-3, 3, length.out = 100), rev(seq(-3, 3, length.out = 100))),
    y = c(rep(0, 100), sqrt(9 - seq(-3, 3, length.out = 100)^2) * 0.15)
  )
  
  
  
  filtered_data <- reactive({
    df <- selected_data()
    
    # Safety check
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      cat("WARNING: selected_data() returned invalid data\n")
      return(empty_pitch_frame())  # typed 0-row frame keeps downstream filters alive
    }
    
    # CRITICAL FIX: Don't require pitcher selection before data loads
    req(input$g_dateRange, input$g_BatterSide, input$g_pitchtype, input$g_velo)
    
    # Start with full dataset
    filtered <- df
    
    # Filter by the selected pitcher. A pitcher the frame does not contain
    # yields an EMPTY frame (never the whole staff); hitter picks are parked
    # behind hidden tabs and keep the unfiltered frame as before.
    gp <- input$global_pitcher
    if (!is.null(gp) && nzchar(gp) && !is_hitter_pick(gp)) {
      filtered <- filtered %>% filter(Pitcher == gp)
    }
    
    # Apply other filters
    filtered <- filtered %>%
      filter(
        safe_as_date(Date) >= as.Date(input$g_dateRange[1]) &
          safe_as_date(Date) <= as.Date(input$g_dateRange[2]),
        BatterSide %in% input$g_BatterSide,
        TaggedPitchType %in% input$g_pitchtype,
        RelSpeed >= input$g_velo[1] & RelSpeed <= input$g_velo[2]
      )

    filtered <- apply_global_filters(filtered)

    return(filtered)
  })
  
  filtered_pitcher_choices <- reactive({
    df <- selected_data()
    
    if (!"Pitcher" %in% names(df)) return(character(0))
    
    if (exists("bio") && "Roster_2026" %in% names(bio)) {
      Roster_filter <- if ("Roster_2026" %in% names(input) && !is.null(input$g_Roster)) {
        input$g_Roster
      } else {
        "Yes"  
      }
      
      filtered_bio <- bio %>% filter(`Roster_2026` %in% Roster_filter)
      available_pitchers <- df %>% 
        filter(Pitcher %in% filtered_bio$Pitcher_FL) %>% 
        pull(Pitcher) %>% 
        unique() %>% 
        sort()
    } else {
      available_pitchers <- sort(unique(df$Pitcher))
    }
    
    return(available_pitchers)
  })
  
  
  if (!exists("pitch_colors")) {
    pitch_colors <- c("Fastball" = '#FA8072',
                      "Four-Seam" = '#FA8072',
                      "Sinker" = "#fdae61",
                      "Slider" = "#A020F0",
                      "Sweeper" = "magenta",
                      "Curveball" = '#2c7bb6',
                      "ChangeUp" = '#90EE90',
                      "Splitter" = '#90EE32',
                      "Cutter" = "red")
  }
  pitch_color_fn <- function(x) {
    pal <- scales::col_factor(pitch_colors,
                              levels = names(pitch_colors),
                              na.color = "#DDDDDD")
    pal(x)
  }
  
  colorize_by <- function(ref, reverse = FALSE) {
    ref <- suppressWarnings(as.numeric(ref))
    ref <- ref[is.finite(ref)]
    if (length(ref) < 2 || diff(range(ref)) == 0) {
      return(function(z) rep("#FFFFFF", length(z)))
    }
    lo <- min(ref); hi <- max(ref)
    pal <- if (reverse)
      scales::gradient_n_pal(c("#00840D","white","#E1463E"), values = c(0,0.5,1))
    else
      scales::gradient_n_pal(c("#E1463E","white","#00840D"), values = c(0,0.5,1))
    
    function(z) {
      znum <- suppressWarnings(as.numeric(z))
      colv <- pal(scales::rescale(znum, to = c(0,1), from = c(lo,hi)))
      colv[!is.finite(znum)] <- "#FFFFFF"  # ensure no NA colors
      colv[is.na(colv)] <- "#FFFFFF"
      colv
    }
  }
  
  # simple safe division
  pdiv <- function(num, den) ifelse(den > 0, num/den, NA_real_)
  
  
