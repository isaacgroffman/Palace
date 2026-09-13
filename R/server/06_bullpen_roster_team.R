  palace_bullpen_server(input, output, session)                     # Fall 2026 pill
  palace_bullpen_leaderboard_server(input, output, session, "bplb") # Leaderboards
  palace_bullpen_history_server(input, output, session, "sched")    # Schedule
 
  # ===================== ROSTER LANDING =====================
  # Grid of clickable CCU pitcher cards (2026 roster).
  roster_bio <- reactive({
    validate(need(!is.null(roster27), "ccu_2027_roster.csv not loaded."))
    r <- roster27[roster27$IsPitcher, , drop = FALSE]
    data.frame(Pitcher    = r$Name_LF,
               Pitcher_FL = r$Name,
               Headshot   = r$Headshot,
               Class      = r$Class,
               Hometown   = r$Hometown,
               Throws     = r$Throws,
               Number     = r$Number,
               Position   = r$Pos,
               stringsAsFactors = FALSE)
  })

  roster_hitter_bio <- reactive({
    validate(need(!is.null(roster27), "ccu_2027_roster.csv not loaded."))
    r <- roster27[!roster27$IsPitcher, , drop = FALSE]
    data.frame(Batter    = r$Name_LF,
               Batter_FL = r$Name,
               Headshot  = r$Headshot,
               Class     = r$Class,
               Hometown  = r$Hometown,
               Bats      = r$Bats,
               Number    = r$Number,
               Position  = r$Pos,
               stringsAsFactors = FALSE)
  })

  output$roster_cards <- renderUI({
    if (isTRUE(input$roster_side == "Hitters")) {
      b <- roster_hitter_bio()
      validate(need(nrow(b) > 0, "No hitters found in ccu_2027_roster.csv."))

      val_or <- function(x, alt = "") if (is.null(x) || is.na(x) || x == "") alt else as.character(x)
      to_fl <- function(x) {
        if (is.na(x) || is.null(x)) return("")
        sub("^\\s*([^,]+),\\s*(.+)\\s*$", "\\2 \\1", x)
      }

      q <- input$roster_search
      if (!is.null(q) && nzchar(trimws(q))) {
        nm_fl <- ifelse(!is.na(b$Batter_FL) & nzchar(b$Batter_FL),
                        b$Batter_FL, vapply(b$Batter, to_fl, character(1)))
        keep <- grepl(trimws(q), nm_fl, ignore.case = TRUE) |
                grepl(trimws(q), b$Batter, ignore.case = TRUE)
        b <- b[keep, , drop = FALSE]
        validate(need(nrow(b) > 0, paste0("No players match \u201c", q, "\u201d.")))
      }

      cards <- lapply(seq_len(nrow(b)), function(i) {
        row <- b[i, ]
        name_fl  <- if (!is.na(row$Batter_FL) && nzchar(row$Batter_FL)) row$Batter_FL else to_fl(row$Batter)
        headshot <- val_or(if ("Headshot" %in% names(row)) row$Headshot else NA,
                           "https://i.imgur.com/zjTu3JS.png")
        class_t  <- val_or(if ("Class" %in% names(row)) row$Class else NA, "")
        home_t   <- val_or(if ("Hometown" %in% names(row)) row$Hometown else NA, "")
        bats_t   <- val_or(if ("Bats" %in% names(row)) row$Bats else NA, "")
        num_t    <- get_jersey_number(
          name_fl, "Coastal Carolina",
          fallback = val_or(if ("Number" %in% names(row)) row$Number else NA, ""))
        pos_t    <- val_or(if ("Position" %in% names(row)) row$Position else NA, "")

        meta_bits <- paste(c(
          if (nzchar(class_t)) class_t else NULL,
          if (nzchar(bats_t)) paste0("Bats ", bats_t) else NULL
        ), collapse = "  \u2022  ")

        safe_name <- gsub("'", "\\\\'", name_fl)

        div(
          class = "roster-card",
          onclick = sprintf("Shiny.setInputValue('roster_pick_hitter', '%s', {priority:'event'})", safe_name),
          div(class = "roster-card-imgwrap",
              tags$img(src = headshot, class = "roster-card-img",
                       onerror = "this.src='https://i.imgur.com/zjTu3JS.png'")),
          div(class = "roster-card-body",
              div(class = "roster-card-name", name_fl),
              div(class = "roster-card-role", if (nzchar(num_t)) paste0("#", num_t, "  ", pos_t) else pos_t),
              if (nzchar(home_t)) div(class = "roster-card-line", paste0("Hometown: ", home_t)) else NULL,
              if (nzchar(meta_bits)) div(class = "roster-card-line", meta_bits) else NULL,
              div(class = "roster-card-btn", "View Profile")
          )
        )
      })

      return(div(class = "roster-grid", cards))
    }
    b <- roster_bio()
    validate(need(nrow(b) > 0, "No pitchers found in ccu_2027_roster.csv."))

    val_or <- function(x, alt = "") if (is.null(x) || is.na(x) || x == "") alt else as.character(x)
    to_fl <- function(x) {
      if (is.na(x) || is.null(x)) return("")
      sub("^\\s*([^,]+),\\s*(.+)\\s*$", "\\2 \\1", x)
    }

    # Filter by the in-tab search box (matches first/last name).
    q <- input$roster_search
    if (!is.null(q) && nzchar(trimws(q))) {
      nm_fl <- ifelse(!is.na(b$Pitcher_FL) & nzchar(b$Pitcher_FL),
                      b$Pitcher_FL, vapply(b$Pitcher, to_fl, character(1)))
      keep <- grepl(trimws(q), nm_fl, ignore.case = TRUE) |
              grepl(trimws(q), b$Pitcher, ignore.case = TRUE)
      b <- b[keep, , drop = FALSE]
      validate(need(nrow(b) > 0, paste0("No players match \u201c", q, "\u201d.")))
    }

    cards <- lapply(seq_len(nrow(b)), function(i) {
      row <- b[i, ]
      name_fl  <- if (!is.na(row$Pitcher_FL) && nzchar(row$Pitcher_FL)) row$Pitcher_FL else to_fl(row$Pitcher)
      headshot <- val_or(if ("Headshot" %in% names(row)) row$Headshot else NA,
                         "https://i.imgur.com/zjTu3JS.png")
      class_t  <- val_or(if ("Class" %in% names(row)) row$Class else NA, "")
      home_t   <- val_or(if ("Hometown" %in% names(row)) row$Hometown else NA, "")
      throws_t <- val_or(if ("Throws" %in% names(row)) row$Throws else NA, "")
      num_t    <- get_jersey_number(
        name_fl, "Coastal Carolina",
        fallback = val_or(if ("Number" %in% names(row)) row$Number else NA, ""))
      pos_t    <- val_or(if ("Position" %in% names(row)) row$Position else NA, "P")

      meta_bits <- paste(c(
        if (nzchar(class_t)) class_t else NULL,
        if (nzchar(throws_t)) paste0("Throws ", throws_t) else NULL
      ), collapse = "  •  ")

      # Escape single quotes in the name for the JS payload
      safe_name <- gsub("'", "\\\\'", name_fl)

      div(
        class = "roster-card",
        onclick = sprintf("Shiny.setInputValue('roster_pick', '%s', {priority:'event'})", safe_name),
        div(class = "roster-card-imgwrap",
            tags$img(src = headshot, class = "roster-card-img",
                     onerror = "this.src='https://i.imgur.com/zjTu3JS.png'")),
        div(class = "roster-card-body",
            div(class = "roster-card-name", name_fl),
            div(class = "roster-card-role", if (nzchar(num_t)) paste0("#", num_t, "  ", pos_t) else pos_t),
            if (nzchar(home_t)) div(class = "roster-card-line", paste0("Hometown: ", home_t)) else NULL,
            if (nzchar(meta_bits)) div(class = "roster-card-line", meta_bits) else NULL,
            div(class = "roster-card-btn", "View Profile")
        )
      )
    })

    div(class = "roster-grid", cards)
  })

  # Clicking a card selects the pitcher and opens the Overview tab.
  observeEvent(input$roster_pick, {
    pick <- input$roster_pick
    req(pick, nzchar(pick))
    # The roster IS the 2027 (Fall 2026) roster: open the Coastal identity
    # of that name in its Fall 2026 context, never a same-named NCAA arm.
    tok <- tryCatch(registry_token_for(pick, kind = "pit", prefer_coastal = TRUE, team = "Coastal Carolina"),
                    error = function(e) NULL)
    if (is.null(tok)) { showNotification(paste0("No data for ", pick, " yet."), type = "warning", duration = 5); return() }
    select_player(tok, season = "Fall26")
  })

  # Clicking a hitter card opens that hitter's Overview page.
  observeEvent(input$roster_pick_hitter, {
    pick <- input$roster_pick_hitter
    req(pick, nzchar(pick))
    tok <- tryCatch(registry_token_for(pick, kind = "bat", prefer_coastal = TRUE, team = "Coastal Carolina"),
                    error = function(e) NULL)
    if (is.null(tok)) { session$userData$hitter_ident <- NULL; open_hitter_page(pick); return() }
    select_player(tok, season = "Fall26")
  })

  output$player_header <- renderPlot({
    req(input$global_pitcher)
    
    tryCatch({
      g <- if (is_coastal_ctx(input$global_pitcher, input$season_type)) {
        create_player_header(bio, input$global_pitcher)
      } else {
        create_ncaa_player_header(input$global_pitcher, filtered_data())
      }
      grid.newpage()
      grid.draw(g)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Player info for", input$global_pitcher), 
           cex = 2, font = 2)
    })
  }, height = 250)
  
  output$count_usage_plot <- renderPlot({
    req(input$global_pitcher)
    count_usage(filtered_data(), input$global_pitcher)
  })
  
  
  count_usage <- function(data, player) {
    data <- data %>%
      filter(Pitcher == player, TaggedPitchType != "Other") %>%
      group_by(TaggedPitchType) %>%
      mutate(n = 100 * n() / nrow(data %>% filter(Pitcher == player))) %>%
      filter(n >= 2) %>%
      ungroup()
    
    if (nrow(data) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0, y = 0, label = "No count data\navailable for this pitcher", size = 4) +
          theme_void()
      )
    }
    
    plot_df <- data %>%
      mutate(
        count = case_when(
          Balls == 0 & Strikes == 0 ~ "0-0",
          Balls == 3 & Strikes == 2 ~ "3-2",
          Balls > Strikes ~ "Behind",
          Strikes > Balls ~ "Ahead"
        ),
        BatterSide = ifelse(BatterSide == "Right", "Vs Right", "Vs Left")
      ) %>%
      group_by(count, BatterSide) %>%
      mutate(total_count = n()) %>%
      group_by(count, TaggedPitchType, BatterSide) %>%
      summarise(n = n(), total = first(total_count), .groups = "drop") %>%
      mutate(
        percentage = n / total * 100,
        # label text, hide super tiny slices to avoid overlap
        pct_label = ifelse(percentage >= 2, scales::percent(percentage / 100, accuracy = 0.1), "")
      ) %>%
      arrange(desc(percentage)) %>%
      filter(count != "Tied")
    
    ggplot(plot_df, aes(x = "", y = percentage, fill = TaggedPitchType)) +
      geom_bar(width = 1, stat = "identity") +
      geom_text(aes(label = pct_label),
                position = position_stack(vjust = 0.5),
                size = 3) +
      coord_polar("y", start = 0) +
      facet_grid(BatterSide ~ count, labeller = label_value) +
      theme_minimal() +
      labs(title = paste(player, " Pitch Usage by Count"), fill = "Pitch Type") +
      scale_fill_manual(values = pitch_colors) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 12),
        legend.position = "bottom"
      )
  }
  
  observeEvent(list(selected_data(), input$Roster_2026_countreport), {
    df <- selected_data()
    if (!"Pitcher" %in% names(df)) {
      updatePickerInput(session, "pitcher_countreport", choices = character(0))
      return()
    }
    available_pitchers <- sort(unique(df$Pitcher))
    if (!is.null(input$Roster_2026_countreport) && "Roster_2026" %in% names(bio)) {
      Roster_pitchers <- bio %>%
        dplyr::filter(Roster_2026 %in% input$Roster_2026_countreport) %>%
        dplyr::pull(Pitcher_FL)
      if (length(Roster_pitchers) > 0)
        available_pitchers <- sort(intersect(available_pitchers, Roster_pitchers))
    }
    if (length(available_pitchers) == 0) available_pitchers <- sort(unique(df$Pitcher))
    updatePickerInput(session, "pitcher_countreport",
                      choices  = available_pitchers,
                      selected = head(available_pitchers, 20))
  })
  
  observeEvent(list(selected_data(), input$Roster_2026_leader), {
    df <- selected_data()
    if (!"Pitcher" %in% names(df)) {
      updatePickerInput(session, "pitcher_leader", choices = character(0))
      return()
    }
    
    # Start with all pitchers
    available_pitchers <- sort(unique(df$Pitcher))
    
    # Apply Roster filter if present
    if (!is.null(input$Roster_2026_leader) && "Roster2026" %in% names(bio)) {
      Roster_pitchers <- bio %>%
        dplyr::filter(Roster2026 %in% input$Roster_2026_leader) %>%
        dplyr::pull(Pitcher_FL)
      if (length(Roster_pitchers) > 0) {
        available_pitchers <- sort(intersect(available_pitchers, Roster_pitchers))
      }
    }
    
    # Fallback so it never ends up empty (but still respects Leaderboard filter)
    if (length(available_pitchers) == 0) {
      available_pitchers <- sort(unique(df$Pitcher))
      if ("Leaderboard" %in% names(bio)) {
        no_pitchers <- bio %>%
          dplyr::mutate(Leaderboard = tolower(trimws(as.character(Leaderboard)))) %>%
          dplyr::filter(Leaderboard == "no") %>%
          dplyr::pull(Pitcher_FL)
        available_pitchers <- setdiff(available_pitchers, no_pitchers)
      }
    }
    
    updatePickerInput(
      session, "pitcher_leader",
      choices  = available_pitchers,
      selected = available_pitchers   # Select all by default
    )
  })
  
  
  
  # Update the existing observeEvent for active_data to include leaderboard filters
  observeEvent(coastal_selected_data(), {
    df <- coastal_selected_data()
    
    # Update 2026 Roster pickers if that column exists
    if ("Roster2026" %in% names(bio)) {
      Roster_choices <- unique(bio$Roster2026)
      Roster_choices <- Roster_choices[!is.na(Roster_choices)]
      if (length(Roster_choices) == 0) Roster_choices <- c("Yes", "No")
      
updatePickerInput(session, "Roster_2026",        choices = Roster_choices, selected = c("Yes", "No"))
updatePickerInput(session, "Roster_2026_spray",  choices = Roster_choices, selected = c("Yes", "No"))
updatePickerInput(session, "Roster_2026_leader", choices = Roster_choices, selected = c("Yes", "No"))
updatePickerInput(session, "Roster_2026_countreport", choices = Roster_choices, selected = c("Yes", "No"))
    }
    
    # ... rest of your existing observeEvent code for other filters ...
    
    # Update leaderboard-specific filters
    if ("BatterSide" %in% names(df)) {
      bs <- unique(df$BatterSide)
      updatePickerInput(session, "BatterSide_leader", choices = bs, selected = bs)
    }
    
    if ("TaggedPitchType" %in% names(df)) {
      pt <- unique(df$TaggedPitchType)
      updatePickerInput(session, "pitchtype_leader", choices = pt, selected = pt)
    }
  })
  
  # ---- UI ----
  column(
    3,
    div(style = "text-align:center;", h3("Count Usage")),
    plotOutput("count_usage_plot", height = "420px"),
    br(),
    div(style = "text-align:center;", h4("Pitch Tilts")),
    plotOutput("clock_plot", height = "400px")
  )
  
  output$clock_plot <- renderPlot({
    
    req(input$global_pitcher)  # if you're selecting pitcher
    player <- input$global_pitcher
    
    hour_positions <- data.frame(
      hour = 1:12,
      angle = seq(30, 360, by = 30)
    )
    
    # limit pitch types for the selected pitcher
    types <- data %>%
      filter(Pitcher == player, TaggedPitchType != "Other") %>%
      group_by(TaggedPitchType) %>%
      summarise(n = 100 * n() / nrow(data %>% filter(Pitcher == player))) %>%
      filter(n >= 1)
    
    types <- unique(types$TaggedPitchType)
    
    pitches <- data %>%
      mutate(SpinAxis = SpinAxis + 180,
             SpinAxis = ifelse(SpinAxis > 360, SpinAxis - 360, SpinAxis)) %>%
      filter(Pitcher == player & TaggedPitchType %in% types) %>%
      group_by(TaggedPitchType) %>%
      summarise(angle = round(median(SpinAxis, na.rm = TRUE)))
    
    ggplot() +
      geom_circle(aes(x0 = 0, y0 = 0, r = 1), size = 1, color = "black", fill = "white") +
      coord_fixed() +
      theme_void() +
      geom_text(data = hour_positions,
                aes(x = 0.85 * cos(pi/180 * (90 - angle)),
                    y = 0.85 * sin(pi/180 * (90 - angle)),
                    label = hour), size = 5) +
      geom_segment(data = pitches,
                   aes(x = 0, y = 0,
                       xend = cos(pi/180 * (90 - angle)),
                       yend = sin(pi/180 * (90 - angle)),
                       color = TaggedPitchType),
                   size = 1, arrow = arrow(length = unit(0.3, "cm"))) +
      geom_circle(aes(x0 = 0, y0 = 0, r = .25), size = 1, color = "black", fill = "white") +
      geom_circle(aes(x0 = 0, y0 = 0, r = .06), size = 1, color = "black", fill = "black") +
      scale_color_manual(values = pitch_colors) +
      labs(title = paste("Pitch Tilts -", player), color = "Pitch Type") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)
      )
  })
  
  
  
  output$arm_angle_plot <- renderPlot({
    req(input$global_pitcher)
    
    # Debug: Check if arm_angle_savant exists in the data
    df <- filtered_data()
    
    if ("arm_angle_savant" %in% names(df)) {
      pitcher_data <- df %>% filter(Pitcher == input$global_pitcher)
      # Try the arm angle plot with smaller figure size
      tryCatch({
        plot_arm_angles_app(
          df = df,
          pitcher_name = input$global_pitcher,
          pitch_colors = pitch_colors,
          shoulder_scale = 0.70,
          view = "plate",
          angle_from_vertical = FALSE,
          show_points = TRUE,
          show_pitcher_figure = FALSE,
          figure_size = 0.08,  # Much smaller - this was 0.6, now 0.08
          mirror_lefties = TRUE,
          show_legend = TRUE
        )
      }, error = function(e) {
        cat("Error in arm angle plot:", e$message, "\n")
        
        # Create a simple debug plot showing what data we have
        df %>%
          filter(Pitcher == input$global_pitcher) %>%
          ggplot(aes(RelSide, RelHeight)) +
          geom_point(aes(fill = TaggedPitchType), color = 'black', pch = 21, na.rm = TRUE, alpha = .99, size = 2.5) +
          scale_fill_manual(values = pitch_colors) +
          scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
          scale_y_continuous(limits = c(4, 7), breaks = seq(4, 7, 0.5), position = 'left') +
          geom_hline(yintercept = mean(df$RelHeight[df$Pitcher == input$global_pitcher], na.rm = TRUE), linetype = 2) +
          geom_vline(xintercept = mean(df$RelSide[df$Pitcher == input$global_pitcher], na.rm = TRUE), linetype = 2) + 
          theme_bw() + 
          theme(legend.position = "right", axis.title = element_blank(),
                legend.text = element_text(size = 18),
                legend.title = element_blank()) +
          coord_fixed() +
          ggtitle(paste("Error:", e$message))
      })
    } else {
      # arm_angle_savant column doesn't exist
      df %>%
        filter(Pitcher == input$global_pitcher) %>%
        ggplot(aes(RelSide, RelHeight)) +
        geom_point(aes(fill = TaggedPitchType), color = 'black', pch = 21, na.rm = TRUE, alpha = .99, size = 2.5) +
        scale_fill_manual(values = pitch_colors) +
        scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
        scale_y_continuous(limits = c(4, 7), breaks = seq(4, 7, 0.5), position = 'left') +
        geom_hline(yintercept = mean(df$RelHeight[df$Pitcher == input$global_pitcher], na.rm = TRUE), linetype = 2) +
        geom_vline(xintercept = mean(df$RelSide[df$Pitcher == input$global_pitcher], na.rm = TRUE), linetype = 2) + 
        theme_bw() + 
        theme(legend.position = "right", axis.title = element_blank(),
              legend.text = element_text(size = 18),
              legend.title = element_blank()) +
        coord_fixed() +
        ggtitle("arm_angle_savant column not found in data")
    }
  })
  
  observeEvent(list(selected_data(), input$Roster_2026_seq), {
    df <- selected_data()
    if (!"Pitcher" %in% names(df)) {
      updateSelectInput(session, "succseq_pitcher", choices = character(0))
      return()
    }
    if (!is.null(input$Roster_2026_seq) && "Roster2026" %in% names(bio)) {
      Roster_pitchers <- bio %>%
        dplyr::filter(Roster2026 %in% input$Roster_2026_seq) %>%
        dplyr::pull(Pitcher_FL)
      available_pitchers <- df %>%
        dplyr::filter(Pitcher %in% Roster_pitchers) %>%
        dplyr::pull(Pitcher) %>%
        unique() %>%
        sort()
    } else {
      available_pitchers <- sort(unique(df$Pitcher))
    }
    
    if (length(available_pitchers) == 0) {
      available_pitchers <- sort(unique(df$Pitcher))
    }
    
    updateSelectInput(session, "succseq_pitcher", choices = available_pitchers)
  })
  
  # Update date range for sequences tab
  observeEvent(selected_data(), {
    df <- selected_data()
    if ("Date" %in% names(df)) {
      rng <- range(df$Date, na.rm = TRUE)
      if (is.finite(rng[1]) && is.finite(rng[2])) {
        updateDateRangeInput(session, "dateRange_seq", start = rng[1], end = rng[2])
      }
    }
    if ("BatterSide" %in% names(df)) {
      bs <- unique(df$BatterSide)
      updatePickerInput(session, "BatterSide_seq", choices = bs, selected = bs)
    }
  })
  
  # Filtered data for sequences with all the same filters as other tabs
  filtered_data_seq <- reactive({
    df <- selected_data()
    req(input$g_dateRange, input$g_BatterSide)
    
    # Apply date and batter side filters
    filtered_df <- df %>%
      dplyr::filter(
        safe_as_date(Date) >= as.Date(input$g_dateRange[1]) &
          safe_as_date(Date) <= as.Date(input$g_dateRange[2]),
        BatterSide %in% input$g_BatterSide
      )
    
    # Apply Roster filter if present
    if (!is.null(input$Roster_2026_seq) && "Roster2026" %in% names(bio)) {
      Roster_pitchers <- bio %>%
        dplyr::filter(Roster2026 %in% input$Roster_2026_seq) %>%
        dplyr::pull(Pitcher_FL)
      filtered_df <- filtered_df %>%
        dplyr::filter(Pitcher %in% Roster_pitchers)
    }
    
    return(filtered_df)
  })
  
  # Updated make_sequence_df function that uses filtered data
  make_sequence_df_filtered <- reactive({
    req(input$global_pitcher)
    df <- filtered_data_seq()
    
    # Keep one season's worth of consistent columns
    x <- df %>%
      dplyr::mutate(
        Pitcher = stringr::str_replace_all(Pitcher, "(\\w+), (\\w+)", "\\2 \\1"),
        csw_ind   = ifelse(PitchCall %in% c("StrikeCalled","StrikeSwinging"), 1, 0),
        strike_ind = ifelse(PitchCall %in% c("StrikeCalled","StrikeSwinging","FoulBall",
                                             "FoulBallNotFieldable","FoulBallFieldable","InPlay"), 1, 0),
        whiff_ind = ifelse(PitchCall == "StrikeSwinging", 1, 0),
        bip_ind   = ifelse(PitchCall == "InPlay", 1, 0),
        soft_ind  = ifelse(PitchCall == "InPlay" & !is.na(ExitSpeed) & ExitSpeed <= 85, 1, 0),
        out_ind   = ifelse(PlayResult %in% c("Out", "FieldersChoice") | KorBB == "Strikeout", 1, 0)
      ) %>%
      dplyr::filter(Pitcher == input$global_pitcher, !is.na(TaggedPitchType), TaggedPitchType != "Other")
    
    # Order pitches inside each PA, then build 2‑pitch sequences
    x <- x %>%
      dplyr::arrange(GameID, Date, Inning, PAofInning, PitchofPA) %>%
      dplyr::group_by(GameID, Date, Inning, PAofInning) %>%
      dplyr::mutate(
        prev_type = dplyr::lag(TaggedPitchType, 1),
        seq_lab   = paste0(prev_type, " → ", TaggedPitchType),
        is_seq    = !is.na(prev_type),
        prev_PlateLocSide = dplyr::lag(PlateLocSide, 1),
        prev_PlateLocHeight = dplyr::lag(PlateLocHeight, 1),
        prev_RelSpeed = dplyr::lag(RelSpeed, 1),
        prev_IVB = dplyr::lag(InducedVertBreak, 1),
        prev_HB = dplyr::lag(HorzBreak, 1),
        prev_stuff = if ("stuff_plus" %in% names(.)) dplyr::lag(stuff_plus, 1) else NA_real_
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(is_seq)
    
    # Aggregate success on the *second* pitch in the pair
    seq_summary <- x %>%
      dplyr::group_by(seq_lab) %>%
      dplyr::summarise(
        Count      = dplyr::n(),
        `Strike %` = round(mean(strike_ind) * 100, 1),
        `Called Strike %` = round(mean(ifelse(PitchCall == "StrikeCalled", 1, 0), na.rm = TRUE) * 100, 1),
        `CSW %`    = round(mean(csw_ind)   * 100, 1),
        `Whiff %`  = round(mean(whiff_ind) * 100, 1),
        `Soft %`   = round(mean(soft_ind)  * 100, 1),
        `Out %`    = round(mean(out_ind)   * 100, 1),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        # Simple composite: reward CSW and whiffs, small credit for soft/out
        Composite = round((`CSW %`*0.5 + `Whiff %`*0.3 + `Soft %`*0.1 + `Out %`*0.1), 1)
      )
    
    return(list(summary = seq_summary, detailed = x))
  })
  
  # Update sequence choices when pitcher or filters change
  observeEvent(list(input$global_pitcher, make_sequence_df_filtered()), {
    # The Sequencing block was removed from the player page, so its inputs
    # no longer exist. Guard rather than delete: the reactive is still here
    # if the section ever comes back.
    req(input$global_pitcher, input$succseq_min_n)

    seq_data <- make_sequence_df_filtered()
    if (!is.null(seq_data$summary)) {
      sequences <- seq_data$summary %>%
        dplyr::filter(Count >= input$succseq_min_n) %>%
        dplyr::pull(seq_lab) %>%
        sort()
      
      updateSelectInput(session, "succseq_sequence", 
                        choices = sequences,
                        selected = if(length(sequences) > 0) sequences[1] else NULL)
    }
  })
  
  output$count_report_preview <- renderPlot({
    req(input$pitcher_countreport, input$dateRange_countreport)
    
    df <- selected_data() %>%
      dplyr::filter(
        Date >= as.Date(input$dateRange_countreport[1]) &
          Date <= as.Date(input$dateRange_countreport[2])
      )
    
    pitchers <- input$pitcher_countreport
    if (length(pitchers) > 20) pitchers <- pitchers[1:20]
    
    usage_data <- compute_count_usage_table(df, pitchers)
    
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 1)))
    
    grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
    draw_count_usage_report(usage_data, pitchers,
                            title = "CCU Pitching Staff \u2014 Count Usage Report",
                            side_filter = "R")
    grid::upViewport()
    
    grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
    draw_count_usage_report(usage_data, pitchers,
                            title = "CCU Pitching Staff \u2014 Count Usage Report",
                            side_filter = "L")
    grid::upViewport()
  }, height = 1800)
  
  output$download_count_report <- downloadHandler(
    filename = function() {
      paste0("CCU_Count_Usage_Report_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      df <- selected_data() %>%
        dplyr::filter(
          Date >= as.Date(input$dateRange_countreport[1]) &
            Date <= as.Date(input$dateRange_countreport[2])
        )
      
      pitchers <- input$pitcher_countreport
      if (length(pitchers) > 20) pitchers <- pitchers[1:20]
      
      usage_data <- compute_count_usage_table(df, pitchers)
      
      pdf(file, width = 11, height = 8.5)
      grid::grid.newpage()
      draw_count_usage_report(usage_data, pitchers,
                              title = "CCU Pitching Staff \u2014 Count Usage Report",
                              side_filter = "R")
      grid::grid.newpage()
      draw_count_usage_report(usage_data, pitchers,
                              title = "CCU Pitching Staff \u2014 Count Usage Report",
                              side_filter = "L")
      dev.off()
    }
  )
  
  
  output$succseq_table <- plotly::renderPlotly({
    req(input$global_pitcher)

    seq_data <- make_sequence_df_filtered()
    validate(need(!is.null(seq_data$summary), "No sequence data."))

    df <- seq_data$summary %>%
      dplyr::filter(Count >= input$succseq_min_n) %>%
      dplyr::select(`Pitch Sequence` = seq_lab, Count,
                    `Strike %`, `Whiff %`, `Out %`)
    validate(need(nrow(df) > 0, "No sequences meet the minimum count."))

    ord_col <- if (isTRUE(input$succseq_show_common)) "Count" else input$succseq_metric
    if (!ord_col %in% names(df)) ord_col <- "Count"
    df <- df %>% dplyr::arrange(dplyr::desc(.data[[ord_col]])) %>% dplyr::slice(1:14)

    # Color each metric column green->red by percentile within the table.
    pal <- scales::gradient_n_pal(c("#E1463E", "#ffffff", "#00840D"))
    shade_col <- function(v) {
      if (length(v) <= 1 || diff(range(v, na.rm = TRUE)) == 0)
        return(rep("#ffffff", length(v)))
      r <- (v - min(v, na.rm = TRUE)) / (max(v, na.rm = TRUE) - min(v, na.rm = TRUE))
      pal(r)
    }
    fill_seq   <- rep("#f4f1ea", nrow(df))
    fill_count <- rep("#f7f7f7", nrow(df))
    fill_strk  <- shade_col(df$`Strike %`)
    fill_whiff <- shade_col(df$`Whiff %`)
    fill_out   <- shade_col(df$`Out %`)

    plotly::plot_ly(
      type = "table",
      header = list(
        values = c("<b>Pitch Sequence</b>", "<b>Count</b>",
                   "<b>Strike %</b>", "<b>Whiff %</b>", "<b>Out %</b>"),
        align = "center",
        fill = list(color = "#0E6E70"),
        font = list(color = "white", size = 14)
      ),
      cells = list(
        values = rbind(df$`Pitch Sequence`, df$Count,
                       df$`Strike %`, df$`Whiff %`, df$`Out %`),
        align = "center",
        height = 30,
        fill = list(color = list(fill_seq, fill_count, fill_strk, fill_whiff, fill_out)),
        font = list(size = 13)
      )
    )
  })
  
  # New strike zone location charts output
  output$succseq_zones <- plotly::renderPlotly({
    req(input$global_pitcher, input$succseq_sequence)
    
    seq_data <- make_sequence_df_filtered()
    validate(need(!is.null(seq_data$detailed), "No sequence data available"))
    
    sequence_pitches <- seq_data$detailed %>%
      dplyr::filter(seq_lab == input$succseq_sequence,
                    !is.na(PlateLocSide), !is.na(PlateLocHeight),
                    !is.na(prev_PlateLocSide), !is.na(prev_PlateLocHeight))
    validate(need(nrow(sequence_pitches) > 0,
                  paste("No location data for sequence:", input$succseq_sequence)))
    
    has_team <- "BatterTeam" %in% names(sequence_pitches)
    has_stuff <- "stuff_plus" %in% names(sequence_pitches)
    fmt1 <- function(v) ifelse(is.na(v), "-", sprintf("%.1f", v))
    fmt0 <- function(v) ifelse(is.na(v), "-", sprintf("%.0f", v))
    sp <- sequence_pitches %>% dplyr::mutate(
      opp = if (has_team) prettify_team(BatterTeam) else "-",
      gdate = if ("Date" %in% names(.)) as.character(Date) else "-",
      cur_stuff = if (has_stuff) stuff_plus else NA_real_
    )

    first_pitch_data <- sp %>%
      dplyr::transmute(PlateLocSide = prev_PlateLocSide,
                       PlateLocHeight = prev_PlateLocHeight,
                       TaggedPitchType = prev_type,
                       opp, gdate,
                       velo = prev_RelSpeed, ivb = prev_IVB, hb = prev_HB, stuff = prev_stuff,
                       Pitch_Order = "First Pitch")
    second_pitch_data <- sp %>%
      dplyr::transmute(PlateLocSide, PlateLocHeight, TaggedPitchType,
                       opp, gdate,
                       velo = RelSpeed, ivb = InducedVertBreak, hb = HorzBreak, stuff = cur_stuff,
                       Pitch_Order = "Second Pitch")
    combined_data <- dplyr::bind_rows(first_pitch_data, second_pitch_data) %>%
      dplyr::mutate(hover = paste0("<b>", TaggedPitchType, "</b>",
                                   "<br>Velo: ", fmt1(velo),
                                   "<br>IVB/HB: ", fmt1(ivb), " / ", fmt1(hb),
                                   "<br>Stuff+: ", fmt0(stuff),
                                   "<br>Date: ", gdate, "<br>Opp: ", opp))
    
    p <- combined_data %>%
      ggplot(aes(x = PlateLocSide, y = PlateLocHeight, fill = TaggedPitchType, text = hover)) +
      geom_point(size = 3, alpha = 0.75, shape = 21, color = "black") +
      scale_fill_manual(values = pitch_colors) +
      annotate("rect", xmin = -0.8303, xmax = 0.8303, ymin = 1.5, ymax = 3.5,
               fill = NA, color = "black", linewidth = 1) +
      coord_fixed(ratio = 1) +
      xlim(-2, 2) + ylim(0, 4.5) +
      facet_wrap(~ Pitch_Order, nrow = 1) +
      theme_void() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 13, face = "bold", color = "#2c3e50"),
        strip.background = element_blank()
      )
    
    plotly::ggplotly(p, tooltip = "text")
  })

  output$total_table <- renderDataTable({
    DT::datatable(
      data.frame(Note = "Team summary would go here"),
      options = list(paging = FALSE, ordering = TRUE, searching = FALSE)
    )
  })
  
  team_summary_data <- reactive({
    df <- selected_data()
    req(input$dateRange_leader, input$BatterSide_leader, input$pitchtype_leader)
    
    filtered_data_team <- df %>% 
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
      filtered_data_team <- filtered_data_team %>%
        dplyr::filter(Pitcher %in% Roster_pitchers)
    }
    
    # Apply pitcher filter if present - for team summary, include all selected pitchers
    if (!is.null(input$pitcher_leader) && length(input$pitcher_leader) > 0) {
      filtered_data_team <- filtered_data_team %>%
        dplyr::filter(Pitcher %in% input$pitcher_leader)
    }
    
    team_data <- filtered_data_team %>%
      summarize(
        Team = "Coastal Carolina",
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
        'Plate %' = round(sum(PlateIndicator, na.rm = TRUE) / n() * 100, 1),
       'K %' = round(sum(is_k, na.rm = TRUE) / sum(PAindicator, na.rm = TRUE) * 100, 1),
        'BB %' = round(sum(WalkIndicator, na.rm = TRUE) / sum(PAindicator, na.rm = TRUE) * 100, 1),
        'LOO %' = round(sum(LOOindicator, na.rm = TRUE) / sum(LeadOffIndicator, na.rm = TRUE) * 100, 1),
 'AVG' = round(sum(HitIndicator, na.rm = TRUE) / sum(ABindicator, na.rm = TRUE), 3),
        'OPS' = round(
          (sum(OnBaseindicator, na.rm = TRUE) / sum(OBPdenominator, na.rm = TRUE)) +
          (sum(totalbases, na.rm = TRUE) / sum(ABindicator, na.rm = TRUE)), 3),
        'Barrel %' = round(sum(BarrelIndicator, na.rm = TRUE) / sum(BIPind, na.rm = TRUE) * 100, 1),
        'IZ Barrel %' = round(sum(IZBarrelIndicator, na.rm = TRUE) / sum(BIPind[StrikeZoneIndicator == 1], na.rm = TRUE) * 100, 1),
        'HH %' = round(sum(HHindicator, na.rm = TRUE) / sum(biphh, na.rm = TRUE) * 100, 1),
        'GB %' = round(sum(GBindicator, na.rm = TRUE) / sum(BIPind, na.rm = TRUE) * 100, 1),
        .groups = 'drop'
      )
    
    return(team_data)
  })
  
  # Updated total_table output to show team summary
  output$total_table <- renderDataTable({
    DT::datatable(
      team_summary_data(),
      options = list(paging = FALSE, ordering = FALSE, searching = FALSE),
      caption = "Team Summary"
    ) %>%
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
        'AVG',
        backgroundColor = styleInterval(brksavg, clrsavg)
      ) %>%
      formatStyle(
        'OPS',
        backgroundColor = styleInterval(brksops, clrsops)
      ) %>%
      formatStyle(
        'Barrel %',
        backgroundColor = styleInterval(brksbarrel, clrsbarrel)
      ) %>%
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
  
  
  
  
  observeEvent(list(selected_data(), input$g_Roster), {
    df <- selected_data()
    if (!"Pitcher" %in% names(df)) {
      updateSelectInput(session, "pitcher_strikepct", choices = character(0))
      return()
    }
    available <- sort(unique(df$Pitcher))
    if (!is.null(input$g_Roster) && "Roster_2026" %in% names(bio)) {
      roster_names <- bio %>%
        dplyr::filter(Roster_2026 %in% input$g_Roster) %>%
        dplyr::pull(Pitcher_FL)
      if (length(roster_names) > 0) available <- sort(intersect(available, roster_names))
    }
    if (length(available) == 0) available <- sort(unique(df$Pitcher))
    updateSelectInput(session, "pitcher_strikepct", choices = available)
  })
  
  observeEvent(selected_data(), {
    df <- selected_data()
    if ("Date" %in% names(df)) {
      rng <- range(df$Date, na.rm = TRUE)
      if (is.finite(rng[1]) && is.finite(rng[2]))
        updateDateRangeInput(session, "dateRange_strikepct", start = rng[1], end = rng[2])
    }
    if ("BatterSide" %in% names(df)) {
      bs <- sort(unique(df$BatterSide))
      updatePickerInput(session, "BatterSide_strikepct", choices = bs, selected = bs)
    }
  })
  
  strikepct_game_data <- reactive({
    df <- selected_data()
    req(input$global_pitcher, input$g_dateRange, input$g_BatterSide)
    
    parse_date_robust <- function(x) {
      x <- as.character(x)
      out <- rep(as.Date(NA), length(x))
      std <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
      out <- dplyr::coalesce(out, std)
      mdy <- suppressWarnings(as.Date(x, format = "%m/%d/%y"))
      out <- dplyr::coalesce(out, mdy)
      still_na <- is.na(out)
      numeric_vals <- suppressWarnings(as.numeric(x[still_na]))
      excel_dates <- as.Date(numeric_vals, origin = "1899-12-30")
      excel_dates[is.na(excel_dates) | format(excel_dates, "%Y") < "2020" | format(excel_dates, "%Y") > "2030"] <- NA
      out[still_na] <- excel_dates
      out
    }
    
    # Master week calendar from all data in range
    all_weeks <- df %>%
      dplyr::mutate(Date_clean = parse_date_robust(safe_as_date(Date))) %>%
      dplyr::filter(
        !is.na(Date_clean),
        Date_clean >= as.Date(input$g_dateRange[1]),
        Date_clean <= as.Date(input$g_dateRange[2])
      ) %>%
      dplyr::mutate(week_start = lubridate::floor_date(Date_clean, unit = "week", week_start = 1)) %>%
      dplyr::distinct(week_start) %>%
      dplyr::arrange(week_start) %>%
      dplyr::mutate(
        week_num = seq_len(dplyr::n()),
        week_label = paste0("Week ", week_num, " (", format(week_start, "%m/%d"), ")")
      )
    
    filtered <- df %>%
      dplyr::filter(
        Pitcher == input$global_pitcher,
        BatterSide %in% input$g_BatterSide
      ) %>%
      dplyr::mutate(Date_clean = parse_date_robust(safe_as_date(Date))) %>%
      dplyr::filter(
        !is.na(Date_clean),
        Date_clean >= as.Date(input$g_dateRange[1]),
        Date_clean <= as.Date(input$g_dateRange[2])
      ) %>%
      dplyr::mutate(week_start = lubridate::floor_date(Date_clean, unit = "week", week_start = 1))
    
    req(nrow(filtered) > 0)
    
    result <- filtered %>%
      dplyr::group_by(week_start) %>%
      dplyr::summarise(
        Games         = length(unique(Date_clean)),
        Opponents     = paste(unique(prettify_team(BatterTeam)), collapse = ", "),
        Pitches       = dplyr::n(),
        `Strike %`    = round(sum(StrikeIndicator, na.rm = TRUE) / dplyr::n() * 100, 1),
        `FB Strike %` = round(
          sum(FBstrikeind, na.rm = TRUE) /
            max(sum(FBindicator, na.rm = TRUE), 1) * 100, 1),
        `OS Strike %` = round(
          sum(OSstrikeind, na.rm = TRUE) /
            max(sum(OSindicator, na.rm = TRUE), 1) * 100, 1),
        `Whiff %`     = round(sum(WhiffIndicator, na.rm = TRUE) /
                                max(sum(SwingIndicator, na.rm = TRUE), 1) * 100, 1),
        `Chase %`     = round(sum(Chaseindicator, na.rm = TRUE) /
                                max(sum(OutofZone, na.rm = TRUE), 1) * 100, 1),
        `Velocity`    = round(mean(RelSpeed, na.rm = TRUE), 1),
        `Induced Vert`= round(mean(InducedVertBreak, na.rm = TRUE), 1),
        `Extension`   = round(mean(Extension, na.rm = TRUE), 2),
        `Stuff+`      = round(mean(stuff_plus, na.rm = TRUE), 0),
        .groups = "drop"
      )
    
    result %>%
      dplyr::left_join(all_weeks, by = "week_start") %>%
      dplyr::arrange(week_num)
  })
  
  strikepct_team_data <- reactive({
    df <- selected_data()
    req(input$g_dateRange, input$g_BatterSide)
    
    parse_date_robust <- function(x) {
      x <- as.character(x)
      out <- rep(as.Date(NA), length(x))
      std <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
      out <- dplyr::coalesce(out, std)
      mdy <- suppressWarnings(as.Date(x, format = "%m/%d/%y"))
      out <- dplyr::coalesce(out, mdy)
      still_na <- is.na(out)
      numeric_vals <- suppressWarnings(as.numeric(x[still_na]))
      excel_dates <- as.Date(numeric_vals, origin = "1899-12-30")
      excel_dates[is.na(excel_dates) | format(excel_dates, "%Y") < "2020" | format(excel_dates, "%Y") > "2030"] <- NA
      out[still_na] <- excel_dates
      out
    }
    
    filtered <- df %>%
      dplyr::filter(BatterSide %in% input$g_BatterSide) %>%
      dplyr::mutate(Date_clean = parse_date_robust(safe_as_date(Date))) %>%
      dplyr::filter(
        !is.na(Date_clean),
        Date_clean >= as.Date(input$g_dateRange[1]),
        Date_clean <= as.Date(input$g_dateRange[2])
      )
    
    # Apply roster filter
    if (!is.null(input$g_Roster) && "Roster_2026" %in% names(bio)) {
      roster_names <- bio %>%
        dplyr::filter(Roster_2026 %in% input$g_Roster) %>%
        dplyr::pull(Pitcher_FL)
      if (length(roster_names) > 0) {
        filtered <- filtered %>% dplyr::filter(Pitcher %in% roster_names)
      }
    }
    
    req(nrow(filtered) > 0)
    
    filtered <- filtered %>%
      dplyr::mutate(week_start = lubridate::floor_date(Date_clean, unit = "week", week_start = 1))
    
    # Master week calendar
    all_weeks <- filtered %>%
      dplyr::distinct(week_start) %>%
      dplyr::arrange(week_start) %>%
      dplyr::mutate(
        week_num = seq_len(dplyr::n()),
        week_label = paste0("Week ", week_num, " (", format(week_start, "%m/%d"), ")")
      )
    
    result <- filtered %>%
      dplyr::group_by(week_start) %>%
      dplyr::summarise(
        n_pitchers    = length(unique(Pitcher)),
        Pitchers_Used = paste(sort(unique(Pitcher)), collapse = ", "),
        Games         = length(unique(Date_clean)),
        Opponents     = paste(unique(prettify_team(BatterTeam)), collapse = ", "),
        Pitches       = dplyr::n(),
        `Strike %`    = round(sum(StrikeIndicator, na.rm = TRUE) / dplyr::n() * 100, 1),
        `FB Strike %` = round(
          sum(FBstrikeind, na.rm = TRUE) /
            max(sum(FBindicator, na.rm = TRUE), 1) * 100, 1),
        `OS Strike %` = round(
          sum(OSstrikeind, na.rm = TRUE) /
            max(sum(OSindicator, na.rm = TRUE), 1) * 100, 1),
        .groups = "drop"
      )
    
    result %>%
      dplyr::left_join(all_weeks, by = "week_start") %>%
      dplyr::arrange(week_num)
  })

  output$strikepct_trend_plot <- plotly::renderPlotly({
    gd <- strikepct_game_data()
    validate(need(nrow(gd) > 0, "No data found for this pitcher in the selected range."))

    gd <- gd %>%
      dplyr::mutate(
        hover_base = paste0(
          "<b>", week_label, "</b><br>",
          "Games: ", Games, "<br>",
          "Opponents: ", Opponents, "<br>",
          "Pitches: ", Pitches
        )
      )

    ts <- input$trend_stat %||% "strike"
    is_pct <- ts %in% c("strike","fbstrike","osstrike","whiff","chase")

    if (ts == "strike") {
      p <- plotly::plot_ly() %>%
        plotly::add_trace(data = gd, x = ~week_num, y = ~`Strike %`, name = "Overall Strike %",
          type = "scatter", mode = "lines+markers",
          line = list(color = "#2C3E50", width = 3, shape = "spline"),
          marker = list(color = "#2C3E50", size = 10, line = list(color = "black", width = 1)),
          text = ~paste0(hover_base, "<br><b>Overall Strike %: ", `Strike %`, "%</b>"), hoverinfo = "text") %>%
        plotly::add_trace(data = gd, x = ~week_num, y = ~`FB Strike %`, name = "FB Strike %",
          type = "scatter", mode = "lines+markers",
          line = list(color = "#3465cb", width = 3, shape = "spline"),
          marker = list(color = "#3465cb", size = 10, line = list(color = "black", width = 1)),
          text = ~paste0(hover_base, "<br><b>FB Strike %: ", `FB Strike %`, "%</b>"), hoverinfo = "text") %>%
        plotly::add_trace(data = gd, x = ~week_num, y = ~`OS Strike %`, name = "OS Strike %",
          type = "scatter", mode = "lines+markers",
          line = list(color = "#65aa02", width = 3, shape = "spline"),
          marker = list(color = "#65aa02", size = 10, line = list(color = "black", width = 1)),
          text = ~paste0(hover_base, "<br><b>OS Strike %: ", `OS Strike %`, "%</b>"), hoverinfo = "text")
      ycols <- c("Strike %","FB Strike %","OS Strike %"); ytitle <- "Strike %"
    } else {
      col <- switch(ts,
        fbstrike = "FB Strike %", osstrike = "OS Strike %", whiff = "Whiff %",
        chase = "Chase %", velo = "Velocity", ivb = "Induced Vert",
        extension = "Extension", stuffplus = "Stuff+")
      yv <- gd[[col]]
      p <- plotly::plot_ly() %>%
        plotly::add_trace(x = gd$week_num, y = yv, name = col,
          type = "scatter", mode = "lines+markers",
          line = list(color = "#0E6E70", width = 3, shape = "spline"),
          marker = list(color = "#0E6E70", size = 11, line = list(color = "black", width = 1)),
          text = paste0(gd$hover_base, "<br><b>", col, ": ", yv,
                        ifelse(is_pct, "%", ""), "</b>"), hoverinfo = "text")
      ycols <- col; ytitle <- col
    }

    yvals <- unlist(gd[ycols]); yvals <- yvals[is.finite(yvals)]
    yr <- if (length(yvals)) c(min(yvals) - 5, max(yvals) + 5) else c(0, 100)
    if (is_pct) yr <- c(max(0, yr[1]), min(100, yr[2]))

    p %>%
      plotly::layout(
        title = list(text = paste0(ytitle, " Trend: ", input$global_pitcher),
                     font = list(size = 18, family = "Arial", color = "black")),
        xaxis = list(title = "", tickvals = gd$week_num, ticktext = gd$week_label,
                     tickangle = -45, tickfont = list(size = 11), dtick = 1),
        yaxis = list(title = ytitle, range = yr,
                     ticksuffix = if (is_pct) "%" else ""),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25, font = list(size = 14)),
        hovermode = "x unified",
        plot_bgcolor = "white", paper_bgcolor = "white",
        margin = list(b = 120, t = 60)
      ) %>%
      plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
  })

  
  output$strikepct_team_plot <- plotly::renderPlotly({
    gd <- strikepct_team_data()
    validate(need(nrow(gd) > 0, "No team data found in the selected range."))
    
    gd <- gd %>%
      dplyr::mutate(
        hover_base = paste0(
          "<b>", week_label, "</b><br>",
          "Pitchers (", n_pitchers, "): ", Pitchers_Used, "<br>",
          "Games: ", Games, "<br>",
          "Opponents: ", Opponents, "<br>",
          "Pitches: ", Pitches
        )
      )
    
    plotly::plot_ly() %>%
      plotly::add_trace(
        data = gd, x = ~week_num,
        y = ~`Strike %`, name = "Overall Strike %",
        type = "scatter", mode = "lines+markers",
        line = list(color = "#2C3E50", width = 3, shape = "spline"),
        marker = list(color = "#2C3E50", size = 10, line = list(color = "black", width = 1)),
        text = ~paste0(hover_base, "<br><b>Overall Strike %: ", `Strike %`, "%</b>"),
        hoverinfo = "text"
      ) %>%
      plotly::add_trace(
        data = gd, x = ~week_num,
        y = ~`FB Strike %`, name = "FB Strike %",
        type = "scatter", mode = "lines+markers",
        line = list(color = "#3465cb", width = 3, shape = "spline"),
        marker = list(color = "#3465cb", size = 10, line = list(color = "black", width = 1)),
        text = ~paste0(hover_base, "<br><b>FB Strike %: ", `FB Strike %`, "%</b>"),
        hoverinfo = "text"
      ) %>%
      plotly::add_trace(
        data = gd, x = ~week_num,
        y = ~`OS Strike %`, name = "OS Strike %",
        type = "scatter", mode = "lines+markers",
        line = list(color = "#65aa02", width = 3, shape = "spline"),
        marker = list(color = "#65aa02", size = 10, line = list(color = "black", width = 1)),
        text = ~paste0(hover_base, "<br><b>OS Strike %: ", `OS Strike %`, "%</b>"),
        hoverinfo = "text"
      ) %>%
      plotly::layout(
        title = list(
          text = "Weekly Strike % Trends: Team Average",
          font = list(size = 18, family = "Arial", color = "black")
        ),
        xaxis = list(title = "", tickvals = gd$week_num, ticktext = gd$week_label,
                     tickangle = -45, tickfont = list(size = 11), dtick = 1),
        yaxis = list(
          title = "Strike %",
          range = c(
            max(0, min(c(gd$`Strike %`, gd$`FB Strike %`, gd$`OS Strike %`), na.rm = TRUE) - 5),
            min(100, max(c(gd$`Strike %`, gd$`FB Strike %`, gd$`OS Strike %`), na.rm = TRUE) + 5)
          ),
          ticksuffix = "%"
        ),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25, font = list(size = 14)),
        hovermode = "x unified",
        plot_bgcolor = "white", paper_bgcolor = "white",
        margin = list(b = 120, t = 60)
      ) %>%
      plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
  })

  
  output$strikepct_trend_table <- renderDataTable({
    gd <- strikepct_game_data()
    DT::datatable(
      gd %>% dplyr::select(week_label, Games, Opponents, Pitches,
                           `Strike %`, `FB Strike %`, `OS Strike %`) %>%
        dplyr::rename(Week = week_label),
      options = list(pageLength = 20, ordering = TRUE, searching = FALSE, scrollX = TRUE)
    ) %>%
      formatStyle('Strike %',    backgroundColor = styleInterval(brks3, clrs3)) %>%
      formatStyle('FB Strike %', backgroundColor = styleInterval(brks4, clrs4)) %>%
      formatStyle('OS Strike %', backgroundColor = styleInterval(brks5, clrs5))
  })
  
  output$strikepct_team_table <- renderDataTable({
    gd <- strikepct_team_data()
    DT::datatable(
      gd %>% dplyr::select(week_label, n_pitchers, Games, Opponents, Pitchers_Used, Pitches,
                           `Strike %`, `FB Strike %`, `OS Strike %`) %>%
        dplyr::rename(Week = week_label, `# Arms` = n_pitchers, Pitchers = Pitchers_Used),
      options = list(pageLength = 20, ordering = TRUE, searching = FALSE, scrollX = TRUE)
    ) %>%
      formatStyle('Strike %',    backgroundColor = styleInterval(brks3, clrs3)) %>%
      formatStyle('FB Strike %', backgroundColor = styleInterval(brks4, clrs4)) %>%
      formatStyle('OS Strike %', backgroundColor = styleInterval(brks5, clrs5))
  })
  
