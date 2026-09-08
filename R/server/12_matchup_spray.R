  # ===================== ADVANCE: MATCHUP SPRAY =====================
  # Same similar-pitch engine as the matrix: for every batter x pitcher
  # pair, pool the batter's similar pitches across the arsenal and plot
  # the balls in play on a faceted spray grid.
  ms_pool_pitcher <- reactive({
    req(input$ms_pitcher_team)
    withProgress(message = "Loading pitcher-team pool...", value = 0.4,
                 load_matchup_team_pool(input$ms_pitcher_team))
  })
  ms_pool_batter <- reactive({
    req(input$ms_batter_team)
    withProgress(message = "Loading batter-team pool...", value = 0.4,
                 load_matchup_team_pool(input$ms_batter_team))
  })
  ms_db <- reactive({
    frames <- Filter(function(d) !is.null(d) && nrow(d) > 0,
                     list(ms_pool_pitcher(), ms_pool_batter()))
    req(length(frames) > 0)
    .mm_dedupe_uid(dplyr::bind_rows(harmonize_types(frames)))
  })

  observeEvent(input$ms_pitcher_team, {
    req(input$ms_pitcher_team)
    invisible(ms_pool_pitcher())
    ch <- withProgress(message = "Loading 2026 staff from TruMedia...", value = 0.5,
                       mm_roster_choices(input$ms_pitcher_team, "pitcher"))
    if (is.null(ch)) ch <- character(0)
    updateSelectizeInput(session, "ms_pitchers", choices = ch,
                         selected = unname(ch), server = FALSE)
  })
  observeEvent(input$ms_batter_team, {
    req(input$ms_batter_team)
    invisible(ms_pool_batter())
    ch <- withProgress(message = "Loading 2026 lineup from TruMedia...", value = 0.5,
                       mm_roster_choices(input$ms_batter_team, "batter"))
    if (is.null(ch)) ch <- character(0)
    updateSelectizeInput(session, "ms_batters", choices = ch,
                         selected = unname(ch), server = FALSE)
  })

  ms_spray_df <- reactiveVal(NULL)

  observeEvent(input$ms_build, {
    req(length(input$ms_pitchers) > 0, length(input$ms_batters) > 0)
    db <- ms_db()
    out <- list()
    withProgress(message = "Building spray grid...", value = 0, {
      total <- length(input$ms_pitchers) * length(input$ms_batters)
      k <- 0
      for (pk_raw in input$ms_pitchers) {
        pk <- split_pitcher_key(pk_raw)
        for (bk_raw in input$ms_batters) {
          bk <- split_batter_key(bk_raw)
          k <- k + 1
          setProgress(k / total, detail = paste(pk$name, "vs", bk$name))
          m <- tryCatch(
            calculate_matchup(db, pk$name, bk$name, mm_scaler(),
                              pitcher_team = pk$team, batter_team = bk$team,
                              pitcher_weight = input$mm_pitcher_weight %||% 0.5,
                              sim_threshold = input$ms_sim_threshold %||% SIMILARITY_THRESHOLD),
            error = function(e) NULL)
          if (is.null(m) || length(m$per_pitch) == 0) next
          sims <- Filter(function(x) is.data.frame(x$similar) && nrow(x$similar) > 0,
                         m$per_pitch)
          if (length(sims) == 0) next
          sim_df <- dplyr::bind_rows(lapply(sims, function(x) x$similar))
          need <- c("Bearing", "Distance", "PitchCall")
          if (!all(need %in% names(sim_df))) next
          bip <- sim_df[!is.na(sim_df$PitchCall) & sim_df$PitchCall == "InPlay" &
                          is.finite(suppressWarnings(as.numeric(sim_df$Bearing))) &
                          is.finite(suppressWarnings(as.numeric(sim_df$Distance))), ,
                        drop = FALSE]
          if (nrow(bip) == 0) next
          bip$.col_pitcher <- pk$name
          bip$.row_batter  <- bk$name
          out[[length(out) + 1]] <- bip
        }
      }
    })
    if (length(out) == 0) {
      showNotification(paste("No balls in play found in the similar-pitch pools.",
                             "TruMedia-sourced pitches carry no spray coordinates,",
                             "so this view draws on parquet-era (TrackMan) rows."),
                       type = "warning", duration = 8)
      ms_spray_df(NULL)
      return()
    }
    df <- dplyr::bind_rows(harmonize_types(out))
    df$Bearing  <- suppressWarnings(as.numeric(df$Bearing))
    df$Distance <- suppressWarnings(as.numeric(df$Distance))
    df$x <- sin(df$Bearing * pi / 180) * df$Distance
    df$y <- cos(df$Bearing * pi / 180) * df$Distance
    df$Result <- dplyr::case_when(
      df$PlayResult == "Single"  ~ "1B",
      df$PlayResult == "Double"  ~ "2B",
      df$PlayResult == "Triple"  ~ "3B",
      df$PlayResult == "HomeRun" ~ "HR",
      TRUE                       ~ "Out"
    )
    df$.col_pitcher <- factor(df$.col_pitcher,
                              levels = vapply(input$ms_pitchers,
                                              function(k) split_pitcher_key(k)$name,
                                              character(1)))
    df$.row_batter <- factor(df$.row_batter,
                             levels = vapply(input$ms_batters,
                                             function(k) split_batter_key(k)$name,
                                             character(1)))
    ms_spray_df(df)
  })

  # Modern dark spray field (broadcast-style): navy panels, light field
  # geometry, direction-share badges, hits highlighted vs muted outs.
  ms_field_plot <- function(df) {
    req(!is.null(df))
    th <- seq(-45, 45, length.out = 120) * pi / 180
    fence <- data.frame(x = sin(th) * 340, y = cos(th) * 340)
    grass <- data.frame(x = sin(th) * 156, y = cos(th) * 156)
    b90 <- 90 / sqrt(2)
    diamond <- data.frame(x = c(0,  b90, 0, -b90, 0),
                          y = c(0,  b90, 2 * b90, b90, 0))
    bases <- data.frame(x = c(b90, 0, -b90), y = c(b90, 2 * b90, b90))
    thm <- seq(0, 2 * pi, length.out = 60)
    mound <- data.frame(x = sin(thm) * 9, y = 60.5 + cos(thm) * 9)

    # direction-share badges (LF / CF / RF thirds) per facet
    lab <- df %>%
      dplyr::group_by(.row_batter, .col_pitcher) %>%
      dplyr::summarise(
        L = round(100 * mean(Bearing < -15, na.rm = TRUE)),
        C = round(100 * mean(abs(Bearing) <= 15, na.rm = TRUE)),
        R = round(100 * mean(Bearing > 15, na.rm = TRUE)),
        .groups = "drop") %>%
      tidyr::pivot_longer(c(L, C, R), names_to = "dir", values_to = "pct") %>%
      dplyr::mutate(
        x = dplyr::case_when(dir == "L" ~ -185, dir == "C" ~ 0, TRUE ~ 185),
        y = dplyr::case_when(dir == "C" ~ 372, TRUE ~ 300),
        txt = paste0(pct, "%"))

    res_cols <- c("Out" = "#9AA3B2", "1B" = "#34D399", "2B" = "#3B82F6",
                  "3B" = "#A78BFA", "HR" = "#F87171")
    ink <- "#8B93A7"   # field line color on the dark panel

    ggplot(df, aes(x = x, y = y)) +
      geom_segment(x = 0, y = 0, xend = sin(-45 * pi/180) * 340,
                   yend = cos(-45 * pi/180) * 340, color = ink, linewidth = .35) +
      geom_segment(x = 0, y = 0, xend = sin(45 * pi/180) * 340,
                   yend = cos(45 * pi/180) * 340, color = ink, linewidth = .35) +
      geom_segment(x = 0, y = 0, xend = 0, yend = 340,
                   color = ink, linewidth = .22, alpha = .5) +
      geom_path(data = fence, aes(x, y), color = ink, linewidth = .45,
                inherit.aes = FALSE) +
      geom_path(data = grass, aes(x, y), color = ink, linewidth = .3,
                alpha = .8, inherit.aes = FALSE) +
      geom_path(data = diamond, aes(x, y), color = ink, linewidth = .35,
                inherit.aes = FALSE) +
      geom_point(data = bases, aes(x, y), shape = 23, size = 1.7,
                 fill = "#6B7488", color = ink, inherit.aes = FALSE) +
      geom_path(data = mound, aes(x, y), color = ink, linewidth = .3,
                inherit.aes = FALSE) +
      geom_label(data = lab, aes(x, y, label = txt),
                 fill = "#2B3048", color = "#E7EAF3", label.size = 0,
                 label.r = grid::unit(6, "pt"), size = 2.9, fontface = "bold",
                 label.padding = grid::unit(3.2, "pt"), inherit.aes = FALSE) +
      geom_point(data = ~ dplyr::filter(.x, Result == "Out"),
                 color = "#9AA3B2", alpha = .55, size = 1.7) +
      geom_point(data = ~ dplyr::filter(.x, Result != "Out"),
                 aes(fill = Result, size = Result == "HR"),
                 shape = 21, color = "#F2F4FA", stroke = .35, alpha = .95) +
      scale_fill_manual(values = res_cols, name = NULL,
                        breaks = c("1B", "2B", "3B", "HR")) +
      scale_size_manual(values = c(`TRUE` = 3.1, `FALSE` = 2.4), guide = "none") +
      coord_fixed(xlim = c(-265, 265), ylim = c(-18, 405), expand = FALSE) +
      facet_grid(.row_batter ~ .col_pitcher, switch = "y") +
      theme_void(base_size = 12) +
      theme(
        plot.background  = element_rect(fill = "#171A2B", color = NA),
        panel.background = element_rect(fill = "#1E2236", color = NA),
        panel.border     = element_rect(fill = NA, color = "#2B3048", linewidth = .6),
        panel.spacing    = grid::unit(7, "pt"),
        strip.text       = element_text(face = "bold", color = "#D7DBE8", size = 9.5,
                                        margin = margin(4, 4, 4, 4)),
        strip.text.y.left = element_text(angle = 0, hjust = 1),
        strip.background = element_rect(fill = "#141728", color = NA),
        legend.position  = "bottom",
        legend.text      = element_text(color = "#D7DBE8", size = 9),
        legend.key       = element_rect(fill = "#1E2236", color = NA),
        plot.margin      = margin(6, 8, 4, 4)) +
      guides(fill = guide_legend(override.aes = list(size = 3.4)))
  }

  .ms_section <- function(df, which) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    s <- suppressWarnings(as.numeric(df$Strikes))
    out <- switch(which,
                  all    = df,
                  pre2k  = df[!is.na(s) & s < 2, , drop = FALSE],
                  twok   = df[!is.na(s) & s == 2, , drop = FALSE])
    if (nrow(out) == 0) NULL else out
  }
  .ms_titles <- c(all = "Overall", pre2k = "Pre-2K Counts", twok = "2 Strikes")

  output$ms_grid_ui <- renderUI({
    df <- ms_spray_df()
    if (is.null(df)) {
      return(div(class = "adv-placeholder",
                 p("Pick teams and players, then hit Build Spray Grid. Each cell ",
                   "shows the batter's balls in play from his similar-pitch pool ",
                   "vs that arm's arsenal.")))
    }
    n_r <- length(levels(df$.row_batter))
    h <- max(300, 215 * n_r)
    section <- function(id, key) {
      if (is.null(.ms_section(df, key))) return(NULL)
      tagList(
        div(class = "ms-section-title", .ms_titles[[key]]),
        plotOutput(id, height = paste0(h, "px")))
    }
    div(class = "mm-card", style = "background:#171A2B; border-color:#2B3048;",
        div(class = "mm-card-header", style = "border-color:#2B3048;",
            span(class = "mm-title", style = "color:#E7EAF3;", "Matchup Spray"),
            span(class = "mm-sub", style = "color:#8B93A7;",
                 paste0(.mm_team_display(.mm_team_label(input$ms_pitcher_team %||% "")),
                        " Pitchers vs ",
                        .mm_team_display(.mm_team_label(input$ms_batter_team %||% "")),
                        " Batters \u2022 balls in play vs similar pitches"))),
        div(style = "padding: 4px 12px 16px;",
            section("ms_plot_all",   "all"),
            section("ms_plot_pre2k", "pre2k"),
            section("ms_plot_2k",    "twok")))
  })

  output$ms_plot_all   <- renderPlot(ms_field_plot(.ms_section(ms_spray_df(), "all")),
                                     bg = "#171A2B")
  output$ms_plot_pre2k <- renderPlot(ms_field_plot(.ms_section(ms_spray_df(), "pre2k")),
                                     bg = "#171A2B")
  output$ms_plot_2k    <- renderPlot(ms_field_plot(.ms_section(ms_spray_df(), "twok")),
                                     bg = "#171A2B")

  # ---- one-page PDF: all players, three sections stacked vertically ----
  output$ms_download_pdf <- downloadHandler(
    filename = function() paste0("matchup_spray_", Sys.Date(), ".pdf"),
    content = function(file) {
      df <- ms_spray_df()
      req(!is.null(df), nrow(df) > 0)
      n_r <- length(levels(df$.row_batter))
      n_c <- length(levels(df$.col_pitcher))
      sec_keys <- Filter(function(k) !is.null(.ms_section(df, k)),
                         c("all", "pre2k", "twok"))
      pdf_w <- max(11, 2.2 + n_c * 1.7)
      pdf_h <- max(8.5, length(sec_keys) * (0.55 + n_r * 1.5) + 0.7)
      pdf(file, width = pdf_w, height = pdf_h, bg = "#171A2B")
      on.exit(try(dev.off(), silent = TRUE), add = TRUE)
      grid::grid.newpage()
      grid::grid.rect(gp = grid::gpar(fill = "#171A2B", col = NA))
      grid::grid.text(
        paste0("Matchup Spray \u2014 ",
               .mm_team_display(.mm_team_label(input$ms_pitcher_team %||% "")),
               " Pitchers vs ",
               .mm_team_display(.mm_team_label(input$ms_batter_team %||% "")),
               " Batters"),
        x = 0.02, y = 0.985, just = c("left", "top"),
        gp = grid::gpar(fontsize = 15, fontface = "bold", col = "#E7EAF3"))
      body_top <- 0.965
      sec_h <- (body_top - 0.005) / length(sec_keys)
      for (si in seq_along(sec_keys)) {
        y_top <- body_top - (si - 1) * sec_h
        grid::grid.text(.ms_titles[[sec_keys[si]]],
                        x = 0.02, y = y_top - 0.008, just = c("left", "top"),
                        gp = grid::gpar(fontsize = 11.5, fontface = "bold",
                                        col = "#8B93A7"))
        grid::pushViewport(grid::viewport(
          x = 0.5, y = y_top - 0.022 - (sec_h - 0.03) / 2,
          width = 0.98, height = sec_h - 0.032))
        print(ms_field_plot(.ms_section(df, sec_keys[si])), newpage = FALSE)
        grid::popViewport()
      }
    },
    contentType = "application/pdf")
