# =============================================================================
# R/palace_bullpen.R — reusable Bullpens view (server + optional UI).
#
# palace_bullpen_server(input, output, session,
#                       prefix  = "bp",
#                       pitcher = reactive(input$global_pitcher))
#
# Components:
#   * palace_bullpen_ui/server           per-pitcher Fall 2026 view
#                                        (Overview + Pitch Log), prefix "bp"
#   * palace_bullpen_history_ui/server   staff calendar (Schedule tab)
#   * palace_bullpen_leaderboard_ui/server  staff leaderboard (Leaderboards tab)
#
# Data: Supabase practice pitches, scoped to `pitcher()`, 60s version poll.
# Video: Edgertronic only (SAS URL minted at click time). AWRE integration
# removed 2026-08-31; restore from git history if wanted later.
#
# Pitcher-name matching strips punctuation AND generational suffixes
# (jr/sr/ii/iii/iv), so bio "Chris Billingsley" matches TrackMan's
# "Billingsley jr., Chris".
#
# Global deps from app.R: calculate_bullpen_summary, pitch_colors,
# gt_theme_guardian.
# =============================================================================

# Shared UI generator. The classic Bullpens tab in app.R should now contain
# just palace_bullpen_ui("bp"); the 2027 roster page uses palace_bullpen_ui("r27bp").
# ---- helpers the module needs (ported from Bullpen Central's R/helpers.R) ----
drop_untagged <- function(df) {
  dplyr::filter(df, !is.na(TaggedPitchType),
                !TaggedPitchType %in% c("", "Other", "Undefined"))
}

bp_in_zone <- function(side, height) {
  !is.na(side) & !is.na(height) &
    side >= -0.8333 & side <= 0.8333 & height >= 1.5 & height <= 3.5
}

# Spin efficiency arrives as a 0-1 fraction from the practice API.
fmt_pct <- function(x) ifelse(is.na(x), "\u2014", sprintf("%.0f%%", 100 * x))

# Per-pitch-type summary for the gt table. First column must be `Pitch` --
# the renderer colors it via pitch_colors.
calculate_bullpen_summary <- function(df) {
  df <- drop_untagged(df)
  if (!nrow(df)) return(tibble::tibble())
  total <- nrow(df)
  eff <- if ("SpinAxis3dSpinEfficiency" %in% names(df)) df$SpinAxis3dSpinEfficiency else NA_real_
  df$.eff <- eff
  df |>
    dplyr::group_by(Pitch = TaggedPitchType) |>
    dplyr::summarise(
      Count      = dplyr::n(),
      `Usage %`  = sprintf("%.0f%%", 100 * dplyr::n() / total),
      Velo       = round(mean(RelSpeed, na.rm = TRUE), 1),
      Max        = round(suppressWarnings(max(RelSpeed, na.rm = TRUE)), 1),
      Spin       = round(mean(SpinRate, na.rm = TRUE), 0),
      `Eff %`    = fmt_pct(mean(.eff, na.rm = TRUE)),
      IVB        = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      HB         = round(mean(HorzBreak, na.rm = TRUE), 1),
      VAA        = round(mean(VertApprAngle, na.rm = TRUE), 1),
      `Rel Ht`   = round(mean(RelHeight, na.rm = TRUE), 2),
      `Rel Side` = round(mean(RelSide, na.rm = TRUE), 2),
      Ext        = round(mean(Extension, na.rm = TRUE), 1),
      `Zone %`   = sprintf("%.0f%%", 100 * mean(bp_in_zone(PlateLocSide, PlateLocHeight))),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(Count)) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                ~ ifelse(is.nan(.x) | is.infinite(.x), NA, .x)))
}

gt_theme_guardian <- function(gt_tbl, ...) {
  if (requireNamespace("gtExtras", quietly = TRUE)) {
    gtExtras::gt_theme_guardian(gt_tbl, ...)
  } else {
    gt_tbl
  }
}

palace_bullpen_ui <- function(prefix = "bp") {
  p <- function(id) paste0(prefix, "_", id)
  tabsetPanel(id = p("tabs"), type = "pills",

    tabPanel("Overview", value = p("tab_overview"),
      div(style = "margin-top:12px;"),
      fluidRow(
        column(6, selectInput(p("session"), "Bullpen Session",
                              choices = NULL, selected = NULL)),
        column(6, div(style = "padding-top:25px;",
                      textOutput(p("session_info")),
                      textOutput(p("video_status"))))
      ),
      div(style = "font-size:12px; color:#666; margin:-6px 0 8px 2px;",
          "Click any pitch on the charts below to open its video."),
      hr(),
      fluidRow(column(12, h3("Summary", class = "brand-teal"),
                      gt::gt_output(p("summary_table")))),
      hr(),
      fluidRow(
        column(6, div(style = "text-align:center;", h4("Movement Profile")),
               plotly::plotlyOutput(p("movement_plot"), height = "480px")),
        column(6, div(style = "text-align:center;", h4("Release Points")),
               plotly::plotlyOutput(p("release_plot"), height = "480px"))
      ),
      hr(),
      fluidRow(
        column(12, div(style = "text-align:center;", h4("Pitch Locations")),
               plotly::plotlyOutput(p("location_plot"), height = "560px"))
      )
    ),

    tabPanel("Pitch Log", value = p("tab_log"),
      div(style = "margin-top:12px; font-size:12px; color:#666;",
          "Every pitch in the selected session scope (set on the Overview tab). ",
          "\u25B6 opens the Edgertronic clip."),
      DT::DTOutput(p("pitch_log"))
    ),
  )
}

# ---- Standalone: staff bullpen HISTORY calendar (Schedule tab) --------------
palace_bullpen_history_ui <- function(prefix = "sched") {
  p <- function(id) paste0(prefix, "_", id)
  tagList(
    div(style = "margin-top:12px;"),
    fluidRow(
      column(3, selectInput(p("hist_month"), "Month", choices = NULL)),
      column(9, div(style = "padding-top:32px; font-size:12px; color:#666;",
                    "All pitchers \u2014 who threw, how many pitches, and when."))
    ),
    uiOutput(p("history_ui"))
  )
}

# ---- Standalone: staff bullpen LEADERBOARD (Leaderboards tab) ---------------
palace_bullpen_leaderboard_ui <- function(prefix = "bplb") {
  p <- function(id) paste0(prefix, "_", id)
  tagList(
    div(style = "margin-top:12px;"),
    fluidRow(
      column(3, shinyWidgets::pickerInput(p("lb_players"), "Pitchers",
                  choices = NULL, multiple = TRUE,
                  options = list(`actions-box` = TRUE,
                                 `selected-text-format` = "count > 2"))),
      column(3, dateRangeInput(p("lb_dates"), "Dates",
                               start = NULL, end = NULL)),
      column(2, selectInput(p("lb_ptype"), "Pitch type", choices = "All")),
      column(2, selectInput(p("lb_metric"), "Sort by",
                  choices = c("Avg Velo" = "Velo", "Max Velo" = "Max",
                              "Spin" = "Spin", "IVB" = "IVB", "HB" = "HB",
                              "Extension" = "Ext", "Rel Height" = "RelH",
                              "Zone%" = "Zone", "Pitches" = "Pitches",
                              "Sessions" = "Sessions"),
                  selected = "Velo")),
      column(2, div(style = "padding-top:26px; display:flex; gap:6px;",
               downloadButton(p("lb_png"), "PNG", class = "btn-sm"),
               downloadButton(p("lb_pdf"), "PDF", class = "btn-sm")))
    ),
    gt::gt_output(p("leader_table"))
  )
}

# Shared staff-wide data feed for the standalone components: each instance
# polls Supabase independently (cheap; Postgres caches the count query).
bp_staff_data <- function(session) {
  ver <- reactivePoll(60 * 1000, session,
                      checkFunc = sb_practice_version,
                      valueFunc = function() Sys.time())
  reactive({
    ver()
    tryCatch(sb_load_practice_pitches(), error = function(e) NULL)
  })
}
# Name matching must survive BOTH orders ("Kason Wagner" vs "Wagner, Kason"),
# punctuation, and generational suffixes: lowercase, drop jr/sr/ii/iii/iv,
# split into tokens, sort them, concatenate. Both formats collapse to the
# same key (e.g. "kasonwagner").
bp_norm_name <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("\\b(jr|sr|ii|iii|iv)\\b", " ", x)
  x <- gsub("[^a-z]+", " ", x)
  vapply(strsplit(trimws(x), " +"),
         function(t) paste(sort(t), collapse = ""), character(1))
}

bp_norm_id <- function(x) tolower(gsub("[^A-Za-z0-9]", "", as.character(x)))

palace_bullpen_server <- function(input, output, session,
                                  prefix = "bp",
                                  pitcher = NULL) {
  if (is.null(pitcher)) pitcher <- reactive(input$global_pitcher)
  pfx <- function(id) paste0(prefix, "_", id)
  # -- Supabase data, refreshed when ingest writes -----------------------------
  bp_db_version <- reactivePoll(
    60 * 1000, session,
    checkFunc = sb_practice_version,
    valueFunc = function() Sys.time()
  )

  bullpen_data_r <- reactive({
    bp_db_version()
    tryCatch(sb_load_practice_pitches(), error = function(e) {
      showNotification(paste("Bullpen DB error:", conditionMessage(e)),
                       type = "error", duration = 10)
      NULL
    })
  })

  bp_pitcher_data <- reactive({
    bpd <- bullpen_data_r()
    if (is.null(bpd) || nrow(bpd) == 0) return(NULL)
    gp <- pitcher()
    if (is.null(gp) || !nzchar(gp)) return(NULL)
    bpd[bp_norm_name(bpd$Pitcher) == bp_norm_name(gp), , drop = FALSE]
  })

  # -- Session picker: selected pitcher's sessions only -------------------------
  observe({
    bpd <- bp_pitcher_data()
    if (is.null(bpd) || nrow(bpd) == 0) {
      updateSelectInput(session, pfx("session"),
                        choices = c("No bullpen sessions for this pitcher" = ""),
                        selected = "")
      return()
    }
    sess <- unique(bpd[, c("session_id", "Date"), drop = FALSE])
    sess <- sess[order(sess$Date, decreasing = TRUE), , drop = FALSE]
    n_by <- table(bpd$session_id)
    v_by <- tapply(bpd$has_edger, bpd$session_id, sum)
    labels <- sprintf("%s — %d pitches%s",
                      format(sess$Date, "%b %d, %Y"),
                      as.integer(n_by[sess$session_id]),
                      ifelse(v_by[sess$session_id] > 0,
                             sprintf("  (%d videos)",
                                     as.integer(v_by[sess$session_id])), ""))
    choices <- c(setNames("__all__",
                          sprintf("All sessions \u2014 %d pitches", nrow(bpd))),
                 setNames(sess$session_id, labels))
    sel <- isolate(input[[pfx("session")]])
    if (is.null(sel) || !sel %in% choices) sel <- "__all__"
    updateSelectInput(session, pfx("session"), choices = choices, selected = sel)
  })

  bp_session_df <- reactive({
    # Scope to the selected PITCHER first: TrackMan practice sessions often
    # contain several pitchers back-to-back, and an unscoped session filter
    # aggregates them all into one view (wrong summary, mixed tags).
    bpd <- bp_pitcher_data()
    req(bpd, input[[pfx("session")]])
    if (!nzchar(input[[pfx("session")]])) return(NULL)
    df <- if (identical(input[[pfx("session")]], "__all__")) bpd
          else bpd[bpd$session_id == input[[pfx("session")]], , drop = FALSE]
    df[order(df$Date, df$PitchNo), , drop = FALSE]
  })

  output[[pfx("session_info")]] <- renderText({
    bpd <- bullpen_data_r()
    if (is.null(bpd) || nrow(bpd) == 0)
      return("No bullpen data in the database yet.")
    pd <- bp_pitcher_data()
    if (is.null(pd) || nrow(pd) == 0)
      return("No bullpen sessions recorded for this pitcher.")
    df <- bp_session_df()
    if (is.null(df) || nrow(df) == 0) return("")
    scope <- if (identical(input[[pfx("session")]], "__all__"))
               sprintf("%d sessions \u2022 ", length(unique(df$session_id))) else ""
    paste0(scope, nrow(df), " pitches \u2022 ",
           length(unique(df$TaggedPitchType[!is.na(df$TaggedPitchType)])),
           " pitch types")
  })

  output[[pfx("video_status")]] <- renderText({
    df <- bp_session_df()
    if (is.null(df) || nrow(df) == 0) return("")
    n_e <- sum(df$has_edger, na.rm = TRUE)
    if (n_e == 0) return("Video: no Edgertronic clips in this session.")
    paste0("Video: ", n_e, " Edgertronic of ", nrow(df),
           " pitches — click a point to watch.")
  })

  # -- Summary + charts (unchanged behavior) ------------------------------------
  output[[pfx("summary_table")]] <- gt::render_gt({
    df <- bp_session_df()
    validate(need(!is.null(df) && nrow(df) > 0, "Select a bullpen session."))
    st <- calculate_bullpen_summary(df)
    pcol_fn <- function(x) vapply(as.character(x), function(p)
      if (!is.na(p) && p %in% names(pitch_colors)) pitch_colors[[p]] else "#eeeeee",
      character(1))
    st %>%
      gt::gt() %>%
      gt_theme_guardian() %>%
      gt::data_color(columns = Pitch, fn = pcol_fn) %>%
      gt::tab_options(table.font.size = gt::px(13),
                      data_row.padding = gt::px(6)) %>%
      gt::cols_align(align = "center", columns = gt::everything())
  })

  bp_theme <- theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "grey88", linewidth = 0.3),
          plot.background = element_rect(fill = "white", color = NA),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
          axis.text = element_text(size = 11, color = "black"),
          axis.title = element_text(size = 12))

  output[[pfx("movement_plot")]] <- plotly::renderPlotly({
    df <- bp_session_df() %>%
      dplyr::filter(!is.na(TaggedPitchType), TaggedPitchType != "Other",
                    TaggedPitchType != "Undefined",
                    !is.na(HorzBreak), !is.na(InducedVertBreak))
    validate(need(nrow(df) > 0, "No movement data for this session."))
    df <- df %>% dplyr::mutate(hover = paste0(
      "<b>", TaggedPitchType, "</b>",
      "<br>Velo: ", sprintf("%.1f", RelSpeed),
      "<br>IVB/HB: ", sprintf("%.1f", InducedVertBreak),
      " / ", sprintf("%.1f", HorzBreak),
      "<br>Spin: ", sprintf("%.0f", SpinRate)))
    centers <- df %>% dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(mean_velo = round(mean(RelSpeed, na.rm = TRUE)),
                       mean_hb = median(HorzBreak, na.rm = TRUE),
                       mean_ivb = median(InducedVertBreak, na.rm = TRUE),
                       .groups = "drop")
    p <- ggplot(df, aes(HorzBreak, InducedVertBreak)) +
      geom_vline(xintercept = 0, color = "black", linewidth = 0.7) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.7) +
      suppressWarnings(geom_point(
        aes(fill = TaggedPitchType, text = hover, customdata = PlayID),
        alpha = 1, shape = 21, color = "black", stroke = 0.4, size = 4)) +
      geom_point(data = centers, aes(mean_hb, mean_ivb, fill = TaggedPitchType),
                 shape = 21, color = "black", stroke = 0.5, size = 9,
                 alpha = 0.95, inherit.aes = FALSE) +
      geom_text(data = centers, aes(mean_hb, mean_ivb, label = mean_velo),
                color = "black", size = 3.1, inherit.aes = FALSE) +
      scale_fill_manual(values = pitch_colors, name = NULL) +
      scale_x_continuous(breaks = seq(-20, 20, by = 10)) +
      scale_y_continuous(breaks = seq(-20, 20, by = 10)) +
      coord_fixed(ratio = 1, xlim = c(-27.5, 27.5), ylim = c(-27.5, 27.5)) +
      labs(x = "Horizontal Break (in)", y = "Induced Vertical Break (in)") +
      bp_theme
    plotly::ggplotly(p, tooltip = "text", source = pfx("move")) %>%
      plotly::event_register("plotly_click")
  })

  output[[pfx("release_plot")]] <- plotly::renderPlotly({
    df <- bp_session_df() %>%
      dplyr::filter(!is.na(RelSide), !is.na(RelHeight),
                    !is.na(TaggedPitchType), TaggedPitchType != "Other",
                    TaggedPitchType != "Undefined")
    validate(need(nrow(df) > 0, "No release data for this session."))
    df <- df %>% dplyr::mutate(hover = paste0(
      "<b>", TaggedPitchType, "</b>",
      "<br>Velo: ", sprintf("%.1f", RelSpeed),
      "<br>Rel: ", sprintf("%.2f", RelSide), " / ", sprintf("%.2f", RelHeight),
      "<br>Ext: ", sprintf("%.2f", Extension)))
    avg_release <- df %>% dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(RelSide = mean(RelSide, na.rm = TRUE),
                       RelHeight = mean(RelHeight, na.rm = TRUE),
                       .groups = "drop")
    p <- ggplot() +
      geom_rect(aes(xmin = -5, xmax = 5, ymin = 0, ymax = 0.83),
                fill = "#632b11", inherit.aes = FALSE) +
      geom_rect(aes(xmin = -0.5, xmax = 0.5, ymin = 0.8, ymax = 0.95),
                fill = "white", color = "black", linewidth = 0.4,
                inherit.aes = FALSE) +
      suppressWarnings(geom_point(
        data = df,
        aes(RelSide, RelHeight, fill = TaggedPitchType,
            text = hover, customdata = PlayID),
        size = 4.2, shape = 21, color = "black", alpha = 1, stroke = 0.4)) +
      geom_point(data = avg_release,
                 aes(RelSide, RelHeight, fill = TaggedPitchType),
                 size = 7, shape = 21, color = "black", stroke = 0.6,
                 alpha = 0.95) +
      annotate("text", x = -3.5, y = 7.5, label = "← 1B", size = 3.8, hjust = 0) +
      annotate("text", x = 3.5, y = 7.5, label = "3B →", size = 3.8, hjust = 1) +
      scale_fill_manual(values = pitch_colors, name = NULL) +
      coord_fixed(ratio = 1, xlim = c(-4, 4), ylim = c(0, 8)) +
      labs(x = "Release Side (ft)", y = "Release Height (ft)") +
      bp_theme
    plotly::ggplotly(p, tooltip = "text", source = pfx("rel")) %>%
      plotly::event_register("plotly_click")
  })

  output[[pfx("location_plot")]] <- plotly::renderPlotly({
    df <- bp_session_df() %>%
      dplyr::filter(!is.na(TaggedPitchType), TaggedPitchType != "",
                    TaggedPitchType != "Undefined", TaggedPitchType != "Other",
                    !is.na(PlateLocSide), !is.na(PlateLocHeight))
    validate(need(nrow(df) > 0, "No location data for this session."))
    df <- df %>% dplyr::mutate(hover = paste0(
      "<b>", TaggedPitchType, "</b>",
      "<br>Velo: ", sprintf("%.1f", RelSpeed),
      "<br>IVB/HB: ", sprintf("%.1f", InducedVertBreak),
      " / ", sprintf("%.1f", HorzBreak)))
    zl <- -0.8333; zr <- 0.8333; zb <- 1.5; zt <- 3.5
    p <- ggplot(df, aes(PlateLocSide, PlateLocHeight)) +
      annotate("segment", x = -0.85, xend = 0.85, y = 0.15, yend = 0.15,
               color = "black", linewidth = 0.6) +
      annotate("segment", x = -0.85, xend = -0.85, y = 0.15, yend = 0.3,
               color = "black", linewidth = 0.6) +
      annotate("segment", x = 0.85, xend = 0.85, y = 0.15, yend = 0.3,
               color = "black", linewidth = 0.6) +
      annotate("segment", x = -0.85, xend = 0, y = 0.3, yend = 0.45,
               color = "black", linewidth = 0.6) +
      annotate("segment", x = 0.85, xend = 0, y = 0.3, yend = 0.45,
               color = "black", linewidth = 0.6) +
      annotate("rect", xmin = -1.1, xmax = 1.1, ymin = 1.2, ymax = 3.8,
               fill = NA, color = "gray55", linetype = "dashed",
               linewidth = 0.5) +
      annotate("rect", xmin = zl, xmax = zr, ymin = zb, ymax = zt,
               fill = NA, color = "black", linewidth = 0.9) +
      suppressWarnings(geom_point(
        aes(fill = TaggedPitchType, text = hover, customdata = PlayID),
        size = 4.4, shape = 21, color = "black", stroke = 0.4, alpha = 1)) +
      scale_fill_manual(values = pitch_colors, name = NULL) +
      coord_fixed(xlim = c(-2.2, 2.2), ylim = c(0, 4.2)) +
      theme_void() +
      theme(plot.background = element_rect(fill = "white", color = NA))
    plotly::ggplotly(p, tooltip = "text", source = pfx("loc")) %>%
      plotly::event_register("plotly_click")
  })

  # -- Video modal: Edger section + AWRE angle player ---------------------------
  bp_current_play <- reactiveVal(NULL)

  bp_open_video_modal <- function(play_id) {
    if (is.null(play_id) || !nzchar(as.character(play_id))) return(invisible())
    df <- bp_session_df()
    if (is.null(df) || !nrow(df)) return(invisible())
    hit <- df[bp_norm_id(df$PlayID) == bp_norm_id(play_id), , drop = FALSE]
    if (!nrow(hit)) return(invisible())
    row <- hit[1, ]

    info <- paste0(
      row$Pitcher, " — ", row$TaggedPitchType,
      " • ", sprintf("%.1f", row$RelSpeed), " mph",
      " • IVB/HB: ", sprintf("%.1f", row$InducedVertBreak),
      " / ", sprintf("%.1f", row$HorzBreak),
      " • Spin: ", sprintf("%.0f", row$SpinRate))

    # Edgertronic: mint fresh URLs (NULL when this pitch has none)
    edger <- NULL
    if (isTRUE(row$has_edger) && !is.na(row$edger_blob)) {
      blobs <- strsplit(
        if (!is.na(row$edger_all_blobs) && nzchar(row$edger_all_blobs))
          row$edger_all_blobs else row$edger_blob,
        "|", fixed = TRUE)[[1]]
      edger <- tryCatch(pv_edger_urls(row$session_id, blobs),
                        error = function(e) NULL)
    }

    # AWRE removed 2026-08-31 (was: exact sl_ angles + constructed fallback).
    # To restore, see the pre-removal palace_bullpen.R in git history.
    clips <- NULL

    bp_current_play(list(pid = bp_norm_id(play_id), clips = clips,
                         edger = edger, info = info,
                         framerate = row$framerate))

    showModal(modalDialog(
      title = div(
        span("Bullpen Video", class = "brand-teal", style = "font-weight:700;"),
        div(style = "font-size:13px; color:#555; font-weight:400;", info)
      ),
      uiOutput(pfx("edger_ui")),
      uiOutput(pfx("video_angle_ui")),
      uiOutput(pfx("video_player_ui")),
      size = "l", easyClose = TRUE, footer = modalButton("Close")
    ))
  }

  output[[pfx("edger_ui")]] <- renderUI({
    cur <- bp_current_play(); req(cur)
    if (is.null(cur$edger) || !length(cur$edger)) return(NULL)
    tagList(
      div(style = "font-weight:600; margin-bottom:4px;",
          icon("bolt"), " Edgertronic",
          if (!is.null(cur$framerate) && !is.na(cur$framerate))
            span(style = "color:#888; font-size:12px; font-weight:400;",
                 sprintf("  %s fps", format(round(cur$framerate),
                                            big.mark = ",")))),
      lapply(cur$edger, function(u)
        tags$video(src = u, controls = NA, autoplay = NA, muted = NA,
                   playsinline = NA,
                   style = "width:100%; margin-bottom:8px; border-radius:6px; background:#000;")),
      if (!is.null(cur$clips) && nrow(cur$clips) > 0) hr()
    )
  })

  output[[pfx("video_angle_ui")]] <- renderUI({
    cur <- bp_current_play(); req(cur)
    clips <- cur$clips
    if (is.null(clips) || nrow(clips) == 0) return(NULL)
    if (nrow(clips) == 1)
      return(div(style = "font-size:13px; color:#666; margin-bottom:6px;",
                 paste0("Angle: ", clips$angle[1])))
    radioButtons(pfx("video_angle"), "Camera Angle",
                 choiceNames = as.list(clips$angle),
                 choiceValues = as.list(clips$url),
                 selected = clips$url[1], inline = TRUE)
  })

  # AWRE removed: the modal shows Edgertronic only; with no Edger clip the
  # player pane shows a plain empty state.
  output[[pfx("video_player_ui")]] <- renderUI({
    cur <- bp_current_play(); req(cur)
    if (!is.null(cur$edger) && length(cur$edger)) return(NULL)
    div(style = "padding:24px; text-align:center; color:#777;",
        icon("video-slash"),
        tags$p(style = "margin-top:8px;", "No Edgertronic clip for this pitch."))
  })

  # -- Pitch Log ----------------------------------------------------------------
  output[[pfx("pitch_log")]] <- DT::renderDT({
    df <- bp_session_df()
    validate(need(!is.null(df) && nrow(df) > 0, "No pitches in scope."))
    watch_js <- sprintf(
      "Shiny.setInputValue('%s', '%%s', {priority: 'event'})", pfx("watch"))
    log_df <- df |>
      dplyr::transmute(
        Date  = format(Date, "%b %d"),
        `#`   = PitchNo,
        Time  = Time,
        Pitch = TaggedPitchType,
        Velo  = round(RelSpeed, 1),
        Spin  = round(SpinRate),
        Tilt,
        IVB   = round(InducedVertBreak, 1),
        HB    = round(HorzBreak, 1),
        `Rel Ht`   = round(RelHeight, 2),
        `Rel Side` = round(RelSide, 2),
        Ext   = round(Extension, 2),
        VAA   = round(VertApprAngle, 1),
        Zone  = ifelse(is.na(in_zone), "", ifelse(in_zone, "In", "Out")),
        Video = ifelse(
          has_edger & !is.na(PlayID),
          sprintf(paste0('<button class="btn btn-sm btn-primary" onclick="',
                         watch_js, '">&#9658;</button>'), PlayID),
          '<span class="text-muted">\u2014</span>')
      )
    DT::datatable(log_df, escape = FALSE, rownames = FALSE, selection = "none",
                  options = list(pageLength = 25, dom = "ftip", scrollX = TRUE),
                  class = "compact stripe hover")
  })

  observeEvent(input[[pfx("watch")]], bp_safe_open(input[[pfx("watch")]]))

  # Crash-proof, throttled modal opening: an error in the video chain shows a
  # notification instead of killing the R session, and clicks within 600ms of
  # the last accepted one are ignored so rapid clicking can't queue a pileup
  # of synchronous TrackMan calls.
  bp_last_click <- reactiveVal(Sys.time() - 10)
  bp_safe_open <- function(pid) {
    now <- Sys.time()
    if (as.numeric(now - isolate(bp_last_click()), units = "secs") < 0.6)
      return(invisible(NULL))
    bp_last_click(now)
    tryCatch(bp_open_video_modal(pid), error = function(e)
      showNotification(paste("Video error:", conditionMessage(e)),
                       type = "error", duration = 8))
  }

  for (src in c(pfx("move"), pfx("rel"), pfx("loc"))) local({
    s <- src
    observeEvent(plotly::event_data("plotly_click", source = s), {
      ed <- plotly::event_data("plotly_click", source = s)
      if (!is.null(ed$customdata)) bp_safe_open(ed$customdata[1])
    })
  })
}

palace_bullpen_history_server <- function(input, output, session, prefix = "sched") {
  p <- function(id) paste0(prefix, "_", id)
  staff_data <- bp_staff_data(session)

  # -- History: staff calendar ----------------------------------------------------
  observe({
    bpd <- staff_data(); req(bpd)
    mons <- sort(unique(format(bpd$Date, "%Y-%m")), decreasing = TRUE)
    labs <- format(as.Date(paste0(mons, "-01")), "%B %Y")
    sel  <- isolate(input[[p("hist_month")]])
    if (is.null(sel) || !sel %in% mons) sel <- mons[1]
    updateSelectInput(session, p("hist_month"),
                      choices = setNames(mons, labs), selected = sel)
  })

  output[[p("history_ui")]] <- renderUI({
    bpd <- staff_data(); req(bpd, input[[p("hist_month")]])
    mon   <- input[[p("hist_month")]]
    first <- as.Date(paste0(mon, "-01"))
    last  <- seq(first, by = "1 month", length.out = 2)[2] - 1
    days  <- seq(first, last, by = "1 day")

    md <- bpd[bpd$Date >= first & bpd$Date <= last, , drop = FALSE]
    per <- if (nrow(md)) {
      agg <- md |>
        dplyr::group_by(Date, Pitcher) |>
        dplyr::summarise(n = dplyr::n(),
                         t = suppressWarnings(min(Time, na.rm = TRUE)),
                         .groups = "drop")
      split(agg, agg$Date)
    } else list()

    day_cell <- function(d) {
      key <- as.character(d)
      ent <- per[[key]]
      body <- if (is.null(ent)) NULL else {
        ent <- ent[order(ent$t), , drop = FALSE]
        lapply(seq_len(nrow(ent)), function(i) {
          last_nm <- sub(",.*$", "", ent$Pitcher[i])
          tm <- ent$t[i]
          tm <- if (is.na(tm) || !nzchar(tm)) "" else substr(tm, 1, 5)
          div(style = "font-size:11px; line-height:1.5; white-space:nowrap;
                       overflow:hidden; text-overflow:ellipsis;",
              span(style = "font-weight:600;", last_nm),
              span(style = "color:#006F71;", sprintf(" %d", ent$n[i])),
              if (nzchar(tm)) span(style = "color:#999;", paste0(" \u00b7 ", tm)))
        })
      }
      div(style = paste0(
            "border:1px solid #e5e7eb; border-radius:6px; min-height:86px; ",
            "padding:4px 6px; background:",
            if (is.null(ent)) "#fafafa" else "#ffffff", ";"),
          div(style = "font-size:11px; color:#9ca3af;", format(d, "%d")),
          body)
    }

    lead_blanks <- (as.POSIXlt(first)$wday)   # 0 = Sunday
    cells <- c(replicate(lead_blanks, div(), simplify = FALSE),
               lapply(days, day_cell))
    hdr <- lapply(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), function(x)
      div(style = "text-align:center; font-weight:600; font-size:12px; color:#6b7280;", x))

    div(style = "display:grid; grid-template-columns:repeat(7, 1fr); gap:6px; margin-top:10px;",
        hdr, cells)
  })

}

palace_bullpen_leaderboard_server <- function(input, output, session, prefix = "bplb") {
  p <- function(id) paste0(prefix, "_", id)
  staff_data <- bp_staff_data(session)
  lb_known_players <- reactiveVal(character(0))

  # -- Leaderboard ------------------------------------------------------------------
  observe({
    bpd <- staff_data(); req(bpd)
    pl <- sort(unique(bpd$Pitcher))
    known <- isolate(lb_known_players())
    if (!identical(pl, known)) {
      lb_known_players(pl)
      sel <- isolate(input[[p("lb_players")]])
      shinyWidgets::updatePickerInput(session, p("lb_players"), choices = pl,
        selected = if (length(sel) && all(sel %in% pl)) sel else pl)
    }
    tp <- sort(unique(bpd$TaggedPitchType[!is.na(bpd$TaggedPitchType)]))
    updateSelectInput(session, p("lb_ptype"), choices = c("All", tp),
      selected = isolate(input[[p("lb_ptype")]]) %||% "All")
    rng <- range(bpd$Date, na.rm = TRUE)
    cur <- isolate(input[[p("lb_dates")]])
    if (is.null(cur) || any(is.na(cur)))
      updateDateRangeInput(session, p("lb_dates"), start = rng[1], end = rng[2],
                           min = rng[1], max = rng[2])
  })

  lb_data <- reactive({
    bpd <- staff_data(); req(bpd)
    pl <- input[[p("lb_players")]]
    if (!is.null(pl)) {
      if (length(pl) == 0) return(NULL)   # user deselected everyone
      bpd <- bpd[bpd$Pitcher %in% pl, , drop = FALSE]
    }
    dr <- input[[p("lb_dates")]]
    if (!is.null(dr) && !any(is.na(dr)))
      bpd <- bpd[bpd$Date >= dr[1] & bpd$Date <= dr[2], , drop = FALSE]
    tp <- input[[p("lb_ptype")]] %||% "All"
    if (!identical(tp, "All"))
      bpd <- bpd[!is.na(bpd$TaggedPitchType) & bpd$TaggedPitchType == tp, , drop = FALSE]
    if (!nrow(bpd)) return(NULL)
    bpd |>
      dplyr::group_by(Pitcher) |>
      dplyr::summarise(
        Pitches  = dplyr::n(),
        Sessions = dplyr::n_distinct(session_id),
        Velo = round(mean(RelSpeed, na.rm = TRUE), 1),
        Max  = round(suppressWarnings(max(RelSpeed, na.rm = TRUE)), 1),
        Spin = round(mean(SpinRate, na.rm = TRUE)),
        IVB  = round(mean(InducedVertBreak, na.rm = TRUE), 1),
        HB   = round(mean(HorzBreak, na.rm = TRUE), 1),
        Ext  = round(mean(Extension, na.rm = TRUE), 2),
        RelH = round(mean(RelHeight, na.rm = TRUE), 2),
        Zone = round(100 * mean(in_zone, na.rm = TRUE), 1),
        .groups = "drop") |>
      dplyr::mutate(dplyr::across(where(is.numeric),
                                  ~ ifelse(is.nan(.x) | is.infinite(.x), NA, .x)))
  })

  lb_gt <- reactive({
    ld <- lb_data()
    validate(need(!is.null(ld) && nrow(ld) > 0, "No pitches match the filters."))
    met <- input[[p("lb_metric")]] %||% "Velo"
    ld <- ld[order(-ld[[met]], ld$Pitcher, na.last = TRUE), , drop = FALSE]
    gwr <- function(x) {
      rng <- range(x, na.rm = TRUE)
      if (!all(is.finite(rng)) || rng[1] == rng[2]) return(rep("#ffffff", length(x)))
      scales::col_numeric(c("#d64541", "#ffffff", "#2ecc71"), domain = rng)(x)
    }
    tbl <- ld |>
      gt::gt(rowname_col = "Pitcher") |>
      gt::tab_header(title = "Bullpen Leaderboard",
                     subtitle = sprintf("Sorted by %s \u2022 %s", met,
                                        format(Sys.Date(), "%b %d, %Y"))) |>
      gt::sub_missing(missing_text = "\u2014") |>
      gt::cols_align("center", columns = gt::everything()) |>
      gt::tab_options(table.font.size = gt::px(13), data_row.padding = gt::px(5)) |>
      gt::tab_style(gt::cell_text(weight = "bold"),
                    locations = gt::cells_body(columns = met))
    for (cn in c("Velo","Max","Spin","IVB","HB","Ext","RelH","Zone","Pitches","Sessions"))
      tbl <- gt::data_color(tbl, columns = dplyr::all_of(cn), fn = gwr)
    tbl
  })

  output[[p("leader_table")]] <- gt::render_gt(lb_gt())

  lb_save <- function(file, kind) {
    tbl <- lb_gt()
    n <- nrow(lb_data() %||% data.frame())
    w <- 11; h <- max(2.5, 1.4 + 0.32 * n)
    if (kind == "png") grDevices::png(file, width = w, height = h, units = "in", res = 200)
    else grDevices::pdf(file, width = w, height = h)
    grid::grid.newpage()
    grid::grid.draw(gt::as_gtable(tbl))
    grDevices::dev.off()
  }
  output[[p("lb_png")]] <- downloadHandler(
    filename = function() sprintf("bullpen_leaderboard_%s.png", Sys.Date()),
    content  = function(file) lb_save(file, "png"))
  output[[p("lb_pdf")]] <- downloadHandler(
    filename = function() sprintf("bullpen_leaderboard_%s.pdf", Sys.Date()),
    content  = function(file) lb_save(file, "pdf"))

}
