# =============================================================================
# R/palace_batting_practice.R — the Batting Practice view (Bullpens tab pill).
#
# Data: TrackMan "Hitting" practice sessions committed by the Upload panel
# (R/palace_upload.R, type "bp"). Each session is a parquet under Storage
# practice/hitting/, listed in practice/hitting/index.json; the index is the
# version probe, so a commit shows up here within a minute. Columns are the
# snake_case names in UP_BP_COLS (exit_speed, launch_angle, direction,
# distance, bearing, hang_time, batter, pitcher, ...).
#
#   palace_bp_ui("bph") / palace_bp_server(input, output, session, "bph")
# =============================================================================
if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.bph_env <- new.env(parent = emptyenv())
BPH_INDEX <- "practice/hitting/index.json"

bph_index <- function(force = FALSE) {
  if (!force && !is.null(.bph_env$index) && difftime(Sys.time(), .bph_env$index_at, units = "secs") < 45) return(.bph_env$index)
  idx <- NULL
  if (sb_storage_enabled()) {
    dest <- tempfile(fileext = ".json"); on.exit(unlink(dest))
    if (sb_storage_download(BPH_INDEX, dest)) idx <- tryCatch(jsonlite::fromJSON(dest, simplifyVector = TRUE), error = function(e) NULL)
  }
  s <- if (!is.null(idx) && is.data.frame(idx$sessions)) idx$sessions else data.frame()
  .bph_env$index <- s; .bph_env$index_at <- Sys.time()
  s
}
bph_version <- function() {
  s <- tryCatch(bph_index(), error = function(e) data.frame())
  if (!nrow(s)) "" else paste(nrow(s), max(as.character(s$uploaded_at %||% "")), sum(as.integer(s$n_rows %||% 0)))
}
# every BP swing in the store, one frame; cached per index version
bph_load <- function(force = FALSE) {
  s <- bph_index(force); if (!nrow(s)) return(NULL)
  key <- paste(s$file, s$uploaded_at %||% "", collapse = "|")
  if (!force && identical(.bph_env$key, key) && !is.null(.bph_env$data)) return(.bph_env$data)
  parts <- lapply(s$file, function(f) {
    dest <- tempfile(fileext = ".parquet"); on.exit(unlink(dest))
    if (!sb_storage_download(f, dest)) return(NULL)
    tryCatch(as.data.frame(arrow::read_parquet(dest)), error = function(e) NULL)
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) return(NULL)
  d <- dplyr::bind_rows(parts)
  d$Date <- as.Date(d$session_date)
  d$batter[is.na(d$batter) | !nzchar(d$batter)] <- "Unknown"
  d$hard_hit <- !is.na(d$exit_speed) & d$exit_speed >= 95
  d$sweet_spot <- !is.na(d$launch_angle) & d$launch_angle >= 8 & d$launch_angle <= 32
  .bph_env$data <- d; .bph_env$key <- key
  d
}
bph_summary <- function(d, by = "batter") {
  if (is.null(d) || !nrow(d)) return(tibble::tibble())
  d |>
    dplyr::group_by(.g = .data[[by]]) |>
    dplyr::summarise(
      Swings = dplyr::n(), Tracked = sum(!is.na(exit_speed)),
      `Avg EV` = round(mean(exit_speed, na.rm = TRUE), 1), `Max EV` = round(suppressWarnings(max(exit_speed, na.rm = TRUE)), 1),
      `EV90` = round(suppressWarnings(stats::quantile(exit_speed, 0.9, na.rm = TRUE, names = FALSE)), 1),
      `Avg LA` = round(mean(launch_angle, na.rm = TRUE), 1),
      `Hard %` = round(100 * mean(hard_hit[!is.na(exit_speed)]), 0),
      `Sweet %` = round(100 * mean(sweet_spot[!is.na(exit_speed)]), 0),
      `Avg Dist` = round(mean(distance, na.rm = TRUE), 0), `Max Dist` = round(suppressWarnings(max(distance, na.rm = TRUE)), 0),
      .groups = "drop") |>
    dplyr::rename(!!(if (by == "batter") "Hitter" else "Date") := .g) |>
    dplyr::arrange(dplyr::desc(Swings)) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ ifelse(is.nan(.x) | is.infinite(.x), NA, .x)))
}

palace_bp_ui <- function(prefix = "bph") {
  p <- function(id) paste0(prefix, "_", id)
  tagList(div(style = "margin-top:12px;"), uiOutput(p("controls")), uiOutput(p("body")))
}

palace_bp_server <- function(input, output, session, prefix = "bph") {
  p <- function(id) paste0(prefix, "_", id)
  ver <- reactivePoll(60 * 1000, session, checkFunc = bph_version, valueFunc = function() Sys.time())
  all_data <- reactive({ ver(); tryCatch(bph_load(), error = function(e) { cat("[bp] load failed:", conditionMessage(e), "\n"); NULL }) })

  output[[p("controls")]] <- renderUI({
    d <- all_data()
    if (is.null(d) || !nrow(d))
      return(div(class = "lb-empty", "No batting practice in the database yet. Admins: Upload → Batting practice pulls TrackMan BP sessions (game-endpoint BattingPractice and practice-endpoint Hitting)."))
    dates <- sort(unique(d$Date), decreasing = TRUE)
    hitters <- sort(unique(d$batter))
    sel_d <- isolate(input[[p("date")]]); if (is.null(sel_d) || !sel_d %in% c("all", as.character(dates))) sel_d <- "all"
    sel_h <- isolate(input[[p("hitter")]]); if (is.null(sel_h) || !sel_h %in% c("all", hitters)) sel_h <- "all"
    fluidRow(
      column(3, selectInput(p("date"), "Session date", choices = c("All dates" = "all", stats::setNames(as.character(dates), format(dates, "%b %d, %Y"))), selected = sel_d)),
      column(3, selectInput(p("hitter"), "Hitter", choices = c("All hitters" = "all", hitters), selected = sel_h)),
      column(6, div(style = "padding-top:32px; font-size:12px; color:#666;",
                    sprintf("%d sessions, %s swings, %d hitters. Exit velocity in mph, launch angle in degrees, distance in feet.",
                            length(unique(d$session_id)), format(nrow(d), big.mark = ","), length(hitters)))))
  })
  sel <- reactive({
    d <- all_data(); req(d)
    if (!identical(input[[p("date")]] %||% "all", "all")) d <- d[as.character(d$Date) == input[[p("date")]], , drop = FALSE]
    if (!identical(input[[p("hitter")]] %||% "all", "all")) d <- d[d$batter == input[[p("hitter")]], , drop = FALSE]
    d
  })
  output[[p("body")]] <- renderUI({
    req(all_data())
    tagList(
      fluidRow(column(12, h3("Summary", class = "brand-teal"), gt::gt_output(p("summary")))),
      hr(),
      fluidRow(column(6, div(style = "text-align:center;", h4("Exit velocity vs launch angle")), plotly::plotlyOutput(p("ev_la"), height = "440px")),
               column(6, div(style = "text-align:center;", h4("Spray (landing)")), plotly::plotlyOutput(p("spray"), height = "440px"))),
      hr(),
      h4("Swing log"), DT::DTOutput(p("log")))
  })
  output[[p("summary")]] <- gt::render_gt({
    d <- sel(); req(nrow(d) > 0)
    by <- if (identical(input[[p("hitter")]] %||% "all", "all")) "batter" else "session_date"
    s <- bph_summary(d, by)
    gt::gt(s) |> gt::sub_missing(missing_text = "—") |> gt::tab_options(table.font.size = gt::px(13)) |> gt_theme_guardian()
  })
  output[[p("ev_la")]] <- plotly::renderPlotly({
    d <- sel(); d <- d[!is.na(d$exit_speed) & !is.na(d$launch_angle), , drop = FALSE]; req(nrow(d) > 0)
    plotly::plot_ly(d, x = ~launch_angle, y = ~exit_speed, color = ~batter, type = "scatter", mode = "markers",
                    marker = list(size = 9, opacity = 0.8),
                    text = ~sprintf("%s<br>%s #%s<br>EV %.1f  LA %.1f<br>Dist %s ft", batter, format(Date, "%b %d"), pitch_no, exit_speed, launch_angle, ifelse(is.na(distance), "—", round(distance))),
                    hoverinfo = "text") |>
      plotly::layout(xaxis = list(title = "Launch angle (°)", range = c(-40, 70)), yaxis = list(title = "Exit velocity (mph)"),
                     shapes = list(list(type = "rect", x0 = 8, x1 = 32, y0 = 95, y1 = 120, fillcolor = "rgba(14,110,112,0.08)", line = list(width = 0))),
                     legend = list(orientation = "h"))
  })
  output[[p("spray")]] <- plotly::renderPlotly({
    d <- sel(); d <- d[!is.na(d$distance) & !is.na(d$bearing), , drop = FALSE]; req(nrow(d) > 0)
    d$x <- d$distance * sin(d$bearing * pi / 180); d$y <- d$distance * cos(d$bearing * pi / 180)
    th <- seq(-45, 45, length.out = 60) * pi / 180
    fence <- data.frame(x = 330 * sin(th), y = 330 * cos(th))
    plotly::plot_ly() |>
      plotly::add_lines(data = fence, x = ~x, y = ~y, line = list(color = "#9CA3AF", width = 1), showlegend = FALSE, hoverinfo = "none") |>
      plotly::add_segments(x = 0, y = 0, xend = 330 * sin(-pi / 4), yend = 330 * cos(-pi / 4), line = list(color = "#9CA3AF", width = 1), showlegend = FALSE, hoverinfo = "none") |>
      plotly::add_segments(x = 0, y = 0, xend = 330 * sin(pi / 4), yend = 330 * cos(pi / 4), line = list(color = "#9CA3AF", width = 1), showlegend = FALSE, hoverinfo = "none") |>
      plotly::add_markers(data = d, x = ~x, y = ~y, color = ~batter, marker = list(size = 9, opacity = 0.8),
                          text = ~sprintf("%s<br>EV %s  LA %s<br>%s ft", batter, round(exit_speed, 1), round(launch_angle, 1), round(distance)), hoverinfo = "text") |>
      plotly::layout(xaxis = list(title = "", range = c(-260, 260), zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(title = "", range = c(-10, 360), zeroline = FALSE, showticklabels = FALSE, scaleanchor = "x"),
                     legend = list(orientation = "h"))
  })
  output[[p("log")]] <- DT::renderDT({
    d <- sel(); req(nrow(d) > 0)
    show <- d[order(d$Date, d$batter, d$pitch_no), intersect(c("Date", "batter", "batter_side", "pitch_no", "pitcher", "tagged_pitch_type", "rel_speed",
                                                               "exit_speed", "launch_angle", "direction", "distance", "bearing", "hang_time", "hit_spin_rate"), names(d)), drop = FALSE]
    names(show) <- c("Date", "Hitter", "Side", "#", "Pitcher", "Pitch", "Velo", "EV", "LA", "Dir", "Dist", "Bearing", "Hang", "Spin")[seq_len(ncol(show))]
    DT::datatable(show, rownames = FALSE, selection = "none", class = "compact stripe", options = list(pageLength = 25, scrollX = TRUE)) |>
      DT::formatRound(intersect(c("Velo", "EV", "LA", "Dir", "Bearing", "Hang"), names(show)), 1) |>
      DT::formatRound(intersect(c("Dist", "Spin"), names(show)), 0)
  })
}
