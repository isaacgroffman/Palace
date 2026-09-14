# =============================================================================
# R/palace_batting_practice.R — Batting Practice for hitters, the way
# R/palace_bullpen.R serves bullpens for pitchers.
#
#   palace_bp_hitter_ui / palace_bp_hitter_server(prefix, hitter = NULL)
#       per-hitter view: stat line, spray / zone / contact charts, EV-bucket
#       heatmaps, trends, session log with per-session report downloads and a
#       date-range report. `hitter` = reactive display name (either "First
#       Last" or "Last, First") drives it from the player page; NULL adds a
#       dropdown (Bullpens > Batting Practice > Hitter).
#   palace_bp_leaderboard_ui / palace_bp_leaderboard_server(prefix)
#       team leaderboard over a date range (colour scale from
#       bp_leaderboard_colors), team trends, PDF leaderboard, all-hitter PDF.
#
# Data: Storage practice/hitting/ (index.json + per-session parquet) written
# by the Upload panel; the index is the version probe (60 s poll). Charts and
# PDFs come from R/palace_bp_reports.R on the frame bp_legacy_frame() builds.
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
# every BP swing in the store (report frame), cached per index version
bph_load <- function(force = FALSE) {
  if (!is.null(.bph_env$override)) return(.bph_env$override)      # tests / offline
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
  d <- bp_legacy_frame(dplyr::bind_rows(parts))
  .bph_env$data <- d; .bph_env$key <- key
  d
}
bp_hitter_has_data <- function(name) {
  if (is.null(name) || !nzchar(name)) return(FALSE)
  d <- tryCatch(bph_load(), error = function(e) NULL)
  !is.null(d) && bp_norm_name(name) %in% bp_norm_name(unique(d$Batter))
}
bph_resolve <- function(name, hitters) {
  if (is.null(name) || !nzchar(name) || !length(hitters)) return(NULL)
  m <- match(bp_norm_name(name), bp_norm_name(hitters)); if (is.na(m)) NULL else hitters[m]
}
bph_first_last <- function(x) sub("^\\s*([^,]+),\\s*(.+)\\s*$", "\\2 \\1", as.character(x))
bph_player_link <- function(name_lf, label = name_lf) {
  fl <- bph_first_last(name_lf)
  sprintf("<span class='tp-player-link' data-side='hitter' data-name='%s' style='cursor:pointer;font-weight:700;'>%s</span>",
          htmltools::htmlEscape(fl, attribute = TRUE), htmltools::htmlEscape(label))
}
bph_stats <- function(df) {
  df %>% dplyr::summarise(
    Swings = dplyr::n(), BBE = sum(BIPind, na.rm = TRUE),
    `Avg EV` = round(mean(ExitSpeed[BIPind == 1], na.rm = TRUE), 1), `Max EV` = round(suppressWarnings(max(ExitSpeed[BIPind == 1], na.rm = TRUE)), 1),
    `Avg LA` = round(mean(Angle[BIPind == 1], na.rm = TRUE), 1), `Max Dist` = round(suppressWarnings(max(Distance[BIPind == 1], na.rm = TRUE)), 0),
    `10-30%` = round(100 * sum(LA1030ind, na.rm = TRUE) / max(1, sum(BIPind, na.rm = TRUE)), 1),
    `HH%` = round(100 * sum(HHind, na.rm = TRUE) / max(1, sum(BIPind, na.rm = TRUE)), 1),
    `Barrel%` = round(100 * sum(Barrelind, na.rm = TRUE) / max(1, sum(BIPind, na.rm = TRUE)), 1), .groups = "drop") %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ ifelse(is.nan(.x) | is.infinite(.x), NA, .x)))
}
bph_staff_data <- function(session) {
  ver <- reactivePoll(60 * 1000, session, checkFunc = bph_version, valueFunc = function() Sys.time())
  reactive({ ver(); tryCatch(bph_load(), error = function(e) { cat("[bp] load failed:", conditionMessage(e), "\n"); NULL }) })
}
bph_empty <- function() div(class = "lb-empty", "No batting practice in the database yet. Admins: Upload → Batting practice pulls TrackMan BP sessions (game-endpoint BattingPractice and practice-endpoint Hitting).")

# ---- per-hitter view -------------------------------------------------------------------
palace_bp_hitter_ui <- function(prefix = "bph") {
  p <- function(id) paste0(prefix, "_", id)
  tagList(div(style = "margin-top:12px;"), uiOutput(p("controls")), uiOutput(p("body")))
}
palace_bp_hitter_server <- function(input, output, session, prefix = "bph", hitter = NULL) {
  p <- function(id) paste0(prefix, "_", id)
  all_data <- bph_staff_data(session)
  hitters <- reactive({ d <- all_data(); if (is.null(d)) character(0) else sort(unique(d$Batter)) })
  cur <- reactive({
    h <- hitters(); if (!length(h)) return(NULL)
    if (is.null(hitter)) { s <- input[[p("hitter")]]; if (is.null(s) || !s %in% h) h[1] else s } else bph_resolve(hitter(), h)
  })
  hd <- reactive({ d <- all_data(); nm <- cur(); req(d, nm); d[d$Batter == nm, , drop = FALSE] })
  # session scope: all sessions or one date
  sd <- reactive({
    d <- hd(); s <- input[[p("scope")]] %||% "all"
    if (identical(s, "all")) d else d[as.character(d$Date) == s, , drop = FALSE]
  })
  scope_label <- reactive({ s <- input[[p("scope")]] %||% "all"; if (identical(s, "all")) bp_date_label(hd()$Date) else format(as.Date(s), "%B %e, %Y") })

  output[[p("controls")]] <- renderUI({
    d <- all_data()
    if (is.null(d) || !nrow(d)) return(bph_empty())
    nm <- cur()
    if (is.null(nm)) return(div(class = "lb-empty", sprintf("No batting practice sessions for %s yet.", if (!is.null(hitter)) hitter() else "this hitter")))
    dd <- d[d$Batter == nm, , drop = FALSE]
    dates <- sort(unique(dd$Date), decreasing = TRUE)
    sel <- isolate(input[[p("scope")]]); if (is.null(sel) || !sel %in% c("all", as.character(dates))) sel <- "all"
    fluidRow(
      if (is.null(hitter)) column(3, selectInput(p("hitter"), "Hitter", choices = hitters(), selected = nm)),
      column(3, selectInput(p("scope"), "Sessions", choices = c("All sessions" = "all", stats::setNames(as.character(dates), format(dates, "%b %d, %Y"))), selected = sel)),
      column(6, div(style = "padding-top:32px; font-size:12px; color:#666;",
                    sprintf("%s: %d sessions, %s swings, %s balls in play. EV in mph, LA in degrees, distance in feet.",
                            nm, length(dates), format(nrow(dd), big.mark = ","), format(sum(dd$BIPind), big.mark = ",")))))
  })

  output[[p("body")]] <- renderUI({
    req(cur())
    tagList(
      fluidRow(column(12, h3("Summary", class = "brand-teal"), gt::gt_output(p("stat_table")))),
      hr(),
      fluidRow(column(6, plotOutput(p("spray"), height = "380px")), column(6, plotOutput(p("zone"), height = "380px"))),
      fluidRow(column(6, plotOutput(p("contact"), height = "360px")), column(6, plotOutput(p("contact_side"), height = "360px"))),
      hr(),
      h3("Location heatmaps by exit velocity", class = "brand-teal"),
      div(style = "font-size:12px; color:#666; margin-bottom:6px;", "Where the balls in play were pitched, split by how hard they were hit (catcher's view)."),
      tabsetPanel(id = p("heat_tabs"), type = "pills",
        tabPanel("Under 85", value = p("h_under85"), plotOutput(p("heat_under85"), height = "400px")),
        tabPanel("85 to 95", value = p("h_mid"), plotOutput(p("heat_mid"), height = "400px")),
        tabPanel("95+", value = p("h_plus95"), plotOutput(p("heat_plus95"), height = "400px"))),
      hr(),
      h3("Exit velo trends", class = "brand-teal"),
      fluidRow(column(6, plotOutput(p("trend_day"), height = "340px")), column(6, uiOutput(p("trend_week_ui")))),
      hr(),
      h3("Session log", class = "brand-teal"),
      div(style = "font-size:12px; color:#666; margin-bottom:6px;", "One row per session. Select a row to download that session's report; use the date range for a multi-session report (adds the trends pages)."),
      DT::DTOutput(p("session_log")),
      div(style = "display:flex; gap:10px; align-items:flex-end; flex-wrap:wrap; margin-top:10px;",
          downloadButton(p("dl_session"), "Session report (PDF)"),
          dateRangeInput(p("range"), "Report window", start = min(hd()$Date), end = max(hd()$Date), width = "260px"),
          downloadButton(p("dl_range"), "Report for window (PDF)"),
          downloadButton(p("dl_csv"), "Swings (CSV)")),
      hr(),
      h4("Swing log"), DT::DTOutput(p("log")))
  })

  output[[p("stat_table")]] <- gt::render_gt({
    d <- sd(); req(nrow(d) > 0)
    gt::gt(bph_stats(d)) |> gt::tab_header(title = cur(), subtitle = scope_label() %||% "") |>
      gt::sub_missing(missing_text = "—") |> gt::cols_align("center") |> gt::tab_options(table.font.size = gt::px(13)) |> gt_theme_guardian()
  })
  output[[p("spray")]]        <- renderPlot(create_bp_spray_chart(cur(), sd()))
  output[[p("zone")]]         <- renderPlot(create_bp_zone_plot(cur(), sd()))
  output[[p("contact")]]      <- renderPlot(create_bp_contact_map(cur(), sd()))
  output[[p("contact_side")]] <- renderPlot(create_bp_contact_side(cur(), sd()))
  for (b in names(BP_EV_BUCKETS)) local({ bb <- b; output[[p(paste0("heat_", bb))]] <- renderPlot(create_bp_ev_heatmap(sd(), cur(), bb)) })
  output[[p("trend_day")]] <- renderPlot(create_bp_ev_trend_chart(hd(), "day", cur(), team_data = all_data()))
  output[[p("trend_week_ui")]] <- renderUI({
    if (bp_is_multiweek(hd())) plotOutput(p("trend_week"), height = "340px")
    else div(style = "padding-top:140px; text-align:center; color:#9CA3AF; font-size:12px;", "Weekly trend appears once sessions span more than one week.")
  })
  output[[p("trend_week")]] <- renderPlot(create_bp_ev_trend_chart(hd(), "week", cur(), team_data = all_data()))

  session_rows <- reactive({
    d <- hd(); req(nrow(d) > 0)
    d %>% dplyr::group_by(Date, session_id) %>% dplyr::group_modify(~ bph_stats(.x)) %>% dplyr::ungroup() %>% dplyr::arrange(dplyr::desc(Date))
  })
  output[[p("session_log")]] <- DT::renderDT({
    s <- session_rows(); show <- s; show$Date <- format(show$Date, "%b %d, %Y"); show$session_id <- NULL
    DT::datatable(show, rownames = FALSE, selection = "single", class = "compact stripe", options = list(dom = "tp", pageLength = 10))
  })
  sel_session <- reactive({ i <- input[[p("session_log_rows_selected")]]; s <- session_rows(); if (length(i)) s[i[1], ] else s[1, ] })
  output[[p("dl_session")]] <- downloadHandler(
    filename = function() sprintf("BP_%s_%s.pdf", gsub("[^A-Za-z0-9]", "", cur()), format(sel_session()$Date)),
    content = function(file) { s <- sel_session(); d <- hd(); d <- d[d$session_id == s$session_id, , drop = FALSE]
      create_bp_pdf(d, cur(), file, report_title = "BP Report", date_label = format(s$Date, "%B %e, %Y")) })
  output[[p("dl_range")]] <- downloadHandler(
    filename = function() sprintf("BP_%s_%s_to_%s.pdf", gsub("[^A-Za-z0-9]", "", cur()), input[[p("range")]][1], input[[p("range")]][2]),
    content = function(file) { r <- input[[p("range")]]; d <- hd(); d <- d[d$Date >= r[1] & d$Date <= r[2], , drop = FALSE]
      validate(need(nrow(d) > 0, "No sessions in that window"))
      create_bp_pdf(d, cur(), file, report_title = "Weekly BP Report", date_label = bp_date_label(d$Date), trends = TRUE) })
  output[[p("dl_csv")]] <- downloadHandler(
    filename = function() sprintf("BP_%s_swings.csv", gsub("[^A-Za-z0-9]", "", cur())),
    content = function(file) utils::write.csv(sd(), file, row.names = FALSE, na = ""))
  output[[p("log")]] <- DT::renderDT({
    d <- sd(); req(nrow(d) > 0)
    show <- d[order(d$Date, d$PitchNo), c("Date", "PitchNo", "BatterSide", "RelSpeed", "ExitSpeed", "Angle", "Direction", "Distance", "Bearing", "HangTime", "Barrelind"), drop = FALSE]
    names(show) <- c("Date", "#", "Side", "Pitch Velo", "EV", "LA", "Dir", "Dist", "Bearing", "Hang", "Barrel")
    show$Barrel <- ifelse(show$Barrel == 1, "✓", "")
    DT::datatable(show, rownames = FALSE, selection = "none", class = "compact stripe", options = list(pageLength = 25, scrollX = TRUE)) |>
      DT::formatRound(c("Pitch Velo", "EV", "LA", "Dir", "Bearing", "Hang"), 1) |> DT::formatRound("Dist", 0)
  })
}

# ---- team leaderboard --------------------------------------------------------------------
palace_bp_leaderboard_ui <- function(prefix = "bplbh") {
  p <- function(id) paste0(prefix, "_", id)
  tagList(div(style = "margin-top:12px;"), uiOutput(p("controls")),
          div(style = "font-size:12px; color:#666; margin:2px 0 8px;", "Balls in play from every BP session in the window, one row per hitter (switch hitters per side). Click a name to open the profile."),
          gt::gt_output(p("table")), hr(), h4("Team exit velo trends", class = "brand-teal"), plotOutput(p("trend"), height = "340px"))
}
palace_bp_leaderboard_server <- function(input, output, session, prefix = "bplbh") {
  p <- function(id) paste0(prefix, "_", id)
  all_data <- bph_staff_data(session)
  output[[p("controls")]] <- renderUI({
    d <- all_data(); if (is.null(d) || !nrow(d)) return(bph_empty())
    dates <- sort(unique(d$Date)); last <- max(dates)
    r <- isolate(input[[p("range")]]); if (is.null(r) || length(r) != 2 || any(is.na(r))) r <- c(max(min(dates), last - 6), last)
    fluidRow(
      column(3, dateRangeInput(p("range"), "Window", start = r[1], end = r[2], min = min(dates), max = last)),
      column(4, div(style = "padding-top:25px; display:flex; gap:6px; flex-wrap:wrap;",
                    actionButton(p("last_session"), "Last session"), actionButton(p("last_week"), "Last 7 days"), actionButton(p("all"), "All"))),
      column(5, div(style = "padding-top:25px; display:flex; gap:6px; flex-wrap:wrap;",
                    downloadButton(p("dl_pdf"), "Leaderboard (PDF)"), downloadButton(p("dl_team"), "Every hitter's report (PDF)"))))
  })
  observeEvent(input[[p("last_session")]], { d <- all_data(); req(d); updateDateRangeInput(session, p("range"), start = max(d$Date), end = max(d$Date)) })
  observeEvent(input[[p("last_week")]], { d <- all_data(); req(d); updateDateRangeInput(session, p("range"), start = max(min(d$Date), max(d$Date) - 6), end = max(d$Date)) })
  observeEvent(input[[p("all")]], { d <- all_data(); req(d); updateDateRangeInput(session, p("range"), start = min(d$Date), end = max(d$Date)) })
  wd <- reactive({ d <- all_data(); r <- input[[p("range")]]; req(d, length(r) == 2); d[d$Date >= r[1] & d$Date <= r[2], , drop = FALSE] })
  lb <- reactive({ d <- wd(); validate(need(nrow(d) > 0, "No BP sessions in that window.")); calculate_bp_leaderboard(d) })
  lb_gt <- function(plain = FALSE) {
    l <- lb(); color_cols <- c("Avg EV", "Max EV", "Max Dist", "10-30%", "HH%", "Barrel%")
    if (!plain) l$Batter <- vapply(l$Batter, function(x) bph_player_link(sub(" \\((LHH|RHH)\\)$", "", x), label = x), character(1))
    tbl <- gt::gt(l) |> gt::tab_header(title = "BP Leaderboard", subtitle = bp_date_label(wd()$Date) %||% "") |>
      gt::sub_missing(missing_text = "—") |> gt::cols_align("center", columns = gt::everything()) |> gt::cols_align("left", columns = "Batter") |>
      gt::cols_label(Batter = "Player") |> gt::tab_options(table.font.size = gt::px(13), data_row.padding = gt::px(5))
    if (!plain) tbl <- gt::fmt_markdown(tbl, columns = "Batter")
    for (cn in color_cols) tbl <- gt::data_color(tbl, columns = dplyr::all_of(cn), fn = bp_leaderboard_colors)
    tbl
  }
  output[[p("table")]] <- gt::render_gt(lb_gt())
  output[[p("trend")]] <- renderPlot({ d <- wd(); req(nrow(d) > 0); create_bp_ev_trend_chart(d, if (bp_is_multiweek(d)) "week" else "day") })
  output[[p("dl_pdf")]] <- downloadHandler(
    filename = function() sprintf("BP_leaderboard_%s_to_%s.pdf", input[[p("range")]][1], input[[p("range")]][2]),
    content = function(file) create_bp_leaderboard_pdf(lb(), bp_date_label(wd()$Date), file, bp_data = wd()))
  output[[p("dl_team")]] <- downloadHandler(
    filename = function() sprintf("BP_reports_%s_to_%s.pdf", input[[p("range")]][1], input[[p("range")]][2]),
    content = function(file) { d <- wd(); weekly <- bp_is_weekly(d)
      create_bp_team_pdf(d, file, report_title = if (weekly) "Weekly BP Report" else "BP Report", date_label = bp_date_label(d$Date), trends = weekly) })
}
