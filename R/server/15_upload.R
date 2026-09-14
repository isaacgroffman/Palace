# =============================================================================
# UPLOAD DATA (admins) — the panel over R/palace_upload.R.
#
# Pills: 1 Fetch -> 2 Review -> 3 Commit, plus Completeness, History, Setup.
# Every long step runs in a callr child (up_bg_start); this file only polls
# the job directory, so the app stays responsive for everyone else.
# Inputs / outputs are prefixed up_. Everything is gated on is_admin().
# =============================================================================
up_rv <- reactiveValues(job = NULL, type = NULL, bg = NULL, status = NULL,
                        staged = NULL, sessions = NULL, issues = NULL, edits = up_edits_default(),
                        pre = NULL, pre_bg = NULL, comp = NULL, comp_bg = NULL, hist_tick = 0L)
up_busy <- function() up_bg_alive(up_rv$bg)

# the Upload tab is hidden by CSS until the server confirms an admin
observeEvent(input$app_ui_ready, {
  session$sendCustomMessage("palaceAdmin", isTRUE(isolate(is_admin())))
})
observeEvent(current_user(), {
  session$sendCustomMessage("palaceAdmin", isTRUE(is_admin()))
}, ignoreNULL = FALSE, ignoreInit = TRUE)

output$upload_page <- renderUI({
  if (!isTRUE(is_admin())) return(div(class = "lb-empty", "Admins only."))
  tagList(
    div(style = "text-align:center; margin:16px 0 4px;",
        h3("Upload data", class = "brand-teal", style = "margin-bottom:2px;"),
        div(style = "color:#6B7280; font-size:13px;",
            "Pull sessions straight from the TrackMan Data API, review them, then commit them to Supabase. Nothing is written until you press Commit.")),
    tabsetPanel(id = "upload_subtabs", type = "pills",
      tabPanel("1 · Fetch",   value = "up_fetch",   uiOutput("up_fetch_ui")),
      tabPanel("2 · Review",  value = "up_review",  uiOutput("up_review_ui")),
      tabPanel("3 · Commit",  value = "up_commit",  uiOutput("up_commit_ui")),
      tabPanel("Completeness",     value = "up_check",   uiOutput("up_check_ui")),
      tabPanel("History",          value = "up_history", uiOutput("up_history_ui")),
      tabPanel("Setup check",      value = "up_setup",   uiOutput("up_setup_ui"))))
})

# ---- shared bits -------------------------------------------------------------------------
up_progress_ui <- function(st, running_label = "Working") {
  if (is.null(st)) return(NULL)
  p <- st$progress; m <- st$meta
  pct <- as.numeric(p$pct %||% 0); msg <- p$msg %||% ""
  status <- m$status %||% ""
  failed <- grepl("failed", status)
  tagList(
    if (st$alive || grepl("ing$", status)) div(style = "font-size:12px; color:#0E6E70; font-weight:600; margin-top:8px;",
        running_label, "… ", sprintf("%d%%", round(pct)), " — ", msg),
    div(class = "progress", style = "height:10px; margin:6px 0;",
        div(class = paste("progress-bar", if (failed) "progress-bar-danger" else if (pct >= 100) "progress-bar-success" else ""),
            role = "progressbar", style = sprintf("width:%d%%;", round(pct)))),
    if (failed) div(style = "background:#FDECEA; border:1px solid #F5C2C0; border-radius:8px; padding:8px 10px; margin:6px 0; font-size:12.5px;",
                    tags$b("Failed: "), m$error %||% "unknown error", tags$br(), tags$span(style = "color:#7F1D1D;", m$hint %||% "")),
    if (length(st$log)) tags$details(open = if (st$alive || failed) NA else NULL, style = "margin-top:4px;",
        tags$summary(style = "font-size:12px; color:#6B7280; cursor:pointer;", "Job log"),
        tags$pre(style = "font-size:11px; max-height:220px; overflow:auto; background:#F7F9FA; padding:6px;", paste(st$log, collapse = "\n"))))
}
up_issue_table <- function(issues) {
  d <- up_issues_df(issues)
  if (!nrow(d)) return(div(style = "font-size:12.5px; color:#166534; margin:6px 0;", "✓ No validation issues."))
  DT::datatable(d, rownames = FALSE, selection = "none", class = "compact stripe",
                options = list(dom = "t", pageLength = 50, ordering = FALSE)) |>
    DT::formatStyle("Level", target = "row", backgroundColor = DT::styleEqual(c("error", "warn", "info"), c("#FDECEA", "#FEF6E4", "#EEF6FF")))
}
up_kv <- function(...) {
  kv <- list(...)
  tags$table(class = "table table-condensed", style = "font-size:12.5px; width:auto; margin-bottom:8px;",
             lapply(names(kv), function(k) tags$tr(tags$th(style = "padding-right:14px; white-space:nowrap;", k), tags$td(kv[[k]]))))
}
up_load_staged <- function() {
  job <- up_rv$job; req(job)
  up_rv$staged   <- tryCatch(up_job_staged(job), error = function(e) NULL)
  up_rv$sessions <- tryCatch(up_job_sessions(job), error = function(e) NULL)
  up_rv$issues   <- tryCatch(up_job_issues(job), error = function(e) list())
  up_rv$edits    <- up_edits_read(job)
}
# poll the running job (fetch or commit)
observe({
  job <- up_rv$job; bg <- up_rv$bg; req(job)
  st <- up_job_status(job, bg)
  up_rv$status <- st
  if (st$alive) { invalidateLater(1500); return() }
  if (identical(st$meta$status, "staged") && is.null(isolate(up_rv$staged))) up_load_staged()
})

# ---- 1 Fetch -----------------------------------------------------------------------------
output$up_fetch_ui <- renderUI({
  req(is_admin())
  s <- up_seasons(); sel_season <- isolate(input$up_season) %||% up_season_default()
  r <- up_season_row(sel_season)
  choices <- stats::setNames(names(UPLOAD_TYPES), vapply(UPLOAD_TYPES, function(t) paste(t$icon, t$label), character(1)))
  tagList(
    div(style = "margin-top:12px;"),
    fluidRow(
      column(4, radioButtons("up_type", "What to upload", choices = choices, selected = isolate(input$up_type) %||% "bullpens"),
                uiOutput("up_type_blurb")),
      column(4, selectInput("up_season", "Season window", choices = stats::setNames(s$key, s$label), selected = sel_season),
                dateRangeInput("up_dates", "Dates (edit freely)", start = min(r$from, Sys.Date()), end = min(r$to, Sys.Date())),
                conditionalPanel("input.up_type == 'games' || input.up_type == 'bp'", selectInput("up_scope", "Sessions to include", UPLOAD_SCOPES, selected = "ccu")),
                conditionalPanel("input.up_type == 'bullpens'", checkboxInput("up_video", "Index Edgertronic clips (slower, needed for video)", TRUE)),
                conditionalPanel("input.up_type == 'trumedia'", textInput("up_lb_args", "build_leaderboards.R arguments", value = "", placeholder = "--pipeline-dir serving_build/master_2026"))),
      column(4, div(style = "padding-top:26px;",
                    actionButton("up_fetch_go", "Fetch from TrackMan", class = "btn-primary btn-block"),
                    uiOutput("up_fetch_status")))),
    hr(), uiOutput("up_fetch_result"))
})
output$up_type_blurb <- renderUI({
  t <- UPLOAD_TYPES[[input$up_type %||% "bullpens"]]; req(t)
  div(style = "font-size:12px; color:#4B5563; background:#F4F7F8; border-radius:8px; padding:8px 10px;",
      t$blurb, tags$br(), tags$b("Writes to: "), t$target)
})
observeEvent(input$up_season, {
  r <- up_season_row(input$up_season); req(nrow(r))
  updateDateRangeInput(session, "up_dates", start = min(r$from, Sys.Date()), end = min(r$to, Sys.Date()))
}, ignoreInit = TRUE)

observeEvent(input$up_fetch_go, {
  req(is_admin())
  if (up_busy()) { showNotification("A job is already running; wait for it to finish.", type = "warning"); return() }
  d <- input$up_dates; req(length(d) == 2, !any(is.na(d)))
  if (d[2] < d[1]) { showNotification("The end date is before the start date.", type = "error"); return() }
  job <- tryCatch(up_job_create(input$up_type, d[1], d[2], user = current_user()$username %||% "", scope = input$up_scope %||% "all",
                                season_key = input$up_season,
                                opts = list(with_video = isTRUE(input$up_video), lb_args = input$up_lb_args %||% "")),
                  error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
  req(job)
  up_rv$job <- job; up_rv$type <- input$up_type; up_rv$status <- NULL
  up_rv$staged <- NULL; up_rv$sessions <- NULL; up_rv$issues <- NULL; up_rv$edits <- up_edits_default()
  up_rv$bg <- up_bg_start("fetch", "up_fetch", list(job_dir = job), job_dir = job)
})
output$up_fetch_status <- renderUI({
  st <- up_rv$status; req(st)
  if (!identical(st$meta$type, up_rv$type)) return(NULL)
  up_progress_ui(st, if (identical(st$meta$status, "committing")) "Committing" else "Fetching")
})
output$up_fetch_result <- renderUI({
  st <- up_rv$status; req(st, !st$alive)
  m <- st$meta; req(m$status %in% c("staged", "committed", "dry_run_done", "commit_failed"))
  sess <- up_rv$sessions; issues <- up_rv$issues
  tagList(
    up_kv("Job" = m$id, "Type" = m$type_label, "Window" = paste(m$from, "to", m$to),
          "Sessions" = sprintf("%d pulled of %d found", as.integer(m$n_sessions %||% 0), as.integer(m$n_sessions_found %||% 0)),
          "Rows staged" = format(as.integer(m$n_rows %||% 0), big.mark = ",")),
    if (identical(m$type, "trumedia")) div(style = "font-size:12.5px;", "Season stats have no fetch step: go to Commit to run the rebuild.")
    else tagList(
      h5("Validation"), up_issue_table(issues),
      h5("Sessions"), DT::renderDT({
        req(sess)
        show <- intersect(c("date", "sessionId", "source", "status", "n_rows", "note", "sessionType", "externalSessionId", "verified"), names(sess))
        DT::datatable(sess[, show, drop = FALSE], rownames = FALSE, selection = "none", class = "compact stripe",
                      options = list(pageLength = 15, scrollX = TRUE)) |>
          DT::formatStyle("status", backgroundColor = DT::styleEqual(c("ok", "failed", "empty"), c("#E7F6EC", "#FDECEA", "#FEF6E4")))
      }, server = FALSE),
      div(style = "margin-top:10px; font-size:12.5px; color:#0E6E70; font-weight:600;",
          if (as.integer(m$n_rows %||% 0) > 0) "Next: open 2 · Review to inspect and edit, then 3 · Commit." else "Nothing to review: widen the window or change the type.")))
})

# ---- 2 Review ----------------------------------------------------------------------------
up_review_cols <- function(df, type) {
  want <- switch(type,
    games = c("pitchUID", "date", "sessionId", "pitcher.name", "pitcher.throws", "batter.name", "pitchTag.taggedPitchType", "pitchTag.pitchCall",
              "pitchTag.playResult", "pitch.release.relSpeed", "pitch.release.spinRate", "pitch.trajectory.inducedVertBreak", "pitch.trajectory.horzBreak",
              "pitch.location.plateLocSide", "pitch.location.plateLocHeight", "hit.launch.exitSpeed", "hit.launch.angle", "homeTeam.name", "awayTeam.name"),
    bp    = c("play_id", "session_date", "session_id", "batter", "batter_side", "pitcher", "pitch_no", "tagged_pitch_type", "rel_speed",
              "exit_speed", "launch_angle", "direction", "distance", "bearing", "hang_time", "hit_spin_rate", "plate_loc_side", "plate_loc_height"),
    c("play_id", "session_date", "session_id", "pitcher", "pitcher_throws", "pitch_no", "tagged_pitch_type", "rel_speed", "spin_rate", "spin_efficiency",
      "induced_vert_break", "horz_break", "vert_appr_angle", "rel_height", "rel_side", "extension", "plate_loc_side", "plate_loc_height", "has_edger"))
  intersect(want, names(df))
}
up_view <- reactive({
  df <- up_rv$staged; req(df, nrow(df) > 0)
  up_apply_edits(df, up_rv$edits, up_id_col(up_rv$type), up_sess_col(up_rv$type), drop_excluded = FALSE)
})
output$up_review_ui <- renderUI({
  req(is_admin())
  df <- up_rv$staged; type <- isolate(up_rv$type)
  if (is.null(df) || !nrow(df)) return(div(class = "lb-empty", "Nothing staged yet: run a fetch first."))
  sess <- isolate(up_rv$sessions); edits <- isolate(up_rv$edits)
  ok <- sess[sess$status == "ok", , drop = FALSE]
  labs <- sprintf("%s · %s · %s rows", ok$date, ifelse(nzchar(ok$note), ok$note, substr(ok$sessionId, 1, 8)), ok$n_rows)
  tagList(
    div(style = "margin-top:12px;"),
    fluidRow(
      column(4, h5("Sessions to include"),
             checkboxGroupInput("up_sess_keep", NULL, choices = stats::setNames(ok$sessionId, labs),
                                selected = setdiff(ok$sessionId, edits$exclude_sessions))),
      column(8, h5("Find & replace (exact match, whole column)"),
             fluidRow(column(4, selectInput("up_fr_col", "Column", choices = names(df), selected = if (type == "games") "pitcher.name" else if (type == "bp") "batter" else "pitcher")),
                      column(3, textInput("up_fr_from", "Find")), column(3, textInput("up_fr_to", "Replace with")),
                      column(2, actionButton("up_fr_go", "Apply", style = "margin-top:25px;"))),
             div(style = "display:flex; gap:6px; flex-wrap:wrap; margin:4px 0 8px;",
                 actionButton("up_excl_rows", "Exclude selected rows"), actionButton("up_incl_rows", "Re-include selected rows"),
                 actionButton("up_reset_edits", "Reset all edits"),
                 downloadButton("up_dl_csv", "CSV"), downloadButton("up_dl_parquet", "Parquet")),
             checkboxInput("up_all_cols", "Show every column", FALSE),
             uiOutput("up_edit_summary"))),
    div(style = "font-size:12px; color:#666; margin-bottom:4px;",
        "Double-click a cell to edit it. Excluded rows are greyed and dropped at commit. The staged file itself is never changed."),
    DT::DTOutput("up_review_table"))
})
output$up_edit_summary <- renderUI({
  e <- up_rv$edits; v <- up_view()
  div(style = "font-size:12.5px; color:#374151;",
      sprintf("%s rows staged, %s excluded (%d by session, %d by row), %d cell edit(s), %d find/replace rule(s). %s rows will be committed.",
              format(nrow(v), big.mark = ","), format(sum(v$.excluded), big.mark = ","),
              length(e$exclude_sessions), length(e$exclude_rows), length(e$cells), length(e$replace),
              format(sum(!v$.excluded), big.mark = ",")))
})
output$up_review_table <- DT::renderDT({
  v <- up_view(); type <- up_rv$type
  cols <- if (isTRUE(input$up_all_cols)) setdiff(names(v), c(".excluded", "raw_json")) else up_review_cols(v, type)
  idc <- up_id_col(type)
  show <- v[, unique(c(".excluded", idc, cols)), drop = FALSE]
  names(show)[1] <- "excluded"
  DT::datatable(show, rownames = FALSE, selection = "multiple", filter = "top", class = "compact stripe",
                editable = list(target = "cell", disable = list(columns = c(0, 1))),
                options = list(pageLength = 25, scrollX = TRUE, autoWidth = FALSE, searchDelay = 400)) |>
    DT::formatStyle("excluded", target = "row", color = DT::styleEqual(c(TRUE, FALSE), c("#9CA3AF", "#111827")))
}, server = TRUE)
observeEvent(input$up_review_table_cell_edit, {
  info <- input$up_review_table_cell_edit; v <- up_view(); type <- up_rv$type
  cols <- if (isTRUE(input$up_all_cols)) setdiff(names(v), c(".excluded", "raw_json")) else up_review_cols(v, type)
  idc <- up_id_col(type); shown <- unique(c(".excluded", idc, cols))
  col <- shown[info$col + 1]; req(!is.null(col), !col %in% c(".excluded", idc))
  id <- as.character(v[[idc]][info$row]); req(!is.na(id))
  e <- up_rv$edits
  e$cells <- Filter(function(c) !(identical(c$id, id) && identical(c$col, col)), e$cells)
  e$cells[[length(e$cells) + 1]] <- list(id = id, col = col, value = as.character(info$value))
  up_rv$edits <- e
})
observeEvent(input$up_fr_go, {
  req(nzchar(input$up_fr_from %||% ""))
  e <- up_rv$edits
  e$replace[[length(e$replace) + 1]] <- list(col = input$up_fr_col, from = input$up_fr_from, to = input$up_fr_to %||% "")
  up_rv$edits <- e
  n <- sum(!is.na(up_rv$staged[[input$up_fr_col]]) & as.character(up_rv$staged[[input$up_fr_col]]) == input$up_fr_from)
  showNotification(sprintf("Rule added: %s '%s' -> '%s' (%d matching row(s) in the staged file)", input$up_fr_col, input$up_fr_from, input$up_fr_to, n), type = "message")
})
observeEvent(input$up_sess_keep, {
  sess <- up_rv$sessions; req(sess)
  ok <- sess$sessionId[sess$status == "ok"]
  e <- up_rv$edits; e$exclude_sessions <- setdiff(ok, input$up_sess_keep); up_rv$edits <- e
}, ignoreNULL = FALSE, ignoreInit = TRUE)
up_selected_ids <- function() {
  rows <- input$up_review_table_rows_selected; v <- up_view(); if (!length(rows)) return(character(0))
  as.character(v[[up_id_col(up_rv$type)]][rows])
}
observeEvent(input$up_excl_rows, { ids <- up_selected_ids(); req(length(ids)); e <- up_rv$edits; e$exclude_rows <- union(e$exclude_rows, ids); up_rv$edits <- e })
observeEvent(input$up_incl_rows, { ids <- up_selected_ids(); req(length(ids)); e <- up_rv$edits; e$exclude_rows <- setdiff(e$exclude_rows, ids); up_rv$edits <- e })
observeEvent(input$up_reset_edits, {
  up_rv$edits <- up_edits_default()
  sess <- up_rv$sessions; if (!is.null(sess)) updateCheckboxGroupInput(session, "up_sess_keep", selected = sess$sessionId[sess$status == "ok"])
})
output$up_dl_csv <- downloadHandler(
  filename = function() sprintf("palace_%s_%s.csv", up_rv$type %||% "upload", format(Sys.Date())),
  content = function(file) { v <- up_view(); v <- v[!v$.excluded, setdiff(names(v), c(".excluded", "raw_json")), drop = FALSE]; utils::write.csv(v, file, row.names = FALSE, na = "") })
output$up_dl_parquet <- downloadHandler(
  filename = function() sprintf("palace_%s_%s.parquet", up_rv$type %||% "upload", format(Sys.Date())),
  content = function(file) { v <- up_view(); v <- v[!v$.excluded, setdiff(names(v), ".excluded"), drop = FALSE]; arrow::write_parquet(v, file) })

# ---- 3 Commit ------------------------------------------------------------------------------
output$up_commit_ui <- renderUI({
  req(is_admin())
  job <- up_rv$job; type <- up_rv$type
  if (is.null(job)) return(div(class = "lb-empty", "Nothing staged yet: run a fetch first."))
  st <- isolate(up_rv$status); m <- st$meta %||% up_job_meta(job)
  spec <- UPLOAD_TYPES[[type]]
  n_commit <- if (identical(type, "trumedia")) NA else { v <- tryCatch(up_view(), error = function(e) NULL); if (is.null(v)) 0L else sum(!v$.excluded) }
  tagList(
    div(style = "margin-top:12px;"),
    up_kv("Job" = m$id, "Type" = spec$label, "Window" = paste(m$from, "to", m$to), "Writes to" = spec$target,
          "Rows to commit" = if (is.na(n_commit)) "n/a (rebuild)" else format(n_commit, big.mark = ","),
          "Sessions" = if (is.na(n_commit)) "n/a" else length(setdiff(up_rv$sessions$sessionId[up_rv$sessions$status == "ok"], up_rv$edits$exclude_sessions))),
    if (identical(type, "games")) div(style = "font-size:12px; color:#92400E; background:#FEF6E4; border-radius:8px; padding:8px 10px; margin-bottom:8px;",
        "Game uploads score the pitches with Pitch Profiler and write pitchprofiler.pitches plus the per-game aggregates. Season tables and the Storage reference pools still come from the full-master batch pipeline (see README)."),
    checkboxInput("up_overwrite", "Replace rows that already exist in Supabase (same ids)", TRUE),
    checkboxInput("up_dry", "Dry run: validate, score and stage only, write nothing", FALSE),
    actionButton("up_commit_go", "Commit to Supabase", class = "btn-primary", disabled = if (!is.na(n_commit) && n_commit == 0) NA else NULL),
    uiOutput("up_commit_status"))
})
observeEvent(input$up_commit_go, {
  req(is_admin(), up_rv$job)
  if (up_busy()) { showNotification("A job is already running; wait for it to finish.", type = "warning"); return() }
  spec <- UPLOAD_TYPES[[up_rv$type]]
  showModal(modalDialog(title = if (isTRUE(input$up_dry)) "Dry run" else "Commit to Supabase?", easyClose = TRUE,
    p(sprintf("This %s %s.", if (isTRUE(input$up_dry)) "dry run simulates writing" else "writes", spec$target)),
    if (isTRUE(input$up_overwrite) && !isTRUE(input$up_dry)) p(style = "color:#92400E;", "Rows with the same ids are replaced."),
    footer = tagList(modalButton("Cancel"), actionButton("up_commit_confirm", if (isTRUE(input$up_dry)) "Run dry run" else "Commit", class = "btn-primary"))))
})
observeEvent(input$up_commit_confirm, {
  removeModal(); req(is_admin(), up_rv$job)
  job <- up_rv$job
  up_edits_write(job, up_rv$edits)
  m <- up_job_meta(job); opts <- m$opts %||% list()
  opts$overwrite <- isTRUE(input$up_overwrite); opts$dry_run <- isTRUE(input$up_dry)
  up_job_update(job, opts = opts, user = current_user()$username %||% m$user)
  up_rv$bg <- up_bg_start("commit", "up_commit", list(job_dir = job), job_dir = job)
  up_rv$hist_tick <- up_rv$hist_tick + 1L
})
output$up_commit_status <- renderUI({
  st <- up_rv$status; req(st)
  m <- st$meta
  if (!m$status %in% c("committing", "committed", "dry_run_done", "commit_failed")) return(NULL)
  res <- m$result
  tagList(
    up_progress_ui(st, "Committing"),
    if (m$status %in% c("committed", "dry_run_done"))
      div(style = "background:#E7F6EC; border:1px solid #BBE5C8; border-radius:8px; padding:8px 10px; margin-top:6px; font-size:12.5px;",
          tags$b(if (m$status == "committed") "✓ Committed. " else "✓ Dry run finished. "),
          sprintf("%s row(s), %s session(s) → %s.", format(res$n_rows %||% NA, big.mark = ","), res$n_sessions %||% NA, res$target %||% ""),
          if (nzchar(res$notes %||% "")) tags$div(style = "margin-top:4px; color:#374151;", res$notes),
          if (m$status == "committed" && m$type %in% c("bullpens", "bp")) tags$div(style = "margin-top:4px;", "The Bullpens tab picks the new rows up within a minute.")))
})

# ---- Completeness --------------------------------------------------------------------------
output$up_check_ui <- renderUI({
  req(is_admin())
  tagList(
    div(style = "margin-top:12px;"),
    fluidRow(
      column(3, selectInput("up_chk_type", "Type", choices = c("Bullpens" = "bullpens", "Batting practice" = "bp", "TrackMan games" = "games"), selected = "bullpens")),
      column(4, dateRangeInput("up_chk_dates", "Dates", start = Sys.Date() - 60, end = Sys.Date())),
      column(3, conditionalPanel("input.up_chk_type == 'games'", selectInput("up_chk_scope", "Scope", UPLOAD_SCOPES, selected = "ccu"))),
      column(2, actionButton("up_chk_go", "Check", class = "btn-primary", style = "margin-top:25px;"))),
    div(style = "font-size:12px; color:#666;", "Compares the sessions TrackMan lists for the window against what Supabase holds, day by day."),
    uiOutput("up_chk_result"))
})
observeEvent(input$up_chk_go, {
  req(is_admin()); d <- input$up_chk_dates; req(length(d) == 2)
  up_rv$comp <- NULL
  up_rv$comp_bg <- up_bg_start("check", "up_completeness", list(type = input$up_chk_type, from = d[1], to = d[2], scope = input$up_chk_scope %||% "all"))
})
observe({
  bg <- up_rv$comp_bg; req(bg)
  if (up_bg_alive(bg)) { invalidateLater(1500); return() }
  up_rv$comp <- up_bg_result(bg); up_rv$comp_bg <- NULL
})
output$up_chk_result <- renderUI({
  if (!is.null(up_rv$comp_bg)) return(div(style = "margin-top:10px; color:#0E6E70; font-weight:600;", "Checking TrackMan and Supabase…"))
  r <- up_rv$comp; req(r)
  if (up_is_error(r)) return(div(style = "background:#FDECEA; border-radius:8px; padding:8px 10px; margin-top:8px;", tags$b("Check failed: "), r$error, tags$br(), up_hint(r$error)))
  tagList(
    div(style = "margin-top:10px; font-size:12.5px;",
        sprintf("%s: TrackMan lists %d session(s) from %s to %s; %d missing from Supabase%s.", UPLOAD_TYPES[[r$type]]$label, r$n_trackman, r$from, r$to, r$n_missing,
                if (r$n_extra > 0) sprintf("; Supabase holds %d session(s) in the window TrackMan did not list", r$n_extra) else "")),
    fluidRow(
      column(5, h5("By date"), DT::renderDT(DT::datatable(r$by_date, rownames = FALSE, selection = "none", class = "compact stripe", options = list(dom = "tp", pageLength = 20)) |>
                 DT::formatStyle("Missing", backgroundColor = DT::styleInterval(0, c("#FFFFFF", "#FDECEA"))), server = FALSE)),
      column(7, h5("Sessions"), DT::renderDT(DT::datatable(r$sessions, rownames = FALSE, selection = "none", class = "compact stripe", options = list(dom = "ftp", pageLength = 20, scrollX = TRUE)) |>
                 DT::formatStyle("In Supabase", backgroundColor = DT::styleEqual(c("yes", "MISSING"), c("#E7F6EC", "#FDECEA"))), server = FALSE))))
})

# ---- History ---------------------------------------------------------------------------------
output$up_history_ui <- renderUI({
  req(is_admin())
  tagList(div(style = "margin-top:12px; display:flex; gap:8px; align-items:center;",
              actionButton("up_hist_refresh", "Refresh"),
              span(style = "font-size:12px; color:#666;", "Every commit, from Storage uploads/log.json. Raw and staged files are archived under uploads/<job id>/.")),
          DT::DTOutput("up_hist_table"), uiOutput("up_hist_notes"))
})
observeEvent(input$up_hist_refresh, up_rv$hist_tick <- up_rv$hist_tick + 1L)
up_hist_entries <- reactive({ up_rv$hist_tick; tryCatch(up_log_read(force = TRUE), error = function(e) list()) })
output$up_hist_table <- DT::renderDT({
  d <- up_log_table(up_hist_entries())
  DT::datatable(d[, setdiff(names(d), "id"), drop = FALSE], rownames = FALSE, selection = "single", class = "compact stripe", options = list(pageLength = 15, scrollX = TRUE))
}, server = FALSE)
output$up_hist_notes <- renderUI({
  i <- input$up_hist_table_rows_selected; req(i)
  d <- up_log_table(up_hist_entries()); e <- Filter(function(x) identical(x$id, d$id[i]), up_hist_entries())
  req(length(e)); e <- e[[1]]
  div(style = "font-size:12.5px; margin-top:8px; background:#F4F7F8; border-radius:8px; padding:8px 10px;",
      tags$b(e$id), tags$br(), "Target: ", e$target %||% "", tags$br(), "Notes: ", e$notes %||% "")
})

# ---- Setup check ---------------------------------------------------------------------------
output$up_setup_ui <- renderUI({
  req(is_admin())
  tagList(div(style = "margin-top:12px;"),
          actionButton("up_pre_go", "Run setup check", class = "btn-primary"),
          span(style = "font-size:12px; color:#666; margin-left:8px;", "Tests every credential and service the uploads need. Nothing is written."),
          uiOutput("up_pre_result"))
})
observeEvent(input$up_pre_go, { req(is_admin()); up_rv$pre <- NULL; up_rv$pre_bg <- up_bg_start("preflight", "up_preflight", list(with_trumedia = TRUE)) })
observe({
  bg <- up_rv$pre_bg; req(bg)
  if (up_bg_alive(bg)) { invalidateLater(1500); return() }
  up_rv$pre <- up_bg_result(bg); up_rv$pre_bg <- NULL
})
output$up_pre_result <- renderUI({
  if (!is.null(up_rv$pre_bg)) return(div(style = "margin-top:10px; color:#0E6E70; font-weight:600;", "Checking… (TrackMan, TruMedia, Supabase, Python)"))
  r <- up_rv$pre; req(r)
  if (up_is_error(r)) return(div(style = "margin-top:8px;", tags$b("Setup check failed: "), r$error))
  tags$table(class = "table table-condensed", style = "font-size:12.5px; margin-top:10px;",
    tags$thead(tags$tr(tags$th(""), tags$th("Service"), tags$th("Detail"), tags$th("What to do"))),
    tags$tbody(lapply(r, function(c) tags$tr(
      tags$td(style = "font-size:15px;", if (isTRUE(c$ok)) "✅" else if (is.na(c$ok)) "⚪" else "❌"),
      tags$td(tags$b(c$name)), tags$td(c$detail), tags$td(style = "color:#7F1D1D;", c$hint %||% "")))))
})
