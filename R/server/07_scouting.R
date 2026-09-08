  # ============================================================
  # ===== SCOUTING (NCAA pitchers): notes + PDF reports ========
  # ============================================================

  # Full (unfiltered) data for the selected NCAA pitcher, plate
  # locations in feet — the units the scouting-report engine expects.
  scout_data <- reactive({
    gp <- input$global_pitcher
    req(gp, nzchar(gp), !is_coastal_ctx(gp, isolate(input$season_type)), is_ncaa_pitcher(gp))
    ncaa_full_ready()   # re-fires when the fully scored frame lands
    df <- ncaa_best_data(gp)
    ncaa_schedule_full_load(gp)
    validate(need(is.data.frame(df) && nrow(df) > 0,
                  "No TrackMan data found for this pitcher."))
    scout_to_feet(df)
  })

  output$scout_pitcher_label <- renderText({
    gp <- input$global_pitcher
    if (is.null(gp) || !nzchar(gp)) return("Select an NCAA pitcher to scout.")
    dir_row <- ncaa_directory[ncaa_directory$display == gp, , drop = FALSE]
    tm <- if (nrow(dir_row) > 0) dir_row$teams[1] else ""
    paste0(gp, if (nzchar(tm)) paste0("  \u2014  ", tm) else "",
           "  \u2022  Notes save across sessions \u2022 PDF matches the Advance report layout.")
  })

  output$scout_arm_angle <- renderPlot({
    create_pitcher_arm_angle_plot(scout_data(), input$global_pitcher)
  }, bg = "transparent")

  output$scout_movement <- renderPlot({
    create_pitcher_movement_plot(scout_data(), input$global_pitcher, exp_movement_grid)
  }, bg = "transparent")

  # ---- Notes storage (persists to /data across restarts) ----
  scout_notes_storage <- reactiveVal({
    p <- scout_notes_path()
    if (file.exists(p)) tryCatch(readRDS(p), error = function(e) list()) else list()
  })

  scout_pitch_types <- reactive({
    df <- scout_data()
    df %>%
      filter(!is.na(TaggedPitchType), TaggedPitchType != "Other") %>%
      count(TaggedPitchType) %>%
      filter(n >= 10) %>%
      arrange(desc(n)) %>%
      pull(TaggedPitchType) %>%
      as.character()
  })

  output$scout_pitch_notes_ui <- renderUI({
    pts <- scout_pitch_types()
    if (length(pts) == 0) {
      return(p("No pitch types with 10+ pitches yet.",
               style = "color:gray; font-style:italic;"))
    }
    saved <- scout_notes_storage()[[input$global_pitcher]]
    fluidRow(
      lapply(pts, function(pt) {
        input_id <- paste0("pdf_note_", gsub("[^a-zA-Z]", "", tolower(pt)))
        column(4, textAreaInput(input_id, paste0(pt, " Notes:"),
                                value = saved$pitch_notes[[pt]] %||% "",
                                rows = 2, width = "100%"))
      })
    )
  })

  get_dynamic_pitch_notes <- function() {
    notes <- list()
    for (pt in scout_pitch_types()) {
      input_id <- paste0("pdf_note_", gsub("[^a-zA-Z]", "", tolower(pt)))
      if (!is.null(input[[input_id]])) notes[[pt]] <- input[[input_id]]
    }
    notes
  }

  observeEvent(input$save_pitcher_notes, {
    gp <- input$global_pitcher
    if (is.null(gp) || !nzchar(gp)) {
      showNotification("No pitcher selected", type = "error")
      return()
    }
    storage <- scout_notes_storage()
    storage[[gp]] <- list(
      delivery_notes = input$pdf_delivery_notes %||% "",
      matchup_notes  = input$pdf_matchup_notes %||% "",
      stamina_notes  = input$pdf_stamina_notes %||% "",
      vs_lhh_notes   = input$pdf_vs_lhh_notes %||% "",
      vs_rhh_notes   = input$pdf_vs_rhh_notes %||% "",
      pitch_notes    = get_dynamic_pitch_notes()
    )
    scout_notes_storage(storage)
    tryCatch(saveRDS(storage, scout_notes_path()),
             error = function(e) cat("WARNING: notes not persisted -", conditionMessage(e), "\n"))
    showNotification(paste("Notes saved for", gp), type = "message", duration = 2)
  })

  # Reload saved notes when switching pitchers
  observeEvent(input$global_pitcher, {
    gp <- input$global_pitcher
    if (is.null(gp) || !nzchar(gp) || !is_ncaa_pitcher(gp)) return()
    saved <- scout_notes_storage()[[gp]]
    updateTextAreaInput(session, "pdf_delivery_notes", value = saved$delivery_notes %||% "")
    updateTextAreaInput(session, "pdf_matchup_notes",  value = saved$matchup_notes %||% "")
    updateTextAreaInput(session, "pdf_stamina_notes",  value = saved$stamina_notes %||% "")
    updateTextAreaInput(session, "pdf_vs_lhh_notes",   value = saved$vs_lhh_notes %||% "")
    updateTextAreaInput(session, "pdf_vs_rhh_notes",   value = saved$vs_rhh_notes %||% "")
  })

  # ---- Shared PDF builder for download + preview ----
  build_scouting_pdf <- function(file) {
    gp <- input$global_pitcher
    create_comprehensive_pitcher_pdf(
      data = scout_data(),
      pitcher_name = gp,
      output_file = file,
      delivery_notes = input$pdf_delivery_notes %||% "",
      count_usage_notes = "",
      pitch_notes = get_dynamic_pitch_notes(),
      overall_notes = input$pdf_stamina_notes %||% "",
      vs_lhh_notes = input$pdf_vs_lhh_notes %||% "",
      vs_rhh_notes = input$pdf_vs_rhh_notes %||% "",
      series_title = "",
      pitcher_role = "",
      matchup_matrix = NULL
    )
  }

  output$download_scouting_pdf <- downloadHandler(
    filename = function() {
      gp <- input$global_pitcher
      req(gp)
      paste0(gsub(" ", "_", gp), "_Scouting_Report_",
             format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      req(input$global_pitcher)
      withProgress(message = "Generating scouting report...", value = 0, {
        incProgress(0.2, detail = "Pulling data...")
        tryCatch({
          build_scouting_pdf(file)
          incProgress(0.8, detail = "Done")
        }, error = function(e) {
          showNotification(paste("PDF error:", conditionMessage(e)), type = "error")
          pdf(file, width = 8.5, height = 11)
          grid::grid.newpage()
          grid::grid.text(paste("Report failed:", conditionMessage(e)),
                          gp = grid::gpar(fontsize = 12))
          dev.off()
        })
      })
    },
    contentType = "application/pdf"
  )

  # ---- In-app PDF preview ----
  scout_preview_dir <- file.path(tempdir(), paste0("scout_prev_", session$token))
  dir.create(scout_preview_dir, recursive = TRUE, showWarnings = FALSE)
  addResourcePath(paste0("scoutprev_", session$token), scout_preview_dir)
  scout_preview_src <- reactiveVal(NULL)

  observeEvent(input$preview_scouting_pdf, {
    gp <- input$global_pitcher
    req(gp, nzchar(gp))
    withProgress(message = "Building preview...", value = 0, {
      incProgress(0.2)
      fname <- paste0(gsub("[^A-Za-z0-9]", "_", gp), ".pdf")
      fpath <- file.path(scout_preview_dir, fname)
      ok <- tryCatch({ build_scouting_pdf(fpath); TRUE },
                     error = function(e) {
                       showNotification(paste("Preview error:", conditionMessage(e)),
                                        type = "error")
                       FALSE
                     })
      incProgress(0.8)
      if (ok) {
        scout_preview_src(paste0(
          "scoutprev_", session$token, "/", fname,
          "?ts=", as.integer(Sys.time())
        ))
      }
    })
  })

  # Clear stale preview when the pitcher changes
  observeEvent(input$global_pitcher, scout_preview_src(NULL))

  output$scout_pdf_preview <- renderUI({
    src <- scout_preview_src()
    if (is.null(src)) {
      return(div(style = "color:#6B7280; font-size:13px; margin:6px 0 20px;",
                 "Click \u201cPreview Report\u201d to render the full PDF in-app."))
    }
    tags$iframe(src = src, style = "width:100%; height:1100px; border:1px solid #ddd; border-radius:8px;")
  })
