
  # Brief disconnects (laptop lid, network blip, HF proxy hiccup) resume the
  # session where supported instead of graying out.
  session$allowReconnect(TRUE)

  
  logged_in <- reactiveVal(FALSE)

  # ===== Shared global count / result / team filters =====
  # Applied on top of date/side/pitchtype/velo by every chart and table.
  apply_count_filter <- function(df, sel) {
    if (is.null(sel) || length(sel) == 0 || !all(c("Balls","Strikes") %in% names(df))) return(df)
    b <- df$Balls; k <- df$Strikes
    cnt <- paste0(b, "-", k)
    keep <- rep(FALSE, nrow(df))
    for (s in sel) {
      keep <- keep | switch(s,
        "grp_hitters"     = cnt %in% c("2-0","3-0","2-1","3-1","1-0"),
        "grp_pitchers"    = cnt %in% c("0-1","0-2","1-2","2-2"),
        "grp_even"        = cnt %in% c("0-0","1-1","3-2"),
        "grp_0k"          = k == 0,
        "grp_1k"          = k == 1,
        "grp_2k"          = k == 2,
        "grp_0b"          = b == 0,
        "grp_1b"          = b == 1,
        "grp_2b"          = b == 2,
        "grp_3b"          = b == 3,
        "grp_2k_notfull"  = k == 2 & b < 3,
        "grp_lt2k"        = k < 2,
        # exact counts like "1-2"
        cnt == s
      )
    }
    df[keep, , drop = FALSE]
  }

  apply_result_filter <- function(df, sel) {
    if (is.null(sel) || length(sel) == 0 || !"PitchCall" %in% names(df)) return(df)
    pc <- df$PitchCall
    keep <- rep(FALSE, nrow(df))
    foul_calls   <- c("FoulBall","FoulBallNotFieldable","FoulBallFieldable")
    strike_calls <- c("StrikeCalled","StrikeSwinging", foul_calls, "InPlay")
    swing_calls  <- c("StrikeSwinging","InPlay", foul_calls)
    for (s in sel) {
      keep <- keep | switch(s,
        "res_strike"  = pc %in% strike_calls,
        "res_whiff"   = pc == "StrikeSwinging",
        "res_called"  = pc == "StrikeCalled",
        "res_foul"    = pc %in% foul_calls,
        "res_ball"    = pc %in% c("BallCalled","BallIntentional","BallinDirt"),
        "res_dirt"    = pc == "BallinDirt",
        "res_hbp"     = pc == "HitByPitch",
        "res_inplay"  = pc == "InPlay",
        "res_swing"   = pc %in% swing_calls,
        "res_take"    = !(pc %in% swing_calls),
        rep(FALSE, nrow(df))
      )
    }
    df[keep, , drop = FALSE]
  }

  apply_global_filters <- function(df) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)
    df <- apply_count_filter(df, input$g_count)
    df <- apply_result_filter(df, input$g_result)
    if (!is.null(input$game_filter_spring26) && length(input$game_filter_spring26) > 0 &&
        "GameID" %in% names(df)) {
      df <- df[df$GameID %in% input$game_filter_spring26, , drop = FALSE]
    }
    if (!is.null(input$g_vs_team) && nzchar(trimws(input$g_vs_team)) &&
        "BatterTeam" %in% names(df)) {
      patt <- trimws(input$g_vs_team)
      df <- df[grepl(patt, df$BatterTeam, ignore.case = TRUE) |
               grepl(patt, prettify_team(df$BatterTeam), ignore.case = TRUE), , drop = FALSE]
    }
    df
  }

  observeEvent(input$reset_filters, {
    updatePickerInput(session, "g_count", selected = character(0))
    updatePickerInput(session, "g_result", selected = character(0))
    updatePickerInput(session, "game_filter_spring26", selected = character(0))
    updateTextInput(session, "g_vs_team", value = "")
  })

  output$page <- renderUI({
    if (logged_in()) {
      app_ui
    } else {
      login_ui
    }
  })
  
  observeEvent(input$login, {
    if (input$password == PASSWORD) {
      logged_in(TRUE)
      output$wrong_pass <- renderText("")
    } else {
      output$wrong_pass <- renderText("Incorrect password, please try again.")
    }
  })
  # Keep the startup loading overlay up until the data is fully filtered down
  # to the selected pitcher and ready to display (covers the ~30s where all
  # team pitchers are still combined in the charts).
  loading_done <- reactiveVal(FALSE)
  observeEvent(filtered_data(), {
    if (isTRUE(loading_done())) return()
    gp <- input$global_pitcher
    fd <- filtered_data()
    if (!is.null(gp) && nzchar(gp) &&
        !is.null(fd) && is.data.frame(fd) && nrow(fd) > 0 &&
        all(fd$Pitcher == gp, na.rm = TRUE)) {
      loading_done(TRUE)
      session$sendCustomMessage("hideLoading", TRUE)
    }
  }, ignoreNULL = TRUE)
  
  # ===== UNIFIED SEARCH: hitter picks route to the hitter Overview =====
  # The header search now lists pitchers AND hitters. Hitter values carry
  # the HB:: prefix; picking one opens that hitter's page, then the search
  # is restored to the last valid pitcher so every pitcher-side consumer
  # of input$global_pitcher keeps a real arm selected. Runs at high
  # priority so it fires before the fan-out / filter-sync observers.
  .tp_last_pitcher <- reactiveVal(NULL)

  # Which flavour of the Players page is live. Everything that used to key
  # off input$main_tabs == "Hitters" now keys off this instead.
  player_mode <- reactiveVal("pitcher")

  observeEvent(input$global_pitcher, {
    gp <- input$global_pitcher
    if (is.null(gp) || !nzchar(gp)) return()
    if (is_hitter_pick(gp)) {
      # The hitter STAYS in the search box now — the Players page reads it
      # and shows hitter pills. Pitcher-side consumers are parked behind
      # hidden tabs (Shiny suspends them), and the standalone pitcher tools
      # get the last real arm restored by the observer just below.
      player_mode("hitter")
      open_hitter_page(sub(paste0("^", HB_PREFIX), "", gp))
    } else {
      player_mode("pitcher")
      .tp_last_pitcher(gp)
    }
  }, priority = 1000)

  # Standalone pitcher tools (Advance, Reports, Coastal leaderboard, spray)
  # still read input$global_pitcher directly. If the user wanders onto one
  # of them while a hitter is loaded, hand those pages back the last real
  # arm so they don't render empty.
  TP_PITCHER_ONLY_TABS <- c("Advance", "ReportsTab", "Pitching Leaderboard",
                            "vCCU Spray Charts", "Count Usage Report")
  observeEvent(input$main_tabs, {
    if (!isTRUE(input$main_tabs %in% TP_PITCHER_ONLY_TABS)) return()
    gp <- input$global_pitcher
    if (!is_hitter_pick(gp)) return()
    prev <- .tp_last_pitcher()
    if (!is.null(prev) && nzchar(prev)) {
      player_mode("pitcher")
      updateSelectizeInput(session, "global_pitcher", selected = prev)
    }
  }, priority = 900)

  # ===== GLOBAL PLAYER SELECT: fan out to every per-tab pitcher input =====
  # The global select is the single source of truth. Whenever it changes,
  # push the value to all the legacy per-tab inputs so existing server logic
  # (which still reads input$global_pitcher, input$selectedPitcher, etc.) keeps working.
  observeEvent(input$global_pitcher, {
    gp <- input$global_pitcher
    req(gp)
    if (is_hitter_pick(gp)) return()   # hitter picks route elsewhere
    # single-select inputs
    updateSelectInput(session, "pitcher",            selected = gp)
    updateSelectInput(session, "selectedPitcher",    selected = gp)
    updateSelectInput(session, "pitcher_spray",      selected = gp)
    # single-select inputs whose UI was folded into the global select are
    # driven directly off input$global_pitcher in their server logic.
    # multi-select pickers: set to the single global pitcher
    updatePickerInput(session, "pitcher_velo",       selected = gp)
    updatePickerInput(session, "pitchertype_metrics",selected = gp)
    updatePickerInput(session, "pitcher_countreport",selected = gp)
    updatePickerInput(session, "pitcher_custom",     selected = gp)
  }, ignoreInit = FALSE)

  # ===== NCAA two-phase load =====
  # First paint renders off the parquet-only fast frame (seconds); the
  # full TruMedia + proModel pipeline is deferred to AFTER that flush
  # reaches the browser, then ncaa_full_ready is bumped so every reactive
  # that took a dependency on it re-renders with the upgraded frame.
  ncaa_full_ready <- reactiveVal(0L)
  .ncaa_upgrade_pending <- new.env(parent = emptyenv())

  # Best frame available RIGHT NOW: fully scored if cached, else fast.
  ncaa_best_data <- function(gp) {
    key <- gsub("[^A-Za-z0-9]", "_", gp)
    full <- ncaa_pitcher_cache[[key]]
    if (!is.null(full)) return(full)
    load_ncaa_pitcher_fast(gp)
  }

  ncaa_schedule_full_load <- function(gp) {
    key <- gsub("[^A-Za-z0-9]", "_", gp)
    if (!is.null(ncaa_pitcher_cache[[key]])) return(invisible())
    if (isTRUE(.ncaa_upgrade_pending[[key]])) return(invisible())
    .ncaa_upgrade_pending[[key]] <- TRUE
    session$onFlushed(function() {
      tryCatch({
        df <- load_ncaa_pitcher_data(gp)
        # widen the date filter to any games the API has that the
        # parquet frame didn't (the newest outing, typically) — but only
        # if this arm is still the selected one
        if (is.data.frame(df) && nrow(df) > 0 && "Date" %in% names(df) &&
            identical(isolate(input$global_pitcher), gp)) {
          rng <- suppressWarnings(range(df$Date, na.rm = TRUE))
          if (all(is.finite(rng)))
            updateDateRangeInput(session, "g_dateRange",
                                 start = rng[1], end = rng[2])
        }
        ncaa_full_ready(isolate(ncaa_full_ready()) + 1L)
      }, error = function(e) {
        cat("[NCAA upgrade] full load failed for", gp, ":",
            conditionMessage(e), "\n")
      }, finally = rm(list = key, envir = .ncaa_upgrade_pending))
    }, once = TRUE)
    invisible()
  }

  # ===== NCAA filter-domain sync (transition-aware) =====
  # The slide-out filters default to Coastal ranges. Only touch them when
  # crossing the Coastal/NCAA boundary or switching between NCAA arms —
  # coastal->coastal switches leave filters alone so a pitcher change
  # costs ONE render cascade instead of three.
  .filter_sync_state <- reactiveValues(mode = NULL, pitcher = NULL)
  observeEvent(input$global_pitcher, {
    gp <- input$global_pitcher
    req(gp, nzchar(gp))
    if (is_hitter_pick(gp)) return()   # hitter picks route elsewhere
    mode <- if (is_coastal_ctx(gp, input$season_type) || !is_ncaa_pitcher(gp)) "coastal" else "ncaa"
    prev_mode    <- .filter_sync_state$mode
    prev_pitcher <- .filter_sync_state$pitcher
    .filter_sync_state$mode    <- mode
    .filter_sync_state$pitcher <- gp

    base_pts <- c("Fastball","Sinker","Cutter","Curveball","Slider",
                  "ChangeUp","Splitter","Sweeper")

    if (identical(mode, "coastal")) {
      # Reset only when RETURNING from NCAA mode
      if (identical(prev_mode, "ncaa")) {
        df <- tryCatch(coastal_selected_data(), error = function(e) NULL)
        if (!is.null(df) && is.data.frame(df) && "Date" %in% names(df)) {
          rng <- suppressWarnings(range(df$Date, na.rm = TRUE))
          if (all(is.finite(rng))) {
            updateDateRangeInput(session, "g_dateRange", start = rng[1], end = rng[2])
          }
        }
        updatePickerInput(session, "g_pitchtype",
                          choices = base_pts, selected = base_pts)
      }
    } else {
      # NCAA: widen to this arm's data (skip if same arm re-selected).
      # Fast frame only here — the full TruMedia/proModel load is deferred
      # past this flush so charts paint first; the upgrade callback
      # re-widens the date range if the API has newer games.
      if (!identical(prev_mode, "ncaa") || !identical(prev_pitcher, gp)) {
        df <- withProgress(message = "Loading NCAA data...", value = 0.4,
                           ncaa_best_data(gp))
        ncaa_schedule_full_load(gp)
        if (is.data.frame(df) && nrow(df) > 0) {
          if ("Date" %in% names(df)) {
            rng <- suppressWarnings(range(df$Date, na.rm = TRUE))
            if (all(is.finite(rng))) {
              updateDateRangeInput(session, "g_dateRange", start = rng[1], end = rng[2])
            }
          }
          pts <- sort(unique(as.character(df$TaggedPitchType)))
          pts <- pts[!is.na(pts) & nzchar(pts) & pts != "Other"]
          all_pts <- union(base_pts, pts)
          updatePickerInput(session, "g_pitchtype",
                            choices = all_pts, selected = all_pts)
        }
      }
    }
  }, ignoreInit = TRUE)

  # ===== PLAYER MODE: which pills the one Players tab shows =====
  # Pitcher picked -> pitcher pills. Coastal arm gets Player Plans +
  #   Bullpens; an NCAA arm gets Scouting instead.
  # Hitter picked  -> hitter pills. The three defense pills stay owned by
  #   the position observer further down, which only reveals the ones the
  #   hitter actually plays.
  TP_PITCHER_PILLS <- c("Overview", "Bullpens", "Scouting")
  TP_HITTER_PILLS  <- c("hp_overview", "hit_inf", "hit_of", "hit_c")

  observe({
    req(logged_in())
    mode <- player_mode()
    gp   <- input$global_pitcher
    # isolate: this observer reacts to WHO is loaded, not to the user
    # clicking between pills. Reading it reactively would re-issue every
    # show/hideTab on each pill click.
    cur  <- isolate(input$player_subtabs)

    if (identical(mode, "hitter")) {
      for (v in TP_PITCHER_PILLS) hideTab("player_subtabs", v)
      showTab("player_subtabs", "hp_overview")
      if (is.null(cur) || isTRUE(cur %in% TP_PITCHER_PILLS)) {
        updateTabsetPanel(session, "player_subtabs", selected = "hp_overview")
      }
      return()
    }

    for (v in TP_HITTER_PILLS) hideTab("player_subtabs", v)
    showTab("player_subtabs", "Overview")

    is_ccu <- is.null(gp) || !nzchar(gp) || is_hitter_pick(gp) ||
              is_coastal_ctx(gp, input$season_type) || !is_ncaa_pitcher(gp)
    if (is_ccu) {
      hideTab("player_subtabs", "Scouting")
      showTab("player_subtabs", "Bullpens")
      if (isTRUE(cur == "Scouting")) cur <- "Overview"
    } else {
      showTab("player_subtabs", "Scouting")
      hideTab("player_subtabs", "Bullpens")
      if (isTRUE(cur == "Bullpens")) cur <- "Overview"
    }
    if (is.null(cur) || isTRUE(cur %in% TP_HITTER_PILLS)) cur <- "Overview"
    if (!identical(cur, isolate(input$player_subtabs))) {
      updateTabsetPanel(session, "player_subtabs", selected = cur)
    }
  })

  # Context strip at the top of the Players page: who is loaded and which
  # side of the ball you're looking at, so the pill row is never ambiguous.
  output$player_mode_head <- renderUI({
    hitter <- identical(player_mode(), "hitter")
    nm <- if (hitter) {
      input$hp_batter %||% ""
    } else {
      gp <- input$global_pitcher %||% ""
      if (is_hitter_pick(gp)) "" else gp
    }
    if (!nzchar(nm)) {
      return(div(style = "text-align:center; margin:16px 0 4px;",
        h3("Players", class = "brand-teal", style = "margin-bottom:2px;"),
        div(style = "color:#6B7280; font-size:13px;",
            "Search any pitcher or hitter up top \u2014 the tabs below follow ",
            "whoever you pick.")))
    }
    badge <- if (hitter) "HITTER" else "PITCHER"
    bg    <- if (hitter) "#B08D57" else "#006F71"
    div(style = "text-align:center; margin:16px 0 4px;",
      div(style = "display:flex; align-items:center; justify-content:center; gap:10px;",
        tags$span(badge, style = paste0(
          "background:", bg, "; color:#fff; font-size:10.5px; font-weight:800;",
          "letter-spacing:.09em; padding:3px 9px; border-radius:20px;")),
        h3(nm, class = "brand-teal", style = "margin:0;")),
      div(style = "color:#6B7280; font-size:13px; margin-top:2px;",
          if (hitter)
            "Hitter profile \u2014 overview, viz, trends and defense."
          else
            "Pitcher profile \u2014 overview, viz, trends and reports."))
  })


  # ===== Top nav routing =====
  observeEvent(input$go_roster, {
    updateTabsetPanel(session, "main_tabs", selected = "Roster")
  })
  # Both header links land on the same Players page — they just declare
  # which side of it you want.
  observeEvent(input$go_pitchers, {
    gp <- input$global_pitcher
    if (is_hitter_pick(gp)) {
      prev <- .tp_last_pitcher()
      if (!is.null(prev) && nzchar(prev))
        updateSelectizeInput(session, "global_pitcher", selected = prev)
    }
    player_mode("pitcher")
    updateTabsetPanel(session, "main_tabs", selected = "Players")
    updateTabsetPanel(session, "player_subtabs", selected = "Overview")
  })
  observeEvent(input$go_hitters, {
    player_mode("hitter")
    updateTabsetPanel(session, "main_tabs", selected = "Players")
    updateTabsetPanel(session, "player_subtabs", selected = "hp_overview")
  })
  observeEvent(input$go_leaderboard, {
    updateTabsetPanel(session, "main_tabs", selected = "Leaderboards")
    updateTabsetPanel(session, "lb_subtabs", selected = "Pitching Leaderboard")
  })
  # ----- Leaderboard dropdown deep-links -----
  observeEvent(input$lb_go_coastal, {
    updateTabsetPanel(session, "main_tabs", selected = "Leaderboards")
    updateTabsetPanel(session, "lb_subtabs", selected = "Pitching Leaderboard")
  })
  observeEvent(input$lb_go_pitchers, {
    updateTabsetPanel(session, "main_tabs", selected = "Leaderboards")
    updateTabsetPanel(session, "lb_subtabs", selected = "League Pitchers")
  })
  observeEvent(input$lb_go_pitches, {
    updateTabsetPanel(session, "main_tabs", selected = "Leaderboards")
    updateTabsetPanel(session, "lb_subtabs", selected = "League Pitches")
  })

  # League boards: same module, independent filter state
  lb_register_board(input, output, session, "pitchers")
  lb_register_board(input, output, session, "pitches")
  observeEvent(input$go_advance, {
    updateTabsetPanel(session, "main_tabs", selected = "Advance")
  })
  observeEvent(input$go_reports, {
    updateTabsetPanel(session, "main_tabs", selected = "ReportsTab")
  })

  # Tabs that are standalone pages (no pitcher-detail tabs shown).
  # Focus mode collapsed the old pitcher-detail row when you opened a
  # standalone page. There is no detail row in the main bar any more, so
  # nothing needs collapsing.
  TP_FOCUS_TAB_VALUES <- character(0)

  # ----- Header dropdown deep-links: Advance -----
  adv_jump <- function(sub) {
    updateTabsetPanel(session, "main_tabs", selected = "Advance")
    updateTabsetPanel(session, "advance_subtabs", selected = sub)
    session$sendCustomMessage("tpFocusMode", TRUE)
  }
  observeEvent(input$adv_go_teamreports, adv_jump("adv_teamreports"))
  observeEvent(input$adv_go_matrix,      adv_jump("adv_matrix"))
  observeEvent(input$adv_go_spray,       adv_jump("adv_spray"))
  observeEvent(input$adv_go_ptproj,      adv_jump("adv_ptproj"))
  observeEvent(input$adv_go_outcomes,    adv_jump("adv_outcomes"))
  observeEvent(input$adv_go_workbook,    adv_jump("adv_teamreports"))

  # ----- Header dropdown deep-links: Reports -----
  rep_jump <- function(sub) {
    updateTabsetPanel(session, "main_tabs", selected = "ReportsTab")
    updateTabsetPanel(session, "reports_subtabs", selected = sub)
    session$sendCustomMessage("tpFocusMode", TRUE)
  }
  observeEvent(input$rep_go_count, rep_jump("Count Usage Report"))
  observeEvent(input$rep_go_spray, rep_jump("vCCU Spray Charts"))

  # Keep the collapsed-tab (focus) mode in sync whenever the main tab changes.
  observeEvent(input$main_tabs, {
    session$sendCustomMessage(
      "tpFocusMode",
      isTRUE(input$main_tabs %in% TP_FOCUS_TAB_VALUES)
    )
  }, ignoreInit = FALSE)

  # Toggle the Roster-only layout (hides player select + Filters button).
  observeEvent(input$main_tabs, {
    session$sendCustomMessage("setOnRoster", isTRUE(input$main_tabs == "Roster"))
  }, ignoreInit = FALSE)

  # ===== Filter sidebar open/close =====
  observeEvent(input$go_filters, {
    session$sendCustomMessage("toggleFilters", TRUE)
  })
  observeEvent(input$close_filters, {
    session$sendCustomMessage("toggleFilters", FALSE)
  })
  observeEvent(input$close_filters_btn, {
    session$sendCustomMessage("toggleFilters", FALSE)
  })
