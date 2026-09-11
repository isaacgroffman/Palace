  # ===================== HITTERS: PROCESS MODEL PAGE =====================
  # Decision Value / Contact / Power / Process+ off per-pitch run values,
  # conditioned on Pitching+ pitch quality. Data: NCAA parquet (both seasons),
  # reclassified pitch types, proModel-scored on load.

  # Hidden hp_batter selectize is the reactive driver; its choices are
  # built per season selection (NCAA directory restricted to the picked
  # seasons' source years + CCU bio hitters) and every entry point —
  # unified search, roster cards, name links — funnels through
  # open_hitter_page().
  .hp_choice_cache <- reactiveValues()
  .hp_season_key <- function() {
    yrs <- season_src_years(input$season_type)
    paste(sort(yrs), collapse = "_")
  }
  ensure_hp_choices <- function() {
    key <- .hp_season_key()
    ch <- .hp_choice_cache[[key]]
    if (!is.null(ch)) return(invisible(ch))
    withProgress(message = "Indexing college hitters...", value = 0.4, {
      dir <- tryCatch(ncaa_batter_directory(), error = function(e) NULL)
    })
    ch <- character(0)
    if (!is.null(dir) && nrow(dir) > 0) {
      yrs <- season_src_years(input$season_type)
      if (length(yrs) > 0 && "srcs" %in% names(dir)) {
        keep <- vapply(dir$srcs, function(s) {
          s <- as.character(s)
          length(s) == 0 || any(s %in% yrs)
        }, logical(1))
        dir <- dir[keep, , drop = FALSE]
      }
      if (nrow(dir) > 0) {
        ch <- setNames(dir$display,
                       paste0(dir$display,
                              ifelse(nzchar(dir$teams), paste0(" \u2014 ", dir$teams), "")))
      }
    }
    if (!is.null(hitter_bio)) {
      extra <- setdiff(unique(as.character(hitter_bio$Batter_FL)), unname(ch))
      extra <- extra[!is.na(extra) & nzchar(extra)]
      if (length(extra) > 0) {
        ch <- c(setNames(extra, paste0(extra, " \u2014 Coastal Carolina")), ch)
      }
    }
    .hp_choice_cache[[key]] <- ch
    invisible(ch)
  }

  open_hitter_page <- function(nm) {
    if (is.null(nm) || !nzchar(nm)) return(invisible(NULL))

    # hp_batter is a HIDDEN input -- nobody ever browses it, it just drives
    # the hp_* reactives. It used to get the entire ~12k NCAA batter
    # directory pushed into it through server-side selectize on every
    # open, which meant indexing the directory (with a progress bar) and
    # then waiting on a server round-trip before input$hp_batter changed.
    # That round-trip is exactly why a hitter would "load but not show".
    # One choice, client side, lands immediately.
    updateSelectizeInput(session, "hp_batter",
                         choices = setNames(nm, nm), selected = nm,
                         server = FALSE)

    player_mode("hitter")
    updateTabsetPanel(session, "main_tabs", selected = "Players")
    # Reveal the hitter pills BEFORE selecting one. hideTab drops the tab
    # from the DOM, so selecting a still-hidden pill is a no-op and we'd
    # be relying on observer ordering to recover.
    for (v in c("Overview", "Bullpens", "Scouting"))
      hideTab("player_subtabs", v)
    showTab("player_subtabs", "hp_overview")
    updateTabsetPanel(session, "player_subtabs", selected = "hp_overview")
  }

  .hp_choices_key_set <- reactiveVal(NULL)
  # (Removed) This used to re-index the whole batter directory and re-push
  # 12k choices into the hidden hp_batter selectize whenever the season or
  # mode changed -- and, because it ran after open_hitter_page, it could
  # clobber the selection that had just been made. The hidden input only
  # ever needs the one hitter that is open. The real, user-facing search
  # list is still built by build_global_hitter_choices() for the header.

  # ---- Hitter player header: CCU bio header (mirrors the pitcher card)
  # for Coastal hitters, DRS26-based header for everyone else. ----
  output$hp_player_header <- renderPlot({
    nm <- input$hp_batter
    req(nm, nzchar(nm))
    tryCatch({
      g <- if (is_coastal_hitter(nm)) {
        create_hitter_player_header(nm)
      } else {
        bd <- tryCatch(hp_data(), error = function(e) NULL)
        create_ncaa_hitter_header(nm, bd)
      }
      grid.newpage()
      grid.draw(g)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Player info for", nm), cex = 2, font = 2)
    })
  }, height = 250)

  # ---- Position-dependent defense tabs (DRS positions + CCU bio) ----
  observeEvent(list(input$hp_batter, player_mode()), {
    # Only this observer owns the three defense pills. In pitcher mode the
    # PLAYER MODE block has already hidden them, so stay out of the way.
    if (!identical(player_mode(), "hitter")) return()
    nm <- input$hp_batter
    cl <- if (is.null(nm) || !nzchar(nm)) {
      list(inf = FALSE, of = FALSE, c = FALSE)
    } else {
      classify_hitter_pos(hitter_pos_string(nm))
    }
    tgl <- function(val, on) {
      if (isTRUE(on)) showTab("player_subtabs", val)
      else hideTab("player_subtabs", val)
    }
    tgl("hit_inf", cl$inf)
    tgl("hit_of",  cl$of)
    tgl("hit_c",   cl$c)
    # Bounce off a defense tab the new hitter doesn't earn.
    cur <- input$player_subtabs
    if (isTRUE(cur %in% c("hit_inf", "hit_of", "hit_c"))) {
      ok <- switch(cur, hit_inf = cl$inf, hit_of = cl$of, hit_c = cl$c, TRUE)
      if (!isTRUE(ok)) {
        updateTabsetPanel(session, "player_subtabs", selected = "hp_overview")
      }
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # DRS rows for one position group -> defense tab content.
  .hp_def_positions <- list(
    inf = c("1B", "2B", "3B", "SS", "INF", "IF", "MIF", "CIF"),
    of  = c("LF", "CF", "RF", "OF"),
    c   = c("C", "CA"))
  hp_def_ui <- function(nm, grp, title) {
    if (is.null(nm) || !nzchar(nm)) {
      return(div(class = "adv-placeholder",
                 p("Pick a hitter from the top search bar.")))
    }
    team <- tryCatch(tm_current_batter_team(nm), error = function(e) NULL)
    r <- tryCatch(drs_lookup(nm, team), error = function(e) NULL)
    rows <- NULL
    if (!is.null(r) && !is.null(drs_bio)) {
      rows <- drs_bio[!is.na(drs_bio$bio_key) & drs_bio$bio_key == r$bio_key, ,
                      drop = FALSE]
      if ("team_name" %in% names(rows) && "team_name" %in% names(r) &&
          !is.na(r$team_name)) {
        tr <- rows[!is.na(rows$team_name) & rows$team_name == r$team_name, ,
                   drop = FALSE]
        if (nrow(tr) > 0) rows <- tr
      }
      keep <- toupper(trimws(as.character(rows$Position))) %in%
        .hp_def_positions[[grp]]
      rows <- rows[keep, , drop = FALSE]
    }
    if (is.null(rows) || nrow(rows) == 0) {
      return(div(class = "hp-card", style = "max-width:640px;margin:18px auto;",
                 div(class = "hp-sec", title),
                 p(style = "color:#6B7280;",
                   "No DRS rows at this position group yet \u2014 advanced ",
                   tolower(title), " metrics land here as the defensive ",
                   "pipeline builds out.")))
    }
    tbl_rows <- lapply(seq_len(nrow(rows)), function(i) {
      v <- if (!is.null(.drs_rating_col))
        suppressWarnings(as.numeric(rows[[.drs_rating_col]][i])) else NA_real_
      tags$tr(
        tags$td(as.character(rows$Position[i])),
        tags$td(style = paste0(
          "font-weight:700;color:",
          if (is.na(v)) "#6B7280" else if (v >= 0) "#00840D" else "#E1463E", ";"),
          if (is.na(v)) "\u2014" else sprintf("%+d", round(v))))
    })
    div(class = "hp-card", style = "max-width:640px;margin:18px auto;",
        div(class = "hp-sec", paste0(title, " \u2014 Defensive Runs Saved")),
        tags$table(class = "table", style = "max-width:420px;",
          tags$thead(tags$tr(tags$th("Position"), tags$th("DRS"))),
          tags$tbody(tbl_rows)),
        div(class = "hp-note",
            "DRS from the D1 defensive file; positive = runs saved vs ",
            "average at that spot."))
  }
  output$hp_def_inf <- renderUI(hp_def_ui(input$hp_batter, "inf", "Infield Defense"))
  output$hp_def_of  <- renderUI(hp_def_ui(input$hp_batter, "of",  "Outfield Defense"))
  output$hp_def_c   <- renderUI(hp_def_ui(input$hp_batter, "c",   "Catching"))

  # ---- Player-name links anywhere in the UI (leaderboards, matrix, ...) ----
  observeEvent(input$player_link_click, {
    pl <- input$player_link_click
    nm <- pl$name %||% ""
    side <- pl$side %||% "pitcher"
    req(nzchar(nm))
    if (identical(side, "hitter")) {
      open_hitter_page(nm)
    } else {
      # Resolve to a name the picker actually holds: TrackMan id first (the
      # leaderboard / matrix carry it), then a spelling-tolerant name match.
      # Pushing an unknown spelling used to leave the picker on a name no
      # frame contains, which made the pitcher filter fall through and the
      # page show every Coastal arm at once.
      pool <- c(session$userData$global_choice_pool, roster27_pitcher_fl, bullpen_pitchers_fl)
      target <- tryCatch(ncaa_display_for_id(pl$id %||% ""), error = function(e) NULL)
      if (is.null(target)) target <- resolve_pitcher_name(nm, pool)
      if (is.null(target)) {
        showNotification(paste0("No pitch data for ", nm, " in the selected season."),
                         type = "warning", duration = 6)
        return()
      }
      updateSelectizeInput(session, "global_pitcher", selected = target)
      player_mode("pitcher")
      updateTabsetPanel(session, "main_tabs", selected = "Players")
      updateTabsetPanel(session, "player_subtabs", selected = "Overview")
    }
  })

  hp_data <- reactive({
    bt <- input$hp_batter
    req(bt, nzchar(bt))
    # hp_tm_team() is the page's own resolver (Coastal short-circuit +
    # tm_current_batter_team); handing it over means the loader never has
    # to guess the team a second time.
    df <- withProgress(message = paste("Loading", bt, "..."), value = 0.4,
                       load_ncaa_batter_data(bt, hp_tm_team(bt)))
    validate(need(is.data.frame(df) && nrow(df) > 0,
                  "No tracked pitches found for this hitter."))
    # honor the header season picker: Spring 2026 selected = only
    # spring 2026 pitches, etc. (load stays cached; this is a cheap cut)
    df <- hp_season_filter(df, input$season_type)
    validate(need(nrow(df) > 0,
                  "No tracked pitches for this hitter in the selected season(s)."))
    df
  })

  # ---- Global filter sidebar applied to the hitter's pitches faced ----
  # Same controls as the pitcher pages, adapted to the batter's box:
  # the Hand picker filters PITCHER hand, "Vs Team" matches the pitcher's
  # team, and the Games picker lists games by pitches FACED.
  hp_data_f <- reactive({
    df <- hp_data()
    if (is.null(df) || nrow(df) == 0) return(df)
    # date range
    if (!is.null(input$g_dateRange) && "Date" %in% names(df)) {
      dt <- safe_as_date(df$Date)
      df <- df[!is.na(dt) & dt >= as.Date(input$g_dateRange[1]) &
                 dt <= as.Date(input$g_dateRange[2]), , drop = FALSE]
    }
    # hand: on the hitter page the hand picker means pitcher hand
    if (!is.null(input$g_BatterSide) &&
        length(input$g_BatterSide) == 1L &&   # one hand selected = a real filter
        "PitcherThrows" %in% names(df)) {
      h <- substr(input$g_BatterSide, 1, 1)
      df <- df[toupper(substr(trimws(as.character(df$PitcherThrows)), 1, 1)) %in% h, ,
               drop = FALSE]
    }
    # pitch type
    if (!is.null(input$g_pitchtype) && "TaggedPitchType" %in% names(df)) {
      df <- df[as.character(df$TaggedPitchType) %in% input$g_pitchtype, ,
               drop = FALSE]
    }
    # velocity of pitches faced
    if (!is.null(input$g_velo) && "RelSpeed" %in% names(df)) {
      rs <- suppressWarnings(as.numeric(df$RelSpeed))
      df <- df[!is.na(rs) & rs >= input$g_velo[1] & rs <= input$g_velo[2], ,
               drop = FALSE]
    }
    # count + result share the pitcher filter helpers verbatim
    df <- apply_count_filter(df, input$g_count)
    df <- apply_result_filter(df, input$g_result)
    # games (faced)
    if (!is.null(input$game_filter_spring26) &&
        length(input$game_filter_spring26) > 0 && "GameID" %in% names(df)) {
      df <- df[df$GameID %in% input$game_filter_spring26, , drop = FALSE]
    }
    # vs team = the PITCHER's team from the batter's box
    if (!is.null(input$g_vs_team) && nzchar(trimws(input$g_vs_team)) &&
        "PitcherTeam" %in% names(df)) {
      patt <- trimws(input$g_vs_team)
      df <- df[grepl(patt, df$PitcherTeam, ignore.case = TRUE) |
                 grepl(patt, prettify_team(df$PitcherTeam),
                       ignore.case = TRUE), , drop = FALSE]
    }
    df
  })

  hp_summary <- reactive({
    d <- hp_data_f()
    validate(need(is.data.frame(d) && nrow(d) > 0,
                  "No pitches survive the current filters."))
    s <- hp_hitter_summary(d)
    validate(need(!is.null(s) && !is.null(s$overall),
                  "Not enough scoreable pitches to grade this hitter."))
    s
  })

  output$hp_header <- renderUI({
    df <- hp_data()
    nf <- tryCatch(nrow(hp_data_f()), error = function(e) nrow(df))
    side <- names(sort(table(as.character(df$BatterSide)), decreasing = TRUE))[1] %||% ""
    team <- names(sort(table(prettify_team(as.character(df$BatterTeam))),
                       decreasing = TRUE))[1] %||% ""
    div(
      tags$span(style = "font-size:20px;font-weight:800;color:#111827;",
                input$hp_batter),
      tags$span(style = "color:#6B7280;font-size:13px;margin-left:10px;",
                paste0(team, if (nzchar(side)) paste0(" \u2022 Bats ", substr(side,1,1)) else "",
                       " \u2022 ", format(nf, big.mark = ","), " pitches",
                       if (nf < nrow(df))
                         paste0(" (of ", format(nrow(df), big.mark = ","),
                                ", filtered)") else "")))
  })

  # ---- Traditional stats strip (TruMedia API), mirrors the pitcher one ----
  hp_tm_team <- function(nm) {
    if (is.null(nm) || !nzchar(nm)) return(NULL)
    if (is_coastal_hitter(nm)) return("Coastal Carolina")
    tm_current_batter_team(nm)
  }

  hp_tm_splits_r <- reactive({
    nm <- input$hp_batter
    req(nm, nzchar(nm))
    if (!tm_enabled()) return(NULL)
    td <- hp_tm_team(nm)
    if (is.null(td) || !nzchar(td)) return(NULL)
    tryCatch(tm_batter_splits(nm, td), error = function(e) NULL)
  })

  output$hp_tm_trad_stats <- renderUI({
    nm <- input$hp_batter
    req(nm, nzchar(nm))
    if (!tm_enabled()) return(NULL)
    sp <- hp_tm_splits_r()
    if (is.null(sp) || is.null(sp$overall)) {
      return(div(style = "text-align:center; color:#9CA3AF; font-size:12px; margin:2px 0 8px;",
                 "TruMedia season stats unavailable for this hitter."))
    }
    row <- sp$overall
    items <- list()
    for (ab in c("G","PA","AB","H","2B","3B","HR","BB","HBP","K","SB",
                 "BA","OBP","SLG")) {
      v <- .tm_val(row, ab)
      if (!is.na(v)) items[[if (ab == "BA") "AVG" else ab]] <- .tp_fmt_stat(ab, v)
    }
    # OPS from the pieces when both made it back
    obp <- suppressWarnings(as.numeric(items[["OBP"]]))
    slg <- suppressWarnings(as.numeric(items[["SLG"]]))
    if (is.finite(obp) && is.finite(slg))
      items[["OPS"]] <- .tp_fmt_stat("OPS", obp + slg)
    if (length(items) == 0) return(NULL)
    cols <- vapply(names(items), function(ab)
      .tp_stat_color(ab, items[[ab]], "bat"), character(1))
    box <- function(lab, val, col) {
      div(style = paste0("background:#F3F7F7; border:1px solid #CFE3E3;",
                         "border-radius:8px; padding:6px 14px; text-align:center;",
                         "min-width:64px;"),
          div(style = "font-size:11px; color:#6B7280; letter-spacing:.4px;", lab),
          div(style = paste0("font-size:18px; font-weight:700; color:", col, ";"),
              val))
    }
    div(style = "display:flex; flex-wrap:wrap; gap:8px; justify-content:center; margin:4px 0 10px;",
        mapply(box, names(items), unlist(items), cols, SIMPLIFY = FALSE))
  })

  .hp_pct_color <- function(p) {
    if (!is.finite(p)) return("#9CA3AF")
    # savant convention: blue = cold, red = elite
    if (p >= 90) "#D82129" else if (p >= 75) "#E8604C" else if (p >= 60) "#EFA487"
    else if (p >= 40) "#9CA3AF" else if (p >= 25) "#8FB4D9" else if (p >= 10) "#5A8FC7"
    else "#3567A6"
  }

  # ---- Percentile chart: every stat subset as savant-style bars ----
  output$hp_stat_pct_bars <- renderUI({
    s <- hp_summary()$overall
    validate(need(!is.null(s), "Not enough scoreable pitches."))
    bar <- function(lab, p, raw_txt, bold = FALSE) {
      pw <- if (is.finite(p)) max(2, min(100, p)) else 0
      col <- .hp_pct_color(p)
      div(class = "hp-bar-row",
        div(class = "hp-bar-lab",
            style = if (bold) "font-weight:800;" else NULL, lab),
        div(class = "hp-bar-wrap",
            div(class = "hp-bar-fill",
                style = paste0("width:", pw, "%;background:", col, ";")),
            div(class = "hp-bar-dot",
                style = paste0("left:", pw, "%;background:", col, ";"),
                if (is.finite(p)) p else "\u2014")),
        div(class = "hp-bar-plus", raw_txt))
    }
    sec <- function(lab) {
      div(style = paste0("font-weight:800;color:#0E6E70;font-size:11px;",
                         "letter-spacing:.06em;margin:14px 0 2px;"), lab)
    }
    fmt1 <- function(x) if (is.finite(x)) sprintf("%.1f", x) else "\u2014"
    fmtg <- function(x) if (is.finite(x)) as.character(x) else "\u2014"
    st <- s$stats; sp <- s$stat_pct
    tagList(
      sec("PROCESS"),
      bar("Process+",      s$pct$prc, fmtg(s$plus$prc), bold = TRUE),
      sec("DECISIONS"),
      bar("Decision+",     s$pct$dv,  fmtg(s$plus$dv), bold = TRUE),
      bar("Z-Swing %",     sp$z_swing, fmt1(st$z_swing)),
      bar("Chase %",       sp$chase,   fmt1(st$chase)),
      bar("Swing %",       sp$swing_pct, fmt1(st$swing_pct)),
      sec("CONTACT"),
      bar("Contact+",      s$pct$cv,  fmtg(s$plus$cv), bold = TRUE),
      bar("Contact %",     sp$contact, fmt1(st$contact)),
      bar("Whiff %",       sp$whiff,   fmt1(st$whiff)),
      bar("Z-Whiff %",     sp$z_whiff, fmt1(st$z_whiff)),
      sec("POWER"),
      bar("Power+",        s$pct$pv,  fmtg(s$plus$pv), bold = TRUE),
      bar("Avg EV",        sp$avg_ev,  fmt1(st$avg_ev)),
      bar("90th EV",       sp$ev90,    fmt1(st$ev90)),
      bar("Hard-Hit %",    sp$hard_hit, fmt1(st$hard_hit))
    )
  })

  # ---- Value by pitch type, vs RHP / vs LHP ----
  render_hp_pitch_table <- function(which) {
    DT::renderDataTable({
      s <- hp_summary()
      d <- s[[which]]
      hand <- if (identical(which, "by_pitch_r")) "RHP" else "LHP"
      validate(need(!is.null(d) && nrow(d) > 0,
                    paste0("No pitches tracked vs ", hand, ".")))
      d$PrcPct <- ifelse(is.finite(d$PrcPct), d$PrcPct, NA)
      names(d) <- c("Pitch", "N", "Usage %", "Swing %", "Whiff %",
                    "DV/100", "Con/100", "Pow/100", "Prc/100", "Pctl")
      brks <- seq(-1.2, 1.2, length.out = 20)
      clrs <- colorRampPalette(c("#3567A6", "#F7F7F7", "#D82129"))(21)
      pbrks <- c(10, 25, 40, 60, 75, 90)
      pclrs <- c("#3567A6", "#5A8FC7", "#8FB4D9", "#F7F7F7",
                 "#EFA487", "#E8604C", "#D82129")
      datatable(d, rownames = FALSE,
                options = list(dom = "t", paging = FALSE, ordering = FALSE,
                               columnDefs = list(list(className = "dt-center",
                                                      targets = "_all")))) %>%
        formatStyle(c("DV/100", "Con/100", "Pow/100", "Prc/100"),
                    backgroundColor = styleInterval(brks, clrs)) %>%
        formatStyle("Pctl", backgroundColor = styleInterval(pbrks, pclrs),
                    fontWeight = "bold") %>%
        formatStyle("Prc/100", fontWeight = "bold")
    })
  }
  output$hp_pitch_table_r <- render_hp_pitch_table("by_pitch_r")
  output$hp_pitch_table_l <- render_hp_pitch_table("by_pitch_l")

  # ---- Diverging raw-value bars per pitch type (0-centered) ----
  output$hp_pt_value_bars <- renderUI({
    s <- hp_summary()
    d <- s$by_pitch
    validate(need(!is.null(d) && nrow(d) > 0,
                  "No pitch-type breakdown available."))
    vmax <- max(abs(d$Prc100), na.rm = TRUE)
    vmax <- max(vmax, 1)
    rows <- lapply(seq_len(nrow(d)), function(i) {
      v <- d$Prc100[i]
      p <- d$PrcPct[i]
      w <- if (is.finite(v)) 50 * min(1, abs(v) / vmax) else 0
      pos <- is.finite(v) && v >= 0
      fill_style <- if (pos) {
        paste0("left:50%;width:", w, "%;background:#00840D;")
      } else {
        paste0("left:", 50 - w, "%;width:", w, "%;background:#E1463E;")
      }
      badge <- if (is.finite(p)) {
        tags$span(style = paste0(
          "display:inline-block;min-width:30px;text-align:center;",
          "border-radius:10px;padding:1px 7px;font-size:11px;font-weight:800;",
          "color:#fff;background:", .hp_pct_color(p), ";"), p)
      } else {
        tags$span(style = "color:#9CA3AF;font-size:11px;", "\u2014")
      }
      div(class = "hp-bar-row",
        div(class = "hp-bar-lab",
            paste0(d$PitchType[i], " (", d$N[i], ")")),
        div(class = "hp-bar-wrap", style = "height:14px;",
            # center zero line
            div(style = paste0("position:absolute;left:50%;top:-3px;bottom:-3px;",
                               "width:2px;background:#9CA3AF;")),
            div(class = "hp-bar-fill",
                style = paste0("position:absolute;height:14px;", fill_style))),
        div(class = "hp-bar-plus", style = "width:110px;",
            tags$span(style = paste0(
              "font-weight:800;margin-right:8px;color:",
              if (is.finite(v) && v >= 0) "#00840D" else "#E1463E", ";"),
              if (is.finite(v)) sprintf("%+.2f", v) else "\u2014"),
            badge))
    })
    tagList(rows,
            div(style = paste0("display:flex;justify-content:space-between;",
                               "color:#9CA3AF;font-size:11px;margin-top:4px;"),
                tags$span("\u2190 runs lost"),
                tags$span("0"),
                tags$span("runs added \u2192")))
  })

  # ============================================================
  # ===== RESTRUCTURED HITTER PLAYER PAGE ======================
  # Mirrors the pitcher page: bio + grouped percentile chart, run
  # value by pitch type, then Metrics / Results / Zone / Batted
  # Ball / Modeling subtabs. Everything hangs off hp_data_f().
  # ============================================================

  # ---- bio card ----
  output$hp_bio_card <- renderUI({
    bt <- input$hp_batter
    req(bt, nzchar(bt))
    df <- tryCatch(hp_data(), error = function(e) NULL)
    nf <- tryCatch(nrow(hp_data_f()), error = function(e) 0)

    vv <- function(x, d = "\u2014") {
      if (is.null(x) || length(x) == 0) return(d)
      x <- x[1]
      if (is.na(x) || !nzchar(trimws(as.character(x)))) d else as.character(x)
    }
    cell <- function(lab, val) div(class = "pp-bio-cell",
                                   div(class = "pp-bio-lab", lab),
                                   div(class = "pp-bio-val", val))

    side <- if (!is.null(df) && "BatterSide" %in% names(df))
      names(sort(table(as.character(df$BatterSide)), decreasing = TRUE))[1] else NA
    team <- if (!is.null(df) && "BatterTeam" %in% names(df))
      names(sort(table(prettify_team(as.character(df$BatterTeam))),
                 decreasing = TRUE))[1] else NA

    r <- tryCatch(drs_lookup(bt, team), error = function(e) NULL)
    hb <- NULL
    if (exists("hitter_bio") && is.data.frame(hitter_bio)) {
      nmcol <- intersect(c("Batter_FL", "Batter", "Player", "Name"), names(hitter_bio))
      if (length(nmcol)) {
        idx <- which(as.character(hitter_bio[[nmcol[1]]]) == bt)[1]
        if (!is.na(idx)) hb <- hitter_bio[idx, , drop = FALSE]
      }
    }
    pick <- function(field, alt = NULL) {
      if (!is.null(hb) && field %in% names(hb)) return(vv(hb[[field]]))
      if (!is.null(r) && !is.null(alt) && alt %in% names(r)) return(vv(r[[alt]]))
      if (!is.null(r) && field %in% names(r)) return(vv(r[[field]]))
      "\u2014"
    }
    logo <- if (!is.null(r) && "team_logo" %in% names(r)) r$team_logo else NULL
    num  <- if (!is.null(hb) && "Number" %in% names(hb)) vv(hb$Number, "")
            else if (!is.null(r) && "Jersey" %in% names(r)) vv(r$Jersey, "") else ""
    bats <- if (!is.na(side) && nzchar(side)) substr(side, 1, 1) else "\u2014"

    hs <- tryCatch(headshot_lookup(bt, team), error = function(e) NULL)
    if ((is.null(logo) || is.na(logo) || !nzchar(as.character(logo))) &&
        !is.null(hs)) logo <- hs$logo
    # headshot: Coastal bio file first, TruMedia headshot table otherwise
    head_src <- if (!is.null(hb) && "Headshot" %in% names(hb) &&
                    !is.na(hb$Headshot[1]) && nzchar(as.character(hb$Headshot[1])))
      as.character(hb$Headshot[1]) else if (!is.null(hs)) hs$headshot else NULL

    div(class = "pp-card",
      div(class = "pp-bio-top",
        if (!is.null(head_src) && !is.na(head_src[1]) && nzchar(head_src[1]))
          tags$img(class = "pp-bio-head", src = head_src[1],
                   onerror = "this.style.display='none';") else NULL,
        div(
          div(class = "pp-bio-name", bt),
          div(class = "pp-bio-sub",
              paste0(if (!is.na(team)) team else "\u2014",
                     if (nzchar(num)) paste0(" \u2022 #", num) else ""))
        ),
        if (!is.null(logo) && !is.na(logo) && nzchar(as.character(logo)))
          tags$img(class = "pp-bio-logo", src = as.character(logo)) else NULL
      ),
      div(class = "pp-bio-grid",
        cell("Pos", pick("Position")), cell("Bats", bats),
        cell("Year", pick("Class")),
        cell("Height", pick("Height")), cell("Weight", pick("Weight")),
        cell("Pitches", format(nf, big.mark = ",")),
        div(class = "pp-bio-cell", style = "grid-column:1 / -1;",
            div(class = "pp-bio-lab", "Hometown"),
            div(class = "pp-bio-val", pick("Hometown")))
      )
    )
  })

  output$hp_pct_title <- renderText({
    yr <- tryCatch({
      d <- hp_data_f()
      y <- suppressWarnings(as.integer(format(safe_as_date(d$Date), "%Y")))
      y <- y[is.finite(y)]
      if (length(y)) max(y, na.rm = TRUE) else NA_integer_
    }, error = function(e) NA_integer_)
    if (is.na(yr)) "Percentile Rankings" else paste(yr, "Percentile Rankings")
  })

  # ---- grouped percentile chart (Swing Decisions / Contact / Power) ----
  output$hp_percentile_chart <- renderUI({
    s <- hp_summary()$overall
    validate(need(!is.null(s), "Not enough scoreable pitches to grade this hitter."))
    st <- s$stats; sp <- s$stat_pct

    f1 <- function(x) if (is.finite(x)) sprintf("%.1f", x) else "\u2014"
    fg <- function(x) if (is.finite(x)) as.character(x) else "\u2014"
    row <- function(sec, lab, pct, disp, grade = FALSE) {
      data.frame(section = sec, label = lab,
                 pct = if (is.finite(pct)) pct / 100 else NA_real_,
                 disp = disp, grade = grade, stringsAsFactors = FALSE)
    }

    rows <- rbind(
      row("Process",         "Process+",    s$pct$prc, fg(s$plus$prc), TRUE),
      row("Swing Decisions", "Decision+",   s$pct$dv,  fg(s$plus$dv),  TRUE),
      row("Swing Decisions", "Z-Swing %",   sp$z_swing,   f1(st$z_swing)),
      row("Swing Decisions", "Chase %",     sp$chase,     f1(st$chase)),
      row("Swing Decisions", "Swing %",     sp$swing_pct, f1(st$swing_pct)),
      row("Contact",         "Contact+",    s$pct$cv,  fg(s$plus$cv),  TRUE),
      row("Contact",         "Contact %",   sp$contact,   f1(st$contact)),
      row("Contact",         "Whiff %",     sp$whiff,     f1(st$whiff)),
      row("Contact",         "Z-Whiff %",   sp$z_whiff,   f1(st$z_whiff)),
      row("Power",           "Power+",      s$pct$pv,  fg(s$plus$pv),  TRUE),
      row("Power",           "Avg EV",      sp$avg_ev,    f1(st$avg_ev)),
      row("Power",           "90th EV",     sp$ev90,      f1(st$ev90)),
      row("Power",           "Hard-Hit %",  sp$hard_hit,  f1(st$hard_hit))
    )
    pp_percentile_ui(rows)
  })

  # ==================== HITTER METRICS ====================

  hp_disc_stats <- function(d) {
    sw  <- !is.na(d$SwingIndicator) & d$SwingIndicator == 1
    pc  <- as.character(d$PitchCall)
    x   <- suppressWarnings(as.numeric(d$PlateLocSide))
    z   <- suppressWarnings(as.numeric(d$PlateLocHeight))
    inz <- is.finite(x) & is.finite(z) & abs(x) <= 0.83 & z >= 1.5 & z <= 3.38
    pdv <- function(a, b) if (isTRUE(b > 0)) 100 * a / b else NA_real_
    tibble::tibble(
      Pitches    = nrow(d),
      `Swing %`  = pdv(sum(sw, na.rm = TRUE), nrow(d)),
      `Z-Swing %`= pdv(sum(sw & inz, na.rm = TRUE), sum(inz, na.rm = TRUE)),
      `Chase %`  = pdv(sum(sw & !inz, na.rm = TRUE), sum(!inz, na.rm = TRUE)),
      `Contact %`= pdv(sum(sw & pc != "StrikeSwinging", na.rm = TRUE),
                       sum(sw, na.rm = TRUE)),
      `Whiff %`  = pdv(sum(sw & pc == "StrikeSwinging", na.rm = TRUE),
                       sum(sw, na.rm = TRUE)),
      `Z-Whiff %`= pdv(sum(sw & inz & pc == "StrikeSwinging", na.rm = TRUE),
                       sum(sw & inz, na.rm = TRUE))
    )
  }

  hp_qoc_stats <- function(d) {
    bip <- as.character(d$PitchCall) == "InPlay"
    ev  <- suppressWarnings(as.numeric(d$ExitSpeed))
    la  <- suppressWarnings(as.numeric(d$Angle))
    dist<- suppressWarnings(as.numeric(d$Distance))
    n   <- sum(bip, na.rm = TRUE)
    pdv <- function(a, b) if (isTRUE(b > 0)) 100 * a / b else NA_real_
    mn  <- function(v) { v <- v[is.finite(v)]; if (length(v)) mean(v) else NA_real_ }
    tibble::tibble(
      `Batted Balls` = n,
      `Avg EV`       = mn(ev[bip]),
      `Max EV`       = if (any(is.finite(ev[bip]))) max(ev[bip], na.rm = TRUE) else NA_real_,
      `90th EV`      = if (any(is.finite(ev[bip])))
                         as.numeric(stats::quantile(ev[bip], 0.9, na.rm = TRUE)) else NA_real_,
      `Avg LA`       = mn(la[bip]),
      `Hard-Hit %`   = pdv(sum(bip & is.finite(ev) & ev >= 95, na.rm = TRUE), n),
      `Sweet-Spot %` = pdv(sum(bip & is.finite(la) & la >= 8 & la <= 32, na.rm = TRUE), n),
      `Avg Dist`     = mn(dist[bip])
    )
  }

  output$hp_discipline_table <- gt::render_gt({
    d <- hp_data_f()
    validate(need(nrow(d) > 0, "No pitches survive the current filters."))
    tb <- dplyr::bind_rows(
      cbind(Split = "Overall", hp_disc_stats(d)),
      cbind(Split = "vs RHP",  hp_disc_stats(d[.hp_phand(d) == "R", , drop = FALSE])),
      cbind(Split = "vs LHP",  hp_disc_stats(d[.hp_phand(d) == "L", , drop = FALSE]))
    )
    tb %>% gt::gt() %>% pp_gt_modern() %>%
      gt::fmt_number(columns = dplyr::where(is.numeric), decimals = 1) %>%
      gt::fmt_number(columns = "Pitches", decimals = 0) %>%
      gt::tab_style(style = gt::cell_text(weight = "bold"),
                    locations = gt::cells_body(columns = 1))
  })

  output$hp_quality_table <- gt::render_gt({
    d <- hp_data_f()
    validate(need(nrow(d) > 0, "No pitches survive the current filters."))
    tb <- dplyr::bind_rows(
      cbind(Split = "Overall", hp_qoc_stats(d)),
      cbind(Split = "vs RHP",  hp_qoc_stats(d[.hp_phand(d) == "R", , drop = FALSE])),
      cbind(Split = "vs LHP",  hp_qoc_stats(d[.hp_phand(d) == "L", , drop = FALSE]))
    )
    tb %>% gt::gt() %>% pp_gt_modern() %>%
      gt::fmt_number(columns = dplyr::where(is.numeric), decimals = 1) %>%
      gt::fmt_number(columns = "Batted Balls", decimals = 0) %>%
      gt::tab_style(style = gt::cell_text(weight = "bold"),
                    locations = gt::cells_body(columns = 1))
  })

  # ==================== HITTER RESULTS ====================

  .hp_phand <- function(d) {
    if (!"PitcherThrows" %in% names(d)) return(rep(NA_character_, nrow(d)))
    toupper(substr(trimws(as.character(d$PitcherThrows)), 1, 1))
  }

  observeEvent(input$g_dateRange, {
    updateDateRangeInput(session, "hp_res_dates",
                         start = input$g_dateRange[1], end = input$g_dateRange[2])
    updateDateRangeInput(session, "hp_mod_dates",
                         start = input$g_dateRange[1], end = input$g_dateRange[2])
  }, ignoreNULL = TRUE)

  .hp_calc <- function(dd, metric) {
    sw  <- !is.na(dd$SwingIndicator) & dd$SwingIndicator == 1
    pc  <- as.character(dd$PitchCall)
    x   <- suppressWarnings(as.numeric(dd$PlateLocSide))
    z   <- suppressWarnings(as.numeric(dd$PlateLocHeight))
    inz <- is.finite(x) & is.finite(z) & abs(x) <= 0.83 & z >= 1.5 & z <= 3.38
    ev  <- suppressWarnings(as.numeric(dd$ExitSpeed))
    la  <- suppressWarnings(as.numeric(dd$Angle))
    bip <- pc == "InPlay"
    s <- function(v) sum(v, na.rm = TRUE)
    pdv <- function(a, b) if (isTRUE(is.finite(b) && b > 0)) a / b else NA_real_
    switch(metric,
      "Swing %"      = 100 * pdv(s(sw), nrow(dd)),
      "Chase %"      = 100 * pdv(s(sw & !inz), s(!inz)),
      "Z-Swing %"    = 100 * pdv(s(sw & inz), s(inz)),
      "Whiff %"      = 100 * pdv(s(sw & pc == "StrikeSwinging"), s(sw)),
      "Z-Whiff %"    = 100 * pdv(s(sw & inz & pc == "StrikeSwinging"), s(sw & inz)),
      "Contact %"    = 100 * pdv(s(sw & pc != "StrikeSwinging"), s(sw)),
      "Hard-Hit %"   = 100 * pdv(s(bip & is.finite(ev) & ev >= 95), s(bip)),
      "Sweet-Spot %" = 100 * pdv(s(bip & is.finite(la) & la >= 8 & la <= 32), s(bip)),
      "Avg EV"       = { v <- ev[bip & is.finite(ev)]; if (length(v)) mean(v) else NA_real_ },
      "AVG"          = pdv(s(dd$HitIndicator), s(dd$ABindicator)),
      "SLG"          = pdv(s(dd$totalbases), s(dd$ABindicator)),
      NA_real_)
  }

  .hp_trend_frame <- function(d, metric, split, side, min_n = 4) {
    if (is.null(d) || !nrow(d)) return(NULL)
    if (side %in% c("R", "L")) d <- d[.hp_phand(d) == side, , drop = FALSE]
    d <- d[!is.na(d$TaggedPitchType) & d$TaggedPitchType != "Other", , drop = FALSE]
    if (!nrow(d)) return(NULL)
    d$.d   <- safe_as_date(d$Date)
    d$.grp <- if (identical(split, "ind")) as.character(d$TaggedPitchType) else "All"
    d <- d[!is.na(d$.d), , drop = FALSE]
    if (!nrow(d)) return(NULL)
    keys <- split(seq_len(nrow(d)), list(as.character(d$.d), d$.grp), drop = TRUE)
    out <- do.call(rbind, lapply(keys, function(ix) {
      dd <- d[ix, , drop = FALSE]
      data.frame(date = dd$.d[1], grp = dd$.grp[1], n = nrow(dd),
                 val = .hp_calc(dd, metric), stringsAsFactors = FALSE)
    }))
    if (is.null(out)) return(NULL)
    out <- out[is.finite(out$val) & out$n >= min_n, , drop = FALSE]
    if (!nrow(out)) return(NULL)
    out[order(out$date), , drop = FALSE]
  }

  hp_trend_source <- function(dates) {
    d <- hp_data_f()
    req(is.data.frame(d))
    if (!is.null(dates) && length(dates) == 2 && all(!is.na(dates))) {
      dd  <- safe_as_date(d$Date)
      sub <- d[!is.na(dd) & dd >= as.Date(dates[1]) & dd <= as.Date(dates[2]), , drop = FALSE]
      if (nrow(sub) > 0) d <- sub
    }
    d
  }

  output$hp_results_trend <- plotly::renderPlotly({
    metric <- input$hp_res_metric %||% "Whiff %"
    fr <- .hp_trend_frame(hp_trend_source(input$hp_res_dates), metric,
                          input$hp_res_split %||% "agg",
                          input$hp_res_trend_side %||% "B")
    .pp_trend_plotly(fr, metric, FALSE, paste0(metric, " by game"))
  })

  hp_results_gt <- function(side) {
    d <- hp_data_f()
    if (side %in% c("R", "L")) d <- d[.hp_phand(d) == side, , drop = FALSE]
    validate(need(nrow(d) > 0, "No pitches for this split."))
    total <- nrow(d)

    res <- d %>%
      dplyr::filter(!is.na(TaggedPitchType), TaggedPitchType != "Other") %>%
      dplyr::group_by(`Pitch` = TaggedPitchType) %>%
      dplyr::group_modify(~ {
        dd <- .x
        tibble::tibble(
          `#`            = nrow(dd),
          Usage          = 100 * nrow(dd) / total,
          `Swing %`      = .hp_calc(dd, "Swing %"),
          `Chase %`      = .hp_calc(dd, "Chase %"),
          `Whiff %`      = .hp_calc(dd, "Whiff %"),
          `Z-Whiff %`    = .hp_calc(dd, "Z-Whiff %"),
          `Contact %`    = .hp_calc(dd, "Contact %"),
          `Avg EV`       = .hp_calc(dd, "Avg EV"),
          `Hard-Hit %`   = .hp_calc(dd, "Hard-Hit %"),
          `Sweet-Spot %` = .hp_calc(dd, "Sweet-Spot %"),
          `AVG`          = .hp_calc(dd, "AVG"),
          `SLG`          = .hp_calc(dd, "SLG"))
      }) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(Usage))

    res %>%
      gt::gt() %>%
      pp_gt_modern() %>%
      gt::fmt_number(columns = c(Usage, `Swing %`, `Chase %`, `Whiff %`, `Z-Whiff %`,
                                 `Contact %`, `Avg EV`, `Hard-Hit %`, `Sweet-Spot %`),
                     decimals = 1) %>%
      gt::fmt_number(columns = c(`AVG`, `SLG`), decimals = 3) %>%
      gt::text_transform(locations = gt::cells_body(columns = `Pitch`),
                         fn = function(v) lapply(v, pp_pitch_chip)) %>%
      gt::sub_missing(missing_text = "\u2014")
  }

  output$hp_results_table_ui <- renderUI({
    g <- hp_results_gt(input$hp_res_side %||% "A")
    div(class = "pp-slide", HTML(as.character(gt::as_raw_html(g))))
  })

  # ==================== HITTER ZONE ====================

  .HP_ZONE_ROWS <- c(all = "All", rhp = "vs RHP", lhp = "vs LHP")

  hp_zone_head <- function(rid) {
    div(class = "pp-row-head",
      div(class = "pp-row-lab", unname(.HP_ZONE_ROWS[[rid]])),
      div(class = "pp-seg",
          radioGroupButtons(paste0("hzone_", rid, "_count"), label = NULL,
            choices = c("All", "0-0", "Pre-2K", "2 Strikes"),
            selected = "All", size = "xs", individual = TRUE)),
      div(class = "pp-seg",
          radioGroupButtons(paste0("hzone_", rid, "_metric"), label = NULL,
            choices = c("Density" = "density", "Swings" = "swing",
                        "Whiffs" = "whiff", "Hard-Hit" = "hardhit",
                        "Solid" = "solid", "xSLG" = "xslg"),
            selected = "density", size = "xs", individual = TRUE))
    )
  }
  output$hp_zone_all_head <- renderUI(hp_zone_head("all"))
  output$hp_zone_rhp_head <- renderUI(hp_zone_head("rhp"))
  output$hp_zone_lhp_head <- renderUI(hp_zone_head("lhp"))

  hp_zone_row_data <- function(rid) {
    d <- hp_data_f()
    if (is.null(d) || !nrow(d)) return(d)
    d <- d[!is.na(d$PlateLocSide) & !is.na(d$PlateLocHeight) &
             !is.na(d$TaggedPitchType) & d$TaggedPitchType != "Other", , drop = FALSE]
    if (identical(rid, "rhp")) d <- d[.hp_phand(d) == "R", , drop = FALSE]
    if (identical(rid, "lhp")) d <- d[.hp_phand(d) == "L", , drop = FALSE]
    if (!nrow(d)) return(d)
    d$CountSplit <- ifelse(d$Balls == 0 & d$Strikes == 0, "0-0",
                    ifelse(d$Strikes == 2, "2 Strikes", "Pre-2K"))
    cnt <- input[[paste0("hzone_", rid, "_count")]] %||% "All"
    if (!identical(cnt, "All"))
      d <- d[!is.na(d$CountSplit) & d$CountSplit == cnt, , drop = FALSE]
    d
  }

  hp_zone_types <- function(rid) {
    d <- hp_zone_row_data(rid)
    if (is.null(d) || !nrow(d)) return(character(0))
    tb <- sort(table(as.character(d$TaggedPitchType)), decreasing = TRUE)
    tb <- tb[tb >= 8]
    utils::head(names(tb), .PP_ZONE_SLOTS)
  }

  hp_zone_strip_ui <- function(rid) {
    types <- hp_zone_types(rid)
    if (!length(types))
      return(div(style = "color:#8A93A0;font-size:12px;padding:8px 2px;",
                 "No pitches for this split."))
    div(class = "pp-zone-strip pp-fade",
      lapply(seq_along(types), function(i) {
        col <- if (!is.na(pitch_colors[types[i]])) unname(pitch_colors[types[i]]) else "#5B6875"
        div(class = "pp-zone-cell",
          plotOutput(paste0("hzone_", rid, "_p", i), height = "230px"),
          div(class = "pp-zone-name", style = paste0("color:", col, ";"), types[i]))
      })
    )
  }
  output$hp_zone_all_strip <- renderUI(hp_zone_strip_ui("all"))
  output$hp_zone_rhp_strip <- renderUI(hp_zone_strip_ui("rhp"))
  output$hp_zone_lhp_strip <- renderUI(hp_zone_strip_ui("lhp"))

  for (.hrid in names(.HP_ZONE_ROWS)) {
    for (.hslot in seq_len(.PP_ZONE_SLOTS)) {
      local({
        rid <- .hrid; ii <- .hslot
        output[[paste0("hzone_", rid, "_p", ii)]] <- renderPlot({
          types <- hp_zone_types(rid)
          req(length(types) >= ii)
          pp_zone_plot(hp_zone_row_data(rid), types[ii],
                       input[[paste0("hzone_", rid, "_metric")]] %||% "density")
        })
      })
    }
  }

  # ==================== BATTED BALL ====================

  HP_EVENT_COLS <- c("1B" = "#E1463E", "2B" = "#F2B705", "3B" = "#CD853F",
                     "HR" = "#00840D", "ROE/FC" = "#FF8C00", "OUT" = "#A9AFB6")

  hp_bip_data <- reactive({
    d <- hp_data_f()
    validate(need(is.data.frame(d) && nrow(d) > 0, "No pitches survive the filters."))
    d <- d[as.character(d$PitchCall) == "InPlay", , drop = FALSE]
    validate(need(nrow(d) > 0, "No balls in play in the current filters."))
    d$event_type <- dplyr::case_when(
      d$PlayResult == "Single"  ~ "1B",
      d$PlayResult == "Double"  ~ "2B",
      d$PlayResult == "Triple"  ~ "3B",
      d$PlayResult == "HomeRun" ~ "HR",
      d$PlayResult %in% c("Error", "FieldersChoice") ~ "ROE/FC",
      TRUE ~ "OUT")
    d
  })

  output$hp_spray_plotly <- plotly::renderPlotly({
    d <- hp_bip_data()
    d <- d[is.finite(suppressWarnings(as.numeric(d$Distance))) &
             is.finite(suppressWarnings(as.numeric(d$Bearing))), , drop = FALSE]
    validate(need(nrow(d) > 0, "No batted balls with distance and bearing."))
    br <- as.numeric(d$Bearing) * pi / 180
    d$x <- as.numeric(d$Distance) * sin(br)
    d$y <- as.numeric(d$Distance) * cos(br)
    d$hover <- paste0("<b>", d$event_type, "</b><br>",
                      "EV: ", ifelse(is.finite(suppressWarnings(as.numeric(d$ExitSpeed))),
                                     paste0(round(as.numeric(d$ExitSpeed), 1), " mph"), "N/A"),
                      "<br>LA: ", ifelse(is.finite(suppressWarnings(as.numeric(d$Angle))),
                                         paste0(round(as.numeric(d$Angle), 1), "\u00b0"), "N/A"),
                      "<br>Dist: ", round(as.numeric(d$Distance)), " ft",
                      "<br>vs ", d$PitcherThrows, "HP")

    th <- seq(-pi/4, pi/4, length.out = 60)
    lineseg <- function(p, dd, w, dash = "solid") {
      plotly::add_trace(p, data = dd, x = ~x, y = ~y, type = "scatter", mode = "lines",
                        line = list(color = "#8A93A0", width = w, dash = dash),
                        showlegend = FALSE, hoverinfo = "none", inherit = FALSE)
    }
    p <- plotly::plot_ly()
    p <- lineseg(p, data.frame(x = c(0, -247.487), y = c(0, 247.487)), 1)
    p <- lineseg(p, data.frame(x = c(0, 247.487),  y = c(0, 247.487)), 1)
    p <- lineseg(p, data.frame(x = c(0, 63.6396, 0, -63.6396, 0),
                               y = c(0, 63.6396, 127.279, 63.6396, 0)), 1)
    p <- lineseg(p, data.frame(x = 350 * sin(th), y = 350 * cos(th)), 0.8)
    p <- lineseg(p, data.frame(x = 160 * sin(th), y = 160 * cos(th)), 0.8, "dot")

    for (evt in names(HP_EVENT_COLS)) {
      sub <- d[d$event_type == evt, , drop = FALSE]
      if (!nrow(sub)) next
      p <- plotly::add_trace(p, data = sub, x = ~x, y = ~y,
        type = "scatter", mode = "markers",
        marker = list(size = 9, color = unname(HP_EVENT_COLS[evt]),
                      line = list(color = "#2B3440", width = 1)),
        text = ~hover, hoverinfo = "text",
        name = paste0(evt, " (", nrow(sub), ")"), inherit = FALSE)
    }
    p %>% plotly::layout(
      autosize = TRUE,
      xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE,
                   showticklabels = FALSE, scaleanchor = "y", scaleratio = 1,
                   range = c(-330, 330)),
      yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE,
                   showticklabels = FALSE, range = c(-20, 420)),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.04,
                    font = list(size = 11)),
      plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF", hovermode = "closest",
      margin = list(t = 10, b = 40, l = 10, r = 10)) %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$hp_radial_ev <- renderPlot({
    d <- hp_bip_data()
    ev <- suppressWarnings(as.numeric(d$ExitSpeed))
    la <- suppressWarnings(as.numeric(d$Angle))
    ok <- is.finite(ev) & is.finite(la)
    validate(need(sum(ok) >= 5, "Not enough batted balls with EV and LA."))
    d <- d[ok, , drop = FALSE]; ev <- ev[ok]; la <- la[ok]
    d$rx <- ev * cos(la * pi / 180)
    d$ry <- ev * sin(la * pi / 180)

    ring <- function(r, col, lty, lw) {
      a <- seq(-pi/2, pi/2, length.out = 120)
      annotate("path", x = r * cos(a), y = r * sin(a),
               color = col, linetype = lty, linewidth = lw)
    }
    barrel <- round(100 * mean(ev >= 95 & la >= 10 & la <= 30), 1)

    ggplot() +
      ring(60, "#D8DFE4", "dashed", 0.3) + ring(80, "#D8DFE4", "dashed", 0.3) +
      ring(95, "#00840D", "solid", 0.6)  + ring(110, "#D8DFE4", "dashed", 0.3) +
      annotate("segment", x = 0, y = 0, xend = 120, yend = 0,
               color = "#B9C3CB", linewidth = 0.4) +
      annotate("segment", x = 0, y = 0, xend = 120 * cos(10 * pi/180),
               yend = 120 * sin(10 * pi/180), color = "#D8DFE4",
               linetype = "dashed", linewidth = 0.3) +
      annotate("segment", x = 0, y = 0, xend = 120 * cos(30 * pi/180),
               yend = 120 * sin(30 * pi/180), color = "#D8DFE4",
               linetype = "dashed", linewidth = 0.3) +
      annotate("text", x = 62, y = -8, label = "60", size = 2.6, color = "#8A93A0") +
      annotate("text", x = 82, y = -8, label = "80", size = 2.6, color = "#8A93A0") +
      annotate("text", x = 97, y = -8, label = "95", size = 2.6,
               color = "#00840D", fontface = "bold") +
      annotate("text", x = 112, y = -8, label = "110", size = 2.6, color = "#8A93A0") +
      annotate("text", x = 116 * cos(10 * pi/180), y = 116 * sin(10 * pi/180),
               label = "10\u00b0", size = 2.6, color = "#8A93A0") +
      annotate("text", x = 116 * cos(30 * pi/180), y = 116 * sin(30 * pi/180),
               label = "30\u00b0", size = 2.6, color = "#8A93A0") +
      geom_point(data = d, aes(rx, ry, fill = event_type), shape = 21,
                 size = 3.4, color = "#2B3440", stroke = 0.4, alpha = 0.85) +
      annotate("label", x = 12, y = 104, hjust = 0, label.size = 0, size = 3.1,
               fill = "#FFFFFF", fontface = "bold",
               label = paste0("Avg EV: ", round(mean(ev), 1),
                              "\nMax EV: ", round(max(ev), 1),
                              "\nAvg LA: ", round(mean(la), 1), "\u00b0",
                              "\nBarrel%: ", barrel, "%")) +
      scale_fill_manual(values = HP_EVENT_COLS, name = NULL) +
      coord_fixed(xlim = c(-20, 125), ylim = c(-35, 122)) +
      theme_void() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 9),
            plot.background = element_rect(fill = "#FFFFFF", color = NA))
  })

  output$hp_sector_spray <- renderPlot({
    d <- hp_bip_data()
    dist <- suppressWarnings(as.numeric(d$Distance))
    bear <- suppressWarnings(as.numeric(d$Bearing))
    la   <- suppressWarnings(as.numeric(d$Angle))
    ok <- is.finite(dist) & is.finite(bear)
    validate(need(sum(ok) > 0, "No batted balls with distance and bearing."))
    d <- d[ok, , drop = FALSE]; dist <- dist[ok]; bear <- bear[ok]; la <- la[ok]

    scope <- input$hp_sector_scope %||% "all"
    if (identical(scope, "gb")) {
      keep <- (is.finite(la) & la < 10) | (dist < 150 & (!is.finite(la) | la < 15))
    } else if (identical(scope, "air")) {
      keep <- is.finite(la) & la >= 10
    } else keep <- rep(TRUE, length(dist))
    validate(need(sum(keep) >= 5, "Not enough batted balls in this bucket."))
    dist <- dist[keep]; bear <- bear[keep]

    abrk <- seq(-45, 45, length.out = 6)
    arad <- abrk * pi / 180
    ai <- cut(bear, breaks = abrk, labels = FALSE, include.lowest = TRUE)
    di <- ifelse(dist < 150, 1L, 2L)
    good <- !is.na(ai) & !is.na(di)
    total <- sum(good)
    validate(need(total > 0, "No batted balls inside the sector grid."))

    stats <- as.data.frame(table(angle_idx = ai[good], dist_idx = di[good]),
                           stringsAsFactors = FALSE)
    names(stats)[3] <- "count"
    stats$angle_idx <- as.integer(stats$angle_idx)
    stats$dist_idx  <- as.integer(stats$dist_idx)
    stats$pct <- round(100 * stats$count / total)

    dbrk <- c(0, 150, 400)
    mk <- function(ri, ro, t0, t1, n = 40) {
      ts <- seq(t0, t1, length.out = n)
      data.frame(x = c(ri * sin(ts), ro * sin(rev(ts))),
                 y = c(ri * cos(ts), ro * cos(rev(ts))))
    }
    polys <- list(); ctr <- list(); k <- 1
    for (i in 1:5) for (j in 1:2) {
      pl <- mk(dbrk[j], dbrk[j+1], arad[i], arad[i+1])
      pl$sid <- k; pl$angle_idx <- i; pl$dist_idx <- j
      polys[[k]] <- pl
      ctr[[k]] <- data.frame(sid = k, angle_idx = i, dist_idx = j,
                             cx = mean(pl$x), cy = mean(pl$y))
      k <- k + 1
    }
    poly_df <- dplyr::bind_rows(polys)
    ctr_df  <- dplyr::bind_rows(ctr) %>%
      dplyr::left_join(stats, by = c("angle_idx", "dist_idx"))
    ctr_df$pct[is.na(ctr_df$pct)] <- 0
    poly_df <- poly_df %>% dplyr::left_join(ctr_df[, c("sid", "pct")], by = "sid")

    ggplot() +
      geom_polygon(data = poly_df, aes(x, y, group = sid, fill = pct),
                   color = "#FFFFFF", linewidth = 0.6) +
      geom_text(data = ctr_df[ctr_df$pct > 0, , drop = FALSE],
                aes(cx, cy, label = paste0(pct, "%")),
                size = 4, fontface = "bold", color = "#243746") +
      annotate("segment", x = 0, y = 0, xend = 247.487, yend = 247.487,
               color = "#3B4754", linewidth = 0.7) +
      annotate("segment", x = 0, y = 0, xend = -247.487, yend = 247.487,
               color = "#3B4754", linewidth = 0.7) +
      annotate("text", x = 0, y = 400, label = paste("Batted balls:", total),
               size = 3.6, fontface = "bold", color = "#5B6875") +
      scale_fill_gradientn(
        colours = c("#FFFFFF", "#E6F2E8", "#B7DCBE", "#7FC08D", "#3F9B57", "#00840D"),
        values = scales::rescale(c(0, 3, 6, 11, 17, 25)),
        limits = c(0, NA), na.value = "#FFFFFF", name = "% of BIP") +
      coord_fixed(xlim = c(-300, 300), ylim = c(-20, 415)) +
      theme_void() +
      theme(legend.position = "right",
            plot.background = element_rect(fill = "#FFFFFF", color = NA))
  })

  output$hp_contact_map <- renderPlot({
    d <- hp_data_f()
    need_cols <- c("ExitSpeed", "ContactPositionX", "ContactPositionY", "ContactPositionZ")
    validate(need(all(need_cols %in% names(d)),
                  "Contact position columns are not available in this data."))
    d <- d %>%
      dplyr::filter(!is.na(ExitSpeed), !is.na(ContactPositionX),
                    !is.na(ContactPositionY), !is.na(ContactPositionZ),
                    !is.na(BatterSide)) %>%
      dplyr::mutate(cx = ContactPositionX * 12, cz = ContactPositionZ * 12)
    validate(need(nrow(d) > 0, "No tracked contact positions."))

    ggplot(d, aes(x = cz, y = cx)) +
      annotate("segment", x = -8.5, y = 17, xend = 8.5, yend = 17, color = "#3B4754") +
      annotate("segment", x = 8.5, y = 8.5, xend = 8.5, yend = 17, color = "#3B4754") +
      annotate("segment", x = -8.5, y = 8.5, xend = -8.5, yend = 17, color = "#3B4754") +
      annotate("segment", x = -8.5, y = 8.5, xend = 0, yend = 0, color = "#3B4754") +
      annotate("segment", x = 8.5, y = 8.5, xend = 0, yend = 0, color = "#3B4754") +
      annotate("rect", xmin = 20, xmax = 48, ymin = -20, ymax = 40,
               fill = NA, color = "#C9D0D8") +
      annotate("rect", xmin = -48, xmax = -20, ymin = -20, ymax = 40,
               fill = NA, color = "#C9D0D8") +
      geom_point(aes(fill = ExitSpeed), color = "#2B3440", stroke = 0.4,
                 shape = 21, alpha = 0.85, size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "#152535", linewidth = 0.7) +
      scale_fill_gradient(name = "Exit Velo", low = "#E1463E", high = "#00840D") +
      coord_fixed(xlim = c(-50, 50), ylim = c(-20, 50)) +
      facet_wrap(~ BatterSide) +
      theme_void() +
      theme(legend.position = "right",
            strip.text = element_text(size = 11, face = "bold", color = "#0E6E70"),
            plot.background = element_rect(fill = "#FFFFFF", color = NA))
  })

  # Launch-angle bucket x spray direction, hitter minus pool rate.
  output$hp_bb_profile <- renderPlot({
    d <- hp_bip_data()
    la <- suppressWarnings(as.numeric(d$Angle))
    br <- suppressWarnings(as.numeric(d$Bearing))
    sd_ <- as.character(d$BatterSide)
    ok <- is.finite(la) & is.finite(br)
    validate(need(sum(ok) >= 15, "Not enough batted balls with launch angle and bearing."))
    la <- la[ok]; br <- br[ok]; sd_ <- sd_[ok]

    # pull = negative bearing for a RHH, positive for a LHH
    lefty <- toupper(substr(sd_, 1, 1)) == "L"
    pull_axis <- ifelse(lefty, -br, br)   # negative = pull, positive = oppo

    bucket <- cut(la, breaks = c(-90, 10, 25, 50, 90),
                  labels = c("Ground Ball", "Line Drive", "Fly Ball", "Pop Up"))
    field <- cut(pull_axis, breaks = c(-90, -15, 15, 90),
                 labels = c("Pull", "Center", "Oppo"))
    keep <- !is.na(bucket) & !is.na(field)
    bucket <- bucket[keep]; field <- field[keep]
    n <- length(bucket)
    validate(need(n >= 15, "Not enough classified batted balls."))

    grid <- expand.grid(bucket = levels(bucket), field = levels(field),
                        stringsAsFactors = FALSE)
    tab <- as.data.frame(table(bucket = bucket, field = field), stringsAsFactors = FALSE)
    names(tab)[3] <- "count"
    grid <- dplyr::left_join(grid, tab, by = c("bucket", "field"))
    grid$count[is.na(grid$count)] <- 0
    grid$pct <- 100 * grid$count / n

    # reference shares (college batted-ball baseline)
    ref <- c("Ground Ball" = 0.44, "Line Drive" = 0.24,
             "Fly Ball" = 0.26, "Pop Up" = 0.06)
    refF <- c("Pull" = 0.38, "Center" = 0.33, "Oppo" = 0.29)
    grid$exp <- 100 * unname(ref[grid$bucket]) * unname(refF[grid$field])
    grid$diff <- grid$pct - grid$exp

    grid$bucket <- factor(grid$bucket,
      levels = c("Pop Up", "Fly Ball", "Line Drive", "Ground Ball"))
    grid$field <- factor(grid$field, levels = c("Oppo", "Center", "Pull"))

    lim <- max(abs(grid$diff), 1)
    ggplot(grid, aes(field, bucket, fill = diff)) +
      geom_tile(color = "#FFFFFF", linewidth = 2) +
      geom_label(aes(label = sprintf("%.1f%%", pct)), fill = "#FFFFFF",
                 label.size = 0, size = 4, fontface = "bold", color = "#243746") +
      scale_fill_gradientn(
        colours = c("#C0392B", "#E8B0AA", "#FFFFFF", "#A8D5B0", "#00840D"),
        limits = c(-lim, lim), name = "vs pool") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(panel.grid = element_blank(),
            axis.text = element_text(face = "bold", size = 12, color = "#243746"),
            plot.background = element_rect(fill = "#FFFFFF", color = NA),
            legend.position = "right")
  })

  # ==================== HITTER MODELING ====================

  output$hp_rolling_process <- renderPlot({
    d <- hp_data_f()
    validate(need("hp_dv" %in% names(d), "Hitter model values are not available."))
    dates <- input$hp_mod_dates
    if (!is.null(dates) && length(dates) == 2 && all(!is.na(dates))) {
      dd  <- safe_as_date(d$Date)
      sub <- d[!is.na(dd) & dd >= as.Date(dates[1]) & dd <= as.Date(dates[2]), , drop = FALSE]
      if (nrow(sub) > 0) d <- sub
    }
    side <- input$hp_mod_side %||% "B"
    if (side %in% c("R", "L")) d <- d[.hp_phand(d) == side, , drop = FALSE]

    d <- d[is.finite(d$hp_dv), , drop = FALSE]
    d <- d[order(safe_as_date(d$Date)), , drop = FALSE]
    w <- input$hp_roll_window %||% 250
    validate(need(nrow(d) >= w, paste0("Need at least ", w,
                                       " scored pitches for this window (have ",
                                       nrow(d), ").")))

    roll <- function(v, w) {
      v[!is.finite(v)] <- 0
      cs <- cumsum(c(0, v))
      (cs[(w + 1):length(cs)] - cs[1:(length(cs) - w)]) / w
    }
    idx <- w:nrow(d)
    dts <- safe_as_date(d$Date)[idx]

    # per-pitch run values -> plus scale using the league fit
    to_plus <- function(v, key) vapply(100 * v, function(z) hp_plus(z, key), numeric(1))
    dv <- to_plus(roll(d$hp_dv, w), "dv")
    cv <- to_plus(roll(d$hp_cv, w), "cv")
    pv <- to_plus(roll(d$hp_pv, w), "pv")
    pr <- to_plus(roll(d$hp_rv, w), "prc")
    validate(need(any(is.finite(pr)), "League baseline unavailable for the plus scale."))

    long <- rbind(
      data.frame(date = dts, comp = "Decisions", val = dv - 100),
      data.frame(date = dts, comp = "Contact",   val = cv - 100),
      data.frame(date = dts, comp = "Power",     val = pv - 100))
    long$comp <- factor(long$comp, levels = c("Decisions", "Contact", "Power"))
    line <- data.frame(date = dts, val = pr)

    ggplot() +
      geom_col(data = long, aes(date, val, fill = comp), width = 1.05) +
      geom_hline(yintercept = 0, color = "#243746", linewidth = 0.5) +
      geom_line(data = line, aes(date, val - 100), color = "#152535", linewidth = 1.1) +
      scale_fill_manual(values = c(Decisions = "#9B7BD4", Contact = "#F07818",
                                   Power = "#00840D"), name = NULL) +
      scale_y_continuous(labels = function(v) v + 100,
                         name = "Process+ (100 = average)") +
      labs(x = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top",
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "#EEF2F4"),
            plot.background = element_rect(fill = "#FFFFFF", color = NA),
            axis.title.y = element_text(size = 11, color = "#5B6875"),
            axis.text = element_text(color = "#77828F"))
  })

  hp_model_gt <- function(side) {
    s <- hp_summary()
    blk <- switch(side, "R" = s$vs_r, "L" = s$vs_l, s$overall)
    validate(need(!is.null(blk), "Not enough scoreable pitches for this split."))
    tb <- tibble::tibble(
      Model = c("Decision+", "Contact+", "Power+", "Process+"),
      Value = c(blk$plus$dv, blk$plus$cv, blk$plus$pv, blk$plus$prc),
      Pctl  = c(blk$pct$dv,  blk$pct$cv,  blk$pct$pv,  blk$pct$prc))
    pct <- tb$Pctl
    tb %>%
      gt::gt() %>%
      pp_gt_modern() %>%
      gt::fmt_number(columns = c(Value, Pctl), decimals = 0) %>%
      gt::text_transform(gt::cells_body(columns = Value),
        fn = function(v) mapply(pp_pill, v, pct / 100, SIMPLIFY = FALSE)) %>%
      gt::tab_style(style = gt::cell_text(weight = "bold"),
                    locations = gt::cells_body(columns = Model)) %>%
      gt::sub_missing(missing_text = "\u2014")
  }

  output$hp_model_tbl_all <- gt::render_gt(hp_model_gt("A"))
  output$hp_model_tbl_r   <- gt::render_gt(hp_model_gt("R"))
  output$hp_model_tbl_l   <- gt::render_gt(hp_model_gt("L"))

  # ---- Viz tab: created / lost heatmaps per aspect ----
  hp_viz_data <- reactive({
    df <- hp_data_f()
    hand <- input$hp_viz_hand %||% "All"
    if (hand != "All" && "PitcherThrows" %in% names(df)) {
      h <- if (identical(hand, "vs RHP")) "R" else "L"
      df <- df[toupper(substr(trimws(as.character(df$PitcherThrows)), 1, 1)) == h, ,
               drop = FALSE]
    }
    df
  })
  hp_viz_mode <- reactive({
    switch(input$hp_viz_layer %||% "Net",
           "Runs Created" = "created",
           "Runs Lost"    = "lost",
           "net")
  })
  .hp_heat_out <- function(col, title) {
    renderPlot({
      hp_value_heatmap(hp_viz_data(), col, title, mode = hp_viz_mode())
    })
  }
  output$hp_heat_rv <- .hp_heat_out("hp_rv", "Overall Value")
  output$hp_heat_dv <- .hp_heat_out("hp_dv", "Decisions")
  output$hp_heat_cv <- .hp_heat_out("hp_cv", "Contact")
  output$hp_heat_pv <- .hp_heat_out("hp_pv", "Power")

  # ---- Filter sidebar context: adapt the pitcher filters to the hitter ----
  # On the Hitters page the Games picker lists games by pitches FACED,
  # the hand picker relabels to Pitcher Hand, and the date range /
  # pitch-type domain widen to the hitter's data — mirroring exactly
  # what the NCAA pitcher sync does for arms.
  .hp_filter_ctx <- reactiveVal(NULL)   # last hitter+season the filters were synced to
  observeEvent(list(input$hp_batter, player_mode(), input$season_type), {
    if (!identical(player_mode(), "hitter")) return()
    nm <- input$hp_batter
    if (is.null(nm) || !nzchar(nm)) return()
    ctx_key <- paste0(nm, "::", .hp_season_key())

    updatePickerInput(session, "g_BatterSide", label = "Pitcher Hand")

    df <- tryCatch(hp_data(), error = function(e) NULL)
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      updatePickerInput(session, "game_filter_spring26",
                        label = "Games (faced)",
                        choices = character(0), selected = character(0))
      return()
    }

    # rebuild the domain only when the hitter or season actually changes
    if (!identical(.hp_filter_ctx(), ctx_key)) {
      .hp_filter_ctx(ctx_key)
      if ("Date" %in% names(df)) {
        rng <- suppressWarnings(range(as.Date(df$Date), na.rm = TRUE))
        if (all(is.finite(rng))) {
          updateDateRangeInput(session, "g_dateRange",
                               start = rng[1], end = rng[2])
        }
      }
      base_pts <- c("Fastball","Sinker","Cutter","Curveball","Slider",
                    "ChangeUp","Splitter","Sweeper")
      pts <- sort(unique(as.character(df$TaggedPitchType)))
      pts <- pts[!is.na(pts) & nzchar(pts) & pts != "Other"]
      all_pts <- union(base_pts, pts)
      updatePickerInput(session, "g_pitchtype",
                        choices = all_pts, selected = all_pts)
    }

    # Games faced: one row per game — date, opposing (pitching) team,
    # pitches faced.
    if (!"GameID" %in% names(df)) return()
    pdat <- df[!is.na(df$Date), , drop = FALSE]
    if (nrow(pdat) == 0) return()
    opp_of <- function(tt) {
      tt <- tt[!is.na(tt) & nzchar(tt)]
      if (length(tt) == 0) return("Unknown")
      top <- names(sort(table(tt), decreasing = TRUE))[1]
      pt <- tryCatch(prettify_team(top), error = function(e) top)
      if (is.na(pt) || !nzchar(pt)) top else pt
    }
    games <- pdat %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(
        Date = suppressWarnings(max(as.Date(Date), na.rm = TRUE)),
        Opp  = opp_of(if ("PitcherTeam" %in% names(pdat)) PitcherTeam
                      else character(0)),
        N    = dplyr::n(),
        .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(Date))
    labs <- paste0(format(games$Date, "%b %d, %Y"), " \u2014 vs ", games$Opp,
                   "  (", games$N, " pitches faced)")
    keep <- intersect(isolate(input$game_filter_spring26) %||% character(0),
                      games$GameID)
    updatePickerInput(session, "game_filter_spring26",
                      label = "Games (faced)",
                      choices = setNames(games$GameID, labs), selected = keep)
    session$sendCustomMessage("setPitchedDates",
                              as.list(sort(unique(as.character(pdat$Date)))))
  }, ignoreInit = FALSE)

  # Leaving the Hitters page hands the picker label back to the pitcher side.
  observeEvent(player_mode(), {
    if (!identical(player_mode(), "hitter")) {
      updatePickerInput(session, "g_BatterSide", label = "Batter Hand")
      .hp_filter_ctx(NULL)   # re-sync domain on the next hitter visit
    }
  })
