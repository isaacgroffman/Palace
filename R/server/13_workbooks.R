  # ===================== ADVANCE: SCOUTING WORKBOOK =====================
  .swb_key <- function(x) gsub("[^A-Za-z0-9]", "_", x)
  .SWB_ROLES <- c("SP1", "SP2", "SP3", "SP4", "RP", "RP - High Lev",
                  "RP - Long", "Closer")
  .swb_sheet_name <- function(role, nm, used) {
    base <- gsub("[\\[\\]\\*/\\\\?:]", "", paste(role, "-", nm))
    base <- trimws(substr(base, 1, 28))
    out <- base; k <- 2
    while (out %in% used) { out <- paste0(substr(base, 1, 25), " ", k); k <- k + 1 }
    out
  }

  # Staff loads from TruMedia PlayerTotals (the 2026 roster with IP/ERA/SV),
  # matched to DRS26.csv for jersey/class/position bio. Directory fallback
  # when the API is unreachable.
  swb_staff <- reactiveVal(NULL)   # data.frame: name, ip, era, sv, src

  # TruMedia PlayerTotals names ("First Last", sometimes with middle
  # names/suffixes) don't always match the NCAA directory display names
  # that load_ncaa_pitcher_data() keys on — the same loader the Scouting
  # tab uses. Resolve each roster name to its directory display so the
  # workbook loads pitch data (and picks up saved notes) exactly like
  # the single-pitcher report does.
  .swb_resolve_display <- function(name, team_disp) {
    if (is.null(name) || !nzchar(name)) return(name)
    if (isTRUE(is_ncaa_pitcher(name))) return(name)   # already a directory key
    cand <- tryCatch(
      unique(ncaa_dir_pairs$display[!is.na(ncaa_dir_pairs$team_disp) &
                                      ncaa_dir_pairs$team_disp == team_disp]),
      error = function(e) character(0))
    if (length(cand) == 0) return(name)
    kn <- norm_player_key(name)
    kc <- norm_player_key(cand)
    hit <- cand[!is.na(kc) & kc == kn]
    if (length(hit) > 0) return(hit[1])
    parts <- strsplit(kn, " ", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      last <- parts[length(parts)]; fi <- substr(parts[1], 1, 1)
      hit <- cand[!is.na(kc) & endsWith(kc, paste0(" ", last)) &
                    substr(kc, 1, 1) == fi]
      if (length(hit) > 0) return(hit[1])
    }
    name
  }

  observeEvent(input$swb_load, {
    req(input$swb_team, nzchar(input$swb_team))
    staff_df <- withProgress(message = "Loading 2026 staff from TruMedia...", value = 0.3, {
      out <- NULL
      tid <- tryCatch(tm_find_team_id(input$swb_team), error = function(e) NULL)
      tot <- if (!is.null(tid))
        tryCatch(tm_team_player_totals(tid, "overall", TM_TRAD_COLS),
                 error = function(e) NULL) else NULL
      if (!is.null(tot) && nrow(tot) > 0) {
        nm_col <- .tm_pick_col(tot, c("playerName", "playerFullName", "^player$", "name"))
        if (!is.null(nm_col)) {
          ip  <- vapply(seq_len(nrow(tot)), function(i)
            suppressWarnings(as.numeric(tm_stat(tot[i, , drop = FALSE], "IP"))), numeric(1))
          era <- vapply(seq_len(nrow(tot)), function(i)
            suppressWarnings(as.numeric(tm_stat(tot[i, , drop = FALSE], "ERA"))), numeric(1))
          sv  <- vapply(seq_len(nrow(tot)), function(i)
            suppressWarnings(as.numeric(tm_stat(tot[i, , drop = FALSE], "SV"))), numeric(1))
          keep <- !is.na(ip) & ip > 0
          if (any(keep)) {
            out <- data.frame(name = as.character(tot[[nm_col]])[keep],
                              ip = ip[keep], era = era[keep], sv = sv[keep],
                              src = "TruMedia", stringsAsFactors = FALSE)
            out <- out[!is.na(out$name) & nzchar(out$name), , drop = FALSE]
            out <- out[order(-out$ip), , drop = FALSE]   # workhorses first
          }
        }
      }
      if (is.null(out) || nrow(out) == 0) {
        # directory fallback (no season stats available)
        nms <- tryCatch(
          ncaa_dir_pairs %>%
            dplyr::filter(team_disp == input$swb_team) %>%
            dplyr::distinct(display) %>% dplyr::arrange(display) %>%
            dplyr::pull(display),
          error = function(e) character(0))
        if (length(nms) > 0)
          out <- data.frame(name = nms, ip = NA_real_, era = NA_real_,
                            sv = NA_real_, src = "directory",
                            stringsAsFactors = FALSE)
      }
      out
    })
    if (is.null(staff_df) || nrow(staff_df) == 0) {
      showNotification(paste("No 2026 staff found for", input$swb_team,
                             "on TruMedia or in the directory."),
                       type = "warning", duration = 6)
      return()
    }
    # map TruMedia roster names onto directory display names so the
    # per-pitcher data load works exactly like the Scouting tab
    staff_df$name <- vapply(staff_df$name, .swb_resolve_display,
                            character(1), team_disp = input$swb_team,
                            USE.NAMES = FALSE)
    n_unres <- sum(!vapply(staff_df$name, is_ncaa_pitcher, logical(1)))
    staff_df <- staff_df[!duplicated(staff_df$name), , drop = FALSE]
    if (n_unres > 0)
      cat("[SWB]", n_unres, "roster name(s) not matched to the NCAA directory",
          "for", input$swb_team, "- their sheets may come back empty\n")
    swb_staff(staff_df)
    updateSelectizeInput(session, "swb_order", choices = staff_df$name,
                         selected = staff_df$name, server = FALSE)
    showNotification(paste0(nrow(staff_df), " arms loaded for ", input$swb_team,
                            if (identical(staff_df$src[1], "TruMedia"))
                              " (TruMedia 2026 roster)" else
                              " (directory fallback — no season stats)",
                            ". Drag to reorder, \u00d7 to drop."),
                     type = "message", duration = 6)
  })

  # row-level remove button -> drop from the order selectize
  observeEvent(input$swb_remove_click, {
    nm <- input$swb_remove_click$name
    cur <- input$swb_order
    if (is.null(nm) || is.null(cur)) return()
    updateSelectizeInput(session, "swb_order",
                         selected = setdiff(cur, nm), server = FALSE)
  })

  output$swb_staff_ui <- renderUI({
    arms <- input$swb_order
    if (is.null(arms) || length(arms) == 0) {
      return(div(class = "adv-placeholder", style = "margin:8px 0; padding:18px 22px;",
                 p(style = "margin:0;",
                   "Load a staff to set roles. Saved Scouting-tab notes print ",
                   "on each pitcher's card automatically.")))
    }
    sdf <- swb_staff()
    rows <- lapply(seq_along(arms), function(i) {
      p <- arms[i]
      d <- drs_info(p, input$swb_team)
      srow <- if (!is.null(sdf)) sdf[sdf$name == p, , drop = FALSE] else NULL
      stat_bits <- character(0)
      if (!is.null(srow) && nrow(srow) == 1) {
        if (!is.na(srow$ip))  stat_bits <- c(stat_bits, paste0(srow$ip, " IP"))
        if (!is.na(srow$era)) stat_bits <- c(stat_bits, paste0(sprintf("%.2f", srow$era), " ERA"))
        if (!is.na(srow$sv) && srow$sv > 0) stat_bits <- c(stat_bits, paste0(round(srow$sv), " SV"))
      }
      bio_bits <- character(0)
      if (nzchar(d$jersey)) bio_bits <- c(bio_bits, paste0("#", d$jersey))
      if (nzchar(d$class))  bio_bits <- c(bio_bits, d$class)
      if (nzchar(d$pos))    bio_bits <- c(bio_bits, d$pos)
      bio_line <- paste(c(if (length(bio_bits)) paste(bio_bits, collapse = " \u00B7 "),
                          if (length(stat_bits)) paste(stat_bits, collapse = " \u00B7 ")),
                        collapse = "  \u2014  ")
      if (!nzchar(bio_line)) bio_line <- "No DRS bio on file"
      rid <- paste0("swb_role_", .swb_key(p))
      div(class = "swb-row",
          div(class = "swb-ord", i),
          div(class = "swb-info",
              div(class = "swb-name", p),
              div(class = "swb-bio", bio_line)),
          div(class = "swb-role",
              selectInput(rid, NULL, choices = .SWB_ROLES,
                          selected = isolate(input[[rid]]) %||%
                            if (i <= 3) paste0("SP", i) else "RP",
                          width = "140px")),
          tags$a(class = "swb-remove", `data-name` = p, href = "#",
                 title = "Remove from workbook", HTML("&times;")))
    })
    tagList(rows)
  })

  # ======================= TEAM HITTER REPORTS =========================
  # Mirrors the Scouting Workbook (swb_*) on the hitter side, then adds the
  # three matchup-matrix pages split by pitcher role and rest.
  .thr_key <- function(x) gsub("[^A-Za-z0-9]", "_", x)

  .mm_lineup_open <- reactiveVal(FALSE)
  observeEvent(input$mm_toggle_lineup, {
    .mm_lineup_open(!isTRUE(.mm_lineup_open()))
  }, ignoreInit = TRUE)
  output$mm_lineup_open <- reactive({ isTRUE(.mm_lineup_open()) })
  outputOptions(output, "mm_lineup_open", suspendWhenHidden = FALSE)

  # The matrix already owns the lineup (input$mm_batters, drag-ordered) and
  # the staff (input$mm_pitchers). These read straight off it instead of
  # keeping a parallel roster, so there is one source of truth per page.
  thr_team_disp <- reactive({
    tk <- input$mm_batter_team
    if (is.null(tk) || !nzchar(tk)) return(NULL)
    tryCatch(.mm_team_display(.mm_team_label(tk)), error = function(e) NULL)
  })
  thr_pitch_team_disp <- reactive({
    tk <- input$mm_pitcher_team
    if (is.null(tk) || !nzchar(tk)) return(NULL)
    tryCatch(.mm_team_display(.mm_team_label(tk)), error = function(e) NULL)
  })

  # Hitter bio, resolved exactly like the hitter Overview page does it:
  # the team hint comes from tm_current_batter_team(), NOT from the raw
  # picker string. drs_lookup uses that hint to break the ~2,600 shared
  # name keys in DRS26 -- without it a common name silently misses and the
  # card falls back to "No DRS bio on file".
  thr_bio <- function(nm, fallback_team = NULL) {
    if (is.null(nm) || !nzchar(nm)) return(list(jersey = "", pos = "", class = "", overall = NA))
    team <- tryCatch(tm_current_batter_team(nm), error = function(e) NULL)
    # character(0) is a real return here; length-check before nzchar or the
    # if() gets a zero-length condition and errors.
    if (length(team) != 1 || is.na(team) || !nzchar(team)) team <- fallback_team
    if (length(team) != 1 || is.na(team) || !nzchar(team)) team <- NULL
    d <- tryCatch(drs_info(nm, team), error = function(e) NULL)
    if (is.null(d)) return(list(jersey = "", pos = "", class = "", overall = NA))
    d
  }

  # Season slash line off the same TruMedia PlayerTotals call the roster
  # loader already makes, keyed on the normalised name so the resolved
  # pool spelling still finds its stat row.
  .thr_stat_lookup <- function(team) {
    out <- NULL
    tid <- tryCatch(tm_find_team_id(team), error = function(e) NULL)
    if (is.null(tid)) return(NULL)
    tot <- tryCatch(tm_team_player_totals(tid, "overall", TM_BAT_COLS),
                    error = function(e) NULL)
    if (is.null(tot) || !nrow(tot)) return(NULL)
    nmc <- .tm_pick_col(tot, c("playerName", "playerFullName", "^player$", "name"))
    if (is.null(nmc)) return(NULL)
    gv <- function(i, ab) suppressWarnings(as.numeric(
      tm_stat(tot[i, , drop = FALSE], ab)))
    out <- data.frame(
      key = norm_player_key(as.character(tot[[nmc]])),
      pa  = vapply(seq_len(nrow(tot)), gv, numeric(1), ab = "PA"),
      ba  = vapply(seq_len(nrow(tot)), gv, numeric(1), ab = "BA"),
      obp = vapply(seq_len(nrow(tot)), gv, numeric(1), ab = "OBP"),
      slg = vapply(seq_len(nrow(tot)), gv, numeric(1), ab = "SLG"),
      hr  = vapply(seq_len(nrow(tot)), gv, numeric(1), ab = "HR"),
      sb  = vapply(seq_len(nrow(tot)), gv, numeric(1), ab = "SB"),
      stringsAsFactors = FALSE)
    out[!duplicated(out$key), , drop = FALSE]
  }

  .thr_stat_line <- function(nm, sdf) {
    if (is.null(sdf) || !nrow(sdf)) return("")
    i <- match(norm_player_key(nm), sdf$key)
    if (is.na(i)) return("")
    f3 <- function(x) if (is.finite(x)) sub("^0", "", sprintf("%.3f", x)) else NA
    bits <- character(0)
    if (all(is.finite(c(sdf$ba[i], sdf$obp[i], sdf$slg[i]))))
      bits <- c(bits, paste(f3(sdf$ba[i]), f3(sdf$obp[i]), f3(sdf$slg[i]), sep = "/"))
    if (is.finite(sdf$hr[i]) && sdf$hr[i] > 0) bits <- c(bits, paste0(round(sdf$hr[i]), " HR"))
    if (is.finite(sdf$sb[i]) && sdf$sb[i] > 0) bits <- c(bits, paste0(round(sdf$sb[i]), " SB"))
    if (is.finite(sdf$pa[i])) bits <- c(bits, paste0(round(sdf$pa[i]), " PA"))
    paste(bits, collapse = " \u00B7 ")
  }

  thr_stats <- reactive({
    td <- thr_team_disp()
    if (is.null(td)) return(NULL)
    .thr_stat_lookup(td)
  })

  # Rest classification off the matrix's own pitcher selection.
  thr_staff <- reactive({
    arms <- input$mm_pitchers
    if (is.null(arms) || !length(arms)) return(NULL)
    pool <- tryCatch(load_matchup_team_pool(input$mm_pitcher_team),
                     error = function(e) NULL)
    thr_reliever_rest(pool, as.character(arms),
                      input$thr_game_date %||% Sys.Date(),
                      input$thr_rested_days %||% 3,
                      input$thr_nonrested_days %||% 2)
  })
  thr_staff  <- reactiveVal(NULL)   # data.frame: pitcher, gs, ip, role, rest

    # Re-classify rest when the date or thresholds move — no reload needed.
      # ---- lineup card: bio + basic stats before you generate anything ----
    output$mm_rest_note <- renderUI({
    st <- thr_staff()
    if (is.null(st) || !nrow(st))
      return(div(style = "color:#6B7280; font-size:12px;",
                 "Pick pitchers above to classify the bullpen by rest."))
    cnt <- function(k) sum(st$rest_class == k, na.rm = TRUE)
    div(style = "color:#6B7280; font-size:12px;",
        sprintf("Bullpen: %s rested \u00b7 %s non-rested \u00b7 %s unknown",
                cnt("Rested"), cnt("Non-Rested"), cnt("Unknown")))
  })

  output$thr_staff_ui <- renderUI({
    st <- thr_staff()
    if (is.null(st) || !nrow(st)) return(NULL)
    mk <- function(cls, label, colr) {
      sub <- st[st$rest_class == cls, , drop = FALSE]
      if (!nrow(sub)) return(NULL)
      div(style = "margin:6px 0;",
          tags$span(label, style = paste0(
            "font-size:11px; font-weight:800; letter-spacing:.06em; color:#fff;",
            "background:", colr, "; padding:3px 9px; border-radius:12px;")),
          tags$span(style = "margin-left:8px; color:#374151; font-size:13px;",
                    paste(sub$pitcher, collapse = ", ")))
    }
    div(style = "margin-top:10px; padding:12px 16px; background:#F7FAFA; border-radius:10px;",
        div(style = "font-weight:700; color:#0E6E70; margin-bottom:6px;",
            "Bullpen by rest (drives Pages 4 and 5)"),
        mk("Rested", "RESTED", "#0E6E70"),
        mk("Non-Rested", "NON-RESTED", "#C0392B"),
        mk("Unknown", "UNKNOWN", "#6B7280"))
  })

    # ---- workbook -------------------------------------------------------
  .THR_COLS <- c(
    "#", "Hitter", "Jersey", "Pos", "B", "Season Line",
    "vs LHP", "vs RHP", "Contact", "Power", "Decisions",
    "SB Att", "Bunts",
    "Miss Zone vs RHP", "FB Miss vs RHP",
    "Miss Zone vs LHP", "FB Miss vs LHP",
    "Pitch-Type Weakness", "Trait Weak vs LHP", "Trait Weak vs RHP",
    "1P Agg", "1P Swing%", "1P Pitches",
    "2K Agg", "2K Swing%", "2K Pitches",
    "Pos vs LHP (P/C/O)", "Pos vs RHP (P/C/O)",
    "Pos Pre-2K", "Pos 2K", "Notes")

  .thr_write_hitters <- function(wb, sheet, names_vec, team, title, sdf = NULL) {
    # best-effort ticks: works inside withProgress, silent outside
    setProgress <- function(...) tryCatch(shiny::setProgress(...),
                                          error = function(e) invisible())
    openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
    S <- openxlsx::createStyle
    st_title <- S(fgFill = "#0E6E70", fontColour = "#FFFFFF", textDecoration = "bold",
                  halign = "center", valign = "center", fontSize = 15)
    st_hdr <- S(fgFill = "#F2F6F6", textDecoration = "bold", halign = "center",
                valign = "center", fontSize = 10, wrapText = TRUE,
                border = "TopBottomLeftRight", borderColour = "#B7C4C4")
    st_cell <- S(halign = "center", valign = "center", fontSize = 11,
                 border = "TopBottomLeftRight", borderColour = "#D5DDDD")
    st_name <- S(halign = "left", valign = "center", fontSize = 11,
                 textDecoration = "bold",
                 border = "TopBottomLeftRight", borderColour = "#D5DDDD")

    nc <- length(.THR_COLS)
    openxlsx::writeData(wb, sheet, title, startRow = 1, startCol = 1)
    openxlsx::mergeCells(wb, sheet, cols = 1:nc, rows = 1)
    openxlsx::addStyle(wb, sheet, st_title, rows = 1, cols = 1:nc, gridExpand = TRUE)
    openxlsx::setRowHeights(wb, sheet, rows = 1, heights = 26)

    openxlsx::writeData(wb, sheet, t(.THR_COLS), startRow = 3, startCol = 1,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet, st_hdr, rows = 3, cols = 1:nc, gridExpand = TRUE)
    openxlsx::setRowHeights(wb, sheet, rows = 3, heights = 42)

    if (length(names_vec) == 0) {
      openxlsx::writeData(wb, sheet, "No hitters in this group.",
                          startRow = 4, startCol = 2)
      return(invisible(NULL))
    }

    for (i in seq_along(names_vec)) {
      nm <- names_vec[i]
      setProgress(detail = paste0("Page card \u2014 ", nm))
      df <- tryCatch(load_ncaa_batter_data(nm, team), error = function(e) NULL)
      bio <- thr_bio(nm, team)
      r <- thr_hitter_row(df, nm, bio)
      slash <- .thr_stat_line(nm, sdf)
      grd <- function(x) if (is.finite(x)) round(x) else "-"
      txt <- function(x) if (is.null(x) || !nzchar(as.character(x))) "-" else x
      vals <- list(
        i, r$Hitter, txt(r$Jersey), txt(r$Pos), txt(r$Side), txt(slash),
        grd(r$vsLHP), grd(r$vsRHP), grd(r$Contact), grd(r$Power), grd(r$Decisions),
        if (is.finite(r$SBA)) r$SBA else "-", if (is.finite(r$Bunts)) r$Bunts else "-",
        txt(r$MissZoneR), txt(r$MissFBZoneR),
        txt(r$MissZoneL), txt(r$MissFBZoneL),
        txt(r$PTWeak), txt(r$TraitWeakL), txt(r$TraitWeakR),
        txt(r$Agg1P), grd(r$Swing1P), if (is.finite(r$Pitches1P)) r$Pitches1P else "-",
        txt(r$Agg2K), grd(r$Swing2K), if (is.finite(r$Pitches2K)) r$Pitches2K else "-",
        txt(r$PosL), txt(r$PosR), txt(r$PosPre2K), txt(r$Pos2K), "")
      rw <- 3 + i
      openxlsx::writeData(wb, sheet, t(unlist(lapply(vals, as.character))),
                          startRow = rw, startCol = 1, colNames = FALSE)
      openxlsx::addStyle(wb, sheet, st_cell, rows = rw, cols = 1:nc, gridExpand = TRUE)
      openxlsx::addStyle(wb, sheet, st_name, rows = rw, cols = 2)
      openxlsx::setRowHeights(wb, sheet, rows = rw, heights = 26)
    }

    # In-game scoring / notes block underneath, with a batting-order slot.
    base <- 3 + length(names_vec) + 2
    openxlsx::writeData(wb, sheet, "IN-GAME SCORING & NOTES",
                        startRow = base, startCol = 1)
    openxlsx::mergeCells(wb, sheet, cols = 1:nc, rows = base)
    openxlsx::addStyle(wb, sheet, st_title, rows = base, cols = 1:nc, gridExpand = TRUE)
    hdr2 <- c("Order", "Hitter", "AB 1", "AB 2", "AB 3", "AB 4", "AB 5",
              "In-Game Notes")
    openxlsx::writeData(wb, sheet, t(hdr2), startRow = base + 1, startCol = 1,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet, st_hdr, rows = base + 1,
                       cols = seq_along(hdr2), gridExpand = TRUE)
    for (i in seq_along(names_vec)) {
      rw <- base + 1 + i
      openxlsx::writeData(wb, sheet, t(c(i, names_vec[i], "", "", "", "", "", "")),
                          startRow = rw, startCol = 1, colNames = FALSE)
      openxlsx::addStyle(wb, sheet, st_cell, rows = rw,
                         cols = seq_along(hdr2), gridExpand = TRUE)
      openxlsx::setRowHeights(wb, sheet, rows = rw, heights = 24)
    }

    openxlsx::setColWidths(wb, sheet, cols = 1, widths = 5)
    openxlsx::setColWidths(wb, sheet, cols = 2, widths = 22)
    openxlsx::setColWidths(wb, sheet, cols = 3:nc, widths = 15)
    openxlsx::freezePane(wb, sheet, firstActiveRow = 4, firstActiveCol = 3)
    invisible(NULL)
  }

  # Detailed matrix page: hitters x pitchers, each cell carrying the 0-100
  # matchup score plus the skill split and the positioning the staff plays.
  .thr_write_matrix <- function(wb, sheet, batters, pitchers, title, team) {
    setProgress <- function(...) tryCatch(shiny::setProgress(...),
                                          error = function(e) invisible())
    openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
    S <- openxlsx::createStyle
    st_title <- S(fgFill = "#B08D57", fontColour = "#FFFFFF", textDecoration = "bold",
                  halign = "center", valign = "center", fontSize = 15)
    st_hdr <- S(fgFill = "#F2F6F6", textDecoration = "bold", halign = "center",
                valign = "center", fontSize = 10, wrapText = TRUE,
                border = "TopBottomLeftRight", borderColour = "#B7C4C4")
    st_cell <- S(halign = "center", valign = "center", fontSize = 11,
                 border = "TopBottomLeftRight", borderColour = "#D5DDDD")

    ncol_total <- max(2, length(pitchers) + 2)
    openxlsx::writeData(wb, sheet, title, startRow = 1, startCol = 1)
    openxlsx::mergeCells(wb, sheet, cols = 1:ncol_total, rows = 1)
    openxlsx::addStyle(wb, sheet, st_title, rows = 1, cols = 1:ncol_total,
                       gridExpand = TRUE)

    if (length(pitchers) == 0 || length(batters) == 0) {
      openxlsx::writeData(wb, sheet,
        "No pitchers in this rest group for the selected game date.",
        startRow = 3, startCol = 1)
      return(invisible(NULL))
    }

    mat <- tryCatch(
      build_matchup_matrix(mm_db(), pitchers, batters, mm_scaler(),
                           pitcher_weight = 0.5),
      error = function(e) { cat("[THR] matrix failed -", conditionMessage(e), "\n"); NULL })

    hdr <- c("Hitter", "B", pitchers)
    openxlsx::writeData(wb, sheet, t(hdr), startRow = 3, startCol = 1, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, st_hdr, rows = 3, cols = seq_along(hdr),
                       gridExpand = TRUE)
    openxlsx::setRowHeights(wb, sheet, rows = 3, heights = 46)

    # build_matchup_matrix returns Batter / Pitcher / MatchupScore (0-100,
    # hitter POV) plus the Decision / Damage RV split and a low-confidence
    # flag. The cell prints the score and marks thin samples with * so a
    # 12-pitch read is never mistaken for a real one.
    cell_for <- function(b, p) {
      if (is.null(mat) || !is.data.frame(mat) ||
          !all(c("Batter", "Pitcher", "MatchupScore") %in% names(mat))) return("-")
      row <- mat[!is.na(mat$Batter) & mat$Batter == b &
                 !is.na(mat$Pitcher) & mat$Pitcher == p, , drop = FALSE]
      if (!nrow(row)) return("-")
      sc <- suppressWarnings(as.numeric(row$MatchupScore[1]))
      if (!is.finite(sc)) return("-")
      paste0(round(sc), if (isTRUE(row$LowConfidence[1])) "*" else "")
    }

    for (i in seq_along(batters)) {
      b <- batters[i]
      bdf <- tryCatch(load_ncaa_batter_data(b, team), error = function(e) NULL)
      sd  <- if (is.null(bdf)) NA_character_ else thr_batter_side(bdf)
      vals <- c(b, sd %||% "-", vapply(pitchers, function(p) cell_for(b, p), character(1)))
      rw <- 3 + i
      openxlsx::writeData(wb, sheet, t(vals), startRow = rw, startCol = 1,
                          colNames = FALSE)
      openxlsx::addStyle(wb, sheet, st_cell, rows = rw, cols = seq_along(vals),
                         gridExpand = TRUE)
    }

    # Positioning block: the three pills the staff actually sets up on,
    # oriented to each hitter's side.
    base <- 3 + length(batters) + 2
    openxlsx::writeData(wb, sheet, "POSITIONING & SKILL SPLIT",
                        startRow = base, startCol = 1)
    openxlsx::mergeCells(wb, sheet, cols = 1:ncol_total, rows = base)
    openxlsx::addStyle(wb, sheet, st_title, rows = base, cols = 1:ncol_total,
                       gridExpand = TRUE)
    hdr2 <- c("Hitter", "B", "Pull %", "Center %", "Oppo %",
              "Decision", "Contact", "Power", "Notes")
    openxlsx::writeData(wb, sheet, t(hdr2), startRow = base + 1, startCol = 1,
                        colNames = FALSE)
    openxlsx::addStyle(wb, sheet, st_hdr, rows = base + 1, cols = seq_along(hdr2),
                       gridExpand = TRUE)
    for (i in seq_along(batters)) {
      b <- batters[i]
      bdf <- tryCatch(load_ncaa_batter_data(b, team), error = function(e) NULL)
      sd <- if (is.null(bdf)) NA_character_ else thr_batter_side(bdf)
      pos <- c(Pull = NA_real_, Center = NA_real_, Oppo = NA_real_)
      lay <- c(Decision = NA_real_, Contact = NA_real_, Power = NA_real_)
      if (!is.null(bdf) && nrow(bdf) > 0) {
        sc <- tryCatch(hp_score_pitches(bdf), error = function(e) NULL)
        if (!is.null(sc) && "hp_dv" %in% names(sc)) {
          hp_build_league()
          bip <- if ("hp_bip" %in% names(sc))
            sc[sc$hp_bip %in% TRUE, , drop = FALSE] else sc[0, , drop = FALSE]
          pos <- thr_positioning(bip, sd)
          lay <- thr_layer_grades(sc, min_n = 25)
        }
      }
      g <- function(x) if (is.finite(x)) round(x) else "-"
      rw <- base + 1 + i
      openxlsx::writeData(wb, sheet,
        t(c(b, sd %||% "-", g(pos[["Pull"]]), g(pos[["Center"]]), g(pos[["Oppo"]]),
            g(lay[["Decision"]]), g(lay[["Contact"]]), g(lay[["Power"]]), "")),
        startRow = rw, startCol = 1, colNames = FALSE)
      openxlsx::addStyle(wb, sheet, st_cell, rows = rw, cols = seq_along(hdr2),
                         gridExpand = TRUE)
    }

    openxlsx::writeData(wb, sheet,
      "0-100 hitter POV: high = the hitter is favoured. * = low sample, read with care.",
      startRow = base + 1 + length(batters) + 2, startCol = 1)

    openxlsx::setColWidths(wb, sheet, cols = 1, widths = 22)
    openxlsx::setColWidths(wb, sheet, cols = 2, widths = 5)
    openxlsx::setColWidths(wb, sheet, cols = 3:ncol_total, widths = 14)
    openxlsx::freezePane(wb, sheet, firstActiveRow = 4, firstActiveCol = 3)
    invisible(NULL)
  }

  output$thr_download <- downloadHandler(
    filename = function() {
      paste0(gsub("[^A-Za-z0-9]+", "_", thr_team_disp() %||% "Team"),
             "_Hitter_Advance_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      ord <- input$mm_batters
      req(length(ord) > 0)
      team <- thr_team_disp() %||% "Opponent"
      starters <- utils::head(ord, 9)
      bench    <- if (length(ord) > 9) ord[10:length(ord)] else character(0)

      st <- thr_staff()
      sp_arms <- character(0); rested <- character(0); nonrested <- character(0)
      if (!is.null(st) && nrow(st)) {
        # Starters here = the arms the staff flagged as likely SP. Without a
        # GS column we fall back to "not classified as a reliever by rest",
        # which is the honest read of what we know.
        rested    <- st$pitcher[st$rest_class == "Rested"]
        nonrested <- st$pitcher[st$rest_class == "Non-Rested"]
        sp_arms   <- st$pitcher[st$rest_class == "Unknown"]
        if (!length(sp_arms)) sp_arms <- utils::head(st$pitcher, 3)
      }

      # Reuse the stat frame the tab already pulled; refetch only if the
      # session was restored without it.
      sdf <- thr_stats() %||% .thr_stat_lookup(team)

      wb <- openxlsx::createWorkbook()
      withProgress(message = "Building hitter advance workbook...", value = 0, {
        setProgress(0.05, detail = "Page 1 \u2014 starters")
        .thr_write_hitters(wb, "1 Starters", starters, team,
                           paste0(team, " \u2014 Starting Nine"), sdf = sdf)
        setProgress(0.35, detail = "Page 2 \u2014 bench")
        .thr_write_hitters(wb, "2 Bench", bench, team,
                           paste0(team, " \u2014 Bench / Reserves"), sdf = sdf)
        setProgress(0.55, detail = "Page 3 \u2014 vs starters")
        .thr_write_matrix(wb, "3 vs Starters", ord, sp_arms,
                          paste0(team, " \u2014 Matchup Matrix vs Starting Pitchers"), team)
        setProgress(0.75, detail = "Page 4 \u2014 vs rested pen")
        .thr_write_matrix(wb, "4 vs Rested RP", ord, rested,
                          paste0(team, " \u2014 Matchup Matrix vs Rested Relievers"), team)
        setProgress(0.9, detail = "Page 5 \u2014 vs non-rested pen")
        .thr_write_matrix(wb, "5 vs Non-Rested RP", ord, nonrested,
                          paste0(team, " \u2014 Matchup Matrix vs Non-Rested Relievers"), team)
      })
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  output$swb_download <- downloadHandler(
    filename = function() {
      paste0(gsub("[^A-Za-z0-9]+", "_", input$swb_team %||% "Team"),
             "_Scouting_Workbook_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      arms <- input$swb_order
      req(length(arms) > 0)
      wb <- openxlsx::createWorkbook()
      used <- character(0)
      withProgress(message = "Building scouting workbook...", value = 0, {
        for (i in seq_along(arms)) {
          p <- arms[i]
          role <- input[[paste0("swb_role_", .swb_key(p))]] %||% "RP"
          setProgress(value = (i - 1) / length(arms),
                      detail = paste0(role, " \u2014 ", p))
          sheet <- .swb_sheet_name(role, p, used)
          used <- c(used, sheet)
          df_p <- tryCatch(scout_to_feet(load_ncaa_pitcher_data(p)),
                           error = function(e) NULL)
          saved <- scout_notes_storage()[[p]] %||% list()
          notes <- list(delivery = saved$delivery_notes %||% "",
                        stamina  = saved$stamina_notes  %||% "",
                        matchup  = saved$matchup_notes  %||% "",
                        vs_rhh   = saved$vs_rhh_notes   %||% "",
                        vs_lhh   = saved$vs_lhh_notes   %||% "")
          if (is.null(df_p) || !is.data.frame(df_p) || nrow(df_p) == 0) {
            openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
            openxlsx::writeData(wb, sheet,
                                paste0("No tracked pitch data found for ", p,
                                       " (no TrackMan directory entry and no ",
                                       "TruMedia pitches located)."),
                                startRow = 2, startCol = 2)
            next
          }
          tryCatch(
            write_scouting_sheet(wb, sheet, p, df_p, notes = notes),
            error = function(e) {
              cat("[SWB] sheet failed for", p, "-", conditionMessage(e), "\n")
              tryCatch(openxlsx::removeWorksheet(wb, sheet),
                       error = function(e2) invisible())
              openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
              openxlsx::writeData(wb, sheet,
                                  paste0("Report failed for ", p, ": ",
                                         conditionMessage(e)),
                                  startRow = 2, startCol = 2)
            })
        }
        if (length(names(wb)) == 0) openxlsx::addWorksheet(wb, "Empty")
        setProgress(0.98, detail = "Saving workbook...")
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      })
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
