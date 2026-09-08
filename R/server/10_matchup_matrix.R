  # ===================== ADVANCE: MATCHUP MATRIX =====================
  # Data pools are loaded per selected team, sourced the same way the 2026
  # pitcher pages are: TruMedia API primary for 2026, NCAA parquet as the
  # 2025 complement / 2026 fallback, in-memory P5/SBC/CCU pool for
  # TrackMan-code teams. The engine db is the union of the two team pools.
  mm_scaler <- reactive({ build_feature_scaler(full_data_p5_compare) })
  mm_matrix <- reactiveVal(NULL)

  mm_pool_pitcher <- reactive({
    req(input$mm_pitcher_team)
    withProgress(message = "Loading pitcher-team pool...", value = 0.4,
                 load_matchup_team_pool(input$mm_pitcher_team))
  })
  mm_pool_batter <- reactive({
    req(input$mm_batter_team)
    withProgress(message = "Loading batter-team pool...", value = 0.4,
                 load_matchup_team_pool(input$mm_batter_team))
  })
  mm_db <- reactive({
    frames <- Filter(function(d) !is.null(d) && nrow(d) > 0,
                     list(mm_pool_pitcher(), mm_pool_batter()))
    req(length(frames) > 0)
    .mm_dedupe_uid(dplyr::bind_rows(harmonize_types(frames)))
  })

  # ---- shared hitter-detail cache (matrix UI + PDF export) ---------------
  # Pool-FIRST sourcing: the batter-team pool already holds every pitch the
  # matrix engine priced, so the detail card can never come up emptier than
  # the matrix itself. The per-hitter TruMedia/parquet loader is only a
  # fallback when the pool subset is thin — its name matching was what left
  # most hitters' bio/stats blank.
  .mm_key_team <- function(k) {
    t <- strsplit(k, "||", fixed = TRUE)[[1]]
    if (length(t) >= 2 && nzchar(t[2])) .mm_team_display(t[2]) else NULL
  }
  mm_detail_env <- new.env(parent = emptyenv())
  mm_hitter_detail <- function(batter, bkey, bio = list()) {
    pool <- tryCatch(mm_pool_batter(), error = function(e) NULL)
    ck <- paste0(batter, "\u241F", bkey, "::",
                 if (is.null(pool)) 0L else nrow(pool))
    hit <- mm_detail_env[[ck]]
    if (!is.null(hit)) return(hit)
    bteam <- .mm_key_team(bkey)
    bdf <- NULL
    if (!is.null(pool) && "Batter" %in% names(pool))
      bdf <- pool[!is.na(pool$Batter) & pool$Batter == batter, , drop = FALSE]
    if (is.null(bdf) || nrow(bdf) < 40) {
      alt <- tryCatch(load_ncaa_batter_data(batter, bteam),
                      error = function(e) NULL)
      if (!is.null(alt) && is.data.frame(alt) &&
          nrow(alt) > (if (is.null(bdf)) 0 else nrow(bdf))) bdf <- alt
    }
    scored <- tryCatch(hp_score_pitches(bdf), error = function(e) bdf)
    if (!is.null(scored) && is.data.frame(scored) && nrow(scored) > 0)
      scored$mm_xslg <- tryCatch(.mm_perpitch_xslg(scored),
                                 error = function(e) rep(NA_real_, nrow(scored)))
    row <- tryCatch(thr_hitter_row(scored, batter, bio),
                    error = function(e) NULL)
    heats <- list(
      missR = mm_mini_heatmap(scored, "R", "miss"),
      missL = mm_mini_heatmap(scored, "L", "miss"),
      xslgR = mm_mini_heatmap(scored, "R", "xslg"),
      xslgL = mm_mini_heatmap(scored, "L", "xslg"))
    out <- list(row = row, df = scored, heats = heats)
    mm_detail_env[[ck]] <- out
    out
  }
  # grades used by both the matrix rows and the PDF: Process model first,
  # TruMedia rate grades (pool-priced EV90) as the thin-sample fallback.
  mm_hitter_grades <- function(batter, bkey) {
    g <- tryCatch(hp_matrix_grades(mm_pool_batter(), batter),
                  error = function(e) NULL)
    if (!is.null(g)) { g$process <- TRUE; return(g) }
    g <- tryCatch(tm_batter_grades(batter, .mm_key_team(bkey),
                                   pool = tryCatch(mm_pool_batter(),
                                                   error = function(e) NULL)),
                  error = function(e) NULL)
    if (!is.null(g)) g$process <- FALSE
    g
  }

  # Populate team pickers the first time the Advance tab opens:
  # TrackMan-code teams from the in-memory pool + every NCAA directory team.
  .mm_choices_set <- reactiveVal(FALSE)
  observeEvent(input$main_tabs, {
    if (!isTRUE(input$main_tabs == "Advance") || isTRUE(.mm_choices_set())) return()
    codes <- sort(unique(c(full_data_p5_compare$PitcherTeam,
                           full_data_p5_compare$BatterTeam)))
    codes <- codes[!is.na(codes) & nzchar(codes)]
    td <- teams_data$team_name[match(codes, teams_data$trackman_abbr)]
    code_choices <- setNames(codes,
                             ifelse(is.na(td), codes, paste0(td, " (", codes, ")")))
    dirteams <- sort(unique(as.character(ncaa_team_directory$team_disp)))
    dirteams <- dirteams[!is.na(dirteams) & nzchar(dirteams)]
    ncaa_choices <- setNames(paste0("ncaa::", dirteams), dirteams)
    grouped <- list(
      "NCAA 2025-26 (TruMedia + parquet)" = ncaa_choices,
      "TrackMan pool (CCU / P5 / SBC)"    = code_choices
    )
    updatePickerInput(session, "mm_pitcher_team", choices = grouped,
                      selected = if ("COA_CHA" %in% codes) "COA_CHA" else NULL)
    updatePickerInput(session, "mm_batter_team", choices = grouped,
                      selected = NULL)
    updatePickerInput(session, "ms_pitcher_team", choices = grouped,
                      selected = if ("COA_CHA" %in% codes) "COA_CHA" else NULL)
    updatePickerInput(session, "ms_batter_team", choices = grouped,
                      selected = NULL)
    updateSelectizeInput(session, "swb_team", choices = c("", dirteams),
                         selected = "", server = FALSE)
    .mm_choices_set(TRUE)
  })

  # ---- TruMedia-only roster loader for the matrix / spray pickers ----
  # Player lists come from PlayerTotals (the live 2026 roster) — never from
  # parquet/TrackMan name scans. Each roster name is then mapped onto the
  # spelling the team's pitch pool uses so the engine's filters hit.
  .mm_roster_cache <- new.env(parent = emptyenv())
  mm_roster_choices <- function(team_key, side = c("pitcher", "batter")) {
    side <- match.arg(side)
    ck <- paste0(side, "::", gsub("[^A-Za-z0-9]", "_", team_key))
    hit <- .mm_roster_cache[[ck]]
    if (!is.null(hit)) return(hit)
    lbl  <- .mm_team_label(team_key)
    disp <- .mm_team_display(lbl)
    tid <- tryCatch(tm_find_team_id(disp), error = function(e) NULL)
    tot <- if (!is.null(tid))
      tryCatch(tm_team_player_totals(tid, "overall",
                 if (side == "pitcher") TM_TRAD_COLS else TM_BAT_COLS),
               error = function(e) NULL) else NULL
    if (is.null(tot) || nrow(tot) == 0) return(NULL)
    nm_col <- .tm_pick_col(tot, c("playerName", "playerFullName", "^player$", "name"))
    if (is.null(nm_col)) return(NULL)
    stat_vec <- function(ab) vapply(seq_len(nrow(tot)), function(i)
      suppressWarnings(as.numeric(tm_stat(tot[i, , drop = FALSE], ab))),
      numeric(1))
    qty <- if (side == "pitcher") stat_vec("IP") else {
      q <- stat_vec("PA")                       # response may carry AB/G only
      if (all(is.na(q))) q <- stat_vec("AB")    # (default-column fallbacks)
      if (all(is.na(q))) q <- stat_vec("G")
      q
    }
    keep <- !is.na(qty) & qty > 0
    if (!any(keep)) return(NULL)
    roster <- as.character(tot[[nm_col]])[keep][order(-qty[keep])]
    roster <- roster[!is.na(roster) & nzchar(roster)]
    if (length(roster) == 0) return(NULL)

    pool <- load_matchup_team_pool(team_key)
    pool_names <- if (side == "pitcher") {
      unique(pool$Pitcher[!is.na(pool$PitcherTeam) & pool$PitcherTeam == lbl])
    } else if ("BatterTeam" %in% names(pool)) {
      unique(pool$Batter[!is.na(pool$BatterTeam) & pool$BatterTeam == lbl])
    } else character(0)
    pool_names <- pool_names[!is.na(pool_names) & nzchar(pool_names)]
    kp <- norm_player_key(pool_names)
    resolve <- function(nm) {
      kn <- norm_player_key(nm)
      h <- pool_names[!is.na(kp) & kp == kn]
      if (length(h) > 0) return(h[1])
      parts <- strsplit(kn, " ", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        last <- parts[length(parts)]; fi <- substr(parts[1], 1, 1)
        h <- pool_names[!is.na(kp) & endsWith(kp, paste0(" ", last)) &
                          substr(kp, 1, 1) == fi]
        if (length(h) > 0) return(h[1])
      }
      nm
    }
    vals <- if (length(pool_names) > 0)
      vapply(roster, resolve, character(1), USE.NAMES = FALSE) else roster
    keys <- paste0(vals, "||", lbl)
    dd <- !duplicated(keys)
    out <- setNames(keys[dd], roster[dd])
    .mm_roster_cache[[ck]] <- out
    out
  }

  # team key -> the label rows carry inside that team's pool
  .mm_team_label <- function(key) {
    if (is.null(key) || !nzchar(key)) return(key)
    if (startsWith(key, "ncaa::")) sub("^ncaa::", "", key) else key
  }

  observeEvent(input$mm_pitcher_team, {
    req(input$mm_pitcher_team)
    invisible(mm_pool_pitcher())   # warm the engine pool
    ch <- withProgress(message = "Loading 2026 staff from TruMedia...", value = 0.5,
                       mm_roster_choices(input$mm_pitcher_team, "pitcher"))
    if (is.null(ch) || length(ch) == 0) {
      showNotification(paste0("TruMedia roster unavailable for ",
                              .mm_team_display(.mm_team_label(input$mm_pitcher_team)),
                              " — no pitchers loaded."),
                       type = "warning", duration = 6)
      ch <- character(0)
    }
    updateSelectizeInput(session, "mm_pitchers", choices = ch,
                         selected = unname(ch), server = FALSE)
  })
  observeEvent(input$mm_batter_team, {
    req(input$mm_batter_team)
    invisible(mm_pool_batter())
    ch <- withProgress(message = "Loading 2026 lineup from TruMedia...", value = 0.5,
                       mm_roster_choices(input$mm_batter_team, "batter"))
    if (is.null(ch) || length(ch) == 0) {
      showNotification(paste0("TruMedia roster unavailable for ",
                              .mm_team_display(.mm_team_label(input$mm_batter_team)),
                              " — no batters loaded."),
                       type = "warning", duration = 6)
      ch <- character(0)
    }
    updateSelectizeInput(session, "mm_batters", choices = ch,
                         selected = unname(ch), server = FALSE)
  })

  observeEvent(input$mm_build, {
    req(length(input$mm_pitchers) > 0, length(input$mm_batters) > 0)
    res <- withProgress(message = "Building matchup matrix...", value = 0, {
      build_matchup_matrix(
        mm_db(), input$mm_pitchers, input$mm_batters, mm_scaler(),
        progress_callback = function(frac, msg) setProgress(frac, detail = msg),
        pitcher_weight = input$mm_pitcher_weight,
        sim_threshold = input$mm_sim_threshold
      )
    })
    mm_matrix(res)
    mm_detail_pair(NULL)
    mm_detail_pt(NULL)
  })

  # Teamworks-style matrix card: batters as rows, pitchers as columns,
  # 0-100 score pills, click any cell to open the matchup detail below.
  output$mm_matrix_ui <- renderUI({
    m <- mm_matrix()
    if (is.null(m) || nrow(m) == 0) {
      return(div(class = "adv-placeholder",
                 p("Pick a pitcher team and a batter team, choose the arms and ",
                   "bats, then hit Generate Matrix.")))
    }
    m <- m %>%
      dplyr::mutate(
        .pkey = paste0(Pitcher, "||", ifelse(is.na(PitcherTeam), "", PitcherTeam)),
        .bkey = paste0(Batter,  "||", ifelse(is.na(BatterTeam),  "", BatterTeam)))

    pit <- m %>% dplyr::distinct(.pkey, Pitcher, PitcherThrows)
    bat <- m %>% dplyr::distinct(.bkey, Batter, BatterSide)
    cellmap <- setNames(
      lapply(seq_len(nrow(m)), function(i)
        list(score = m$MatchupScore[i], lc = isTRUE(m$LowConfidence[i]),
             rvs = if ("PitchRVs" %in% names(m)) m$PitchRVs[i] else NA_character_,
             dec = if ("DecisionRV" %in% names(m)) m$DecisionRV[i] else NA_real_,
             dmg = if ("DamageRV"   %in% names(m)) m$DamageRV[i]   else NA_real_)),
      paste0(m$.pkey, "\u241F", m$.bkey))

    view <- input$mm_view %||% "pitcher"
    .parse_rvs <- function(s) {
      if (is.null(s) || length(s) != 1 || is.na(s) || !nzchar(s)) return(NULL)
      kv <- strsplit(strsplit(s, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
      d <- data.frame(pt = vapply(kv, function(z) z[1], character(1)),
                      rv = suppressWarnings(as.numeric(
                        vapply(kv, function(z) z[2] %||% NA_character_,
                               character(1)))),
                      n  = suppressWarnings(as.numeric(
                        vapply(kv, function(z) z[3] %||% NA_character_,
                               character(1)))),
                      stringsAsFactors = FALSE)
      d$n[is.na(d$n)] <- 0
      d[!is.na(d$rv), , drop = FALSE]
    }
    .MM_MINI_MIN_SIM <- 10   # below this, the pill's RV leans on platoon priors

    # Detailed hitter cards come from the shared session cache
    # (mm_hitter_detail): pool-first data, per-pitch xSLG, mini heatmaps.
    # The PDF export reads the SAME cache so print always matches screen.

    p_lbl <- .mm_team_label(input$mm_pitcher_team %||% "")
    b_lbl <- .mm_team_label(input$mm_batter_team %||% "")
    sub <- paste0(.mm_team_display(p_lbl), " Pitchers vs ",
                  .mm_team_display(b_lbl), " Batters \u2022 ",
                  if (identical(view, "hitter")) "Hitter Facing" else "Pitcher Facing")

    # Visible confirmation of the pricing basis
    sub <- paste0(sub, " \u2022 actual DRE run value")

    .team_of_key <- function(k) {
      t <- strsplit(k, "||", fixed = TRUE)[[1]]
      if (length(t) >= 2 && nzchar(t[2])) .mm_team_display(t[2]) else NULL
    }
    .hand_class <- function(h) {
      if (identical(h, "L")) "mm-lefty" else if (identical(h, "S")) "mm-switch" else NULL
    }

    detailed_on <- isTRUE(input$mm_detailed)
    heat_heads <- if (detailed_on) list(
      tags$th(class = "mm-heat-h", "Miss%", tags$br(), "vs RHP"),
      tags$th(class = "mm-heat-h", "Miss%", tags$br(), "vs LHP"),
      tags$th(class = "mm-heat-h", "xSLG",  tags$br(), "vs RHP"),
      tags$th(class = "mm-heat-h", "xSLG",  tags$br(), "vs LHP")) else NULL

    head_cells <- c(
      list(tags$th(class = "mm-bh", "Batter")),
      heat_heads,
      lapply(seq_len(nrow(pit)), function(j) {
        thr <- substr(pit$PitcherThrows[j] %||% "", 1, 1)
        d <- drs_info(pit$Pitcher[j], .team_of_key(pit$.pkey[j]))
        top <- paste0(if (nzchar(d$jersey)) paste0("#", d$jersey, " ") else "",
                      pit$Pitcher[j])
        sub_bits <- c(if (nzchar(thr) && !is.na(thr)) paste0("(", thr, ")"),
                      if (nzchar(d$class)) d$class)
        tags$th(
          div(class = paste("tp-player-link", .hand_class(thr)),
              `data-name` = pit$Pitcher[j], `data-side` = "pitcher",
              top),
          div(class = paste("mm-hand", .hand_class(thr)),
              paste(sub_bits, collapse = " \u00B7 ")))
      }))

    body_rows <- lapply(seq_len(nrow(bat)), function(i) {
      side <- substr(bat$BatterSide[i] %||% "", 1, 1)
      d <- drs_info(bat$Batter[i], .team_of_key(bat$.bkey[i]))
      nm <- paste0(if (nzchar(d$jersey)) paste0("#", d$jersey, " ") else "",
                   bat$Batter[i])
      meta_bits <- c(if (nzchar(side) && !is.na(side)) paste0("(", side, ")"),
                     if (nzchar(d$pos)) d$pos,
                     if (nzchar(d$class)) d$class)
      drs_pill <- if (!is.na(d$overall)) {
        .drs_bg <- drs_pill_color(d$overall)
        tags$span(class = "mm-drs-pill",
                  style = paste0("background:", .drs_bg, "; color:",
                                 pill_text_color(.drs_bg), ";"),
                  sprintf("%+d", round(as.numeric(d$overall))))
      } else NULL

      # hitter grades: per-pitch Process model first (Decision Value /
      # Contact / Power / Process+, run-value based, pitch-quality
      # conditioned) computed from the batter team's pitch pool; the
      # TruMedia rate-stat grades (EV90 now priced from pitch data)
      # remain the fallback for thin samples.
      g <- mm_hitter_grades(bat$Batter[i], bat$.bkey[i])
      g_process <- !is.null(g) && isTRUE(g$process)
      gpill <- function(lab, v, agg = FALSE) {
        na <- is.null(v) || !is.finite(v)
        .bg <- if (na) NULL else grade_pill_color(v)
        tags$span(
          class = paste("mm-gpill", if (agg) "mm-gagg", if (na) "mm-gna"),
          style = if (na) NULL else
            paste0("background:", .bg, "; color:", pill_text_color(.bg), ";"),
          title = sprintf("%s: %s (%s)", lab,
                          if (na) "n/a" else as.character(round(v)),
                          if (g_process)
                            "Process model \u2014 runs vs pitch expectation, 100 = avg"
                          else "100 = section avg"),
          tags$span(class = "mm-glab", lab),
          tags$span(class = "mm-gval",
                    if (na) "\u2014" else as.character(round(v))))
      }
      grades_row <- if (!is.null(g)) {
        div(class = "mm-grades",
            gpill(if (g_process) "Prc" else "Agg", g$aggregate, agg = TRUE),
            gpill("Con", g$contact),
            gpill("Pow", g$power),
            gpill("Dec", g$decision))
      } else NULL

      # ---- Detailed view: the scouting card, folded into the row ----
      # This uses bat$Batter (the clean name off the matrix result) and
      # .team_of_key() for the team -- NOT input$mm_batters, which holds
      # composite "Name||Team" keys. Miss/xSLG locations are no longer
      # written out as text: they render as heatmap COLUMNS (heat_tds).
      detail_block <- NULL
      heat_tds <- NULL
      if (detailed_on) {
        det <- mm_hitter_detail(bat$Batter[i], bat$.bkey[i], d)
        r <- det$row
        heat_tds <- lapply(det$heats[c("missR", "missL", "xslgR", "xslgL")],
                           function(p) {
          uri <- tryCatch(.gg_data_uri(p), error = function(e) NULL)
          tags$td(class = "mm-heatcell",
                  if (!is.null(uri)) tags$img(src = uri, class = "mm-heatimg")
                  else tags$span(class = "mm-hand", "\u2014"))
        })
        if (!is.null(r)) {
          shown <- function(val, suffix = "", digits = 0) {
            if (is.null(val) || length(val) == 0) return("\u2014")
            if (is.numeric(val)) {
              if (!is.finite(val[1])) return("\u2014")
              return(paste0(round(val[1], digits), suffix))
            }
            txt <- trimws(as.character(val[1]))
            if (!nzchar(txt)) "\u2014" else paste0(txt, suffix)
          }
          chip <- function(lab, val, suffix = "", digits = 0) {
            tags$span(class = "mm-chip",
                      tags$span(class = "mm-chip-l", lab),
                      tags$span(class = "mm-chip-v", shown(val, suffix, digits)))
          }
          swing_line <- function(flag, pct, n) {
            paste0(shown(flag), " ", shown(pct, "%"),
                   " / ", shown(n), " pitches")
          }
          detail_block <- div(class = "mm-detail",
            div(class = "mm-chiprow",
                chip("SB attempts", r$SBA), chip("Bunts", r$Bunts),
                chip("EV90", r$EV90, " mph", 1),
                chip("Max EV", r$MaxEV, " mph", 1)),
            div(class = "mm-chiprow",
                chip("1P agg", swing_line(r$Agg1P, r$Swing1P, r$Pitches1P)),
                chip("2K agg", swing_line(r$Agg2K, r$Swing2K, r$Pitches2K))),
            div(class = "mm-chiprow",
                chip("PT weakness", r$PTWeak),
                chip("Trait vL", r$TraitWeakL),
                chip("Trait vR", r$TraitWeakR)),
            div(class = "mm-chiprow",
                chip("Pull/Ctr/Oppo vL", r$PosL),
                chip("Pull/Ctr/Oppo vR", r$PosR),
                chip("Pre-2K", r$PosPre2K),
                chip("2K", r$Pos2K)),
            div(class = "mm-chiprow",
                chip("Notes", ""))
          )
        }
      }

      name_td <- tags$td(class = "mm-namecell",
        div(class = "mm-hitter-head",
            div(class = "mm-hitter-main",
                tags$span(class = paste("mm-name", "tp-player-link", .hand_class(side)),
                          `data-name` = bat$Batter[i], `data-side` = "hitter",
                          nm),
                tags$span(class = "mm-hand", paste(meta_bits, collapse = " \u00B7 "))),
            drs_pill),
        grades_row,
        detail_block)
      cell_tds <- lapply(seq_len(nrow(pit)), function(j) {
        cm <- cellmap[[paste0(pit$.pkey[j], "\u241F", bat$.bkey[i])]]
        sc <- if (is.null(cm)) NA_real_ else cm$score
        # hitter-facing flips the scale: 100 = best hitter matchup
        disp <- if (identical(view, "hitter") && !is.na(sc)) 100 - sc else sc
        lc <- !is.null(cm) && isTRUE(cm$lc)
        bg <- matchup_score_color(disp)
        # luminance-based text: white automatically on the darkest greens/reds
        fg <- if (is.na(disp)) "#9AA1A6" else pill_text_color(bg)
        cls <- paste("mm-cell", if (lc) "mm-lc", if (is.na(disp)) "mm-na")

        # arsenal ranked by RV vs similar pitches; best-for-this-view first
        minis <- NULL
        rvd <- .parse_rvs(if (is.null(cm)) NULL else cm$rvs)
        if (!is.null(rvd) && nrow(rvd) > 0) {
          # engine rv100 is HITTER run value: low = good for the pitcher
          rvd <- rvd[order(if (identical(view, "hitter")) -rvd$rv else rvd$rv), ,
                     drop = FALSE]
          rvd <- head(rvd, 4)
          # absolute coloring on the same 0-100 scale as the cell itself:
          # a bad-everywhere arsenal shows shades of red, not a fake green
          mini_sc <- vapply(rvd$rv, rv_to_matchup_score, numeric(1))
          if (identical(view, "hitter")) mini_sc <- 100 - mini_sc
          minis <- div(class = "mm-minirow",
                       lapply(seq_len(nrow(rvd)), function(k) {
                         .mbg <- matchup_score_color(mini_sc[k])
                         tags$span(class = paste("mm-mini",
                                                 if (rvd$n[k] < .MM_MINI_MIN_SIM)
                                                   "mm-mini-lc"),
                                   style = paste0("background:", .mbg,
                                                  "; color:",
                                                  pill_text_color(.mbg), ";"),
                                   title = sprintf("%s: %+.2f RV/100 (%d similar)",
                                                   rvd$pt[k], rvd$rv[k],
                                                   as.integer(rvd$n[k])),
                                   .xl_abbrev(rvd$pt[k]))
                       }))
        }

        tags$td(div(class = cls,
                    style = paste0("background:", bg, "; color:", fg, ";"),
                    `data-p` = pit$.pkey[j], `data-b` = bat$.bkey[i],
                    div(class = "mm-cell-score",
                        if (is.na(disp)) "\u2014" else as.character(round(disp))),
                    # Decision / Damage split (toggle): raw signed hitter
                    # RV/100 components — Dec = net value on takes, calls,
                    # whiffs and fouls; Dmg = expected damage on contact.
                    # Positive favors the BAT in either perspective.
                    if (isTRUE(input$mm_split) && !is.null(cm) &&
                        is.finite(cm$dec %||% NA_real_) &&
                        is.finite(cm$dmg %||% NA_real_))
                      div(style = paste0("font-size:9px; line-height:1.1; ",
                                         "opacity:.85; margin-top:1px;"),
                          title = paste0("Batter-side split (hitter RV/100): ",
                                         "Decision = balls/strikes/fouls/whiffs, ",
                                         "Damage = expected value on contact"),
                          sprintf("D %+.1f \u00B7 C %+.1f", cm$dec, cm$dmg)),
                    minis))
      })
      tags$tr(name_td, heat_tds, cell_tds)
    })

    div(class = "mm-card",
        div(class = "mm-card-header",
            span(class = "mm-title", "Matchup Projections"),
            span(class = "mm-sub", sub)),
        tags$table(class = "mm-table",
                   tags$thead(tags$tr(head_cells)),
                   tags$tbody(body_rows)))
  })
