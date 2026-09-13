  # ============================================================
  # ===== TRUMEDIA: trad stats + splits + Excel scouting export =
  # ============================================================

  # Team display name for the selected pitcher (CCU or NCAA).
  tm_pitcher_team <- function(gp) {
    if (is.null(gp) || !nzchar(gp)) return(NULL)
    if (is_coastal_pitcher(gp)) return("Coastal Carolina")
    # transfer-aware: TruMedia pulls are current-season (2026), so hand
    # the API his 2026 team, not the first team alphabetically
    tm_current_team(gp)
  }

  # All TruMedia splits for the current pitcher. tm_get_csv() caches
  # by URL, so switching between pitchers on the same team costs
  # nothing after the first pull.
  tm_splits_r <- reactive({
    gp <- input$global_pitcher
    req(gp, nzchar(gp))
    if (!tm_enabled()) return(NULL)
    td <- tm_pitcher_team(gp)
    if (is.null(td) || !nzchar(td)) return(NULL)
    tryCatch(tm_pitcher_splits(gp, td), error = function(e) NULL)
  })

  .tm_val <- function(row, abbr) {
    v <- tm_stat(row, abbr)
    if (length(v) == 0 || is.na(v)) return(NA_character_)
    as.character(v)
  }

  # ---- Overview: season table (one row per year x team x league) ----
  ps_rows <- reactive({
    gp <- input$global_pitcher
    req(gp, nzchar(gp), !is_hitter_pick(gp))
    tryCatch(season_rows_pitcher(gp, ident = player_ident_for(gp, "pit")), error = function(e) {
      cat("[season table]", conditionMessage(e), "\n"); NULL })
  })
  output$ps_table <- renderUI({
    season_table_html(ps_rows(), "pit", input$ps_mode %||% "Advanced",
                      "Show Expected" %in% (input$ps_exp %||% character(0)))
  })
  output$ps_note <- renderUI({
    r <- ps_rows()
    if (is.null(r) || !nrow(r)) return(NULL)
    src <- unique(r$Source)
    txt <- if ("team" %in% src) "TruMedia team lines" else if ("live" %in% src) "TruMedia (per team)"
           else if ("calendar" %in% src) "TruMedia calendar-year totals (spring + summer combined until the team-scoped tables are rebuilt)"
           else "TrackMan / bullpens only"
    span(class = "ps-note", txt)
  })

  # ---- Scouting tab: season line + splits tables ----
  .tm_html_table <- function(d) {
    if (is.null(d) || nrow(d) == 0) return(NULL)
    tags$table(style = "border-collapse:collapse; margin:6px auto 12px; font-size:13px;",
      tags$tr(lapply(names(d), function(n)
        tags$th(n, style = paste0("border-bottom:2px solid #0E6E70; background:#E8F1F1;",
                                  "padding:5px 12px; color:#0E6E70;")))),
      lapply(seq_len(nrow(d)), function(i)
        tags$tr(lapply(as.character(unlist(d[i, ])), function(v)
          tags$td(ifelse(is.na(v) | v == "NA", "—", v),
                  style = "border-bottom:1px solid #E5E7EB; padding:5px 12px; text-align:center;")))))
  }

  tm_season_line_df <- function(sp) {
    if (is.null(sp$overall)) return(NULL)
    row <- sp$overall
    abbrs <- c("W","L","G","GS","SV","IP","H","R","ER","BB","K","HBP","HR","ERA","WHIP","BA")
    vals <- vapply(abbrs, function(a) .tm_val(row, a), character(1))
    keep <- !is.na(vals)
    if (!any(keep)) return(NULL)
    as.data.frame(as.list(setNames(vals[keep], abbrs[keep])),
                  check.names = FALSE, stringsAsFactors = FALSE)
  }

  tm_splits_df <- function(sp) {
    abbrs <- c("PA","BA","SLG","K%","BB%","InZoneWhiff%","Chase%","FPStk%")
    rows <- lapply(c("vLHH","vRHH","RISP"), function(nm) {
      row <- sp[[nm]]
      if (is.null(row)) return(NULL)
      vals <- vapply(abbrs, function(a) .tm_val(row, a), character(1))
      cbind(data.frame(Split = nm, stringsAsFactors = FALSE),
            as.data.frame(as.list(setNames(vals, abbrs)),
                          check.names = FALSE, stringsAsFactors = FALSE))
    })
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0) return(NULL)
    dplyr::bind_rows(rows)
  }

  # Step-by-step connection debug shown in the Scouting tab expander
  output$tm_debug <- renderUI({
    gp <- input$global_pitcher
    if (is.null(gp) || !nzchar(gp)) return(NULL)
    lines <- character(0)
    add <- function(...) lines <<- c(lines, paste0(...))
    add("1. Secrets: ", if (tm_enabled()) "OK"
        else "MISSING - set TM_USERNAME / TM_SITENAME / TM_MASTER_TOKEN in Space settings")
    if (tm_enabled()) {
      tok <- tryCatch(tm_token(), error = function(e) NULL)
      add("2. Temp token: ", if (is.null(tok)) "FAILED" else "OK")
      if (!is.null(tok)) {
        teams <- tryCatch(tm_all_teams(), error = function(e) NULL)
        if (is.null(teams)) {
          add("3. AllTeams: FAILED - ", .tm_env$last_error %||% "no detail")
        } else {
          add("3. AllTeams: ", nrow(teams), " teams [",
              paste(head(names(teams), 6), collapse = ", "), "]")
          td  <- tm_pitcher_team(gp)
          tid <- if (!is.null(td)) tm_find_team_id(td) else NULL
          add("4. Team lookup '", td %||% "?", "': ",
              if (is.null(tid)) "NO MATCH" else paste0("teamId ", tid))
          if (!is.null(tid)) {
            pt <- tryCatch(tm_team_player_totals(tid, "overall"),
                           error = function(e) NULL)
            if (is.null(pt)) {
              add("5. PlayerTotals: FAILED - ", .tm_env$last_error %||% "no detail")
            } else {
              add("5. PlayerTotals: ", nrow(pt), " players [",
                  paste(head(names(pt), 8), collapse = ", "), "]")
              row <- tm_player_row(pt, gp)
              add("6. Player match '", gp, "': ",
                  if (is.null(row)) "NO MATCH (check name column above)" else "OK")
            }
          }
        }
      }
    }
    if (!is.null(.tm_env$last_error))
      add("Last error: ", .tm_env$last_error)
    div(style = paste0("font-family:monospace; font-size:11px; color:#374151;",
                       "background:#F9FAFB; border:1px solid #E5E7EB;",
                       "border-radius:8px; padding:8px 10px;"),
        lapply(lines, div))
  })

  output$tm_scout_splits_ui <- renderUI({
    if (!tm_enabled()) {
      return(div(style = "color:#9CA3AF; font-size:12px; margin:4px 0;",
                 "Set TM_USERNAME / TM_SITENAME / TM_MASTER_TOKEN secrets to enable TruMedia stats."))
    }
    sp <- tm_splits_r()
    if (is.null(sp)) {
      err <- .tm_env$last_error
      return(div(style = "color:#9CA3AF; font-size:12px; margin:4px 0;",
                 "No TruMedia data for this pitcher/team.",
                 if (!is.null(err))
                   div(style = "color:#DC2626; font-family:monospace; margin-top:2px;",
                       paste0("Last API error: ", err))))
    }
    tagList(.tm_html_table(tm_season_line_df(sp)),
            .tm_html_table(tm_splits_df(sp)))
  })

  # ---- Excel scouting report export (full template layout) ----
  # Replicates the two-rail advance template: header line, advanced
  # metrics, movement + pitch metrics, per-side results with location
  # heatmaps and damage spray, count usage / RISP count usage grids,
  # sequencing matrices, situational block (LOB%, P/IP, IP/G, inning
  # charts, wOBA by TTO, FB velo by pitch count), run game. Manual
  # sections (delivery notes, approaches via note inputs, run game
  # times, tells, release screenshot) are left as bordered boxes.

  .xl_abbrev <- function(pt) {
    map <- c("Fastball"="FB","Four-Seam"="FB","FourSeamFastBall"="FB",
             "4-Seam Fastball"="FB","FF"="FB","Sinker"="SNK","Two-Seam"="SNK",
             "TwoSeamFastBall"="SNK","2-Seam Fastball"="SNK","SI"="SNK",
             "Slider"="SL","Sweeper"="SWP","Cutter"="CT","Curveball"="CB",
             "CurveBall"="CB","Knuckle Curve"="KC","Slurve"="SLV",
             "ChangeUp"="CH","Changeup"="CH","Splitter"="SPL")
    out <- map[as.character(pt)]
    ifelse(is.na(out), toupper(substr(as.character(pt), 1, 3)), out)
  }

  .xl_pitch_fill <- function(ab) {
    # single source of truth: the Savant-style pitch_colors palette the
    # movement chart uses, keyed here by report abbreviation so pitch
    # tags, chips, and charts all match across the whole card
    fills <- c(FB  = unname(pitch_colors["Fastball"]),
               SNK = unname(pitch_colors["Sinker"]),
               SL  = unname(pitch_colors["Slider"]),
               SWP = unname(pitch_colors["Sweeper"]),
               CB  = unname(pitch_colors["Curveball"]),
               KC  = unname(pitch_colors["Knuckle Curve"]),
               SLV = unname(pitch_colors["Slurve"]),
               CH  = unname(pitch_colors["ChangeUp"]),
               SPL = unname(pitch_colors["Splitter"]),
               CT  = unname(pitch_colors["Cutter"]))
    out <- fills[ab]
    ifelse(is.na(out), "#6B7280", out)
  }

  .xl_zone_plot <- function(d, point_mode = FALSE) {
    d <- d[!is.na(d$PlateLocSide) & !is.na(d$PlateLocHeight), , drop = FALSE]
    p <- ggplot2::ggplot(d, ggplot2::aes(x = PlateLocSide, y = PlateLocHeight))
    if (nrow(d) >= 8 && !point_mode) {
      p <- p + ggplot2::stat_density_2d(
        ggplot2::aes(fill = ggplot2::after_stat(density)),
        geom = "raster", contour = FALSE, n = 120) +
        ggplot2::scale_fill_gradientn(
          colours = c("white", "#DBEAFE", "#3B82F6", "#FDE047", "#EF4444"),
          guide = "none")
    } else if (nrow(d) > 0) {
      p <- p + ggplot2::geom_point(color = "#EF4444", alpha = 0.75, size = 2.2)
    }
    p +
      ggplot2::annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.38,
                        fill = NA, color = "black", linewidth = 0.7) +
      ggplot2::coord_fixed(xlim = c(-1.9, 1.9), ylim = c(0.4, 4.3), expand = FALSE) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white",
                                                             color = "grey75"))
  }

  # spray chart of damage BIP (needs Bearing + Distance; 2025 parquet
  # rows have them, API rows currently don't)
  .xl_spray_plot <- function(d) {
    ok <- all(c("Bearing", "Distance") %in% names(d))
    if (ok) d <- d[!is.na(d$Bearing) & !is.na(d$Distance), , drop = FALSE]
    if (!ok || nrow(d) == 0) {
      return(ggplot2::ggplot() + ggplot2::theme_void() +
               ggplot2::annotate("text", x = 0, y = 0, size = 3,
                                 label = "No spray data"))
    }
    d$sx <- d$Distance * sin(d$Bearing * pi / 180)
    d$sy <- d$Distance * cos(d$Bearing * pi / 180)
    arc <- function(r) data.frame(x = r * sin(seq(-pi/4, pi/4, length.out = 60)),
                                  y = r * cos(seq(-pi/4, pi/4, length.out = 60)))
    ggplot2::ggplot(d, ggplot2::aes(sx, sy)) +
      ggplot2::geom_path(data = arc(330), ggplot2::aes(x, y),
                         color = "grey55", inherit.aes = FALSE) +
      ggplot2::geom_path(data = arc(150), ggplot2::aes(x, y),
                         color = "grey75", linetype = "dashed", inherit.aes = FALSE) +
      ggplot2::geom_segment(x = 0, y = 0, xend = -233, yend = 233,
                            color = "grey55", linewidth = 0.4) +
      ggplot2::geom_segment(x = 0, y = 0, xend = 233, yend = 233,
                            color = "grey55", linewidth = 0.4) +
      ggplot2::geom_point(color = "#D93025", alpha = 0.8, size = 2) +
      ggplot2::coord_fixed(xlim = c(-260, 260), ylim = c(0, 360)) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white",
                                                             color = "grey75"))
  }

  # previous-pitch x current-pitch transition matrix within each PA
  .xl_seq_plot <- function(d, side_label) {
    d <- d[d$TaggedPitchType != "Other", , drop = FALSE]
    d <- d[order(d$GameID, d$Inning, d$PAofInning, d$PitchofPA), , drop = FALSE]
    key <- paste(d$GameID, d$Inning, d$`Top.Bottom` %||% "", d$PAofInning)
    prev <- ave(as.character(d$TaggedPitchType), key,
                FUN = function(z) c(NA, head(z, -1)))
    tr <- data.frame(prev = prev, cur = as.character(d$TaggedPitchType))
    tr <- tr[!is.na(tr$prev), , drop = FALSE]
    if (nrow(tr) < 10) {
      return(ggplot2::ggplot() + ggplot2::theme_void() +
               ggplot2::annotate("text", x = 0, y = 0, size = 3,
                                 label = "Insufficient sequencing data"))
    }
    tab <- as.data.frame(table(tr$prev, tr$cur), stringsAsFactors = FALSE)
    names(tab) <- c("prev", "cur", "n")
    tab <- dplyr::group_by(tab, prev) |>
      dplyr::mutate(pct = 100 * n / sum(n)) |> dplyr::ungroup()
    ggplot2::ggplot(tab, ggplot2::aes(cur, prev, fill = pct)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(pct >= 1,
                         paste0(round(pct), "%"), "")), size = 3) +
      ggplot2::scale_fill_gradientn(colours = c("#E1463E", "#FFFFFF", "#00840D"),
                                    limits = c(0, 100), guide = "none") +
      ggplot2::labs(title = paste0("Pitch Sequencing vs ", side_label),
                    x = "Current Pitch", y = "Previous Pitch") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                        hjust = 0.5, size = 12),
                     panel.grid = ggplot2::element_blank(),
                     plot.background = ggplot2::element_rect(fill = "white",
                                                             color = NA))
  }

  # ---- Reusable one-sheet Excel scouting card (TruMedia-backed) ----
  # wb: open openxlsx workbook; .sheet: sheet name (created here); gp:
  # pitcher display name; df: pitch data already run through scout_to_feet;
  # notes: list(delivery, stamina, matchup, vs_rhh, vs_lhh) strings.
  # Used by the single-pitcher Export to Excel AND the Advance-tab team
  # Scouting Workbook (one sheet per arm).
  write_scouting_sheet <- function(wb, .sheet, gp, df, notes = list()) {
    # progress ticks are best-effort: works inside withProgress, silent outside
    incProgress <- function(...) tryCatch(shiny::incProgress(...),
                                          error = function(e) invisible())
    openxlsx::addWorksheet(wb, .sheet, gridLines = FALSE)
        df_all <- df  # full history kept for spray charts (Bearing/
                      # Distance only exist on parquet-era rows)
        # TruMedia comparisons are current-season; restrict the report
        # to the TM_SEASON year so computed stats match theirs
        if ("Date" %in% names(df)) {
          cur <- df[!is.na(df$Date) &
                      df$Date >= as.Date(sprintf("%d-01-01", TM_SEASON)), ,
                    drop = FALSE]
          if (nrow(cur) >= 20) df <- cur
        }
        incProgress(0.08, detail = "Computing stats...")

        has  <- function(nm) nm %in% names(df)
        pct_n  <- function(x) ifelse(is.finite(x), round(100 * x, 1), NA_real_)
        avg_n  <- function(x) ifelse(is.finite(x), round(x, 3), NA_real_)
        side_of <- function(d, s) d[toupper(substr(d$BatterSide, 1, 1)) == s, , drop = FALSE]

        u_all <- df %>% dplyr::filter(TaggedPitchType != "Other") %>%
          dplyr::count(TaggedPitchType) %>%
          dplyr::mutate(pct = 100 * n / sum(n),
                        ab  = .xl_abbrev(TaggedPitchType)) %>%
          dplyr::arrange(dplyr::desc(pct))
        top5 <- head(u_all$TaggedPitchType, 5)
        top5_ab <- .xl_abbrev(top5)

        # ---- TruMedia header line ----
        sp <- tryCatch(tm_splits_r(), error = function(e) NULL)
        if (is.null(sp) || is.null(sp$overall)) {
          td_ <- tm_pitcher_team(gp)
          if (!is.null(td_))
            sp <- tryCatch(tm_pitcher_splits(gp, td_), error = function(e) NULL)
          cat("TruMedia header row:",
              if (is.null(sp$overall)) "MISSING (using computed fallbacks)"
              else "OK", "\n")
        }
        tv <- function(split, abbrs) {
          if (is.null(sp) || is.null(sp[[split]])) return("")
          for (ab in abbrs) {
            v <- tm_stat(sp[[split]], ab)
            if (length(v) == 1 && !is.na(v)) return(as.character(v))
          }
          ""
        }
        ba_side_calc <- function(d) avg_n(pdiv(sum(d$HitIndicator, na.rm = TRUE),
                                               sum(d$ABindicator, na.rm = TRUE)))
        str_side <- function(d) pct_n(pdiv(sum(d$StrikeIndicator, na.rm = TRUE),
                                           nrow(d)))
        bio <- tryCatch(bio_lookup(gp, tm_pitcher_team(gp)),
                        error = function(e) NULL)
        # DRS26 is the AUTHORITATIVE jersey/class source for every
        # pitcher (bioinfo.csv only backstops when DRS has nothing)
        drs_r <- tryCatch(drs_lookup(gp, tm_pitcher_team(gp)),
                          error = function(e) NULL)
        if (is.null(bio)) bio <- list()
        has_ <- function(x) !is.null(x) && length(x) > 0 &&
          !is.na(x) && nzchar(as.character(x))
        if (!is.null(drs_r)) {
          if ("Jersey" %in% names(drs_r) && has_(drs_r$Jersey))
            bio$Jersey <- as.character(drs_r$Jersey)
          if (has_(drs_r$Class))
            bio$ClassYear <- as.character(drs_r$Class)
        }
        # bio throwsHand is authoritative — fixes lefties mis-tagged in
        # pitch data (movement chart + expected-movement mirroring)
        bh_ <- toupper(substr(bio$throwsHand %||% "", 1, 1))
        if (bh_ %in% c("L", "R")) {
          df$PitcherThrows <- c(L = "Left", R = "Right")[[bh_]]
          df_all$PitcherThrows <- df$PitcherThrows[1]
        }
        vl <- side_of(df, "L"); vr <- side_of(df, "R")
        vlhh_ba <- tv("vLHH", "BA"); if (!nzchar(vlhh_ba)) vlhh_ba <- ba_side_calc(vl)
        vrhh_ba <- tv("vRHH", "BA"); if (!nzchar(vrhh_ba)) vrhh_ba <- ba_side_calc(vr)
        wl <- {
          w <- tv("overall", "W"); l <- tv("overall", "L")
          if (nzchar(w) || nzchar(l)) paste0(ifelse(nzchar(w), w, "0"), "-",
                                             ifelse(nzchar(l), l, "0")) else ""
        }
        ip_tm <- suppressWarnings(as.numeric(tv("overall", "IP")))
        g_tm  <- suppressWarnings(as.numeric(tv("overall", "G")))
        # computed fallbacks (pitch data) for anything TruMedia missed
        fb_num <- function(tm_val, calc) {
          v <- suppressWarnings(as.numeric(tm_val))
          if (is.finite(v)) v else calc
        }
        k_ct   <- sum(df$KorBB == "Strikeout", na.rm = TRUE)
        h_ct   <- sum(df$HitIndicator, na.rm = TRUE)
        bb_ct  <- sum(df$WalkIndicator, na.rm = TRUE)
        hbp_ct <- sum(df$HBPIndicator, na.rm = TRUE)
        # starts: pitcher threw the game's very first pitch
        gs_ct <- if ("GameID" %in% names(df)) {
          sum(vapply(split(df, df$GameID), function(g_)
            any(g_$Inning == 1 & g_$PAofInning == 1 & g_$PitchofPA == 1,
                na.rm = TRUE), logical(1)))
        } else NA
        if (!is.finite(ip_tm)) {
          outs_est <- sum(df$KorBB == "Strikeout", na.rm = TRUE) +
            sum(df$PlayResult %in% c("Out", "FieldersChoice", "Sacrifice"),
                na.rm = TRUE)
          if (outs_est > 0) ip_tm <- round(outs_est / 3, 1)
        }
        if (!is.finite(g_tm) && has("GameID"))
          g_tm <- dplyr::n_distinct(df$GameID[!is.na(df$GameID)])
        # IP display uses baseball notation (fraction = outs, .0/.1/.2);
        # ratio math (P/IP, IP/G, WHIP, ERA) uses true decimal innings
        ip_disp <- tm_ip_display(ip_tm)
        ip_dec  <- tm_ip_decimal(ip_disp)

        # ---- advanced metrics ----
        adv <- c(
          `K%`   = pct_n(pdiv(sum(df$KorBB == "Strikeout", na.rm = TRUE),
                              sum(df$PAindicator, na.rm = TRUE))),
          `BB%`  = pct_n(pdiv(sum(df$WalkIndicator, na.rm = TRUE),
                              sum(df$PAindicator, na.rm = TRUE))),
          `Barrel%` = pct_n(pdiv(sum(df$BarrelIndicator, na.rm = TRUE),
                                 sum(df$biphh, na.rm = TRUE))),
          `IZBarrel%` = pct_n(pdiv(sum(df$IZBarrelIndicator, na.rm = TRUE),
                                   sum(df$BIPind == 1 & df$StrikeZoneIndicator == 1,
                                       na.rm = TRUE))),
          `HH%`  = pct_n(pdiv(sum(df$HHindicator, na.rm = TRUE),
                              sum(df$biphh, na.rm = TRUE)))
        )

        # ---- last 3 games ----
        last3 <- ""
        if (has("Date")) {
          gtab <- df %>% dplyr::filter(!is.na(Date)) %>%
            dplyr::count(Date) %>% dplyr::arrange(dplyr::desc(Date)) %>% head(3)
          if (nrow(gtab) > 0)
            last3 <- paste(sprintf("%s (%d)",
                                   sub("^0", "", format(gtab$Date, "%m/%d")),
                                   gtab$n), collapse = ", ")
        }

        # ---- pitch metrics table (numeric for CF) ----
        velo_band <- function(pt) {
          d <- df[df$TaggedPitchType == pt & !is.na(df$RelSpeed), , drop = FALSE]
          if (nrow(d) < 5) return("")
          q <- round(stats::quantile(d$RelSpeed, c(0.1, 0.9), na.rm = TRUE))
          paste0(q[1], "-", q[2])
        }
        pm <- df %>% dplyr::filter(TaggedPitchType %in% top5) %>%
          dplyr::group_by(TaggedPitchType) %>%
          dplyr::summarise(
            `%`     = round(100 * dplyr::n() / nrow(df), 0),
            Spin    = round(mean(SpinRate, na.rm = TRUE), 0),
            IVB     = round(mean(InducedVertBreak, na.rm = TRUE), 0),
            HB      = round(mean(HorzBreak, na.rm = TRUE), 0),
            `Ext.`  = round(mean(Extension, na.rm = TRUE), 1),
            `Rel Ht.` = round(mean(RelHeight, na.rm = TRUE), 1),
            .groups = "drop") %>%
          dplyr::arrange(dplyr::desc(`%`)) %>%
          dplyr::mutate(Velo = vapply(TaggedPitchType, velo_band, character(1)),
                        Pitch = .xl_abbrev(TaggedPitchType)) %>%
          dplyr::select(Pitch, `%`, Velo, Spin, IVB, HB, `Ext.`, `Rel Ht.`)

        # ---- per-side results table ----
        side_tbl <- function(d) {
          if (nrow(d) == 0) return(NULL)
          d %>% dplyr::filter(TaggedPitchType %in% top5) %>%
            dplyr::group_by(TaggedPitchType) %>%
            dplyr::summarise(
              `%`       = round(100 * dplyr::n() / nrow(d), 0),
              `Strike%` = pct_n(pdiv(sum(StrikeIndicator, na.rm = TRUE), dplyr::n())),
              `Zone%`   = pct_n(pdiv(sum(StrikeZoneIndicator, na.rm = TRUE), dplyr::n())),
              `Miss%`   = pct_n(pdiv(sum(WhiffIndicator, na.rm = TRUE),
                                     sum(SwingIndicator, na.rm = TRUE))),
              BA        = avg_n(pdiv(sum(HitIndicator, na.rm = TRUE),
                                     sum(ABindicator, na.rm = TRUE))),
              SLG       = avg_n(pdiv(sum(totalbases, na.rm = TRUE),
                                     sum(ABindicator, na.rm = TRUE))),
              .groups = "drop") %>%
            dplyr::arrange(dplyr::desc(`%`)) %>%
            dplyr::mutate(Pitch = .xl_abbrev(TaggedPitchType), .before = 1)
        }
        res_r <- side_tbl(vr); res_l <- side_tbl(vl)

        # prefer TruMedia BA/SLG per pitch type + hand over pitch-data
        # estimates whenever the API supports the combined filter
        tm_tid <- tryCatch(tm_find_team_id(tm_pitcher_team(gp)),
                           error = function(e) NULL)
        enrich_ba <- function(res, hand) {
          if (is.null(res) || is.null(tm_tid)) return(res)
          for (ri in seq_len(nrow(res))) {
            row <- tm_pitch_side_row(tm_tid, gp, hand, res$TaggedPitchType[ri])
            if (!is.null(row)) {
              ba  <- suppressWarnings(as.numeric(tm_stat(row, "BA")))
              slg <- suppressWarnings(as.numeric(tm_stat(row, "SLG")))
              if (is.finite(ba))  res$BA[ri]  <- round(ba, 3)
              if (is.finite(slg)) res$SLG[ri] <- round(slg, 3)
            }
          }
          res
        }
        res_r <- enrich_ba(res_r, "R")
        res_l <- enrich_ba(res_l, "L")
        if (!is.null(res_r)) res_r$TaggedPitchType <- NULL
        if (!is.null(res_l)) res_l$TaggedPitchType <- NULL

        # ---- count usage grids (advance-card rows: no Full Count —
        # 3-2 rolls into 2 Strikes) ----
        count_cat <- function(d) {
          dplyr::case_when(
            d$Balls == 0 & d$Strikes == 0 ~ "First Pitch",
            d$Strikes == 2                ~ "2 Strikes",
            d$Strikes > d$Balls           ~ "Pre 2K",
            d$Balls > d$Strikes           ~ "Hitters Count",
            TRUE ~ NA_character_
          )
        }
        cats <- c("First Pitch", "Pre 2K", "Hitters Count", "2 Strikes")
        usage_grid <- function(d) {
          if (nrow(d) == 0)
            return(matrix(NA_real_, nrow = length(cats), ncol = length(top5),
                          dimnames = list(cats, top5_ab)))
          d$cat <- count_cat(d)
          m <- sapply(top5, function(pt) sapply(cats, function(cc) {
            dd <- d[!is.na(d$cat) & d$cat == cc, , drop = FALSE]
            if (nrow(dd) == 0) NA_real_
            else round(100 * sum(dd$TaggedPitchType == pt) / nrow(dd), 0)
          }))
          m
        }
        risp_rows <- if (has("ManOnSecond"))
          df[which(df$ManOnSecond | df$ManOnThird), , drop = FALSE]
          else df[0, , drop = FALSE]
        # putaway (Kill%) = K per PA that reached two strikes;
        # 1Pk% = strike rate on 0-0. Both also computed on the RISP
        # subset for the SITUATIONAL block.
        kill_pct <- function(d) {
          if (nrow(d) == 0 ||
              !all(c("GameID", "Inning", "PAofInning") %in% names(d)))
            return(NA_real_)
          tb  <- if ("Top.Bottom" %in% names(d))
            as.character(d[["Top.Bottom"]]) else ""
          key <- paste(d$GameID, d$Inning, tb, d$PAofInning)
          two <- tapply(d$Strikes == 2, key, function(z) any(z, na.rm = TRUE))
          ks  <- tapply(d$KorBB == "Strikeout", key,
                        function(z) any(z, na.rm = TRUE))
          idx <- names(two)[which(two %in% TRUE)]
          if (length(idx) == 0) return(NA_real_)
          pct_n(mean(ks[idx] %in% TRUE))
        }
        fps_pct <- function(d) {
          fp <- d[which(d$Balls == 0 & d$Strikes == 0), , drop = FALSE]
          if (nrow(fp) == 0) return(NA_real_)
          pct_n(pdiv(sum(fp$StrikeIndicator, na.rm = TRUE), nrow(fp)))
        }
        adv <- c(adv,
          `HR%`   = pct_n(pdiv(sum(df$PlayResult == "HomeRun", na.rm = TRUE),
                               sum(df$PAindicator, na.rm = TRUE))),
          `Kill%` = kill_pct(df),
          `1Pk%`  = fps_pct(df))
        kill_risp <- kill_pct(risp_rows)
        fps_risp  <- fps_pct(risp_rows)
        grid_all_r  <- usage_grid(vr)
        grid_all_l  <- usage_grid(vl)
        grid_risp_r <- usage_grid(side_of(risp_rows, "R"))
        grid_risp_l <- usage_grid(side_of(risp_rows, "L"))

        # ---- situational stats ----
        n_pitches <- nrow(df)
        p_ip <- if (is.finite(ip_dec) && ip_dec > 0) round(n_pitches / ip_dec, 1) else NA
        ip_g <- if (is.finite(ip_dec) && is.finite(g_tm) && g_tm > 0)
          round(ip_dec / g_tm, 1) else NA
        # LOB% via classic formula; runs allowed from TM if present,
        # else approximated from batting-team run deltas per game
        h_tm  <- suppressWarnings(as.numeric(tv("overall", "H")))
        bb_tm <- suppressWarnings(as.numeric(tv("overall", "BB")))
        hbp_tm<- suppressWarnings(as.numeric(tv("overall", "HBP")))
        hr_tm <- suppressWarnings(as.numeric(tv("overall", "HR")))
        r_tm  <- suppressWarnings(as.numeric(tv("overall", "R")))
        if (!is.finite(h_tm))   h_tm   <- sum(df$HitIndicator, na.rm = TRUE)
        if (!is.finite(bb_tm))  bb_tm  <- sum(df$WalkIndicator, na.rm = TRUE)
        if (!is.finite(hbp_tm)) hbp_tm <- sum(df$HBPIndicator, na.rm = TRUE)
        if (!is.finite(hr_tm))  hr_tm  <- sum(df$PlayResult == "HomeRun", na.rm = TRUE)
        if (!is.finite(r_tm) && has("BattingTeamRuns") && has("GameID")) {
          rt <- df %>% dplyr::filter(!is.na(BattingTeamRuns)) %>%
            dplyr::group_by(GameID) %>%
            dplyr::summarise(ra = max(BattingTeamRuns) - min(BattingTeamRuns),
                             .groups = "drop")
          if (nrow(rt) > 0) r_tm <- sum(rt$ra, na.rm = TRUE)
        }
        lob <- if (all(is.finite(c(h_tm, bb_tm, hbp_tm, hr_tm, r_tm)))) {
          den <- h_tm + bb_tm + hbp_tm - 1.4 * hr_tm
          if (den > 0) round(100 * (h_tm + bb_tm + hbp_tm - r_tm) / den, 1) else NA
        } else NA
        if (is.finite(lob)) lob <- max(0, min(lob, 100))

        # FIP: TruMedia's if available, else classic formula with the
        # standard ~3.10 constant (uncalibrated to league, but stable
        # enough for card-to-card comparison)
        fip <- {
          v <- suppressWarnings(as.numeric(tv("overall", "FIP")))
          if (is.finite(v)) round(v, 2)
          else {
            k_n <- fb_num(tv("overall", c("K", "SO")), k_ct)
            if (is.finite(ip_dec) && ip_dec > 0 &&
                all(is.finite(c(hr_tm, bb_tm, hbp_tm, k_n))))
              round((13 * hr_tm + 3 * (bb_tm + hbp_tm) - 2 * k_n) /
                      ip_dec + 3.10, 2)
            else NA
          }
        }

        # SB--SBA from API stolen base flags
        sb_sba <- if (has("SBAttempt")) {
          paste0(sum(df$SBSuccess, na.rm = TRUE), "--",
                 sum(df$SBAttempt, na.rm = TRUE))
        } else ""

        incProgress(0.3, detail = "Rendering charts...")
        png_of <- function(p, w, h) {
          f <- tempfile(fileext = ".png")
          ok <- tryCatch({ ggplot2::ggsave(f, p, width = w, height = h,
                                           dpi = 140, bg = "white"); TRUE },
                         error = function(e) FALSE)
          if (ok) f else NA_character_
        }

        # movement: Viz-tab chart, big average points + expected
        # diamonds, no individual pitches
        mov_png <- png_of(create_square_movement_plot(df), 3.2, 3.3)

        # location columns are split: top half = overall frequency,
        # bottom half = miss (whiff) locations for that pitch
        loc_img <- function(d, pt) png_of(.xl_zone_plot(
          d[d$TaggedPitchType == pt, , drop = FALSE]), 1.4, 1.6)
        miss_img_pt <- function(d, pt) png_of(.xl_zone_plot(
          d[d$TaggedPitchType == pt & d$WhiffIndicator == 1, , drop = FALSE],
          point_mode = TRUE), 1.4, 1.6)
        locs_r  <- lapply(top5, function(pt) loc_img(vr, pt))
        locs_l  <- lapply(top5, function(pt) loc_img(vl, pt))
        misses_r <- lapply(top5, function(pt) miss_img_pt(vr, pt))
        misses_l <- lapply(top5, function(pt) miss_img_pt(vl, pt))

        dmg <- df[which(df$SolidContact == 1 | df$HHindicator == 1), , drop = FALSE]
        whf <- df[which(df$WhiffIndicator == 1), , drop = FALSE]
        dmg_spray <- df_all[which(df_all$SolidContact == 1 |
                                    df_all$HHindicator == 1), , drop = FALSE]
        spray_r <- png_of(.xl_spray_plot(side_of(dmg_spray, "R")), 1.6, 2.0)
        spray_l <- png_of(.xl_spray_plot(side_of(dmg_spray, "L")), 1.6, 2.0)
        miss_r  <- png_of(.xl_zone_plot(side_of(whf, "R")), 1.6, 2.0)
        miss_l  <- png_of(.xl_zone_plot(side_of(whf, "L")), 1.6, 2.0)
        dmgl_r  <- png_of(.xl_zone_plot(side_of(dmg, "R"), point_mode = TRUE), 1.6, 2.0)
        dmgl_l  <- png_of(.xl_zone_plot(side_of(dmg, "L"), point_mode = TRUE), 1.6, 2.0)

        seq_r <- png_of(.xl_seq_plot(vr, "RHH"), 3.2, 2.4)
        seq_l <- png_of(.xl_seq_plot(vl, "LHH"), 3.2, 2.4)

        # inning charts
        pitch_cols_named <- setNames(.xl_pitch_fill(.xl_abbrev(top5)), top5)
        inn_d <- df[df$TaggedPitchType %in% top5 & !is.na(df$Inning), , drop = FALSE]
        inn_count <- ggplot2::ggplot(inn_d,
            ggplot2::aes(factor(Inning), fill = TaggedPitchType)) +
          ggplot2::geom_bar() +
          ggplot2::scale_fill_manual(values = pitch_cols_named, name = NULL) +
          ggplot2::labs(title = "Pitch Count by Inning", x = "Inning", y = NULL) +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12),
                         legend.position = "bottom",
                         plot.background = ggplot2::element_rect(fill = "white", color = NA))
        inn_usage <- ggplot2::ggplot(inn_d,
            ggplot2::aes(factor(Inning), fill = TaggedPitchType)) +
          ggplot2::geom_bar(position = "fill") +
          ggplot2::scale_y_continuous(labels = function(z) paste0(100 * z, "%")) +
          ggplot2::scale_fill_manual(values = pitch_cols_named, name = NULL) +
          ggplot2::labs(title = "Pitch Usage by Inning", x = "Inning", y = NULL) +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12),
                         legend.position = "bottom",
                         plot.background = ggplot2::element_rect(fill = "white", color = NA))
        inn1_png <- png_of(inn_count, 3.85, 2.6)
        inn2_png <- png_of(inn_usage, 3.85, 2.6)

        # wOBA by times through order
        tto_png <- NA_character_
        pa <- df %>%
          dplyr::filter(!is.na(GameID), !is.na(Batter)) %>%
          dplyr::arrange(GameID, Inning, PAofInning, PitchofPA) %>%
          dplyr::group_by(GameID, Inning, PAofInning, Batter) %>%
          dplyr::summarise(
            is_pa  = max(PAindicator, na.rm = TRUE) == 1,
            k      = any(KorBB == "Strikeout"),
            bb     = any(WalkIndicator == 1, na.rm = TRUE),
            hbp    = any(HBPIndicator == 1, na.rm = TRUE),
            res    = dplyr::last(PlayResult[PlayResult != "Undefined"]),
            .groups = "drop")
        if (nrow(pa) > 0) {
          pa <- pa %>% dplyr::filter(is_pa | k | bb) %>%
            dplyr::group_by(GameID, Batter) %>%
            dplyr::mutate(tto = dplyr::row_number()) %>% dplyr::ungroup() %>%
            dplyr::mutate(tto = pmin(tto, 3),
              w = dplyr::case_when(
                bb ~ 0.69, hbp ~ 0.72,
                res %in% "Single"  ~ 0.89, res %in% "Double" ~ 1.27,
                res %in% "Triple"  ~ 1.62, res %in% "HomeRun" ~ 2.10,
                TRUE ~ 0))
          tto_tab <- pa %>% dplyr::group_by(tto) %>%
            dplyr::summarise(woba = round(mean(w), 3), n = dplyr::n(),
                             .groups = "drop")
          tto_p <- ggplot2::ggplot(tto_tab,
              ggplot2::aes(factor(tto), woba)) +
            ggplot2::geom_col(fill = "#0E6E70", width = 0.6) +
            ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", woba)),
                               vjust = -0.4, size = 3, fontface = "bold") +
            ggplot2::scale_x_discrete(labels = function(z)
              paste0(z, "x (n=", tto_tab$n[match(z, tto_tab$tto)], ")")) +
            ggplot2::labs(title = "wOBA by Times Through Order",
                          x = NULL, y = "wOBA") +
            ggplot2::expand_limits(y = max(tto_tab$woba) * 1.2) +
            ggplot2::theme_minimal(base_size = 11) +
            ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                              size = 12, hjust = 0.5),
                           plot.background = ggplot2::element_rect(fill = "white",
                                                                   color = NA))
          tto_png <- png_of(tto_p, 3.85, 2.6)
        }

        # FB velo by pitch count within outing
        velo_png <- NA_character_
        fbv <- df %>% dplyr::filter(!is.na(GameID)) %>%
          dplyr::arrange(GameID, Inning, PAofInning, PitchofPA) %>%
          dplyr::group_by(GameID) %>%
          dplyr::mutate(pitch_num = dplyr::row_number()) %>% dplyr::ungroup() %>%
          dplyr::filter(FBindicator == 1, !is.na(RelSpeed))
        if (nrow(fbv) >= 10) {
          velo_p <- ggplot2::ggplot(fbv, ggplot2::aes(pitch_num, RelSpeed)) +
            ggplot2::geom_point(color = "#0E6E70", alpha = 0.35, size = 1) +
            ggplot2::geom_smooth(se = FALSE, color = "#B08D57",
                                 linewidth = 1, method = "loess",
                                 formula = y ~ x) +
            ggplot2::labs(title = "FB Velo by Pitch Count",
                          x = "Pitch of Outing", y = "MPH") +
            ggplot2::theme_minimal(base_size = 11) +
            ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                              size = 12, hjust = 0.5),
                           plot.background = ggplot2::element_rect(fill = "white",
                                                                   color = NA))
          velo_png <- png_of(velo_p, 3.85, 2.6)
        }

        incProgress(0.65, detail = "Writing workbook...")
        thr_ch  <- toupper(substr(df$PitcherThrows[1] %||% "", 1, 1))
        if (is.na(thr_ch) || !nzchar(thr_ch))
          thr_ch <- toupper(substr(bio$throwsHand %||% "", 1, 1))
        hand_tag <- if (identical(thr_ch, "L")) " (LHP)"
                    else if (identical(thr_ch, "R")) " (RHP)" else ""

        L1 <- 1; L2 <- 17; CREASE <- 18:25; R1c <- 26; R2c <- 39
        openxlsx::setColWidths(wb, .sheet, cols = L1:L2, widths = 8.2)
        openxlsx::setColWidths(wb, .sheet, cols = CREASE, widths = 2.2)  # fold crease
        openxlsx::setColWidths(wb, .sheet, cols = R1c:R2c, widths = 8.2)
        openxlsx::pageSetup(wb, .sheet, orientation = "landscape",
                            left = 0.1, right = 0.1, top = 0.1, bottom = 0.1,
                            header = 0.05, footer = 0.05)
        # left rail prints as page 1, right rail as page 2
        openxlsx::pageBreak(wb, .sheet, i = max(CREASE), type = "column")

        ROW_IN <- 0.205
        rows_for <- function(h) ceiling(h / ROW_IN) + 1

        S <- openxlsx::createStyle
        st_teal <- S(fgFill = "#0E6E70", fontColour = "#FFFFFF", textDecoration = "bold",
                     halign = "center", valign = "center", fontSize = 14)
        st_tan  <- S(fgFill = "#B08D57", fontColour = "#FFFFFF", textDecoration = "bold",
                     halign = "center", valign = "center", fontSize = 14)
        st_blk  <- S(fgFill = "#111111", fontColour = "#FFFFFF", textDecoration = "bold",
                     halign = "center", valign = "center", fontSize = 14)
        st_navy <- S(fgFill = "#1F2A44", fontColour = "#FFFFFF", textDecoration = "bold",
                     halign = "center", valign = "center", fontSize = 14)
        st_red  <- S(fgFill = "#C0392B", fontColour = "#FFFFFF", textDecoration = "bold",
                     halign = "center", valign = "center", fontSize = 14)
        st_hdr  <- S(fgFill = "#F2F6F6", textDecoration = "bold", halign = "center",
                     valign = "center", fontSize = 13,
                     border = "TopBottomLeftRight", borderColour = "#B7C4C4")
        st_c14  <- S(halign = "center", valign = "center", fontSize = 14,
                     border = "TopBottomLeftRight", borderColour = "#D5DDDD")
        st_c14b <- S(halign = "center", valign = "center", fontSize = 14,
                     textDecoration = "bold",
                     border = "TopBottomLeftRight", borderColour = "#D5DDDD")
        st_ba14 <- S(halign = "center", valign = "center", numFmt = "0.000",
                     fontSize = 14, border = "TopBottomLeftRight",
                     borderColour = "#D5DDDD")
        st_h20  <- S(halign = "center", valign = "center", fontSize = 20,
                     textDecoration = "bold",
                     border = "TopBottomLeftRight", borderColour = "#D5DDDD")
        st_h20name <- S(halign = "center", valign = "center", fontSize = 20,
                     textDecoration = "bold", fontColour = "#C0392B",
                     border = "TopBottomLeftRight", borderColour = "#D5DDDD")
        st_h20ba <- S(halign = "center", valign = "center", numFmt = "0.000",
                     fontSize = 20, textDecoration = "bold",
                     border = "TopBottomLeftRight", borderColour = "#D5DDDD")
        st_h20era <- S(halign = "center", valign = "center", numFmt = "0.00",
                     fontSize = 20, textDecoration = "bold",
                     border = "TopBottomLeftRight", borderColour = "#D5DDDD")
        st_h20ip <- S(halign = "center", valign = "center", numFmt = "0.0",
                     fontSize = 20, textDecoration = "bold",
                     border = "TopBottomLeftRight", borderColour = "#D5DDDD")
        st_m20  <- S(halign = "center", valign = "center", fontSize = 20,
                     textDecoration = "bold",
                     border = "TopBottomLeftRight", borderColour = "#D5DDDD")
        st_m20d <- S(halign = "center", valign = "center", fontSize = 20,
                     textDecoration = "bold", numFmt = "0.0",
                     border = "TopBottomLeftRight", borderColour = "#D5DDDD")
        st_box  <- S(wrapText = TRUE, valign = "top", fontSize = 14,
                     border = "TopBottomLeftRight", borderColour = "#111111")
        st_appr <- S(wrapText = TRUE, valign = "center", halign = "center",
                     textDecoration = "bold", fontSize = 14,
                     border = "TopBottomLeftRight", borderColour = "#111111")
        st_1dec16 <- S(halign = "center", valign = "center", numFmt = "0.0",
                     fontSize = 16, textDecoration = "bold",
                     border = "TopBottomLeftRight", borderColour = "#D5DDDD")
        st_1dec20 <- S(halign = "center", valign = "center", numFmt = "0.0",
                     fontSize = 20, textDecoration = "bold",
                     border = "TopBottomLeftRight", borderColour = "#D5DDDD")
        st_c20b <- S(halign = "center", valign = "center", fontSize = 20,
                     textDecoration = "bold",
                     border = "TopBottomLeftRight", borderColour = "#D5DDDD")

        put <- function(txt, row, c1, c2 = c1, style = NULL, rowspan = 1) {
          openxlsx::writeData(wb, .sheet, txt, startRow = row, startCol = c1)
          if (c2 > c1 || rowspan > 1)
            openxlsx::mergeCells(wb, .sheet, cols = c1:c2, rows = row:(row + rowspan - 1))
          if (!is.null(style))
            openxlsx::addStyle(wb, .sheet, style, rows = row:(row + rowspan - 1),
                               cols = c1:c2, gridExpand = TRUE)
        }
        img <- function(path, row, col, w, h) {
          if (!is.na(path) && file.exists(path))
            openxlsx::insertImage(wb, .sheet, path, startRow = row, startCol = col,
                                  width = w, height = h, units = "in")
        }
        # Auto-fit an image into a drawn box: borders the cell range,
        # then inserts the image sized to fill it (small inset so the
        # border stays visible). Returns the first row AFTER the box.
        # row_h: per-row height in inches (0.205 default rows; pass
        # 0.293 when the rows were set to 22pt).
        COL_IN <- 0.646   # 8.2-char column ≈ 62 px ≈ 0.646 in
        st_imgbox <- S(border = "TopBottomLeftRight",
                       borderColour = "#111111")
        boxed_img <- function(path, r1, c1, c2, nrows = NULL, h_in = NULL,
                              row_h = ROW_IN) {
          if (is.null(nrows)) nrows <- max(2, ceiling(h_in / row_h))
          put("", r1, c1, c2, st_imgbox, rowspan = nrows)
          if (!is.na(path) && file.exists(path)) {
            openxlsx::insertImage(
              wb, .sheet, path, startRow = r1, startCol = c1,
              width  = (c2 - c1 + 1) * COL_IN * 0.985,
              height = nrows * row_h * 0.96, units = "in")
          }
          r1 + nrows
        }
        chip <- function(ab, row, c1, c2 = c1) {
          put(paste0(ab, "%"), row, c1, c2,
              S(fgFill = .xl_pitch_fill(ab), fontColour = "#FFFFFF",
                textDecoration = "bold", halign = "center", valign = "center",
                fontSize = 12, border = "TopBottomLeftRight",
                borderColour = "#111111"))
        }
        pitch_cell <- function(ab, row, col, rowspan = 1, fs = 14) {
          put(ab, row, col, col,
              S(fgFill = .xl_pitch_fill(ab), fontColour = "#FFFFFF",
                textDecoration = "bold", halign = "center", valign = "center",
                fontSize = fs, border = "TopBottomLeftRight",
                borderColour = "#111111"), rowspan = rowspan)
        }
        cf <- function(rows, cols, low_good = FALSE) {
          sty <- if (low_good) c("#00840D", "#FFFFFF", "#E1463E")
                 else          c("#E1463E", "#FFFFFF", "#00840D")
          openxlsx::conditionalFormatting(wb, .sheet, cols = cols, rows = rows,
                                          type = "colourScale", style = sty)
        }
        # CF rule styles: openxlsx bakes a full <font> block into the
        # dxf even when only a fill is requested, so a bare bgFill rule
        # silently resets flagged cells to 11pt Calibri — and manual
        # font changes appear "stuck" because the live rule re-applies
        # it. All cf_thr cells are 20pt bold stats, so carry that font
        # in the dxf too.
        g_fill <- S(bgFill = "#C6EFCE", fontSize = 20, textDecoration = "bold")
        r_fill <- S(bgFill = "#FFC7CE", fontSize = 20, textDecoration = "bold")
        cf_thr <- function(row, col, good, bad, high_good = TRUE) {
          rules <- if (high_good) c(paste0(">=", good), paste0("<=", bad))
                   else           c(paste0("<=", good), paste0(">=", bad))
          openxlsx::conditionalFormatting(wb, .sheet, cols = col, rows = row,
                                          rule = rules[1], style = g_fill)
          openxlsx::conditionalFormatting(wb, .sheet, cols = col, rows = row,
                                          rule = rules[2], style = r_fill)
        }
        numify <- function(x) suppressWarnings(as.numeric(x))

        # ================= LEFT RAIL =================
        openxlsx::setRowHeights(wb, .sheet, rows = 1, heights = 22)
        openxlsx::setRowHeights(wb, .sheet, rows = 2:3, heights = 21)
        hdr_lab <- c("#", "Pitcher Name", "Class", "G", "IP", "W-L", "H",
                     "BB", "K", "HBP", "vLHH", "vRHH", "ERA", "WHIP", "FIP")
        hdr_c1  <- c(1, 2, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
        hdr_c2  <- c(1, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
        for (i in seq_along(hdr_lab)) put(hdr_lab[i], 1, hdr_c1[i], hdr_c2[i], st_teal)
        # values merged across rows 2:3 (except vLHH/vRHH: BA over Str%)
        put(numify(bio$Jersey %||% NA), 2, 1, 1, st_h20, rowspan = 2)
        put(paste0(gp, hand_tag), 2, 2, 4,
            if (identical(thr_ch, "L")) st_h20name else st_h20, rowspan = 2)
        put(bio$ClassYear %||% "", 2, 5, 5, st_h20, rowspan = 2)
        put(if (is.finite(g_tm)) g_tm else numify(tv("overall", "G")),
            2, 6, 6, st_h20, rowspan = 2)
        put(if (is.finite(ip_disp)) ip_disp else NA, 2, 7, 7, st_h20ip, rowspan = 2)
        put(wl, 2, 8, 8, st_h20, rowspan = 2)
        put(fb_num(tv("overall", "H"), h_ct),   2, 9, 9, st_h20, rowspan = 2)
        put(fb_num(tv("overall", "BB"), bb_ct), 2, 10, 10, st_h20, rowspan = 2)
        put(fb_num(tv("overall", c("K", "SO")), k_ct), 2, 11, 11, st_h20, rowspan = 2)
        put(fb_num(tv("overall", "HBP"), hbp_ct), 2, 12, 12, st_h20, rowspan = 2)
        put(numify(vlhh_ba), 2, 13, 13, st_h20ba)
        put(numify(vrhh_ba), 2, 14, 14, st_h20ba)
        put(str_side(vl), 3, 13, 13, st_1dec20)
        put(str_side(vr), 3, 14, 14, st_1dec20)
        era_fb <- if (is.finite(r_tm) && is.finite(ip_dec) && ip_dec > 0)
          round(9 * r_tm / ip_dec, 2) else NA
        whip_fb <- if (is.finite(ip_dec) && ip_dec > 0)
          round((h_ct + bb_ct) / ip_dec, 2) else NA
        put(fb_num(tv("overall", "ERA"), era_fb),   2, 15, 15, st_h20era, rowspan = 2)
        put(fb_num(tv("overall", "WHIP"), whip_fb), 2, 16, 16, st_h20era, rowspan = 2)
        put(fip, 2, 17, 17, st_h20era, rowspan = 2)
        cf_thr(2, 13, 0.220, 0.280, high_good = FALSE)
        cf_thr(2, 14, 0.220, 0.280, high_good = FALSE)
        cf_thr(3, 13, 65, 60); cf_thr(3, 14, 65, 60)
        cf_thr(2, 15, 3.60, 5.40, high_good = FALSE)
        cf_thr(2, 16, 1.25, 1.60, high_good = FALSE)
        cf_thr(2, 17, 3.80, 5.20, high_good = FALSE)

        put("Delivery Notes", 4, 1, 4, st_tan)
        put("Last 3 Games", 4, 5, 9, st_tan)
        put("ADVANCED METRICS", 4, 10, 17, st_tan)
        put(notes$delivery %||% "", 5, 1, 4, st_box, rowspan = 3)
        put(last3, 5, 5, 9, st_appr, rowspan = 3)
        adv_thr <- list(`K%` = c(25, 17, TRUE), `BB%` = c(8, 12, FALSE),
                        `Barrel%` = c(5, 9, FALSE), `IZBarrel%` = c(6, 10, FALSE),
                        `HH%` = c(32, 42, FALSE), `HR%` = c(1.5, 3.5, FALSE),
                        `Kill%` = c(30, 20, TRUE), `1Pk%` = c(62, 55, TRUE))
        for (i in seq_along(adv)) {
          cc <- 9 + i
          put(names(adv)[i], 5, cc, cc, st_hdr)
          put(unname(adv[i]), 6, cc, cc, st_c20b, rowspan = 2)
          th <- adv_thr[[names(adv)[i]]]
          if (!is.null(th)) cf_thr(6, cc, th[1], th[2], high_good = as.logical(th[3]))
        }

        # ---- PITCH METRICS: full-width centerpiece, 2 rows x 2 cols ----
        put("PITCH METRICS", 8, 1, 17, st_teal)
        openxlsx::setRowHeights(wb, .sheet, rows = 9, heights = 24)
        put("Pitch", 9, 1, 1, st_hdr)
        put("%",     9, 2, 3, st_hdr)
        put("Velo",  9, 4, 5, st_hdr)
        put("Spin",  9, 6, 7, st_hdr)
        put("IVB",   9, 8, 9, st_hdr)
        put("HB",    9, 10, 11, st_hdr)
        put("Ht.",   9, 12, 12, st_hdr)
        put("NOTES", 9, 13, 17, st_hdr)
        n_pm <- nrow(pm)
        for (ri in seq_len(n_pm)) {
          r_ <- 10 + 2 * (ri - 1)
          openxlsx::setRowHeights(wb, .sheet, rows = r_:(r_ + 1), heights = 20)
          pitch_cell(pm$Pitch[ri], r_, 1, rowspan = 2, fs = 18)
          put(pm$`%`[ri],       r_, 2, 3,  st_m20,  rowspan = 2)
          put(pm$Velo[ri],      r_, 4, 5,  st_m20,  rowspan = 2)
          put(pm$Spin[ri],      r_, 6, 7,  st_m20,  rowspan = 2)
          put(pm$IVB[ri],       r_, 8, 9,  st_m20,  rowspan = 2)
          put(pm$HB[ri],        r_, 10, 11, st_m20, rowspan = 2)
          put(pm$`Rel Ht.`[ri], r_, 12, 12, st_m20d, rowspan = 2)
          put("", r_, 13, 17, st_m20, rowspan = 2)
        }
        if (n_pm > 1) {
          pm_rows <- 10:(9 + 2 * n_pm)
          cf(pm_rows, 2); cf(pm_rows, 6)
        }
        mr0 <- 10 + 2 * n_pm + 1

        # movement | release photo | overall summary — three panels
        # evenly spaced, one blank gap column on each side of each
        # (gaps at 1, 7, 12, 17)
        put("Movement (Diamonds = Expected)", mr0, 2, 6, st_tan)
        put("Release Point", mr0, 8, 11, st_tan)
        put("OVERALL SUMMARY:", mr0, 13, 16, st_tan)
        mov_end <- boxed_img(mov_png, mr0 + 1, 2, 6, h_in = 3.3)
        mov_rows <- mov_end - (mr0 + 1)
        put("Paste release screenshot here", mr0 + 1, 8, 11, st_box,
            rowspan = mov_rows)
        put(paste(c(notes$stamina %||% "",
                    notes$matchup %||% ""), collapse = "  "),
            mr0 + 1, 13, 16, st_box, rowspan = mov_rows)
        lc <- mov_end + 1

        side_block <- function(r0, label, bar_style, res, locs, misses,
                               spray, missp, dmgp, appr_txt) {
          put(label, r0, 1, 17, bar_style)
          rh <- r0 + 1
          res_h <- c("Pitch", "%", "Strike%", "Zone%", "Miss%", "BA", "SLG")
          for (i in seq_along(res_h)) put(res_h[i], rh, i, i, st_hdr)
          loc_c1 <- c(8, 10, 12, 14, 16)
          loc_c2 <- c(9, 11, 13, 15, 17)
          for (k in seq_along(locs)) {
            ab <- .xl_abbrev(top5[k])
            put(paste(ab, "Locations"), rh, loc_c1[k], loc_c2[k],
                S(fgFill = .xl_pitch_fill(ab), fontColour = "#FFFFFF",
                  textDecoration = "bold", halign = "center", fontSize = 11))
          }
          # each pitch takes TWO rows (like the pitch metrics table);
          # location columns split vertically — top half frequency,
          # bottom half miss locations
          n_res    <- if (is.null(res)) 0 else nrow(res)
          box_rows <- max(2 * n_res, 8)
          half     <- floor(box_rows / 2)
          openxlsx::setRowHeights(wb, .sheet, rows = (rh + 1):(rh + box_rows),
                                  heights = 22)
          if (n_res > 0) {
            for (ri in seq_len(n_res)) {
              r_ <- rh + 1 + 2 * (ri - 1)
              pitch_cell(res$Pitch[ri], r_, 1, rowspan = 2, fs = 16)
              for (ci in 2:ncol(res)) {
                sty <- if (names(res)[ci] %in% c("BA", "SLG")) st_ba14 else st_c14
                put(res[[ci]][ri], r_, ci, ci, sty, rowspan = 2)
              }
            }
            if (n_res > 1) {
              body <- (rh + 1):(rh + 2 * n_res)
              cf(body, 2); cf(body, 3); cf(body, 4); cf(body, 5)
              cf(body, 6, low_good = TRUE); cf(body, 7, low_good = TRUE)
            }
          }
          for (k in seq_along(locs)) {
            boxed_img(locs[[k]], rh + 1, loc_c1[k], loc_c2[k],
                      nrows = half, row_h = 0.293)
            boxed_img(misses[[k]], rh + 1 + half, loc_c1[k], loc_c2[k],
                      nrows = box_rows - half, row_h = 0.293)
          }
          rb <- rh + box_rows + 1
          put("Miss", rb, 1, 2, st_blk)
          put("Damage", rb, 3, 4, st_blk)
          put("Damage Spray", rb, 5, 6, st_blk)
          put(sub("^Vs\\. ", "", paste(label, "Approach")), rb, 7, 17, st_blk)
          sm_rows <- rows_for(1.35)
          boxed_img(missp, rb + 1, 1, 2, nrows = sm_rows)
          boxed_img(dmgp,  rb + 1, 3, 4, nrows = sm_rows)
          boxed_img(spray, rb + 1, 5, 6, nrows = sm_rows)
          put(appr_txt, rb + 1, 7, 17, st_appr, rowspan = sm_rows)
          rb + 1 + sm_rows + 1
        }
        lc <- side_block(lc, "Vs. Right Handed Hitters", st_navy, res_r,
                         locs_r, misses_r, spray_r, miss_r, dmgl_r,
                         notes$vs_rhh %||% "")
        lc <- side_block(lc, "Vs. Left Handed Hitters", st_red, res_l,
                         locs_l, misses_l, spray_l, miss_l, dmgl_l,
                         notes$vs_lhh %||% "")

        put("RUN GAME", lc, 1, 17, st_tan)
        rg_h  <- c("Situation", "Leg Times", "Hold Time", "Pick Times",
                   "Overall Run Notes", "SB--SBA")
        rg_c1 <- c(1, 3, 5, 7, 9, 15); rg_c2 <- c(2, 4, 6, 8, 14, 17)
        for (i in seq_along(rg_h)) put(rg_h[i], lc + 1, rg_c1[i], rg_c2[i], st_hdr)
        put("R1", lc + 2, 1, 2, st_teal, rowspan = 2)
        put("R2", lc + 4, 1, 2, st_teal, rowspan = 2)
        for (rr_ in c(lc + 2, lc + 4)) {
          put("", rr_, 3, 4, st_box, rowspan = 2)
          put("", rr_, 5, 6, st_box, rowspan = 2)
          put("", rr_, 7, 8, st_box, rowspan = 2)
        }
        put("", lc + 2, 9, 14, st_box, rowspan = 4)
        put(sb_sba, lc + 2, 15, 17, st_appr, rowspan = 4)
        left_end <- lc + 6

        # ================= RIGHT RAIL (26:39, fold crease at 18:20) ==
        cats_disp <- c("First Pitch", "Pre 2K", "Hitters Count", "2 Strikes")
        write_usage_grid <- function(r0, title, bar_style, gr, gl) {
          put(title, r0, R1c, R2c, bar_style)
          rh <- r0 + 1
          put("Vs. RHH", rh, 26, 27, st_navy)
          put("Vs. LHH", rh, 33, 34, st_red)
          nk <- min(5, ncol(gr), length(top5_ab))
          for (k in seq_len(nk)) {
            chip(top5_ab[k], rh, 27 + k)
            chip(top5_ab[k], rh, 34 + k)
          }
          for (ri in seq_along(cats)) {
            r_ <- rh + 1 + 2 * (ri - 1)
            openxlsx::setRowHeights(wb, .sheet, rows = r_:(r_ + 1), heights = 22)
            put(cats_disp[ri], r_, 26, 27, st_hdr, rowspan = 2)
            put(cats_disp[ri], r_, 33, 34, st_hdr, rowspan = 2)
            for (k in seq_len(nk)) {
              put(gr[ri, k], r_, 27 + k, 27 + k, st_c14, rowspan = 2)
              put(gl[ri, k], r_, 34 + k, 34 + k, st_c14, rowspan = 2)
            }
          }
          body <- (rh + 1):(rh + 2 * length(cats))
          cf(body, 28:32); cf(body, 35:39)
          rh + 2 * length(cats) + 1
        }
        rr0 <- write_usage_grid(1, "Overall Count Usage", st_teal,
                                grid_all_r, grid_all_l)
        rr0 <- write_usage_grid(rr0 + 1, "RISP Count Usage", st_tan,
                                grid_risp_r, grid_risp_l) + 1

        put("SEQUENCING", rr0, 26, 35, st_blk)
        put("OVERALL USAGE NOTES", rr0, 36, 39, st_teal)
        seq_end <- boxed_img(seq_r, rr0 + 1, 26, 30, h_in = 2.4)
        boxed_img(seq_l, rr0 + 1, 31, 35, h_in = 2.4)
        seq_rows <- seq_end - (rr0 + 1) + 1
        put("RHH", rr0 + 1, 36, 39, st_blk)
        put("", rr0 + 2, 36, 39, st_box, rowspan = ceiling((seq_rows - 3) / 2))
        mid_r <- rr0 + 2 + ceiling((seq_rows - 3) / 2)
        put("LHH", mid_r, 36, 39, st_red)
        # one extra row on the LHH notes box past the seq matrices
        put("", mid_r + 1, 36, 39, st_box,
            rowspan = max(2, seq_end - mid_r))
        rr0 <- seq_end + 1

        put("SITUATIONAL", rr0, 26, 39, st_tan)
        put("LOB%",       rr0 + 1, 26, 27, st_hdr)
        put("P/IP",       rr0 + 1, 28, 29, st_hdr)
        put("IP/G",       rr0 + 1, 30, 31, st_hdr)
        put("RISP Kill%", rr0 + 1, 32, 35, st_hdr)
        put("RISP 1PK%",  rr0 + 1, 36, 39, st_hdr)
        # stat values span two rows, 20pt
        openxlsx::setRowHeights(wb, .sheet, rows = (rr0 + 2):(rr0 + 3), heights = 22)
        put(lob,       rr0 + 2, 26, 27, st_1dec20, rowspan = 2)
        put(p_ip,      rr0 + 2, 28, 29, st_1dec20, rowspan = 2)
        put(ip_g,      rr0 + 2, 30, 31, st_1dec20, rowspan = 2)
        put(kill_risp, rr0 + 2, 32, 35, st_1dec20, rowspan = 2)
        put(fps_risp,  rr0 + 2, 36, 39, st_1dec20, rowspan = 2)
        cf_thr(rr0 + 2, 26, 75, 68, high_good = TRUE)
        cf_thr(rr0 + 2, 28, 15.5, 17.5, high_good = FALSE)
        cf_thr(rr0 + 2, 32, 35, 25, high_good = TRUE)
        cf_thr(rr0 + 2, 36, 62, 55, high_good = TRUE)
        # 2x2 chart grid: two-column gap (32:33) between the columns
        # and one gap row between the top and bottom charts
        g_r0 <- rr0 + 5
        ch_end <- boxed_img(tto_png, g_r0, 26, 31, h_in = 2.6)
        boxed_img(inn1_png, g_r0, 34, 39, h_in = 2.6)
        ch_rows <- ch_end - g_r0
        boxed_img(velo_png, g_r0 + ch_rows + 1, 26, 31, h_in = 2.6)
        boxed_img(inn2_png, g_r0 + ch_rows + 1, 34, 39, h_in = 2.6)
        rr0 <- g_r0 + 2 * ch_rows + 2
        # velo/stamina takeaway strip (hand-typed, like the card)
        put("", rr0, 26, 39, st_appr, rowspan = 4)
        rr0 <- rr0 + 5

        put("PITCHER FIELDING NOTES", rr0, 26, 32, st_teal)
        put("TELL NOTES", rr0, 33, 39, st_blk)
        note_span <- 6
        put("", rr0 + 1, 26, 32, st_box, rowspan = note_span)
        put("", rr0 + 1, 33, 39, st_box, rowspan = note_span)

    invisible(TRUE)
  }

  output$download_scouting_xlsx <- downloadHandler(
    filename = function() {
      gp <- input$global_pitcher; req(gp)
      paste0(gsub(" ", "_", gp), "_Scouting_Report_",
             format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      gp <- input$global_pitcher
      req(gp, nzchar(gp))
      withProgress(message = "Building scouting report...", value = 0, {
        wb <- openxlsx::createWorkbook()
        write_scouting_sheet(
          wb, "Scouting Report", gp, scout_data(),
          notes = list(delivery = input$pdf_delivery_notes %||% "",
                       stamina  = input$pdf_stamina_notes %||% "",
                       matchup  = input$pdf_matchup_notes %||% "",
                       vs_rhh   = input$pdf_vs_rhh_notes %||% "",
                       vs_lhh   = input$pdf_vs_lhh_notes %||% ""))
        incProgress(0.95, detail = "Saving...")
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      })
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
