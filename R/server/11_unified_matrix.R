  # ============ BETA: MATCHUP+ ADVANCED MATRIX (process model) ============
  # Deliberately reuses mm_db() / mm_scaler() / the mm_ roster selections so
  # the beta grid is a like-for-like second read on exactly the pitches the
  # DRE matrix priced. Nothing here mutates production state.
  mmp_matrix   <- reactiveVal(NULL)
  mmp_detail   <- reactiveVal(NULL)   # list(p = pkey, b = bkey)

  output$mmp_selection_note <- renderUI({
    p_lbl <- .mm_team_display(.mm_team_label(input$mm_pitcher_team %||% ""))
    b_lbl <- .mm_team_display(.mm_team_label(input$mm_batter_team  %||% ""))
    np <- length(input$mm_pitchers %||% character(0))
    nb <- length(input$mm_batters  %||% character(0))
    if (np == 0 || nb == 0)
      return(div(style = "color:#B45309; font-size:12px; line-height:1.45;",
                 tags$b("No selection yet."), tags$br(),
                 "Pick a pitcher team and a batter team on the Matchup Matrix tab."))
    div(style = "color:#6B7280; font-size:12px; line-height:1.45;",
        tags$b(sprintf("%d arms \u00D7 %d bats", np, nb)), tags$br(),
        span(style = "color:#0B3D3E; font-weight:650;",
             paste0(p_lbl, " pitchers vs ", b_lbl, " batters")))
  })

  observeEvent(input$mmp_build, {
    if (length(input$mm_pitchers %||% character(0)) == 0 ||
        length(input$mm_batters  %||% character(0)) == 0) {
      showNotification(paste0("Select a pitcher team and a batter team on the ",
                              "Matchup Matrix tab first \u2014 Matchup+ reads the ",
                              "same rosters."), type = "warning", duration = 7)
      return()
    }
    res <- withProgress(message = "Building Matchup+ matrix...", value = 0, {
      # the league basis has to exist before any plus conversion happens
      setProgress(0.02, detail = "Loading hitter process-model basis...")
      ok <- tryCatch(hp_build_league(), error = function(e) FALSE)
      if (!isTRUE(ok)) return(NULL)
      lay <- input$mmu_layers %||% character(0)
      build_unified_matchup_matrix(
        mm_db(), input$mm_pitchers, input$mm_batters, mm_scaler(),
        progress_callback = function(frac, msg) setProgress(frac, detail = msg),
        sim_threshold = input$mmp_sim_threshold %||% SIMILARITY_THRESHOLD,
        shrink_k = input$mmp_shrink %||% MMP_CRED_K,
        pitcher_weight = input$mm_pitcher_weight %||% 0.5,
        # the DRE pass and the head-to-head sweep both cost real time, so only
        # run them when a layer actually needs them
        want_dre = "dre" %in% lay ||
                   !identical(input$mmu_primary %||% "predicted", "predicted"),
        want_h2h = "h2h" %in% lay)
    })
    if (is.null(res)) {
      showNotification(paste0("Hitter process model unavailable \u2014 the league ",
                              "expectation tables could not be built."),
                       type = "error", duration = 8)
      return()
    }
    mmp_matrix(res)
    mmp_detail(NULL)
  })

  # ---- shared parse of the packed per-pitch string ----
  .mmp_parse <- function(s) {
    if (is.null(s) || length(s) != 1 || is.na(s) || !nzchar(s)) return(NULL)
    kv <- strsplit(strsplit(s, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
    num <- function(k) suppressWarnings(as.numeric(
      vapply(kv, function(z) z[k] %||% NA_character_, character(1))))
    d <- data.frame(pt  = vapply(kv, function(z) z[1], character(1)),
                    prc = num(2), dec = num(3), con = num(4), pow = num(5),
                    n   = num(6), stringsAsFactors = FALSE)
    d$n[is.na(d$n)] <- 0
    d[is.finite(d$prc), , drop = FALSE]
  }

  # ---- the grid ----
  output$mmp_matrix_ui <- renderUI({
    m <- mmp_matrix()
    if (is.null(m) || nrow(m) == 0)
      return(div(class = "adv-placeholder",
                 p("Set the teams and rosters on the Matchup Matrix tab, then ",
                   "hit Build Matchup+ Matrix.")))

    view    <- input$mmp_view %||% "hitter"
    primary <- input$mmu_primary %||% "predicted"
    lay     <- input$mmu_layers %||% c("layers", "arsenal", "profile")
    show_layers <- "layers"     %in% lay
    show_pt     <- "arsenal"    %in% lay
    show_dre    <- "dre"        %in% lay
    show_h2h    <- "h2h"        %in% lay
    show_profile <- "profile"   %in% lay
    sel     <- mmp_detail()

    .fmt <- function(x) if (is.null(x) || !is.finite(x)) "\u2014" else as.character(round(x))
    # MatchupScore is a 0-100 sigmoid; map it onto the 40-160 plus ramp so one
    # colour function serves both headlines without inventing a second palette
    # vectorised: the relative-colour path passes the whole column through this
    .dre_as_plus <- function(s) {
      if (is.null(s) || length(s) == 0) return(numeric(0))
      s <- suppressWarnings(as.numeric(s))
      ifelse(is.finite(s), 40 + (s / 100) * 120, NA_real_)
    }

    # Relative mode re-centres the colour ramp on THIS grid's own mean and
    # spread. Against a lineup that is uniformly above average the absolute
    # ramp paints every cell the same shade and the grid stops discriminating;
    # relative mode restores contrast without touching the numbers.
    ctr <- 100; spr <- 1
    if (isTRUE(input$mmp_relative)) {
      vv <- if (identical(primary, "actual") && "DreHitter" %in% names(m))
        .dre_as_plus(m$DreHitter[is.finite(m$DreHitter)])
      else m$ProcessPlus[is.finite(m$ProcessPlus)]
      if (length(vv) >= 4) {
        s <- stats::sd(vv)
        if (is.finite(s) && s > 1e-6) { ctr <- mean(vv); spr <- s }
      }
    }
    pcol <- function(v) mmp_plus_color(v, view, ctr, spr)

    m <- m %>% dplyr::mutate(
      .pkey = paste0(Pitcher, "||", ifelse(is.na(PitcherTeam), "", PitcherTeam)),
      .bkey = paste0(Batter,  "||", ifelse(is.na(BatterTeam),  "", BatterTeam)))

    # ---- one column per ARM, not per (arm, hand) pair ----------------------
    # distinct() over PitcherThrows split a pitcher into two identical columns
    # whenever his hand was recorded on some rows and missing on others -- the
    # duplicate "#48 Josh Swink (L)" / "#48 Josh Swink" pair. Collapse on the
    # key alone and carry the first non-missing hand forward. Same for the
    # batter side.
    .first_known <- function(x) {
      x <- x[!is.na(x) & nzchar(as.character(x))]
      if (length(x) == 0) NA_character_ else as.character(x[1])
    }
    pit <- m %>%
      dplyr::group_by(.pkey) %>%
      dplyr::summarise(Pitcher = dplyr::first(Pitcher),
                       PitcherThrows = .first_known(PitcherThrows),
                       .groups = "drop") %>%
      as.data.frame(stringsAsFactors = FALSE)
    bat <- m %>%
      dplyr::group_by(.bkey) %>%
      dplyr::summarise(Batter = dplyr::first(Batter),
                       BatterSide = .first_known(BatterSide),
                       .groups = "drop") %>%
      as.data.frame(stringsAsFactors = FALSE)
    .col <- function(nm, i, default = NA_real_)
      if (nm %in% names(m)) m[[nm]][i] else default
    # a duplicated (arm, bat) pair would silently shadow itself in the lookup
    m <- m[!duplicated(paste0(m$.pkey, "\u241F", m$.bkey)), , drop = FALSE]
    cellmap <- setNames(
      lapply(seq_len(nrow(m)), function(i) list(
        prc = m$ProcessPlus[i],  dec = m$DecisionPlus[i],
        con = m$ContactPlus[i],  pow = m$PowerPlus[i],
        dre = .col("DreHitter", i),
        h2h_pa = .col("H2H_PA", i), h2h_h  = .col("H2H_H", i),
        h2h_hr = .col("H2H_HR", i), h2h_k  = .col("H2H_K", i),
        h2h_bb = .col("H2H_BB", i),
        lc  = isTRUE(m$LowConfidence[i]),
        nsim = m$TotalSimilar[i], pool = m$PoolN[i],
        pt  = if ("PitchPlus" %in% names(m)) m$PitchPlus[i] else NA_character_)),
      paste0(m$.pkey, "\u241F", m$.bkey))

    .team_of_key <- function(k) {
      t <- strsplit(k, "||", fixed = TRUE)[[1]]
      if (length(t) >= 2 && nzchar(t[2])) .mm_team_display(t[2]) else NULL
    }
    .hand_class <- function(h)
      if (identical(h, "L")) "mm-lefty" else if (identical(h, "S")) "mm-switch" else NULL

    p_lbl <- .mm_team_display(.mm_team_label(input$mm_pitcher_team %||% ""))
    b_lbl <- .mm_team_display(.mm_team_label(input$mm_batter_team  %||% ""))
    sub <- paste0(p_lbl, " Pitchers vs ", b_lbl, " Batters \u2022 ",
                  if (identical(view, "hitter")) "Hitter Facing" else "Pitcher Facing",
                  " \u2022 ",
                  switch(primary,
                         actual = "DRE score",
                         both   = "Process+ and DRE",
                         "Process+"),
                  if (isTRUE(input$mmp_relative)) " \u2022 colour relative to this grid" else "")

    head_cells <- c(
      list(tags$th(class = "mm-bh", "Batter")),
      lapply(seq_len(nrow(pit)), function(j) {
        thr <- substr(pit$PitcherThrows[j] %||% "", 1, 1)
        d <- drs_info(pit$Pitcher[j], .team_of_key(pit$.pkey[j]))
        top <- paste0(if (nzchar(d$jersey)) paste0("#", d$jersey, " ") else "",
                      pit$Pitcher[j])
        sub_bits <- c(if (nzchar(thr) && !is.na(thr)) paste0("(", thr, ")"),
                      if (nzchar(d$class)) d$class)
        tags$th(
          div(class = paste("tp-player-link", .hand_class(thr)),
              `data-name` = pit$Pitcher[j], `data-side` = "pitcher", top),
          div(class = paste("mm-hand", .hand_class(thr)),
              paste(sub_bits, collapse = " \u00B7 ")))
      }))

    # one sub-pill: DEC / CON / POW. Coloring uses the same perspective flip
    # as the headline so a cell never mixes two color conventions.
    subpill <- function(lab, v, tip) {
      na <- is.null(v) || !is.finite(v)
      bg <- if (na) "#EDEFF1" else pcol(v)
      tags$span(class = "mmp-subpill",  # layers are always hitter-scale
                style = paste0("background:", bg, "; color:",
                               if (na) "#9AA1A6" else pill_text_color(bg), ";"),
                title = tip,
                tags$span(class = "mmp-sl", lab),
                tags$span(class = "mmp-sv",
                          if (na) "\u2014" else as.character(round(v))))
    }

    body_rows <- lapply(seq_len(nrow(bat)), function(i) {
      side <- substr(bat$BatterSide[i] %||% "", 1, 1)
      d <- drs_info(bat$Batter[i], .team_of_key(bat$.bkey[i]))
      nm <- paste0(if (nzchar(d$jersey)) paste0("#", d$jersey, " ") else "",
                   bat$Batter[i])
      meta_bits <- c(if (nzchar(side) && !is.na(side)) paste0("(", side, ")"),
                     if (nzchar(d$pos)) d$pos,
                     if (nzchar(d$class)) d$class)
      drs_pill <- if (!is.na(d$overall)) {
        .bg <- drs_pill_color(d$overall)
        tags$span(class = "mm-drs-pill",
                  style = paste0("background:", .bg, "; color:",
                                 pill_text_color(.bg), ";"),
                  sprintf("%+d", round(as.numeric(d$overall))))
      } else NULL

      # ---- detailed hitter profile ----
      # Replaces the old single season strip: the same four grades resolved by
      # platoon AND count state, plus the types he punishes / struggles with.
      # Thin cells print a dash rather than borrowing the overall number.
      season_row <- if (show_profile) {
        pf <- tryCatch(hp_hitter_profile(mm_pool_batter(), bat$Batter[i],
                                         .team_of_key(bat$.bkey[i])),
                       error = function(e) NULL)
        if (is.null(pf)) NULL else {
          gp <- function(lab, v, tip) {
            na <- is.null(v) || !is.finite(v)
            .bg <- if (na) "#EDEFF1" else grade_pill_color(v)
            tags$span(class = paste("hpf-pill", if (na) "hpf-na"),
                      style = if (na) NULL else
                        paste0("background:", .bg, "; color:", pill_text_color(.bg), ";"),
                      title = tip,
                      tags$span(class = "hpf-lab", lab),
                      tags$span(class = "hpf-val",
                                if (na) "\u2014" else as.character(round(v))))
          }
          # each layer carries its own denominator into the tooltip, so a dash
          # says WHICH sample was short rather than just "not enough"
          grade_row <- function(cell, rowlab, sidelab, call = NULL) {
            tip <- function(k, n, unit, need) sprintf("%s %s, %s \u2014 %s",
              sidelab, rowlab, k,
              if (is.null(cell)) sprintf("under %d pitches, not shown", pf$min_cell)
              else if (!is.finite(n) || n < need)
                sprintf("only %d %s (needs %d), withheld", n %||% 0L, unit, need)
              else sprintf("%d %s", n, unit))
            call_chip <- if (is.null(call)) NULL else {
              tags$span(class = "hpf-call",
                        title = sprintf(
                          "%s in this count: Process+ %d over %d pitches%s. %s",
                          call$pt, round(call$prc), as.integer(call$n),
                          if (is.finite(call$margin %||% NA_real_))
                            sprintf(", %d points clear of the next-worst offering",
                                    round(call$margin)) else "",
                          "Only shown when one pitch is a clear outlier."),
                        "\u2192 ", pitch_abbrev(call$pt))
            }
            div(class = "hpf-row",
                gp("PRC", cell$prc %||% NA_real_,
                   tip("Process+", cell$n %||% NA_integer_, "pitches", pf$min_cell)),
                gp("CON", cell$con %||% NA_real_,
                   tip("Contact+", cell$n_swing %||% NA_integer_, "swings",
                       HP_PROFILE_MIN_SWING)),
                gp("POW", cell$pow %||% NA_real_,
                   tip("Power+", cell$n_bip %||% NA_integer_, "batted balls",
                       HP_PROFILE_MIN_BIP)),
                gp("DEC", cell$dec %||% NA_real_,
                   tip("Decision+", cell$n %||% NA_integer_, "pitches", pf$min_cell)),
                call_chip)
          }
          tag_strip <- function(tab, kind) {
            lab <- if (identical(kind, "pun")) "Punishes" else "Struggles"
            if (is.null(tab) || nrow(tab) == 0)
              return(div(class = "hpf-tagline",
                         tags$span(class = "hpf-tagcap", lab),
                         tags$span(class = "hpf-tagnone",
                                   title = sprintf("No pitch type cleared %d pitches AND %d plus points off average",
                                                   pf$min_type, HP_PROFILE_EDGE),
                                   "\u2014")))
            div(class = "hpf-tagline",
                tags$span(class = "hpf-tagcap", lab),
                lapply(seq_len(nrow(tab)), function(k) {
                  .b <- grade_pill_color(tab$prc[k])
                  tags$span(class = "hpf-tag",
                            style = paste0("background:", .b, "; color:",
                                           pill_text_color(.b), ";"),
                            title = sprintf("%s: Process+ %d over %d pitches",
                                            tab$pt[k], round(tab$prc[k]),
                                            as.integer(tab$n[k])),
                            pitch_abbrev(tab$pt[k]))
                }))
          }
          side_col <- function(sb, sidelab) {
            if (is.null(sb) || sb$n < pf$min_cell)
              return(div(class = "hpf-side",
                         div(class = "hpf-sidecap", sidelab),
                         div(class = "hpf-thin",
                             sprintf("%d pitches \u2014 too thin", sb$n %||% 0))))
            div(class = "hpf-side",
                div(class = "hpf-sidecap", sidelab),
                div(class = "hpf-grid",
                    div(class = "hpf-rlab", "OVR"), grade_row(sb$ovr, "overall", sidelab),
                    div(class = "hpf-rlab", "1P"),
                    grade_row(sb$fp, "first pitch", sidelab, sb$call_fp),
                    div(class = "hpf-rlab", "2K"),
                    grade_row(sb$tk, "two strikes", sidelab, sb$call_tk)),
                tag_strip(sb$punishes, "pun"), tag_strip(sb$struggles, "str"))
          }
          div(class = "hpf-wrap",
              side_col(pf$R, "vRHP"), side_col(pf$L, "vLHP"))
        }
      } else NULL

      name_td <- tags$td(class = "mm-namecell",
        div(class = "mm-hitter-head",
            div(class = "mm-hitter-main",
                tags$span(class = paste("mm-name", "tp-player-link", .hand_class(side)),
                          `data-name` = bat$Batter[i], `data-side` = "hitter", nm),
                tags$span(class = "mm-hand", paste(meta_bits, collapse = " \u00B7 "))),
            drs_pill),
        season_row)

      cell_tds <- lapply(seq_len(nrow(pit)), function(j) {
        key <- paste0(pit$.pkey[j], "\u241F", bat$.bkey[i])
        cm  <- cellmap[[key]]
        v   <- if (is.null(cm)) NA_real_ else cm$prc
        dv  <- if (is.null(cm)) NA_real_ else cm$dre
        na  <- !is.finite(v) && !is.finite(dv)
        # colour always follows the headline the user chose
        head_v <- if (identical(primary, "actual")) .dre_as_plus(dv) else v
        bg  <- if (!is.finite(head_v)) "#EDEFF1" else pcol(head_v)
        fg  <- if (!is.finite(head_v)) "#9AA1A6" else pill_text_color(bg)
        is_sel <- !is.null(sel) && identical(sel$p, pit$.pkey[j]) &&
          identical(sel$b, bat$.bkey[i])
        cls <- paste("mmp-cell",
                     if (!is.null(cm) && isTRUE(cm$lc)) "mmp-lc",
                     if (na) "mmp-na", if (is_sel) "mmp-sel")

        # ---- headline ----
        head_block <- if (identical(primary, "both")) {
          div(class = "mmu-dual",
              div(class = "mmu-half",
                  div(class = "mmp-score", .fmt(v)),
                  div(class = "mmp-sublab", "Prc+")),
              div(class = "mmu-half mmu-half-r",
                  div(class = "mmp-score", .fmt(dv)),
                  div(class = "mmp-sublab", "DRE")))
        } else if (identical(primary, "actual")) {
          tagList(div(class = "mmp-score", .fmt(dv)),
                  div(class = "mmp-sublab", "DRE Score"))
        } else {
          tagList(div(class = "mmp-score", .fmt(v)),
                  div(class = "mmp-sublab",
                      "Process+"))
        }

        # ---- optional: Dec / Con / Pow ----
        lay_row <- if (show_layers && is.finite(v))
          div(class = "mmp-subrow",
              subpill("Dec", cm$dec, "Decision+ \u2014 swing/take vs what the pitch was worth"),
              subpill("Con", cm$con, "Contact+ \u2014 bat-to-ball given the decision"),
              subpill("Pow", cm$pow, "Power+ \u2014 contact quality given contact")) else NULL

        # ---- optional: secondary DRE line (when it is not already the headline) ----
        dre_row <- if (show_dre && !identical(primary, "actual") &&
                       !identical(primary, "both") && is.finite(dv)) {
          .b <- pcol(.dre_as_plus(dv))
          div(class = "mmu-strip",
              tags$span(class = "mmu-chip",
                        style = paste0("background:", .b, "; color:", pill_text_color(.b), ";"),
                        title = paste0("Actual DRE score, shown HITTER-facing like every ",
                                       "other number here (100 - the production tab's ",
                                       "pitcher-facing value). Higher = the bat has ",
                                       "really produced against this arsenal."),
                        paste0("DRE ", round(dv))))
        } else NULL

        # ---- optional: head-to-head ----
        h2h_row <- if (show_h2h && !is.null(cm)) {
          pa <- cm$h2h_pa
          if (!is.finite(pa) || pa <= 0)
            div(class = "mmu-strip", tags$span(class = "mmu-h2h mmu-h2h-none",
                title = "These two have never faced each other in the pool", "no H2H"))
          else div(class = "mmu-strip",
              tags$span(class = "mmu-h2h",
                        title = sprintf("Actual head-to-head: %d PA, %d H, %d HR, %d K, %d BB. Anecdote, not a projection.",
                                        as.integer(pa), as.integer(cm$h2h_h %||% 0),
                                        as.integer(cm$h2h_hr %||% 0), as.integer(cm$h2h_k %||% 0),
                                        as.integer(cm$h2h_bb %||% 0)),
                        sprintf("%dPA \u00B7 %dH%s", as.integer(pa),
                                as.integer(cm$h2h_h %||% 0),
                                if ((cm$h2h_hr %||% 0) > 0)
                                  sprintf(" \u00B7 %dHR", as.integer(cm$h2h_hr)) else "")))
        } else NULL

        # ---- optional: per-pitch arsenal pills ----
        pt_row <- NULL
        if (show_pt && is.finite(v)) {
          pd <- .mmp_parse(cm$pt)
          if (!is.null(pd) && nrow(pd) > 0) {
            pd <- pd[order(if (identical(view, "hitter")) -pd$prc else pd$prc), ,
                     drop = FALSE]
            pd <- head(pd, 4)
            pt_row <- div(class = "mmp-ptrow", lapply(seq_len(nrow(pd)), function(k) {
              .b <- pcol(pd$prc[k])
              tags$span(class = paste("mmp-ptpill",
                                      if (pd$n[k] < MMP_MIN_SIMILAR) "mmp-ptlc"),
                        style = paste0("background:", .b, "; color:",
                                       pill_text_color(.b), ";"),
                        title = sprintf("%s: Prc %d (Dec %d / Con %d / Pow %d) \u2014 %d similar",
                                        pd$pt[k], round(pd$prc[k]), round(pd$dec[k]),
                                        round(pd$con[k]), round(pd$pow[k]),
                                        as.integer(pd$n[k])),
                        pitch_abbrev(pd$pt[k]))
            }))
          }
        }

        tags$td(div(
          class = cls,
          style = paste0("background:", bg, "; color:", fg, ";"),
          `data-p` = pit$.pkey[j], `data-b` = bat$.bkey[i],
          title = if (na) "No usable similar-pitch sample" else
            sprintf("Predicted %s / Actual %s \u2014 Dec %s / Con %s / Pow %s \u00B7 %d similar pitches from a %d-pitch pool",
                    .fmt(v), .fmt(dv),
                    if (is.finite(cm$dec)) round(cm$dec) else "\u2014",
                    if (is.finite(cm$con)) round(cm$con) else "\u2014",
                    if (is.finite(cm$pow)) round(cm$pow) else "\u2014",
                    as.integer(cm$nsim %||% 0), as.integer(cm$pool %||% 0)),
          head_block, lay_row, dre_row, pt_row, h2h_row))
      })
      tags$tr(name_td, cell_tds)
    })

    div(class = "mm-card",
        div(class = "mm-card-header",
            span(class = "mm-title", "Unified Matchup Matrix"),
            span(class = "mmp-beta-tag", "Beta"),
            span(class = "mm-sub", sub)),
        tags$table(class = "mm-table",
                   tags$thead(tags$tr(head_cells)),
                   tags$tbody(body_rows)))
  })

  # ---- plain-English read under the grid ----
  output$mmp_notes <- renderUI({
    m <- mmp_matrix()
    if (is.null(m) || nrow(m) == 0) return(NULL)
    v <- m[is.finite(m$ProcessPlus), , drop = FALSE]
    if (nrow(v) == 0) return(NULL)
    p_lbl <- .mm_team_display(.mm_team_label(input$mm_pitcher_team %||% ""))
    b_lbl <- .mm_team_display(.mm_team_label(input$mm_batter_team  %||% ""))

    avg <- mean(v$ProcessPlus)
    verdict <- if (avg >= 104) {
      sprintf("%s bats process this staff well \u2014 average Process+ %d across %d matchups.",
              b_lbl, round(avg), nrow(v))
    } else if (avg <= 96) {
      sprintf("%s arms win the process battle \u2014 average Process+ %d across %d matchups.",
              p_lbl, round(avg), nrow(v))
    } else {
      sprintf("Even on process \u2014 average Process+ %d across %d matchups.",
              round(avg), nrow(v))
    }

    pair_line <- function(rows) paste(
      vapply(seq_len(nrow(rows)), function(i)
        sprintf("%s vs %s (%d)", rows$Pitcher[i], rows$Batter[i],
                round(rows$ProcessPlus[i])), character(1)),
      collapse = " \u00B7 ")
    best_bat <- head(v[order(-v$ProcessPlus), , drop = FALSE], 3)
    best_arm <- head(v[order(v$ProcessPlus), , drop = FALSE], 3)

    p_avg <- tapply(v$ProcessPlus, v$Pitcher, mean)
    b_avg <- tapply(v$ProcessPlus, v$Batter,  mean)

    # which layer is the staff actually winning?
    lay <- c(Decision = mean(v$DecisionPlus, na.rm = TRUE),
             Contact  = mean(v$ContactPlus,  na.rm = TRUE),
             Power    = mean(v$PowerPlus,    na.rm = TRUE))
    lay <- lay[is.finite(lay)]
    layer_line <- if (length(lay) > 0) sprintf(
      "Layer split across the grid: %s. The lineup's strongest layer is %s (%d); the staff suppresses %s best (%d).",
      paste(sprintf("%s %d", names(lay), round(lay)), collapse = " \u00B7 "),
      names(lay)[which.max(lay)], round(max(lay)),
      names(lay)[which.min(lay)], round(min(lay))) else NULL

    lines <- c(
      sprintf("Best bat matchups: %s.", pair_line(best_bat)),
      sprintf("Best arm matchups: %s.", pair_line(best_arm)),
      sprintf("%s handles this lineup best (avg %d); %s has the toughest draw (avg %d).",
              names(p_avg)[which.min(p_avg)], round(min(p_avg)),
              names(p_avg)[which.max(p_avg)], round(max(p_avg))),
      sprintf("Biggest process threat: %s (avg %d). Most attackable bat: %s (avg %d).",
              names(b_avg)[which.max(b_avg)], round(max(b_avg)),
              names(b_avg)[which.min(b_avg)], round(min(b_avg))),
      layer_line)
    if (any(m$LowConfidence, na.rm = TRUE))
      lines <- c(lines, sprintf(
        "%d of %d cells lean on the platoon prior (dashed) \u2014 thin similar-pitch samples.",
        sum(m$LowConfidence, na.rm = TRUE), nrow(m)))

    div(class = "mm-gameplan", style = "margin: 0 0 14px;",
        div(class = "mm-gameplan-title", "Matchup+ Overview"),
        tags$p(style = "font-weight:650; color:#0B3D3E; margin:0 0 6px;", verdict),
        tags$ul(style = "margin:0; padding-left:18px;",
                lapply(Filter(Negate(is.null), lines), function(x) tags$li(x))))
  })

  # ---- click a cell -> per-pitch-type breakdown ----
  observeEvent(input$mmp_cell_click, {
    cc <- input$mmp_cell_click
    req(cc$p, cc$b)
    mmp_detail(list(p = cc$p, b = cc$b))
    session$sendCustomMessage("mmpScrollDetail", list(nonce = Sys.time()))
  })

  output$mmp_detail_ui <- renderUI({
    sel <- mmp_detail(); m <- mmp_matrix()
    if (is.null(sel) || is.null(m) || nrow(m) == 0) return(NULL)
    view <- input$mmp_view %||% "hitter"
    mm <- m %>% dplyr::mutate(
      .pkey = paste0(Pitcher, "||", ifelse(is.na(PitcherTeam), "", PitcherTeam)),
      .bkey = paste0(Batter,  "||", ifelse(is.na(BatterTeam),  "", BatterTeam)))
    row <- mm[mm$.pkey == sel$p & mm$.bkey == sel$b, , drop = FALSE]
    if (nrow(row) == 0) return(NULL)
    row <- row[1, , drop = FALSE]

    pd <- .mmp_parse(row$PitchPlus)
    dpill <- function(v) {
      if (is.null(v) || !is.finite(v)) return(tags$span(class = "mmp-dpill",
        style = "background:#EDEFF1; color:#9AA1A6;", "\u2014"))
      bg <- mmp_plus_color(v, view)
      tags$span(class = "mmp-dpill",
                style = paste0("background:", bg, "; color:", pill_text_color(bg), ";"),
                as.character(round(v)))
    }

    tbl <- if (is.null(pd) || nrow(pd) == 0)
      div(style = "color:#9AA1A6; font-size:13px; padding:14px 0;",
          "No pitch type in this arsenal cleared the minimum sample.")
    else tags$table(class = "mmp-dtable",
      tags$thead(tags$tr(
        tags$th("Pitch"), tags$th("Process+"), tags$th("Decision+"),
        tags$th("Contact+"), tags$th("Power+"), tags$th("Similar pitches"))),
      tags$tbody(lapply(seq_len(nrow(pd)), function(k) tags$tr(
        tags$td(pd$pt[k]),
        tags$td(dpill(pd$prc[k])), tags$td(dpill(pd$dec[k])),
        tags$td(dpill(pd$con[k])), tags$td(dpill(pd$pow[k])),
        tags$td(style = if (pd$n[k] < MMP_MIN_SIMILAR)
          "color:#B45309; font-weight:650;" else NULL,
          sprintf("%d%s", as.integer(pd$n[k]),
                  if (pd$n[k] < MMP_MIN_SIMILAR) " \u00B7 prior" else ""))))))

    div(class = "mm-card", style = "margin-top:4px;",
      div(class = "mm-card-header",
          span(class = "mm-title",
               sprintf("%s vs %s", row$Pitcher[1], row$Batter[1])),
          span(class = "mmp-beta-tag", "Matchup+"),
          span(class = "mm-sub", sprintf(
            "%s\u2011handed arm vs %s\u2011handed bat \u2022 %d similar pitches drawn from a %d\u2011pitch pool%s",
            substr(row$PitcherThrows[1] %||% "?", 1, 1),
            substr(row$BatterSide[1] %||% "?", 1, 1),
            as.integer(row$TotalSimilar[1] %||% 0),
            as.integer(row$PoolN[1] %||% 0),
            if (isTRUE(row$LowConfidence[1])) " \u2014 low confidence" else ""))),
      div(style = "padding:14px 22px 18px;",
          div(style = "display:flex; gap:10px; flex-wrap:wrap; margin-bottom:14px;",
              lapply(list(c("Process+", row$ProcessPlus[1]),
                          c("Decision+", row$DecisionPlus[1]),
                          c("Contact+", row$ContactPlus[1]),
                          c("Power+", row$PowerPlus[1])), function(z) {
                v <- suppressWarnings(as.numeric(z[2]))
                bg <- mmp_plus_color(v, view)
                div(style = paste0("min-width:96px; text-align:center; padding:8px 14px; ",
                                   "border-radius:10px; background:", bg, "; color:",
                                   if (is.finite(v)) pill_text_color(bg) else "#9AA1A6", ";"),
                    div(style = "font-size:9px; font-weight:800; letter-spacing:.07em; text-transform:uppercase; opacity:.75;",
                        z[1]),
                    div(style = "font-size:20px; font-weight:850;",
                        if (is.finite(v)) as.character(round(v)) else "\u2014"))
              })),
          tbl,
          div(style = "color:#9AA1A6; font-size:11.5px; margin-top:12px; line-height:1.5;",
              "Each row is the hitter's process against the pitches in his history that sit ",
              "inside the similarity radius of that offering. Pitch types marked ",
              tags$b("prior"), " fell under the similar-pitch minimum and are shown at ",
              "his platoon-wide baseline. Arsenal totals are usage-weighted against ",
              "this hitter's side."),
          # ---- optional: the hitter's whiff / xSLG zone maps ----
          # Reuses the same mm_mini_heatmap panels the production detail view
          # builds, so the two tabs cannot disagree about what a hitter's zone
          # looks like. Behind a toggle because each pair is four ggplots.
          if ("heatmaps" %in% (input$mmu_layers %||% character(0))) {
            det <- tryCatch(mm_hitter_detail(row$Batter[1], sel$b),
                            error = function(e) NULL)
            hs <- det$heats
            if (is.null(hs)) div(style = "color:#9AA1A6; font-size:11.5px; margin-top:10px;",
                                 "Heatmaps unavailable for this hitter.")
            else tagList(
              tags$hr(style = "margin:14px 0 10px; border-color:#ECEFF1;"),
              div(class = "hpf-heats",
                  lapply(list(c("missR", "Whiff vs RHP"), c("missL", "Whiff vs LHP"),
                              c("xslgR", "xSLG vs RHP"), c("xslgL", "xSLG vs LHP")),
                         function(z) {
                           p <- hs[[z[1]]]
                           if (is.null(p)) return(NULL)
                           div(class = "hpf-heat",
                               div(class = "hpf-heat-cap", z[2]),
                               {
                                 uri <- tryCatch(.gg_data_uri(p), error = function(e) NULL)
                                 if (is.null(uri)) tags$span(class = "hpf-tagnone", "\u2014")
                                 else tags$img(src = uri,
                                               style = "width:132px; height:auto;")
                               })
                         })))
          }))
  })

  output$mmp_download_csv <- downloadHandler(
    filename = function() sprintf("matchup_plus_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      m <- mmp_matrix()
      if (is.null(m) || nrow(m) == 0) {
        utils::write.csv(data.frame(note = "No matrix built"), file, row.names = FALSE)
        return()
      }
      utils::write.csv(m, file, row.names = FALSE, na = "")
    })

  # Plain-English overview under the matrix: favored team, best and
  # toughest matchups, standout arms and bats.
  output$mm_overview_notes <- renderUI({
    m <- mm_matrix()
    if (is.null(m) || nrow(m) == 0) return(NULL)
    v <- m[!is.na(m$MatchupScore), , drop = FALSE]
    if (nrow(v) == 0) return(NULL)
    view <- input$mm_view %||% "pitcher"
    disp <- function(s) if (identical(view, "hitter")) 100 - s else s
    p_lbl <- .mm_team_display(.mm_team_label(input$mm_pitcher_team %||% ""))
    b_lbl <- .mm_team_display(.mm_team_label(input$mm_batter_team %||% ""))

    avg <- mean(v$MatchupScore)
    verdict <- if (avg >= 55) {
      sprintf("%s arms are favored overall \u2014 average score %d/100 across %d matchups.",
              p_lbl, round(disp(avg)), nrow(v))
    } else if (avg <= 45) {
      sprintf("%s bats are favored overall \u2014 average score %d/100 across %d matchups.",
              b_lbl, round(disp(avg)), nrow(v))
    } else {
      sprintf("Even matchup on paper \u2014 average score %d/100 across %d matchups.",
              round(disp(avg)), nrow(v))
    }

    pair_line <- function(rows) paste(
      vapply(seq_len(nrow(rows)), function(i)
        sprintf("%s vs %s (%d)", rows$Pitcher[i], rows$Batter[i],
                round(disp(rows$MatchupScore[i]))), character(1)),
      collapse = " \u00B7 ")
    best_p  <- head(v[order(-v$MatchupScore), , drop = FALSE], 3)
    worst_p <- head(v[order(v$MatchupScore), , drop = FALSE], 3)

    p_avg <- tapply(v$MatchupScore, v$Pitcher, mean)
    b_avg <- tapply(v$MatchupScore, v$Batter, mean)
    arm_best  <- names(p_avg)[which.max(p_avg)]
    arm_worst <- names(p_avg)[which.min(p_avg)]
    bat_threat <- names(b_avg)[which.min(b_avg)]
    bat_easy   <- names(b_avg)[which.max(b_avg)]

    lines <- c(
      sprintf("Best pitcher matchups: %s.", pair_line(best_p)),
      sprintf("Toughest assignments: %s.", pair_line(worst_p)),
      sprintf("%s profiles best against this lineup (avg %d); %s has the toughest draw (avg %d).",
              arm_best, round(disp(p_avg[[arm_best]])),
              arm_worst, round(disp(p_avg[[arm_worst]]))),
      sprintf("Biggest threat in the lineup: %s (avg %d). Most favorable bat to attack: %s (avg %d).",
              bat_threat, round(disp(b_avg[[bat_threat]])),
              bat_easy, round(disp(b_avg[[bat_easy]]))))
    if (any(m$LowConfidence, na.rm = TRUE))
      lines <- c(lines, sprintf("%d of %d cells are low confidence (dashed) \u2014 weigh those lightly.",
                                sum(m$LowConfidence, na.rm = TRUE), nrow(m)))

    div(class = "mm-gameplan", style = "margin: 0 0 14px;",
        div(class = "mm-gameplan-title", "Overview"),
        div(class = "mm-overview-verdict", verdict),
        tags$ul(lapply(lines, tags$li)))
  })

  # Cell click -> open the per-pitch detail table for that pair
  mm_detail_pair <- reactiveVal(NULL)
  mm_detail_pt   <- reactiveVal(NULL)
  observeEvent(input$mm_cell_click, {
    pk <- input$mm_cell_click$p; bk <- input$mm_cell_click$b
    if (is.null(pk) || is.null(bk)) return()
    mm_detail_pair(list(p = pk, b = bk))
    mm_detail_pt(NULL)
    session$sendCustomMessage("mmScrollDetail", TRUE)
  })
  # RV/100 pill click -> reveal movement + location charts for that pitch
  observeEvent(input$mm_rv_click, {
    pt <- input$mm_rv_click$pt
    if (is.null(pt) || !nzchar(pt)) return()
    mm_detail_pt(pt)
  })

  output$mm_download_pdf <- downloadHandler(
    filename = function() paste0("matchup_matrix_", Sys.Date(), ".pdf"),
    content = function(file) {
      m <- mm_matrix()
      req(!is.null(m), nrow(m) > 0)
      # Detailed export reads the SAME session caches the UI render filled
      # (mm_hitter_detail / mm_hitter_grades), so the printed columns,
      # heatmaps, grades and notes always match what's on screen.
      create_matchup_matrix_pdf(
        m, file, view = input$mm_view %||% "pitcher",
        pitcher_team_lbl = .mm_team_display(.mm_team_label(input$mm_pitcher_team %||% "")),
        batter_team_lbl  = .mm_team_display(.mm_team_label(input$mm_batter_team %||% "")),
        detailed = isTRUE(input$mm_detailed),
        detail_fetch = function(batter, bkey)
          mm_hitter_detail(batter, bkey,
                           tryCatch(drs_info(batter, .mm_key_team(bkey)),
                                    error = function(e) list())),
        grades_fetch = mm_hitter_grades)
    },
    contentType = "application/pdf"
  )

  # ---- Per-matchup detail: per-pitch stats table + drill-in charts ----
  mm_detail_res <- reactive({
    pair <- mm_detail_pair()
    req(!is.null(pair))
    pk <- split_pitcher_key(pair$p)
    bk <- split_batter_key(pair$b)
    req(nzchar(pk$name %||% ""), nzchar(bk$name %||% ""))
    tryCatch(
      calculate_matchup(mm_db(), pk$name, bk$name, mm_scaler(),
                        pitcher_team = pk$team, batter_team = bk$team,
                        pitcher_weight = input$mm_pitcher_weight %||% 0.5,
                        sim_threshold = input$mm_sim_threshold %||% SIMILARITY_THRESHOLD),
      error = function(e) {
        showNotification(paste("Matchup detail failed:", conditionMessage(e)),
                         type = "error", duration = 6)
        NULL
      })
  })

  mm_detail_pp <- reactive({
    res <- mm_detail_res()
    pt <- mm_detail_pt()
    req(!is.null(res), !is.null(pt))
    res$per_pitch[[pt]]
  })

  .MM_DETAIL_STATS <- c("AvgEV", "BA", "SLG", "Whiff%", "Chase%", "CSW%",
                        "HardHit%", "wOBA", "K%")

  # Rule-based plain-English game plan (pitcher perspective) built from the
  # per-pitch RVs and similar-pitch stats of a computed matchup.
  mm_gameplan <- function(res) {
    pps <- res$per_pitch
    if (length(pps) == 0) return(character(0))
    info <- lapply(names(pps), function(pt) {
      pp <- pps[[pt]]
      st <- similar_pool_stats(pp$similar)
      list(pt = pt, ab = pitch_abbrev(pt),
           rv = pp$rv100 %||% NA_real_, n = pp$n_similar %||% 0,
           usage = pp$usage_vs_hand %||% 0,
           chase = st[["Chase%"]], whiff = st[["Whiff%"]],
           hh = st[["HardHit%"]], ev = st[["AvgEV"]],
           slg = st[["SLG"]], woba = st[["wOBA"]], k = st[["K%"]])
    })
    ok_rv <- Filter(function(x) is.numeric(x$rv) && length(x$rv) == 1 &&
                      !is.na(x$rv), info)
    lines <- character(0)
    add <- function(s) lines <<- c(lines, s)
    fmt3 <- function(v) sub("^0", "", sprintf("%.3f", v))
    v_ok <- function(v) !is.null(v) && length(v) == 1 && !is.na(v)

    if (length(ok_rv) > 0) {
      best  <- ok_rv[[which.min(vapply(ok_rv, function(x) x$rv, numeric(1)))]]
      worst <- ok_rv[[which.max(vapply(ok_rv, function(x) x$rv, numeric(1)))]]
      add(sprintf("%s is his best overall option (%+.1f RV/100 vs similar%s).",
                  best$ab, best$rv,
                  if (best$n < 10) ", small sample" else ""))
      dmg <- (v_ok(worst$slg) && worst$slg >= 0.500) ||
             (v_ok(worst$hh) && worst$hh >= 40) ||
             (v_ok(worst$woba) && worst$woba >= 0.420)
      if (!identical(worst$pt, best$pt) && (worst$rv > 0.4 || dmg)) {
        bits <- c(if (v_ok(worst$slg)) paste0("SLG ", fmt3(worst$slg)),
                  if (v_ok(worst$hh))  paste0(round(worst$hh), "% hard hit"))
        add(sprintf("Can do damage vs %s%s \u2014 keep it off the plate when behind.",
                    worst$ab,
                    if (length(bits)) paste0(" (", paste(bits, collapse = ", "), ")") else ""))
      }
    }

    ch_ok <- Filter(function(x) v_ok(x$chase), info)
    if (length(ch_ok) > 0) {
      chb <- ch_ok[[which.max(vapply(ch_ok, function(x) x$chase, numeric(1)))]]
      mean_chase <- mean(vapply(ch_ok, function(x) x$chase, numeric(1)), na.rm = TRUE)
      if (chb$chase >= 28) {
        add(sprintf("Use %s to hunt chases late in counts (%d%% chase on similar).",
                    chb$ab, round(chb$chase)))
      }
      if (mean_chase <= 22 && length(ok_rv) > 0) {
        early <- ok_rv[[which.min(vapply(ok_rv, function(x)
          if (v_ok(x$woba)) x$woba else Inf, numeric(1)))]]
        if (v_ok(early$woba)) {
          add(sprintf(paste0("Patient hitter (%d%% avg chase) \u2014 attack the zone ",
                             "early; %s in-zone is low risk (wOBA %s vs similar)."),
                      round(mean_chase), early$ab, fmt3(early$woba)))
        } else {
          add(sprintf("Patient hitter (%d%% avg chase) \u2014 attack the zone early for free strikes.",
                      round(mean_chase)))
        }
      }
    }

    wf_ok <- Filter(function(x) v_ok(x$whiff), info)
    if (length(wf_ok) > 0) {
      wfb <- wf_ok[[which.max(vapply(wf_ok, function(x) x$whiff, numeric(1)))]]
      if (wfb$whiff >= 26)
        add(sprintf("%s misses bats (%d%% whiff) \u2014 the putaway pitch at two strikes.",
                    wfb$ab, round(wfb$whiff)))
    }
    ev_ok <- Filter(function(x) v_ok(x$ev), info)
    if (length(ev_ok) > 1) {
      evb <- ev_ok[[which.min(vapply(ev_ok, function(x) x$ev, numeric(1)))]]
      add(sprintf("To suppress contact quality, go %s (%.0f avg EV when he connects).",
                  evb$ab, evb$ev))
    }
    comp <- rv_to_matchup_score(res$composite_rv100)
    if (!is.na(comp)) {
      if (comp >= 70) add("Strong pitcher matchup overall \u2014 attack.")
      else if (comp <= 30) add("Tough matchup \u2014 pitch around damage zones or consider the pen.")
    }
    if (isTRUE(res$low_confidence))
      add("Low similar-pitch sample \u2014 lean on the staff's eyes over these numbers.")
    lines
  }

  output$mm_detail_ui <- renderUI({
    pair <- mm_detail_pair()
    if (is.null(pair)) return(NULL)
    res <- mm_detail_res()
    if (is.null(res)) {
      return(div(class = "adv-placeholder",
                 p("Couldn't compute this matchup — not enough data for the pair.")))
    }
    pk <- split_pitcher_key(pair$p); bk <- split_batter_key(pair$b)
    pd <- drs_info(pk$name, .mm_team_display(pk$team %||% ""))
    bd <- drs_info(bk$name, .mm_team_display(bk$team %||% ""))
    comp_sc <- rv_to_matchup_score(res$composite_rv100)
    if (identical(input$mm_view %||% "pitcher", "hitter") && !is.na(comp_sc))
      comp_sc <- 100 - comp_sc

    fmt_name <- function(nm, d, hand) {
      paste0(if (nzchar(d$jersey)) paste0("#", d$jersey, " ") else "", nm,
             if (!is.null(hand) && nzchar(hand) && !is.na(hand))
               paste0(" (", substr(hand, 1, 1), ")") else "")
    }

    # order pitch types by usage
    pps <- res$per_pitch
    if (length(pps) > 1) {
      us <- vapply(pps, function(x) as.numeric(x$usage_vs_hand %||% 0), numeric(1))
      pps <- pps[order(-us)]
    }

    pill <- function(bg, label, extra_class = NULL, data_pt = NULL) {
      fg <- pill_text_color(bg)
      tags$td(div(class = paste(c("mm-stat-pill", extra_class), collapse = " "),
                  style = paste0("background:", bg, "; color:", fg, ";"),
                  `data-pt` = data_pt, label))
    }

    stat_rows <- lapply(names(pps), function(pt) {
      pp <- pps[[pt]]
      st <- similar_pool_stats(pp$similar)
      rv_sc <- rv_to_matchup_score(pp$rv100)
      rv_lab <- if (is.na(pp$rv100 %||% NA_real_)) "\u2014" else sprintf("%+.1f", pp$rv100)
      # Batter-side Decision / Damage split. Dec + Dmg equals the batter
      # estimate feeding RV/100; RV/100 itself also carries the pitcher
      # prior, so it can sit off the sum by the prior's pull.
      dec <- pp$dec100 %||% NA_real_
      dmg <- pp$dmg100 %||% NA_real_
      dec_lab <- if (is.na(dec)) "\u2014" else sprintf("%+.1f", dec)
      dmg_lab <- if (is.na(dmg)) "\u2014" else sprintf("%+.1f", dmg)
      dec_sc <- rv_to_matchup_score(dec)
      dmg_sc <- rv_to_matchup_score(dmg)
      sel <- identical(mm_detail_pt(), pt)
      cells <- c(
        list(tags$td(class = "mm-namecell",
               tags$span(style = paste0("display:inline-block; width:10px; height:10px;",
                                        "border-radius:50%; margin-right:7px;",
                                        "background:", pitch_colors[[pt]] %||% "#888", ";")),
               tags$span(class = "mm-name", pt)),
             tags$td(div(class = "mm-stat-plain",
                         paste0(round(100 * (pp$usage_vs_hand %||% 0)), "%"))),
             tags$td(div(class = "mm-stat-plain", pp$n_similar %||% 0)),
             pill(if (is.na(dec_sc)) "#EEF1F2" else matchup_score_color(dec_sc), dec_lab),
             pill(if (is.na(dmg_sc)) "#EEF1F2" else matchup_score_color(dmg_sc), dmg_lab),
             pill(matchup_score_color(rv_sc), rv_lab,
                  extra_class = paste("mm-rv-cell", if (sel) "mm-rv-sel"),
                  data_pt = pt)),
        lapply(.MM_DETAIL_STATS, function(s) {
          v <- st[[s]]
          lab <- if (is.null(v) || is.na(v)) "\u2014" else
            if (s %in% c("BA", "SLG", "wOBA")) sub("^0", "", sprintf("%.3f", v)) else
              as.character(v)
          pill(mm_stat_color(s, v), lab)
        }))
      tags$tr(cells)
    })

    header_cells <- c(list(tags$th(class = "mm-bh", "Pitch"),
                           tags$th("Usage"), tags$th("Sim N"),
                           tags$th(title = paste0("Decision: this batter's actual DRE run ",
                                                  "value on balls, called strikes, whiffs ",
                                                  "and fouls vs similar pitches ",
                                                  "(+ favors the bat)"),
                                   "Dec"),
                           tags$th(title = paste0("Damage: this batter's actual DRE run ",
                                                  "value on balls he put in play vs ",
                                                  "similar pitches"),
                                   "Dmg"),
                           tags$th("RV/100")),
                      lapply(.MM_DETAIL_STATS, tags$th))

    plan <- tryCatch(mm_gameplan(res), error = function(e) character(0))
    plan_ui <- if (length(plan) > 0) {
      div(class = "mm-gameplan",
          div(class = "mm-gameplan-title", "Game Plan"),
          tags$ul(lapply(plan, tags$li)))
    } else NULL

    charts <- if (!is.null(mm_detail_pt())) {
      div(style = "padding: 4px 22px 18px;",
          h5(style = "font-weight:700; color:#0B3D3E; margin: 6px 0 2px;",
             paste0(mm_detail_pt(), " — similar pitches ", bk$name, " has seen")),
          fluidRow(
            column(6, plotly::plotlyOutput("mm_detail_movement", height = "420px")),
            column(6, plotly::plotlyOutput("mm_detail_location", height = "420px"))))
    } else {
      div(style = "padding: 0 22px 14px; color:#8A9296; font-size:12px;",
          "Click a RV/100 pill to see the movement and location of those similar pitches.")
    }

    div(class = "mm-card",
        div(class = "mm-card-header",
            span(class = "mm-title", paste0(fmt_name(pk$name, pd, res$pitcher_throws),
                                            "  vs  ",
                                            fmt_name(bk$name, bd, res$batter_side))),
            span(class = "mm-sub",
                 sprintf("stats vs similar pitches \u2022 %d similar total%s",
                         res$total_similar %||% 0L,
                         if (isTRUE(res$low_confidence)) " (low confidence)" else "")),
            span(class = "mm-cell mm-comp-pill",
                 style = paste0("background:", matchup_score_color(comp_sc), "; color:",
                                if (is.na(comp_sc)) "#9AA1A6"
                                else pill_text_color(matchup_score_color(comp_sc)), ";"),
                 if (is.na(comp_sc)) "\u2014" else round(comp_sc))),
        tags$table(class = "mm-table",
                   tags$thead(tags$tr(header_cells)),
                   tags$tbody(stat_rows)),
        plan_ui,
        charts)
  })

  output$mm_detail_movement <- plotly::renderPlotly({
    pp <- mm_detail_pp()
    req(!is.null(pp))
    .detail_movement_plotly(pp$similar, pp$target, pp$pitch_type)
  })

  output$mm_detail_location <- plotly::renderPlotly({
    pp <- mm_detail_pp()
    req(!is.null(pp))
    .detail_location_plotly(pp$similar, pp$pitch_type)
  })
