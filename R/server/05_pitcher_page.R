  # ============================================================
  # ===== RESTRUCTURED PITCHER PLAYER PAGE =====================
  # Bio card + percentile chart (row 1), grade bars + grade stats
  # (row 2), then Metrics / Results / Zone / Angles / Modeling
  # subtabs. Everything below is scoped to input$global_pitcher and
  # the global sidebar filters, exactly like the pieces it replaces.
  # ============================================================

  # ---- shared look-and-feel helpers for the new tables ----
  # ONE conditional-formatting ramp for the whole player page:
  # red = poor, white = average, green = strong (0 -> 1).
  .PP_GRADE_PAL <- scales::gradient_n_pal(
    c("#C0392B", "#E1685E", "#F6D5D2", "#FFFFFF", "#CFE6D2", "#5FAE6B", "#00840D"),
    values = c(0, 0.16, 0.36, 0.5, 0.64, 0.84, 1))
  pp_grade_col <- function(pct) {
    if (length(pct) != 1 || !is.finite(pct)) return("#F2F4F6")
    .PP_GRADE_PAL(pmin(pmax(pct, 0), 1))
  }

  pp_pill <- function(label, pct) {
    txt <- htmltools::htmlEscape(ifelse(is.na(label), "-", as.character(label)))
    if (length(pct) != 1 || !is.finite(pct))
      return(gt::html(sprintf("<span style='color:#3B4754;'>%s</span>", txt)))
    col <- pp_grade_col(pct)
    fg  <- if (pct > 0.86 || pct < 0.14) "#FFFFFF" else "#243746"
    gt::html(sprintf(
      "<span title='D1 percentile: %d' style='background:%s;color:%s;padding:3px 10px;border-radius:999px;font-weight:600;font-size:12px;display:inline-block;min-width:48px;cursor:help;'>%s</span>",
      round(pct * 100), col, fg, txt))
  }

  # Grouped savant-style percentile chart. `rows` needs: section, label,
  # disp, pct (0-1) and optionally grade (bold row). Sections render as a
  # small header with a rule; each bar gets a bubble and a dotted leader
  # out to the raw value.
  pp_percentile_ui <- function(rows) {
    if (is.null(rows) || !nrow(rows)) return(div(style = "color:#8A93A0;", "No data."))
    if (!"section"   %in% names(rows)) rows$section   <- ""
    if (!"grade"     %in% names(rows)) rows$grade     <- FALSE
    if (!"sep_after" %in% names(rows)) rows$sep_after <- FALSE
    secs <- unique(rows$section)
    gl <- lapply(c(20, 40, 60, 80), function(g)
      div(class = "pp-pct-gl", style = paste0("left:", g, "%;")))

    tagList(lapply(secs, function(sc) {
      sub <- rows[rows$section == sc, , drop = FALSE]
      tagList(
        if (nzchar(sc)) div(class = "pp-pct-sec", tags$span(sc)) else NULL,
        lapply(seq_len(nrow(sub)), function(i) {
          r   <- sub[i, ]
          p   <- suppressWarnings(as.numeric(r$pct))
          has <- is.finite(p)
          col <- if (has) pp_grade_col(p) else "#DDE3E8"
          wpc <- if (has) max(2.5, min(100, p * 100)) else 0
          fg  <- if (has && (p > 0.86 || p < 0.14)) "#FFFFFF" else "#243746"
          tagList(div(class = "pp-pct-row",
            div(class = if (isTRUE(r$grade)) "pp-pct-lab is-grade" else "pp-pct-lab",
                r$label),
            div(class = "pp-pct-track",
              gl,
              div(class = "pp-pct-fill",
                  style = paste0("width:", wpc, "%;background:", col, ";")),
              if (has) div(class = "pp-pct-lead",
                           style = paste0("left:", wpc, "%;")) else NULL,
              div(class = "pp-pct-bub",
                  style = paste0("left:", wpc, "%;background:", col,
                                 ";color:", fg, ";"),
                  if (has) sprintf("%d", round(p * 100)) else "\u2014")
            ),
            div(class = "pp-pct-val", r$disp)
          ),
          if (isTRUE(r$sep_after)) div(class = "pp-pct-sep") else NULL)
        })
      )
    }))
  }

  pp_pitch_chip <- function(x) {
    nm <- as.character(x)
    if (length(nm) == 0 || is.na(nm)) nm <- "-"
    hit <- unname(pitch_colors[nm])
    col <- if (length(hit) == 1 && !is.na(hit)) hit else "#77828F"
    gt::html(sprintf("<span style='color:%s;font-weight:700;'>%s</span>",
                     col, htmltools::htmlEscape(nm)))
  }

  # Clean, borderless white table styling used across the new subtabs.
  pp_gt_modern <- function(tb) {
    tb %>%
      gt::tab_options(
        table.width                   = gt::pct(100),
        table.background.color        = "#FFFFFF",
        table.font.size               = gt::px(13),
        table.border.top.style        = "none",
        table.border.bottom.style     = "none",
        column_labels.background.color = "#FFFFFF",
        column_labels.font.weight     = "bold",
        column_labels.font.size       = gt::px(11),
        column_labels.border.top.style = "none",
        column_labels.border.bottom.color = "#ECEFF2",
        table_body.hlines.color       = "#F4F6F8",
        table_body.border.top.color   = "#ECEFF2",
        table_body.border.bottom.color = "#ECEFF2",
        data_row.padding              = gt::px(9)
      ) %>%
      gt::opt_table_font(font = c("Inter", "Helvetica Neue", "Arial", "sans-serif")) %>%
      gt::cols_align(align = "center", columns = gt::everything()) %>%
      gt::tab_style(
        style = gt::cell_text(color = "#8A93A0", transform = "uppercase"),
        locations = gt::cells_column_labels()) %>%
      gt::tab_style(
        style = gt::cell_text(align = "left"),
        locations = gt::cells_body(columns = 1))
  }

  # ---- pitch family map (matches .RC_FAMILY_MAP) ----
  .pp_fam <- function(x) {
    m <- c(Fastball = "FB", `Four-Seam` = "FB", `4-Seam` = "FB", Sinker = "FB",
           Cutter = "BB", Slider = "BB", Sweeper = "BB", Curveball = "BB",
           Slurve = "BB", `Knuckle Curve` = "BB",
           ChangeUp = "OS", Changeup = "OS", Splitter = "OS")
    unname(m[as.character(x)])
  }

  # ================= BIO CARD (vertically condensed) =================
  output$pp_bio_card <- renderUI({
    req(input$global_pitcher)
    nm <- input$global_pitcher

    vv <- function(x, d = "\u2014") {
      if (is.null(x) || length(x) == 0) return(d)
      x <- x[1]
      if (is.na(x) || !nzchar(trimws(as.character(x)))) d else as.character(x)
    }
    cell <- function(lab, val) div(class = "pp-bio-cell",
                                   div(class = "pp-bio-lab", lab),
                                   div(class = "pp-bio-val", val))

    throws_txt <- tryCatch({
      d <- filtered_data()
      tc <- intersect(c("PitcherThrows", "Throws"), names(d))
      if (length(tc)) {
        t <- stats::na.omit(unique(as.character(d[[tc[1]]])))
        if (length(t)) toupper(substr(t[1], 1, 1)) else NA_character_
      } else NA_character_
    }, error = function(e) NA_character_)

    n_pitch <- tryCatch(nrow(filtered_data()), error = function(e) 0)

    # 2027 roster is the primary bio source for Coastal players; the legacy
    # TruMedia-era bio and the NCAA DRS lookup are fallbacks.
    r27row <- NULL
    if (!is.null(roster27) && "Fall26" %in% (input$season_type %||% character(0))) {
      m <- roster27[bp_norm_name(roster27$Name) == bp_norm_name(nm), , drop = FALSE]
      if (nrow(m) > 0) r27row <- m[1, , drop = FALSE]
    }

    if (!is.null(r27row)) {
      disp   <- vv(r27row$Name, nm)
      num    <- vv(r27row$Number, "")
      pos    <- vv(r27row$Pos, "P")
      cls    <- vv(r27row$Class)
      home   <- vv(r27row$Hometown)
      ht     <- vv(r27row$Height)
      wt     <- if (!is.na(r27row$Weight)) paste0(r27row$Weight, " lbs") else "\u2014"
      bats   <- vv(r27row$Bats)
      thr    <- vv(r27row$Throws, if (!is.na(throws_txt)) throws_txt else "\u2014")
      bt     <- paste0(substr(bats, 1, 1), "-", substr(thr, 1, 1))
      team   <- "Coastal Carolina"
      logo   <- tryCatch(ccu_logo_url, error = function(e) NULL)
    } else if (isTRUE(is_coastal_pitcher(nm)) && exists("bio") && is.data.frame(bio)) {
      row <- bio[which(bio$Pitcher_FL == nm | bio$Pitcher == nm)[1], , drop = FALSE]
      hc <- function(k) k %in% names(row) && nrow(row) > 0
      to_fl <- function(x) {
        if (is.na(x)) return(nm)
        out <- sub("^\\s*([^,]+),\\s*(.+)\\s*$", "\\2 \\1", x)
        if (identical(out, x)) x else out
      }
      disp   <- if (hc("Pitcher")) to_fl(row$Pitcher) else nm
      num    <- if (hc("Number")) vv(row$Number, "") else ""
      pos    <- if (hc("Position")) vv(row$Position) else "P"
      cls    <- if (hc("Class")) vv(row$Class) else "\u2014"
      home   <- if (hc("Hometown")) vv(row$Hometown) else "\u2014"
      ht     <- if (hc("Height")) vv(row$Height) else "\u2014"
      wt     <- if (hc("Weight")) paste0(vv(row$Weight), " lbs") else "\u2014"
      bats   <- if (hc("Bats")) vv(row$Bats) else "\u2014"
      thr    <- if (!is.na(throws_txt)) throws_txt else if (hc("Throws")) vv(row$Throws) else "\u2014"
      bt     <- paste0(substr(bats, 1, 1), "-", substr(thr, 1, 1))
      team   <- "Coastal Carolina"
      logo   <- tryCatch(ccu_logo_url, error = function(e) NULL)
    } else {
      # Identity by TrackMan id (directory, else the loaded pitches), so the
      # bio, crest and conference are this arm's and not a namesake's.
      pid <- tryCatch(ps_pitcher_id(nm), error = function(e) NA_character_)
      if (is.na(pid)) pid <- tryCatch({
        d <- filtered_data(); v <- unique(stats::na.omit(as.character(d$PitcherId)))
        v <- v[nzchar(v) & !v %in% c("NA", "0")]; if (length(v)) v[1] else NA_character_
      }, error = function(e) NA_character_)
      tk_team <- tryCatch(tm_current_team(nm), error = function(e) NULL)
      pb <- tryCatch(player_bio(nm, team = tk_team, tm_id = pid, kind = "pit"), error = function(e) NULL)
      disp <- nm
      num  <- if (!is.null(pb)) vv(pb$jersey, "") else ""
      pos  <- if (!is.null(pb)) vv(pb$pos, "P") else "P"
      cls  <- if (!is.null(pb)) vv(pb$class) else "\u2014"
      home <- if (!is.null(pb)) vv(pb$hometown) else "\u2014"
      ht   <- if (!is.null(pb)) vv(pb$height) else "\u2014"
      wt   <- if (!is.null(pb) && is.finite(pb$weight)) paste0(pb$weight, " lbs") else "\u2014"
      thr  <- if (!is.na(throws_txt)) throws_txt else if (!is.null(pb)) vv(pb$throws) else "\u2014"
      bt   <- if (!is.null(pb) && !is.na(pb$bats)) paste0(substr(pb$bats, 1, 1), "-", substr(thr, 1, 1))
              else if (!is.na(throws_txt)) paste0("T: ", throws_txt) else "\u2014"
      team <- if (!is.null(pb) && !is.na(pb$team)) pb$team else tryCatch(prettify_team(tk_team), error = function(e) "\u2014")
      if (is.null(team) || is.na(team) || !nzchar(team)) team <- "\u2014"
      logo <- if (!is.null(pb)) pb$logo else NULL
      conf <- if (!is.null(pb)) pb$conf else NA_character_
      pb_head <- if (!is.null(pb)) pb$headshot else NULL
    }
    if (!exists("conf", inherits = FALSE)) conf <- "SBELT"
    if (!exists("pb_head", inherits = FALSE)) pb_head <- NULL

    sub <- paste0(team, if (nzchar(num)) paste0(" \u2022 #", num) else "",
                  if (!is.na(conf) && nzchar(conf)) paste0(" \u2022 ", conf) else "")

    logo_bad <- is.null(logo) || length(logo) == 0 ||
                isTRUE(is.na(logo[1])) || !nzchar(as.character(logo[1]))
    hs <- if (logo_bad || is.null(pb_head) || is.na(pb_head)) tryCatch(headshot_lookup(nm, team), error = function(e) NULL) else NULL
    if (logo_bad && !is.null(hs)) logo <- hs$logo
    # Headshot: the 2027 roster CSV wins for roster players, then the
    # id-resolved reference headshot, then the name-matched table.
    head_src <- if (!is.null(r27row) && !is.na(r27row$Headshot) &&
                    nzchar(r27row$Headshot)) r27row$Headshot
                else if (!is.null(pb_head) && !is.na(pb_head)) pb_head
                else if (!is.null(hs)) hs$headshot else NULL

    div(class = "pp-card",
      div(class = "pp-bio-top",
        if (!is.null(head_src) && !is.na(head_src[1]) && nzchar(head_src[1]))
          tags$img(class = "pp-bio-head", src = head_src[1],
                   onerror = "this.style.display='none';") else NULL,
        div(
          div(class = "pp-bio-name", disp),
          div(class = "pp-bio-sub", sub)
        ),
        if (!is.null(logo) && !is.na(logo) && nzchar(as.character(logo)))
          tags$img(class = "pp-bio-logo", src = as.character(logo)) else NULL
      ),
      div(class = "pp-bio-grid",
        cell("Pos", pos), cell("B-T", bt), cell("Year", cls),
        cell("Height", ht), cell("Weight", wt), cell("Pitches", format(n_pitch, big.mark = ",")),
        div(class = "pp-bio-cell", style = "grid-column:1 / -1;",
            div(class = "pp-bio-lab", "Hometown"),
            div(class = "pp-bio-val", home))
      )
    )
  })

  # ============ PERCENTILE CHART (savant-style rows) ============
  output$pp_pct_title <- renderText({
    yr <- tryCatch({
      d <- filtered_data()
      y <- suppressWarnings(as.integer(format(safe_as_date(d$Date), "%Y")))
      y <- y[is.finite(y)]
      if (length(y)) max(y, na.rm = TRUE) else NA_integer_
    }, error = function(e) NA_integer_)
    if (is.na(yr)) "Percentile Rankings" else paste(yr, "Percentile Rankings")
  })

  # Per-arm (or single-arm) summary of every stat the chart needs.
  pp_pct_per_arm <- function(df, min_n = 100) {
    if (is.null(df) || !nrow(df)) return(NULL)
    d <- df
    d$.fam    <- .pp_fam(d$TaggedPitchType)
    d$.is_fb  <- !is.na(d$.fam) & d$.fam == "FB"
    d$.is_bb  <- !is.na(d$.fam) & d$.fam == "BB"
    d$.is_os  <- !is.na(d$.fam) & d$.fam == "OS"
    d$.bip    <- as.character(d$PitchCall) == "InPlay"
    d$.ev     <- suppressWarnings(as.numeric(d$ExitSpeed))
    d$.k      <- if ("KorBB" %in% names(d))
                   as.character(d$KorBB) == "Strikeout" else NA
    for (cc in c("stuff_plus", "location_plus", "pitching_plus", "RelSpeed",
                 "Extension", "StrikeIndicator", "SwingIndicator",
                 "WhiffIndicator", "Chaseindicator", "OutofZone",
                 "HHindicator", "biphh", "WalkIndicator", "PAindicator"))
      if (!cc %in% names(d)) d[[cc]] <- NA_real_

    mv <- function(x) { x <- suppressWarnings(as.numeric(x)); x <- x[is.finite(x)]
                        if (length(x)) mean(x) else NA_real_ }
    pdv <- function(a, b) if (isTRUE(is.finite(b) && b > 0)) 100 * a / b else NA_real_
    sm  <- function(x) sum(x, na.rm = TRUE)

    one <- function(dd) {
      tibble::tibble(
        n            = nrow(dd),
        `Pitching+`  = mv(dd$pitching_plus),
        `FB Value`   = mv(dd$stuff_plus[dd$.is_fb]),
        `BB Value`   = mv(dd$stuff_plus[dd$.is_bb]),
        `OS Value`   = mv(dd$stuff_plus[dd$.is_os]),
        `Location+`  = mv(dd$location_plus),
        `FBVelo`     = mv(dd$RelSpeed[dd$.is_fb]),
        `FBStrike%`  = pdv(sm(dd$StrikeIndicator[dd$.is_fb]), sm(dd$.is_fb)),
        `AvgEV`      = mv(dd$.ev[dd$.bip]),
        `Chase%`     = pdv(sm(dd$Chaseindicator), sm(dd$OutofZone)),
        `Whiff%`     = pdv(sm(dd$WhiffIndicator), sm(dd$SwingIndicator)),
        `K%`         = pdv(sm(dd$.k), sm(dd$PAindicator)),
        `BB%`        = pdv(sm(dd$WalkIndicator), sm(dd$PAindicator)),
        `HardHit%`   = pdv(sm(dd$HHindicator), sm(dd$biphh)),
        `Extension`  = mv(dd$Extension))
    }

    if (!"Pitcher" %in% names(d) || length(unique(d$Pitcher)) == 1L) {
      out <- one(d); out$Pitcher <- as.character(d$Pitcher[1])
      return(out[, c("Pitcher", setdiff(names(out), "Pitcher")), drop = FALSE])
    }
    d %>%
      dplyr::group_by(Pitcher) %>%
      dplyr::group_modify(~ one(.x)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(n >= min_n)
  }

  # ---- Pool distribution for every row on the pitcher percentile chart ----
  # One per-arm frame off the selected pool, cached by pool name. Each stat
  # becomes an ecdf; stats where LOW is good are stored negated so a higher
  # displayed percentile always means better.
  .pp_pct_pool_cache <- new.env(parent = emptyenv())

  # stats whose pool ecdf is built on the negated value (low = good)
  .PP_PCT_FLIP <- c("xBA", "xSLG", "xwOBAcon", "BB%", "HardHit%", "AvgEV")

  pp_pct_pool <- reactive({
    pool_name <- input$global_pool %||% "D1"
    hit <- .pp_pct_pool_cache[[pool_name]]
    if (!is.null(hit)) return(hit)

    pool <- tryCatch(grade_pool_df(), error = function(e) NULL)
    if (is.null(pool) || !nrow(pool) || !"Pitcher" %in% names(pool)) return(list())

    per <- tryCatch(pp_pct_per_arm(pool), error = function(e) NULL)
    if (is.null(per) || !nrow(per)) return(list())

    # xBA / xSLG come free off the cached pool call the grades already make
    xs <- tryCatch(xs_pool_arm(), error = function(e) NULL)
    if (!is.null(xs) && "Pitcher" %in% names(xs)) {
      keep <- intersect(c("Pitcher", "xBA", "xSLG"), names(xs))
      per <- dplyr::left_join(per, xs[, keep, drop = FALSE], by = "Pitcher")
    }
    if (!"xBA"  %in% names(per)) per$xBA  <- NA_real_
    if (!"xSLG" %in% names(per)) per$xSLG <- NA_real_

    # xwOBAcon = xwOBA over balls in play only
    # xwOBAcon straight from the per-pitch expected terms the pipeline stored
    # (xwobacon_bbe: model value on tracked contact, actual outcome otherwise)
    con <- tryCatch({
      if ("xwobacon_bbe" %in% names(pool)) {
        bip <- pool[!is.na(pool$xwobacon_bbe), , drop = FALSE]
        if (nrow(bip) > 0)
          bip %>% dplyr::group_by(Pitcher) %>%
            dplyr::summarise(xwOBA = mean(xwobacon_bbe, na.rm = TRUE), .groups = "drop") %>%
            as.data.frame() else NULL
      } else NULL
    }, error = function(e) NULL)
    if (!is.null(con) && all(c("Pitcher", "xwOBA") %in% names(con))) {
      con <- con[, c("Pitcher", "xwOBA"), drop = FALSE]
      names(con) <- c("Pitcher", "xwOBAcon")
      per <- dplyr::left_join(per, con, by = "Pitcher")
    }
    if (!"xwOBAcon" %in% names(per)) per$xwOBAcon <- NA_real_

    keys <- setdiff(names(per), c("Pitcher", "n"))
    out <- lapply(keys, function(k) {
      v <- suppressWarnings(as.numeric(per[[k]]))
      v <- v[is.finite(v)]
      if (length(v) < 15) return(NULL)
      if (k %in% .PP_PCT_FLIP) stats::ecdf(-v) else stats::ecdf(v)
    })
    names(out) <- keys
    .pp_pct_pool_cache[[pool_name]] <- out
    out
  })

  pp_pct_rows <- reactive({
    req(input$global_pitcher)
    d <- filtered_data() %>% dplyr::filter(Pitcher == input$global_pitcher)
    validate(need(nrow(d) > 0, "No data for this pitcher in the current filters."))

    me <- pp_pct_per_arm(d, min_n = 0)
    validate(need(!is.null(me) && nrow(me) > 0, "Not enough pitches to grade."))

    # this arm's x-stats, same definitions as the pool
    xs <- tryCatch(pp_expected_stats_cached(d, group_cols = c("Pitcher")),
                   error = function(e) NULL)
    me$xBA  <- if (!is.null(xs) && "xBA"  %in% names(xs)) xs$xBA[1]  else NA_real_
    me$xSLG <- if (!is.null(xs) && "xSLG" %in% names(xs)) xs$xSLG[1] else NA_real_
    con <- tryCatch({
      bip <- d[as.character(d$PitchCall) == "InPlay", , drop = FALSE]
      if (nrow(bip) > 0)
        pp_expected_stats_cached(bip, group_cols = c("Pitcher")) else NULL
    }, error = function(e) NULL)
    me$xwOBAcon <- if (!is.null(con) && "xwOBA" %in% names(con))
      con$xwOBA[1] else NA_real_

    ec <- pp_pct_pool()

    # label | key | decimals | bold | rule-after
    spec <- list(
      list("Pitching Value",  "Pitching+", 0, TRUE,  TRUE),
      list("FB Value",        "FB Value",  0, TRUE,  FALSE),
      list("BB Value",        "BB Value",  0, TRUE,  FALSE),
      list("OS Value",        "OS Value",  0, TRUE,  TRUE),
      list("Location Value",  "Location+", 0, TRUE,  TRUE),
      list("xBA",             "xBA",       3, FALSE, FALSE),
      list("xSLG",            "xSLG",      3, FALSE, FALSE),
      list("xwOBAcon",        "xwOBAcon",  3, FALSE, FALSE),
      list("FB Velo",         "FBVelo",    1, FALSE, FALSE),
      list("FB Strike%",      "FBStrike%", 1, FALSE, FALSE),
      list("Avg Exit Velo",   "AvgEV",     1, FALSE, FALSE),
      list("Chase%",          "Chase%",    1, FALSE, FALSE),
      list("Whiff%",          "Whiff%",    1, FALSE, FALSE),
      list("K%",              "K%",        1, FALSE, FALSE),
      list("BB%",             "BB%",       1, FALSE, FALSE),
      list("Hard Hit%",       "HardHit%",  1, FALSE, FALSE),
      list("Extension",       "Extension", 1, FALSE, FALSE)
    )

    do.call(rbind, lapply(spec, function(sp) {
      lab <- sp[[1]]; key <- sp[[2]]; dec <- sp[[3]]
      v <- suppressWarnings(as.numeric(me[[key]] %||% NA_real_))
      if (length(v) != 1) v <- NA_real_
      f <- ec[[key]]
      p <- if (is.null(f) || !is.finite(v)) NA_real_
           else as.numeric(f(if (key %in% .PP_PCT_FLIP) -v else v))
      disp <- if (!is.finite(v)) "\u2014"
              else if (dec == 3) sub("^0", "", formatC(v, format = "f", digits = 3))
              else formatC(v, format = "f", digits = dec)
      data.frame(section = "", label = lab, disp = disp, pct = p,
                 grade = sp[[4]], sep_after = sp[[5]],
                 stringsAsFactors = FALSE)
    }))
  })

  output$pp_percentile_chart <- renderUI({
    pp_percentile_ui(pp_pct_rows())
  })

  # ==================== METRICS SUBTAB ====================

  output$pp_rel_title <- renderText({
    switch(input$pp_rel_chart %||% "points",
           "consistency" = "Release Consistency",
           "extension"   = "Release Height vs Extension",
           "Release Points")
  })

  # ---- Release points (RelSide x RelHeight, mound behind) ----
  output$pp_release_points <- renderPlot({
    df <- viz_data() %>%
      dplyr::filter(!is.na(RelSide), !is.na(RelHeight),
                    !is.na(TaggedPitchType), TaggedPitchType != "Other")
    validate(need(nrow(df) > 0, "No release point data."))

    show_ind <- input$pp_rel_individual %||% TRUE
    show_avg <- input$pp_rel_average %||% TRUE
    if (!show_ind && !show_avg) show_ind <- TRUE

    avg <- df %>%
      dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(RelSide = mean(RelSide, na.rm = TRUE),
                       RelHeight = mean(RelHeight, na.rm = TRUE),
                       .groups = "drop")

    p <- ggplot(df, aes(RelSide, RelHeight)) +
      annotate("rect", xmin = -5, xmax = 5, ymin = 0, ymax = .83,
               fill = "#632b11") +
      annotate("rect", xmin = -0.5, xmax = 0.5, ymin = 0.8, ymax = .95,
               fill = "white", color = "black", linewidth = .5)
    if (isTRUE(show_ind))
      p <- p + geom_point(aes(fill = TaggedPitchType), size = 4.2, shape = 21,
                          color = "black", stroke = .2,
                          alpha = if (isTRUE(show_avg)) .35 else .7)
    if (isTRUE(show_avg))
      p <- p + geom_point(data = avg, aes(RelSide, RelHeight, fill = TaggedPitchType),
                          size = 7, shape = 21, color = "black", stroke = .7)
    p +
      annotate("text", x = -5, y = 8, label = "\u2190 1B",
               color = "#6B7280", size = 3.4, hjust = 0) +
      annotate("text", x = 5, y = 8, label = "3B \u2192",
               color = "#6B7280", size = 3.4, hjust = 1) +
      scale_fill_manual(values = pitch_colors) +
      xlim(-5, 5) + ylim(0, 8) +
      labs(x = "Horizontal Release (ft)", y = "Vertical Release (ft)", fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#F1F4F6"),
        panel.background = element_rect(fill = "#FFFFFF", color = NA),
        plot.background  = element_rect(fill = "#FFFFFF", color = NA),
        axis.text  = element_text(size = 11, color = "#5B6875"),
        axis.title = element_text(size = 11, color = "#5B6875")
      )
  })

  # ---- Pitch metrics table (no plus columns; clean white pills) ----
  output$pp_metrics_table <- gt::render_gt({
    df <- filtered_data()
    req(nrow(df) > 0)
    total <- nrow(df)

    mov <- df %>%
      dplyr::group_by(`Pitch` = TaggedPitchType) %>%
      dplyr::summarise(
        `#`                 = dplyr::n(),
        Usage               = 100 * dplyr::n() / total,
        `Avg Velo`          = mean(RelSpeed, na.rm = TRUE),
        `Max Velo`          = max(RelSpeed, na.rm = TRUE),
        `Avg Spin`          = mean(SpinRate, na.rm = TRUE),
        `Avg IVB`           = mean(InducedVertBreak, na.rm = TRUE),
        `Avg HB`            = mean(HorzBreak, na.rm = TRUE),
        `Avg Rel Ht (ft)`   = mean(RelHeight, na.rm = TRUE),
        `Avg Rel Side (ft)` = mean(RelSide, na.rm = TRUE),
        `Avg Ext (ft)`      = mean(Extension, na.rm = TRUE),
        VAA                 = mean(VertApprAngle, na.rm = TRUE),
        HAA                 = mean(HorzApprAngle, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(Usage))

    mov <- mov %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        .p_speed_avg = get_mov_ecdf(p5_maps, `Pitch`, "speed")(`Avg Velo`),
        .p_speed_max = get_mov_ecdf(p5_maps, `Pitch`, "speed")(`Max Velo`),
        .p_spin      = get_mov_ecdf(p5_maps, `Pitch`, "spin") (`Avg Spin`),
        .p_ext       = get_mov_ecdf(p5_maps, `Pitch`, "ext")  (`Avg Ext (ft)`)
      ) %>%
      dplyr::ungroup()

    mov %>%
      gt::gt() %>%
      pp_gt_modern() %>%
      gt::fmt_number(columns = c(Usage), decimals = 1) %>%
      gt::fmt_number(
        columns = c(`Avg Velo`, `Max Velo`, `Avg IVB`, `Avg HB`,
                    `Avg Rel Ht (ft)`, `Avg Rel Side (ft)`, `Avg Ext (ft)`, VAA, HAA),
        decimals = 1) %>%
      gt::fmt_number(columns = c(`Avg Spin`), decimals = 0) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = `Pitch`),
        fn = function(v) lapply(v, pp_pitch_chip)) %>%
      gt::cols_hide(columns = c(.p_speed_avg, .p_speed_max, .p_spin, .p_ext)) %>%
      gt::text_transform(gt::cells_body(columns = `Avg Velo`),
        fn = function(v) mapply(pp_pill, v, mov$.p_speed_avg, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Max Velo`),
        fn = function(v) mapply(pp_pill, v, mov$.p_speed_max, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Avg Spin`),
        fn = function(v) mapply(pp_pill, v, mov$.p_spin, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Avg Ext (ft)`),
        fn = function(v) mapply(pp_pill, v, mov$.p_ext, SIMPLIFY = FALSE))
  })

  # ==================== RESULTS SUBTAB ====================

  # Keep the per-tab date pickers in step with the global range.
  observeEvent(input$g_dateRange, {
    updateDateRangeInput(session, "pp_res_dates",
                         start = input$g_dateRange[1], end = input$g_dateRange[2])
    updateDateRangeInput(session, "pp_mod_dates",
                         start = input$g_dateRange[1], end = input$g_dateRange[2])
  }, ignoreNULL = TRUE)

  # One metric off an arbitrary slice of pitches.
  .pp_calc <- function(dd, metric) {
    pdv <- function(a, b) if (isTRUE(is.finite(b) && b > 0)) a / b else NA_real_
    s <- function(x) sum(x, na.rm = TRUE)
    zero_zero <- dd$Balls == 0 & dd$Strikes == 0
    zero_zero[is.na(zero_zero)] <- FALSE
    two_k <- dd$Strikes == 2
    two_k[is.na(two_k)] <- FALSE
    switch(metric,
      "Strike %"   = 100 * pdv(s(dd$StrikeIndicator), nrow(dd)),
      "0-0 Zone %" = 100 * pdv(s(dd$StrikeZoneIndicator[zero_zero]), s(zero_zero)),
      "Swing %"    = 100 * pdv(s(dd$SwingIndicator), nrow(dd)),
      "Whiff %"    = 100 * pdv(s(dd$WhiffIndicator), s(dd$SwingIndicator)),
      "Z-Whiff %"  = 100 * pdv(s(dd$Zwhiffind), s(dd$Zswing)),
      "2K Miss %"  = 100 * pdv(s(dd$WhiffIndicator[two_k]), s(dd$SwingIndicator[two_k])),
      "Chase %"    = 100 * pdv(s(dd$Chaseindicator), s(dd$OutofZone)),
      "GB %"       = 100 * pdv(s(dd$GBindicator), s(dd$BIPind)),
      "HH %"       = 100 * pdv(s(dd$HHindicator), s(dd$biphh)),
      "AVG"        = pdv(s(dd$HitIndicator), s(dd$ABindicator)),
      "SLG"        = pdv(s(dd$totalbases), s(dd$ABindicator)),
      "proStuff+"    = mean(dd$stuff_plus, na.rm = TRUE),
      "proLocation+" = mean(dd$location_plus, na.rm = TRUE),
      "proPitching+" = mean(dd$pitching_plus, na.rm = TRUE),
      NA_real_)
  }

  # Trend frame: one row per (game date, series).
  .pp_trend_frame <- function(d, metric, split, side, min_n = 4) {
    if (is.null(d) || !nrow(d)) return(NULL)
    if (side %in% c("R", "L")) d <- d[norm_side(d$BatterSide) == side, , drop = FALSE]
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
                 val = .pp_calc(dd, metric), stringsAsFactors = FALSE)
    }))
    if (is.null(out)) return(NULL)
    out <- out[is.finite(out$val) & out$n >= min_n, , drop = FALSE]
    if (!nrow(out)) return(NULL)
    out[order(out$date), , drop = FALSE]
  }

  .pp_trend_plotly <- function(fr, metric, is_model, title) {
    if (is.null(fr) || !nrow(fr))
      return(plotly::plotly_empty(type = "scatter", mode = "markers") %>%
               plotly::layout(title = list(text = "No data for this selection",
                                           font = list(size = 14, color = "#8A93A0"))))
    is_pct <- grepl("%", metric, fixed = TRUE)
    grps <- unique(fr$grp)
    ord <- order(vapply(grps, function(g) -sum(fr$n[fr$grp == g]), numeric(1)))
    grps <- grps[ord]

    p <- plotly::plot_ly()
    for (g in grps) {
      sub <- fr[fr$grp == g, , drop = FALSE]
      col <- if (identical(g, "All")) "#152535"
             else if (!is.na(pitch_colors[g])) unname(pitch_colors[g]) else "#77828F"
      p <- p %>% plotly::add_trace(
        x = sub$date, y = sub$val, name = g,
        type = "scatter", mode = "lines+markers",
        line = list(color = col, width = 2.4, shape = "spline", smoothing = 0.6),
        marker = list(color = col, size = 7,
                      line = list(color = "#FFFFFF", width = 1.2)),
        hovertemplate = paste0("<b>", g, "</b><br>%{x|%b %d}<br>", metric,
                               ": %{y:.1f}<br>Pitches: ", sub$n, "<extra></extra>"))
    }

    yv <- fr$val[is.finite(fr$val)]
    pad <- if (length(yv)) max(2, diff(range(yv)) * 0.12) else 5
    yr <- if (length(yv)) c(min(yv) - pad, max(yv) + pad) else c(0, 100)
    if (is_pct) yr <- c(max(0, yr[1]), min(100, yr[2]))

    p <- p %>% plotly::layout(
      autosize = TRUE,
      title = list(text = title, font = list(size = 14, color = "#243746")),
      xaxis = list(title = "", showgrid = FALSE, showline = TRUE,
                   linecolor = "#ECEFF2", tickfont = list(size = 11, color = "#77828F")),
      yaxis = list(title = list(text = metric, font = list(size = 12, color = "#77828F")),
                   range = yr, gridcolor = "#F2F5F7", zeroline = FALSE,
                   tickfont = list(size = 11, color = "#77828F"),
                   ticksuffix = if (is_pct) "%" else ""),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.16,
                    font = list(size = 12)),
      hovermode = "closest",
      plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",
      margin = list(t = 44, b = 62, l = 56, r = 20),
      transition = list(duration = 350, easing = "cubic-in-out"))

    if (isTRUE(is_model)) {
      p <- p %>% plotly::add_segments(
        x = min(fr$date), xend = max(fr$date), y = 100, yend = 100,
        line = list(color = "#243746", width = 1, dash = "solid"),
        showlegend = FALSE, hoverinfo = "skip", inherit = FALSE)
    }
    p %>% plotly::config(displayModeBar = FALSE, responsive = TRUE)
  }

  pp_trend_source <- function(dates) {
    d <- filtered_data()
    req(is.data.frame(d))
    if (!is.null(dates) && length(dates) == 2 && all(!is.na(dates))) {
      dd  <- safe_as_date(d$Date)
      sub <- d[!is.na(dd) & dd >= as.Date(dates[1]) & dd <= as.Date(dates[2]), , drop = FALSE]
      # An un-synced picker (first paint) must not blank the chart.
      if (nrow(sub) > 0) d <- sub
    }
    d
  }

  output$pp_results_trend <- plotly::renderPlotly({
    metric <- input$pp_res_metric %||% "Strike %"
    fr <- .pp_trend_frame(pp_trend_source(input$pp_res_dates), metric,
                          input$pp_res_split %||% "ind",
                          input$pp_res_trend_side %||% "B")
    .pp_trend_plotly(fr, metric, FALSE, paste0(metric, " by outing"))
  })

  # ---- Combined results table with a side toggle ----
  pp_results_gt <- function(side) {
    df <- filtered_data()
    if (side %in% c("R", "L")) df <- df %>% dplyr::filter(norm_side(BatterSide) == side)
    validate(need(nrow(df) > 0, "No pitches for this split."))
    ref_side <- if (identical(side, "L")) "L" else "R"
    total <- nrow(df)

    res <- df %>%
      dplyr::group_by(`Pitch` = TaggedPitchType) %>%
      dplyr::summarise(
        `#`          = dplyr::n(),
        Usage        = 100 * dplyr::n() / total,
        `0-0 Zone %` = pdiv(sum(StrikeZoneIndicator[Balls == 0 & Strikes == 0], na.rm = TRUE),
                            sum(Balls == 0 & Strikes == 0, na.rm = TRUE)),
        `Strike %`   = pdiv(sum(StrikeIndicator, na.rm = TRUE), dplyr::n()),
        `Swing %`    = pdiv(sum(SwingIndicator, na.rm = TRUE), dplyr::n()),
        `AVG`        = pdiv(sum(HitIndicator, na.rm = TRUE), sum(ABindicator, na.rm = TRUE)),
        `SLG`        = pdiv(sum(totalbases, na.rm = TRUE), sum(ABindicator, na.rm = TRUE)),
        `Whiff %`    = pdiv(sum(WhiffIndicator, na.rm = TRUE), sum(SwingIndicator, na.rm = TRUE)),
        `2K Miss %`  = pdiv(sum(WhiffIndicator[Strikes == 2], na.rm = TRUE),
                            sum(SwingIndicator[Strikes == 2], na.rm = TRUE)),
        `Z-Whiff %`  = pdiv(sum(Zwhiffind, na.rm = TRUE), sum(Zswing, na.rm = TRUE)),
        `Chase %`    = pdiv(sum(Chaseindicator, na.rm = TRUE), sum(OutofZone, na.rm = TRUE)),
        `HH %`       = pdiv(sum(HHindicator, na.rm = TRUE), sum(biphh, na.rm = TRUE)),
        `GB %`       = pdiv(sum(GBindicator, na.rm = TRUE), sum(BIPind, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(Usage)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        .p_zone00  = get_res_ecdf(p5_maps, `Pitch`, ref_side, "zone00") (`0-0 Zone %`),
        .p_strike  = get_res_ecdf(p5_maps, `Pitch`, ref_side, "strike") (`Strike %`),
        .p_swing   = get_res_ecdf(p5_maps, `Pitch`, ref_side, "swing")  (`Swing %`),
        .p_whiff   = get_res_ecdf(p5_maps, `Pitch`, ref_side, "whiff")  (`Whiff %`),
        .p_whiff2k = get_res_ecdf(p5_maps, `Pitch`, ref_side, "whiff2k")(`2K Miss %`),
        .p_zwhiff  = get_res_ecdf(p5_maps, `Pitch`, ref_side, "zwhiff") (`Z-Whiff %`),
        .p_chase   = get_res_ecdf(p5_maps, `Pitch`, ref_side, "chase")  (`Chase %`),
        .p_hh_low  = get_res_ecdf(p5_maps, `Pitch`, ref_side, "hh_low") ( -`HH %` ),
        .p_avg_low = get_res_ecdf(p5_maps, `Pitch`, ref_side, "avg_low")( -`AVG`  ),
        .p_slg_low = get_res_ecdf(p5_maps, `Pitch`, ref_side, "slg_low")( -`SLG`  ),
        .p_gb      = get_res_ecdf(p5_maps, `Pitch`, ref_side, "gb")     ( `GB %` )
      ) %>%
      dplyr::ungroup()

    xs_pt <- tryCatch(pp_expected_stats_cached(df, group_cols = c("TaggedPitchType")),
                      error = function(e) NULL)
    if (!is.null(xs_pt) && "TaggedPitchType" %in% names(xs_pt)) {
      res <- res %>%
        dplyr::left_join(xs_pt, by = c("Pitch" = "TaggedPitchType")) %>%
        dplyr::relocate(dplyr::any_of(c("xBA", "xSLG", "xwOBA")), .after = "SLG")
    }

    g <- res %>%
      gt::gt() %>%
      pp_gt_modern() %>%
      gt::fmt_number(columns = Usage, decimals = 1) %>%
      gt::fmt_percent(columns = c(`0-0 Zone %`, `Strike %`, `Swing %`, `Whiff %`,
                                  `2K Miss %`, `Z-Whiff %`, `Chase %`, `HH %`, `GB %`),
                      decimals = 1) %>%
      gt::fmt_number(columns = c(`AVG`, `SLG`), decimals = 3) %>%
      gt::fmt_number(columns = dplyr::any_of(c("xBA", "xSLG", "xwOBA")), decimals = 3) %>%
      gt::text_transform(locations = gt::cells_body(columns = `Pitch`),
                         fn = function(v) lapply(v, pp_pitch_chip)) %>%
      gt::cols_hide(columns = c(.p_zone00, .p_strike, .p_swing, .p_whiff, .p_whiff2k,
                                .p_zwhiff, .p_chase, .p_hh_low, .p_avg_low, .p_slg_low, .p_gb)) %>%
      gt::text_transform(gt::cells_body(columns = `0-0 Zone %`),
        fn = function(v) mapply(pp_pill, v, res$.p_zone00, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Strike %`),
        fn = function(v) mapply(pp_pill, v, res$.p_strike, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Swing %`),
        fn = function(v) mapply(pp_pill, v, res$.p_swing, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Whiff %`),
        fn = function(v) mapply(pp_pill, v, res$.p_whiff, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `2K Miss %`),
        fn = function(v) mapply(pp_pill, v, res$.p_whiff2k, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Z-Whiff %`),
        fn = function(v) mapply(pp_pill, v, res$.p_zwhiff, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Chase %`),
        fn = function(v) mapply(pp_pill, v, res$.p_chase, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `HH %`),
        fn = function(v) mapply(pp_pill, v, res$.p_hh_low, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `AVG`),
        fn = function(v) mapply(pp_pill, v, res$.p_avg_low, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `SLG`),
        fn = function(v) mapply(pp_pill, v, res$.p_slg_low, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `GB %`),
        fn = function(v) mapply(pp_pill, v, res$.p_gb, SIMPLIFY = FALSE))

    tryCatch(add_xs_shading(g, res, xs_pitch_ref()), error = function(e) g)
  }

  output$pp_results_table_ui <- renderUI({
    side <- input$pp_res_side %||% "A"
    g <- pp_results_gt(side)
    div(class = "pp-slide", HTML(as.character(gt::as_raw_html(g))))
  })

  # ==================== ZONE SUBTAB ====================

  .PP_ZONE_ROWS <- c(all = "All", rhh = "vs RHH", lhh = "vs LHH")
  .PP_ZONE_SLOTS <- 8

  pp_zone_head <- function(rid) {
    div(class = "pp-row-head",
      div(class = "pp-row-lab", unname(.PP_ZONE_ROWS[[rid]])),
      div(class = "pp-seg",
          radioGroupButtons(paste0("zone_", rid, "_count"), label = NULL,
            choices = c("All", "0-0", "Pre-2K", "2 Strikes"),
            selected = "All", size = "xs", individual = TRUE)),
      div(class = "pp-seg",
          radioGroupButtons(paste0("zone_", rid, "_metric"), label = NULL,
            choices = c("Density" = "density", "xSLG" = "xslg", "Whiffs" = "whiff",
                        "Called" = "called", "Balls" = "ball", "Hard-Hit" = "hardhit"),
            selected = "density", size = "xs", individual = TRUE))
    )
  }
  output$pp_zone_all_head <- renderUI(pp_zone_head("all"))
  output$pp_zone_rhh_head <- renderUI(pp_zone_head("rhh"))
  output$pp_zone_lhh_head <- renderUI(pp_zone_head("lhh"))

  pp_zone_row_data <- function(rid) {
    d <- location_base_data()
    if (identical(rid, "rhh")) d <- d[norm_side(d$BatterSide) == "R", , drop = FALSE]
    if (identical(rid, "lhh")) d <- d[norm_side(d$BatterSide) == "L", , drop = FALSE]
    cnt <- input[[paste0("zone_", rid, "_count")]] %||% "All"
    if (!identical(cnt, "All")) d <- d[!is.na(d$CountSplit) & d$CountSplit == cnt, , drop = FALSE]
    d
  }

  pp_zone_types <- function(rid) {
    d <- pp_zone_row_data(rid)
    if (!nrow(d)) return(character(0))
    tb <- sort(table(as.character(d$TaggedPitchType)), decreasing = TRUE)
    tb <- tb[tb >= 8]
    utils::head(names(tb), .PP_ZONE_SLOTS)
  }

  pp_zone_strip_ui <- function(rid) {
    types <- pp_zone_types(rid)
    if (!length(types))
      return(div(style = "color:#8A93A0;font-size:12px;padding:8px 2px;",
                 "No pitches for this split."))
    div(class = "pp-zone-strip pp-fade",
      lapply(seq_along(types), function(i) {
        col <- if (!is.na(pitch_colors[types[i]])) unname(pitch_colors[types[i]]) else "#5B6875"
        div(class = "pp-zone-cell",
          plotOutput(paste0("zone_", rid, "_p", i), height = "230px"),
          div(class = "pp-zone-name", style = paste0("color:", col, ";"), types[i]))
      })
    )
  }
  output$pp_zone_all_strip <- renderUI(pp_zone_strip_ui("all"))
  output$pp_zone_rhh_strip <- renderUI(pp_zone_strip_ui("rhh"))
  output$pp_zone_lhh_strip <- renderUI(pp_zone_strip_ui("lhh"))

  # Per-pitch xSLG is a Python round trip -> compute once per row frame.
  .pp_xslg_cache <- new.env(parent = emptyenv())
  pp_xslg_for <- function(d) {
    n <- nrow(d)
    if (!n) return(numeric(0))
    key <- paste(n,
                 round(sum(d$PlateLocSide, na.rm = TRUE), 4),
                 round(sum(d$PlateLocHeight, na.rm = TRUE), 4), sep = "|")
    hit <- .pp_xslg_cache[[key]]
    if (!is.null(hit) && length(hit) == n) return(hit)
    v <- tryCatch(.mm_perpitch_xslg(d), error = function(e) rep(NA_real_, n))
    if (length(v) != n) v <- rep(NA_real_, n)
    .pp_xslg_cache[[key]] <- v
    v
  }

  # Kernel-smoothed field on a regular grid over the zone window.
  # bw scales with sample size (Silverman-style n^(-1/6)): a 30-pitch panel
  # gets a wider kernel than a 400-pitch panel, so the hot region reads as a
  # broad area of concentration rather than a pinpoint spike on one pitch.
  .pp_bw <- function(n) {
    if (!is.finite(n) || n <= 0) return(0.34)
    max(0.22, min(0.60, 0.30 * (170 / n)^(1/6)))
  }

  .pp_field <- function(x, z, v = NULL, bw = NULL, min_w = 1.4) {
    n <- length(x)
    if (is.null(bw)) bw <- .pp_bw(n)
    gx <- seq(-1.75, 1.75, length.out = 48)
    gz <- seq(0.45, 4.55, length.out = 56)
    g <- expand.grid(x = gx, z = gz)
    W <- exp(-(outer(g$x, x, "-")^2 + outer(g$z, z, "-")^2) / (2 * bw^2))
    ws <- rowSums(W)
    if (is.null(v)) {
      # Clip the ceiling at a high quantile rather than the raw max so one
      # tight cluster cannot swallow the whole colour range.
      hi <- suppressWarnings(stats::quantile(ws, 0.98, na.rm = TRUE))
      if (!is.finite(hi) || hi <= 0) hi <- max(ws)
      g$val <- if (is.finite(hi) && hi > 0) pmin(ws / hi, 1) else NA_real_
      g$val[!is.finite(g$val) | g$val < 0.06] <- NA_real_
    } else {
      g$val <- as.numeric(W %*% v) / pmax(ws, 1e-9)
      g$val[ws < min_w] <- NA_real_
    }
    g
  }

  .PP_HEAT_COLS <- c("#8095C6", "#B7C5DF", "#FFFFFF", "#F2B2AD", "#CD524B")

  # Shared zone-panel renderer for pitchers AND hitters.
  # `metric` semantics:
  #   density        -> density of every pitch in the slice
  #   whiff/called/  -> the slice is FILTERED to those events and the panel
  #   ball/swing/       shows where they happen (points = those events)
  #   hardhit/solid
  #   xslg           -> kernel-weighted xSLG surface over balls in play
  pp_zone_plot <- function(d_row, pitch, metric, pitch_col = "TaggedPitchType") {
    zl <- -0.83083; zr <- 0.83083; zb <- 1.5; zt <- 3.3775
    plate <- data.frame(
      x = c(-0.708, 0.708, 0.708, 0, -0.708),
      y = c(0.62, 0.62, 0.80, 0.95, 0.80))

    shell <- function(note) {
      ggplot() +
        geom_polygon(data = plate, aes(x, y), fill = "#E9ECEF", color = NA) +
        annotate("rect", xmin = zl, xmax = zr, ymin = zb, ymax = zt,
                 fill = NA, color = "#3B4754", linewidth = 0.55) +
        coord_fixed(xlim = c(-1.75, 1.75), ylim = c(0.45, 4.55), expand = FALSE) +
        labs(subtitle = note) +
        theme_void() +
        theme(plot.background  = element_rect(fill = "#FFFFFF", color = NA),
              panel.background = element_rect(fill = "#FFFFFF", color = NA),
              legend.position  = "none",
              plot.subtitle = element_text(hjust = 0.5, size = 8, color = "#8A93A0"),
              plot.margin = margin(2, 2, 2, 2))
    }

    if (is.null(d_row) || !nrow(d_row)) return(shell("no data"))

    # xSLG is the one value-surface metric; compute on the whole row frame
    # (cache-friendly) before narrowing to this pitch type.
    vv <- if (identical(metric, "xslg")) pp_xslg_for(d_row) else NULL

    keep <- !is.na(d_row[[pitch_col]]) & d_row[[pitch_col]] == pitch
    d <- d_row[keep, , drop = FALSE]
    if (!is.null(vv)) vv <- vv[keep]
    if (!nrow(d)) return(shell("no data"))

    pc  <- as.character(d$PitchCall)
    sw  <- !is.na(d$SwingIndicator) & d$SwingIndicator == 1
    ev  <- suppressWarnings(as.numeric(d$ExitSpeed))
    la  <- suppressWarnings(as.numeric(d$Angle))
    bip <- pc == "InPlay"

    # Event metrics FILTER the slice; the points drawn are exactly the
    # events being mapped, so dots and surface always agree.
    sel <- switch(metric,
      "density" = rep(TRUE, nrow(d)),
      "swing"   = sw,
      "whiff"   = sw & pc == "StrikeSwinging",
      "called"  = pc == "StrikeCalled",
      "ball"    = pc %in% c("BallCalled", "BallinDirt", "BallIntentional"),
      "hardhit" = bip & is.finite(ev) & ev >= 95,
      "solid"   = bip & is.finite(ev) & is.finite(la) &
                  ((ev >= 92 & la >= 8) | (ev >= 95 & la >= 0)),
      "xslg"    = bip & is.finite(vv),
      rep(TRUE, nrow(d)))
    sel[is.na(sel)] <- FALSE

    lab <- switch(metric, "density" = "pitches", "swing" = "swings",
                  "whiff" = "whiffs", "called" = "called strikes",
                  "ball" = "balls", "hardhit" = "hard-hit",
                  "solid" = "solid contact", "xslg" = "BIP", "pitches")

    x <- suppressWarnings(as.numeric(d$PlateLocSide))[sel]
    z <- suppressWarnings(as.numeric(d$PlateLocHeight))[sel]
    if (!is.null(vv)) vv <- vv[sel]

    ok <- is.finite(x) & is.finite(z) & (if (is.null(vv)) TRUE else is.finite(vv))
    x <- x[ok]; z <- z[ok]; if (!is.null(vv)) vv <- vv[ok]

    min_n <- if (identical(metric, "xslg")) 12 else 6
    if (length(x) < min_n) return(shell(paste0(length(x), " ", lab)))

    fld <- tryCatch(.pp_field(x, z, vv), error = function(e) NULL)
    if (is.null(fld) || !nrow(fld) || !any(is.finite(fld$val)))
      return(shell(paste0(length(x), " ", lab)))

    rng <- range(fld$val, finite = TRUE)
    if (!all(is.finite(rng)) || diff(rng) < 1e-9) rng <- rng[1] + c(-0.5, 0.5)

    ggplot() +
      geom_raster(data = fld, aes(x, z, fill = val), interpolate = TRUE) +
      scale_fill_gradientn(colours = .PP_HEAT_COLS, limits = rng,
                           na.value = "transparent") +
      geom_polygon(data = plate, aes(x, y), fill = "#E9ECEF", color = NA) +
      annotate("rect", xmin = zl, xmax = zr, ymin = zb, ymax = zt,
               fill = NA, color = "#3B4754", linewidth = 0.55) +
      geom_point(data = data.frame(x = x, z = z), aes(x, z),
                 color = "#2B3440", size = 0.75, alpha = 0.5) +
      coord_fixed(xlim = c(-1.75, 1.75), ylim = c(0.45, 4.55), expand = FALSE) +
      labs(subtitle = paste0(length(x), " ", lab)) +
      theme_void() +
      theme(plot.background  = element_rect(fill = "#FFFFFF", color = NA),
            panel.background = element_rect(fill = "#FFFFFF", color = NA),
            legend.position  = "none",
            plot.subtitle = element_text(hjust = 0.5, size = 8, color = "#8A93A0"),
            plot.margin = margin(2, 2, 2, 2))
  }

  # Register the fixed pool of zone slots (3 rows x 8 pitch types).
  for (.rid in names(.PP_ZONE_ROWS)) {
    for (.slot in seq_len(.PP_ZONE_SLOTS)) {
      local({
        rid <- .rid; ii <- .slot
        output[[paste0("zone_", rid, "_p", ii)]] <- renderPlot({
          types <- pp_zone_types(rid)
          req(length(types) >= ii)
          pp_zone_plot(pp_zone_row_data(rid), types[ii],
                       input[[paste0("zone_", rid, "_metric")]] %||% "density")
        })
      })
    }
  }

  # ==================== ANGLES SUBTAB ====================
  # Vertical gauges: same percentile arc, transposed so Rise is the top
  # end and Depth the bottom end.
  render_gauge_facets_v <- function(g) {
    n <- length(g$types); ncol <- min(5, n)
    arc  <- g$arc;  arc$vx  <- arc$y;  arc$vy  <- arc$x
    mark <- g$mark; mark$vx <- mark$my; mark$vy <- mark$mx

    ggplot() +
      geom_path(data = arc, aes(vx, vy, color = col, group = TaggedPitchType),
                linewidth = 5.5, lineend = "round") +
      scale_color_identity() +
      geom_segment(data = mark, aes(x = 0, y = 0, xend = vx, yend = vy),
                   color = "#c0392b", linewidth = 1.05,
                   arrow = grid::arrow(length = unit(0.11, "inches"))) +
      geom_point(data = mark, aes(vx, vy), size = 4.2, color = "#111",
                 fill = "white", shape = 21, stroke = 1.1) +
      geom_text(data = g$txt, aes(x = 0.15, y = 1.52, label = v_lab),
                hjust = 0.5, size = 3.8, fontface = "bold") +
      geom_text(data = g$txt, aes(x = 0.15, y = 1.34, label = lead_lab),
                hjust = 0.5, size = 3.1, color = "#2e9b3f", fontface = "bold") +
      annotate("text", x = -0.28, y = 1.02, label = g$high_label,
               size = 3.4, fontface = "bold", hjust = 1) +
      annotate("text", x = -0.28, y = -1.02, label = g$low_label,
               size = 3.4, fontface = "bold", hjust = 1) +
      coord_fixed(xlim = c(-1.25, 1.35), ylim = c(-1.35, 1.66), clip = "off") +
      facet_wrap(~ TaggedPitchType, ncol = ncol) +
      theme_void(base_size = 13) +
      theme(strip.text = element_text(face = "bold", size = 13, color = "#0E6E70"),
            panel.spacing = unit(10, "pt"),
            plot.margin = margin(6, 10, 6, 10))
  }

  # Override the horizontal VAA gauge defined earlier with the vertical one.
  output$vaa_percentile_plot <- renderPlot({
    df <- viz_data() %>% dplyr::filter(!is.na(VertApprAngle))
    validate(need(nrow(df) > 0, "No VAA data."))
    pool <- grade_pool_df() %>% dplyr::filter(!is.na(VertApprAngle))
    validate(need(nrow(pool) > 50, "Insufficient pool data for VAA."))
    g <- build_gauge_facets(df, pool, "VertApprAngle",
      low_label = "Depth", high_label = "Rise",
      low_word = "depth", high_word = "rise",
      metric_disp = "VAA")
    validate(need(!is.null(g), "Not enough data per pitch type for VAA."))
    render_gauge_facets_v(g)
  }, height = function() {
    n <- tryCatch(length(build_gauge_facets(
      viz_data() %>% dplyr::filter(!is.na(VertApprAngle)),
      grade_pool_df() %>% dplyr::filter(!is.na(VertApprAngle)),
      "VertApprAngle", "d", "r", "d", "r", "VAA")$types), error = function(e) 4)
    if (is.null(n) || n == 0) n <- 4
    330 * ceiling(n / 5)
  })

  # ---- Angle scatters: pitch-type mean + dashed covariance ellipse ----
  # 68% (1-sigma) ellipse per pitch type, drawn on fixed axes so the shape
  # of an arsenal is comparable between pitchers and across seasons.
  .pp_ellipse <- function(x, y, level = 0.68, n = 90) {
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]; y <- y[ok]
    if (length(x) < 8) return(NULL)
    mu <- c(mean(x), mean(y))
    S  <- tryCatch(stats::cov(cbind(x, y)), error = function(e) NULL)
    if (is.null(S) || any(!is.finite(S))) return(NULL)
    ev <- tryCatch(eigen(S, symmetric = TRUE), error = function(e) NULL)
    if (is.null(ev) || any(ev$values <= 0)) return(NULL)
    r  <- sqrt(stats::qchisq(level, df = 2))
    th <- seq(0, 2 * pi, length.out = n)
    pts <- t(mu + ev$vectors %*% diag(sqrt(ev$values)) %*% rbind(cos(th), sin(th)) * r)
    data.frame(ex = pts[, 1], ey = pts[, 2])
  }

  .pp_angle_chart <- function(df, xcol, ycol, xlab, ylab,
                              xlim, ylim, xbreaks, ybreaks, short) {
    df <- df %>%
      dplyr::filter(!is.na(.data[[xcol]]), !is.na(.data[[ycol]]),
                    !is.na(TaggedPitchType), TaggedPitchType != "Other")
    validate(need(nrow(df) > 0, "No angle data for this selection."))

    types <- df %>% dplyr::count(TaggedPitchType) %>%
      dplyr::filter(n >= 8) %>% dplyr::arrange(dplyr::desc(n)) %>%
      dplyr::pull(TaggedPitchType)
    validate(need(length(types) > 0, "Not enough pitches per type."))

    ell <- do.call(rbind, lapply(types, function(pt) {
      d <- df[df$TaggedPitchType == pt, , drop = FALSE]
      e <- .pp_ellipse(d[[xcol]], d[[ycol]])
      if (is.null(e)) return(NULL)
      e$TaggedPitchType <- pt
      e
    }))

    centers <- df %>%
      dplyr::filter(TaggedPitchType %in% types) %>%
      dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(cx = mean(.data[[xcol]], na.rm = TRUE),
                       cy = mean(.data[[ycol]], na.rm = TRUE),
                       n  = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(hover = paste0("<b>", TaggedPitchType, "</b>",
                                   "<br>", short, "V: ", sprintf("%.1f", cy), "\u00b0",
                                   "<br>", short, "H: ", sprintf("%.1f", cx), "\u00b0",
                                   "<br>n: ", n))

    p <- ggplot()
    if (!is.null(ell) && nrow(ell)) {
      p <- p +
        geom_polygon(data = ell, aes(ex, ey, fill = TaggedPitchType,
                                     group = TaggedPitchType),
                     alpha = 0.22, color = NA) +
        geom_path(data = ell, aes(ex, ey, color = TaggedPitchType,
                                  group = TaggedPitchType),
                  linetype = "dashed", linewidth = 0.6)
    }
    p +
      geom_hline(yintercept = 0, color = "#8A93A0", linewidth = 0.4) +
      geom_vline(xintercept = 0, color = "#8A93A0", linewidth = 0.4) +
      geom_point(data = centers, aes(cx, cy, fill = TaggedPitchType, text = hover),
                 shape = 21, size = 3.6, color = "#2B3440", stroke = 0.5) +
      scale_fill_manual(values = pitch_colors, name = "Pitch") +
      scale_color_manual(values = pitch_colors, guide = "none") +
      scale_x_continuous(limits = xlim, breaks = xbreaks) +
      scale_y_continuous(limits = ylim, breaks = ybreaks) +
      labs(x = xlab, y = ylab) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#E6EAEE", linewidth = 0.35,
                                        linetype = "dashed"),
        panel.background = element_rect(fill = "#FFFFFF", color = NA),
        plot.background  = element_rect(fill = "#FFFFFF", color = NA),
        axis.title = element_text(size = 11, color = "#5B6875"),
        axis.text  = element_text(size = 10, color = "#77828F"),
        legend.position = "none"
      )
  }

  output$approach_angle_plot <- plotly::renderPlotly({
    g <- .pp_angle_chart(viz_data(), "HorzApprAngle", "VertApprAngle",
                         "Horizontal Approach Angle (\u00b0)",
                         "Vertical Approach Angle (\u00b0)",
                         xlim = c(-10, 10), ylim = c(-20, 0),
                         xbreaks = seq(-10, 10, 5), ybreaks = seq(-20, 0, 5),
                         short = "AA")
    plotly::ggplotly(g, tooltip = "text") %>%
      plotly::layout(autosize = TRUE, plot_bgcolor = "#FFFFFF",
                     paper_bgcolor = "#FFFFFF") %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  })

  output$release_angle_plot <- plotly::renderPlotly({
    d <- viz_data()
    validate(need(all(c("VertRelAngle", "HorzRelAngle") %in% names(d)),
                  "Release-angle columns not available in this data."))
    g <- .pp_angle_chart(d, "HorzRelAngle", "VertRelAngle",
                         "Horizontal Release Angle (\u00b0)",
                         "Vertical Release Angle (\u00b0)",
                         xlim = c(-10, 10), ylim = c(-10, 10),
                         xbreaks = seq(-10, 10, 5), ybreaks = seq(-10, 10, 5),
                         short = "RA")
    plotly::ggplotly(g, tooltip = "text") %>%
      plotly::layout(autosize = TRUE, plot_bgcolor = "#FFFFFF",
                     paper_bgcolor = "#FFFFFF") %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  })

  # ==================== MODELING SUBTAB ====================

  output$pp_modeling_trend <- plotly::renderPlotly({
    metric <- input$pp_mod_metric %||% "proStuff+"
    fr <- .pp_trend_frame(pp_trend_source(input$pp_mod_dates), metric,
                          input$pp_mod_split %||% "ind",
                          input$pp_mod_trend_side %||% "B")
    .pp_trend_plotly(fr, metric, TRUE, paste0(metric, " by outing"))
  })

  pp_model_gt <- function(side) {
    df <- filtered_data()
    if (side %in% c("R", "L")) df <- df %>% dplyr::filter(norm_side(BatterSide) == side)
    validate(need(nrow(df) > 0, "No pitches for this split."))
    total <- nrow(df)

    mod <- df %>%
      dplyr::group_by(`Pitch` = TaggedPitchType) %>%
      dplyr::summarise(
        `#`             = dplyr::n(),
        Usage           = 100 * dplyr::n() / total,
        `proStuff+`     = mean(stuff_plus, na.rm = TRUE),
        `proLocation+`  = mean(location_plus, na.rm = TRUE),
        `proPitching+`  = mean(pitching_plus, na.rm = TRUE),
        .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(Usage)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        .p_stuff    = get_mov_ecdf(p5_maps, `Pitch`, "stuffplus")(`proStuff+`),
        .p_location = get_mov_ecdf(p5_maps, `Pitch`, "locationplus")(`proLocation+`),
        .p_pitching = get_mov_ecdf(p5_maps, `Pitch`, "pitchingplus")(`proPitching+`)
      ) %>%
      dplyr::ungroup()

    mod %>%
      gt::gt() %>%
      pp_gt_modern() %>%
      gt::fmt_number(columns = Usage, decimals = 1) %>%
      gt::fmt_number(columns = c(`proStuff+`, `proLocation+`, `proPitching+`), decimals = 0) %>%
      gt::text_transform(locations = gt::cells_body(columns = `Pitch`),
                         fn = function(v) lapply(v, pp_pitch_chip)) %>%
      gt::cols_hide(columns = c(.p_stuff, .p_location, .p_pitching)) %>%
      gt::text_transform(gt::cells_body(columns = `proStuff+`),
        fn = function(v) mapply(pp_pill, v, mod$.p_stuff, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `proLocation+`),
        fn = function(v) mapply(pp_pill, v, mod$.p_location, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `proPitching+`),
        fn = function(v) mapply(pp_pill, v, mod$.p_pitching, SIMPLIFY = FALSE))
  }

  output$pp_model_tbl_all <- gt::render_gt(pp_model_gt("A"))
  output$pp_model_tbl_r   <- gt::render_gt(pp_model_gt("R"))
  output$pp_model_tbl_l   <- gt::render_gt(pp_model_gt("L"))
