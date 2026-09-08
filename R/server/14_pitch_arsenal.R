  # ===================== PITCH ARSENAL (Player Plans) =====================
  # pitch types this guy actually has, with counts (drives the picker)
  pa_avail_types <- reactive({
    gp <- input$global_pitcher
    if (is.null(gp) || !nzchar(gp)) return(NULL)
    pool <- tryCatch(selected_data(), error = function(e) NULL)
    if (is.null(pool) || !is.data.frame(pool) || !"Pitcher" %in% names(pool)) return(NULL)
    dp <- pool[pool$Pitcher == gp & !is.na(pool$TaggedPitchType) &
                 pool$TaggedPitchType != "", , drop = FALSE]
    if (nrow(dp) == 0) return(NULL)
    tb <- table(dp$TaggedPitchType)
    sort(tb[tb > 0], decreasing = TRUE)
  })

  output$pa_pitch_picker <- renderUI({
    tb <- pa_avail_types()
    if (is.null(tb) || length(tb) == 0)
      return(div(style = "color:#9CA3AF; font-size:12px;", "No pitch types found."))
    ch <- setNames(names(tb), sprintf("%s (%d)", names(tb), as.integer(tb)))
    checkboxGroupInput("pa_keep_types", NULL, choices = ch,
                       selected = names(tb), inline = TRUE)
  })

  pa_model <- reactive({
    gp <- input$global_pitcher
    if (is.null(gp) || !nzchar(gp)) return(NULL)
    pool <- tryCatch(selected_data(), error = function(e) NULL)
    if (is.null(pool) || !is.data.frame(pool) || nrow(pool) == 0) return(NULL)
    if (!"Pitcher" %in% names(pool) || !(gp %in% pool$Pitcher)) return(NULL)
    # drop pitch types the user unticked -- for THIS pitcher only, so the rest
    # of the league still backs the similarity pool and the familiarity curve
    # Widen the reference pool. selected_data() is Coastal-only for Coastal arms
    # and a SINGLE pitcher's frame for NCAA arms -- neither gives a league to
    # compare against. Fold in full_data_p5_compare (the Matchup Matrix pool) so
    # similarity, the staff concentration anchor and the exposure reference all
    # have a real league behind them, and so the tab works for opponent arms.
    ref <- tryCatch(get("full_data_p5_compare", inherits = TRUE),
                    error = function(e) NULL)
    if (is.data.frame(ref) && nrow(ref) > 0 && "Pitcher" %in% names(ref)) {
      shared <- intersect(names(ref), names(pool))
      if (length(shared) >= 8) {
        extra <- ref[!(ref$Pitcher %in% unique(pool$Pitcher)), shared, drop = FALSE]
        if (nrow(extra) > 0)
          pool <- dplyr::bind_rows(pool[, shared, drop = FALSE], extra)
      }
    }
    # unticked pitch types are RE-TAGGED onto the kept types by shape (not
    # discarded), so a splitter logged as a changeup still counts -- as a
    # splitter. Local to this model only.
    keep <- input$pa_keep_types
    note <- NULL
    if (!is.null(keep) && length(keep) >= 2) {
      rt   <- tryCatch(pa_retag(pool, gp, keep),
                       error = function(e) list(pool = pool, note = NULL))
      pool <- rt$pool; note <- rt$note
    }
    res <- tryCatch(pa_build_model(pool, gp),
             error = function(e) { cat("PitchArsenal:", conditionMessage(e), "\n"); NULL })
    if (!is.null(res)) res$retag_note <- note
    res
  })

  output$pa_header <- renderUI({
    gp <- input$global_pitcher
    if (is.null(gp) || !nzchar(gp))
      return(div(style = "color:#888; padding:20px;",
                 "Select a pitcher above to see the pitch-arsenal diagnostic."))
    m <- pa_model()
    if (is.null(m))
      return(div(style = "color:#888; padding:20px;",
                 paste0("Not enough pitch data for ", gp,
                        " to model a mix (need pitch type, count, and stuff grades).")))
    div(h3(paste0(m$pitcher, " \u2014 Pitch Arsenal"), class = "brand-teal"),
        p(style = "color:#666; margin-top:-6px;",
          sprintf("%d pitches modeled. Current vs modeled mix below is a %s, not a prescription.",
                  m$n_pitches, "diagnostic")))
  })

  output$pa_debug <- renderPrint({
    m <- pa_model()
    if (is.null(m) || is.null(m$dbg)) { cat("no model\n"); return(invisible(NULL)) }
    d <- m$dbg
    cat("pool rows        :", d$n_rows_pool, " | pitcher rows:", d$n_rows_pitch,
        " | pitchers in pool:", d$n_pitchers, "\n")
    cat("unmapped events  :", d$ev_na_pct, "%   <- if ~100, PitchCall/PlayResult are not being read\n")
    cat("sd(run value)    :", d$rv_sd, " | sd(count-adj):", d$rv_adj_sd,
        "  <- if 0, every pitch scored the same\n")
    cat("shape feats ok   :", d$feat_ok, "\n\n")
    tab <- data.frame(pitch = m$types,
                      thrown = as.integer(m$n_by),
                      sim_n  = as.integer(d$n_sim),
                      simval = as.numeric(d$v_sim),
                      value  = as.numeric(d$value100),
                      setup  = as.numeric(d$seq100),
                      setupn = as.integer(d$setup_n),
                      compos = as.numeric(d$composite),
                      usecred = round(as.numeric(d$cred_u), 3),
                      rawmix = sprintf("%.0f%%", 100 * as.numeric(d$modeled_raw)),
                      check.names = FALSE)
    print(tab, row.names = FALSE)
    cat("\nsd(composite)    :", d$comp_sd,
        "  <- if 0, all pitches look identical -> mix comes out even\n")
    cat("HHI observed     :", d$h_obs, " | even mix:", d$h_uni, "\n")
    cat("target HHI       :", d$target_h, " | achieved:", d$achieved_h,
        " | even mix:", d$h_uni, "\n")
    cat("anchor (staff)   :", d$h_obs, " | exposure ratio:", d$div_scale,
        " | delta:", d$delta_used, "\n")
    cat("cost spread      :", d$cost_spread,
        "  <- if 0, all pitches score the same and the mix must come out even\n")
    cat("A:", d$A, " | N:", d$N_total, "\n")
  })

  output$pa_retag_note <- renderUI({
    m <- pa_model()
    if (is.null(m) || is.null(m$retag_note)) return(NULL)
    div(style = "font-size:11.5px; color:#0f7d7d; margin-top:6px;", m$retag_note)
  })

  output$pa_mix_plot <- renderPlot({
    m <- pa_model(); req(m)
    df <- data.frame(
      pitch   = m$types,
      current = m$actual  * 100,
      ideal   = m$modeled * 100,
      stringsAsFactors = FALSE)
    df <- df[order(df$current), ]                 # biggest usage on top
    df$pitch <- factor(df$pitch, levels = df$pitch)
    cols <- pitch_colors[as.character(df$pitch)]  # exact TruMedia shape colors
    cols[is.na(cols)] <- "gray50"
    names(cols) <- as.character(df$pitch)

    ggplot(df, aes(y = pitch)) +
      # connector: current -> ideal
      geom_segment(aes(x = current, xend = ideal, yend = pitch, color = pitch),
                   linewidth = 1.6, lineend = "round", alpha = 0.5) +
      # current usage: hollow circle (white fill, colored ring)
      geom_point(aes(x = current, color = pitch), shape = 21, fill = "white",
                 size = 6.5, stroke = 1.9) +
      # modeled ideal: filled circle
      geom_point(aes(x = ideal, color = pitch, fill = pitch),
                 shape = 21, size = 6.5, stroke = 1.9) +
      # value labels (current above, ideal below so they never collide)
      geom_text(aes(x = current, label = sprintf("%.0f%%", current)),
                vjust = -1.5, size = 3.5, color = "#6b7280") +
      geom_text(aes(x = ideal, label = sprintf("%.0f%%", ideal)),
                vjust = 2.4, size = 3.9, fontface = "bold",
                color = cols[as.character(df$pitch)]) +
      scale_color_manual(values = cols, guide = "none") +
      scale_fill_manual(values = cols, guide = "none") +
      scale_x_continuous(labels = function(z) paste0(z, "%"),
                         expand = expansion(mult = c(0.10, 0.14))) +
      labs(x = "Usage", y = NULL,
           title = "Pitch mix: current vs. modeled ideal",
           subtitle = "\u25CB current usage      \u25CF modeled ideal") +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(color = "#eef0f2"),
        plot.title    = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(color = "#8a8f98", size = 11.5,
                                     margin = margin(b = 8)),
        axis.text.y   = element_text(face = "bold", size = 12.5, color = cols),
        axis.title.x  = element_text(color = "#8a8f98"))
  })

  output$pa_diag <- renderUI({
    m <- pa_model(); req(m)
    msg <- if (identical(m$stuff_tier, "below-average")) {
      paste0("Stuff grade ", round(m$stuff_plus),
             " (below the staff median). This is the population where mix ",
             "actually moves the needle \u2014 if his mix is far from modeled, ",
             "it's a plausible part of why he's struggling.")
    } else if (identical(m$stuff_tier, "above-average")) {
      paste0("Stuff grade ", round(m$stuff_plus),
             " (above the staff median). Good stuff is a cushion that hides a ",
             "messy mix \u2014 be skeptical the mix is the story here, even if ",
             "the gap is large.")
    } else {
      paste("No stuff grades in this data source, so the mix-vs-stuff split",
            "isn't available for this pitcher. Read the gap on its own.")
    }
    famline <- if (isTRUE(m$fam$identified)) {
      sprintf("Familiarity tax measured from your data: %.4f RV per repeat look (z = %.1f, %d matchups).",
              m$fam$delta, m$fam$z, m$fam$n_matchups)
    } else {
      sprintf("Familiarity tax: college data couldn't identify it (%s) \u2014 using the MLB prior (%.4f).",
              m$fam$reason, m$fam$delta_used)
    }
    warn <- NULL
    if (!is.null(m$dbg)) {
      if (isTRUE(m$dbg$ev_na_pct > 90)) {
        warn <- paste0("No run value could be read from this data: ",
                       m$dbg$ev_na_pct, "% of pitches did not map to an outcome ",
                       "(check PitchCall / PlayResult). The mix below is not ",
                       "meaningful.")
      } else if (isTRUE(m$dbg$comp_sd == 0) || isTRUE(!is.finite(m$dbg$comp_sd))) {
        warn <- paste("Every pitch scored identically, so the model has nothing",
                      "to separate them and returns an even mix. See Model",
                      "internals below.")
      }
    }
    div(style = "background:#f4f6f6; border-radius:8px; padding:12px 16px;",
        if (!is.null(warn))
          div(style = paste("background:#fff4e5; border:1px solid #f0c38a;",
                            "border-radius:6px; padding:8px 10px; margin-bottom:8px;",
                            "font-size:12.5px; color:#8a5a00;"), warn),
        strong(sprintf("Mix gap: %.1f pp", m$gap_pp)), " \u2014 ",
        span(if (identical(m$stuff_tier, "unknown")) "stuff tier: n/a."
             else sprintf("stuff tier: %s.", m$stuff_tier)), br(),
        em(msg), br(),
        span(style = "color:#555; font-size:12px;", famline), br(),
        span(style = "color:#555; font-size:12px;",
             sprintf("Spread anchor: staff mixes at HHI %s; weights RV %.0f%% / setup %.0f%% / familiarity %.0f%%.",
                     if (is.null(m$h_obs) || !is.finite(m$h_obs)) "n/a"
                     else sprintf("%.2f", m$h_obs),
                     100*PA_W_RV, 100*PA_W_SEQ, 100*PA_W_FAM)))
  })

  output$pa_rv_table <- renderTable({
    m <- pa_model(); req(m)
    data.frame(
      Pitch         = m$types,
      Thrown        = as.integer(m$n_by),
      `Value/100`   = sprintf("%+.2f", as.numeric(m$value) * 100),
      `Current %`   = sprintf("%.0f%%", m$actual  * 100),
      `Modeled %`   = sprintf("%.0f%%", m$modeled * 100),
      `Modeled RV`  = sprintf("%+.2f", m$rv_change),
      check.names = FALSE)
  }, striped = TRUE, hover = TRUE, width = "100%")
