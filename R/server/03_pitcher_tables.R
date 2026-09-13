  observeEvent(coastal_selected_data(), {
    df <- coastal_selected_data()
    
    # Update 2026 Roster pickers if that column exists
    if ("Roster_2026" %in% names(bio)) {
      Roster_choices <- unique(bio$Roster_2026)
      Roster_choices <- Roster_choices[!is.na(Roster_choices)]
      if (length(Roster_choices) == 0) Roster_choices <- c("Yes", "No")
      
      updatePickerInput(session, "g_Roster",           choices = Roster_choices, selected = Roster_choices)
      updatePickerInput(session, "Roster_2026_spray",  choices = Roster_choices, selected = "Yes")
    }
    
    # Pitcher choices, intersecting with 2026 Roster if requested
    if ("Pitcher" %in% names(df)) {
      # Start with all available
      available_pitchers <- sort(unique(df$Pitcher))
      
      # If we have Roster, filter by selected flag(s)
      if ("Roster_2026" %in% names(bio)) {
        # Use currently selected Roster filter if present, default to "Yes"
        Roster_filter <- if (!is.null(input$g_Roster)) input$g_Roster else "Yes"
        # Accept Yes variations
        Roster_yes <- c("Yes", "YES", "yes", "Y")
        if (any(Roster_filter %in% Roster_yes)) {
          Roster_pitchers <- bio %>%
            filter(Roster_2026 %in% Roster_filter) %>%
            pull(Pitcher_FL)
          # Intersect with data names (First Last)
          ap <- df %>%
            filter(Pitcher %in% Roster_pitchers) %>%
            pull(Pitcher) %>%
            unique() %>%
            sort()
          if (length(ap) > 0) available_pitchers <- ap
        }
      }
      
      # Fallback so UI is never empty
      if (length(available_pitchers) == 0) {
        available_pitchers <- sort(unique(df$Pitcher))
      }
      # 2027 roster pitchers ride along so freshman cards can open a page
      # even before they have spring game data (their Fall 2026 pill works).
      available_pitchers <- union(available_pitchers,
                                  c(roster27_pitcher_fl, bullpen_pitchers_fl))
      
      # The identity search (R/18_player_registry.R) owns the pick. The
      # hidden name input only ever holds the open player and never snaps
      # to a default arm when the season data refreshes.
      cp <- session$userData$cur_player
      keep_global <- if (!is.null(cp) && identical(cp$kind, "pit")) cp$name else NULL
      session$userData$global_choices <- NULL
      if (!is.null(keep_global) && !identical(input$global_pitcher, keep_global))
        updateSelectizeInput(session, "global_pitcher", choices = setNames(keep_global, keep_global),
                             selected = keep_global, server = FALSE)

      updateSelectInput(session, "pitcher",        choices = available_pitchers)
      updateSelectInput(session, "selectedPitcher",choices = available_pitchers)
      updateSelectInput(session, "pitcher_spray",  choices = available_pitchers)
      updatePickerInput(session, "pitcher_velo",   choices = available_pitchers)
      updatePickerInput(session, "pitchertype_metrics", choices = available_pitchers)
    }
    
    # Update date ranges from the active data
    if ("Date" %in% names(df)) {
      rng <- range(df$Date, na.rm = TRUE)
      if (is.finite(rng[1]) && is.finite(rng[2])) {
        updateDateRangeInput(session, "g_dateRange",      start = rng[1], end = rng[2])
        updateDateRangeInput(session, "dateRange_perc",   start = rng[1], end = rng[2])
        updateDateRangeInput(session, "dateRange_spray",  start = rng[1], end = rng[2])
        updateDateRangeInput(session, "dateRange_metrics",start = rng[1], end = rng[2])
        updateDateRangeInput(session, "dateRange_leader", start = rng[1], end = rng[2])
        updateDateRangeInput(session, "dateRange_velo",   start = rng[1], end = rng[2])
      }
    }
    
    # BatterSide choices
    if ("BatterSide" %in% names(df)) {
      bs <- unique(df$BatterSide)
      updatePickerInput(session, "g_BatterSide",       choices = bs, selected = bs)
      updatePickerInput(session, "BatterSide_perc",    choices = bs, selected = bs)
      updatePickerInput(session, "BatterSide_heatmap", choices = bs, selected = bs)
      updatePickerInput(session, "BatterSide_plp",     choices = bs, selected = bs)
      updatePickerInput(session, "BatterSide_spray",   choices = bs, selected = bs)
      updatePickerInput(session, "BatterSide_metrics", choices = bs, selected = bs)
      updatePickerInput(session, "BatterSide_leader",  choices = bs, selected = bs)
    }
    
    # Pitch types
    if ("TaggedPitchType" %in% names(df)) {
      pt <- unique(df$TaggedPitchType)
      updatePickerInput(session, "g_pitchtype",      choices = pt, selected = pt)
      updatePickerInput(session, "pitchtype_perc",   choices = pt, selected = pt)
      updatePickerInput(session, "pitchtype_date",   choices = pt, selected = pt)
      updatePickerInput(session, "pitchtype_spray",  choices = pt, selected = pt)
      updatePickerInput(session, "pitchtype_metrics",choices = pt, selected = pt)
      updatePickerInput(session, "pitchtype_leader", choices = pt, selected = pt)
      updatePickerInput(session, "pitchtype_velo",   choices = pt, selected = pt)
    }
    
    # Velocity slider
    if ("RelSpeed" %in% names(df)) {
      vr <- range(df$RelSpeed, na.rm = TRUE)
      if (is.finite(vr[1]) && is.finite(vr[2])) {
        updateSliderInput(session, "g_velo",
                          min = floor(vr[1]),
                          max = ceiling(vr[2]),
                          value = c(floor(vr[1]), ceiling(vr[2])))
      }
    }
  })
  
  observeEvent(selected_data(), {
    df <- selected_data()
    if ("Pitcher" %in% names(df)) {
      all_pitchers <- sort(unique(df$Pitcher))
      updatePickerInput(session, "pitcher_spray",
                        choices = all_pitchers,
                        selected = all_pitchers)   # select all by default
    }
  })
  
  observeEvent(list(selected_data(), input$Roster_2026_custom), {
    df <- selected_data()
    if (!"Pitcher" %in% names(df)) {
      updatePickerInput(session, "pitcher_custom", choices = character(0))
      return()
    }
    
    # Start with all pitchers in the active dataset
    available_pitchers <- sort(unique(df$Pitcher))
    
    # If we have a Roster filter selected, intersect with bio
    if (!is.null(input$Roster_2026_custom) && "Roster_2026" %in% names(bio)) {
      Roster_pitchers <- bio %>%
        dplyr::filter(Roster_2026 %in% input$Roster_2026_custom) %>%
        dplyr::pull(Pitcher_FL)
      if (length(Roster_pitchers) > 0) {
        available_pitchers <- sort(intersect(available_pitchers, Roster_pitchers))
      }
    }
    
    # Fallback so it never ends up empty
    if (length(available_pitchers) == 0) {
      available_pitchers <- sort(unique(df$Pitcher))
    }
    
    updatePickerInput(
      session, "pitcher_custom",
      choices = available_pitchers,
      selected = available_pitchers  # Select all by default
    )
  })
  
  # Update date range for custom leaderboard
  observeEvent(selected_data(), {
    df <- selected_data()
    if ("Date" %in% names(df)) {
      rng <- range(df$Date, na.rm = TRUE)
      if (is.finite(rng[1]) && is.finite(rng[2])) {
        updateDateRangeInput(session, "dateRange_custom", start = rng[1], end = rng[2])
      }
    }
    if ("BatterSide" %in% names(df)) {
      bs <- sort(unique(df$BatterSide))
      updatePickerInput(session, "BatterSide_custom", choices = bs, selected = bs)
    }
    if ("TaggedPitchType" %in% names(df)) {
      pt <- sort(unique(df$TaggedPitchType))
      updatePickerInput(session, "pitchtype_custom", choices = pt, selected = pt)
    }
  })
  
  
  observeEvent(input$g_Roster, {
    choices <- filtered_pitcher_choices()
    updateSelectInput(session, "pitcher", choices = choices)
  }, ignoreInit = TRUE)
  
  observeEvent(input$Roster_2026_spray, {
    df <- selected_data()
    if (!"Pitcher" %in% names(df)) return()
    
    if (exists("bio") && "Roster_2026" %in% names(bio)) {
      filtered_bio <- bio %>% filter(`Roster_2026` %in% input$Roster_2026_spray)
      choices <- df %>% 
        filter(Pitcher %in% filtered_bio$Pitcher_FL) %>% 
        pull(Pitcher) %>% 
        unique() %>% 
        sort()
    } else {
      choices <- sort(unique(df$Pitcher))
    }
    
    updateSelectInput(session, "pitcher_spray", choices = choices)
  }, ignoreInit = TRUE)
  
  bio_key <- reactive({
    req(input$global_pitcher)
    bio_match <- bio[
      bio$Pitcher_FL == input$global_pitcher | bio$Pitcher == input$global_pitcher,
    ]
    validate(need(nrow(bio_match) > 0, paste("No bio found for", input$global_pitcher)))
    bio_match$Pitcher[1]  # returns "Last, First" key if you need it
  })
  
  # Process data for overview table
  processed_data <- reactive({
    df <- selected_data()
    req(input$g_dateRange, input$g_BatterSide, input$g_pitchtype)
    
    filtered_data2 <- df %>% 
      filter(
        safe_as_date(Date) >= as.Date(input$g_dateRange[1]) &
          safe_as_date(Date) <= as.Date(input$g_dateRange[2]),
        BatterSide %in% input$g_BatterSide,
        TaggedPitchType %in% input$g_pitchtype,
        RelSpeed >= input$g_velo[1] & RelSpeed <= input$g_velo[2]
      )
    filtered_data2 <- apply_global_filters(filtered_data2)
    
    # Calculate statistics for each pitcher
    summary_data <- filtered_data2 %>%
      group_by(Pitcher) %>%
      summarize(
        'BF' = sum(PAindicator, na.rm = TRUE),
        'Avg FB Velo' = round(mean(RelSpeed[TaggedPitchType == "Fastball" | TaggedPitchType == 'Sinker'], na.rm = TRUE), 1),
        'Max FB Velo' = round(max(RelSpeed[TaggedPitchType == "Fastball" | TaggedPitchType == 'Sinker'], na.rm = TRUE), 1),
        'Strike %' = round(sum(StrikeIndicator, na.rm = TRUE) / n() * 100, 1),
        'FB Strike %' = round(sum(FBstrikeind, na.rm = TRUE) / sum(FBindicator, na.rm = TRUE) * 100, 1),
        'OS Strike %' = round(sum(OSstrikeind, na.rm = TRUE) / sum(OSindicator, na.rm = TRUE) * 100, 1),
        'E+A %' = round((sum(EarlyIndicator, na.rm = TRUE) + sum(AheadIndicator, na.rm = TRUE)) / sum(PAindicator, na.rm = TRUE) * 100, 1),
        '1PK %' = round(sum(FPSindicator, na.rm = TRUE) / sum(FPindicator, na.rm = TRUE) * 100, 1),
        'Whiff %' = round(sum(WhiffIndicator, na.rm = TRUE) / sum(SwingIndicator, na.rm = TRUE) * 100, 1),
        'IZ Whiff %' = round(sum(Zwhiffind, na.rm = TRUE) / sum(Zswing, na.rm = TRUE) * 100, 1),
        'Chase %' = round(sum(Chaseindicator, na.rm = TRUE) / sum(OutofZone, na.rm = TRUE) * 100, 1),
        'K %' = round(sum(is_k, na.rm = TRUE) / sum(PAindicator, na.rm = TRUE) * 100, 1),
        'BB %' = round(sum(WalkIndicator, na.rm = TRUE) / sum(PAindicator, na.rm = TRUE) * 100, 1),
        'LOO %' = round(sum(LOOindicator, na.rm = TRUE) / sum(LeadOffIndicator, na.rm = TRUE) * 100, 1),
        'AVG' = round(sum(HitIndicator, na.rm = TRUE) / sum(ABindicator, na.rm = TRUE), 3),
        'OPS' = round((sum(OnBaseindicator, na.rm = TRUE) / sum(PAindicator, na.rm = TRUE)) + (sum(totalbases, na.rm = TRUE) / sum(ABindicator, na.rm = TRUE)), 3),
        'HH %' = round(sum(HHindicator, na.rm = TRUE) / sum(biphh, na.rm = TRUE) * 100, 1),
        'GB %' = round(sum(GBindicator, na.rm = TRUE) / sum(BIPind, na.rm = TRUE) * 100, 1),
        'Stuff+' = round(mean(stuff_plus, na.rm = TRUE)),
        'Pitching+' = round(mean(pitching_plus, na.rm = TRUE)),
        'Location+' = round(mean(location_plus, na.rm = TRUE)),
        .groups = 'drop'
      ) %>%
      mutate(across(where(is.numeric), ~ifelse(is.nan(.) | is.infinite(.), NA_real_, .)))
  })
             
brks   <- seq(82, 98, 1)
clrs   <- colorRampPalette(c("#E1463E","white","#00840D"))(length(brks) + 1)

# Max FB Velo (goal: 93.0) — higher better
brks2  <- seq(85, 101, 1)
clrs2  <- colorRampPalette(c("#E1463E","white","#00840D"))(length(brks2) + 1)

brksloo <- seq(45, 80, 1)
clrsloo <- colorRampPalette(c("#E1463E", "white", "#00840D"))(length(brksloo) + 1)

# Strike % (goal: 62) — higher better
brks3  <- seq(54, 70, 1)
clrs3  <- colorRampPalette(c("#E1463E","white","#00840D"))(length(brks3) + 1)

# FB Strike % (goal: 64) — higher better
brks4  <- seq(56, 72, 1)
clrs4  <- colorRampPalette(c("#E1463E","white","#00840D"))(length(brks4) + 1)

# OS Strike % (goal: 60) — higher better
brks5  <- seq(50, 70, 1)
clrs5  <- colorRampPalette(c("#E1463E","white","#00840D"))(length(brks5) + 1)

# E+A % (goal: 68) — higher better
brksepa <- seq(58, 78, 1)
clrsepa <- colorRampPalette(c("#E1463E","white","#00840D"))(length(brksepa) + 1)

# 1PK % (goal: 59) — higher better
brks6  <- seq(49, 69, 1)
clrs6  <- colorRampPalette(c("#E1463E","white","#00840D"))(length(brks6) + 1)

# Whiff % (goal: 28) — higher better
brks7  <- seq(18, 38, 1)
clrs7  <- colorRampPalette(c("#E1463E","white","#00840D"))(length(brks7) + 1)

# IZ Whiff % (goal: 20) — higher better
brks8  <- seq(10, 30, 1)
clrs8  <- colorRampPalette(c("#E1463E","white","#00840D"))(length(brks8) + 1)

# Chase % (goal: 25) — higher better
brkschase <- seq(15, 35, 1)
clrschase <- colorRampPalette(c("#E1463E","white","#00840D"))(length(brkschase) + 1)

# Plate % (goal: 63) — higher better
brksplate <- seq(53, 73, 1)
clrsplate <- colorRampPalette(c("#E1463E","white","#00840D"))(length(brksplate) + 1)

# K % (goal: 25) — higher better
brks9  <- seq(15, 35, 1)
clrs9  <- colorRampPalette(c("#E1463E","white","#00840D"))(length(brks9) + 1)

# BB % (goal: 11) — LOWER better
brksbb <- seq(3, 19, 1)
clrsbb <- colorRampPalette(c("#00840D","white","#E1463E"))(length(brksbb) + 1)

# AVG (goal: .247) — LOWER better
brksavg <- seq(.150, .344, .001)
clrsavg <- colorRampPalette(c("#00840D","white","#E1463E"))(length(brksavg) + 1)

# OPS (goal: .745) — LOWER better
brksops <- seq(.500, .990, .001)
clrsops <- colorRampPalette(c("#00840D","white","#E1463E"))(length(brksops) + 1)

# Barrel % (goal: 18) — LOWER better
brksbarrel <- seq(8, 28, 1)
clrsbarrel <- colorRampPalette(c("#00840D","white","#E1463E"))(length(brksbarrel) + 1)

# IZ Barrel % (goal: 21) — LOWER better
brksizbarrel <- seq(11, 31, 1)
clrsizbarrel <- colorRampPalette(c("#00840D","white","#E1463E"))(length(brksizbarrel) + 1)

# HH % (goal: 39) — LOWER better
brkshh <- seq(29, 49, 1)
clrshh <- colorRampPalette(c("#00840D","white","#E1463E"))(length(brkshh) + 1)

# GB % (goal: 43) — higher better
brksgb <- seq(33, 53, 1)
clrsgb <- colorRampPalette(c("#E1463E","white","#00840D"))(length(brksgb) + 1)

# Stuff+ / Pitching+ / Location+ — keep centered on 100 (league average)
brksstuff    <- seq(80, 120, 1)
clrsstuff    <- colorRampPalette(c("#E1463E","white","#00840D"))(length(brksstuff) + 1)
brkspitching <- seq(80, 120, 1)
clrspitching <- colorRampPalette(c("#E1463E","white","#00840D"))(length(brkspitching) + 1)
  
  # ===== Per-pitcher game list + pitched-day highlighting =====
  # Rebuilds the Games picker for whoever is selected (coastal or NCAA):
  # one row per game with date, opponent, and pitches thrown. Selection
  # stays empty by default ("All games") so rebuilding the list doesn't
  # trigger extra render cascades; a prior selection survives only if
  # those games belong to the new pitcher too.
  observeEvent(list(selected_data(), input$global_pitcher, input$main_tabs,
                    player_mode()), {
    if (identical(player_mode(), "hitter")) return()  # hitter context owns the picker there
    gp <- input$global_pitcher
    df <- tryCatch(selected_data(), error = function(e) NULL)
    if (is.null(gp) || !nzchar(gp) || is.null(df) ||
        !is.data.frame(df) || nrow(df) == 0 || !"Pitcher" %in% names(df)) return()

    pdat <- df %>% dplyr::filter(Pitcher == gp, !is.na(Date))
    if (nrow(pdat) == 0 || !"GameID" %in% names(pdat)) {
      updatePickerInput(session, "game_filter_spring26",
                        label = "Games (pitched)",
                        choices = character(0), selected = character(0))
      session$sendCustomMessage("setPitchedDates", list())
      return()
    }

    opp_of <- function(bt) {
      bt <- bt[!is.na(bt) & nzchar(bt)]
      if (length(bt) == 0) return("Unknown")
      top <- names(sort(table(bt), decreasing = TRUE))[1]
      pt <- tryCatch(prettify_team(top), error = function(e) top)
      if (is.na(pt) || !nzchar(pt)) top else pt
    }

    games <- pdat %>%
      dplyr::group_by(GameID) %>%
      dplyr::summarise(
        Date = suppressWarnings(max(Date, na.rm = TRUE)),
        Opp  = opp_of(if ("BatterTeam" %in% names(pdat)) BatterTeam else character(0)),
        N    = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(Date))

    labs <- paste0(format(games$Date, "%b %d, %Y"), " \u2014 vs ", games$Opp,
                   "  (", games$N, " pitches)")
    choices <- setNames(games$GameID, labs)

    keep <- intersect(isolate(input$game_filter_spring26) %||% character(0),
                      games$GameID)
    updatePickerInput(session, "game_filter_spring26",
                      label = "Games (pitched)",
                      choices = choices, selected = keep)

    # Highlight the days this pitcher actually pitched in the date picker
    session$sendCustomMessage("setPitchedDates",
                              as.list(sort(unique(as.character(pdat$Date)))))
  })
  
  # ===== Expected-stat shading (xBA / xSLG / xwOBA) =====================
  # p5_ref carries no expected stats, so there is no ecdf in p5_maps for
  # them -- which is why these columns printed plain while every stat
  # around them was shaded. Build the reference from the grading pool
  # instead: one cached proModel call, per pitch type, so an xwOBA on a
  # slider is graded against other sliders rather than against fastballs.
  .xs_ref_cache <- new.env(parent = emptyenv())
  xs_pitch_ref <- reactive({
    pool_name <- input$global_pool %||% "D1"
    hit <- .xs_ref_cache[[pool_name]]
    if (!is.null(hit)) return(hit)
    pool <- tryCatch(grade_pool_df(), error = function(e) NULL)
    if (is.null(pool) || !nrow(pool)) return(NULL)
    xs <- tryCatch(pp_pool_xstats(pool, by = "PitcherPitchType"),   # pitcher_season_pitch_type
                   error = function(e) NULL)
    if (is.null(xs) || !"TaggedPitchType" %in% names(xs)) return(NULL)
    have <- intersect(c("xBA", "xSLG", "xwOBA"), names(xs))
    if (!length(have)) return(NULL)
    out <- xs %>%
      dplyr::group_by(TaggedPitchType) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(have),
        list(m = ~ mean(.x, na.rm = TRUE), s = ~ stats::sd(.x, na.rm = TRUE)),
        .names = "{.col}__{.fn}"), .groups = "drop")
    .xs_ref_cache[[pool_name]] <- out
    out
  })

  # Lower is better, so the z is flipped before pnorm -- a low xwOBA
  # comes out green, matching AVG / SLG / HH%.
  xs_shade_pct <- function(ref, pitches, stat, vals) {
    n <- length(vals)
    if (is.null(ref) || !nrow(ref)) return(rep(NA_real_, n))
    mc <- paste0(stat, "__m"); sc <- paste0(stat, "__s")
    if (!all(c(mc, sc) %in% names(ref))) return(rep(NA_real_, n))
    idx <- match(as.character(pitches), as.character(ref$TaggedPitchType))
    mu <- ref[[mc]][idx]; sg <- ref[[sc]][idx]
    v  <- suppressWarnings(as.numeric(vals))
    ok <- is.finite(v) & is.finite(mu) & is.finite(sg) & sg > 0
    out <- rep(NA_real_, n)
    out[ok] <- stats::pnorm(-(v[ok] - mu[ok]) / sg[ok])
    out
  }

  # Appended to each split table. No-ops cleanly when the model is
  # unavailable, leaving the cells plain rather than faking a percentile.
  add_xs_shading <- function(tb, res, ref) {
    if (is.null(res) || !"Pitch" %in% names(res)) return(tb)
    for (sc in intersect(c("xBA", "xSLG", "xwOBA"), names(res))) {
      pv <- xs_shade_pct(ref, res$Pitch, sc, res[[sc]])
      if (all(is.na(pv))) next
      local({
        col <- sc; pct <- pv
        tb <<- gt::text_transform(tb,
          locations = gt::cells_body(columns = dplyr::all_of(col)),
          fn = function(v) mapply(shade_cell_html, v, pct, SIMPLIFY = FALSE))
      })
    }
    tb
  }

  output$vrhh_table <- gt::render_gt({
    df <- filtered_data() %>% dplyr::filter(norm_side(BatterSide) == "R")
    req(nrow(df) > 0)
    
    total <- nrow(df)
    
    res <- df %>%
      dplyr::group_by(`Pitch` = TaggedPitchType) %>%
      dplyr::summarise(
        `#`         = dplyr::n(),
        Usage       = 100 * dplyr::n() / total,  # visible, not colored
        `0-0 Zone %` = pdiv(sum(StrikeZoneIndicator[Balls == 0 & Strikes == 0], na.rm = TRUE),
                            sum(Balls == 0 & Strikes == 0, na.rm = TRUE)),
        `Strike %`  = pdiv(sum(StrikeIndicator,    na.rm = TRUE), dplyr::n()),
        `Swing %`   = pdiv(sum(SwingIndicator,     na.rm = TRUE), dplyr::n()),
        `AVG`       = pdiv(sum(HitIndicator,       na.rm = TRUE), sum(ABindicator,    na.rm = TRUE)),
        `SLG`       = pdiv(sum(totalbases,         na.rm = TRUE), sum(ABindicator,    na.rm = TRUE)),
        `Whiff %`   = pdiv(sum(WhiffIndicator,     na.rm = TRUE), sum(SwingIndicator, na.rm = TRUE)),
        `2K Miss %` = pdiv(sum(WhiffIndicator[Strikes == 2], na.rm = TRUE),
                           sum(SwingIndicator[Strikes == 2], na.rm = TRUE)),
        `Z-Whiff %` = pdiv(sum(Zwhiffind,          na.rm = TRUE), sum(Zswing,         na.rm = TRUE)),
        `Chase %`   = pdiv(sum(Chaseindicator,     na.rm = TRUE), sum(OutofZone,      na.rm = TRUE)),
        `HH %`      = pdiv(sum(HHindicator,        na.rm = TRUE), sum(biphh,          na.rm = TRUE)),
        `GB %`      = pdiv(sum(GBindicator,        na.rm = TRUE), sum(BIPind,         na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(Usage)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        # high-is-better
        .p_zone00  = get_res_ecdf(p5_maps, `Pitch`, "R", "zone00") (`0-0 Zone %`),
        .p_strike  = get_res_ecdf(p5_maps, `Pitch`, "R", "strike") (`Strike %`),
        .p_swing   = get_res_ecdf(p5_maps, `Pitch`, "R", "swing")  (`Swing %`),
        .p_whiff   = get_res_ecdf(p5_maps, `Pitch`, "R", "whiff")  (`Whiff %`),
        .p_whiff2k = get_res_ecdf(p5_maps, `Pitch`, "R", "whiff2k")(`2K Miss %`),
        .p_zwhiff  = get_res_ecdf(p5_maps, `Pitch`, "R", "zwhiff") (`Z-Whiff %`),
        .p_chase   = get_res_ecdf(p5_maps, `Pitch`, "R", "chase")  (`Chase %`),
        .p_hh_low  = get_res_ecdf(p5_maps, `Pitch`, "R", "hh_low") ( -`HH %` ),
        .p_avg_low = get_res_ecdf(p5_maps, `Pitch`, "R", "avg_low")( -`AVG`  ),
        .p_slg_low = get_res_ecdf(p5_maps, `Pitch`, "R", "slg_low")( -`SLG`  ),
        .p_gb      = get_res_ecdf(p5_maps, `Pitch`, "R", "gb")     ( `GB %` )
      ) %>%
      dplyr::ungroup()

    # Expected stats per pitch type for this side's PAs (terminal-pitch
    # attribution, same as the overall table)
    xs_pt <- pp_expected_stats_cached(df, group_cols = c("TaggedPitchType"))
    if (!is.null(xs_pt) && "TaggedPitchType" %in% names(xs_pt)) {
      res <- res %>%
        dplyr::left_join(xs_pt, by = c("Pitch" = "TaggedPitchType")) %>%
        dplyr::relocate(dplyr::any_of(c("xBA", "xSLG", "xwOBA")), .after = "SLG")
    }

    res %>%
      gt::gt() %>%
      gt_theme_guardian() %>%
      gt::cols_label(.list = c(`#` = "#")) %>%
      # format FIRST so text_transform receives display strings
      gt::fmt_number(columns = Usage, decimals = 1) %>%
      gt::fmt_percent(columns = c(`0-0 Zone %`,`Strike %`,`Swing %`,`Whiff %`,`2K Miss %`,`Z-Whiff %`,`Chase %`,`HH %`,`GB %`), decimals = 1) %>%
      gt::fmt_number(columns = c(`AVG`,`SLG`), decimals = 3) %>%
      gt::fmt_number(columns = dplyr::any_of(c("xBA","xSLG","xwOBA")), decimals = 3) %>%
      gt::data_color(columns = `Pitch`, colors = pitch_color_fn) %>%
      gt::cols_hide(columns = c(.p_zone00,.p_strike,.p_swing,.p_whiff,.p_whiff2k,.p_zwhiff,.p_chase,.p_hh_low,.p_avg_low,.p_slg_low,.p_gb)) %>%
      # higher-better
      gt::text_transform(gt::cells_body(columns = `0-0 Zone %`), fn = function(v) mapply(shade_cell_html, v, res$.p_zone00, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Strike %`), fn = function(v) mapply(shade_cell_html, v, res$.p_strike, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Swing %`), fn = function(v) mapply(shade_cell_html, v, res$.p_swing, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Whiff %`),  fn = function(v) mapply(shade_cell_html, v, res$.p_whiff,  SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `2K Miss %`), fn = function(v) mapply(shade_cell_html, v, res$.p_whiff2k, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Z-Whiff %`),fn = function(v) mapply(shade_cell_html, v, res$.p_zwhiff, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Chase %`),  fn = function(v) mapply(shade_cell_html, v, res$.p_chase,  SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `HH %`), fn = function(v) mapply(shade_cell_html, v, res$.p_hh_low,  SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `AVG`),  fn = function(v) mapply(shade_cell_html, v, res$.p_avg_low, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `SLG`),  fn = function(v) mapply(shade_cell_html, v, res$.p_slg_low, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `GB %`),  fn = function(v) mapply(shade_cell_html, v, res$.p_gb,     SIMPLIFY = FALSE)) %>%
      add_xs_shading(res, xs_pitch_ref())
  })
  
  
  
  output$overall_table <- gt::render_gt({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    total <- nrow(df)
    
    res <- df %>%
      dplyr::group_by(`Pitch` = TaggedPitchType) %>%
      dplyr::summarise(
        `#`         = dplyr::n(),
        Usage       = 100 * dplyr::n() / total,
        `0-0 Zone %` = pdiv(sum(StrikeZoneIndicator[Balls == 0 & Strikes == 0], na.rm = TRUE),
                            sum(Balls == 0 & Strikes == 0, na.rm = TRUE)),
        `Strike %`  = pdiv(sum(StrikeIndicator,    na.rm = TRUE), dplyr::n()),
        `Swing %`   = pdiv(sum(SwingIndicator,     na.rm = TRUE), dplyr::n()),
        `AVG`       = pdiv(sum(HitIndicator,       na.rm = TRUE), sum(ABindicator,    na.rm = TRUE)),
        `SLG`       = pdiv(sum(totalbases,         na.rm = TRUE), sum(ABindicator,    na.rm = TRUE)),
        `Whiff %`   = pdiv(sum(WhiffIndicator,     na.rm = TRUE), sum(SwingIndicator, na.rm = TRUE)),
        `2K Miss %` = pdiv(sum(WhiffIndicator[Strikes == 2], na.rm = TRUE),
                           sum(SwingIndicator[Strikes == 2], na.rm = TRUE)),
        `Z-Whiff %` = pdiv(sum(Zwhiffind,          na.rm = TRUE), sum(Zswing,         na.rm = TRUE)),
        `Chase %`   = pdiv(sum(Chaseindicator,     na.rm = TRUE), sum(OutofZone,      na.rm = TRUE)),
        `HH %`      = pdiv(sum(HHindicator,        na.rm = TRUE), sum(biphh,          na.rm = TRUE)),
        `GB %`      = pdiv(sum(GBindicator,        na.rm = TRUE), sum(BIPind,         na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(Usage)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        # high-better
        .p_zone00  = get_res_ecdf(p5_maps, `Pitch`, "L", "zone00") (`0-0 Zone %`),
        .p_strike  = get_res_ecdf(p5_maps, `Pitch`, "L", "strike") (`Strike %`),
        .p_swing   = get_res_ecdf(p5_maps, `Pitch`, "L", "swing")  (`Swing %`),
        .p_whiff   = get_res_ecdf(p5_maps, `Pitch`, "L", "whiff")  (`Whiff %`),
        .p_whiff2k = get_res_ecdf(p5_maps, `Pitch`, "L", "whiff2k")(`2K Miss %`),
        .p_zwhiff  = get_res_ecdf(p5_maps, `Pitch`, "L", "zwhiff") (`Z-Whiff %`),
        .p_chase   = get_res_ecdf(p5_maps, `Pitch`, "L", "chase")  (`Chase %`),
        .p_hh_low  = get_res_ecdf(p5_maps, `Pitch`, "L", "hh_low") ( -`HH %` ),
        .p_avg_low = get_res_ecdf(p5_maps, `Pitch`, "L", "avg_low")( -`AVG`  ),
        .p_slg_low = get_res_ecdf(p5_maps, `Pitch`, "R", "slg_low")( -`SLG`  ),
        .p_gb      = get_res_ecdf(p5_maps, `Pitch`, "L", "gb")     ( `GB %` )
      ) %>%
      dplyr::ungroup()

    # Expected stats per pitch type: PA-terminal events attribute to the
    # pitch that ended the AB (guide §5), so this grouping is supported.
    xs_pt <- pp_expected_stats_cached(df, group_cols = c("TaggedPitchType"))
    if (!is.null(xs_pt) && "TaggedPitchType" %in% names(xs_pt)) {
      res <- res %>%
        dplyr::left_join(xs_pt, by = c("Pitch" = "TaggedPitchType")) %>%
        dplyr::relocate(dplyr::any_of(c("xBA", "xSLG", "xwOBA")), .after = "SLG")
    }

    res %>%
      gt::gt() %>%
      gt_theme_guardian() %>%
      gt::cols_label(.list = c(`#` = "#")) %>%
      gt::fmt_number(columns = Usage, decimals = 1) %>%
      gt::fmt_percent(columns = c(`0-0 Zone %`,`Strike %`,`Swing %`,`Whiff %`,`2K Miss %`,`Z-Whiff %`,`Chase %`,`HH %`,`GB %`), decimals = 1) %>%
      gt::fmt_number(columns = c(`AVG`,`SLG`), decimals = 3) %>%
      gt::fmt_number(columns = dplyr::any_of(c("xBA","xSLG","xwOBA")), decimals = 3) %>%
      gt::data_color(columns = `Pitch`, colors = pitch_color_fn) %>%
      gt::cols_hide(columns = c(.p_zone00,.p_strike,.p_swing,.p_whiff,.p_whiff2k,.p_zwhiff,.p_chase,.p_hh_low,.p_avg_low,.p_slg_low,.p_gb)) %>%
      # higher-better
      gt::text_transform(gt::cells_body(columns = `0-0 Zone %`), fn = function(v) mapply(shade_cell_html, v, res$.p_zone00, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Strike %`), fn = function(v) mapply(shade_cell_html, v, res$.p_strike, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Swing %`), fn = function(v) mapply(shade_cell_html, v, res$.p_swing, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Whiff %`),  fn = function(v) mapply(shade_cell_html, v, res$.p_whiff,  SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `2K Miss %`), fn = function(v) mapply(shade_cell_html, v, res$.p_whiff2k, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Z-Whiff %`),fn = function(v) mapply(shade_cell_html, v, res$.p_zwhiff, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Chase %`),  fn = function(v) mapply(shade_cell_html, v, res$.p_chase,  SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `HH %`), fn = function(v) mapply(shade_cell_html, v, res$.p_hh_low,  SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `AVG`),  fn = function(v) mapply(shade_cell_html, v, res$.p_avg_low, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `SLG`),  fn = function(v) mapply(shade_cell_html, v, res$.p_slg_low, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `GB %`),  fn = function(v) mapply(shade_cell_html, v, res$.p_gb,     SIMPLIFY = FALSE)) %>%
      add_xs_shading(res, xs_pitch_ref())
  })
  
  
  
  output$vlhh_table <- gt::render_gt({
    df <- filtered_data() %>% dplyr::filter(norm_side(BatterSide) == "L")
    req(nrow(df) > 0)
    
    total <- nrow(df)
    
    res <- df %>%
      dplyr::group_by(`Pitch` = TaggedPitchType) %>%
      dplyr::summarise(
        `#`         = dplyr::n(),
        Usage       = 100 * dplyr::n() / total,
        `0-0 Zone %` = pdiv(sum(StrikeZoneIndicator[Balls == 0 & Strikes == 0], na.rm = TRUE),
                            sum(Balls == 0 & Strikes == 0, na.rm = TRUE)),
        `Strike %`  = pdiv(sum(StrikeIndicator,    na.rm = TRUE), dplyr::n()),
        `Swing %`   = pdiv(sum(SwingIndicator,     na.rm = TRUE), dplyr::n()),
        `AVG`       = pdiv(sum(HitIndicator,       na.rm = TRUE), sum(ABindicator,    na.rm = TRUE)),
        `SLG`       = pdiv(sum(totalbases,         na.rm = TRUE), sum(ABindicator,    na.rm = TRUE)),
        `Whiff %`   = pdiv(sum(WhiffIndicator,     na.rm = TRUE), sum(SwingIndicator, na.rm = TRUE)),
        `2K Miss %` = pdiv(sum(WhiffIndicator[Strikes == 2], na.rm = TRUE),
                           sum(SwingIndicator[Strikes == 2], na.rm = TRUE)),
        `Z-Whiff %` = pdiv(sum(Zwhiffind,          na.rm = TRUE), sum(Zswing,         na.rm = TRUE)),
        `Chase %`   = pdiv(sum(Chaseindicator,     na.rm = TRUE), sum(OutofZone,      na.rm = TRUE)),
        `HH %`      = pdiv(sum(HHindicator,        na.rm = TRUE), sum(biphh,          na.rm = TRUE)),
        `GB %`      = pdiv(sum(GBindicator,        na.rm = TRUE), sum(BIPind,         na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(Usage)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        # high-better
        .p_zone00  = get_res_ecdf(p5_maps, `Pitch`, "L", "zone00") (`0-0 Zone %`),
        .p_strike  = get_res_ecdf(p5_maps, `Pitch`, "L", "strike") (`Strike %`),
        .p_swing   = get_res_ecdf(p5_maps, `Pitch`, "L", "swing")  (`Swing %`),
        .p_whiff   = get_res_ecdf(p5_maps, `Pitch`, "L", "whiff")  (`Whiff %`),
        .p_whiff2k = get_res_ecdf(p5_maps, `Pitch`, "L", "whiff2k")(`2K Miss %`),
        .p_zwhiff  = get_res_ecdf(p5_maps, `Pitch`, "L", "zwhiff") (`Z-Whiff %`),
        .p_chase   = get_res_ecdf(p5_maps, `Pitch`, "L", "chase")  (`Chase %`),
        .p_hh_low  = get_res_ecdf(p5_maps, `Pitch`, "L", "hh_low") ( -`HH %` ),
        .p_avg_low = get_res_ecdf(p5_maps, `Pitch`, "L", "avg_low")( -`AVG`  ),
        .p_slg_low = get_res_ecdf(p5_maps, `Pitch`, "R", "slg_low")( -`SLG`  ),
        .p_gb      = get_res_ecdf(p5_maps, `Pitch`, "L", "gb")     ( `GB %` )
      ) %>%
      dplyr::ungroup()

    # Expected stats per pitch type for this side's PAs (terminal-pitch
    # attribution, same as the overall table)
    xs_pt <- pp_expected_stats_cached(df, group_cols = c("TaggedPitchType"))
    if (!is.null(xs_pt) && "TaggedPitchType" %in% names(xs_pt)) {
      res <- res %>%
        dplyr::left_join(xs_pt, by = c("Pitch" = "TaggedPitchType")) %>%
        dplyr::relocate(dplyr::any_of(c("xBA", "xSLG", "xwOBA")), .after = "SLG")
    }

    res %>%
      gt::gt() %>%
      gt_theme_guardian() %>%
      gt::cols_label(.list = c(`#` = "#")) %>%
      gt::fmt_number(columns = Usage, decimals = 1) %>%
      gt::fmt_percent(columns = c(`0-0 Zone %`,`Strike %`,`Swing %`,`Whiff %`,`2K Miss %`,`Z-Whiff %`,`Chase %`,`HH %`,`GB %`), decimals = 1) %>%
      gt::fmt_number(columns = c(`AVG`,`SLG`), decimals = 3) %>%
      gt::fmt_number(columns = dplyr::any_of(c("xBA","xSLG","xwOBA")), decimals = 3) %>%
      gt::data_color(columns = `Pitch`, colors = pitch_color_fn) %>%
      gt::cols_hide(columns = c(.p_zone00,.p_strike,.p_swing,.p_whiff,.p_whiff2k,.p_zwhiff,.p_chase,.p_hh_low,.p_avg_low,.p_slg_low,.p_gb)) %>%
      # higher-better
      gt::text_transform(gt::cells_body(columns = `0-0 Zone %`), fn = function(v) mapply(shade_cell_html, v, res$.p_zone00, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Strike %`), fn = function(v) mapply(shade_cell_html, v, res$.p_strike, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Swing %`), fn = function(v) mapply(shade_cell_html, v, res$.p_swing, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Whiff %`),  fn = function(v) mapply(shade_cell_html, v, res$.p_whiff,  SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `2K Miss %`), fn = function(v) mapply(shade_cell_html, v, res$.p_whiff2k, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Z-Whiff %`),fn = function(v) mapply(shade_cell_html, v, res$.p_zwhiff, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `Chase %`),  fn = function(v) mapply(shade_cell_html, v, res$.p_chase,  SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `HH %`), fn = function(v) mapply(shade_cell_html, v, res$.p_hh_low,  SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `AVG`),  fn = function(v) mapply(shade_cell_html, v, res$.p_avg_low, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `SLG`),  fn = function(v) mapply(shade_cell_html, v, res$.p_slg_low, SIMPLIFY = FALSE)) %>%
      gt::text_transform(gt::cells_body(columns = `GB %`),  fn = function(v) mapply(shade_cell_html, v, res$.p_gb,     SIMPLIFY = FALSE)) %>%
      add_xs_shading(res, xs_pitch_ref())
  })
  
  
  output$movement_table <- gt::render_gt({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    total <- nrow(df)
    
    mov <- df %>%
      dplyr::group_by(`Pitch` = TaggedPitchType) %>%
      dplyr::summarise(
        `#`                 = dplyr::n(),
        Usage               = 100 * dplyr::n() / total,   # visible, NOT colored
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
        `Stuff+`            = mean(stuff_plus, na.rm = TRUE),
        `Pitching+`            = mean(pitching_plus, na.rm = TRUE),
        `Location+`            = mean(location_plus, na.rm = TRUE),
        .groups = "drop"
      ) %>% dplyr::arrange(dplyr::desc(Usage))
    
    # P5 percentiles ONLY for columns we color
    mov <- mov %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        .p_speed_avg = get_mov_ecdf(p5_maps, `Pitch`, "speed")(`Avg Velo`),
        .p_speed_max = get_mov_ecdf(p5_maps, `Pitch`, "speed")(`Max Velo`),
        .p_spin      = get_mov_ecdf(p5_maps, `Pitch`, "spin") (`Avg Spin`),
        .p_ext       = get_mov_ecdf(p5_maps, `Pitch`, "ext")  (`Avg Ext (ft)`),
        .p_stuff     = get_mov_ecdf(p5_maps, `Pitch`, "stuffplus")(`Stuff+`),
        .p_pitching  = get_mov_ecdf(p5_maps, `Pitch`, "pitchingplus")(`Pitching+`),
        .p_location  = get_mov_ecdf(p5_maps, `Pitch`, "locationplus")(`Location+`)
      ) %>%
      dplyr::ungroup()
    
    mov %>%
      gt::gt() %>%
      gt_theme_guardian() %>%
      gt::cols_label(.list = c(`#` = "#")) %>%
      gt::fmt_number(columns = c(Usage), decimals = 1) %>%
      gt::fmt_number(
        columns = c(`Avg Velo`,`Max Velo`,`Avg IVB`,`Avg HB`,
                    `Avg Rel Ht (ft)`,`Avg Rel Side (ft)`,`Avg Ext (ft)`, VAA, HAA),
        decimals = 1
      ) %>%
      gt::fmt_number(columns = c(`Avg Spin`,`Stuff+`, `Pitching+`, `Location+`), decimals = 0) %>%
      gt::data_color(columns = `Pitch`, colors = pitch_color_fn) %>%
      gt::cols_hide(columns = c(.p_speed_avg,.p_speed_max,.p_spin,.p_ext,.p_stuff,.p_pitching,.p_location)) %>%
      # COLOR ONLY these columns
      gt::text_transform(
        locations = gt::cells_body(columns = `Avg Velo`),
        fn = function(vals) mapply(shade_cell_html, vals, mov$.p_speed_avg, SIMPLIFY = FALSE)
      ) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = `Max Velo`),
        fn = function(vals) mapply(shade_cell_html, vals, mov$.p_speed_max, SIMPLIFY = FALSE)
      ) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = `Avg Spin`),
        fn = function(vals) mapply(shade_cell_html, vals, mov$.p_spin, SIMPLIFY = FALSE)
      ) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = `Avg Ext (ft)`),
        fn = function(vals) mapply(shade_cell_html, vals, mov$.p_ext, SIMPLIFY = FALSE)
      ) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = `Stuff+`),
        fn = function(vals) mapply(shade_cell_html, vals, mov$.p_stuff, SIMPLIFY = FALSE)
      ) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = `Pitching+`),
        fn = function(vals) mapply(shade_cell_html, vals, mov$.p_pitching, SIMPLIFY = FALSE)
      ) %>%
      gt::text_transform(
        locations = gt::cells_body(columns = `Location+`),
        fn = function(vals) mapply(shade_cell_html, vals, mov$.p_location, SIMPLIFY = FALSE)
      )
  })
  
  
  
