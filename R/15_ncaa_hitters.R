# NCAA HITTER DIRECTORY + LOADER (mirrors the pitcher pipeline)
# ----------------------------------------------------------------------------
# Every pitch a hitter has faced, from the NCAA parquet datasets (2025 + 2026,
# both halves of every tracked game). On load the frame gets: pitch-type
# reclassification for the (non-Coastal) pitchers faced, proModel scoring so
# every pitch carries pitching_xrv / Pitching+, and the hitter Process model.
# ============================================================================
.hb_env <- new.env(parent = emptyenv())

ncaa_batter_directory <- function() {
  if (!is.null(.hb_env$dir)) return(.hb_env$dir)
  pull <- function(ds, src) {
    if (is.null(ds)) return(NULL)
    out <- tryCatch(
      ds %>% dplyr::distinct(Batter, BatterTeam) %>% dplyr::collect() %>%
        as.data.frame(),
      error = function(e) NULL)
    if (is.null(out) || nrow(out) == 0) return(NULL)
    out$src <- src
    out
  }
  raw <- dplyr::bind_rows(Filter(Negate(is.null), list(
    pull(NCAA25_DS, "2025"), pull(NCAA26_DS, "2026"))))
  if (nrow(raw) == 0) {
    .hb_env$dir <- data.frame(display = character(0), teams = character(0))
    return(.hb_env$dir)
  }
  d <- raw %>%
    dplyr::filter(!is.na(Batter), nzchar(Batter)) %>%
    dplyr::mutate(
      display   = ifelse(grepl(",", Batter), normalize_lastfirst(Batter), Batter),
      team_disp = prettify_team(BatterTeam))
  # raw pairs kept for transfer-aware current-team resolution
  .hb_env$pairs <- d %>%
    dplyr::distinct(display, BatterTeam, team_disp, src)
  d <- d %>%
    dplyr::group_by(display) %>%
    dplyr::summarise(
      raw_names = list(unique(Batter)),
      teams = paste(sort(unique(team_disp[!is.na(team_disp)])), collapse = " / "),
      srcs  = list(unique(src)),
      .groups = "drop") %>%
    dplyr::arrange(display)
  .hb_env$dir <- d
  d
}

# Batter's CURRENT (2026) team for the TruMedia pull — 2026 directory pair
# first, DRS newest-team fallback, then whatever team the directory has.
# Transfer-aware for the same reason the pitcher loader is.
tm_current_batter_team <- function(display_name) {
  if (is.null(display_name) || !nzchar(display_name)) return(NULL)
  pr <- .hb_env$pairs
  if (!is.null(pr) && nrow(pr) > 0) {
    r26 <- pr[pr$display == display_name & pr$src == "2026" &
                !is.na(pr$team_disp) & nzchar(pr$team_disp), , drop = FALSE]
    if (nrow(r26) > 0) return(r26$team_disp[1])
  }
  r <- tryCatch(drs_lookup(display_name), error = function(e) NULL)
  if (!is.null(r) && "newestTeamName" %in% names(r) &&
      !is.na(r$newestTeamName) && nzchar(as.character(r$newestTeamName))) {
    return(as.character(r$newestTeamName))
  }
  dir <- .hb_env$dir
  if (!is.null(dir)) {
    row <- dir[dir$display == display_name, , drop = FALSE]
    if (nrow(row) > 0 && nzchar(row$teams[1]))
      return(trimws(strsplit(row$teams[1], "/")[[1]][1]))
  }
  NULL
}

.hb_cache <- new.env(parent = emptyenv())
load_ncaa_batter_data <- function(display_name, team_disp = NULL) {
  team_key <- if (is.null(team_disp) || !nzchar(team_disp %||% "")) "" else as.character(team_disp)
  key <- paste(gsub("[^A-Za-z0-9]", "_", display_name),
               gsub("[^A-Za-z0-9]", "_", team_key), sep = "__")
  cached <- .hb_cache[[key]]
  if (!is.null(cached)) return(cached)

  dir <- ncaa_batter_directory()
  raws <- unique(c(unlist(dir$raw_names[dir$display == display_name]),
                   display_name))
  raws <- raws[!is.na(raws) & nzchar(raws)]
  if (length(raws) == 0) return(data.frame())

  # Same collector the pitcher loader uses. The old hand-rolled version
  # dropped a hardcoded list of five time-ish column names, which missed
  # whichever column actually carried the time32 type -- hence the
  # repeated "Unsupported cast from string to time32" failures.
  # .arrow_collect scans the schema and drops every time32/time64/duration
  # column, whatever it happens to be called.
  frames_pull <- function(ds) {
    .arrow_collect(ds, function(q) dplyr::filter(q, Batter %in% raws),
                   context = "NCAA batter pull")
  }

  # 2026 pitch data comes from TruMedia (API is the primary source; the
  # parquet dataset is only a fallback if the API is unreachable) —
  # exactly like load_ncaa_pitcher_data. 2025 stays on the parquet
  # dataset unchanged. Transfer-aware: search on the batter's CURRENT
  # (2026) team.
  # Prefer a team the CALLER already knows (the matchup matrix and the
  # hitter workbook both pick a batter team explicitly). Reverse-resolving
  # it from the directory only works for hitters already in .hb_env$pairs,
  # which is why opponent bats fell through to parquet.
  if (is.null(team_disp) || !nzchar(team_disp %||% ""))
    team_disp <- tm_current_batter_team(display_name)
  tm26 <- tryCatch(tm_load_batter_pitches(display_name, team_disp),
                   error = function(e) {
                     cat("TruMedia 2026 batter load failed:",
                         conditionMessage(e), "\n")
                     NULL
                   })
  src26 <- if (!is.null(tm26) && nrow(tm26) > 0) {
    cat("  [Hitter 2026 source] TruMedia API:", nrow(tm26), "pitches\n")
    tm26
  } else {
    cat("  [Hitter 2026 source] TruMedia unavailable (",
        .tm_env$last_error %||% "no detail", ") - parquet fallback\n")
    frames_pull(NCAA26_DS)
  }

  frames <- Filter(function(d) !is.null(d) && nrow(d) > 0,
                   list(frames_pull(NCAA25_DS), src26))
  if (length(frames) == 0) return(data.frame())
  df <- dplyr::bind_rows(harmonize_types(frames))
  df <- .mm_dedupe_uid(df)   # NA-safe: rows without a PitchUID all survive
  df$Batter <- display_name
  if ("Pitcher" %in% names(df))
    df$Pitcher <- ifelse(grepl(",", df$Pitcher),
                         normalize_lastfirst(df$Pitcher), df$Pitcher)
  df <- normalize_plate_location(df)
  if ("Date" %in% names(df)) df$Date <- safe_as_date(df$Date)

  # corrected pitch types for the (non-Coastal) arms this hitter faced
  df <- reclassify_noncoastal_pitches(df, context = paste0("hitter:", display_name))
  # Pitching+ per pitch, so the Process model can condition on pitch quality
  df <- tryCatch(score_promodel(df, "ncaa_ind"), error = function(e) {
    cat("  [HitterLoad] proModel scoring failed:", conditionMessage(e), "\n"); df
  })
  # Decision / Contact / Power / Process per pitch
  df <- tryCatch(hp_score_pitches(df), error = function(e) {
    cat("  [HitterLoad] process scoring failed:", conditionMessage(e), "\n"); df
  })
  .hb_cache[[key]] <- df
  df
}


ncaa_pitcher_cache      <- new.env(parent = emptyenv())  # fully scored frames
ncaa_pitcher_fast_cache <- new.env(parent = emptyenv())  # parquet-only frames

# ---- Persistent scout cache (same-day validity) ---------------------------
# /data survives restarts on the deployment host (same convention as
# scouting notes); tempdir() keeps local runs working. Frames are keyed by
# pitcher + today's date: pitch data only changes when a game finishes, so
# same-day reuse is safe and tomorrow's first load naturally refreshes.
.SCOUT_CACHE_VER <- 1L
.scout_cache_dir <- function() {
  d <- if (dir.exists("/data")) "/data/scout_cache"
       else file.path(tempdir(), "scout_cache")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}
.scout_cache_path <- function(key) {
  file.path(.scout_cache_dir(),
            paste0("full_v", .SCOUT_CACHE_VER, "_", key, "_",
                   format(Sys.Date(), "%Y%m%d"), ".rds"))
}
.scout_cache_read <- function(key) {
  p <- .scout_cache_path(key)
  if (file.exists(p)) tryCatch(readRDS(p), error = function(e) NULL) else NULL
}
.scout_cache_write <- function(key, df) {
  tryCatch({
    saveRDS(df, .scout_cache_path(key))
    # sweep prior-day files so the cache dir doesn't grow without bound
    old <- list.files(.scout_cache_dir(), full.names = TRUE)
    old <- old[file.mtime(old) < Sys.time() - 7 * 86400]
    if (length(old) > 0) unlink(old)
  }, error = function(e) NULL)
  invisible()
}

.scout_elapsed <- function(t0)
  sprintf("%.1fs", as.numeric(difftime(Sys.time(), t0, units = "secs")))

.ncaa_pull_rows <- function(ds, raws) {
  .arrow_collect(ds, function(q) dplyr::filter(q, Pitcher %in% raws),
                 context = "NCAA pull")
}

# ---- FAST loader: parquet sources only, no TruMedia, no proModel ----------
# Enough for every chart (movement, arm slot, locations, indicators) in a
# few seconds. Plus scores are NA until the full loader upgrades the frame;
# the server bumps a reactive nonce when that lands so outputs re-render.
load_ncaa_pitcher_fast <- function(display_name) {
  key <- gsub("[^A-Za-z0-9]", "_", display_name)
  full <- ncaa_pitcher_cache[[key]]
  if (!is.null(full)) return(full)
  cached <- ncaa_pitcher_fast_cache[[key]]
  if (!is.null(cached)) return(cached)
  disk <- .scout_cache_read(key)
  if (!is.null(disk)) {
    cat("  [scout cache] disk hit (full frame):", display_name, "\n")
    ncaa_pitcher_cache[[key]] <- disk
    return(disk)
  }

  t0 <- Sys.time()
  raws <- unique(c(ncaa_raw_lookup[[display_name]], display_name))
  raws <- raws[!is.na(raws) & nzchar(raws)]
  if (length(raws) == 0) return(data.frame())

  frames <- Filter(function(d) !is.null(d) && nrow(d) > 0,
                   list(.ncaa_pull_rows(NCAA25_DS, raws),
                        .ncaa_pull_rows(NCAA26_DS, raws)))
  if (length(frames) == 0) return(data.frame())

  df <- bind_rows(harmonize_types(frames))
  if ("PitchUID" %in% names(df)) df <- df %>% distinct(PitchUID, .keep_all = TRUE)
  df$Pitcher <- display_name
  if ("Batter" %in% names(df)) {
    df$Batter <- ifelse(grepl(",", df$Batter), normalize_lastfirst(df$Batter), df$Batter)
  }
  df <- reclassify_noncoastal_pitches(df, context = paste0(display_name, " (fast)"))
  df <- prep_ncaa_pitcher(df, fast = TRUE)
  ncaa_pitcher_fast_cache[[key]] <- df
  cat("  [scout timing] fast frame ready in", .scout_elapsed(t0),
      "-", nrow(df), "pitches\n")
  df
}

load_ncaa_pitcher_data <- function(display_name) {
  key <- gsub("[^A-Za-z0-9]", "_", display_name)
  cached <- ncaa_pitcher_cache[[key]]
  if (!is.null(cached)) return(cached)
  disk <- .scout_cache_read(key)
  if (!is.null(disk)) {
    cat("  [scout cache] disk hit (full frame):", display_name, "\n")
    ncaa_pitcher_cache[[key]] <- disk
    return(disk)
  }
  t_all <- Sys.time()

  raws <- unique(c(ncaa_raw_lookup[[display_name]], display_name))
  raws <- raws[!is.na(raws) & nzchar(raws)]
  if (length(raws) == 0) return(data.frame())

  pull_one <- function(ds) .ncaa_pull_rows(ds, raws)

  # 2026 pitch data comes from TruMedia (API is the primary source;
  # the parquet dataset is only a fallback if the API is unreachable).
  # 2025 stays on the parquet dataset unchanged. Coastal pitchers
  # never enter this function — their custom-tagged TrackMan data
  # flows through the coastal pipeline untouched.
  # transfer-aware: search the API on the pitcher's CURRENT (2026)
  # team, not the alphabetical-first of every team he's ever been on
  t_tm <- Sys.time()
  team_disp <- tm_current_team(display_name)
  tm26 <- tryCatch(tm_load_pitcher_pitches(display_name, team_disp),
                   error = function(e) {
                     cat("TruMedia 2026 load failed:", conditionMessage(e), "\n")
                     NULL
                   })
  cat("  [scout timing] TruMedia 2026 pull:", .scout_elapsed(t_tm), "\n")
  src26 <- if (!is.null(tm26) && nrow(tm26) > 0) {
    cat("  [2026 source] TruMedia API:", nrow(tm26), "pitches\n")
    tm26
  } else {
    cat("  [2026 source] TruMedia unavailable (",
        .tm_env$last_error %||% "no detail", ") - parquet fallback\n")
    pull_one(NCAA26_DS)
  }

  t_pq <- Sys.time()
  frames <- Filter(function(d) !is.null(d) && nrow(d) > 0,
                   list(pull_one(NCAA25_DS), src26))
  cat("  [scout timing] parquet pulls:", .scout_elapsed(t_pq), "\n")
  if (length(frames) == 0) return(data.frame())

  df <- bind_rows(harmonize_types(frames))
  if ("PitchUID" %in% names(df)) df <- df %>% distinct(PitchUID, .keep_all = TRUE)

  # unify naming so Pitcher == the selected display name everywhere
  df$Pitcher <- display_name
  if ("Batter" %in% names(df)) {
    df$Batter <- ifelse(grepl(",", df$Batter), normalize_lastfirst(df$Batter), df$Batter)
  }

  # Non-Coastal arm: reclassify pitch types from geometry (partition before
  # naming) and flag spin errors BEFORE prep, so every tab downstream —
  # Overview/Viz/Trends, Scouting + PDF, workbook, Advance projections —
  # reads the corrected TaggedPitchType. Original tag kept in
  # OriginalPitchType; disagreements flagged in PitchTypeReclassified /
  # PitchFamilyChanged / SpinErrorFlag.
  t_rc <- Sys.time()
  df <- reclassify_noncoastal_pitches(df, context = display_name)
  cat("  [scout timing] reclassify:", .scout_elapsed(t_rc), "\n")

  t_pr <- Sys.time()
  df <- prep_ncaa_pitcher(df)
  cat("  [scout timing] prep + proModel:", .scout_elapsed(t_pr), "\n")
  ncaa_pitcher_cache[[key]] <- df
  .scout_cache_write(key, df)
  cat("  [scout timing] TOTAL full load:", .scout_elapsed(t_all),
      "-", nrow(df), "pitches for", display_name, "\n")
  df
}

# Scouting PDF works in feet — convert an app-convention (inches) frame back.
scout_to_feet <- function(df) {
  if (!all(c("PlateLocSide", "PlateLocHeight") %in% names(df))) return(df)
  med_h <- suppressWarnings(median(abs(df$PlateLocHeight), na.rm = TRUE))
  if (is.finite(med_h) && med_h > 8) {
    df$PlateLocSide   <- df$PlateLocSide / 12
    df$PlateLocHeight <- df$PlateLocHeight / 12
  }
  df
}

# ============================================================
