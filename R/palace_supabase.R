# =============================================================================
# R/palace_supabase.R — Palace's read-only connection to Supabase.
#
# Serves the Bullpens tab today; every future Supabase-backed tab reads
# through this same pool. Column aliases return Palace's existing names
# (Pitcher, Date, PlayID, session_id, ...) so downstream code is untouched.
#
# Env vars (Connect Cloud Variables panel; R/palace_secrets.R locally):
#   SB_DB_HOST   e.g. aws-0-us-east-1.pooler.supabase.com
#   SB_DB_USER   e.g. bullpen_reader.ryqzkosdbksawrcrdqrc   (role.projectref)
#   SB_DB_PASS   the bullpen_reader password
#   SB_DB_PORT   optional, default 5432 (session pooler)
# =============================================================================

library(pool)
library(RPostgres)
library(DBI)

.palace_pool_env <- new.env(parent = emptyenv())

palace_pool <- function() {
  p <- .palace_pool_env$p
  if (!is.null(p) && pool::dbIsValid(p)) return(p)
  host <- Sys.getenv("SB_DB_HOST"); user <- Sys.getenv("SB_DB_USER")
  pass <- Sys.getenv("SB_DB_PASS")
  if (!nzchar(host) || !nzchar(user) || !nzchar(pass))
    stop("Supabase credentials missing: set SB_DB_HOST, SB_DB_USER, SB_DB_PASS")
  # Session pooler (5432) by default: the transaction pooler (6543) cuts off
  # wide multi-thousand-row result sets from pitchprofiler.pitches.
  port <- suppressWarnings(as.integer(Sys.getenv("SB_DB_PORT", "5432")))
  if (is.na(port)) port <- 5432L
  # the pooler intermittently refuses a brand-new connection; try a few times
  for (attempt in 1:4) {
    p <- tryCatch(pool::dbPool(
      RPostgres::Postgres(),
      host = host, port = port, dbname = "postgres",
      user = user, password = pass, sslmode = "require",
      minSize = 1, maxSize = 4,
      idleTimeout = 120, validationInterval = 10
    ), error = function(e) e)
    if (!inherits(p, "error")) { .palace_pool_env$p <- p; return(p) }
    cat("[supabase] connect failed (attempt", attempt, ") -", substr(conditionMessage(p), 1, 80), "\n")
    Sys.sleep(3 * attempt)
  }
  stop("Supabase: could not connect after 4 attempts: ", conditionMessage(p))
}

# A plain (non-pooled) connection with the same settings as the pool.
.pp_connect <- function() {
  port <- suppressWarnings(as.integer(Sys.getenv("SB_DB_PORT", "5432")))
  if (is.na(port)) port <- 5432L
  DBI::dbConnect(RPostgres::Postgres(),
                 host = Sys.getenv("SB_DB_HOST"), port = port, dbname = "postgres",
                 user = Sys.getenv("SB_DB_USER"), password = Sys.getenv("SB_DB_PASS"),
                 sslmode = "require", connect_timeout = 10)
}

# The Supabase pooler silently kills sessions it considers idle; a pooled
# connection can look valid and then die on its next fetch. Readers call this
# before retrying so the retry runs on a brand-new connection.
palace_pool_reset <- function() {
  p <- .palace_pool_env$p
  if (!is.null(p)) try(pool::poolClose(p), silent = TRUE)
  .palace_pool_env$p <- NULL
  invisible()
}

# All practice pitches, in Palace's column vocabulary.
sb_load_practice_pitches <- function() {
  sql <- '
    select
      play_id                                  as "PlayID",
      session_id                               as "session_id",
      pitcher                                  as "Pitcher",
      pitcher_throws                           as "PitcherThrows",
      session_date                             as "Date",
      pitch_no                                 as "PitchNo",
      time::text                               as "Time",
      tagged_pitch_type                        as "TaggedPitchType",
      rel_speed                                as "RelSpeed",
      spin_rate                                as "SpinRate",
      spin_axis                                as "SpinAxis",
      tilt                                     as "Tilt",
      induced_vert_break                       as "InducedVertBreak",
      horz_break                               as "HorzBreak",
      vert_appr_angle                          as "VertApprAngle",
      rel_height                               as "RelHeight",
      rel_side                                 as "RelSide",
      extension                                as "Extension",
      plate_loc_side                           as "PlateLocSide",
      plate_loc_height                         as "PlateLocHeight",
      spin_efficiency                          as "SpinAxis3dSpinEfficiency",
      (plate_loc_side  between -0.8333 and 0.8333 and
       plate_loc_height between  1.5    and 3.5)  as "in_zone",
      pitch_uid,
      coalesce(has_edger, false)               as "has_edger",
      edger_blob,
      array_to_string(edger_all_blobs, \'|\')  as "edger_all_blobs",
      framerate,
      coalesce(has_awre, false)                as "has_awre",
      awre_ppdk
    from pitches
    order by session_date, pitch_no
  '
  d <- DBI::dbGetQuery(palace_pool(), sql)
  if (nrow(d) == 0) return(NULL)
  d$Date <- as.Date(d$Date)
  # HH:MM:SS for display and for the constructed-clip fallback, which
  # expects a local wall-clock time string exactly as TrackMan reports it.
  d$Time <- ifelse(nchar(d$Time) >= 19, substr(d$Time, 12, 19), d$Time)
  d
}

# Freshness probe for reactivePoll — changes whenever ingest writes.
sb_practice_version <- function() {
  tryCatch(
    DBI::dbGetQuery(palace_pool(),
      "select coalesce(max(updated_at)::text,'') || '-' || count(*)::text as v
       from pitches")$v,
    error = function(e) NA_character_
  )
}

# =============================================================================
# PITCH PROFILER TABLES — schema `pitchprofiler`, built by
# scripts/process_master.py + scripts/upload_supabase.R from the master
# TrackMan file. Every 2026 NCAA pitch, already:
#   * reclassified (partition-before-naming; Coastal custom tags kept),
#   * scored per pitch (stuff_xrv / pitching_xrv / location_xrv + plus),
#   * decomposed for expected stats (xba_bbe / xtb_bbe / xwobacon_bbe).
# Readers return TrackMan-CSV column names so the existing NCAA loaders,
# prep and every tab read them unchanged. Every reader returns NULL when
# Supabase or the tables are unavailable, and callers fall back to the
# TruMedia / parquet paths they used before.
# =============================================================================
.pp_sb <- new.env(parent = emptyenv())

# TRUE is remembered for the session; a failure is only remembered for 30s so
# a transient pooler hiccup never disables Supabase for the app's lifetime.
pp_sb_available <- function() {
  if (isTRUE(.pp_sb$ok)) return(TRUE)
  if (!is.null(.pp_sb$failed_at) && as.numeric(difftime(Sys.time(), .pp_sb$failed_at, units = "secs")) < 30)
    return(FALSE)
  ok <- FALSE
  for (attempt in 1:3) {
    ok <- tryCatch({
      n <- DBI::dbGetQuery(palace_pool(),
        "select count(*) as n from information_schema.tables
          where table_schema = 'pitchprofiler' and table_name = 'pitches'")$n
      as.numeric(n) > 0
    }, error = function(e) {
      cat("[pitchprofiler] Supabase check failed (attempt", attempt, ") -", substr(conditionMessage(e), 1, 80), "\n")
      palace_pool_reset()
      NA
    })
    if (!is.na(ok)) break
    Sys.sleep(2 * attempt)
  }
  if (isTRUE(ok)) {
    .pp_sb$ok <- TRUE
    cat("[pitchprofiler] Supabase pitchprofiler.pitches available\n")
    return(TRUE)
  }
  .pp_sb$failed_at <- Sys.time()
  FALSE
}

# pitchprofiler.pitches column -> TrackMan CSV name the app reads
.PP_COLMAP <- c(
  pitch_uid = "PitchUID", session_id = "GameUID", game_id = "GameID", play_id = "PlayID",
  game_date = "Date", local_date_time = "LocalDateTime", utc_date_time = "UTCDateTime",
  session_type = "SessionType", pitch_no = "PitchNo", pa_of_inning = "PAofInning",
  pitch_of_pa = "PitchofPA", inning = "Inning", top_bottom = "Top.Bottom",
  outs_raw = "Outs", balls_raw = "Balls", strikes_raw = "Strikes",
  pitcher_name = "Pitcher", pitcher_id = "PitcherId", pitcher_throws = "PitcherThrows",
  pitcher_team = "PitcherTeam", batter_name = "Batter", batter_id = "BatterId",
  batter_side = "BatterSide", batter_team = "BatterTeam", catcher_name = "Catcher",
  catcher_id = "CatcherId", catcher_throws = "CatcherThrows", catcher_team = "CatcherTeam",
  pitch_type = "TaggedPitchType", pitch_type_tagged = "OriginalPitchType",
  pitch_type_model = "ModelPitchType", pitch_type_source = "PitchTypeSource",
  reclassified = "PitchTypeReclassified", family_changed = "PitchFamilyChanged",
  spin_error_flag = "SpinErrorFlag", auto_pitch_type = "AutoPitchType",
  pitch_call = "PitchCall", kor_bb = "KorBB", play_result = "PlayResult",
  tagged_hit_type = "TaggedHitType", auto_hit_type = "AutoHitType",
  outs_on_play = "OutsOnPlay", runs_scored = "RunsScored",
  rel_speed = "RelSpeed", vert_rel_angle = "VertRelAngle", horz_rel_angle = "HorzRelAngle",
  spin_rate = "SpinRate", spin_axis = "SpinAxis", tilt = "Tilt", rel_height = "RelHeight",
  rel_side = "RelSide", extension = "Extension", vert_break = "VertBreak",
  induced_vert_break = "InducedVertBreak", horz_break = "HorzBreak",
  plate_loc_height = "PlateLocHeight", plate_loc_side = "PlateLocSide",
  zone_speed = "ZoneSpeed", vert_appr_angle = "VertApprAngle", horz_appr_angle = "HorzApprAngle",
  zone_time = "ZoneTime", exit_speed = "ExitSpeed", launch_angle = "Angle",
  launch_direction = "Direction", hit_spin_rate = "HitSpinRate", hit_distance = "Distance",
  hit_last_tracked_distance = "LastTrackedDistance", hit_bearing = "Bearing",
  hit_hang_time = "HangTime", pfxx = "pfxx", pfxz = "pfxz", x0 = "x0", y0 = "y0", z0 = "z0",
  vx0 = "vx0", vy0 = "vy0", vz0 = "vz0", ax0 = "ax0", ay0 = "ay0", az0 = "az0",
  home_team = "HomeTeam", away_team = "AwayTeam", stadium = "Stadium", level = "Level",
  league = "League", effective_velo = "EffectiveVelo", hit_max_height = "MaxHeight",
  speed_drop = "SpeedDrop", contact_position_x = "ContactPositionX",
  contact_position_y = "ContactPositionY", contact_position_z = "ContactPositionZ",
  stuff_xrv = "stuff_xrv", pitching_xrv = "pitching_xrv", location_xrv = "location_xrv",
  stuff_plus = "stuff_plus", pitching_plus = "pitching_plus", location_plus = "location_plus",
  valid = "pp_valid", invalid_reason = "pp_invalid_reason",
  is_bbe = "is_bbe", bbe_tracked = "bbe_tracked",
  xba_bbe = "xba_bbe", xtb_bbe = "xtb_bbe", xwobacon_bbe = "xwobacon_bbe"
)

# TrackMan column names -> the pitchprofiler.pitches columns to select, so a
# caller can ask for just what it needs (wide pulls are what the pooler drops).
pp_cols_for <- function(trackman_names) {
  names(.PP_COLMAP)[.PP_COLMAP %in% trackman_names]
}

.pp_quote_in <- function(x) {
  x <- unique(as.character(x)); x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(NULL)
  paste(DBI::dbQuoteString(DBI::ANSI(), x), collapse = ", ")
}

.pp_select_sql <- function(cols) {
  cols <- intersect(cols, names(.PP_COLMAP))
  paste(sprintf('%s as "%s"', cols, .PP_COLMAP[cols]), collapse = ",\n    ")
}

# Run one query against pitchprofiler.pitches and return an app-shaped frame
# (or NULL). Rows are fetched in keyset pages on pitch_uid (the primary key):
# the Supabase pooler cuts off any single result set beyond a few tens of MB,
# so a 600k-row reference pool arrives as ~25 pages of 25k rows, each retried
# on a fresh checkout if the pooler drops it. The frame is marked as already
# reclassified + scored so reclassify_noncoastal_pitches() and score_promodel()
# leave it alone.
PP_PAGE_ROWS <- 10000L

pp_query_pitches <- function(where, cols = names(.PP_COLMAP), context = "pitchprofiler",
                             page = PP_PAGE_ROWS, strict = FALSE, attempts = 3L) {
  if (!pp_sb_available()) return(NULL)
  cols <- union(cols, "pitch_uid")
  sel <- .pp_select_sql(cols)
  t0 <- Sys.time()
  # One dedicated connection PER PAGE. The Supabase pooler kills a session
  # right after it has served one large result set (the next fetch on the
  # same connection fails instantly), so pooled connections cannot be reused
  # for these pulls. A fresh connect costs ~0.3s; a page takes ~1.5s.
  fetch <- function(sql) {
    for (attempt in seq_len(attempts)) {
      d <- tryCatch({
        con <- .pp_connect()
        on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
        DBI::dbGetQuery(con, sql)
      }, error = function(e) e)
      if (!inherits(d, "error")) return(d)
      cat("[pitchprofiler]", context, "page failed (attempt", attempt, ") -",
          substr(conditionMessage(d), 1, 80), "\n")
      Sys.sleep(2 * attempt)
    }
    NULL
  }
  pages <- list(); last <- ""; n_pages <- 0L
  repeat {
    sql <- sprintf("select\n    %s\n  from pitchprofiler.pitches\n  where (%s) and pitch_uid > %s\n  order by pitch_uid\n  limit %d",
                   sel, where, DBI::dbQuoteString(DBI::ANSI(), last), as.integer(page))
    d <- fetch(sql)
    if (is.null(d)) {
      # strict callers (reference pools) must never see a partial frame
      if (strict) { cat("[pitchprofiler]", context, "incomplete after", n_pages, "page(s) - giving up\n"); return(NULL) }
      if (n_pages == 0L) return(NULL)
      cat("[pitchprofiler]", context, "returning", n_pages, "page(s), the rest could not be fetched\n")
      break
    }
    if (nrow(d) == 0) break
    n_pages <- n_pages + 1L
    pages[[n_pages]] <- d
    if (nrow(d) < page) break
    last <- max(d$PitchUID)
  }
  if (n_pages == 0L) return(NULL)
  d <- if (n_pages == 1L) pages[[1]] else dplyr::bind_rows(pages)
  for (nm in names(d)) {
    x <- d[[nm]]
    if (inherits(x, "integer64")) d[[nm]] <- as.numeric(x)
    else if (is.integer(x)) d[[nm]] <- as.numeric(x)
  }
  if ("Date" %in% names(d)) d$Date <- as.Date(d$Date)
  d$Notes <- NA_character_
  d$src <- "2026"
  cat(sprintf("  [pitchprofiler] %s: %d pitches in %d page(s), %.1fs\n", context, nrow(d), n_pages,
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  d
}

# Every pitch thrown by these raw TrackMan pitcher names ("Last, First").
pp_pitcher_rows <- function(raw_names) {
  q <- .pp_quote_in(raw_names)
  if (is.null(q)) return(NULL)
  pp_query_pitches(sprintf("pitcher_name in (%s)", q), context = "pitcher pull")
}

# Every pitch these hitters faced.
pp_batter_rows <- function(raw_names) {
  q <- .pp_quote_in(raw_names)
  if (is.null(q)) return(NULL)
  pp_query_pitches(sprintf("batter_name in (%s)", q), context = "batter pull")
}

# Both halves of every game a team played (its pitchers AND its batters).
pp_team_rows <- function(team_codes, cols = names(.PP_COLMAP)) {
  q <- .pp_quote_in(team_codes)
  if (is.null(q)) return(NULL)
  pp_query_pitches(sprintf("(pitcher_team in (%s) or batter_team in (%s))", q, q),
                   cols = cols, context = "team pull")
}

# Pitcher/team directory for the global search (one row per pitcher-team).
pp_directory <- function() {
  if (!pp_sb_available()) return(NULL)
  d <- tryCatch(DBI::dbGetQuery(palace_pool(),
    "select distinct pitcher_name as \"Pitcher\", pitcher_team as \"PitcherTeam\"
       from pitchprofiler.pitcher_season
      where pitcher_name is not null and pitcher_name <> ''"),
    error = function(e) { cat("[pitchprofiler] directory failed -", conditionMessage(e), "\n"); NULL })
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d$src <- "2026"
  d
}

# Aggregate tables (pitcher_season, pitcher_season_pitch_type, pitcher_game,
# pitcher_game_pitch_type) for leaderboards and team reports.
pp_table <- function(name, where = NULL) {
  if (!pp_sb_available()) return(NULL)
  name <- match.arg(name, c("pitcher_season", "pitcher_season_pitch_type",
                            "pitcher_game", "pitcher_game_pitch_type"))
  sql <- sprintf("select * from pitchprofiler.%s%s", name,
                 if (is.null(where)) "" else paste0(" where ", where))
  d <- tryCatch(DBI::dbGetQuery(palace_pool(), sql), error = function(e) {
    cat("[pitchprofiler]", name, "failed -", conditionMessage(e), "\n"); NULL })
  if (is.null(d)) return(NULL)
  for (nm in names(d)) if (inherits(d[[nm]], "integer64")) d[[nm]] <- as.numeric(d[[nm]])
  d
}

# Bullpen pitcher names as ingested ("Last, First", occasionally with stray
# whitespace or a missing space after the comma). R/04_players_roster.R calls
# this before flipping them to "First Last" for the global search.
bp_fix_names <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\\s+", " ", x)
  x <- sub("^([^,]+),\\s*(.+)$", "\\1, \\2", x)
  x[!is.na(x) & nzchar(x)]
}

# ---- pools for R/07_pools.R -------------------------------------------------
# Columns a processed pool needs (everything process_pitcher_data, the ecdf
# maps, the matchup matrix and the hitter models read); leaves out the wide
# catcher-throw / contact-position / 9P columns nobody uses.
PP_POOL_COLS <- c(
  "Pitcher", "PitcherId", "PitcherThrows", "PitcherTeam", "Batter", "BatterId",
  "BatterSide", "BatterTeam", "HomeTeam", "AwayTeam", "Date", "GameID", "PitchUID",
  "PitchNo", "Inning", "PAofInning", "PitchofPA", "Outs", "Balls", "Strikes",
  "OutsOnPlay", "RunsScored", "Tilt", "ZoneSpeed", "EffectiveVelo", "vx0", "ax0", "az0",
  "TaggedPitchType", "OriginalPitchType", "PitchTypeSource",
  "PitchCall", "KorBB", "PlayResult", "TaggedHitType",
  "RelSpeed", "VertRelAngle", "HorzRelAngle", "SpinRate", "SpinAxis", "RelHeight",
  "RelSide", "Extension", "InducedVertBreak", "HorzBreak", "PlateLocHeight",
  "PlateLocSide", "VertApprAngle", "HorzApprAngle", "ExitSpeed", "Angle", "Direction",
  "Distance", "Bearing", "HangTime", "vy0", "ay0", "y0", "Stadium", "Level", "League",
  "stuff_xrv", "pitching_xrv", "location_xrv", "stuff_plus", "pitching_plus",
  "location_plus", "is_bbe", "bbe_tracked", "xba_bbe", "xtb_bbe", "xwobacon_bbe"
)

# Coastal's 2026 games (Standard sessions; bullpens live in public.pitches).
pp_coastal_games <- function() {
  pp_query_pitches("pitcher_team = 'COA_CHA' and session_type = 'Standard'",
                   context = "Coastal 2026 games")
}

# Every pitch in the named leagues (the 2026 reference pools). One league at
# a time, strict (complete or NULL), small pages, many retries: a reference
# pool that is silently missing half its rows would skew every percentile.
pp_league_pool <- function(leagues) {
  parts <- list()
  for (lg in leagues) {
    d <- pp_query_pitches(sprintf("league = %s and session_type = 'Standard'", DBI::dbQuoteString(DBI::ANSI(), lg)),
                          cols = pp_cols_for(PP_POOL_COLS), context = paste("league pool", lg),
                          page = 5000L, strict = TRUE, attempts = 8L)
    if (is.null(d)) return(NULL)
    parts[[lg]] <- d
  }
  if (length(parts) == 0) return(NULL)
  if (length(parts) == 1) parts[[1]] else dplyr::bind_rows(parts)
}

# Batter/team directory for the hitter search (one row per batter-team).
pp_batter_directory <- function() {
  if (!pp_sb_available()) return(NULL)
  d <- tryCatch(DBI::dbGetQuery(palace_pool(),
    "select distinct batter_name as \"Batter\", batter_team as \"BatterTeam\"
       from pitchprofiler.pitches
      where batter_name is not null and batter_name <> ''"),
    error = function(e) { cat("[pitchprofiler] batter directory failed -", conditionMessage(e), "\n"); NULL })
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d$src <- "2026"
  d
}

# Build stamp of the scored tables (changes when process_master.py reloads
# them); used to key on-disk caches of the processed reference pools.
pp_built_at <- function() {
  if (!pp_sb_available()) return(NA_character_)
  tryCatch(as.character(DBI::dbGetQuery(palace_pool(),
    "select max(built_at) as b from pitchprofiler.pitcher_season")$b[1]),
    error = function(e) NA_character_)
}

# ---- Supabase Storage (bucket `palace-serving`) -----------------------------
# Large precomputed objects (the processed P5 / Sun Belt reference pools) are
# shipped as parquet files through Storage over HTTPS: the Postgres pooler
# throttles and drops multi-hundred-thousand-row result sets, a file download
# does not. Built by scripts/build_reference_pools.R after each reload.
#   SUPABASE_URL          https://<projectref>.supabase.co
#   SUPABASE_SECRET_KEY   a secret (service) API key; the bucket is private
SB_STORAGE_BUCKET <- "palace-serving"

sb_storage_enabled <- function() {
  nzchar(Sys.getenv("SUPABASE_URL")) && nzchar(Sys.getenv("SUPABASE_SECRET_KEY"))
}

.sb_storage_req <- function(path) {
  key <- Sys.getenv("SUPABASE_SECRET_KEY")
  httr2::request(sprintf("%s/storage/v1/object/%s/%s",
                         sub("/+$", "", Sys.getenv("SUPABASE_URL")), SB_STORAGE_BUCKET, path)) |>
    httr2::req_headers(apikey = key, Authorization = paste("Bearer", key)) |>
    httr2::req_timeout(600)
}

# Download one object to `dest`; TRUE on success. Missing objects are not an
# error (callers fall back), other failures are logged.
sb_storage_download <- function(path, dest) {
  if (!sb_storage_enabled()) return(FALSE)
  resp <- tryCatch(.sb_storage_req(path) |> httr2::req_error(is_error = function(r) FALSE) |>
                     httr2::req_perform(path = dest), error = function(e) e)
  if (inherits(resp, "error")) {
    cat("[storage] download failed", path, "-", conditionMessage(resp), "\n"); return(FALSE)
  }
  ok <- httr2::resp_status(resp) == 200
  if (!ok) {
    cat("[storage]", path, "not available (HTTP", httr2::resp_status(resp), ")\n")
    if (file.exists(dest)) unlink(dest)
  }
  ok
}

# Upload a local file (upsert). Used by the build scripts, never by the app.
sb_storage_upload <- function(local, path, content_type = "application/octet-stream") {
  if (!sb_storage_enabled()) stop("set SUPABASE_URL and SUPABASE_SECRET_KEY")
  resp <- .sb_storage_req(path) |>
    httr2::req_method("POST") |>
    httr2::req_headers(`x-upsert` = "true", `Content-Type` = content_type) |>
    httr2::req_body_file(local) |>
    httr2::req_perform()
  invisible(httr2::resp_status(resp) < 300)
}
