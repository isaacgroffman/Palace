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
      user = user, password = pass, sslmode = "require", connect_timeout = 10,
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

# Run a small query with retries; the pool is rebuilt between attempts because
# the pooler kills sessions silently and a dead one can still look valid.
.pp_with_retry <- function(what, fn, attempts = 4L) {
  for (attempt in seq_len(attempts)) {
    out <- tryCatch(fn(), error = function(e) e)
    if (!inherits(out, "error")) return(out)
    cat("[pitchprofiler]", what, "failed (attempt", attempt, ") -", substr(conditionMessage(out), 1, 80), "\n")
    palace_pool_reset()
    Sys.sleep(2 * attempt)
  }
  NULL
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
  d <- .pp_with_retry("directory", function() DBI::dbGetQuery(palace_pool(),
    "select distinct pitcher_name as \"Pitcher\", pitcher_team as \"PitcherTeam\",
            pitcher_id as \"PitcherId\"
       from pitchprofiler.pitcher_season
      where pitcher_name is not null and pitcher_name <> ''"))
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
  d <- .pp_with_retry(name, function() DBI::dbGetQuery(palace_pool(), sql))
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

# ---- Bullpen pitches (public.pitches) ----------------------------------------
# Read through Supabase's REST API (PostgREST) with the service key: it does
# not depend on the Postgres pooler or SB_DB_*, and it is what every other
# tab already uses for Storage. The direct SQL path is the fallback. One
# copy per process, refreshed when the ingest job writes (version probe).
.bp_env <- new.env(parent = emptyenv())

.sb_rest_req <- function(path) {
  key <- Sys.getenv("SUPABASE_SECRET_KEY"); url <- Sys.getenv("SUPABASE_URL")
  if (!nzchar(key) || !nzchar(url)) return(NULL)
  httr2::request(paste0(sub("/+$", "", url), "/rest/v1/", path)) |>
    httr2::req_headers(apikey = key, Authorization = paste("Bearer", key)) |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = 3, retry_on_failure = TRUE) |>
    httr2::req_error(is_error = function(r) FALSE)
}

# every row of a table via REST, paged 1000 at a time
.sb_rest_rows <- function(table, select = "*", order = NULL, page = 1000L) {
  out <- list(); from <- 0L
  repeat {
    rq <- .sb_rest_req(table)
    if (is.null(rq)) return(NULL)
    rq <- rq |> httr2::req_url_query(select = select) |>
      httr2::req_headers(Range = sprintf("%d-%d", from, from + page - 1L), `Range-Unit` = "items")
    if (!is.null(order)) rq <- rq |> httr2::req_url_query(order = order)
    resp <- tryCatch(httr2::req_perform(rq), error = function(e) NULL)
    if (is.null(resp) || httr2::resp_status(resp) >= 300) {
      cat("[supabase] REST", table, "failed:", if (is.null(resp)) "no response" else httr2::resp_status(resp), "\n")
      return(NULL)
    }
    d <- tryCatch(jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(d) || length(d) == 0) break
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    out[[length(out) + 1]] <- d
    if (nrow(d) < page) break
    from <- from + page
  }
  if (!length(out)) return(data.frame())
  dplyr::bind_rows(out)
}

# "<max updated_at>-<row count>": changes whenever the ingest job writes
sb_practice_version <- function() {
  rq <- .sb_rest_req("pitches")
  if (!is.null(rq)) {
    resp <- tryCatch(rq |> httr2::req_url_query(select = "updated_at", order = "updated_at.desc.nullslast", limit = 1) |>
                       httr2::req_headers(Prefer = "count=exact") |> httr2::req_perform(), error = function(e) NULL)
    if (!is.null(resp) && httr2::resp_status(resp) < 300) {
      cr <- httr2::resp_header(resp, "content-range") %||% ""
      n  <- sub("^.*/", "", cr)
      d  <- tryCatch(jsonlite::fromJSON(httr2::resp_body_string(resp)), error = function(e) NULL)
      up <- if (is.data.frame(d) && nrow(d)) as.character(d$updated_at[1]) else ""
      return(paste0(up, "-", n))
    }
  }
  tryCatch(
    DBI::dbGetQuery(palace_pool(),
      "select coalesce(max(updated_at)::text,'') || '-' || count(*)::text as v
       from pitches")$v,
    error = function(e) NA_character_)
}

.BP_COLS <- c(play_id = "PlayID", session_id = "session_id", pitcher = "Pitcher", pitcher_throws = "PitcherThrows",
              session_date = "Date", pitch_no = "PitchNo", time = "Time", tagged_pitch_type = "TaggedPitchType",
              rel_speed = "RelSpeed", spin_rate = "SpinRate", spin_axis = "SpinAxis", tilt = "Tilt",
              induced_vert_break = "InducedVertBreak", horz_break = "HorzBreak", vert_appr_angle = "VertApprAngle",
              rel_height = "RelHeight", rel_side = "RelSide", extension = "Extension",
              plate_loc_side = "PlateLocSide", plate_loc_height = "PlateLocHeight",
              spin_efficiency = "SpinAxis3dSpinEfficiency", pitch_uid = "pitch_uid", has_edger = "has_edger",
              edger_blob = "edger_blob", edger_all_blobs = "edger_all_blobs", framerate = "framerate",
              has_awre = "has_awre", awre_ppdk = "awre_ppdk")

.sb_practice_rest <- function() {
  d <- .sb_rest_rows("pitches", select = paste(names(.BP_COLS), collapse = ","),
                     order = "session_date.asc,pitch_no.asc")
  if (is.null(d) || !nrow(d)) return(d)
  for (nm in names(.BP_COLS)) if (!nm %in% names(d)) d[[nm]] <- NA
  blobs <- d$edger_all_blobs
  d$edger_all_blobs <- vapply(seq_len(nrow(d)), function(i) {
    b <- if (is.list(blobs)) blobs[[i]] else blobs[i]
    b <- b[!is.na(b) & nzchar(as.character(b))]
    if (length(b)) paste(b, collapse = "|") else NA_character_
  }, character(1))
  d <- d[, names(.BP_COLS), drop = FALSE]; names(d) <- unname(.BP_COLS)
  d$has_edger <- d$has_edger %in% TRUE; d$has_awre <- d$has_awre %in% TRUE
  tt <- as.character(d$Time)
  d$Time <- ifelse(is.na(tt), NA_character_, ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}", tt), substr(tt, 12, 19), substr(tt, 1, 8)))
  d
}

.sb_practice_sql <- function() {
  sql <- '
    select
      play_id as "PlayID", session_id as "session_id", pitcher as "Pitcher",
      pitcher_throws as "PitcherThrows", session_date as "Date", pitch_no as "PitchNo",
      time::text as "Time", tagged_pitch_type as "TaggedPitchType", rel_speed as "RelSpeed",
      spin_rate as "SpinRate", spin_axis as "SpinAxis", tilt as "Tilt",
      induced_vert_break as "InducedVertBreak", horz_break as "HorzBreak",
      vert_appr_angle as "VertApprAngle", rel_height as "RelHeight", rel_side as "RelSide",
      extension as "Extension", plate_loc_side as "PlateLocSide", plate_loc_height as "PlateLocHeight",
      spin_efficiency as "SpinAxis3dSpinEfficiency", pitch_uid,
      coalesce(has_edger, false) as "has_edger", edger_blob,
      array_to_string(edger_all_blobs, \'|\') as "edger_all_blobs", framerate,
      coalesce(has_awre, false) as "has_awre", awre_ppdk
    from pitches order by session_date, pitch_no'
  d <- DBI::dbGetQuery(palace_pool(), sql)
  d$Time <- ifelse(nchar(d$Time) >= 19, substr(d$Time, 12, 19), d$Time)
  d
}

# All practice pitches, in Palace's column vocabulary. Cached per process
# and keyed by the version probe, so every session and tab shares one copy.
sb_load_practice_pitches <- function(force = FALSE) {
  ver <- sb_practice_version()
  if (!force && !is.null(.bp_env$data) && identical(.bp_env$version, ver)) return(.bp_env$data)
  t0 <- Sys.time()
  d <- tryCatch(.sb_practice_rest(), error = function(e) { cat("[bullpen] REST load failed:", conditionMessage(e), "\n"); NULL })
  src <- "REST"
  if (is.null(d)) { d <- .sb_practice_sql(); src <- "Postgres" }
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d$Date <- as.Date(d$Date)
  d$in_zone <- !is.na(d$PlateLocSide) & !is.na(d$PlateLocHeight) &
    d$PlateLocSide >= -0.8333 & d$PlateLocSide <= 0.8333 & d$PlateLocHeight >= 1.5 & d$PlateLocHeight <= 3.5
  cat(sprintf("[bullpen] %d practice pitches via %s in %.1fs (version %s)\n", nrow(d), src,
              as.numeric(difftime(Sys.time(), t0, units = "secs")), ver %||% "?"))
  .bp_env$data <- d; .bp_env$version <- ver
  d
}

# The bullpen frame for any part of the app (same object the Bullpens tab
# renders): Pitcher is "Last, First"; Date is a Date; plate locations in feet.
bullpen_pitches <- function() tryCatch(sb_load_practice_pitches(), error = function(e) NULL)

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
                   context = "Coastal 2026 games", page = 2000L, strict = TRUE, attempts = 8L)
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

# Expected stats for the pitchers in a grading pool, from the aggregate
# tables (computed by the pipeline with the bundle's Savant-style rules), so
# the app never runs the Python expected-stats chain over 600k pool pitches.
# Joins on TrackMan pitcher id and returns the pool's own Pitcher spelling.
pp_pool_xstats <- function(pool, by = c("Pitcher", "PitcherPitchType")) {
  by <- match.arg(by)
  if (is.null(pool) || !nrow(pool) || !all(c("Pitcher", "PitcherId") %in% names(pool))) return(NULL)
  ids <- pool[!is.na(pool$PitcherId) & nzchar(as.character(pool$PitcherId)), c("PitcherId", "Pitcher")]
  ids <- ids[!duplicated(ids$PitcherId), , drop = FALSE]
  if (!nrow(ids)) return(NULL)
  tbl <- if (by == "Pitcher") "pitcher_season" else "pitcher_season_pitch_type"
  cols <- if (by == "Pitcher") "pitcher_id, pitches, xba, xslg, xwoba" else "pitcher_id, pitch_type, pitches, xba, xslg, xwoba"
  parts <- list()
  for (chunk in split(as.character(ids$PitcherId), ceiling(seq_len(nrow(ids)) / 400))) {
    q <- .pp_quote_in(chunk)
    d <- tryCatch(DBI::dbGetQuery(palace_pool(), sprintf("select %s from pitchprofiler.%s where pitcher_id in (%s)", cols, tbl, q)),
                  error = function(e) { cat("[pitchprofiler] pool xstats failed -", conditionMessage(e), "\n"); NULL })
    if (is.null(d)) return(NULL)
    parts[[length(parts) + 1]] <- d
  }
  d <- dplyr::bind_rows(parts)
  if (!nrow(d)) return(NULL)
  d$pitcher_id <- as.character(d$pitcher_id)
  # one row per pitcher (the most-pitched season row wins if an id repeats)
  d <- d[order(-d$pitches), , drop = FALSE]
  key <- if (by == "Pitcher") d$pitcher_id else paste(d$pitcher_id, d$pitch_type)
  d <- d[!duplicated(key), , drop = FALSE]
  out <- data.frame(Pitcher = ids$Pitcher[match(d$pitcher_id, as.character(ids$PitcherId))],
                    xBA = d$xba, xSLG = d$xslg, xwOBA = d$xwoba, stringsAsFactors = FALSE)
  if (by != "Pitcher") out$TaggedPitchType <- d$pitch_type
  out[!is.na(out$Pitcher), , drop = FALSE]
}

# Batter/team directory for the hitter search (one row per batter-team).
pp_batter_directory <- function() {
  if (!pp_sb_available()) return(NULL)
  d <- .pp_with_retry("batter directory", function() DBI::dbGetQuery(palace_pool(),
    "select distinct batter_name as \"Batter\", batter_team as \"BatterTeam\",
            batter_id as \"BatterId\"
       from pitchprofiler.pitches
      where batter_name is not null and batter_name <> ''"))
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d$src <- "2026"
  d
}

# Build stamp of the scored tables (changes when process_master.py reloads
# them); used to key on-disk caches of the processed reference pools.
pp_built_at <- function() {
  if (!is.null(.pp_sb$built_at)) return(.pp_sb$built_at)   # one query per process
  if (!pp_sb_available()) return(NA_character_)
  b <- .pp_with_retry("build stamp", function() DBI::dbGetQuery(palace_pool(),
    "select max(built_at) as b from pitchprofiler.pitcher_season")$b[1])
  out <- if (is.null(b)) NA_character_ else as.character(b)
  if (!is.na(out)) .pp_sb$built_at <- out
  out
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
sb_storage_download <- function(path, dest, attempts = 4L) {
  if (!sb_storage_enabled()) return(FALSE)
  for (attempt in seq_len(attempts)) {
    resp <- tryCatch(.sb_storage_req(path) |> httr2::req_error(is_error = function(r) FALSE) |>
                       httr2::req_retry(max_tries = 3, retry_on_failure = TRUE) |>
                       httr2::req_perform(path = dest), error = function(e) e)
    if (inherits(resp, "error")) {
      # transient network error (a 40 MB download over a flaky link): retry
      why <- conditionMessage(resp)
      if (!is.null(resp$parent)) why <- paste(why, "/", conditionMessage(resp$parent))
      cat("[storage] download of", path, "failed (attempt", attempt, ") -", substr(why, 1, 120), "\n")
      if (file.exists(dest)) unlink(dest)
      Sys.sleep(3 * attempt)
      next
    }
    status <- httr2::resp_status(resp)
    if (status == 200 && file.exists(dest) && file.info(dest)$size > 0) return(TRUE)
    if (file.exists(dest)) unlink(dest)
    if (status == 404 || status == 400) {           # object is not there: no point retrying
      cat("[storage]", path, "not available (HTTP", status, ")\n"); return(FALSE)
    }
    cat("[storage]", path, "HTTP", status, "(attempt", attempt, ")\n")
    Sys.sleep(3 * attempt)
  }
  FALSE
}

# Upload a local file (upsert). Used by the build scripts, never by the app.
sb_storage_upload <- function(local, path, content_type = "application/octet-stream") {
  if (!sb_storage_enabled()) stop("set SUPABASE_URL and SUPABASE_SECRET_KEY")
  # a multi-MB upload occasionally has its connection reset mid-body; retry
  # the whole request a few times before giving up
  resp <- NULL
  for (attempt in 1:4) {
    resp <- tryCatch(.sb_storage_req(path) |>
      httr2::req_method("POST") |>
      httr2::req_headers(`x-upsert` = "true", `Content-Type` = content_type) |>
      httr2::req_body_file(local) |>
      httr2::req_timeout(300) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform(), error = function(e) e)
    if (!inherits(resp, "error")) break
    cat(sprintf("[storage] upload of %s attempt %d failed: %s\n", path, attempt, conditionMessage(resp)))
    Sys.sleep(3 * attempt)
  }
  if (inherits(resp, "error")) stop(sprintf("[storage] upload of %s failed: %s", path, conditionMessage(resp)))
  if (httr2::resp_status(resp) >= 300) {
    body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    stop(sprintf("[storage] upload of %s (%.1f MB) failed: HTTP %d %s", path,
                 file.size(local) / 1e6, httr2::resp_status(resp), substr(body, 1, 300)))
  }
  invisible(TRUE)
}
