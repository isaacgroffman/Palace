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
# =============================================================================

library(pool)
library(RPostgres)
library(DBI)

palace_pool <- local({
  p <- NULL
  function() {
    if (!is.null(p) && pool::dbIsValid(p)) return(p)
    host <- Sys.getenv("SB_DB_HOST"); user <- Sys.getenv("SB_DB_USER")
    pass <- Sys.getenv("SB_DB_PASS")
    if (!nzchar(host) || !nzchar(user) || !nzchar(pass))
      stop("Supabase credentials missing: set SB_DB_HOST, SB_DB_USER, SB_DB_PASS")
    p <<- pool::dbPool(
      RPostgres::Postgres(),
      host = host, port = 6543, dbname = "postgres",
      user = user, password = pass, sslmode = "require",
      minSize = 1, maxSize = 4
    )
    p
  }
})

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

pp_sb_available <- function() {
  if (!is.null(.pp_sb$ok)) return(.pp_sb$ok)
  .pp_sb$ok <- tryCatch({
    n <- DBI::dbGetQuery(palace_pool(),
      "select count(*) as n from information_schema.tables
        where table_schema = 'pitchprofiler' and table_name = 'pitches'")$n
    as.numeric(n) > 0
  }, error = function(e) {
    cat("[pitchprofiler] Supabase tables unavailable -", conditionMessage(e), "\n")
    FALSE
  })
  if (isTRUE(.pp_sb$ok)) cat("[pitchprofiler] Supabase pitchprofiler.pitches available\n")
  .pp_sb$ok
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
# (or NULL). The frame is marked as already reclassified + scored so
# reclassify_noncoastal_pitches() and score_promodel() leave it alone.
pp_query_pitches <- function(where, cols = names(.PP_COLMAP), context = "pitchprofiler") {
  if (!pp_sb_available()) return(NULL)
  sql <- sprintf("select\n    %s\n  from pitchprofiler.pitches\n  where %s", .pp_select_sql(cols), where)
  t0 <- Sys.time()
  # the Supabase pooler occasionally drops an idle session: one retry on a
  # fresh checkout before giving up (callers then fall back to TruMedia/parquet)
  run <- function() DBI::dbGetQuery(palace_pool(), sql)
  d <- tryCatch(run(), error = function(e) {
    cat("[pitchprofiler]", context, "query failed once -", conditionMessage(e), "- retrying\n")
    Sys.sleep(1)
    tryCatch(run(), error = function(e2) {
      cat("[pitchprofiler]", context, "query failed -", conditionMessage(e2), "\n")
      NULL
    })
  })
  if (is.null(d) || nrow(d) == 0) return(NULL)
  for (nm in names(d)) {
    x <- d[[nm]]
    if (inherits(x, "integer64")) d[[nm]] <- as.numeric(x)
    else if (is.integer(x)) d[[nm]] <- as.numeric(x)
  }
  if ("Date" %in% names(d)) d$Date <- as.Date(d$Date)
  d$Notes <- NA_character_
  d$src <- "2026"
  cat(sprintf("  [pitchprofiler] %s: %d pitches in %.1fs\n", context, nrow(d),
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
