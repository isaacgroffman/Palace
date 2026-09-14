# =============================================================================
# R/palace_upload.R — the admin "Upload data" feature.
#
# Admins pull data straight from the TrackMan Data API (games, bullpens,
# batting practice) or TruMedia (season stats), review and edit what came
# back, and commit it to Supabase. Every step runs in a background R process
# (callr) so the app stays responsive; progress, logs and results are files
# in the job directory, which the Upload panel polls.
#
#   Fetch   discover sessions for a season window -> pull plays + balls ->
#           canonical frame (games: the master TrackMan v3 schema in
#           reference/trackman_master_schema.csv) -> validation report
#   Review  editable table, bulk find/replace, row / session exclusions,
#           CSV and parquet downloads
#   Commit  games: scripts/process_master.py scores the staged pitches, rows
#                  go to pitchprofiler.pitches + per-game aggregates (season
#                  aggregates refreshed for the affected pitchers), the
#                  Coastal pool in Storage is rebuilt;
#           bullpens: PostgREST upsert into public.pitches / public.sessions
#                  (same rows the practice ingest script writes);
#           batting practice: parquet in Storage practice/hitting/ (+ table
#                  public.batting_practice when the role may create it);
#           trumedia: scripts/build_leaderboards.R in the background.
#   Check   date-by-date completeness: what TrackMan has vs what Supabase has
#   Log     uploads/log.json in Storage: who uploaded what, when, with the
#           raw / staged / scored files kept under uploads/<id>/
#
# This file is function-only (no boot work) and every call is namespaced, so
# the worker process can source it on its own. Secrets come from the
# environment only (TM_CLIENT_ID / TM_CLIENT_SECRET, SUPABASE_URL /
# SUPABASE_SECRET_KEY, SB_DB_*, TM_USERNAME / TM_SITENAME / TM_MASTER_TOKEN).
# =============================================================================

.up_env <- new.env(parent = emptyenv())
.up_env$jobs <- list(); .up_env$version <- 0L

if (!exists("%||%", mode = "function")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

UP_TM_TOKEN_URL <- "https://login.trackmanbaseball.com/connect/token"
UP_TM_API       <- "https://dataapi.trackmanbaseball.com/api/v1"
UP_BUCKET       <- "palace-serving"
UP_LOG_PATH     <- "uploads/log.json"
UP_MASTER_SCHEMA_FILE <- "reference/trackman_master_schema.csv"

# ---- catalog ------------------------------------------------------------------
UPLOAD_TYPES <- list(
  games = list(key = "games", label = "TrackMan games", icon = "⚾",
    blurb = "Game sessions from the TrackMan Data API, scored with Pitch Profiler and written to pitchprofiler.pitches (with per-game aggregates).",
    session_kind = "game", target = "Supabase pitchprofiler.pitches"),
  bullpens = list(key = "bullpens", label = "Bullpens", icon = "\U0001F3AF",
    blurb = "TrackMan practice sessions (Pitching) with Edgertronic clip paths, written to public.pitches: the Bullpens tab source.",
    session_kind = "practice", practice_type = "Pitching", target = "Supabase public.pitches"),
  bp = list(key = "bp", label = "Batting practice", icon = "\U0001F3CF",
    blurb = "Batting practice from both TrackMan sources: game-endpoint BattingPractice sessions (how Coastal's BP is tracked) and practice-endpoint Hitting sessions. Parquet in Storage practice/hitting/ (the Bullpens > Batting Practice pill), mirrored to public.batting_practice once that table exists.",
    session_kind = "practice", practice_type = "Hitting", target = "Storage practice/hitting/ (+ public.batting_practice)"),
  trumedia = list(key = "trumedia", label = "TruMedia season stats", icon = "\U0001F4CA",
    blurb = "Rebuilds the season leaderboard tables (lb/<season>/ in Storage) from TruMedia PlayerTotals through scripts/build_leaderboards.R.",
    session_kind = "trumedia", target = "Storage lb/<season>/")
)
UPLOAD_SCOPES <- c("Coastal Carolina games only" = "ccu", "Sun Belt games" = "sunbelt",
                   "All D1 games" = "d1", "Everything TrackMan returns" = "all")

# Season windows. "Season" here is the calendar block a pull covers: pre-season
# (January), spring, summer ball and fall ball. Defaults are editable in the UI.
up_seasons <- function(years = NULL) {
  yr <- as.integer(format(Sys.Date(), "%Y"))
  if (is.null(years)) years <- (yr - 1):(yr + 1)
  rows <- lapply(years, function(y) data.frame(
    key   = sprintf(c("winter%d", "spring%d", "summer%d", "fall%d"), y),
    label = paste(c("Pre-season", "Spring", "Summer", "Fall"), y),
    from  = as.Date(sprintf(c("%d-01-01", "%d-02-01", "%d-06-01", "%d-09-01"), y)),
    to    = as.Date(sprintf(c("%d-01-31", "%d-06-30", "%d-08-31", "%d-12-15"), y)),
    year  = y, stringsAsFactors = FALSE))
  do.call(rbind, rows)
}
up_season_default <- function() {
  s <- up_seasons(); today <- Sys.Date()
  i <- which(s$from <= today & s$to >= today)
  if (length(i)) s$key[i[1]] else { j <- which(s$to <= today); if (length(j)) s$key[max(j)] else s$key[1] }
}
up_season_row <- function(key) { s <- up_seasons(); s[match(key, s$key), , drop = FALSE] }

# ---- files, logs, progress -------------------------------------------------------
up_root_dir <- function() {
  d <- Sys.getenv("PALACE_UPLOAD_DIR")
  if (!nzchar(d)) d <- if (dir.exists("/data")) "/data/palace_uploads" else path.expand("~/.palace_uploads")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}
up_now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
up_log <- function(job_dir, ...) {
  line <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", paste0(..., collapse = ""))
  cat(line, "\n")
  if (!is.null(job_dir) && dir.exists(job_dir)) cat(line, "\n", file = file.path(job_dir, "log.txt"), append = TRUE, sep = "")
  invisible(line)
}
up_write_json <- function(x, path) {
  tmp <- paste0(path, ".tmp")
  jsonlite::write_json(x, tmp, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE, digits = NA)
  file.rename(tmp, path)
  invisible(path)
}
up_read_json <- function(path, simplify = FALSE) {
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = simplify), error = function(e) NULL)
}
up_progress <- function(job_dir, pct, msg, stage = NULL) {
  if (is.null(job_dir)) return(invisible())
  up_write_json(list(pct = round(100 * min(1, max(0, pct))), msg = msg, stage = stage, at = up_now()),
                file.path(job_dir, "progress.json"))
}

# Plain-English troubleshooting for the errors admins actually hit.
up_hint <- function(msg) {
  m <- tolower(paste(msg, collapse = " "))
  hits <- c(
    "trackman credentials missing|tm_client" = "Set TM_CLIENT_ID and TM_CLIENT_SECRET (Connect Cloud: content Variables, secret; locally: ~/.Renviron), then reopen the panel.",
    "token request failed|invalid_client|unauthorized_client" = "TrackMan rejected the client id / secret. Check TM_CLIENT_ID and TM_CLIENT_SECRET for typos or a rotated secret.",
    "http 401|http 403" = "The API refused the request (401/403). The TrackMan token may have expired mid-run or the client is not entitled to this endpoint; retry, then check the credentials.",
    "http 429|too many requests" = "TrackMan rate-limited the pull. The client waits and retries automatically; if it keeps happening, fetch a shorter date window.",
    "supabase_url|supabase_secret_key|storage is not configured" = "Set SUPABASE_URL and SUPABASE_SECRET_KEY (the secret API key from Project Settings > API).",
    "sb_db_host|sb_db_user|sb_db_pass|credentials missing" = "Set SB_DB_HOST, SB_DB_USER and SB_DB_PASS. Writes need the postgres.<projectref> role, not the read-only bullpen_reader.",
    "password authentication failed" = "Postgres rejected SB_DB_USER / SB_DB_PASS. Copy the pooler user (postgres.<projectref>) and password from Project Settings > Database.",
    "permission denied|must be owner" = "The database role cannot write this table. Use SB_DB_USER=postgres.<projectref> (or grant INSERT/DELETE on pitchprofiler.* to the role).",
    "could not resolve host|could not connect|timed out|timeout|connection reset|ssl" = "Network problem reaching the service. Check the host's outbound access, then retry; the job can be re-run from the same staged data.",
    "no module named|lightgbm|polars|python not found|scorer" = "The Pitch Profiler scorer needs a Python with lightgbm, polars, pandas, pyarrow. Set PALACE_PYTHON (or RETICULATE_PYTHON) to that interpreter; locally ~/.venvs/pitchprofiler/bin/python.",
    "row count drifted|missing for some rows" = "The scorer could not join its outputs back to the pitches (duplicate or empty pitchUIDs). Exclude the flagged rows in Review and commit again.",
    "no sessions|nothing to fetch|0 sessions" = "TrackMan returned no sessions in that window for the chosen scope. Widen the dates, change the scope, or check the Completeness tab for what TrackMan has.",
    "tm_username|tm_sitename|tm_master_token|token_expired_or_not_found|temp token" = "TruMedia credentials are missing or the master token expired. Ask the TruMedia site admin for a new master token and set TM_MASTER_TOKEN.",
    "disk|no space" = "The upload directory is full. Set PALACE_UPLOAD_DIR to a larger volume or clear old jobs from the History tab."
  )
  for (k in names(hits)) if (grepl(k, m)) return(unname(hits[[k]]))
  "Open the job log for the full error, fix the cause, then run the step again; nothing is committed until the commit step reports success."
}

# ---- TrackMan Data API ---------------------------------------------------------------
up_tm_creds <- function() {
  id <- Sys.getenv("TM_CLIENT_ID"); if (!nzchar(id)) id <- "CoastalCarolina-Palace"
  sec <- Sys.getenv("TM_CLIENT_SECRET"); if (!nzchar(sec)) sec <- Sys.getenv("TM_SECRET")
  list(id = id, secret = sec)
}
up_tm_enabled <- function() nzchar(up_tm_creds()$secret)
up_tm_token <- function(force = FALSE) {
  cr <- up_tm_creds()
  if (!nzchar(cr$secret)) stop("TrackMan credentials missing: set TM_CLIENT_ID and TM_CLIENT_SECRET")
  tk <- .up_env$tm_token
  if (!force && !is.null(tk) && tk$expires_at > Sys.time() + 120) return(tk$access_token)
  resp <- httr2::request(UP_TM_TOKEN_URL) |>
    httr2::req_body_form(client_id = cr$id, client_secret = cr$secret, grant_type = "client_credentials") |>
    httr2::req_timeout(30) |> httr2::req_error(is_error = function(r) FALSE) |> httr2::req_perform()
  if (httr2::resp_status(resp) >= 300)
    stop(sprintf("TrackMan token request failed (HTTP %d): %s", httr2::resp_status(resp),
                 substr(tryCatch(httr2::resp_body_string(resp), error = function(e) ""), 1, 200)))
  b <- httr2::resp_body_json(resp)
  .up_env$tm_token <- list(access_token = b$access_token, expires_at = Sys.time() + as.numeric(b$expires_in %||% 3600))
  b$access_token
}
up_tm_req <- function(path) {
  httr2::request(paste0(UP_TM_API, path)) |>
    httr2::req_auth_bearer_token(up_tm_token()) |>
    httr2::req_throttle(capacity = 30, fill_time_s = 60) |>
    httr2::req_retry(max_tries = 5, backoff = function(i) min(30, 2^i), retry_on_failure = TRUE) |>
    httr2::req_timeout(180)
}
up_tm_body <- function(resp) {
  txt <- httr2::resp_body_string(resp)
  if (!nzchar(txt) || trimws(txt) %in% c("[]", "{}", "null")) return(NULL)
  txt
}
# flattened data.frame (dotted names, the master file convention)
up_tm_get <- function(path) {
  txt <- up_tm_body(up_tm_req(path) |> httr2::req_perform())
  if (is.null(txt)) return(NULL)
  jsonlite::fromJSON(txt, flatten = TRUE)
}
# raw nested list (the practice flatteners want the tree)
up_tm_get_raw <- function(path) {
  txt <- up_tm_body(up_tm_req(path) |> httr2::req_perform())
  if (is.null(txt)) return(list())
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}
up_tm_post <- function(path, body) {
  txt <- up_tm_body(up_tm_req(path) |> httr2::req_headers(accept = "text/plain") |>
                      httr2::req_body_json(body, type = "application/json-patch+json") |> httr2::req_perform())
  if (is.null(txt)) return(NULL)
  jsonlite::fromJSON(txt, flatten = TRUE)
}

up_session_date <- function(local, utc) {
  local <- as.character(local); utc <- as.character(utc)
  pick <- ifelse(!is.na(local) & nzchar(local), local, utc)
  out <- as.Date(rep(NA_integer_, length(pick)))
  for (i in which(!is.na(pick) & nzchar(pick))) {
    s <- pick[i]
    d <- if (grepl("^\\d{4}-\\d{2}-\\d{2}", s)) as.Date(substr(s, 1, 10)) else
      as.Date(substr(s, 1, 10), tryFormats = c("%m/%d/%Y", "%d/%m/%Y"), optional = TRUE)
    if (!is.na(d)) out[i] <- d
  }
  out
}
up_date_chunks <- function(from, to, days) {
  starts <- seq(as.Date(from), as.Date(to), by = paste(days, "days"))
  lapply(starts, function(s) list(from = s, to = min(s + days - 1, as.Date(to))))
}
# bind discovery frames whose column types disagree between chunks
up_bind_chr <- function(frames) {
  frames <- Filter(function(f) !is.null(f) && NROW(f) > 0, frames)
  if (!length(frames)) return(NULL)
  frames <- lapply(frames, function(f) { f <- as.data.frame(f, stringsAsFactors = FALSE)
    f <- f[, !vapply(f, is.list, logical(1)), drop = FALSE]; f[] <- lapply(f, as.character); f })
  dplyr::bind_rows(frames)
}
up_empty_sessions <- function() data.frame(sessionId = character(0), date = as.Date(character(0)), stringsAsFactors = FALSE)

# Sessions TrackMan has in [from, to]. kind = "game" | "practice".
up_tm_discover <- function(kind, from, to, session_type = "All", job_dir = NULL, progress = NULL) {
  from <- as.Date(from); to <- as.Date(to)
  chunks <- up_date_chunks(from, to, if (kind == "game") 5 else 28)
  out <- list()
  for (i in seq_along(chunks)) {
    ch <- chunks[[i]]
    if (is.function(progress)) progress((i - 1) / length(chunks), sprintf("Discovering %s sessions %s to %s", kind, ch$from, ch$to))
    body <- list(sessionType = session_type,
                 utcDateFrom = paste0(format(ch$from), "T00:00:00.000Z"),
                 utcDateTo   = paste0(format(ch$to), "T23:59:59.999Z"))
    res <- tryCatch(up_tm_post(paste0("/discovery/", kind, "/sessions"), body), error = function(e) {
      up_log(job_dir, "discovery ", ch$from, " to ", ch$to, " failed: ", conditionMessage(e)); NULL })
    if (!is.null(res) && NROW(res)) out[[length(out) + 1]] <- res
  }
  d <- up_bind_chr(out)
  if (is.null(d) || !nrow(d) || !"sessionId" %in% names(d)) return(up_empty_sessions())
  d <- d[!duplicated(d$sessionId), , drop = FALSE]
  d$date <- up_session_date(d$gameDateLocal %||% rep(NA, nrow(d)), d$gameDateUtc %||% rep(NA, nrow(d)))
  if ("verified" %in% names(d)) d$verified <- tolower(d$verified) %in% c("true", "t", "1")
  d <- d[!is.na(d$date) & d$date >= from & d$date <= to, , drop = FALSE]
  d[order(d$date, d$sessionId), , drop = FALSE]
}

up_scope_filter <- function(s, scope) {
  if (!NROW(s) || is.null(scope) || identical(scope, "all")) return(s)
  gc <- function(nm) if (nm %in% names(s)) as.character(s[[nm]]) else rep(NA_character_, nrow(s))
  keep <- switch(scope,
    ccu = grepl("coastal carolina", gc("homeTeam.name"), ignore.case = TRUE) |
          grepl("coastal carolina", gc("awayTeam.name"), ignore.case = TRUE) |
          gc("homeTeam.shortName") %in% "COA_CHA" | gc("awayTeam.shortName") %in% "COA_CHA",
    sunbelt = gc("league.shortName") %in% "SBELT" | gc("league.name") %in% "NCAA Sun Belt",
    d1 = gc("level.name") %in% "D1",
    rep(TRUE, nrow(s)))
  keep[is.na(keep)] <- FALSE
  s[keep, , drop = FALSE]
}

# ---- games: pull one session (plays + balls) -> master-schema rows ------------------
UP_GAME_META <- c("gameID", "externalSessionId", "sessionType", "verified", "gameDateUtc", "gameDateLocal",
                  "level.name", "league.name", "league.shortName", "location.field.name", "location.venue.name",
                  "homeTeam.name", "homeTeam.shortName", "awayTeam.name", "awayTeam.shortName")
up_tm_pull_game <- function(session_id, meta_row = NULL) {
  plays <- up_tm_get(paste0("/data/game/plays/", session_id))
  if (is.null(plays) || !NROW(plays)) return(NULL)
  plays <- as.data.frame(plays, stringsAsFactors = FALSE)
  plays <- plays[, !vapply(plays, is.list, logical(1)), drop = FALSE]
  if (!"playID" %in% names(plays)) return(NULL)
  plays$join_id <- trimws(as.character(plays$playID))
  plays <- plays[!duplicated(plays$join_id), , drop = FALSE]
  balls <- tryCatch(up_tm_get(paste0("/data/game/balls/", session_id)), error = function(e) NULL)
  out <- plays
  if (!is.null(balls) && NROW(balls) && all(c("kind", "playId") %in% names(balls))) {
    balls <- as.data.frame(balls, stringsAsFactors = FALSE)
    balls$join_id <- trimws(as.character(balls$playId))
    balls <- balls[, !vapply(balls, is.list, logical(1)), drop = FALSE]
    for (k in c("Pitch", "Hit", "CatcherThrow")) {
      b <- balls[!is.na(balls$kind) & balls$kind == k, , drop = FALSE]
      if (!nrow(b)) next
      b <- b[, setdiff(names(b), c("kind", "playId", "version", "trackId", "trackStartTime")), drop = FALSE]
      keep <- names(b) == "join_id" | !vapply(b, function(x) all(is.na(x)), logical(1))
      b <- b[!duplicated(b$join_id), keep, drop = FALSE]
      out <- dplyr::left_join(out, b, by = "join_id", suffix = c("", paste0(".", k)))
    }
  }
  if (!is.null(meta_row)) for (k in intersect(UP_GAME_META, names(meta_row))) out[[k]] <- rep(meta_row[[k]][1], nrow(out))
  out$sessionId <- session_id
  out$date <- rep(as.character(meta_row$date[1] %||% NA), nrow(out))
  out$join_id <- NULL
  out
}

up_master_schema <- function() {
  if (is.null(.up_env$schema)) {
    s <- utils::read.csv(UP_MASTER_SCHEMA_FILE, stringsAsFactors = FALSE)
    .up_env$schema <- stats::setNames(s$type, s$column)
  }
  .up_env$schema
}
# Coerce a pulled frame to the master file's 125 columns and types (extra
# columns are dropped and reported, missing ones are typed NA).
up_canonicalize_master <- function(df) {
  types <- up_master_schema(); n <- NROW(df)
  cols <- lapply(names(types), function(v) {
    t <- types[[v]]
    if (!v %in% names(df)) return(switch(t, character = rep(NA_character_, n), double = rep(NA_real_, n),
                                         integer = rep(NA_integer_, n), rep(NA, n)))
    x <- df[[v]]
    switch(t, character = as.character(x), double = suppressWarnings(as.numeric(x)),
           integer = suppressWarnings(as.integer(round(as.numeric(x)))), logical = as.logical(x), x)
  })
  names(cols) <- names(types)
  out <- as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
  attr(out, "dropped") <- setdiff(names(df), names(types))
  out
}

# ---- practice: bullpens (public.pitches rows) and batting practice ----------------------
.up_nz <- function(x, type = NA_real_) if (is.null(x) || length(x) == 0) type else x[[1]]
.up_as_list_of <- function(x, key) { if (!length(x)) return(list()); if (!is.null(x[[key]])) list(x) else x }

up_list_blobs <- function(base, sas) {
  out <- character(0); marker <- NULL
  repeat {
    url <- paste0(base, "/", sas, "&comp=list&restype=container")
    if (!is.null(marker)) url <- paste0(url, "&marker=", utils::URLencode(marker, TRUE))
    xml <- httr2::request(url) |> httr2::req_retry(max_tries = 3) |> httr2::req_perform() |> httr2::resp_body_xml()
    out <- c(out, xml2::xml_text(xml2::xml_find_all(xml, ".//Blobs/Blob/Name")))
    marker <- xml2::xml_text(xml2::xml_find_first(xml, "./NextMarker"))
    if (is.na(marker) || !nzchar(marker)) break
  }
  out
}
up_edger_index <- function(sid) {
  toks <- tryCatch(up_tm_get_raw(paste0("/media/practice/videotokens/", sid)), error = function(e) list())
  edger <- Filter(function(t) identical(t$type, "EdgertronicVideos"), toks)
  if (!length(edger)) return(NULL)
  e <- edger[[1]]
  blobs <- tryCatch(up_list_blobs(sprintf("https://%s.blob.core.windows.net/%s", e$entityPath, e$endpoint), e$token),
                    error = function(err) character(0))
  if (!length(blobs)) return(NULL)
  parts <- strsplit(blobs, "/", fixed = TRUE)
  d <- data.frame(blob = blobs,
                  play_id = vapply(parts, function(p) if (length(p) >= 2) p[2] else NA_character_, character(1)),
                  video_type = vapply(parts, function(p) if (length(p) >= 3) p[3] else NA_character_, character(1)),
                  stringsAsFactors = FALSE)
  d <- d[is.na(d$video_type) | grepl("edger", d$video_type, ignore.case = TRUE), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  sp <- split(d$blob, d$play_id)
  data.frame(play_id = names(sp), edger_blob = vapply(sp, `[[`, character(1), 1),
             edger_all_blobs = vapply(sp, paste, character(1), collapse = "|"), stringsAsFactors = FALSE)
}
up_edger_meta <- function(sid) {
  res <- tryCatch(up_tm_get_raw(paste0("/media/practice/videometadata/", sid)), error = function(e) list())
  res <- .up_as_list_of(res, "videoClipId")
  if (!length(res)) return(NULL)
  d <- do.call(rbind, lapply(res, function(x) data.frame(
    play_id = x$playId %||% NA_character_, camera_type = x$cameraType %||% NA_character_,
    framerate = .up_nz(x$framerate), clip_seconds = .up_nz(x$videoDurationInSeconds), stringsAsFactors = FALSE)))
  d <- d[is.na(d$camera_type) | grepl("edger", d$camera_type, ignore.case = TRUE), , drop = FALSE]
  d <- d[!duplicated(d$play_id), c("play_id", "framerate", "clip_seconds"), drop = FALSE]
  if (nrow(d)) d else NULL
}
up_flat_play <- function(p) data.frame(
  play_id = p$playID %||% NA_character_, play_date = p$date %||% NA_character_, play_time = p$time %||% NA_character_,
  pitcher = p$pitcher$pitcher %||% NA_character_, pitcher_id = as.character(p$pitcher$pitcherId %||% NA),
  pitcher_throws = p$pitcher$pitcherThrows %||% NA_character_,
  batter = p$batter$batter %||% NA_character_, batter_id = as.character(p$batter$batterId %||% NA),
  batter_side = p$batter$batterSide %||% NA_character_,
  tagged_pitch_type = p$pitchTag$taggedPitchType %||% NA_character_,
  pitch_no = as.integer(p$taggerBehavior$pitchNo %||% NA), pitch_session = p$taggerBehavior$pitchSession %||% NA_character_,
  stringsAsFactors = FALSE)
up_flat_pitch <- function(b) { p <- b$pitch; r <- p$release; tr <- p$trajectory; lo <- p$location; data.frame(
  play_id = b$playId %||% NA_character_, pitch_uid = p$pitchUID %||% NA_character_,
  rel_speed = .up_nz(r$relSpeed), spin_rate = .up_nz(r$spinRate), spin_axis = .up_nz(r$spinAxis),
  tilt = .up_nz(r$tilt, NA_character_), rel_height = .up_nz(r$relHeight), rel_side = .up_nz(r$relSide),
  extension = .up_nz(r$extension), spin_efficiency = .up_nz(r$spinAxis3dSpinEfficiency),
  vert_rel_angle = .up_nz(r$vertRelAngle), horz_rel_angle = .up_nz(r$horzRelAngle),
  induced_vert_break = .up_nz(tr$inducedVertBreak), horz_break = .up_nz(tr$horzBreak), vert_break = .up_nz(tr$vertBreak),
  plate_loc_height = .up_nz(lo$plateLocHeight), plate_loc_side = .up_nz(lo$plateLocSide),
  vert_appr_angle = .up_nz(lo$vertApprAngle), horz_appr_angle = .up_nz(lo$horzApprAngle), zone_speed = .up_nz(lo$zoneSpeed),
  stringsAsFactors = FALSE) }
up_flat_hit <- function(b) { h <- b$hit; l <- h$launch; ld <- h$landing; data.frame(
  play_id = b$playId %||% NA_character_, hit_uid = h$hitUID %||% NA_character_,
  exit_speed = .up_nz(l$exitSpeed), launch_angle = .up_nz(l$angle), direction = .up_nz(l$direction),
  hit_spin_rate = .up_nz(l$spinRate), contact_position_x = .up_nz(l$contactPosition$x),
  contact_position_y = .up_nz(l$contactPosition$y), contact_position_z = .up_nz(l$contactPosition$z),
  last_tracked_distance = .up_nz(h$lastTrackedDistance), hang_time = .up_nz(ld$hangTime),
  distance = .up_nz(ld$distance), bearing = .up_nz(ld$bearing), stringsAsFactors = FALSE) }

# One practice session -> flat rows. kind "Pitching" (bullpens) or "Hitting" (BP).
up_tm_pull_practice <- function(sid, sdate, kind = "Pitching", meta_row = NULL, with_video = TRUE) {
  plays <- .up_as_list_of(tryCatch(up_tm_get_raw(paste0("/data/practice/plays/", sid)), error = function(e) list()), "playID")
  if (!length(plays)) return(NULL)
  balls <- .up_as_list_of(tryCatch(up_tm_get_raw(paste0("/data/practice/balls/", sid)), error = function(e) list()), "trackType")
  play_df <- do.call(rbind, lapply(plays, up_flat_play))
  play_df <- play_df[!is.na(play_df$play_id), , drop = FALSE]
  pitch_l <- Filter(function(b) identical(b$trackType, "Pitch"), balls)
  if (length(pitch_l)) {
    pitch_df <- do.call(rbind, lapply(pitch_l, up_flat_pitch))
    pitch_df <- pitch_df[!is.na(pitch_df$play_id) & !duplicated(pitch_df$play_id), , drop = FALSE]
    play_df <- dplyr::left_join(play_df, pitch_df, by = "play_id")
  }
  if (identical(kind, "Hitting")) {
    hit_l <- Filter(function(b) identical(b$trackType, "Hit"), balls)
    if (length(hit_l)) {
      hit_df <- do.call(rbind, lapply(hit_l, up_flat_hit))
      hit_df <- hit_df[!is.na(hit_df$play_id) & !duplicated(hit_df$play_id), , drop = FALSE]
      play_df <- dplyr::left_join(play_df, hit_df, by = "play_id")
    }
  }
  if (identical(kind, "Pitching") && isTRUE(with_video)) {
    idx <- up_edger_index(sid); emd <- up_edger_meta(sid)
    if (!is.null(idx)) play_df <- dplyr::left_join(play_df, idx, by = "play_id")
    if (!is.null(emd)) play_df <- dplyr::left_join(play_df, emd, by = "play_id")
  }
  for (col in c("edger_blob", "edger_all_blobs", "pitch_uid", "tilt")) if (!col %in% names(play_df)) play_df[[col]] <- NA_character_
  for (col in c("framerate", "clip_seconds")) if (!col %in% names(play_df)) play_df[[col]] <- NA_real_
  raw_by_id <- stats::setNames(plays, vapply(plays, function(p) p$playID %||% NA_character_, character(1)))
  play_df$raw_json <- vapply(play_df$play_id, function(id) {
    r <- raw_by_id[[id]]; if (is.null(r)) NA_character_ else as.character(jsonlite::toJSON(r, auto_unbox = TRUE, null = "null"))
  }, character(1), USE.NAMES = FALSE)
  play_df$has_edger <- !is.na(play_df$edger_blob)
  play_df$session_id <- sid
  play_df$session_date <- as.character(as.Date(sdate))
  play_df$time <- ifelse(!is.na(play_df$play_date) & !is.na(play_df$play_time), paste0(play_df$play_date, "T", play_df$play_time), NA_character_)
  play_df$session_type <- as.character(meta_row$sessionType[1] %||% kind)
  play_df$external_session_id <- as.character(meta_row$externalSessionId[1] %||% NA)
  play_df$pitcher[is.na(play_df$pitcher) | !nzchar(play_df$pitcher)] <- "Unknown"
  play_df[order(play_df$pitch_no), , drop = FALSE]
}

# public.pitches columns (the practice ingest contract). raw is rebuilt from raw_json at upsert.
UP_BULLPEN_COLS <- c("play_id", "session_id", "pitcher", "pitcher_throws", "session_date", "pitch_no", "time",
                     "tagged_pitch_type", "rel_speed", "spin_rate", "spin_axis", "tilt", "induced_vert_break", "horz_break",
                     "vert_appr_angle", "rel_height", "rel_side", "extension", "plate_loc_side", "plate_loc_height",
                     "spin_efficiency", "pitch_uid", "has_edger", "edger_blob", "edger_all_blobs", "framerate", "clip_seconds")

# ---- validation ------------------------------------------------------------------------
up_issue <- function(level, code, message, n = NA, ids = NULL) list(level = level, code = code, message = message, n = n, ids = ids)
up_issues_df <- function(issues) {
  if (!length(issues)) return(data.frame(Level = character(0), Issue = character(0), Rows = integer(0), stringsAsFactors = FALSE))
  data.frame(Level = vapply(issues, `[[`, character(1), "level"), Issue = vapply(issues, `[[`, character(1), "message"),
             Rows = vapply(issues, function(i) as.integer(i$n %||% NA), integer(1)), stringsAsFactors = FALSE)
}
up_validate_games <- function(df, sessions = NULL) {
  iss <- list(); n <- nrow(df)
  uid <- df$pitchUID
  bad_uid <- is.na(uid) | !nzchar(uid)
  if (any(bad_uid)) iss[[length(iss) + 1]] <- up_issue("error", "no_uid", "Pitches without a pitchUID cannot be stored (primary key). They are excluded automatically.", sum(bad_uid))
  dup <- duplicated(uid) & !bad_uid
  if (any(dup)) iss[[length(iss) + 1]] <- up_issue("error", "dup_uid", "Duplicate pitchUIDs: only the first copy of each is kept.", sum(dup))
  untracked <- is.na(df[["pitch.release.relSpeed"]])
  if (any(untracked)) iss[[length(iss) + 1]] <- up_issue("warn", "untracked", "Pitches with no TrackMan ball tracking (no release speed). Kept; they get no Pitch Profiler score.", sum(untracked))
  if (!is.null(df$sessionId)) {
    per <- tapply(untracked, df$sessionId, mean)
    dead <- names(per)[per >= 0.999]
    if (length(dead)) iss[[length(iss) + 1]] <- up_issue("warn", "dead_session", sprintf("%d session(s) have plays but zero ball tracking; TrackMan may not have processed them yet. Consider excluding them and re-fetching later.", length(dead)), sum(df$sessionId %in% dead), dead)
  }
  nop <- is.na(df[["pitcher.name"]]) | !nzchar(df[["pitcher.name"]])
  if (any(nop)) iss[[length(iss) + 1]] <- up_issue("warn", "no_pitcher", "Pitches with no pitcher name (untagged). Kept, but they are left out of every pitcher table.", sum(nop))
  noid <- !nop & (is.na(df[["pitcher.id"]]) | !nzchar(df[["pitcher.id"]]))
  if (any(noid)) iss[[length(iss) + 1]] <- up_issue("info", "no_pitcher_id", "Pitchers without a TrackMan id are keyed by name (name:<Pitcher>).", sum(noid))
  nocall <- is.na(df[["pitchTag.pitchCall"]]) | df[["pitchTag.pitchCall"]] %in% c("", "Undefined")
  if (mean(nocall) > 0.5) iss[[length(iss) + 1]] <- up_issue("warn", "no_call", "More than half the pitches have no pitch call: this looks like an untagged session (BP / scrimmage). Stats will be tracking-only.", sum(nocall))
  st <- table(df$sessionType, useNA = "ifany")
  if (length(st) > 1 || !"Standard" %in% names(st))
    iss[[length(iss) + 1]] <- up_issue("info", "session_types", paste0("Session types in this pull: ", paste(names(st), st, sep = " = ", collapse = ", "), ". Only Standard sessions feed the game pools."), n)
  odd_pt <- df[["pitchTag.taggedPitchType"]]
  known <- c("Fastball", "FourSeamFastBall", "TwoSeamFastBall", "Sinker", "Cutter", "Slider", "Curveball", "ChangeUp", "Splitter", "Sweeper", "Knuckleball", "Other", "Undefined", NA)
  bad_pt <- !odd_pt %in% known & !is.na(odd_pt)
  if (any(bad_pt)) iss[[length(iss) + 1]] <- up_issue("info", "pitch_types", paste0("Custom / unusual tagged pitch types: ", paste(unique(odd_pt[bad_pt]), collapse = ", "), ". Fine for Coastal custom tags; fix typos in Review."), sum(bad_pt))
  if (!is.null(sessions) && nrow(sessions)) {
    failed <- sessions[sessions$status != "ok", , drop = FALSE]
    if (nrow(failed)) iss[[length(iss) + 1]] <- up_issue("warn", "pull_failed", sprintf("%d discovered session(s) could not be pulled (see the sessions list). Re-run the fetch to retry them.", nrow(failed)), nrow(failed), failed$sessionId)
  }
  iss
}
up_validate_practice <- function(df, kind = "Pitching") {
  iss <- list()
  bad <- is.na(df$play_id) | !nzchar(df$play_id)
  if (any(bad)) iss[[length(iss) + 1]] <- up_issue("error", "no_play_id", "Rows without a play id cannot be stored (primary key). Excluded automatically.", sum(bad))
  dup <- duplicated(df$play_id) & !bad
  if (any(dup)) iss[[length(iss) + 1]] <- up_issue("error", "dup_play", "Duplicate play ids: only the first copy is kept.", sum(dup))
  untracked <- is.na(df$rel_speed)
  if (any(untracked)) iss[[length(iss) + 1]] <- up_issue("warn", "untracked", "Plays with no ball tracking (no release speed). Kept, so the pitch count stays right; they show no metrics.", sum(untracked))
  unk <- df$pitcher %in% c("Unknown", "", NA)
  if (any(unk) && identical(kind, "Pitching")) iss[[length(iss) + 1]] <- up_issue("warn", "unknown_pitcher", "Pitches tagged to 'Unknown' pitcher. Fix the name in Review (find & replace) or they land under Unknown in the Bullpens tab.", sum(unk))
  unkb <- if ("batter" %in% names(df)) df$batter %in% c("Unknown", "", NA) else FALSE
  if (any(unkb) && identical(kind, "Hitting")) iss[[length(iss) + 1]] <- up_issue("warn", "unknown_batter", "Swings with no batter name. Fix the name in Review (find & replace) or they show under Unknown in the Batting Practice view.", sum(unkb))
  if (identical(kind, "Pitching")) {
    nov <- !isTRUE(any(df$has_edger))
    if (nov) iss[[length(iss) + 1]] <- up_issue("info", "no_video", "No Edgertronic clips were found for these sessions (normal when the camera was not running).", nrow(df))
  } else {
    noh <- if ("exit_speed" %in% names(df)) is.na(df$exit_speed) else rep(TRUE, nrow(df))
    if (any(noh)) iss[[length(iss) + 1]] <- up_issue("info", "no_hit", "Swings without batted-ball tracking (take, miss, or untracked contact).", sum(noh))
  }
  iss
}

# ---- Supabase: Postgres --------------------------------------------------------------
up_pg_enabled <- function() all(nzchar(Sys.getenv(c("SB_DB_HOST", "SB_DB_USER", "SB_DB_PASS"))))
up_pg_connect <- function(attempts = 4L) {
  if (!up_pg_enabled()) stop("Supabase credentials missing: set SB_DB_HOST, SB_DB_USER, SB_DB_PASS")
  port <- suppressWarnings(as.integer(Sys.getenv("SB_DB_PORT", "5432"))); if (is.na(port)) port <- 5432L
  last <- NULL
  for (i in seq_len(attempts)) {
    con <- tryCatch(DBI::dbConnect(RPostgres::Postgres(), host = Sys.getenv("SB_DB_HOST"), port = port, dbname = "postgres",
                                   user = Sys.getenv("SB_DB_USER"), password = Sys.getenv("SB_DB_PASS"),
                                   sslmode = "require", connect_timeout = 15), error = function(e) e)
    if (!inherits(con, "error")) return(con)
    last <- con; Sys.sleep(3 * i)
  }
  stop(conditionMessage(last))
}
# A connection holder that reconnects between retries (the pooler drops sessions).
up_pg_holder <- function() { h <- new.env(parent = emptyenv()); h$con <- NULL
  h$get <- function() { if (is.null(h$con) || !DBI::dbIsValid(h$con)) h$con <- up_pg_connect(); h$con }
  h$reset <- function() { if (!is.null(h$con)) try(DBI::dbDisconnect(h$con), silent = TRUE); h$con <- NULL }
  h$close <- h$reset
  h }
up_pg_retry <- function(h, what, fn, attempts = 5L, job_dir = NULL) {
  for (i in seq_len(attempts)) {
    out <- tryCatch(fn(h$get()), error = function(e) e)
    if (!inherits(out, "error")) return(out)
    up_log(job_dir, what, " failed (attempt ", i, "): ", substr(conditionMessage(out), 1, 160))
    if (grepl("permission denied|password authentication|does not exist|syntax error", conditionMessage(out), ignore.case = TRUE)) stop(conditionMessage(out))
    h$reset(); Sys.sleep(4 * i)
  }
  stop("giving up on ", what)
}
up_pg_in <- function(x) paste(DBI::dbQuoteString(DBI::ANSI(), unique(as.character(x))), collapse = ", ")
up_pg_columns <- function(con, schema, table) {
  DBI::dbGetQuery(con, "select column_name from information_schema.columns where table_schema = $1 and table_name = $2 order by ordinal_position",
                  params = list(schema, table))$column_name
}
up_pg_frame <- function(df) {
  for (nm in names(df)) { x <- df[[nm]]
    if (inherits(x, "integer64")) df[[nm]] <- as.numeric(x)
    else if (is.list(x)) df[[nm]] <- vapply(x, function(v) paste(v, collapse = "|"), character(1))
    else if (inherits(x, "POSIXt")) df[[nm]] <- format(x, "%Y-%m-%d %H:%M:%S%z")
    else if (is.factor(x)) df[[nm]] <- as.character(x) }
  df
}
# Replace rows of `table` keyed by `keys` (character vector of column names):
# delete the incoming keys, then append in batches. Returns rows written.
up_pg_replace <- function(h, schema, table, df, keys, job_dir = NULL, batch = 2000L, overwrite = TRUE) {
  if (!nrow(df)) return(0L)
  target <- DBI::Id(schema = schema, table = table)
  have <- up_pg_retry(h, paste0("columns of ", table), function(con) up_pg_columns(con, schema, table), job_dir = job_dir)
  extra <- setdiff(names(df), have); missing <- setdiff(have, names(df))
  if (length(extra)) up_log(job_dir, table, ": dropping ", length(extra), " column(s) the table lacks: ", paste(head(extra, 8), collapse = ", "))
  for (m in missing) df[[m]] <- NA
  df <- up_pg_frame(df[, have, drop = FALSE])
  if (length(keys) == 1) {
    kv <- unique(as.character(df[[keys]]))
    existing <- character(0)
    for (s in seq(1, length(kv), by = 5000)) {
      part <- kv[s:min(s + 4999, length(kv))]
      ex <- up_pg_retry(h, paste0("existing ", table), function(con) DBI::dbGetQuery(con,
        sprintf("select %s as k from %s.%s where %s in (%s)", keys, schema, table, keys, up_pg_in(part)))$k, job_dir = job_dir)
      existing <- c(existing, as.character(ex))
    }
    if (length(existing)) {
      if (overwrite) {
        up_log(job_dir, table, ": replacing ", length(existing), " existing row(s)")
        for (s in seq(1, length(existing), by = 5000)) { part <- existing[s:min(s + 4999, length(existing))]
          up_pg_retry(h, paste0("delete ", table), function(con) DBI::dbExecute(con,
            sprintf("delete from %s.%s where %s in (%s)", schema, table, keys, up_pg_in(part))), job_dir = job_dir) }
      } else {
        up_log(job_dir, table, ": skipping ", length(existing), " row(s) already present (overwrite off)")
        df <- df[!as.character(df[[keys]]) %in% existing, , drop = FALSE]
      }
    }
  } else {
    # composite key: delete by the first key's values restricted to the second
    k1 <- unique(as.character(df[[keys[1]]])); k2 <- unique(as.character(df[[keys[2]]]))
    up_pg_retry(h, paste0("delete ", table), function(con) DBI::dbExecute(con,
      sprintf("delete from %s.%s where %s in (%s) and %s in (%s)", schema, table, keys[1], up_pg_in(k1), keys[2], up_pg_in(k2))), job_dir = job_dir)
  }
  n <- 0L
  if (!nrow(df)) return(0L)
  for (s in seq(1, nrow(df), by = batch)) {
    part <- df[s:min(s + batch - 1L, nrow(df)), , drop = FALSE]
    up_pg_retry(h, sprintf("%s batch @%d", table, s), function(con) DBI::dbAppendTable(con, target, part), job_dir = job_dir)
    n <- n + nrow(part)
    up_progress_sub(job_dir, sprintf("%s: %d / %d rows written", table, n, nrow(df)))
  }
  n
}
up_progress_sub <- function(job_dir, msg) {
  p <- up_read_json(file.path(job_dir %||% "", "progress.json"))
  if (!is.null(p)) up_progress(job_dir, (p$pct %||% 0) / 100, msg, p$stage)
}

# ---- Supabase: PostgREST (public schema) ----------------------------------------------
up_rest_enabled <- function() nzchar(Sys.getenv("SUPABASE_URL")) && nzchar(Sys.getenv("SUPABASE_SECRET_KEY"))
up_rest_req <- function(table) {
  if (!up_rest_enabled()) stop("Supabase Storage is not configured: set SUPABASE_URL and SUPABASE_SECRET_KEY")
  key <- Sys.getenv("SUPABASE_SECRET_KEY")
  httr2::request(paste0(sub("/+$", "", Sys.getenv("SUPABASE_URL")), "/rest/v1/", table)) |>
    httr2::req_headers(apikey = key, Authorization = paste("Bearer", key)) |>
    httr2::req_timeout(120) |> httr2::req_retry(max_tries = 3, retry_on_failure = TRUE) |>
    httr2::req_error(is_error = function(r) FALSE)
}
up_rest_check <- function(resp, what) {
  if (httr2::resp_status(resp) >= 300)
    stop(sprintf("%s: HTTP %d %s", what, httr2::resp_status(resp), substr(tryCatch(httr2::resp_body_string(resp), error = function(e) ""), 1, 300)))
  resp
}
up_rest_upsert <- function(table, rows, on_conflict, chunk = 200L, job_dir = NULL, progress = NULL) {
  if (!length(rows)) return(0L)
  n <- 0L
  for (s in seq(1, length(rows), by = chunk)) {
    part <- rows[s:min(s + chunk - 1L, length(rows))]
    up_rest_check(up_rest_req(table) |> httr2::req_url_query(on_conflict = on_conflict) |>
      httr2::req_headers(Prefer = "resolution=merge-duplicates,return=minimal") |>
      httr2::req_body_json(unname(part), auto_unbox = TRUE, null = "null", na = "null", digits = NA) |>
      httr2::req_perform(), paste("upsert", table))
    n <- n + length(part)
    if (is.function(progress)) progress(n / length(rows), sprintf("%s: %d / %d rows upserted", table, n, length(rows)))
  }
  n
}
up_rest_select <- function(table, select = "*", filters = list(), page = 1000L, order = NULL) {
  out <- list(); from <- 0L
  repeat {
    rq <- up_rest_req(table) |> httr2::req_url_query(select = select) |>
      httr2::req_headers(Range = sprintf("%d-%d", from, from + page - 1L), `Range-Unit` = "items")
    for (k in seq_along(filters)) rq <- rq |> httr2::req_url_query(!!names(filters)[k] := filters[[k]])
    if (!is.null(order)) rq <- rq |> httr2::req_url_query(order = order)
    resp <- up_rest_check(httr2::req_perform(rq), paste("select", table))
    d <- tryCatch(jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(d) || !length(d)) break
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    out[[length(out) + 1]] <- d
    if (nrow(d) < page) break
    from <- from + page
  }
  if (!length(out)) return(data.frame())
  dplyr::bind_rows(out)
}
# which of these ids already exist in `table`
up_rest_existing <- function(table, col, ids) {
  ids <- unique(ids[!is.na(ids)]); if (!length(ids)) return(character(0))
  found <- character(0)
  for (s in seq(1, length(ids), by = 300)) {
    part <- ids[s:min(s + 299, length(ids))]
    d <- up_rest_select(table, select = col, filters = stats::setNames(list(paste0("in.(", paste(part, collapse = ","), ")")), col))
    if (nrow(d)) found <- c(found, as.character(d[[col]]))
  }
  found
}

# ---- Supabase: Storage -------------------------------------------------------------------
up_storage_req <- function(path, method = "GET") {
  if (!up_rest_enabled()) stop("Supabase Storage is not configured: set SUPABASE_URL and SUPABASE_SECRET_KEY")
  key <- Sys.getenv("SUPABASE_SECRET_KEY")
  httr2::request(sprintf("%s/storage/v1/object/%s/%s", sub("/+$", "", Sys.getenv("SUPABASE_URL")), UP_BUCKET, path)) |>
    httr2::req_method(method) |>
    httr2::req_headers(apikey = key, Authorization = paste("Bearer", key)) |>
    httr2::req_timeout(600) |> httr2::req_error(is_error = function(r) FALSE)
}
up_storage_upload <- function(local, path, content_type = "application/octet-stream") {
  resp <- NULL
  for (attempt in 1:4) {
    resp <- tryCatch(up_storage_req(path, "POST") |>
      httr2::req_headers(`x-upsert` = "true", `Content-Type` = content_type) |>
      httr2::req_body_file(local) |> httr2::req_perform(), error = function(e) e)
    if (!inherits(resp, "error") && httr2::resp_status(resp) < 300) return(invisible(TRUE))
    Sys.sleep(3 * attempt)
  }
  if (inherits(resp, "error")) stop("Storage upload of ", path, " failed: ", conditionMessage(resp))
  stop(sprintf("Storage upload of %s failed: HTTP %d %s", path, httr2::resp_status(resp),
               substr(tryCatch(httr2::resp_body_string(resp), error = function(e) ""), 1, 200)))
}
up_storage_download <- function(path, dest) {
  for (attempt in 1:3) {
    resp <- tryCatch(up_storage_req(path) |> httr2::req_perform(path = dest), error = function(e) e)
    if (!inherits(resp, "error")) {
      st <- httr2::resp_status(resp)
      if (st == 200 && file.exists(dest) && file.info(dest)$size > 0) return(TRUE)
      if (file.exists(dest)) unlink(dest)
      if (st %in% c(400, 404)) return(FALSE)
    } else if (file.exists(dest)) unlink(dest)
    Sys.sleep(2 * attempt)
  }
  FALSE
}
up_storage_list <- function(prefix, limit = 1000L) {
  key <- Sys.getenv("SUPABASE_SECRET_KEY")
  resp <- httr2::request(sprintf("%s/storage/v1/object/list/%s", sub("/+$", "", Sys.getenv("SUPABASE_URL")), UP_BUCKET)) |>
    httr2::req_method("POST") |> httr2::req_headers(apikey = key, Authorization = paste("Bearer", key)) |>
    httr2::req_body_json(list(prefix = prefix, limit = limit, offset = 0, sortBy = list(column = "name", order = "asc"))) |>
    httr2::req_timeout(60) |> httr2::req_error(is_error = function(r) FALSE) |> httr2::req_perform()
  up_rest_check(resp, "Storage list")
  d <- tryCatch(jsonlite::fromJSON(httr2::resp_body_string(resp)), error = function(e) NULL)
  if (is.null(d) || !length(d)) return(data.frame(name = character(0)))
  as.data.frame(d, stringsAsFactors = FALSE)
}
up_storage_read_parquet <- function(path) {
  dest <- tempfile(fileext = ".parquet"); on.exit(unlink(dest))
  if (!up_storage_download(path, dest)) return(NULL)
  tryCatch(as.data.frame(arrow::read_parquet(dest)), error = function(e) NULL)
}
up_storage_read_json <- function(path, simplify = FALSE) {
  dest <- tempfile(fileext = ".json"); on.exit(unlink(dest))
  if (!up_storage_download(path, dest)) return(NULL)
  tryCatch(jsonlite::fromJSON(dest, simplifyVector = simplify), error = function(e) NULL)
}
up_storage_put_df <- function(df, path) {
  tmp <- tempfile(fileext = ".parquet"); on.exit(unlink(tmp))
  arrow::write_parquet(df, tmp, compression = "zstd")
  up_storage_upload(tmp, path); invisible(file.size(tmp))
}
up_storage_put_json <- function(x, path) {
  tmp <- tempfile(fileext = ".json"); on.exit(unlink(tmp))
  up_write_json(x, tmp); up_storage_upload(tmp, path, "application/json"); invisible(TRUE)
}

# ---- the upload log (Storage uploads/log.json) --------------------------------------------
up_log_read <- function(force = FALSE) {
  if (!force && !is.null(.up_env$uplog) && difftime(Sys.time(), .up_env$uplog_at, units = "secs") < 20) return(.up_env$uplog)
  entries <- list()
  if (up_rest_enabled()) {
    obj <- tryCatch(up_storage_read_json(UP_LOG_PATH), error = function(e) NULL)
    if (is.list(obj) && is.list(obj$entries)) entries <- obj$entries
  }
  .up_env$uplog <- entries; .up_env$uplog_at <- Sys.time()
  entries
}
up_log_append <- function(entry) {
  entries <- up_log_read(force = TRUE)
  i <- which(vapply(entries, function(e) identical(e$id, entry$id), logical(1)))
  if (length(i)) entries[[i[1]]] <- entry else entries[[length(entries) + 1]] <- entry
  up_storage_put_json(list(version = 1L, updated_at = up_now(), entries = entries), UP_LOG_PATH)
  .up_env$uplog <- entries; .up_env$uplog_at <- Sys.time()
  invisible(entries)
}
up_log_table <- function(entries = up_log_read()) {
  if (!length(entries)) return(data.frame(When = character(0), Who = character(0), Type = character(0), Season = character(0),
                                          Dates = character(0), Sessions = integer(0), Rows = integer(0), Status = character(0), id = character(0)))
  g <- function(e, k, d = NA) { v <- e[[k]]; if (is.null(v) || !length(v)) d else v[[1]] }
  d <- do.call(rbind, lapply(entries, function(e) data.frame(
    When = as.character(g(e, "committed_at", g(e, "created", ""))), Who = as.character(g(e, "user", "")),
    Type = as.character(UPLOAD_TYPES[[g(e, "type", "games")]]$label %||% g(e, "type", "")),
    Season = as.character(g(e, "season_label", "")),
    Dates = paste0(g(e, "from", ""), " to ", g(e, "to", "")),
    Sessions = as.integer(g(e, "n_sessions", NA)), Rows = as.integer(g(e, "n_rows", NA)),
    Status = as.character(g(e, "status", "")), id = as.character(g(e, "id", "")), stringsAsFactors = FALSE)))
  d[order(d$When, decreasing = TRUE), , drop = FALSE]
}

# ---- Python scorer --------------------------------------------------------------------------
up_python <- function() {
  cands <- c(Sys.getenv("PALACE_PYTHON"), Sys.getenv("RETICULATE_PYTHON"), path.expand("~/.venvs/pitchprofiler/bin/python"),
             Sys.which("python3"), Sys.which("python"))
  cands <- cands[nzchar(cands) & file.exists(cands)]
  for (p in cands) {
    ok <- tryCatch(processx::run(p, c("-c", "import lightgbm, polars, pandas, pyarrow, numpy"), error_on_status = FALSE, timeout = 60)$status == 0,
                   error = function(e) FALSE)
    if (isTRUE(ok)) return(p)
  }
  NULL
}
up_score_pitches <- function(staged, out_dir, job_dir = NULL, workers = 2L) {
  py <- up_python()
  if (is.null(py)) stop("Pitch Profiler scorer: no Python with lightgbm/polars/pandas/pyarrow found (set PALACE_PYTHON)")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  log_file <- file.path(job_dir %||% out_dir, "scorer.log")
  res <- processx::run(py, c("scripts/process_master.py", "--master", staged, "--out", out_dir, "--workers", as.character(workers), "--chunks", "1"),
                       env = c("current", PYTHONPATH = paste(c("models", "scripts", Sys.getenv("PYTHONPATH")), collapse = .Platform$path.sep)),
                       error_on_status = FALSE, stdout = log_file, stderr = "2>&1", timeout = 3600)
  tail_txt <- if (file.exists(log_file)) paste(utils::tail(readLines(log_file, warn = FALSE), 12), collapse = "\n") else ""
  if (res$status != 0 || !file.exists(file.path(out_dir, "pitches.parquet")))
    stop("scorer failed (exit ", res$status, "):\n", tail_txt)
  up_log(job_dir, "scorer: ", gsub("\n", " | ", utils::tail(strsplit(tail_txt, "\n")[[1]], 3) |> paste(collapse = " ")))
  out_dir
}
up_aggregate_subset <- function(pitches_parquet, out_dir, job_dir = NULL) {
  py <- up_python(); if (is.null(py)) stop("scorer python not found")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  log_file <- file.path(job_dir %||% out_dir, "aggregate.log")
  res <- processx::run(py, c("scripts/aggregate_subset.py", "--pitches", pitches_parquet, "--out", out_dir),
                       env = c("current", PYTHONPATH = paste(c("models", "scripts", Sys.getenv("PYTHONPATH")), collapse = .Platform$path.sep)),
                       error_on_status = FALSE, stdout = log_file, stderr = "2>&1", timeout = 1800)
  if (res$status != 0) stop("season aggregate refresh failed: ", paste(utils::tail(readLines(log_file, warn = FALSE), 6), collapse = " | "))
  out_dir
}

# ---- preflight -------------------------------------------------------------------------------
up_check <- function(name, ok, detail, hint = NULL) list(name = name, ok = ok, detail = detail, hint = hint)
up_preflight <- function(with_trumedia = TRUE) {
  checks <- list()
  # TrackMan
  cr <- up_tm_creds()
  if (!nzchar(cr$secret)) checks[[length(checks) + 1]] <- up_check("TrackMan Data API", FALSE, "TM_CLIENT_SECRET is not set", up_hint("trackman credentials missing"))
  else {
    tk <- tryCatch(up_tm_token(force = TRUE), error = function(e) e)
    checks[[length(checks) + 1]] <- if (inherits(tk, "error")) up_check("TrackMan Data API", FALSE, conditionMessage(tk), up_hint(conditionMessage(tk)))
      else up_check("TrackMan Data API", TRUE, paste0("token minted for client ", cr$id))
  }
  # TruMedia
  if (with_trumedia) {
    if (!all(nzchar(Sys.getenv(c("TM_USERNAME", "TM_SITENAME", "TM_MASTER_TOKEN")))))
      checks[[length(checks) + 1]] <- up_check("TruMedia", NA, "TM_USERNAME / TM_SITENAME / TM_MASTER_TOKEN not set (only the season-stats rebuild needs them)", up_hint("tm_master_token"))
    else {
      res <- tryCatch(httr2::request("https://api.trumedianetworks.com/v1/siteadmin/api/createTempPBToken") |>
        httr2::req_body_json(list(username = Sys.getenv("TM_USERNAME"), sitename = Sys.getenv("TM_SITENAME"), token = Sys.getenv("TM_MASTER_TOKEN"))) |>
        httr2::req_timeout(30) |> httr2::req_error(is_error = function(r) FALSE) |> httr2::req_perform(), error = function(e) e)
      if (inherits(res, "error")) checks[[length(checks) + 1]] <- up_check("TruMedia", FALSE, conditionMessage(res), up_hint(conditionMessage(res)))
      else {
        body <- tryCatch(httr2::resp_body_string(res), error = function(e) "")
        ok <- httr2::resp_status(res) == 200 && grepl("pbTempToken", body)
        checks[[length(checks) + 1]] <- up_check("TruMedia", ok, if (ok) "temp token minted" else paste0("HTTP ", httr2::resp_status(res), " ", substr(body, 1, 120)),
                                                 if (!ok) up_hint(paste("temp token", body)))
      }
    }
  }
  # Storage + REST
  if (!up_rest_enabled()) checks[[length(checks) + 1]] <- up_check("Supabase Storage / REST", FALSE, "SUPABASE_URL / SUPABASE_SECRET_KEY not set", up_hint("supabase_url"))
  else {
    st <- tryCatch(up_storage_list("uploads", 5), error = function(e) e)
    checks[[length(checks) + 1]] <- if (inherits(st, "error")) up_check("Supabase Storage", FALSE, conditionMessage(st), up_hint(conditionMessage(st)))
      else up_check("Supabase Storage", TRUE, paste0("bucket ", UP_BUCKET, " reachable; upload log ", if (any(grepl("log.json", st$name))) "present" else "will be created"))
    rs <- tryCatch(up_rest_select("pitches", select = "play_id", page = 1L), error = function(e) e)
    checks[[length(checks) + 1]] <- if (inherits(rs, "error")) up_check("Supabase REST (public.pitches)", FALSE, conditionMessage(rs), up_hint(conditionMessage(rs)))
      else up_check("Supabase REST (public.pitches)", TRUE, "bullpen table reachable through PostgREST")
  }
  # Postgres write
  if (!up_pg_enabled()) checks[[length(checks) + 1]] <- up_check("Supabase Postgres (pitchprofiler)", FALSE, "SB_DB_HOST / SB_DB_USER / SB_DB_PASS not set: game uploads cannot be committed", up_hint("sb_db_host"))
  else {
    con <- tryCatch(up_pg_connect(2L), error = function(e) e)
    if (inherits(con, "error")) checks[[length(checks) + 1]] <- up_check("Supabase Postgres (pitchprofiler)", FALSE, conditionMessage(con), up_hint(conditionMessage(con)))
    else {
      pr <- tryCatch(DBI::dbGetQuery(con, "select has_table_privilege('pitchprofiler.pitches','INSERT') as ins, has_table_privilege('pitchprofiler.pitches','DELETE') as del, current_user as u"), error = function(e) e)
      try(DBI::dbDisconnect(con), silent = TRUE)
      if (inherits(pr, "error")) checks[[length(checks) + 1]] <- up_check("Supabase Postgres (pitchprofiler)", FALSE, conditionMessage(pr), up_hint(conditionMessage(pr)))
      else {
        ok <- isTRUE(pr$ins) && isTRUE(pr$del)
        checks[[length(checks) + 1]] <- up_check("Supabase Postgres (pitchprofiler)", ok,
          if (ok) paste0("connected as ", pr$u, " with INSERT + DELETE on pitchprofiler.pitches") else paste0("connected as ", pr$u, " but it cannot write pitchprofiler.pitches"),
          if (!ok) up_hint("permission denied"))
      }
    }
  }
  # scorer
  py <- up_python()
  checks[[length(checks) + 1]] <- if (is.null(py)) up_check("Pitch Profiler scorer (Python)", FALSE, "no interpreter with lightgbm + polars + pandas + pyarrow", up_hint("no module named"))
    else up_check("Pitch Profiler scorer (Python)", TRUE, py)
  # job dir
  d <- tryCatch(up_root_dir(), error = function(e) e)
  checks[[length(checks) + 1]] <- if (inherits(d, "error") || !file.access(d, 2) == 0) up_check("Upload working directory", FALSE, "not writable", up_hint("disk"))
    else up_check("Upload working directory", TRUE, d)
  checks[[length(checks) + 1]] <- up_check("Background worker (callr)", requireNamespace("callr", quietly = TRUE) && requireNamespace("processx", quietly = TRUE),
                                           "jobs run in a separate R process so the app stays responsive")
  checks
}

# =============================================================================
# Jobs. A job is a directory under up_root_dir():
#   meta.json      type / dates / user / status / result
#   progress.json  pct + message (written by the worker, polled by the panel)
#   log.txt        human log
#   staged.parquet what the fetch produced (canonical frame)
#   sessions.csv   every discovered session with its pull status
#   issues.json    validation report
#   edits.json     review edits (exclusions, find/replace, cell edits)
#   committed.parquet / scored/   what the commit wrote
# The fetch and commit stages run through up_bg_start() in a callr child that
# sources this file, so nothing here may touch Shiny.
# =============================================================================
up_job_id  <- function() sprintf("%s-%s", format(Sys.time(), "%Y%m%d-%H%M%S"), paste(sample(c(letters, 0:9), 5, TRUE), collapse = ""))
up_job_dir <- function(id) file.path(up_root_dir(), id)
up_id_col  <- function(type) if (identical(type, "games")) "pitchUID" else "play_id"
up_sess_col <- function(type) if (identical(type, "games")) "sessionId" else "session_id"

up_job_create <- function(type, from, to, user = "", scope = "all", season_key = NULL, opts = list()) {
  if (!type %in% names(UPLOAD_TYPES)) stop("unknown upload type: ", type)
  id <- up_job_id(); d <- up_job_dir(id); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  sr <- if (!is.null(season_key) && nzchar(season_key)) up_season_row(season_key) else NULL
  up_write_json(list(id = id, type = type, type_label = UPLOAD_TYPES[[type]]$label,
                     from = as.character(as.Date(from)), to = as.character(as.Date(to)),
                     scope = scope %||% "all", user = user %||% "", season_key = season_key,
                     season_label = if (!is.null(sr) && nrow(sr)) sr$label[1] else "",
                     created = up_now(), status = "created", opts = opts),
                file.path(d, "meta.json"))
  up_log(d, "job ", id, " created: ", type, " ", from, " to ", to, " by ", user %||% "?")
  d
}
up_job_meta <- function(job_dir) up_read_json(file.path(job_dir, "meta.json"))
up_job_update <- function(job_dir, ...) {
  m <- up_job_meta(job_dir) %||% list(); new <- list(...)
  for (nm in names(new)) m[nm] <- list(new[[nm]])
  up_write_json(m, file.path(job_dir, "meta.json")); invisible(m)
}
up_job_list <- function() {
  root <- up_root_dir(); dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  metas <- Filter(Negate(is.null), lapply(dirs, up_job_meta))
  metas[order(vapply(metas, function(m) m$created %||% "", character(1)), decreasing = TRUE)]
}
up_job_log_tail <- function(job_dir, n = 40) {
  f <- file.path(job_dir, "log.txt"); if (!file.exists(f)) return(character(0))
  utils::tail(readLines(f, warn = FALSE), n)
}

# ---- background worker -----------------------------------------------------------------
# Runs `fun_name`(args) in a fresh R process that sources this file from the
# app directory. Result (or an "up_error") lands in an rds the caller polls.
up_bg_start <- function(name, fun_name, args = list(), job_dir = NULL) {
  wd <- getwd(); out <- tempfile(pattern = paste0("up_", name, "_"), fileext = ".rds")
  stdout <- if (!is.null(job_dir)) file.path(job_dir, paste0("worker_", name, ".txt")) else tempfile(fileext = ".txt")
  proc <- callr::r_bg(function(wd, fun_name, args, out) {
    setwd(wd); source("R/palace_upload.R")
    res <- tryCatch(do.call(get(fun_name), args),
                    error = function(e) structure(list(error = conditionMessage(e)), class = "up_error"))
    saveRDS(res, out); TRUE
  }, args = list(wd = wd, fun_name = fun_name, args = args, out = out),
     stdout = stdout, stderr = "2>&1", supervise = TRUE)
  list(proc = proc, out = out, stdout = stdout, name = name, job_dir = job_dir, started = Sys.time())
}
up_bg_alive  <- function(bg) !is.null(bg) && bg$proc$is_alive()
up_bg_result <- function(bg) {
  if (is.null(bg) || bg$proc$is_alive()) return(NULL)
  if (file.exists(bg$out)) return(tryCatch(readRDS(bg$out), error = function(e) structure(list(error = conditionMessage(e)), class = "up_error")))
  tail_txt <- if (file.exists(bg$stdout)) paste(utils::tail(readLines(bg$stdout, warn = FALSE), 8), collapse = "\n") else ""
  structure(list(error = paste0("the worker stopped without a result", if (nzchar(tail_txt)) paste0(":\n", tail_txt) else "")), class = "up_error")
}
up_is_error <- function(x) inherits(x, "up_error")

# Meta + progress + log for the panel; marks the job failed when its worker
# died (or errored) without reporting.
up_job_status <- function(job_dir, bg = NULL) {
  m <- up_job_meta(job_dir); alive <- up_bg_alive(bg)
  st <- m$status %||% ""
  if (!alive && !is.null(bg) && grepl("ing$|^created$", st)) {
    res <- up_bg_result(bg)
    if (up_is_error(res)) {
      stage <- if (identical(st, "committing")) "commit" else "fetch"
      up_log(job_dir, "ERROR: ", res$error)
      m <- up_job_update(job_dir, status = paste0(stage, "_failed"), error = res$error, hint = up_hint(res$error))
    } else if (grepl("ing$|^created$", m$status %||% "")) {
      m <- up_job_update(job_dir, status = "failed", error = "the worker stopped without reporting a result", hint = up_hint(""))
    }
  }
  list(meta = m, progress = up_read_json(file.path(job_dir, "progress.json")), alive = alive, log = up_job_log_tail(job_dir))
}

# ---- fetch --------------------------------------------------------------------------------
UP_PRACTICE_NUM <- c("rel_speed", "spin_rate", "spin_axis", "rel_height", "rel_side", "extension", "spin_efficiency",
                     "vert_rel_angle", "horz_rel_angle", "induced_vert_break", "horz_break", "vert_break",
                     "plate_loc_height", "plate_loc_side", "vert_appr_angle", "horz_appr_angle", "zone_speed",
                     "exit_speed", "launch_angle", "direction", "hit_spin_rate", "contact_position_x",
                     "contact_position_y", "contact_position_z", "last_tracked_distance", "hang_time", "distance",
                     "bearing", "framerate", "clip_seconds")
up_bind_practice <- function(frames) {
  frames <- Filter(function(f) !is.null(f) && nrow(f) > 0, frames)
  if (!length(frames)) return(NULL)
  out <- tryCatch(dplyr::bind_rows(frames), error = function(e) NULL)
  if (is.null(out)) {
    out <- up_bind_chr(frames)
    for (cn in intersect(UP_PRACTICE_NUM, names(out))) out[[cn]] <- suppressWarnings(as.numeric(out[[cn]]))
    if ("pitch_no" %in% names(out)) out$pitch_no <- suppressWarnings(as.integer(out$pitch_no))
    if ("has_edger" %in% names(out)) out$has_edger <- tolower(out$has_edger) %in% "true"
  }
  out
}

up_fetch <- function(job_dir) {
  m <- up_job_meta(job_dir); if (is.null(m)) stop("job not found: ", job_dir)
  spec <- UPLOAD_TYPES[[m$type]]
  up_job_update(job_dir, status = "fetching", fetch_started = up_now(), error = NULL, hint = NULL)
  prog <- function(p, msg) up_progress(job_dir, p, msg, "fetch")
  up_log(job_dir, "fetch ", m$type, " ", m$from, " to ", m$to, " (scope ", m$scope %||% "all", ")")
  if (identical(m$type, "trumedia")) {
    prog(1, "Nothing to fetch: the season-stats rebuild talks to TruMedia during commit.")
    up_job_update(job_dir, status = "staged", n_sessions = 0L, n_sessions_found = 0L, n_rows = 0L, staged_at = up_now())
    return(invisible(job_dir))
  }
  kind <- spec$session_kind
  if (identical(m$type, "bp")) {
    sess <- up_bp_discover(m$from, m$to, m$scope %||% "all", job_dir = job_dir, progress = function(p, msg) prog(0.15 * p, msg))
  } else {
    sess <- up_tm_discover(kind, m$from, m$to, session_type = if (kind == "practice") spec$practice_type else "All",
                           job_dir = job_dir, progress = function(p, msg) prog(0.15 * p, msg))
    if (kind == "practice" && nrow(sess) && "sessionType" %in% names(sess))
      sess <- sess[is.na(sess$sessionType) | sess$sessionType == spec$practice_type, , drop = FALSE]
    if (kind == "game") sess <- up_scope_filter(sess, m$scope)
    sess$source <- rep(kind, nrow(sess))
  }
  up_log(job_dir, nrow(sess), " session(s) to pull", if (identical(m$type, "bp")) sprintf(" (%d practice/Hitting, %d game/BattingPractice)", sum(sess$source == "practice"), sum(sess$source == "game")) else "")
  sess$status <- rep("pending", nrow(sess)); sess$n_rows <- rep(NA_integer_, nrow(sess)); sess$note <- rep("", nrow(sess))
  frames <- vector("list", nrow(sess))
  for (i in seq_len(nrow(sess))) {
    sid <- sess$sessionId[i]
    prog(0.15 + 0.8 * (i - 1) / max(1, nrow(sess)), sprintf("Pulling session %d of %d (%s)", i, nrow(sess), format(sess$date[i])))
    df <- tryCatch(
      if (identical(m$type, "bp") && identical(sess$source[i], "game")) up_bp_from_game(up_tm_pull_game(sid, sess[i, , drop = FALSE]))
      else if (kind == "game") up_tm_pull_game(sid, sess[i, , drop = FALSE])
      else up_tm_pull_practice(sid, sess$date[i], spec$practice_type, sess[i, , drop = FALSE], with_video = !isFALSE(m$opts$with_video)),
      error = function(e) e)
    if (inherits(df, "error")) {
      sess$status[i] <- "failed"; sess$note[i] <- substr(conditionMessage(df), 1, 200)
      up_log(job_dir, "session ", sid, " failed: ", conditionMessage(df)); next
    }
    if (is.null(df) || !nrow(df)) { sess$status[i] <- "empty"; sess$note[i] <- "no plays"; next }
    sess$status[i] <- "ok"; sess$n_rows[i] <- nrow(df); frames[[i]] <- df
    if (kind == "practice") {
      who <- if (identical(spec$practice_type, "Hitting")) df$batter else df$pitcher
      tab <- sort(table(who[!is.na(who) & nzchar(who)]), decreasing = TRUE)
      sess$note[i] <- paste(utils::head(names(tab), 3), collapse = ", ")
    } else {
      sess$note[i] <- paste(sess[["awayTeam.name"]][i] %||% "", "at", sess[["homeTeam.name"]][i] %||% "")
    }
    up_log(job_dir, "session ", sid, " (", format(sess$date[i]), "): ", nrow(df), " rows", if (nzchar(sess$note[i])) paste0(" - ", sess$note[i]) else "")
  }
  prog(0.96, "Staging and validating")
  staged <- if (kind == "game") up_bind_chr(frames) else up_bind_practice(frames)
  if (is.null(staged)) staged <- data.frame()
  if (kind == "game" && nrow(staged)) {
    staged <- up_canonicalize_master(staged)
    dropped <- attr(staged, "dropped")
    if (length(dropped)) up_log(job_dir, length(dropped), " column(s) outside the master schema dropped: ", paste(utils::head(dropped, 10), collapse = ", "))
  }
  issues <- if (!nrow(staged)) list(up_issue("warn", "no_sessions", if (nrow(sess)) "Sessions were found but none produced rows (see the sessions list)." else "No sessions in that window for this type and scope.", 0L))
            else if (kind == "game") up_validate_games(staged, sess) else up_validate_practice(staged, spec$practice_type)
  failed <- sess[sess$status == "failed", , drop = FALSE]
  if (nrow(failed) && kind == "practice")
    issues[[length(issues) + 1]] <- up_issue("warn", "pull_failed", sprintf("%d session(s) could not be pulled; re-run the fetch to retry them.", nrow(failed)), nrow(failed), failed$sessionId)
  arrow::write_parquet(staged, file.path(job_dir, "staged.parquet"))
  out_sess <- as.data.frame(sess, stringsAsFactors = FALSE); out_sess$date <- as.character(out_sess$date)
  utils::write.csv(out_sess, file.path(job_dir, "sessions.csv"), row.names = FALSE)
  up_write_json(issues, file.path(job_dir, "issues.json"))
  up_job_update(job_dir, status = "staged", n_sessions = sum(sess$status == "ok"), n_sessions_found = nrow(sess),
                n_rows = nrow(staged), staged_at = up_now())
  prog(1, sprintf("Staged %s rows from %d of %d session(s)", format(nrow(staged), big.mark = ","), sum(sess$status == "ok"), nrow(sess)))
  up_log(job_dir, "staged ", nrow(staged), " rows")
  invisible(job_dir)
}
up_job_staged   <- function(job_dir) { f <- file.path(job_dir, "staged.parquet"); if (file.exists(f)) as.data.frame(arrow::read_parquet(f)) else NULL }
up_job_sessions <- function(job_dir) { f <- file.path(job_dir, "sessions.csv"); if (file.exists(f)) utils::read.csv(f, stringsAsFactors = FALSE, colClasses = "character") else NULL }
up_job_issues   <- function(job_dir) { x <- up_read_json(file.path(job_dir, "issues.json")); if (is.null(x)) list() else x }

# ---- review edits -------------------------------------------------------------------------
up_edits_default <- function() list(exclude_rows = character(0), exclude_sessions = character(0), cells = list(), replace = list())
up_edits_read <- function(job_dir) {
  e <- up_read_json(file.path(job_dir, "edits.json")); d <- up_edits_default()
  if (is.null(e)) return(d)
  d$exclude_rows <- as.character(unlist(e$exclude_rows)); d$exclude_sessions <- as.character(unlist(e$exclude_sessions))
  d$cells <- e$cells %||% list(); d$replace <- e$replace %||% list(); d
}
up_edits_write <- function(job_dir, edits) up_write_json(edits, file.path(job_dir, "edits.json"))
.up_set_cell <- function(x, i, value) {
  if (is.numeric(x)) x[i] <- suppressWarnings(as.numeric(value))
  else if (is.logical(x)) x[i] <- tolower(as.character(value)) %in% c("true", "t", "1", "yes")
  else x[i] <- as.character(value)
  x
}
up_apply_edits <- function(df, edits, id_col, sess_col = NULL, drop_excluded = TRUE) {
  if (is.null(df) || !nrow(df)) return(df)
  for (r in edits$replace) {
    if (is.null(r$col) || !r$col %in% names(df)) next
    x <- df[[r$col]]; hit <- !is.na(x) & as.character(x) == as.character(r$from)
    if (any(hit)) df[[r$col]] <- .up_set_cell(x, which(hit), r$to)
  }
  ids <- as.character(df[[id_col]])
  for (cell in edits$cells) {
    if (is.null(cell$col) || !cell$col %in% names(df)) next
    i <- match(as.character(cell$id), ids)
    if (!is.na(i)) df[[cell$col]] <- .up_set_cell(df[[cell$col]], i, cell$value)
  }
  excl <- ids %in% as.character(edits$exclude_rows)
  if (length(edits$exclude_sessions) && !is.null(sess_col) && sess_col %in% names(df))
    excl <- excl | as.character(df[[sess_col]]) %in% as.character(edits$exclude_sessions)
  if (drop_excluded) df[!excl, , drop = FALSE] else { df$.excluded <- excl; df }
}
# The frame a commit writes: staged + edits, without empty / duplicate ids.
up_commit_frame <- function(job_dir) {
  m <- up_job_meta(job_dir); df <- up_job_staged(job_dir)
  if (is.null(df) || !nrow(df)) return(df)
  idc <- up_id_col(m$type)
  df <- up_apply_edits(df, up_edits_read(job_dir), idc, up_sess_col(m$type))
  if (idc %in% names(df)) {
    ids <- as.character(df[[idc]]); keep <- !is.na(ids) & nzchar(ids) & !duplicated(ids)
    df <- df[keep, , drop = FALSE]
  }
  df
}

# ---- commit --------------------------------------------------------------------------------
# rows for PostgREST: every row carries every column (bulk upserts require it),
# NA -> null, an "a|b" blob string -> array
up_df_rows <- function(df, array_cols = character(0)) {
  cols <- names(df)
  lapply(seq_len(nrow(df)), function(i) {
    r <- lapply(cols, function(cn) { v <- df[[cn]][i]; if (length(v) != 1 || is.na(v)) NA else v }); names(r) <- cols
    for (ac in intersect(array_cols, cols)) if (!is.na(r[[ac]])) r[[ac]] <- as.list(strsplit(as.character(r[[ac]]), "|", fixed = TRUE)[[1]])
    r
  })
}
up_utc_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

up_bullpen_rows <- function(df) {
  cols <- intersect(UP_BULLPEN_COLS, names(df))
  for (cn in setdiff(UP_BULLPEN_COLS, cols)) df[[cn]] <- NA
  out <- df[, UP_BULLPEN_COLS, drop = FALSE]
  out$has_edger <- out$has_edger %in% TRUE
  out$updated_at <- up_utc_now()
  rows <- up_df_rows(out, array_cols = "edger_all_blobs")
  raw <- if ("raw_json" %in% names(df)) df$raw_json else rep(NA_character_, nrow(df))
  for (i in seq_along(rows)) rows[[i]]$raw <- if (!is.na(raw[i])) tryCatch(jsonlite::fromJSON(raw[i], simplifyVector = FALSE), error = function(e) NA) else NA
  rows
}
up_session_rows <- function(df, who_col = "pitcher") {
  sp <- split(df, df$session_id); now <- up_utc_now()
  lapply(names(sp), function(sid) {
    s <- sp[[sid]]; who <- s[[who_col]]; tab <- sort(table(who[!is.na(who) & nzchar(who)]), decreasing = TRUE)
    list(session_id = sid, session_date = as.character(s$session_date[1]), pitcher = if (length(tab)) names(tab)[1] else NA,
         facility = NA, ingested_at = now)
  })
}
up_commit_bullpens <- function(df, job_dir, m, prog, dry = FALSE) {
  if (is.null(df) || !nrow(df)) stop("nothing to commit: no staged rows survive the exclusions")
  overwrite <- !isFALSE(m$opts$overwrite)
  prog(0.05, "Checking which pitches Supabase already has")
  existing <- tryCatch(up_rest_existing("pitches", "play_id", df$play_id),
                       error = function(e) { up_log(job_dir, "could not check existing rows: ", conditionMessage(e)); character(0) })
  n_exist <- sum(df$play_id %in% existing)
  if (!overwrite && n_exist) { df <- df[!df$play_id %in% existing, , drop = FALSE]; up_log(job_dir, "skipping ", n_exist, " row(s) already in public.pitches (overwrite off)") }
  if (!nrow(df)) stop("nothing to commit: every staged pitch is already in public.pitches (turn overwrite on to replace them)")
  rows <- up_bullpen_rows(df); sess <- up_session_rows(df, "pitcher")
  notes <- sprintf("%d new pitch(es), %d already present%s, %d session(s)", nrow(df) - if (overwrite) n_exist else 0L,
                   n_exist, if (overwrite) " (replaced)" else " (skipped)", length(sess))
  if (dry) { up_log(job_dir, "dry run: would upsert ", nrow(df), " rows into public.pitches; ", notes)
    return(list(n_rows = nrow(df), n_sessions = length(sess), target = "public.pitches (dry run, nothing written)", notes = notes)) }
  n <- up_rest_upsert("pitches", rows, "play_id", job_dir = job_dir, progress = function(p, msg) prog(0.1 + 0.8 * p, msg))
  prog(0.92, "Updating public.sessions")
  up_rest_upsert("sessions", sess, "session_id", job_dir = job_dir)
  up_log(job_dir, "public.pitches: ", n, " rows upserted; ", notes)
  list(n_rows = n, n_sessions = length(sess), target = "Supabase public.pitches", notes = notes)
}

UP_BP_COLS <- c("play_id", "session_id", "session_date", "time", "pitch_no", "batter", "batter_id", "batter_side",
                "pitcher", "pitcher_id", "pitcher_throws", "tagged_pitch_type", "rel_speed", "spin_rate",
                "induced_vert_break", "horz_break", "plate_loc_side", "plate_loc_height", "vert_appr_angle",
                "hit_uid", "exit_speed", "launch_angle", "direction", "hit_spin_rate", "distance", "bearing", "hang_time",
                "contact_position_x", "contact_position_y", "contact_position_z", "last_tracked_distance",
                "session_type", "external_session_id")
UP_BP_PREFIX <- "practice/hitting"
UP_BP_INDEX  <- "practice/hitting/index.json"
up_bp_index <- function() {
  x <- tryCatch(up_storage_read_json(UP_BP_INDEX, simplify = FALSE), error = function(e) NULL)
  if (is.null(x) || !is.list(x$sessions)) list(version = 1L, updated_at = NULL, sessions = list()) else x
}
up_rest_table_exists <- function(table) {
  resp <- up_rest_req(table) |> httr2::req_url_query(select = "*", limit = 1) |> httr2::req_perform()
  st <- httr2::resp_status(resp)
  if (st %in% c(200L, 206L)) return(TRUE)
  body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  if (st == 404L || grepl("PGRST205|does not exist", body)) return(FALSE)
  stop(sprintf("Supabase REST %s: HTTP %d %s", table, st, substr(body, 1, 200)))
}
up_commit_bp <- function(df, job_dir, m, prog, dry = FALSE) {
  if (is.null(df) || !nrow(df)) stop("nothing to commit: no staged rows survive the exclusions")
  for (cn in setdiff(UP_BP_COLS, names(df))) df[[cn]] <- NA
  out <- df[, UP_BP_COLS, drop = FALSE]
  for (cn in intersect(UP_PRACTICE_NUM, names(out))) out[[cn]] <- suppressWarnings(as.numeric(out[[cn]]))
  out$pitch_no <- suppressWarnings(as.integer(out$pitch_no))
  out$upload_id <- m$id; out$uploaded_at <- up_utc_now()
  sp <- split(out, out$session_id)
  idx <- up_bp_index(); entries <- idx$sessions
  key_of <- function(e) e$session_id %||% ""
  i <- 0L
  for (sid in names(sp)) {
    i <- i + 1L; s <- sp[[sid]]
    path <- sprintf("%s/%s_%s.parquet", UP_BP_PREFIX, as.character(s$session_date[1]), sid)
    prog(0.05 + 0.6 * (i - 1) / length(sp), sprintf("Writing session %d of %d to Storage", i, length(sp)))
    if (!dry) up_storage_put_df(s, path)
    who <- s$batter[!is.na(s$batter) & nzchar(s$batter)]
    entry <- list(session_id = sid, date = as.character(s$session_date[1]), file = path, n_rows = nrow(s),
                  n_bbe = sum(!is.na(s$exit_speed)), batters = paste(unique(who), collapse = ", "),
                  upload_id = m$id, uploaded_at = up_utc_now())
    j <- which(vapply(entries, key_of, character(1)) == sid)
    if (length(j)) entries[[j[1]]] <- entry else entries[[length(entries) + 1]] <- entry
    up_log(job_dir, if (dry) "dry run: would write " else "wrote ", path, " (", nrow(s), " rows)")
  }
  if (!dry) up_storage_put_json(list(version = 1L, updated_at = up_utc_now(), sessions = unname(entries)), UP_BP_INDEX)
  notes <- sprintf("%d session(s), %d swing(s), %d with batted-ball tracking", length(sp), nrow(out), sum(!is.na(out$exit_speed)))
  target <- "Storage practice/hitting/"
  prog(0.7, "Checking public.batting_practice")
  has_tbl <- tryCatch(up_rest_table_exists("batting_practice"), error = function(e) { up_log(job_dir, conditionMessage(e)); FALSE })
  if (has_tbl) {
    if (!dry) {
      out$updated_at <- up_utc_now()
      up_rest_upsert("batting_practice", up_df_rows(out), "play_id", job_dir = job_dir, progress = function(p, msg) prog(0.7 + 0.28 * p, msg))
    }
    target <- paste0(target, " + public.batting_practice")
  } else {
    notes <- paste0(notes, ". public.batting_practice does not exist yet: run supabase/migrations/20260914_batting_practice.sql in the Supabase SQL editor to mirror BP rows into Postgres (Storage is the app's source either way).")
    up_log(job_dir, "public.batting_practice not found; Storage only")
  }
  if (dry) target <- paste0(target, " (dry run, nothing written)")
  list(n_rows = nrow(out), n_sessions = length(sp), target = target, notes = notes)
}

up_commit_games <- function(df, job_dir, m, prog, dry = FALSE) {
  if (is.null(df) || !nrow(df)) stop("nothing to commit: no staged rows survive the exclusions")
  if (!dry && !up_pg_enabled()) stop("Supabase credentials missing: set SB_DB_HOST, SB_DB_USER, SB_DB_PASS (game uploads write pitchprofiler.* through Postgres)")
  staged_path <- file.path(job_dir, "commit_master.parquet"); arrow::write_parquet(df, staged_path)
  prog(0.05, "Scoring with Pitch Profiler (reclassify + stuff/location/xstats)")
  out <- up_score_pitches(staged_path, file.path(job_dir, "scored"), job_dir, workers = 2L)
  pitches <- as.data.frame(arrow::read_parquet(file.path(out, "pitches.parquet")))
  n_games <- length(unique(pitches$game_id))
  note_season <- "Season tables (pitchprofiler.pitcher_season*) and the Storage reference pools are not refreshed by an upload: rerun scripts/process_master.py + scripts/upload_supabase.R on the full master, then scripts/build_reference_pools.R."
  if (dry) return(list(n_rows = nrow(pitches), n_sessions = n_games, target = "pitchprofiler.pitches (dry run, scored only)", notes = note_season))
  h <- up_pg_holder(); on.exit(h$close(), add = TRUE)
  prog(0.5, "Writing pitchprofiler.pitches")
  n <- up_pg_replace(h, "pitchprofiler", "pitches", pitches, keys = "pitch_uid", job_dir = job_dir, overwrite = !isFALSE(m$opts$overwrite))
  for (t in c("pitcher_game", "pitcher_game_pitch_type")) {
    f <- file.path(out, paste0(t, ".parquet")); if (!file.exists(f)) next
    prog(0.85, paste("Writing pitchprofiler.", t))
    d <- as.data.frame(arrow::read_parquet(f))
    up_pg_replace(h, "pitchprofiler", t, d, keys = "game_id", job_dir = job_dir, overwrite = TRUE)
  }
  list(n_rows = n, n_sessions = n_games, target = "Supabase pitchprofiler.pitches + per-game aggregates",
       notes = paste0(n, " scored pitches from ", n_games, " game(s). ", note_season))
}

up_commit_trumedia <- function(job_dir, m, prog, dry = FALSE) {
  if (!all(nzchar(Sys.getenv(c("TM_USERNAME", "TM_SITENAME", "TM_MASTER_TOKEN")))))
    stop("TruMedia credentials missing: set TM_USERNAME, TM_SITENAME and TM_MASTER_TOKEN")
  extra <- trimws(m$opts$lb_args %||% "")
  args <- c("scripts/build_leaderboards.R", if (nzchar(extra)) strsplit(extra, "\\s+")[[1]])
  if (dry) return(list(n_rows = NA, n_sessions = NA, target = "Storage lb/<season>/ (dry run)", notes = paste("would run: Rscript", paste(args, collapse = " "))))
  prog(0.1, "Running scripts/build_leaderboards.R (this takes a while)")
  log_file <- file.path(job_dir, "leaderboards.log")
  res <- processx::run(file.path(R.home("bin"), "Rscript"), args, error_on_status = FALSE, stdout = log_file, stderr = "2>&1", timeout = 3 * 3600)
  tail_txt <- if (file.exists(log_file)) paste(utils::tail(readLines(log_file, warn = FALSE), 10), collapse = " | ") else ""
  if (res$status != 0) stop("build_leaderboards.R failed (exit ", res$status, "): ", tail_txt)
  list(n_rows = NA, n_sessions = NA, target = "Storage lb/<season>/", notes = tail_txt)
}

up_archive_job <- function(job_dir, m) {
  files <- c("staged.parquet", "committed.parquet", "sessions.csv", "issues.json", "edits.json", "log.txt", "meta.json")
  for (f in files) {
    p <- file.path(job_dir, f); if (!file.exists(p)) next
    ct <- if (grepl("json$", f)) "application/json" else if (grepl("csv$|txt$", f)) "text/plain" else "application/octet-stream"
    tryCatch(up_storage_upload(p, sprintf("uploads/%s/%s", m$id, f), ct), error = function(e) up_log(job_dir, "archive of ", f, " failed: ", conditionMessage(e)))
  }
}

up_commit <- function(job_dir) {
  m <- up_job_meta(job_dir); if (is.null(m)) stop("job not found: ", job_dir)
  dry <- isTRUE(m$opts$dry_run)
  up_job_update(job_dir, status = "committing", commit_started = up_now(), error = NULL, hint = NULL)
  prog <- function(p, msg) up_progress(job_dir, p, msg, "commit")
  up_log(job_dir, if (dry) "dry-run commit " else "commit ", m$type, " by ", m$user %||% "?")
  df <- NULL
  if (!identical(m$type, "trumedia")) {
    df <- up_commit_frame(job_dir)
    if (!is.null(df)) arrow::write_parquet(df, file.path(job_dir, "committed.parquet"))
    up_log(job_dir, NROW(df), " row(s) after review edits")
  }
  res <- switch(m$type,
    bullpens = up_commit_bullpens(df, job_dir, m, prog, dry),
    bp       = up_commit_bp(df, job_dir, m, prog, dry),
    games    = up_commit_games(df, job_dir, m, prog, dry),
    trumedia = up_commit_trumedia(job_dir, m, prog, dry),
    stop("unknown upload type ", m$type))
  entry <- list(id = m$id, type = m$type, user = m$user, season_label = m$season_label, from = m$from, to = m$to,
                created = m$created, committed_at = up_now(), n_sessions = res$n_sessions, n_rows = res$n_rows,
                status = if (dry) "dry run" else "committed", target = res$target, notes = res$notes)
  if (!dry) {
    prog(0.97, "Archiving the job to Storage uploads/")
    up_archive_job(job_dir, m)
    tryCatch(up_log_append(entry), error = function(e) up_log(job_dir, "upload log not updated: ", conditionMessage(e)))
  }
  up_job_update(job_dir, status = if (dry) "dry_run_done" else "committed", committed_at = up_now(), result = res)
  prog(1, if (dry) "Dry run finished; nothing was written." else sprintf("Committed to %s", res$target))
  up_log(job_dir, "done: ", res$target, " - ", res$notes)
  invisible(res)
}

# ---- completeness: what TrackMan has vs what Supabase has ------------------------------
up_completeness <- function(type, from, to, scope = "all") {
  spec <- UPLOAD_TYPES[[type]]; kind <- spec$session_kind
  if (identical(type, "trumedia")) stop("Completeness applies to TrackMan session types")
  if (identical(type, "bp")) sess <- up_bp_discover(from, to, scope) else {
    sess <- up_tm_discover(kind, from, to, session_type = if (kind == "practice") spec$practice_type else "All")
    if (kind == "practice" && nrow(sess) && "sessionType" %in% names(sess))
      sess <- sess[is.na(sess$sessionType) | sess$sessionType == spec$practice_type, , drop = FALSE]
    if (kind == "game") sess <- up_scope_filter(sess, scope)
  }
  have <- switch(type,
    bullpens = {
      d <- up_rest_select("pitches", select = "session_id", filters = list(and = sprintf("(session_date.gte.%s,session_date.lte.%s)", as.Date(from), as.Date(to))))
      if (nrow(d)) table(d$session_id) else table(character(0))
    },
    bp = {
      e <- up_bp_index()$sessions
      ids <- vapply(e, function(x) x$session_id %||% "", character(1)); n <- vapply(e, function(x) as.integer(x$n_rows %||% 0), integer(1))
      stats::setNames(n, ids)
    },
    games = {
      if (!up_pg_enabled()) NULL else {
        con <- up_pg_connect(2L); on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
        d <- DBI::dbGetQuery(con, "select game_id, count(*) as n from pitchprofiler.pitches where game_date between $1 and $2 group by 1",
                             params = list(as.character(as.Date(from)), as.character(as.Date(to))))
        stats::setNames(d$n, d$game_id)
      }
    })
  ids <- if (nrow(sess)) sess$sessionId else character(0)
  in_sb <- if (is.null(have)) rep(NA, length(ids)) else ids %in% names(have)
  rows  <- if (is.null(have)) rep(NA_integer_, length(ids)) else as.integer(ifelse(in_sb, have[ids], 0L))
  what <- if (!nrow(sess)) character(0) else if (kind == "game") paste(sess[["awayTeam.name"]] %||% "", "at", sess[["homeTeam.name"]] %||% "") else as.character(sess$sessionType %||% spec$practice_type)
  sessions <- data.frame(Date = if (nrow(sess)) as.character(sess$date) else character(0), Session = ids, What = what,
                         `In Supabase` = ifelse(is.na(in_sb), "unknown (no SB_DB_* here)", ifelse(in_sb, "yes", "MISSING")),
                         `Rows in Supabase` = rows, check.names = FALSE, stringsAsFactors = FALSE)
  by_date <- if (nrow(sessions)) {
    sp <- split(sessions, sessions$Date)
    do.call(rbind, lapply(names(sp), function(d) data.frame(Date = d, `TrackMan sessions` = nrow(sp[[d]]),
      Uploaded = sum(sp[[d]]$`In Supabase` == "yes"), Missing = sum(sp[[d]]$`In Supabase` == "MISSING"), check.names = FALSE, stringsAsFactors = FALSE)))
  } else data.frame(Date = character(0), `TrackMan sessions` = integer(0), Uploaded = integer(0), Missing = integer(0), check.names = FALSE)
  extra_sb <- if (!is.null(have)) setdiff(names(have), ids) else character(0)
  list(sessions = sessions, by_date = by_date, n_trackman = length(ids), n_missing = sum(sessions$`In Supabase` == "MISSING"),
       n_extra = length(extra_sb), checked_at = up_now(), from = as.character(as.Date(from)), to = as.character(as.Date(to)), type = type)
}

# ---- batting practice from the GAME endpoint -------------------------------------------
# Coastal's BP is tracked as game-endpoint sessions with sessionType
# "BattingPractice" (the practice endpoint's "Hitting" type is the other
# source). Map master-schema (dotted) rows onto the BP snake_case columns.
UP_BP_FROM_GAME <- c(
  play_id = "playID", session_id = "sessionId", session_date = "date", time = "localDateTime", pitch_no = "taggerBehavior.pitchNo",
  batter = "batter.name", batter_id = "batter.id", batter_side = "batter.side", pitcher = "pitcher.name", pitcher_id = "pitcher.id",
  pitcher_throws = "pitcher.throws", tagged_pitch_type = "pitchTag.taggedPitchType", rel_speed = "pitch.release.relSpeed",
  spin_rate = "pitch.release.spinRate", induced_vert_break = "pitch.movement.inducedVertBreak", horz_break = "pitch.movement.horzBreak",
  plate_loc_side = "pitch.location.plateLocSide", plate_loc_height = "pitch.location.plateLocHeight", vert_appr_angle = "pitch.location.vertApprAngle",
  hit_uid = "pitchUID", exit_speed = "hit.launch.exitSpeed", launch_angle = "hit.launch.angle", direction = "hit.launch.direction",
  hit_spin_rate = "hit.launch.hitSpinRate", distance = "hit.landingFlat.distance", bearing = "hit.landingFlat.bearing",
  hang_time = "hit.landingFlat.hangTime", contact_position_x = "hit.launch.contactPosition.x", contact_position_y = "hit.launch.contactPosition.y",
  contact_position_z = "hit.launch.contactPosition.z", last_tracked_distance = "hit.lastTrackedDistance", session_type = "sessionType",
  external_session_id = "externalSessionId")
up_bp_from_game <- function(g) {
  if (is.null(g) || !nrow(g)) return(NULL)
  out <- lapply(names(UP_BP_FROM_GAME), function(nm) { src <- UP_BP_FROM_GAME[[nm]]; if (src %in% names(g)) g[[src]] else rep(NA, nrow(g)) })
  names(out) <- names(UP_BP_FROM_GAME)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  for (cn in intersect(UP_PRACTICE_NUM, names(out))) out[[cn]] <- suppressWarnings(as.numeric(out[[cn]]))
  out$pitch_no <- suppressWarnings(as.integer(out$pitch_no))
  for (cn in setdiff(names(out), c(UP_PRACTICE_NUM, "pitch_no"))) out[[cn]] <- as.character(out[[cn]])
  out$session_date <- substr(out$session_date, 1, 10)
  out$batter[is.na(out$batter) | !nzchar(out$batter)] <- "Unknown"
  out$has_edger <- FALSE; out$raw_json <- NA_character_
  out[order(out$pitch_no), , drop = FALSE]
}
# Both BP sources for a window, one frame (source column: practice | game).
up_bp_discover <- function(from, to, scope = "all", job_dir = NULL, progress = NULL) {
  sp <- up_tm_discover("practice", from, to, session_type = "Hitting", job_dir = job_dir, progress = progress)
  if (nrow(sp) && "sessionType" %in% names(sp)) sp <- sp[is.na(sp$sessionType) | sp$sessionType == "Hitting", , drop = FALSE]
  if (nrow(sp)) sp$source <- "practice"
  sg <- up_tm_discover("game", from, to, session_type = "All", job_dir = job_dir, progress = progress)
  if (nrow(sg) && "sessionType" %in% names(sg)) sg <- sg[!is.na(sg$sessionType) & sg$sessionType == "BattingPractice", , drop = FALSE] else sg <- up_empty_sessions()
  if (nrow(sg)) { sg <- up_scope_filter(sg, scope); if (nrow(sg)) sg$source <- "game" }
  d <- up_bind_chr(list(if (nrow(sp)) sp, if (nrow(sg)) sg))
  if (is.null(d) || !nrow(d)) return(cbind(up_empty_sessions(), source = character(0)))
  d$date <- as.Date(d$date); d <- d[!duplicated(d$sessionId), , drop = FALSE]
  d[order(d$date, d$sessionId), , drop = FALSE]
}
