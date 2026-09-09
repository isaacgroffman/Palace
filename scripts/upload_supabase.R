#!/usr/bin/env Rscript
# =============================================================================
# Upload the tables built by scripts/process_master.py to Supabase (Postgres).
#
#   Rscript scripts/upload_supabase.R serving_build/master_2026            # all tables
#   Rscript scripts/upload_supabase.R serving_build/master_2026 pitcher_season pitcher_game
#   Rscript scripts/upload_supabase.R serving_build/master_2026 --resume pitches   # continue an interrupted load
#
# Credentials, same names Bullpen Central uses (R/supabase.R there), but this
# needs a role that can CREATE / INSERT (bullpen_reader is SELECT-only):
#   SB_DB_HOST   e.g. aws-1-us-east-1.pooler.supabase.com
#   SB_DB_USER   e.g. postgres.<projectref>
#   SB_DB_PASS
#   SB_DB_PORT   optional, default 5432 (session pooler; bulk COPY needs a
#                session, not the 6543 transaction pooler)
#
# What it does: runs schema.sql (drop + create in schema `pitchprofiler`), then
# COPYs each parquet in. pitches.parquet (2.6M rows x ~200 cols) goes up in 20k-row
# batches (the pooler drops larger COPYs); aggregate tables in 50k-row batches.
# =============================================================================
suppressPackageStartupMessages({ library(DBI); library(RPostgres); library(arrow) })

args   <- commandArgs(trailingOnly = TRUE)
resume_flag <- "--resume" %in% args
if (length(args) < 1) stop("usage: upload_supabase.R <out dir> [table ...]")
dir    <- args[1]
args   <- args[args != "--resume"]
want   <- if (length(args) > 1) args[-1] else c("pitcher_season", "pitcher_season_pitch_type",
                                                 "pitcher_game", "pitcher_game_pitch_type", "pitches")
if (!file.exists(file.path(dir, "schema.sql"))) stop("no schema.sql in ", dir, " - run process_master.py first")

host <- Sys.getenv("SB_DB_HOST"); user <- Sys.getenv("SB_DB_USER"); pass <- Sys.getenv("SB_DB_PASS")
port <- as.integer(Sys.getenv("SB_DB_PORT", "5432"))
if (!nzchar(host) || !nzchar(user) || !nzchar(pass)) stop("set SB_DB_HOST, SB_DB_USER, SB_DB_PASS")

connect <- function() dbConnect(RPostgres::Postgres(), host = host, port = port, dbname = "postgres",
                                 user = user, password = pass, sslmode = "require")
con <- connect()
on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)

# The Supabase pooler occasionally drops a session mid-COPY. Every statement /
# batch goes through this: on failure, reconnect and retry (up to 6 times).
with_retry <- function(what, fn) {
  for (attempt in 1:6) {
    ok <- tryCatch({ fn(); TRUE }, error = function(e) {
      cat(sprintf("[upload] %s failed (attempt %d): %s\n", what, attempt, substr(conditionMessage(e), 1, 120)))
      FALSE
    })
    if (ok) return(invisible(TRUE))
    Sys.sleep(5 * attempt)
    try(dbDisconnect(con), silent = TRUE)
    # the reconnect itself can be refused while the pooler recovers
    for (c_try in 1:10) {
      new_con <- tryCatch(connect(), error = function(e) NULL)
      if (!is.null(new_con)) break
      cat(sprintf("[upload] reconnect failed (%d), waiting\n", c_try)); Sys.sleep(10 * c_try)
    }
    if (is.null(new_con)) stop("[upload] could not reconnect to the database")
    con <<- new_con
  }
  stop("[upload] giving up on ", what)
}
resume <- resume_flag

# ---- schema: run only the statements for the requested tables ---------------
ddl <- readLines(file.path(dir, "schema.sql"))
stmts <- strsplit(paste(ddl[!grepl("^--", ddl)], collapse = "\n"), ";\\s*\n")[[1]]
stmts <- trimws(stmts); stmts <- stmts[nzchar(stmts)]
keep <- vapply(stmts, function(s) {
  grepl("create schema", s) || any(vapply(want, function(t) grepl(paste0("pitchprofiler\\.", t, "\\b"), s), logical(1)))
}, logical(1))
if (resume) {
  cat("[upload] --resume: keeping existing tables\n")
} else {
  for (s in stmts[keep]) with_retry(substr(s, 1, 40), function() dbExecute(con, s))
}
cat("[upload] schema ready for:", paste(want, collapse = ", "), "\n")

# ---- data -------------------------------------------------------------------
to_pg_frame <- function(df) {
  # RPostgres handles logical/integer/double/character/Date natively; arrow
  # types that are not one of those are coerced so COPY never chokes.
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "integer64")) df[[nm]] <- as.numeric(x)
    else if (is.list(x)) df[[nm]] <- vapply(x, function(v) paste(v, collapse = "|"), character(1))
    else if (inherits(x, "POSIXt")) df[[nm]] <- format(x, "%Y-%m-%d %H:%M:%S%z")
  }
  df
}

for (tbl in want) {
  f <- file.path(dir, paste0(tbl, ".parquet"))
  if (!file.exists(f)) { cat("[upload] skip", tbl, "(no parquet)\n"); next }
  target <- DBI::Id(schema = "pitchprofiler", table = tbl)
  existing <- if (resume) as.numeric(dbGetQuery(con, sprintf("select count(*) from pitchprofiler.%s", tbl))[[1]]) else 0
  if (tbl == "pitches") {
    # rows are appended in file order, so the count already loaded is the offset to skip
    pf <- arrow::ParquetFileReader$create(f)
    n_rg <- pf$num_row_groups; total <- 0; step <- 5000L
    if (existing > 0) cat(sprintf("[upload] pitches: resuming after %d rows\n", existing))
    for (i in seq_len(n_rg) - 1) {
      n_i <- pf$ReadRowGroup(i, column_indices = 0L)$num_rows
      if (total + n_i <= existing) { total <- total + n_i; next }
      chunk <- as.data.frame(pf$ReadRowGroup(i))
      for (start in seq(1, nrow(chunk), by = step)) {
        end <- min(start + step - 1L, nrow(chunk))
        if (total + (end - start + 1L) <= existing) { total <- total + (end - start + 1L); next }
        skip <- max(0L, existing - total)
        part <- to_pg_frame(chunk[(start + skip):end, , drop = FALSE])
        with_retry(sprintf("pitches batch @%d", total), function() dbAppendTable(con, target, part))
        total <- total + (end - start + 1L)
        cat(sprintf("[upload] pitches: %d rows\n", total))
      }
      rm(chunk); gc()
    }
  } else {
    if (resume && existing > 0) { cat(sprintf("[upload] %s: already has %d rows, skipping\n", tbl, existing)); next }
    df <- to_pg_frame(as.data.frame(arrow::read_parquet(f)))
    step <- 50000L
    for (start in seq(1, nrow(df), by = step)) {
      part <- df[start:min(start + step - 1L, nrow(df)), , drop = FALSE]
      with_retry(sprintf("%s batch @%d", tbl, start), function() dbAppendTable(con, target, part))
    }
    cat(sprintf("[upload] %s: %d rows\n", tbl, nrow(df)))
  }
}
cat("[upload] done\n")
