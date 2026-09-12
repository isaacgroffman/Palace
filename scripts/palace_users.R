#!/usr/bin/env Rscript
# Manage Palace logins from the terminal (same store the app's Users panel
# edits: Supabase Storage, palace-serving/auth/users.json).
#
#   Rscript scripts/palace_users.R list
#   Rscript scripts/palace_users.R add <username> <password> [display name] [role]
#   Rscript scripts/palace_users.R passwd <username> <new password>
#   Rscript scripts/palace_users.R role <username> <admin|coach|analyst|player|viewer>
#   Rscript scripts/palace_users.R disable <username> | enable <username>
#   Rscript scripts/palace_users.R remove <username>
#
# Needs SUPABASE_URL and SUPABASE_SECRET_KEY in the environment (or ~/.Renviron).
suppressPackageStartupMessages(library(magrittr))
`%||%` <- function(a, b) if (is.null(a)) b else a
readRenviron("~/.Renviron")
source("R/palace_supabase.R"); source("R/palace_auth.R")
if (!sb_storage_enabled()) stop("set SUPABASE_URL and SUPABASE_SECRET_KEY")

args <- commandArgs(trailingOnly = TRUE)
cmd <- args[1] %||% "list"
a <- function(i) if (length(args) >= i) args[i] else NULL
switch(cmd,
  list    = { tb <- auth_user_table(); if (nrow(tb)) print(tb, row.names = FALSE) else cat("no users yet\n") },
  add     = { auth_add_user(a(2), a(3), a(4) %||% a(2), a(5) %||% "coach"); cat("added", auth_norm_user(a(2)), "\n") },
  passwd  = { auth_set_password(a(2), a(3)); cat("password reset for", a(2), "(signed out everywhere)\n") },
  role    = { auth_set_role(a(2), a(3)); cat(a(2), "is now", a(3), "\n") },
  disable = { auth_set_active(a(2), FALSE); cat("disabled", a(2), "\n") },
  enable  = { auth_set_active(a(2), TRUE); cat("enabled", a(2), "\n") },
  remove  = { auth_remove_user(a(2)); cat("removed", a(2), "\n") },
  stop("unknown command: ", cmd))
