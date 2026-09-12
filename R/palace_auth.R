# =============================================================================
# R/palace_auth.R — per-user logins.
#
# Users live in one JSON object in Supabase Storage (palace-serving,
# auth/users.json), managed from the app's Users panel (admins) or
# scripts/palace_users.R. Passwords are stored as bcrypt-pbkdf hashes with a
# per-user salt; a login sets a signed, expiring session cookie so a refresh,
# a closed laptop or a new tab lands straight in the app.
#
#   auth_login(username, password)   -> user record or NULL
#   auth_session_token(user)         -> cookie value (30 days)
#   auth_session_user(token)         -> user record or NULL (signature, expiry,
#                                       active flag and token_version checked)
#   auth_add_user / auth_set_password / auth_set_active / auth_remove_user /
#   auth_logout_everywhere            -> admin operations
#
# Break-glass: the legacy `password` env var still signs in as `admin`
# (role admin) so nobody is locked out before the first user exists; unset
# it once real accounts are in place.
# =============================================================================

.auth_env <- new.env(parent = emptyenv())
AUTH_USERS_PATH  <- "auth/users.json"
AUTH_COOKIE_DAYS <- 30
AUTH_ROLES       <- c("admin", "coach", "analyst", "player", "viewer")

auth_norm_user <- function(x) tolower(trimws(as.character(x %||% "")))
auth_valid_username <- function(x) grepl("^[a-z0-9][a-z0-9._-]{1,31}$", auth_norm_user(x))

# ---- secret + hashing ---------------------------------------------------------
auth_secret <- function() {
  s <- Sys.getenv("PALACE_SESSION_SECRET")
  if (!nzchar(s)) s <- paste0(Sys.getenv("SUPABASE_SECRET_KEY"), "|", Sys.getenv("password"))
  if (!nzchar(gsub("\\|", "", s))) s <- "palace-dev-secret"
  auth_hex(openssl::sha256(charToRaw(paste0("palace-session:v2:", s))))
}
auth_hex <- function(r) paste(as.character(r), collapse = "")
auth_new_salt <- function() auth_hex(openssl::rand_bytes(16))
openssl_hex2raw <- function(h) as.raw(strtoi(substring(h, seq(1, nchar(h), 2), seq(2, nchar(h), 2)), 16L))
auth_hash <- function(password, salt) {
  auth_hex(openssl::bcrypt_pbkdf(as.character(password), openssl_hex2raw(salt), rounds = 48L, size = 32L))
}
auth_hmac <- function(msg) auth_hex(openssl::sha256(charToRaw(msg), key = charToRaw(auth_secret())))
auth_same <- function(a, b) {
  a <- charToRaw(as.character(a)); b <- charToRaw(as.character(b))
  length(a) == length(b) && sum(bitwXor(as.integer(a), as.integer(b))) == 0
}

# ---- the users file ---------------------------------------------------------------
auth_users <- function(force = FALSE) {
  if (!force && !is.null(.auth_env$users) &&
      difftime(Sys.time(), .auth_env$at, units = "secs") < 60) return(.auth_env$users)
  users <- NULL
  if (sb_storage_enabled()) {
    dest <- tempfile(fileext = ".json")
    if (sb_storage_download(AUTH_USERS_PATH, dest, attempts = 2L)) {
      obj <- tryCatch(jsonlite::fromJSON(dest, simplifyVector = FALSE), error = function(e) NULL)
      unlink(dest)
      if (is.list(obj) && is.list(obj$users)) users <- obj$users
    }
    if (is.null(users)) users <- list()   # reachable but empty
  }
  .auth_env$users <- users; .auth_env$at <- Sys.time()
  users
}
auth_save_users <- function(users) {
  if (!sb_storage_enabled()) stop("Supabase Storage is not configured (SUPABASE_URL / SUPABASE_SECRET_KEY)")
  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(list(version = 1L, updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), users = users),
                       tmp, auto_unbox = TRUE, null = "null", pretty = TRUE)
  sb_storage_upload(tmp, AUTH_USERS_PATH, content_type = "application/json")
  unlink(tmp)
  .auth_env$users <- users; .auth_env$at <- Sys.time()
  invisible(users)
}
auth_find <- function(users, username) {
  u <- auth_norm_user(username)
  for (i in seq_along(users)) if (identical(auth_norm_user(users[[i]]$username), u)) return(i)
  NA_integer_
}
auth_public <- function(u) {
  list(username = u$username, display = u$display %||% u$username, role = u$role %||% "viewer",
       active = isTRUE(u$active %||% TRUE), token_version = as.integer(u$token_version %||% 1L),
       created = u$created %||% NA, last_login = u$last_login %||% NA, legacy = isTRUE(u$legacy))
}
auth_user_table <- function() {
  users <- auth_users()
  if (!length(users)) return(data.frame())
  do.call(rbind, lapply(users, function(u) data.frame(
    Username = u$username, Name = u$display %||% u$username, Role = u$role %||% "viewer",
    Active = isTRUE(u$active %||% TRUE), `Last login` = as.character(u$last_login %||% "never"),
    check.names = FALSE, stringsAsFactors = FALSE)))
}

# ---- login ------------------------------------------------------------------------
auth_legacy_admin <- function() {
  pw <- Sys.getenv("password")
  if (!nzchar(pw)) return(NULL)
  list(username = "admin", display = "Admin", role = "admin", active = TRUE, legacy = TRUE,
       token_version = as.integer(strtoi(substr(auth_hex(openssl::sha256(charToRaw(pw))), 1, 6), 16L)))
}

auth_login <- function(username, password) {
  username <- auth_norm_user(username); password <- as.character(password %||% "")
  if (!nzchar(username) || !nzchar(password)) return(NULL)
  users <- auth_users()
  i <- auth_find(users, username)
  if (!is.na(i)) {
    u <- users[[i]]
    if (!isTRUE(u$active %||% TRUE)) return(NULL)
    if (auth_same(auth_hash(password, u$salt), u$hash)) {
      users[[i]]$last_login <- format(Sys.time(), "%Y-%m-%d %H:%M")
      tryCatch(auth_save_users(users), error = function(e) NULL)
      return(auth_public(users[[i]]))
    }
    return(NULL)
  }
  # break-glass admin from the legacy env password
  la <- auth_legacy_admin()
  if (!is.null(la) && identical(username, "admin") && auth_same(password, Sys.getenv("password"))) return(la)
  NULL
}

# ---- session tokens ----------------------------------------------------------------
auth_session_token <- function(user, days = AUTH_COOKIE_DAYS) {
  exp <- as.integer(Sys.time()) + as.integer(days * 86400)
  body <- paste(user$username, exp, as.integer(user$token_version %||% 1L), sep = "|")
  paste(body, auth_hmac(body), sep = "|")
}
auth_session_user <- function(token) {
  token <- as.character(token %||% "")
  parts <- strsplit(token, "|", fixed = TRUE)[[1]]
  if (length(parts) != 4) return(NULL)
  body <- paste(parts[1:3], collapse = "|")
  if (!auth_same(auth_hmac(body), parts[4])) return(NULL)
  exp <- suppressWarnings(as.integer(parts[2]))
  if (!is.finite(exp) || exp < as.integer(Sys.time())) return(NULL)
  ver <- suppressWarnings(as.integer(parts[3]))
  users <- auth_users()
  i <- auth_find(users, parts[1])
  if (!is.na(i)) {
    u <- users[[i]]
    if (!isTRUE(u$active %||% TRUE) || !identical(as.integer(u$token_version %||% 1L), ver)) return(NULL)
    return(auth_public(u))
  }
  la <- auth_legacy_admin()
  if (!is.null(la) && identical(parts[1], "admin") && identical(la$token_version, ver)) return(la)
  NULL
}

# ---- admin operations ----------------------------------------------------------------
auth_add_user <- function(username, password, display = NULL, role = "viewer") {
  username <- auth_norm_user(username)
  if (!auth_valid_username(username)) stop("username: 2-32 chars, letters / digits / . _ -")
  if (nchar(password %||% "") < 8) stop("password must be at least 8 characters")
  if (!role %in% AUTH_ROLES) stop("role must be one of ", paste(AUTH_ROLES, collapse = ", "))
  users <- auth_users(force = TRUE)
  if (!is.na(auth_find(users, username))) stop("user '", username, "' already exists")
  salt <- auth_new_salt()
  users[[length(users) + 1]] <- list(
    username = username, display = if (nzchar(display %||% "")) display else username, role = role,
    salt = salt, hash = auth_hash(password, salt), active = TRUE, token_version = 1L,
    created = format(Sys.time(), "%Y-%m-%d"), last_login = NULL)
  auth_save_users(users)
  invisible(username)
}
auth_set_password <- function(username, password) {
  if (nchar(password %||% "") < 8) stop("password must be at least 8 characters")
  users <- auth_users(force = TRUE); i <- auth_find(users, username)
  if (is.na(i)) stop("no such user")
  users[[i]]$salt <- auth_new_salt(); users[[i]]$hash <- auth_hash(password, users[[i]]$salt)
  users[[i]]$token_version <- as.integer(users[[i]]$token_version %||% 1L) + 1L   # signs out every device
  auth_save_users(users); invisible(TRUE)
}
auth_set_role <- function(username, role) {
  if (!role %in% AUTH_ROLES) stop("bad role")
  users <- auth_users(force = TRUE); i <- auth_find(users, username)
  if (is.na(i)) stop("no such user")
  users[[i]]$role <- role; auth_save_users(users); invisible(TRUE)
}
auth_set_active <- function(username, active) {
  users <- auth_users(force = TRUE); i <- auth_find(users, username)
  if (is.na(i)) stop("no such user")
  users[[i]]$active <- isTRUE(active)
  if (!isTRUE(active)) users[[i]]$token_version <- as.integer(users[[i]]$token_version %||% 1L) + 1L
  auth_save_users(users); invisible(TRUE)
}
auth_remove_user <- function(username) {
  users <- auth_users(force = TRUE); i <- auth_find(users, username)
  if (is.na(i)) stop("no such user")
  auth_save_users(users[-i]); invisible(TRUE)
}
auth_logout_everywhere <- function(username) {
  users <- auth_users(force = TRUE); i <- auth_find(users, username)
  if (is.na(i)) stop("no such user")
  users[[i]]$token_version <- as.integer(users[[i]]$token_version %||% 1L) + 1L
  auth_save_users(users); invisible(TRUE)
}
auth_admin_count <- function() {
  users <- auth_users()
  sum(vapply(users, function(u) identical(u$role, "admin") && isTRUE(u$active %||% TRUE), logical(1)))
}
