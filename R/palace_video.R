# =============================================================================
# R/palace_video.R — Edgertronic video source for Palace's bullpen modal.
# (AWRE integration removed 2026-08-31; restore from git history.)
#
# Two exact sources, both keyed at ingest time and stored in Supabase:
#   * Edgertronic: blob paths -> SAS URL minted here at click time
#                  (TrackMan media API; tokens expire, paths don't)
#
# Env vars: TM_CLIENT_ID, TM_SECRET.
# =============================================================================

library(httr2)

.PV_TM_TOKEN_URL <- "https://login.trackmanbaseball.com/connect/token"
.PV_TM_API       <- "https://dataapi.trackmanbaseball.com/api/v1"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---- TrackMan auth (cached token) --------------------------------------------
pv_tm_token <- local({
  cache <- NULL
  function() {
    cid <- Sys.getenv("TM_CLIENT_ID"); sec <- Sys.getenv("TM_SECRET")
    if (!nzchar(cid) || !nzchar(sec)) return(NULL)
    if (!is.null(cache) && cache$expires_at > Sys.time() + 120)
      return(cache$access_token)
    resp <- tryCatch(
      request(.PV_TM_TOKEN_URL) |>
        req_body_form(client_id = cid, client_secret = sec,
                      grant_type = "client_credentials") |>
        req_perform() |> resp_body_json(),
      error = function(e) NULL)
    if (is.null(resp)) return(NULL)
    cache <<- list(access_token = resp$access_token,
                   expires_at = Sys.time() + as.numeric(resp$expires_in %||% 3600))
    cache$access_token
  }
})

# Session-level SAS token cache (tokens live for hours; don't re-mint per clip)
.pv_sas_cache <- new.env(parent = emptyenv())

pv_edger_sas <- function(session_id) {
  hit <- .pv_sas_cache[[session_id]]
  if (!is.null(hit) && difftime(Sys.time(), hit$fetched, units = "mins") < 20)
    return(hit$e)
  tok <- pv_tm_token()
  if (is.null(tok)) return(NULL)
  toks <- tryCatch(
    request(paste0(.PV_TM_API, "/media/practice/videotokens/", session_id)) |>
      req_auth_bearer_token(tok) |>
      req_retry(max_tries = 2) |>
      req_perform() |> resp_body_json(simplifyVector = FALSE),
    error = function(e) NULL)
  edger <- purrr::keep(toks %||% list(), ~ identical(.x$type, "EdgertronicVideos"))
  if (!length(edger)) return(NULL)
  e <- edger[[1]]
  .pv_sas_cache[[session_id]] <- list(e = e, fetched = Sys.time())
  e
}

# Blob path(s) -> fresh playable URLs. NULL when minting isn't possible.
pv_edger_urls <- function(session_id, blobs) {
  e <- pv_edger_sas(session_id)
  if (is.null(e)) return(NULL)
  vapply(blobs, function(blob) {
    enc <- paste(vapply(strsplit(blob, "/", fixed = TRUE)[[1]],
                        utils::URLencode, character(1), reserved = TRUE),
                 collapse = "/")
    sprintf("https://%s.blob.core.windows.net/%s/%s%s",
            e$entityPath, e$endpoint, enc, e$token)
  }, character(1), USE.NAMES = FALSE)
}
