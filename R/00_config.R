# =============================================================================
# CONFIG — env vars and shiny-verb guards. No data is loaded here.
# =============================================================================

# Keep the Shiny verbs pinned even if a later package masks them.
observe   <- shiny::observe
reactive  <- shiny::reactive
req       <- shiny::req
validate  <- shiny::validate
need      <- shiny::need

PASSWORD <- Sys.getenv("password")

# "serve" = read precomputed pools/artifacts (fast boot, the target state)
# "build" = rebuild everything from raw sources at boot (original behaviour;
#           also what scripts/build_serving_data.R runs)
PALACE_DATA_MODE    <- tolower(Sys.getenv("PALACE_DATA_MODE", unset = "build"))
PALACE_SERVING_REPO <- Sys.getenv("PALACE_SERVING_REPO", unset = "CoastalBaseball/PalaceServing")
PALACE_SERVING_DIR  <- Sys.getenv("PALACE_SERVING_DIR",  unset = "")   # local folder override
PALACE_PREWARM      <- isTRUE(as.logical(Sys.getenv("PALACE_PREWARM", unset = "FALSE")))

palace_serve_mode <- function() identical(PALACE_DATA_MODE, "serve")
cat("[Palace] data mode:", PALACE_DATA_MODE, "\n")
