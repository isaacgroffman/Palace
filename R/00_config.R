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

# All pitch data comes from Supabase (R/palace_supabase.R). The 2026 reference
# pools are pulled lazily on first use; PALACE_PREWARM=TRUE forces them at boot.
PALACE_PREWARM <- isTRUE(as.logical(Sys.getenv("PALACE_PREWARM", unset = "FALSE")))
cat("[Palace] data: Supabase 2026\n")
