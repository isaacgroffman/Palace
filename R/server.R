# =============================================================================
# SERVER — one function; its body is split into R/server/*.R, sourced here
# with local = TRUE so every chunk shares the same environment (input,
# output, session, and every reactive defined in an earlier chunk).
# Order matters: files are sourced alphabetically, so keep the NN_ prefix.
# =============================================================================
server <- function(input, output, session) {
  for (.f in sort(list.files("R/server", pattern = "\\.R$", full.names = TRUE)))
    source(.f, local = TRUE)
}
