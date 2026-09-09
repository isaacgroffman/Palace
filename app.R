# =============================================================================
# Palace — Coastal Carolina Baseball analytics (Shiny)
#
# This file only wires things together. Everything else lives in R/:
#   R/00_config.R ... R/20_pitch_arsenal.R   global code, sourced IN ORDER
#   R/ui.R is inside 19_ui.R (app_ui); R/server.R sources R/server/*.R
#
# Data: every pitch comes from Supabase (R/palace_supabase.R, R/07_pools.R).
# =============================================================================
if (!nzchar(Sys.getenv("RETICULATE_PYTHON")))
  Sys.setenv(RETICULATE_PYTHON = "/usr/bin/python3")

library(shiny)
library(DT)
library(dplyr)
library(magrittr)   # `%>%` explicitly: newer dplyr builds on Connect no longer re-export it
library(scales)
library(readr)
library(tidyverse)
library(bslib)
library(htmltools)
library(shinyWidgets)
library(ggplot2)
library(grid)
library(magick)
library(arrow)
library(gt)
library(gtExtras)
library(lubridate)
library(ggimage)
library(plotly)
library(purrr)
library(tidyr)
library(ggforce)
library(httr)
library(reticulate)
library(openxlsx)
library(pool)
library(RPostgres)
library(DBI)
library(httr2)
# NOTE: tidymodels / parsnip / recipes / xgboost were attached in the old
# monolith but nothing in the app calls them (grades come from the Python
# Pitch Profiler bundle). Dropped: faster attach, no more infer::observe masking.

source("R/00_config.R",          local = TRUE)
source("R/palace_supabase.R",    local = TRUE)
source("R/palace_video.R",       local = TRUE)
source("R/palace_bullpen.R",     local = TRUE)
source("R/palace_artifacts.R", local = TRUE)
for (f in sort(list.files("R", pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE)))
  if (basename(f) != "00_config.R") source(f, local = TRUE)
source("R/server.R", local = TRUE)

ui <- fluidPage(
  uiOutput("page")
)

shinyApp(ui, server)


