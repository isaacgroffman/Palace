# Contributing to Palace

## Where things go
- New global helper → the numbered `R/NN_*.R` file that matches its area. It may only
  reference things defined in a lower-numbered file (files are sourced in order).
- New server logic → the matching `R/server/NN_*.R` chunk. Chunks share one environment;
  use a prefix for your reactive/output ids (`pp_`, `hp_`, `mm_`, `lb_`, ...) like the rest.
- New data source → `R/03_data_access.R` only. Nothing else reads files or repos directly.
- Anything that has to be computed from the big pools → `scripts/build_serving_data.R`,
  saved as an artifact, read in `R/07_pools.R`. Do not add work to boot.

## Before you push
```
Rscript scripts/check_sources.R      # every file parses
Rscript -e 'shiny::runApp(".")'      # boots in serve mode with PALACE_SERVING_DIR or the HF serving repo
```
Note the boot log: every `[serve]` line is a pool/artifact read; nothing else should take time.

## Rules
- Surgical edits over rewrites. Keep the original code shape; move code, do not restyle it.
- No secrets, no `.parquet`/`.csv` in git (only `reference/`).
- Flag data-quality issues in the log (`cat("WARNING: ...")`) rather than fixing them silently.
