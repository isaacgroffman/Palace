# Contributing to Palace

## Where things go
- New global helper → the numbered `R/NN_*.R` file that matches its area. It may only
  reference things defined in a lower-numbered file (files are sourced in order).
- New server logic → the matching `R/server/NN_*.R` chunk. Chunks share one environment;
  use a prefix for your reactive/output ids (`pp_`, `hp_`, `mm_`, `lb_`, ...) like the rest.
- New data source → `R/palace_supabase.R` (Supabase) or `R/03_data_access.R` (repo files). Nothing else reads data directly.
- Anything computed from the big pools → lazy (`delayedAssign`) in `R/07_pools.R`, or precomputed by
  `scripts/process_master.py` into a Supabase table. Do not add work to boot.

## Before you push
```
Rscript scripts/check_sources.R      # every file parses
Rscript -e 'shiny::runApp(".")'      # boots against Supabase (SB_DB_* in .env / ~/.Renviron)
```
Note the boot log: `[pools]` and `[pitchprofiler]` lines are the Supabase reads; nothing else should take time.

## Rules
- Surgical edits over rewrites. Keep the original code shape; move code, do not restyle it.
- No secrets, no `.parquet`/`.csv` in git (only `reference/`).
- Flag data-quality issues in the log (`cat("WARNING: ...")`) rather than fixing them silently.
