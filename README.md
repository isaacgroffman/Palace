# Palace — Coastal Carolina Baseball analytics

Shiny app (R + a Python Pitch Profiler bundle via reticulate) for CCU baseball:
pitcher/hitter player pages, bullpens, NCAA scouting, league leaderboards,
matchup matrices, advance reports.

## Layout

```
app.R                     libraries + source() wiring only (60 lines). Start here.
R/
  00_config.R             env vars, data-mode switch
  palace_supabase.R       Supabase pool (bullpen/practice data)
  palace_video.R          Edgertronic video URLs (TrackMan media API)
  palace_bullpen.R        Bullpen tab module
  01_reference.R          team/league/conference lookups
  02_trumedia_api.R       TruMedia API client + pitch/bio helpers
  03_data_access.R        Hugging Face cache layer + the SERVING LAYER (only file that knows where data lives)
  04_players_roster.R     Coastal bios, 2027 roster, Supabase bullpen pitcher list
  05_promodel.R           Pitch Profiler (proStuff+/proPitching+/proLocation+) wrapper
  06_pitch_processing.R   process_pitcher_data(), indicators, P5 percentile maps, movement helpers
  07_pools.R              the pitch frames the app reads — serve (precomputed) vs build (legacy) branch
  08_ncaa_data.R          NCAA datasets (lazy arrow) + pitcher directory
  09_leaderboard.R        league leaderboards (TruMedia)
  10_ncaa_bio.R           DRS26 / TruMedia bio + headshots
  11_ncaa_pitcher_loader.R  per-pitcher NCAA loads (cached)
  12_pitch_reclassification.R
  13_hitter_process_model.R
  14_hitter_profile.R
  15_ncaa_hitters.R
  16_player_headers.R
  17_viz_and_reports.R    charts, PDF/report builders
  18_matchup.R            matchup engine, Matchup+, unified matrix
  19_ui.R                 brand marks, UI helpers, app_ui
  20_pitch_arsenal.R
  server.R                server <- function(...) { source R/server/*.R with local = TRUE }
  server/01..14_*.R       server body, one chunk per feature area (same environment, order matters)
reference/                tiny static CSVs that ARE committed (roster, team map, leagues)
scripts/build_serving_data.R   produces the precomputed pools/artifacts (see "Data")
models/, pitchprofiler_models/, sample/, verify_install.py   Python Pitch Profiler bundle (unchanged)
raw/                      gitignored inputs for the build script
```

Global files are sourced in numeric order; a file may only use things defined
in files with a lower number. Server chunks are sourced alphabetically inside
`server()` and share one environment, so a reactive defined in `03_` is
visible in `09_`.

## Data

Nothing large is committed. `R/03_data_access.R` is the only place that knows
where data comes from:

| Source | What | Mode |
|---|---|---|
| `PALACE_SERVING_REPO` (HF dataset, default `CoastalBaseball/PalaceServing`) | `pools/*.parquet`, `artifacts/*.rds`, `files/*.csv` written by `scripts/build_serving_data.R` | serve |
| `CoastalBaseball/PitcherAppFiles`, `CoastalBaseball/2026MasterDataset`, `CoastalBaseball/AdvancePitcher` | raw TrackMan pools, NCAA pbp, bios | build (and NCAA per-pitcher loads in both modes) |
| Supabase | bullpen/practice pitches, players; `pitchprofiler.*` = every 2026 NCAA pitch reclassified + scored (see below) | both |
| TruMedia / TrackMan APIs | leaderboards, stats, video tokens | both |

### `PALACE_DATA_MODE`

- **`serve`** (target): boot reads eight small Coastal parquets. The P5/SBC
  reference pools (~520k rows), `full_data_p5_compare`, the P5 percentile maps,
  movement/slot grids and the NCAA directory are precomputed artifacts, and the
  heavy ones are `delayedAssign` promises — downloaded on first use, not at boot.
  `PALACE_PREWARM=TRUE` forces them at boot instead.
- **`build`** (default until you flip it): the original startup path — downloads
  every raw pool, runs `process_pitcher_data()` and Python scoring on all of
  them, binds, filters back. Slow, but it is also what the build script runs,
  so serve mode can never diverge from it.

### Scoring the full master TrackMan file

`scripts/process_master.py` runs `~/master_trackman_2026.parquet` (2.6M NCAA
pitches, TrackMan v3 API export) through the Pitch Profiler bundle: pitch-type
reclassification, per-pitch proStuff+/proPitching+/proLocation+, per-pitch
expected-stat terms, and four aggregate tables ready for Supabase
(`scripts/upload_supabase.R`). See `scripts/README_process_master.md`.

Once uploaded, the app reads those tables through `R/palace_supabase.R`
(`pp_pitcher_rows`, `pp_batter_rows`, `pp_team_rows`, `pp_directory`,
`pp_table`): NCAA pitcher pages, hitter pages and matchup pools take their
2026 rows from Supabase already reclassified and scored, so no Python scoring
runs per request; TruMedia and the parquet datasets stay as fallbacks.
`score_promodel()` only sends rows without a score to the bundle, and the
reclassifier skips pitcher-seasons that arrived typed. Needs `SB_DB_HOST`,
`SB_DB_USER`, `SB_DB_PASS` (read-only role is enough).

### Rebuilding the serving data

Whenever a raw pool changes (new season file, corrected bios, model update):

```
Rscript scripts/build_serving_data.R --upload
```

Local dry run: omit `--upload`, then run the app with
`PALACE_DATA_MODE=serve PALACE_SERVING_DIR=serving_build`.

Roadmap: Supabase replaces the HF serving repo. Only `palace_serving_file()`,
`pool_read()`, `artifact_read()` and `palace_file_candidates()` change.

## Running locally

```
cp .env.example .env      # fill in secrets; readRenviron(".env") or use dotenv
Rscript -e 'shiny::runApp(".")'
```

Python side: `python3 verify_install.py` must pass (LightGBM bundle reproduces
production numbers) before anything else is debugged.

## Deploying

Connect Cloud / HF Space needs: the env vars in `.env.example`, Python 3 with
`lightgbm==4.7.0 polars pandas numpy pyarrow` + `huggingface_hub`, and the
Python bundle directories present at the repo root. Set `PALACE_DATA_MODE=serve`
once a serving build has been uploaded.

## Changes from the HF Space monolith (`app.R` v87)

- Split into `R/` + `R/server/` files; **no logic changes** except those listed here.
- Removed ~650 lines of dead code: pre-Supabase bullpen helpers, the whole
  AWRE Exchange/HLS module (product removed 2026-08-31; still in git history of
  the Space), `college_re24/re288` loads, `TEAM_REPORT_COLS`, `LB_TM_SHAPE_TOKENS`,
  `PA_SHRINK_K`, `pitcher_bio_lookup`, the unused `selected_spray` reactive,
  `spraySBC_2025`/`sprayP5_2025`.
- Dropped `tidymodels`, `parsnip`, `recipes`, `xgboost` (unreferenced).
- `build_p5_slot_grid()` moved out of `server()` — it was rebuilt from the full
  P5 pool on every session.
- `get("full_data_p5_compare", envir = globalenv())` → `inherits = TRUE` in the
  pitch-arsenal code: the app env is not the global env under `runApp`, so the
  league pool was silently never found.
- Small CSVs moved to `reference/` (`merged_teams (2).csv` → `reference/merged_teams.csv`).
- `DRS26.csv` / `trumedia_player_bio_master.csv` resolve local-first, then
  `files/` in the serving repo, so they no longer need to be committed.
