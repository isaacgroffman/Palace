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
  03_data_access.R        pool_all() + local lookup-file resolver (Supabase readers live in palace_supabase.R)
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
scripts/process_master.py      scores the master TrackMan file -> Supabase tables (see "Data")
scripts/upload_supabase.R      loads them
models/, pitchprofiler_models/, sample/, verify_install.py   Python Pitch Profiler bundle (unchanged)
```

Global files are sourced in numeric order; a file may only use things defined
in files with a lower number. Server chunks are sourced alphabetically inside
`server()` and share one environment, so a reactive defined in `03_` is
visible in `09_`.

## Data

Everything comes from Supabase, read through `R/palace_supabase.R`. Nothing
is downloaded from anywhere else, and no 2025 data is used.

| Supabase table | What | Read when |
|---|---|---|
| `pitchprofiler.pitches` | every 2026 NCAA pitch (2.58M), reclassified + scored by the Pitch Profiler bundle | Coastal games at boot; NCAA pitcher / hitter / matchup pulls on demand; P5 + Sun Belt reference pools on first use |
| `pitchprofiler.pitcher_season`, `..._pitch_type`, `pitcher_game`, `..._pitch_type` | aggregates with plus grades and expected stats | NCAA directory at boot (`pp_directory`); available to leaderboards via `pp_table()` |
| `public.pitches` | Coastal bullpens with Edgertronic clips | read through the REST API with the service key (no pooler), cached per process; `bullpen_pitches()` returns the frame for any tab |

`R/07_pools.R` builds the app's frames. `spring26` (Coastal 2026 games) is
read from Storage and processed at boot in a few seconds. Everything the
pitcher page grades against is a **precomputed artifact** in Supabase Storage
(bucket `palace-serving`, `ref/<build stamp>/`), built once per scored-table
build by `scripts/build_reference_pools.R`:

| Artifact | What | Size |
|---|---|---|
| `p5_maps.rds` | percentile maps (1,001-knot step functions, 0.1% precision) | ~1 MB |
| `exp_movement_grid.rds`, `p5_slot_grid.rds` | expected movement by arm slot | KB |
| `grade_P5.parquet`, `grade_SBC.parquet` | slim grading pools (`P5_slim`, `SBC_slim`): only the columns the grading code reads | ~40 cols |
| `P5_*.parquet`, `SBC.parquet` | full processed reference pools (`P5_2026`, `SBC_2026`) | ~150 MB |

The full pools only load for the matchup matrix, hitter process model and
pitch arsenal. Per-pitcher expected stats for a grading pool come from
`pitchprofiler.pitcher_season(_pitch_type)`, so no Python runs at request
time. The percentile maps, grids, slim grading pools and the hitter
Process-model state are precomputed artifacts in Supabase Storage
(`scripts/build_reference_pools.R`), so a pitcher or hitter page never loads
the full pools. `PALACE_PREWARM=TRUE` forces everything at boot. The retired seasons
(`data`, `fall25`, `prespring`) exist as zero-row frames so nothing
downstream had to change.

The Supabase Postgres pooler throttles and drops large result sets, so the
Coastal games (boot) and the reference pools (~600k pitches, first use) come
from Supabase **Storage** (bucket `palace-serving`,
`ref/<build stamp>/<part>.parquet`; the reference pools already processed).
Lookup order: disk cache (`/data/ref_cache`) → Storage → live database pull.
After every reload of the scored tables, rebuild them once:

```
Rscript scripts/build_reference_pools.R     # ~25 min, needs SUPABASE_URL + SUPABASE_SECRET_KEY (parts: COASTAL SBC P5)
```

Small lookups live in the repo: `reference/*.csv` (rosters, bios, team map,
leagues, heights), `DRS26.csv`, `trumedia_player_bio_master.csv`.

### Leaderboards and the Advance tab

`scripts/build_leaderboards.R` writes `lb/<season>/` to Storage: the season
team list and league-wide TruMedia pitching / batting totals (raw API
columns, so `tm_team_player_totals()` and `tm_all_teams()` are served
without a request), the two leaderboard boards (TruMedia traditional stats +
TrackMan velo / spin / release / arm angle / Stuff+ / Pitching+ / Location+
/ RV / xstats, joined on the TrackMan player id), and a hitters table
(TruMedia line + TrackMan contact and discipline). Rerun it after a
TruMedia refresh or a pipeline reload:

```
Rscript scripts/build_leaderboards.R --pipeline-dir serving_build/master_2026
```

### Refreshing the scored tables

When the master TrackMan file changes, rerun the pipeline and reload:

```
PYTHONPATH=models:scripts python scripts/process_master.py --master ~/master_trackman_2026.parquet
Rscript scripts/upload_supabase.R serving_build/master_2026
Rscript scripts/build_reference_pools.R            # pools + artifacts into Storage (~25 min)
```

See `scripts/README_process_master.md`. The pipeline reclassifies pitch types
(same engine as `R/12`, verified identical), scores every pitch, and never
trims outliers or velocity.

## Sessions

- A successful login sets a 30-day `palace_auth` cookie (a hash derived from
  the app password, never the password); a new session presenting it skips
  the form. Changing `password` invalidates every cookie.
- The page state (`?p=<player>&tab=&sub=&s=`) is kept in the URL and restored
  at session start, so a refresh, a reconnect or a shared link lands on the
  same player and tab. `session$allowReconnect(TRUE)` resumes brief drops.
- Per-user logins: replace `auth_token()` in `R/server/01_core.R` with a
  per-user HMAC over a users table; the cookie and restore plumbing stay.

## Running locally

```
cp .env.example .env      # fill in secrets; readRenviron(".env") or use dotenv
# needs SB_DB_HOST / SB_DB_USER / SB_DB_PASS; nothing else is required to boot
Rscript -e 'shiny::runApp(".")'
```

Python side: `python3 verify_install.py` must pass (LightGBM bundle reproduces
production numbers) before anything else is debugged.

## Deploying

Connect Cloud needs the env vars in `.env.example` (Supabase is required; TruMedia
and TrackMan are optional) and installs Python from `requirements.txt` via
`manifest.json` (regenerate with `rsconnect::writeManifest()` after adding an R
package). Primary file: `app.R`.

## Changes from the original monolith (`app.R` v87)

- Split into `R/` + `R/server/` files; **no logic changes** except those listed here.
- Removed ~650 lines of dead code: pre-Supabase bullpen helpers, the whole
  AWRE Exchange/HLS module (product removed 2026-08-31; still in git history), `college_re24/re288` loads, `TEAM_REPORT_COLS`, `LB_TM_SHAPE_TOKENS`,
  `PA_SHRINK_K`, `pitcher_bio_lookup`, the unused `selected_spray` reactive,
  `spraySBC_2025`/`sprayP5_2025`.
- Dropped `tidymodels`, `parsnip`, `recipes`, `xgboost` (unreferenced).
- `build_p5_slot_grid()` moved out of `server()` — it was rebuilt from the full
  P5 pool on every session.
- `get("full_data_p5_compare", envir = globalenv())` → `inherits = TRUE` in the
  pitch-arsenal code: the app env is not the global env under `runApp`, so the
  league pool was silently never found.
- Small CSVs moved to `reference/` (`merged_teams (2).csv` → `reference/merged_teams.csv`).
- `DRS26.csv` / `trumedia_player_bio_master.csv` are committed with the app and resolved locally.
