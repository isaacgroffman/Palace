# Scoring the master TrackMan file

`scripts/process_master.py` runs the whole `master_trackman_2026.parquet`
(TrackMan v3 API export, dotted column names) through the Pitch Profiler
bundle and writes Supabase-ready tables.

## Run

```bash
# one-time: a Python with the bundle's deps (no system python on this Mac had them)
python3.12 -m venv ~/.venvs/pitchprofiler
~/.venvs/pitchprofiler/bin/pip install "lightgbm>=4.5,<5" polars pandas numpy pyarrow scipy scikit-learn

cd "~/Desktop/palace 2"
PYTHONPATH=models:scripts ~/.venvs/pitchprofiler/bin/python verify_install.py   # must PASS
PYTHONPATH=models:scripts ~/.venvs/pitchprofiler/bin/python scripts/process_master.py \
    --master ~/master_trackman_2026.parquet --out serving_build/master_2026
```

About 10 minutes for 2.58M pitches on this laptop. `--limit-games 60` gives a
6-second smoke run. Then:

```bash
SB_DB_HOST=... SB_DB_USER=... SB_DB_PASS=... Rscript scripts/upload_supabase.R serving_build/master_2026
```

The upload needs a role that can CREATE and INSERT (the app's `bullpen_reader`
is SELECT-only). Tables land in schema `pitchprofiler`.

## Database size (read before uploading `pitches`)

The Supabase project (`ryqzkosdbksawrcrdqrc`) is on the free plan, 500 MB.
The four aggregate tables take about 265 MB and are loaded. The full
per-pitch table would be roughly 4.5 GB in Postgres (410k rows measured at
716 MB), so it does not fit without a plan upgrade or a slimmer column set.
Loading past the quota puts the whole project into read-only mode, which
also blocks the bullpen app's ingest; if that happens, `set
default_transaction_read_only = off` in a session and drop the table.

## What each stage does

| Stage | What | Where |
|---|---|---|
| A | Pitch-type reclassification per pitcher-season: geometry-first clustering, then naming. Python port of `R/12_pitch_reclassification.R`, verified label-for-label identical on 5,129 pitches across 9 pitchers. Coastal (COA_CHA) arms keep their custom tags (the classifier only fills a Coastal tag that is Undefined/Other/blank, `pitch_type_source = coastal_model_fill`); everyone else with 25+ usable pitches gets the model type. | `scripts/palace_pipeline/reclassify.py` |
| B | proStuff+ / proPitching+ / proLocation+ per pitch through the **unchanged** bundle (`clean` -> `score_pitches`), park correction on. Joined back on the five-column key, never positionally. | bundle + `columns.py` |
| C | Expected stats decomposed per pitch so any grouping can be summed. Reproduces `aggregate_xstats` to 1e-15 on 300 pitchers and 1,547 pitcher-by-pitch-type groups. | `scripts/palace_pipeline/xstats_perpitch.py` |
| D | `pitches.parquet`: every master column (snake_case) plus A, B, C. | |
| E | Aggregates: `pitcher_season`, `pitcher_season_pitch_type`, `pitcher_game`, `pitcher_game_pitch_type` (parquet + csv). Plus grades are computed from each group's **mean xRV** against the shipped reference, per guide section 4.3. | |

## No outlier removal

Nothing in this pipeline trims velocity or drops a pitch. The bundle's
plausibility checks run in flag-and-keep mode (`valid`, `invalid_reason`);
a pitch outside a physical bound just gets no plus score. The reclassifier's
spin-error detector imputes a flagged spin value **for clustering only** and
keeps the row (`spin_error_flag`). `velo_max` in the aggregates is the true
max over every pitch.

## Things the master file needed

- 1.2% of rows (bullpen / adhoc sessions) have no plate-appearance numbering.
  They get a synthetic key so the bundle can join them; `k5_adjusted` marks
  them. Their sequencing features are null, as they should be.
- Value drift in the raw feed is normalized (`StirkeCalled`, `Homerun`,
  `Left ` with a trailing space, `Sacrificie`, ...). Raw values are kept in
  the `*_raw` columns.
- Stadium names are spelled `Springs Brooks Stadium` here and
  `SpringsBrooksStadium` in the park table. Punctuation is stripped to match
  (`stadium_match = name`); Standard games with no name match fall back to the
  home team's owned venue (`home_team`); the rest get zero park correction
  (`none`). 43% of pitches end up with a park factor.
- The reference constants are a college pitcher-season population. Per-pitch
  plus values are far noisier than season grades (that is expected); use the
  aggregate tables' plus columns for grades.

## Key output columns

Per pitch (`pitches.parquet`):

- identity: `pitch_uid` (PK), `pitcher_key` (TrackMan pitcher id, else `name:<Pitcher>`), `season`, `is_coastal`
- pitch type: `tagged_pitch_type_raw`, `pitch_type_tagged` (canonical), `pitch_type_model`, `pitch_type` (final), `pitch_type_source` (`coastal_tag` / `coastal_model_fill` / `model` / `tag_small_sample` / `tag_untracked` / `tag_no_pitcher`), `reclassified`, `family_changed`, `spin_error_flag`
- model: the 19 cleaned features, `valid`, `valid_except_call`, `invalid_reason`, `stuff_xrv`, `pitching_xrv`, `location_xrv`, `stuff_plus`, `pitching_plus`, `location_plus`, `stadium`, `stadium_match`, `k5_adjusted`
- outcomes: `pitch_call`, `play_result`, `is_strike`, `is_swing`, `is_whiff`, `is_csw`, `in_zone`, `is_chase`, `is_bbe`, `is_k`, `is_bb`, `is_hbp`, `is_sf`, `is_sac_bunt`, `is_ab_bbe`, `bbe_tracked`, `xba_bbe`, `xtb_bbe`, `xwobacon_bbe`, `p_single`..`p_hr`, `is_hit`, `total_bases`, `wobacon_actual`

Aggregates: counts, mean xRV and plus, velo (avg / max / p90), movement,
release, approach angles, strike / CSW / swing / whiff / zone / chase rates,
PA / AB / K / BB / HBP / BBE, BA / SLG / wOBA, xBA / xSLG / xwOBA, EV, hard-hit.
`in_zone` is a fixed zone (|side| <= 0.83 ft, 1.5 to 3.5 ft).
