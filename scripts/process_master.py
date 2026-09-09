#!/usr/bin/env python
"""Score the whole master TrackMan parquet with the Pitch Profiler bundle.

    PYTHONPATH=models:scripts python scripts/process_master.py \
        --master ~/master_trackman_2026.parquet --out serving_build/master_2026

Stages
  A  reclassify pitch types per pitcher-season (partition-before-naming,
     Python port of R/12_pitch_reclassification.R; verified identical).
     Coastal arms keep their custom tags; everyone else gets the model type.
     NO outlier / velocity trimming anywhere: every pitch keeps its row and
     its measured values. Max velo is never touched.
  B  proStuff+ / proPitching+ / proLocation+ per pitch through the unchanged
     production bundle (cleaning.process_frame -> scoring.score), park
     correction on, joined back on the five-column key (never positional).
  C  expected stats decomposed per pitch (xBA / xSLG / xwOBA terms + the
     actual-outcome terms) so any grouping can be summed later.
  D  per-pitch table: every master column (snake_case) + A + B + C.
  E  aggregate tables: pitcher_season, pitcher_season_pitch_type,
     pitcher_game, pitcher_game_pitch_type. Plus grades are computed from
     the mean xRV of each group against the shipped reference.

Outputs (in --out): pitches.parquet, <table>.parquet + <table>.csv,
schema.sql (Postgres DDL matching the files), manifest.json.
Upload with: Rscript scripts/upload_supabase.R <out dir>
"""
from __future__ import annotations

import argparse
import datetime as dt
import json
import multiprocessing as mp
import sys
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent
sys.path.insert(0, str(ROOT / "models"))
sys.path.insert(0, str(HERE))

import numpy as np  # noqa: E402
import polars as pl  # noqa: E402
import pyarrow as pa  # noqa: E402
import pyarrow.parquet as pq  # noqa: E402

from pitchprofiler_models import REQUIRED_RAW_COLUMNS, clean, load_bundle, score_pitches  # noqa: E402
from pitchprofiler_models.cleaning import FEATURES  # noqa: E402
from palace_pipeline.columns import (  # noqa: E402
    DROP_FROM_PITCHES, K5, add_stadium, fix_k5, load_modeling_frame, snake_name,
)
from palace_pipeline.reclassify import canon_tag, family, reclassify_group  # noqa: E402
from palace_pipeline.xstats_perpitch import per_pitch_xstats, xstats_aggregate_exprs  # noqa: E402

COASTAL_TEAMS = {"COA_CHA", "Coastal Carolina", "Coastal Carolina University"}
MODEL_VERSION = "pitchprofiler-bundle-2026-07 | reclass-R12-port-v1"
STRIKE_CALLS = ["StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay", "AutomaticStrike"]
SWING_CALLS = ["StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay"]
ZONE_HALF_WIDTH_FT = 0.83
ZONE_BOTTOM_FT, ZONE_TOP_FT = 1.5, 3.5

XRV = ["stuff_xrv", "pitching_xrv", "location_xrv"]
PLUS = {"stuff_xrv": ("stuff_plus", "stuff"), "pitching_xrv": ("pitching_plus", "pitching"),
        "location_xrv": ("location_plus", "location")}


def log(msg: str) -> None:
    print(f"[{dt.datetime.now():%H:%M:%S}] {msg}", flush=True)


# =============================================================================
# Stage A: reclassification
# =============================================================================
def _work(payload):
    key, uids, velo, ivb, hb, spin, hand, tags = payload
    types, flags, usable, info = reclassify_group(velo, ivb, hb, spin, hand, tags)
    return key, uids, types, flags, usable, info


def stage_reclassify(m: pl.DataFrame, workers: int) -> pl.DataFrame:
    r = m.select(["PitchUID", "Pitcher", "PitcherId", "PitcherTeam", "PitcherThrows", "Date", "GameID", "PitchNo",
                  "TaggedPitchType", "RelSpeed", "InducedVertBreak", "HorzBreak", "SpinRate"])
    r = r.with_columns([
        pl.when(pl.col("PitcherId").is_not_null() & (pl.col("PitcherId") != ""))
          .then(pl.col("PitcherId")).otherwise("name:" + pl.col("Pitcher").fill_null("")).alias("pitcher_key"),
        pl.col("Date").str.slice(0, 4).alias("season"),
        pl.col("PitcherTeam").is_in(list(COASTAL_TEAMS)).fill_null(False).alias("is_coastal"),
        pl.col("TaggedPitchType").map_elements(canon_tag, return_dtype=pl.Utf8).alias("pitch_type_tagged"),
    ]).sort(["pitcher_key", "season", "Date", "GameID", "PitchNo"])

    payloads = []
    for (key, season), g in r.filter(pl.col("Pitcher").fill_null("") != "").group_by(["pitcher_key", "season"], maintain_order=True):
        payloads.append(((key, season), g["PitchUID"].to_list(),
                         g["RelSpeed"].to_numpy(), g["InducedVertBreak"].to_numpy(), g["HorzBreak"].to_numpy(),
                         g["SpinRate"].to_numpy(), g["PitcherThrows"].to_list(), g["TaggedPitchType"].to_list()))
    log(f"A: {len(payloads)} pitcher-seasons to classify on {workers} workers")

    uids, types, flags, usable, notes = [], [], [], [], []
    t0 = time.time()
    done = 0
    with mp.get_context("spawn").Pool(workers) as pool:
        for key, u, t, f, us, info in pool.imap_unordered(_work, payloads, chunksize=8):
            n = len(u)
            uids.extend(u)
            types.extend(t if t is not None else [None] * n)
            flags.extend(f.tolist())
            usable.extend(us.tolist())
            notes.extend([info] * n)
            done += 1
            if done % 1000 == 0:
                log(f"A: {done}/{len(payloads)} groups, {time.time() - t0:.0f}s")
    res = pl.DataFrame({"PitchUID": uids, "pitch_type_model": types, "spin_error_flag": flags,
                        "reclass_usable": usable, "reclass_note": notes})
    r = r.join(res, on="PitchUID", how="left")

    old = pl.col("pitch_type_tagged")
    new = pl.col("pitch_type_model")
    # Coastal custom tags are ground truth and are kept; the classifier only
    # fills a Coastal pitch whose tag is Undefined / Other / blank.
    no_tag = old.is_null() | old.is_in(["", "Other", "Undefined"])
    r = r.with_columns([
        pl.when(pl.col("is_coastal") & no_tag & new.is_not_null()).then(new)
          .when(pl.col("is_coastal")).then(old)
          .when(new.is_not_null()).then(new)
          .otherwise(old).alias("pitch_type"),
        pl.when(pl.col("Pitcher").fill_null("") == "").then(pl.lit("tag_no_pitcher"))
          .when(pl.col("is_coastal") & no_tag & new.is_not_null()).then(pl.lit("coastal_model_fill"))
          .when(pl.col("is_coastal")).then(pl.lit("coastal_tag"))
          .when(new.is_not_null()).then(pl.lit("model"))
          .when(pl.col("reclass_usable").fill_null(False)).then(pl.lit("tag_small_sample"))
          .otherwise(pl.lit("tag_untracked")).alias("pitch_type_source"),
    ])
    changed = (~pl.col("is_coastal") & new.is_not_null() & old.is_not_null()
               & ~old.is_in(["", "Other", "Undefined"]) & (old != new)).fill_null(False)
    r = r.with_columns(changed.alias("reclassified"))
    r = r.with_columns([
        old.map_elements(family, return_dtype=pl.Utf8).alias("_fo"),
        new.map_elements(family, return_dtype=pl.Utf8).alias("_fn"),
    ]).with_columns(
        (pl.col("reclassified") & pl.col("_fo").is_not_null() & pl.col("_fn").is_not_null()
         & (pl.col("_fo") != pl.col("_fn"))).fill_null(False).alias("family_changed")
    ).drop(["_fo", "_fn"])

    log("A: pitch_type_source counts: " + r.group_by("pitch_type_source").len().sort("len", descending=True).to_pandas().to_string(index=False).replace("\n", " | "))
    nm = r.filter(pl.col("pitch_type_source") == "model")
    log(f"A: among model-typed pitches: {nm['reclassified'].mean() * 100:.1f}% tag changed, "
        f"{nm['family_changed'].mean() * 100:.2f}% family changed, {int(nm['spin_error_flag'].sum())} spin flags")
    return r.select(["PitchUID", "pitcher_key", "season", "is_coastal", "pitch_type_tagged", "pitch_type_model",
                     "pitch_type", "pitch_type_source", "reclassified", "family_changed", "spin_error_flag",
                     "reclass_note"])


# =============================================================================
# Stage B: proStuff+ / proPitching+ / proLocation+
# =============================================================================
def stage_score(m: pl.DataFrame, reclass: pl.DataFrame, bundle, smap: pl.DataFrame, n_chunks: int) -> pl.DataFrame:
    m2 = (m.join(reclass.select(["PitchUID", "pitch_type"]), on="PitchUID", how="left")
            .with_columns(pl.col("pitch_type").alias("TaggedPitchType")))  # sequencing uses the final type
    m2 = fix_k5(m2)
    m2 = add_stadium(m2, smap)
    log("B: stadium match: " + m2.group_by("stadium_match").len().sort("len", descending=True).to_pandas().to_string(index=False).replace("\n", " | "))
    send_cols = list(REQUIRED_RAW_COLUMNS) + ["Stadium", "Date", "PitchUID"]
    m2 = m2.with_columns((pl.col("GameID").hash() % n_chunks).alias("_chunk"))
    ref = bundle.reference
    parts = []
    for i in range(n_chunks):
        sub = m2.filter(pl.col("_chunk") == i).select(send_cols + ["k5_adjusted", "stadium_match"])
        t0 = time.time()
        cleaned = clean(sub, bundle, apply_park_correction=True)
        scored = score_pitches(cleaned, bundle)
        sc = pl.from_pandas(scored[K5 + XRV]).with_columns([
            pl.col("Inning").cast(pl.Int64), pl.col("PAofInning").cast(pl.Int64), pl.col("PitchofPA").cast(pl.Int64)])
        cl = cleaned.with_columns([pl.col("Inning").cast(pl.Int64), pl.col("PAofInning").cast(pl.Int64), pl.col("PitchofPA").cast(pl.Int64)])
        out = (cl.join(sc, on=K5, how="left")
                 .join(sub.select(K5 + ["PitchUID", "Stadium", "k5_adjusted", "stadium_match"])
                          .with_columns([pl.col("Inning").cast(pl.Int64), pl.col("PAofInning").cast(pl.Int64), pl.col("PitchofPA").cast(pl.Int64)]),
                       on=K5, how="left"))
        if out.height != sub.height or out["PitchUID"].null_count():
            raise RuntimeError(f"chunk {i}: join back on K5 lost rows ({out.height} vs {sub.height}, "
                               f"{out['PitchUID'].null_count()} null uids)")
        out = out.with_columns([
            (100.0 + (ref[short]["mean"] - pl.col(x)) / ref[short]["std"] * 10.0).alias(plus)
            for x, (plus, short) in PLUS.items()
        ])
        parts.append(out.select(["PitchUID", "game_year", "p_throws", "stand", "Stadium", "stadium_match", "k5_adjusted"]
                                + FEATURES + ["valid", "valid_except_call", "invalid_reason"] + XRV
                                + [PLUS[x][0] for x in XRV]))
        log(f"B: chunk {i + 1}/{n_chunks}: {sub.height} pitches, {int(out['stuff_xrv'].is_not_null().sum())} scored, {time.time() - t0:.0f}s")
    res = pl.concat(parts)
    log(f"B: scored {int(res['stuff_xrv'].is_not_null().sum())}/{res.height} pitches; invalid reasons: "
        + res.group_by("invalid_reason").len().sort("len", descending=True).head(8).to_pandas().to_string(index=False).replace("\n", " | "))
    return res.rename({"Stadium": "stadium", "PitchUID": "PitchUID"})


# =============================================================================
# Stage C: per-pitch outcome terms
# =============================================================================
def stage_outcomes(m: pl.DataFrame, bundle) -> pl.DataFrame:
    xs = per_pitch_xstats(m, bundle.xstats_models, bundle.xstats_constants)
    call = pl.col("PitchCall")
    has_loc = pl.col("PlateLocSide").is_not_null() & pl.col("PlateLocHeight").is_not_null()
    in_zone = pl.when(has_loc).then(
        (pl.col("PlateLocSide").abs() <= ZONE_HALF_WIDTH_FT)
        & pl.col("PlateLocHeight").is_between(ZONE_BOTTOM_FT, ZONE_TOP_FT)).otherwise(None)
    swing = call.is_in(SWING_CALLS).fill_null(False)
    ev = m.select([
        pl.col("PitchUID"),
        pl.col("PitchCall").alias("pitch_call"), pl.col("PlayResult").alias("play_result"),
        pl.col("TaggedHitType").alias("tagged_hit_type"), pl.col("KorBB").alias("kor_bb"),
        pl.col("PitcherThrows").alias("pitcher_throws"), pl.col("BatterSide").alias("batter_side"),
        (call.is_not_null() & (call != "Undefined")).alias("has_call"),
        call.is_in(STRIKE_CALLS).fill_null(False).alias("is_strike"),
        swing.alias("is_swing"),
        (call == "StrikeSwinging").fill_null(False).alias("is_whiff"),
        call.is_in(["StrikeCalled", "StrikeSwinging"]).fill_null(False).alias("is_csw"),
        in_zone.alias("in_zone"),
        (swing & (in_zone == False)).fill_null(False).alias("is_chase"),  # noqa: E712
    ])
    return ev.join(xs, on="PitchUID", how="left")


# =============================================================================
# Stage D: per-pitch table
# =============================================================================
def stage_write_pitches(master: str, outputs: pl.DataFrame, out_path: Path, built_at: str) -> int:
    pf = pq.ParquetFile(master)
    writer = None
    total = 0
    outputs = outputs.rename({"PitchUID": "pitch_uid"})
    n_batches = 0
    for batch in pf.iter_batches(batch_size=250_000):
        n_batches += 1
        rg = n_batches
        t = pl.from_arrow(pa.Table.from_batches([batch]))
        t = t.drop([c for c in DROP_FROM_PITCHES if c in t.columns])
        t = t.rename({c: snake_name(c) for c in t.columns})
        t = t.join(outputs, on="pitch_uid", how="left").with_columns([
            pl.lit(MODEL_VERSION).alias("model_version"), pl.lit(built_at).alias("built_at"),
        ])
        if t["pitcher_key"].null_count():
            raise RuntimeError("pitch outputs missing for some rows in row group %d" % rg)
        tbl = t.to_arrow()
        if writer is None:
            writer = pq.ParquetWriter(str(out_path), tbl.schema, compression="zstd")
        writer.write_table(tbl)
        total += t.height
        log(f"D: wrote batch {rg} ({t.height} rows, {t.width} cols), {total} total")
        del t, tbl
    writer.close()
    return total


# =============================================================================
# Stage E: aggregates
# =============================================================================
def _mode(c: str) -> pl.Expr:
    return pl.col(c).drop_nulls().mode().first().alias(c)


def _agg_exprs(w: dict) -> list[pl.Expr]:
    scored = pl.col("stuff_xrv").is_not_null()
    tracked = pl.col("bbe_tracked")
    has_call = pl.col("has_call")
    has_loc = pl.col("in_zone").is_not_null()
    rate = lambda num, den: pl.when(den > 0).then(num / den).otherwise(None)  # noqa: E731
    swings = pl.col("is_swing").sum()
    out = [
        pl.len().alias("pitches"),
        pl.col("rel_speed").is_not_null().sum().alias("pitches_tracked"),
        scored.sum().alias("pitches_scored"),
        pl.col("reclassified").sum().alias("n_reclassified"),
        pl.col("family_changed").sum().alias("n_family_changed"),
        pl.col("spin_error_flag").sum().alias("n_spin_flags"),
        pl.col("stuff_xrv").mean().alias("stuff_xrv"),
        pl.col("pitching_xrv").mean().alias("pitching_xrv"),
        pl.col("location_xrv").mean().alias("location_xrv"),
        pl.col("rel_speed").mean().alias("velo_avg"),
        pl.col("rel_speed").max().alias("velo_max"),
        pl.col("rel_speed").quantile(0.9).alias("velo_p90"),
        pl.col("induced_vert_break").mean().alias("ivb_avg"),
        pl.col("horz_break").mean().alias("hb_avg"),
        pl.col("spin_rate").mean().alias("spin_avg"),
        pl.col("extension").mean().alias("ext_avg"),
        pl.col("rel_height").mean().alias("rel_height_avg"),
        pl.col("rel_side").mean().alias("rel_side_avg"),
        pl.col("vert_appr_angle").mean().alias("vaa_avg"),
        pl.col("horz_appr_angle").mean().alias("haa_avg"),
        pl.col("plate_loc_height").mean().alias("plate_height_avg"),
        pl.col("plate_loc_side").mean().alias("plate_side_avg"),
        has_call.sum().alias("pitches_with_call"),
        rate(pl.col("is_strike").sum(), has_call.sum()).alias("strike_pct"),
        rate(pl.col("is_csw").sum(), has_call.sum()).alias("csw_pct"),
        rate(swings, has_call.sum()).alias("swing_pct"),
        rate(pl.col("is_whiff").sum(), swings).alias("whiff_pct"),
        rate(pl.col("in_zone").fill_null(False).sum(), has_loc.sum()).alias("zone_pct"),
        rate(pl.col("is_chase").sum(), (has_loc & ~pl.col("in_zone").fill_null(True)).sum()).alias("chase_pct"),
        rate((pl.col("is_whiff") & pl.col("in_zone").fill_null(False)).sum(),
             (pl.col("is_swing") & pl.col("in_zone").fill_null(False)).sum()).alias("zone_whiff_pct"),
        pl.col("exit_speed").filter(tracked).mean().alias("ev_avg"),
        pl.col("launch_angle").filter(tracked).mean().alias("la_avg"),
        rate((tracked & (pl.col("exit_speed") >= 95)).sum(), tracked.sum()).alias("hard_hit_pct"),
    ]
    out += xstats_aggregate_exprs(w)
    return out


def _finish(agg: pl.DataFrame, ref: dict) -> pl.DataFrame:
    pa_ = pl.col("pa")
    return agg.with_columns(
        [(100.0 + (ref[short]["mean"] - pl.col(x)) / ref[short]["std"] * 10.0).alias(plus) for x, (plus, short) in PLUS.items()]
        + [pl.when(pa_ > 0).then(pl.col("k") / pa_).otherwise(None).alias("k_pct"),
           pl.when(pa_ > 0).then(pl.col("bb") / pa_).otherwise(None).alias("bb_pct")]
    )


def stage_aggregates(pitches_path: Path, bundle, out_dir: Path, built_at: str) -> dict[str, tuple[pl.DataFrame, list[str]]]:
    w = bundle.xstats_constants["woba"]["weights"]
    ref = bundle.reference
    need = ["pitch_uid", "pitcher_key", "season", "pitcher_name", "pitcher_id", "pitcher_team", "pitcher_throws", "level", "league",
            "game_id", "game_date", "session_type", "home_team", "away_team", "batter_team", "venue_name", "is_coastal",
            "pitch_type", "reclassified", "family_changed", "spin_error_flag",
            "rel_speed", "induced_vert_break", "horz_break", "spin_rate", "extension", "rel_height", "rel_side",
            "vert_appr_angle", "horz_appr_angle", "plate_loc_height", "plate_loc_side", "exit_speed", "launch_angle",
            "has_call", "is_strike", "is_swing", "is_whiff", "is_csw", "in_zone", "is_chase",
            "is_bbe", "is_k", "is_bb", "is_hbp", "is_sf", "is_sac_bunt", "is_ab_bbe", "bbe_tracked",
            "xba_bbe", "xtb_bbe", "xwobacon_bbe", "is_hit", "total_bases", "wobacon_actual"] + XRV
    p = pl.read_parquet(str(pitches_path), columns=need).filter(pl.col("pitcher_name").fill_null("") != "")
    log(f"E: aggregating {p.height} pitches with a named pitcher")
    stamp = [pl.lit(MODEL_VERSION).alias("model_version"), pl.lit(built_at).alias("built_at")]

    season_labels = [_mode("pitcher_name"), _mode("pitcher_id"), _mode("pitcher_team"), _mode("pitcher_throws"),
                     _mode("level"), _mode("league"), pl.col("is_coastal").any().alias("is_coastal"),
                     pl.col("game_id").n_unique().alias("games"), pl.col("game_date").min().alias("first_date"),
                     pl.col("game_date").max().alias("last_date")]
    ps = _finish(p.group_by(["pitcher_key", "season"]).agg(season_labels + _agg_exprs(w)), ref).with_columns(stamp)
    tables = {"pitcher_season": (ps.sort(["season", "pitcher_name"]), ["pitcher_key", "season"])}

    pst = _finish(p.group_by(["pitcher_key", "season", "pitch_type"]).agg(season_labels + _agg_exprs(w)), ref)
    pst = pst.join(ps.select(["pitcher_key", "season", pl.col("pitches").alias("_tot")]), on=["pitcher_key", "season"]) \
             .with_columns((pl.col("pitches") / pl.col("_tot")).alias("usage_pct")).drop("_tot").with_columns(stamp)
    tables["pitcher_season_pitch_type"] = (pst.sort(["season", "pitcher_name", "pitch_type"]), ["pitcher_key", "season", "pitch_type"])

    game_labels = [_mode("pitcher_name"), _mode("pitcher_id"), _mode("pitcher_team"), _mode("pitcher_throws"), _mode("season"),
                   _mode("game_date"), _mode("session_type"), _mode("home_team"), _mode("away_team"),
                   _mode("batter_team").alias("opponent"), _mode("venue_name"), _mode("level"), _mode("league"),
                   pl.col("is_coastal").any().alias("is_coastal")]
    pg = _finish(p.group_by(["pitcher_key", "game_id"]).agg(game_labels + _agg_exprs(w)), ref).with_columns(stamp)
    tables["pitcher_game"] = (pg.sort(["game_date", "pitcher_name"]), ["pitcher_key", "game_id"])

    pgt = _finish(p.group_by(["pitcher_key", "game_id", "pitch_type"]).agg(game_labels + _agg_exprs(w)), ref)
    pgt = pgt.join(pg.select(["pitcher_key", "game_id", pl.col("pitches").alias("_tot")]), on=["pitcher_key", "game_id"]) \
             .with_columns((pl.col("pitches") / pl.col("_tot")).alias("usage_pct")).drop("_tot").with_columns(stamp)
    tables["pitcher_game_pitch_type"] = (pgt.sort(["game_date", "pitcher_name", "pitch_type"]), ["pitcher_key", "game_id", "pitch_type"])

    for name, (df, _) in tables.items():
        df.write_parquet(str(out_dir / f"{name}.parquet"), compression="zstd")
        df.write_csv(str(out_dir / f"{name}.csv"))
        log(f"E: {name}: {df.height} rows x {df.width} cols")
    return tables


# =============================================================================
# DDL
# =============================================================================
def _sql_type(dtype: pl.DataType) -> str:
    if dtype == pl.Utf8 or dtype == pl.String:
        return "text"
    if dtype == pl.Boolean:
        return "boolean"
    if dtype in (pl.Int8, pl.Int16, pl.Int32, pl.UInt8, pl.UInt16, pl.UInt32):
        return "integer"
    if dtype in (pl.Int64, pl.UInt64):
        return "bigint"
    if dtype in (pl.Float32, pl.Float64):
        return "double precision"
    if dtype == pl.Date:
        return "date"
    if isinstance(dtype, pl.Datetime):
        return "timestamptz"
    return "text"


def write_ddl(tables: dict[str, tuple[pl.Schema, list[str]]], path: Path) -> None:
    lines = ["-- Generated by scripts/process_master.py. Column order matches the parquet/CSV files.",
             "-- Tables live in schema pitchprofiler; adjust the search_path or prefix if you prefer public.",
             "create schema if not exists pitchprofiler;", ""]
    for name, (schema, pk) in tables.items():
        lines.append(f"drop table if exists pitchprofiler.{name};")
        lines.append(f"create table pitchprofiler.{name} (")
        cols = [f"    {c} {_sql_type(t)}" for c, t in schema.items()]
        cols.append(f"    primary key ({', '.join(pk)})")
        lines.append(",\n".join(cols))
        lines.append(");")
        for c in pk:
            if c != pk[0]:
                lines.append(f"create index on pitchprofiler.{name} ({c});")
        if "pitcher_key" in schema and "pitcher_key" not in pk[:1]:
            lines.append(f"create index on pitchprofiler.{name} (pitcher_key);")
        if name == "pitches":  # the app pulls by pitcher / batter / team / game
            for c in ("pitcher_name", "batter_name", "pitcher_team", "batter_team", "game_id"):
                lines.append(f"create index pitches_{c}_idx on pitchprofiler.{name} ({c});")
        lines.append("")
    path.write_text("\n".join(lines), encoding="utf-8")


# =============================================================================
def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--master", default=str(Path.home() / "master_trackman_2026.parquet"))
    ap.add_argument("--out", default=str(ROOT / "serving_build" / "master_2026"))
    ap.add_argument("--workers", type=int, default=max(1, mp.cpu_count() // 2))
    ap.add_argument("--chunks", type=int, default=6)
    ap.add_argument("--limit-games", type=int, default=0, help="debug: only the first N games")
    args = ap.parse_args()

    out_dir = Path(args.out)
    out_dir.mkdir(parents=True, exist_ok=True)
    built_at = dt.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    t_start = time.time()

    bundle = load_bundle(ROOT / "models")
    smap = pl.read_csv(str(ROOT / "models" / "park" / "stadium_map.csv"))
    master = args.master
    m = load_modeling_frame(master)
    if args.limit_games:
        keep = m.select("GameID").unique().sort("GameID").head(args.limit_games)["GameID"].to_list()
        m = m.filter(pl.col("GameID").is_in(keep))
        sub_master = out_dir / "_master_subset.parquet"
        pl.scan_parquet(master).filter(pl.col("gameID").is_in(keep)).sink_parquet(str(sub_master))
        master = str(sub_master)
    log(f"loaded {m.height} pitches, {m.select('GameID').n_unique()} games")

    reclass = stage_reclassify(m, args.workers)
    scores = stage_score(m, reclass, bundle, smap, args.chunks)
    outcomes = stage_outcomes(m, bundle)
    outputs = reclass.join(scores, on="PitchUID", how="left").join(outcomes, on="PitchUID", how="left")
    if outputs.height != m.height:
        raise RuntimeError("output row count drifted")
    del scores, outcomes

    pitches_path = out_dir / "pitches.parquet"
    n = stage_write_pitches(master, outputs, pitches_path, built_at)
    del outputs, m
    log(f"D: pitches.parquet: {n} rows")

    tables = stage_aggregates(pitches_path, bundle, out_dir, built_at)
    ddl = {"pitches": (pl.read_parquet_schema(str(pitches_path)), ["pitch_uid"])}
    ddl.update({k: (v[0].schema, v[1]) for k, v in tables.items()})
    write_ddl(ddl, out_dir / "schema.sql")

    manifest = {
        "built_at": built_at, "model_version": MODEL_VERSION, "master": args.master,
        "rows": {"pitches": n, **{k: v[0].height for k, v in tables.items()}},
        "minutes": round((time.time() - t_start) / 60, 1),
        "notes": ["no outlier or velocity trimming anywhere; every master pitch keeps its row",
                  "Coastal (COA_CHA) pitchers keep their custom tags (classifier fills only Undefined/Other/blank tags); pitch_type_model always holds the classifier output",
                  "plus grades in aggregate tables are computed from mean xRV against the shipped reference",
                  "in_zone uses a fixed zone: |side| <= 0.83 ft, 1.5 <= height <= 3.5 ft"],
    }
    (out_dir / "manifest.json").write_text(json.dumps(manifest, indent=2), encoding="utf-8")
    log(f"done in {manifest['minutes']} min -> {out_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
