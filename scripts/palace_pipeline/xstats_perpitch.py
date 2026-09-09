"""Per-pitch decomposition of the bundle's Savant-style expected stats.

`pitchprofiler_models.xstats.aggregate_xstats` returns one xBA/xSLG/xwOBA per
group. Supabase wants the pieces attached to every pitch so any grouping can
be summed later. This module emits, per pitch, exactly the terms that
function sums, using the same chain boosters, the same screen clipping, the
same K/BB derivation and the same sac-fly / sac-bunt rules:

    is_k, is_bb, is_hbp, is_bbe, is_ab_bbe, is_sf, is_sac_bunt, bbe_tracked
    xba_bbe, xtb_bbe, xwobacon_bbe        (model on tracked BBE, actual fallback otherwise)
    p_single, p_double, p_triple, p_hr    (model only, null when untracked)
    is_hit, total_bases, wobacon_actual   (actual outcome, for actual BA/SLG/wOBA)

Aggregate rules (identical to aggregate_xstats):
    ab       = sum(is_ab_bbe) + sum(is_k)
    woba_den = ab + sum(is_bb) + sum(is_sf) + sum(is_hbp)
    xba      = sum(xba_bbe  where is_ab_bbe) / ab
    xslg     = sum(xtb_bbe  where is_ab_bbe) / ab
    xwoba    = (sum(xwobacon_bbe where is_bbe & ~is_sac_bunt) + w_bb*bb + w_hbp*hbp) / woba_den
`check_against_bundle` proves the equality on any frame.
"""
from __future__ import annotations

import numpy as np
import pandas as pd
import polars as pl

from pitchprofiler_models import xstats as xs

CSW_CALLS = list(xs._CSW_CALLS)
BALL_CALLS = list(xs._BALL_CALLS)
HIT_RESULTS = list(xs._HIT_RESULTS)
ACTUAL_TB = dict(xs._ACTUAL_TB)


def per_pitch_xstats(df: pl.DataFrame, boosters: dict, constants: dict) -> pl.DataFrame:
    """df needs PitchUID, PitchCall, PlayResult, KorBB, Balls, Strikes,
    ExitSpeed, Angle, TaggedHitType. Returns PitchUID + the columns above."""
    w = constants["woba"]["weights"]
    screen = constants["screen"]
    calls = pl.col("PitchCall")
    pr = pl.col("PlayResult")
    balls = pl.col("Balls").cast(pl.Float64, strict=False)
    strikes = pl.col("Strikes").cast(pl.Float64, strict=False)
    korbb = pl.col("KorBB").fill_null("")

    out = df.select([
        pl.col("PitchUID"),
        ((calls == "InPlay") & pr.is_not_null() & (pr != "Undefined")).alias("is_bbe"),
        ((korbb == "Strikeout") | (calls.is_in(CSW_CALLS) & (strikes >= 2))).fill_null(False).alias("is_k"),
        ((korbb == "Walk") | (calls.is_in(BALL_CALLS) & (balls >= 3))).fill_null(False).alias("is_bb"),
        (calls == "HitByPitch").fill_null(False).alias("is_hbp"),
        pr,
        pl.col("TaggedHitType"),
        pl.col("ExitSpeed").cast(pl.Float64, strict=False),
        pl.col("Angle").cast(pl.Float64, strict=False),
    ])
    is_sac = out["is_bbe"] & (out["PlayResult"] == "Sacrifice").fill_null(False)
    is_bunt = (out["TaggedHitType"] == "Bunt").fill_null(False)
    out = out.with_columns([
        (is_sac & is_bunt).alias("is_sac_bunt"),
        (is_sac & ~is_bunt).alias("is_sf"),
    ]).with_columns(
        (pl.col("is_bbe") & ~pl.col("is_sac_bunt") & ~pl.col("is_sf")).alias("is_ab_bbe"),
        (pl.col("is_bbe") & pl.col("ExitSpeed").is_not_null() & pl.col("Angle").is_not_null()).alias("bbe_tracked"),
    )

    n = out.height
    xba = np.zeros(n); xtb = np.zeros(n); xcon = np.zeros(n)
    p1 = np.full(n, np.nan); p2 = np.full(n, np.nan); p3 = np.full(n, np.nan); phr = np.full(n, np.nan)
    bbe = out["is_bbe"].to_numpy()
    tracked = out["bbe_tracked"].to_numpy()
    if tracked.any():
        ev = out["ExitSpeed"].to_numpy()[tracked]
        la = out["Angle"].to_numpy()[tracked]
        link = xs.predict_chain(boosters, ev, la, screen)
        cls = xs.compose_classes(link)
        xba[tracked] = link["p_hit"]
        xtb[tracked] = cls["p_single"] + 2 * cls["p_double"] + 3 * cls["p_triple"] + 4 * cls["p_hr"]
        xcon[tracked] = (w["single"] * cls["p_single"] + w["double"] * cls["p_double"]
                         + w["triple"] * cls["p_triple"] + w["home_run"] * cls["p_hr"])
        p1[tracked] = cls["p_single"]; p2[tracked] = cls["p_double"]
        p3[tracked] = cls["p_triple"]; phr[tracked] = cls["p_hr"]
    untracked = bbe & ~tracked
    play = out["PlayResult"].to_pandas()
    a_xba, a_xtb, a_con = xs._actual_expected(w, play)
    xba[untracked] = a_xba[untracked]; xtb[untracked] = a_xtb[untracked]; xcon[untracked] = a_con[untracked]

    # actual outcomes on BBE rows (for actual BA / SLG / wOBA with the same denominators)
    is_hit = np.where(bbe, a_xba, 0.0)
    tb = np.where(bbe, a_xtb, 0.0)
    con_actual = np.where(bbe, a_con, 0.0)

    return out.select(["PitchUID", "is_bbe", "is_k", "is_bb", "is_hbp", "is_sf", "is_sac_bunt", "is_ab_bbe", "bbe_tracked"]).with_columns([
        pl.Series("xba_bbe", xba), pl.Series("xtb_bbe", xtb), pl.Series("xwobacon_bbe", xcon),
        pl.Series("p_single", p1), pl.Series("p_double", p2), pl.Series("p_triple", p3), pl.Series("p_hr", phr),
        pl.Series("is_hit", is_hit.astype(bool)), pl.Series("total_bases", tb), pl.Series("wobacon_actual", con_actual),
    ])


def xstats_aggregate_exprs(w: dict) -> list[pl.Expr]:
    """Polars aggregation expressions reproducing aggregate_xstats."""
    ab = pl.col("is_ab_bbe").sum() + pl.col("is_k").sum()
    bb = pl.col("is_bb").sum()
    hbp = pl.col("is_hbp").sum()
    sf = pl.col("is_sf").sum()
    den = ab + bb + sf + hbp
    xba_num = pl.when(pl.col("is_ab_bbe")).then(pl.col("xba_bbe")).otherwise(0.0).sum()
    xtb_num = pl.when(pl.col("is_ab_bbe")).then(pl.col("xtb_bbe")).otherwise(0.0).sum()
    con = pl.when(pl.col("is_bbe") & ~pl.col("is_sac_bunt")).then(pl.col("xwobacon_bbe")).otherwise(0.0).sum()
    hits = pl.when(pl.col("is_ab_bbe")).then(pl.col("is_hit").cast(pl.Float64)).otherwise(0.0).sum()
    tbs = pl.when(pl.col("is_ab_bbe")).then(pl.col("total_bases")).otherwise(0.0).sum()
    acon = pl.when(pl.col("is_bbe") & ~pl.col("is_sac_bunt")).then(pl.col("wobacon_actual")).otherwise(0.0).sum()
    # aggregate_xstats returns all-None when ab == 0, and None for xwoba alone
    # when the wOBA denominator is 0; mirror both.
    nz = lambda num, d: pl.when((ab > 0) & (d > 0)).then(num / d).otherwise(None)  # noqa: E731
    return [
        (ab + bb + hbp + sf + pl.col("is_sac_bunt").sum()).alias("pa"),
        ab.alias("ab"), pl.col("is_k").sum().alias("k"), bb.alias("bb"), hbp.alias("hbp"), sf.alias("sf"),
        pl.col("is_sac_bunt").sum().alias("sac_bunt"),
        pl.col("is_bbe").sum().alias("bbe"), pl.col("bbe_tracked").sum().alias("bbe_tracked"),
        hits.alias("hits"), tbs.alias("total_bases"),
        nz(hits, ab).alias("ba"), nz(tbs, ab).alias("slg"),
        nz(acon + w["walk"] * bb + w["hit_by_pitch"] * hbp, den).alias("woba"),
        nz(xba_num, ab).alias("xba"), nz(xtb_num, ab).alias("xslg"),
        nz(con + w["walk"] * bb + w["hit_by_pitch"] * hbp, den).alias("xwoba"),
    ]


def check_against_bundle(raw: pl.DataFrame, per_pitch: pl.DataFrame, boosters: dict, constants: dict,
                         group_col: str = "Pitcher", tol: float = 1e-9) -> tuple[int, float]:
    """Compare the per-pitch aggregation with aggregate_xstats per group.
    Returns (groups compared, max abs difference)."""
    w = constants["woba"]["weights"]
    mine = (raw.select(["PitchUID", group_col]).join(per_pitch, on="PitchUID", how="left")
               .group_by(group_col).agg(xstats_aggregate_exprs(w)).to_pandas().set_index(group_col))
    pdf = raw.to_pandas()
    worst = 0.0
    n = 0
    for key, sub in pdf.groupby(group_col, dropna=True):
        ref = xs.aggregate_xstats(sub, boosters, constants)
        row = mine.loc[key]
        for k in ("xba", "xslg", "xwoba"):
            a, b = ref[k], row[k]
            if a is None and (b is None or pd.isna(b)):
                continue
            if a is None or b is None or pd.isna(b):
                print(f"  mismatch {group_col}={key!r} {k}: bundle={a} per-pitch={b}")
                worst = max(worst, float("inf"))
            else:
                worst = max(worst, abs(float(a) - float(b)))
        n += 1
    return n, worst
