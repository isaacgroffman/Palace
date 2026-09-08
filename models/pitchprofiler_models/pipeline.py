"""End-to-end: raw college TrackMan file to the six metrics.

    proStuff+ / proPitching+ / proLocation+   (pitcher-season grades)
    xBA / xSLG / xwOBA                        (expected stats)

The two halves consume DIFFERENT frames, which is the single easiest thing to
get wrong:

  * The plus grades run off the CLEANED FEATURE frame produced by
    ``cleaning.process_frame``, which normalizes handedness, park-corrects
    movement, derives accelerations and sequencing, and nulls features that
    fail a plausibility check.
  * The expected stats run off the RAW TrackMan frame, because they read
    outcome columns (PitchCall, PlayResult, ExitSpeed, Angle, KorBB,
    TaggedHitType, Balls, Strikes) that cleaning does not carry forward.

Both are exposed here so a caller never has to know which is which.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import polars as pl

from . import cleaning, scoring, xstats
from .loader import ModelBundle

# Raw TrackMan columns cleaning.process_frame reads. A file missing any of these
# cannot produce the plus grades.
REQUIRED_RAW_COLUMNS = [
    "GameID", "Inning", "BatterTeam", "PAofInning", "PitchofPA",
    "Pitcher", "Batter", "PitcherThrows", "BatterSide", "TaggedPitchType",
    "RelSpeed", "SpinRate", "RelHeight", "RelSide", "Extension",
    "InducedVertBreak", "HorzBreak", "SpinAxis",
    "PlateLocSide", "PlateLocHeight", "VertRelAngle", "HorzRelAngle",
    "Balls", "Strikes", "Outs", "PitchCall",
    "vy0", "ay0", "y0",
]


def check_columns(df: pl.DataFrame | pd.DataFrame) -> list[str]:
    """Return the required raw columns that are missing from ``df``."""
    have = set(df.columns)
    return [c for c in REQUIRED_RAW_COLUMNS if c not in have]


def to_polars(df: pl.DataFrame | pd.DataFrame) -> pl.DataFrame:
    """Accept either frame type; cleaning is written against polars."""
    if isinstance(df, pl.DataFrame):
        return df
    return pl.from_pandas(df)


def clean(
    raw: pl.DataFrame | pd.DataFrame,
    bundle: ModelBundle,
    *,
    apply_park_correction: bool = True,
) -> pl.DataFrame:
    """Raw TrackMan frame to the 19 model features plus validity flags.

    Park correction is ON by default, matching production College behavior. It
    is a no-op for any venue absent from the park-factor table, and for every
    venue when the bundle ships without a ``park/`` directory.
    """
    raw_pl = to_polars(raw)
    missing = check_columns(raw_pl)
    if missing:
        raise ValueError("TrackMan frame is missing required columns: " + ", ".join(missing))
    smap, pf = bundle.venue_tables if bundle.venue_tables else (None, None)
    return cleaning.process_frame(
        raw_pl, smap, pf, apply_park_correction=apply_park_correction
    )


def score_pitches(cleaned: pl.DataFrame, bundle: ModelBundle) -> pd.DataFrame:
    """Per-pitch xRV for every scoreable pitch in a cleaned frame.

    Keeps rows on ``valid_except_call`` rather than ``valid``: PitchCall is an
    OUTCOME, not a model input, so a pitch whose only defect is an undefined
    call is still fully scoreable. This matches production.

    Returns a pandas frame carrying the identity columns plus stuff_xrv,
    pitching_xrv and location_xrv. Lower xRV is better for the pitcher.
    """
    keep = cleaned.filter(pl.col("valid_except_call").fill_null(False))
    df = keep.to_pandas()
    if df.empty:
        return df.assign(stuff_xrv=[], pitching_xrv=[], location_xrv=[])
    out = scoring.score(df, bundle.models, bundle.weights)
    for name, values in out.items():
        df[name] = values
    return df


def pitcher_grades(
    scored: pd.DataFrame,
    bundle: ModelBundle,
    group_cols: tuple[str, ...] = ("Pitcher", "game_year"),
) -> pd.DataFrame:
    """Aggregate per-pitch xRV to grades.

    The plus is a GRADE over an aggregate, not a per-pitch value: mean the xRV
    within each group, then scale that mean against the between-pitcher-season
    reference. The shipped reference was built on (Pitcher, game_year) means,
    so that grouping is the calibrated one. Adding a pitch-type column gives the
    per-pitch-type grades the dashboard shows; those are scaled against the same
    reference and therefore carry more spread than a full-season grade.
    """
    if scored.empty:
        return scored
    agg = scoring.aggregate_xrv(scored, group_cols=list(group_cols))
    return scoring.apply_pitcher_plus(agg, bundle.reference)


def expected_stats(
    raw: pl.DataFrame | pd.DataFrame,
    bundle: ModelBundle,
    group_cols: tuple[str, ...] = ("Pitcher",),
) -> pd.DataFrame:
    """xBA / xSLG / xwOBA per group, computed off the RAW frame.

    Aggregation is Savant style: a batted ball with both ExitSpeed and Angle
    scores off the model chain; one missing either falls back to its actual
    outcome. Strikeouts are added to at-bats and sacrifices are excluded.

    Pass ``group_cols=("Pitcher", "TaggedPitchType")`` for per-pitch-type
    expected stats. Each plate-appearance-terminal event is attributed to the
    terminal pitch's row, so grouping by pitch type attributes the outcome to
    the pitch that ended the at-bat.
    """
    df = raw.to_pandas() if isinstance(raw, pl.DataFrame) else raw.copy()
    rows = []
    for key, sub in df.groupby(list(group_cols), dropna=True):
        stats = xstats.aggregate_xstats(sub, bundle.xstats_models, bundle.xstats_constants)
        key_tuple = key if isinstance(key, tuple) else (key,)
        rows.append({**dict(zip(group_cols, key_tuple)), **stats, "pitches": len(sub)})
    return pd.DataFrame(rows)


def run_all(
    raw: pl.DataFrame | pd.DataFrame,
    bundle: ModelBundle,
    *,
    apply_park_correction: bool = True,
) -> dict[str, pd.DataFrame]:
    """Everything at once.

    Returns ``{"cleaned", "scored", "grades", "xstats"}``. ``grades`` carries
    proStuff+/proPitching+/proLocation+ per pitcher-season; ``xstats`` carries
    xBA/xSLG/xwOBA per pitcher.
    """
    cleaned = clean(raw, bundle, apply_park_correction=apply_park_correction)
    scored = score_pitches(cleaned, bundle)
    return {
        "cleaned": cleaned.to_pandas(),
        "scored": scored,
        "grades": pitcher_grades(scored, bundle),
        "xstats": expected_stats(raw, bundle),
    }
