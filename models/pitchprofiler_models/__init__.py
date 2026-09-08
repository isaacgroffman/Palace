"""Pitch Profiler college model bundle.

Six metrics from a raw college TrackMan file:

    proStuff+      pitch physics and release only
    proPitching+   the above plus location, approach angles, count, sequencing
    proLocation+   what remains after the raw stuff is stripped out
    xBA / xSLG / xwOBA   expected stats from a monotone batted-ball chain

Quick start::

    import polars as pl
    from pitchprofiler_models import load_bundle, run_all

    bundle = load_bundle("models")
    raw = pl.read_csv("game.csv")          # or pl.read_parquet(...)
    out = run_all(raw, bundle)

    print(out["grades"])   # proStuff+ / proPitching+ / proLocation+
    print(out["xstats"])   # xBA / xSLG / xwOBA

See MODELS-IMPLEMENTATION-GUIDE.md for the full contract.
"""

from .loader import ModelBundle, load_bundle
from .pipeline import (
    REQUIRED_RAW_COLUMNS,
    check_columns,
    clean,
    expected_stats,
    pitcher_grades,
    run_all,
    score_pitches,
)

__all__ = [
    "ModelBundle",
    "load_bundle",
    "REQUIRED_RAW_COLUMNS",
    "check_columns",
    "clean",
    "expected_stats",
    "pitcher_grades",
    "run_all",
    "score_pitches",
]
