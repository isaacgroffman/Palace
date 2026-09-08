"""Artifact loading for the Pitch Profiler college model bundle.

Loads the six proStuff/proPitching LightGBM boosters, the per-count weight
dict, the plus-scaling reference, the four expected-stats chain boosters, the
xstats constants, and the optional park-factor venue tables.

This mirrors the production ``LeagueModelRepository`` with the SHA256 integrity
verification removed (that guards against artifact tampering in a deployed
service and has no meaning for a local copy).
"""

from __future__ import annotations

import json
import pickle
from dataclasses import dataclass, field
from pathlib import Path

import lightgbm as lgb
import polars as pl

# The six boosters the proStuff/proPitching scorer expects, by the exact key
# names ``scoring.score`` indexes with.
BOOSTERS = ["stage1", "stage2_pitch", "stage2_stuff", "stage2t", "hr_pitch", "hr_stuff"]

# Expected-stats chain links. hit / xbh / hr take [ExitSpeed, Angle];
# triple takes [Angle] only.
XSTATS_BOOSTERS = ["hit", "xbh", "hr", "triple"]


@dataclass
class ModelBundle:
    """Every artifact needed to score a college TrackMan file."""

    models: dict[str, lgb.Booster]
    weights: dict
    reference: dict[str, dict[str, float]]
    xstats_models: dict[str, lgb.Booster]
    xstats_constants: dict
    run_values: dict
    venue_tables: tuple[pl.DataFrame, pl.DataFrame] | None = None
    stuff_scalars: dict = field(default_factory=dict)


def load_bundle(models_dir: str | Path) -> ModelBundle:
    """Load the bundle from the ``models/`` directory shipped alongside this package.

    Expected layout::

        models/
          promodel/  stage1.txt stage2_pitch.txt stage2_stuff.txt stage2t.txt
                     hr_pitch.txt hr_stuff.txt weights.pkl
                     production_constants.json run_values.json
          xstats/    model_hit.txt model_xbh.txt model_hr.txt model_triple.txt
                     constants.json
          park/      stadium_map.csv park_factor_corrections.csv   (optional)
    """
    root = Path(models_dir)
    pro = root / "promodel"
    xs = root / "xstats"

    models = {n: lgb.Booster(model_file=str(pro / f"{n}.txt")) for n in BOOSTERS}

    # weights.pkl is a first-party artifact shipped inside this bundle, not user
    # input. Production SHA256-verifies it before unpickling; if you wire this
    # into a service that accepts uploaded artifacts, restore that check.
    with open(pro / "weights.pkl", "rb") as fh:
        weights = dict(pickle.load(fh))  # noqa: S301

    with open(pro / "production_constants.json", encoding="utf-8") as fh:
        constants = json.load(fh)

    with open(pro / "run_values.json", encoding="utf-8") as fh:
        run_values = json.load(fh)

    xstats_models = {
        n: lgb.Booster(model_file=str(xs / f"model_{n}.txt")) for n in XSTATS_BOOSTERS
    }
    with open(xs / "constants.json", encoding="utf-8") as fh:
        xstats_constants = json.load(fh)

    # weights.pkl already carries BOTH the six per-count pitching weight tables
    # and the three stuff scalars (W_whiff_stuff, W_foul_stuff, HR_RV). Those
    # three are duplicated in production_constants.json. Verify they agree, so a
    # mismatched pair of artifacts fails at load instead of silently scoring
    # proStuff+ against the wrong run values.
    for key, value in constants["stuff_scalars"].items():
        if key in weights and weights[key] != value:
            raise ValueError("artifact mismatch on " + key)
        weights.setdefault(key, value)

    venue_tables = None
    park = root / "park"
    if park.is_dir():
        smap = pl.read_csv(str(park / "stadium_map.csv")).select(
            ["raw_stadium", "canonical_venue"]
        )
        pf = pl.read_csv(str(park / "park_factor_corrections.csv")).select(
            ["canonical_venue", "ivb_calib", "hb_calib_v2"]
        )
        venue_tables = (smap, pf)

    return ModelBundle(
        models=models,
        weights=weights,
        reference=constants["reference"],
        xstats_models=xstats_models,
        xstats_constants=xstats_constants,
        run_values=run_values,
        venue_tables=venue_tables,
        stuff_scalars=constants["stuff_scalars"],
    )
