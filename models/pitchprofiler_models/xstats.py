"""Expected-stats (xBA / xSLG / xwOBA) scoring for the pitcher dashboard.

Mirrors the aggregation semantics of the training library
(V12 college-promodeling/expected_stats/xstats_lib.py). Every function here is
pure: the four LightGBM chain boosters and the league constants block are
injected by the caller (``LeagueModelRepository`` loads and integrity-verifies
them), so nothing in this module touches the filesystem.

Model interface expectations (what the injected boosters must satisfy):
  * ``boosters`` is a dict with keys ``hit``, ``xbh``, ``hr``, ``triple``.
  * ``hit`` / ``xbh`` / ``hr`` each accept a 2-column ``[ExitSpeed, Angle]``
    matrix and return P(link) per row.
  * ``triple`` accepts a 1-column ``[Angle]`` matrix (launch angle only).
  * ``constants`` is the parsed ``xstats/constants.json`` with a ``screen`` block
    (ev_min / ev_max / la_abs_max) used to clip inputs and a
    ``woba["weights"]`` block giving the six event wOBA weights.

Chain composition (monotone chain, triple link sees Angle only):
    P(1B) = P_hit * (1 - P_xbh)
    P(2B) = P_hit * P_xbh * (1 - P_hr) * (1 - P_triple)
    P(3B) = P_hit * P_xbh * (1 - P_hr) * P_triple
    P(HR) = P_hit * P_xbh * P_hr
Per-BBE expected values: xBA = P_hit; xTB = P1B + 2 P2B + 3 P3B + 4 PHR;
xwOBAcon = w1B P1B + w2B P2B + w3B P3B + wHR PHR.

Aggregation (Savant style): tracked BBE (EV and LA present) score off the model;
untracked BBE fall back to their ACTUAL outcome. AB excludes all Sacrifice rows
and adds K; sac flies stay out of AB but their contact value counts toward xwOBA
via the wOBA denominator (AB + BB + SF + HBP). When TaggedHitType is present,
Sacrifice + TaggedHitType == "Bunt" is a sac bunt (excluded from xwOBA); without
TaggedHitType every Sacrifice is treated as a sac fly.

K / BB detection: unions the official-scoring KorBB field ("Strikeout" /
"Walk") with a count-derived signal (a called/swinging strike at Strikes>=2 is
strike three; a ball call at Balls>=3 is ball four), mirroring
``league_run_values.pitch_run_values``. KorBB is not always populated on
every upload; Balls/Strikes/PitchCall are core tracked columns and are always
present. Without the count-derived fallback, an upload missing KorBB silently
drops every strikeout from AB, producing a "contact-only" xBA/xSLG/xwOBA that
looks legitimate but is not (bug found + fixed 2026-07-08: real uploads are
not guaranteed to carry KorBB, per the Phase-1 ingest research).

ExitSpeed / Angle sourcing: read here by literal column name off the raw
TrackMan frame (see the tracked-BBE check below). The frontend's DuckDB-WASM
ingest allowlist (columnAllowlist.json) is a separate, independently
maintained list of columns kept when pruning a large-parquet upload in the
browser; it omitted ExitSpeed/Angle entirely (bug found + fixed 2026-07-09),
so every upload through that path had zero tracked BBE regardless of what the
raw file actually recorded, silently collapsing xBA/xSLG/xwOBA to the
untracked actual-outcome fallback for every pitcher. The backend coverage
test (test_column_allowlist_covers_pipeline.py) only guarded columns cleaning/
joining would crash without (_CRITICAL/_DATE_SOURCES/K5); it never covered
what downstream consumers like this module read, so the gap shipped silently.
"""

import numpy as np
import pandas as pd

# Inlined verbatim from app/utils/league_rate_stats.py and league_run_values.py
# so this package has no dependency on the wider application.
_CSW_CALLS = frozenset(["StrikeCalled", "StrikeSwinging"])
_BALL_CALLS = frozenset(["BallCalled", "BallInDirt", "BallinDirt", "BallIntentional"])

# PlayResult spellings span feed generations: 2024-25 files emit HomeRun,
# 2026 files emit Homerun. Both count (same drift class as the foul PitchCalls).
_HIT_RESULTS = ("Single", "Double", "Triple", "HomeRun", "Homerun")
_ACTUAL_TB = {
    "Single": 1.0,
    "Double": 2.0,
    "Triple": 3.0,
    "HomeRun": 4.0,
    "Homerun": 4.0,
}


def predict_chain(
    boosters: dict, ev: np.ndarray, la: np.ndarray, screen: dict
) -> dict[str, np.ndarray]:
    """Chain link probabilities for EV / LA arrays, clipped to the screen bounds."""
    ev_c = np.clip(np.asarray(ev, dtype=float), screen["ev_min"], screen["ev_max"])
    la_c = np.clip(np.asarray(la, dtype=float), -screen["la_abs_max"], screen["la_abs_max"])
    feats = np.column_stack([ev_c, la_c])
    return {
        "p_hit": np.asarray(boosters["hit"].predict(feats), dtype=float),
        "p_xbh": np.asarray(boosters["xbh"].predict(feats), dtype=float),
        "p_hr": np.asarray(boosters["hr"].predict(feats), dtype=float),
        "p_triple": np.asarray(boosters["triple"].predict(la_c.reshape(-1, 1)), dtype=float),
    }


def compose_classes(link_probs: dict[str, np.ndarray]) -> dict[str, np.ndarray]:
    """Chain link probabilities -> per-class outcome probabilities."""
    p_hit = link_probs["p_hit"]
    p_xbh = link_probs["p_xbh"]
    p_hr = link_probs["p_hr"]
    p_3 = link_probs["p_triple"]
    return {
        "p_single": p_hit * (1.0 - p_xbh),
        "p_double": p_hit * p_xbh * (1.0 - p_hr) * (1.0 - p_3),
        "p_triple": p_hit * p_xbh * (1.0 - p_hr) * p_3,
        "p_hr": p_hit * p_xbh * p_hr,
    }


def _model_expected(
    boosters: dict, weights: dict, ev: np.ndarray, la: np.ndarray, screen: dict
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Model (xba_ev, xtb_ev, xwobacon_ev) arrays for tracked BBE."""
    link = predict_chain(boosters, ev, la, screen)
    cls = compose_classes(link)
    xba_ev = link["p_hit"]
    xtb_ev = (
        cls["p_single"]
        + 2.0 * cls["p_double"]
        + 3.0 * cls["p_triple"]
        + 4.0 * cls["p_hr"]
    )
    xwobacon_ev = (
        weights["single"] * cls["p_single"]
        + weights["double"] * cls["p_double"]
        + weights["triple"] * cls["p_triple"]
        + weights["home_run"] * cls["p_hr"]
    )
    return xba_ev, xtb_ev, xwobacon_ev


def _actual_expected(
    weights: dict, play_result: pd.Series
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Actual-outcome (xba_ev, xtb_ev, xwobacon_ev) fallback for untracked BBE."""
    pr = play_result.astype(object)
    xba_ev = pr.isin(_HIT_RESULTS).to_numpy(dtype=float)
    xtb_ev = pr.map(_ACTUAL_TB).fillna(0.0).to_numpy(dtype=float)
    con_map = {
        "Single": weights["single"],
        "Double": weights["double"],
        "Triple": weights["triple"],
        "HomeRun": weights["home_run"],
        "Homerun": weights["home_run"],
    }
    xwobacon_ev = pr.map(con_map).fillna(0.0).to_numpy(dtype=float)
    return xba_ev, xtb_ev, xwobacon_ev


def _sac_flags(bbe: pd.DataFrame) -> tuple[pd.Series, pd.Series]:
    """(is_sac_bunt, is_sac_fly) for a BBE frame, TaggedHitType-aware.

    Without a TaggedHitType column every Sacrifice is treated as a sac fly.
    """
    is_sac = bbe["PlayResult"] == "Sacrifice"
    if "TaggedHitType" in bbe.columns:
        is_bunt = bbe["TaggedHitType"] == "Bunt"
    else:
        is_bunt = pd.Series(False, index=bbe.index)
    is_sac_bunt = is_sac & is_bunt
    is_sac_fly = is_sac & ~is_bunt
    return is_sac_bunt, is_sac_fly


def aggregate_xstats(df: pd.DataFrame, boosters: dict, constants: dict) -> dict[str, float | None]:
    """Savant-style aggregate xBA / xSLG / xwOBA over a pitch sub-frame.

    Each PA-terminal event is attributed to the terminal pitch's row, so calling
    this on a per-pitch-type sub-frame attributes outcomes to that pitch type.
    Returns {"xba", "xslg", "xwoba"} with None values when there is no at-bat
    (or, for xwoba alone, no wOBA-denominator plate appearances).
    """
    none: dict[str, float | None] = {"xba": None, "xslg": None, "xwoba": None}
    if "PitchCall" not in df.columns:
        return none

    weights = constants["woba"]["weights"]
    screen = constants["screen"]

    calls = df["PitchCall"]
    korbb = df["KorBB"] if "KorBB" in df.columns else pd.Series("", index=df.index)
    play_result = df["PlayResult"] if "PlayResult" in df.columns else pd.Series(None, index=df.index)

    # Terminal-event masks attributed to this pitch's row.
    bbe_mask = (calls == "InPlay") & play_result.notna() & (play_result != "Undefined")
    # K / BB: KorBB is an official-scoring field that is not always populated
    # on every upload (survives _join_display only IF the source file carried
    # it). Balls/Strikes/PitchCall are core tracked columns, always present
    # when a pitch is tracked at all, and mirror the same derivation
    # league_run_values.pitch_run_values already uses for count-state RV: a
    # called/swinging strike at Strikes>=2 (going into the pitch) is strike
    # three; a ball call at Balls>=3 is ball four. Union with the KorBB signal
    # so either source counting the event is sufficient -- this fixes uploads
    # missing KorBB without changing behavior for uploads that have it.
    if {"Balls", "Strikes"} <= set(df.columns):
        balls = pd.to_numeric(df["Balls"], errors="coerce")
        strikes = pd.to_numeric(df["Strikes"], errors="coerce")
        derived_k = calls.isin(_CSW_CALLS) & strikes.ge(2)
        derived_bb = calls.isin(_BALL_CALLS) & balls.ge(3)
    else:
        derived_k = pd.Series(False, index=df.index)
        derived_bb = pd.Series(False, index=df.index)
    k = int(((korbb == "Strikeout") | derived_k).sum())
    bb = int(((korbb == "Walk") | derived_bb).sum())
    hbp = int((calls == "HitByPitch").sum())

    bbe = df[bbe_mask].copy()
    is_sac_bunt, is_sac_fly = _sac_flags(bbe) if len(bbe) else (
        pd.Series(dtype=bool),
        pd.Series(dtype=bool),
    )
    sf = int(is_sac_fly.sum()) if len(bbe) else 0

    in_ab_mask = ~is_sac_bunt & ~is_sac_fly if len(bbe) else pd.Series(dtype=bool)
    n_ab_bbe = int(in_ab_mask.sum()) if len(bbe) else 0
    ab = n_ab_bbe + k
    woba_den = ab + bb + sf + hbp

    if ab == 0:
        return none

    # Per-BBE expected values: model for tracked, actual fallback for untracked.
    if len(bbe):
        ev = (
            pd.to_numeric(bbe["ExitSpeed"], errors="coerce")
            if "ExitSpeed" in bbe.columns
            else pd.Series(np.nan, index=bbe.index)
        )
        la = (
            pd.to_numeric(bbe["Angle"], errors="coerce")
            if "Angle" in bbe.columns
            else pd.Series(np.nan, index=bbe.index)
        )
        tracked = (ev.notna() & la.notna()).to_numpy()

        xba_ev = np.zeros(len(bbe), dtype=float)
        xtb_ev = np.zeros(len(bbe), dtype=float)
        xwobacon_ev = np.zeros(len(bbe), dtype=float)

        if tracked.any():
            m_xba, m_xtb, m_con = _model_expected(
                boosters, weights, ev.to_numpy()[tracked], la.to_numpy()[tracked], screen
            )
            xba_ev[tracked] = m_xba
            xtb_ev[tracked] = m_xtb
            xwobacon_ev[tracked] = m_con
        if (~tracked).any():
            # Actual fallback computed over all BBE rows (positional), then only
            # the untracked positions are used.
            a_xba, a_xtb, a_con = _actual_expected(weights, bbe["PlayResult"])
            xba_ev[~tracked] = a_xba[~tracked]
            xtb_ev[~tracked] = a_xtb[~tracked]
            xwobacon_ev[~tracked] = a_con[~tracked]

        bbe = bbe.assign(
            _xba_ev=xba_ev,
            _xtb_ev=xtb_ev,
            _xwobacon_ev=xwobacon_ev,
            _is_sac_bunt=is_sac_bunt.to_numpy(),
            _in_ab=in_ab_mask.to_numpy(),
        )
        in_ab_bbe = bbe[bbe["_in_ab"]]
        xba = float(in_ab_bbe["_xba_ev"].sum()) / ab
        xslg = float(in_ab_bbe["_xtb_ev"].sum()) / ab

        # Sac flies stay out of AB but their contact value counts toward xwOBA.
        con_sum = float(bbe[~bbe["_is_sac_bunt"]]["_xwobacon_ev"].sum())
    else:
        xba = 0.0
        xslg = 0.0
        con_sum = 0.0

    xwoba = (
        (con_sum + weights["walk"] * bb + weights["hit_by_pitch"] * hbp) / woba_den
        if woba_den
        else None
    )
    return {"xba": xba, "xslg": xslg, "xwoba": xwoba}
