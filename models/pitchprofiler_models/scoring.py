"""Multi-stage proStuff/proPitching/proLocation scorer (serving subset, College by default).

Vendored VERBATIM from the V12 college-promodeling cv_lib.py serving path, with
ONE edit: the regression `foil` model is dropped (it was a training-time foil
only). `score` returns exactly {stuff_xrv, pitching_xrv, location_xrv} and never
references models["foil"]. The stuff/pitching/location math is byte-identical to
the original, so a golden captured from the original (which also emitted
foil_xrv) reproduces exactly here.

Only the inference subset is ported: the feature-set constants, the categorical
frame builder, the per-count pitch-weight resolver, scoring, plus scaling, and
the pitcher-season aggregation/grade. Training-only code (train_*,
recompute_weights, plus_scale_stats, compute_plus_reference, load_data, CV
helpers, production loaders) is intentionally absent.

ASCII only. LightGBM boosters and the weights dict are supplied by the caller.
"""

import numpy as np
import pandas as pd

# ----------------------------------------------------------------------------
# Feature sets
# ----------------------------------------------------------------------------
# proStuff: raw physical stuff only. NO plate location, NO count, NO sequencing.
STUFF_FEATURES = [
    "release_speed",
    "induced_vertical_acceleration",
    "horizontal_acceleration_normalized",
    "release_spin_rate",
    "release_pos_z",
    "release_pos_x_normalized",
    "release_extension",
    "hand_split",
    "p_throws",
]

# proPitching: the 8 stuff features plus location, approach angles, count, sequencing.
PITCH_FEATURES = [
    "release_speed",
    "induced_vertical_acceleration",
    "horizontal_acceleration_normalized",
    "release_spin_rate",
    "release_pos_z",
    "release_pos_x_normalized",
    "release_extension",
    "hand_split",
    "plate_x_normalized",
    "plate_z_normalized",
    "vra",
    "hra_normalized",
    "vra_diff",
    "hra_normalized_diff",
    "times_through_order",
    "times_seen_pitch_type",
    "balls",
    "strikes",
    "outs_when_up",
    "p_throws",
]

# Categorical features (subset of whichever feature set is used).
CATEGORICAL = ["hand_split", "p_throws", "balls", "strikes", "outs_when_up"]

# Fixed, explicit categories so LightGBM integer codes are deterministic across
# train and any future report (a plain .astype('category') would assign codes
# from whatever values happen to be present, which is unsafe at inference time).
# Values outside these lists become NaN (LightGBM treats them as missing).
FIXED_CATEGORIES = {
    "hand_split": ["SHH", "OHH"],
    "p_throws": ["R", "L"],
    "balls": [0, 1, 2, 3],
    "strikes": [0, 1, 2],
    "outs_when_up": [0, 1, 2],
}

# Hierarchical fallback threshold for per-count pitching weights.
SPARSE_CELL_MIN = 30


# ----------------------------------------------------------------------------
# Feature frame builder (categoricals as pandas category for LightGBM)
# ----------------------------------------------------------------------------
def _make_X(df, feature_list):
    X = df[feature_list].copy()
    for c in feature_list:
        if c in CATEGORICAL:
            cats = FIXED_CATEGORIES[c]
            # numeric categoricals (balls/strikes/outs) must be coerced to a
            # numeric dtype first so values match the integer category list.
            if c not in ("hand_split", "p_throws"):
                vals = pd.to_numeric(X[c], errors="coerce")
            else:
                vals = X[c]
            # Fixed categories -> deterministic codes; out-of-list -> NaN.
            X[c] = pd.Categorical(vals, categories=cats)  # type: ignore[call-overload]  # pandas-stubs overload gap
        else:
            X[c] = pd.to_numeric(X[c], errors="coerce").astype("float64")
    return X


# ----------------------------------------------------------------------------
# Per-count pitch-weight resolution (hierarchical fallback)
# ----------------------------------------------------------------------------
def _resolve_pitch_weight(weights, name, b, s, o):
    cell, marg, global_mean = weights["pitching"][name]
    c = cell.get((b, s, o))
    if c is not None and c[1] >= SPARSE_CELL_MIN:
        return c[0]
    mg = marg.get((b, s))
    if mg is not None and mg[1] >= SPARSE_CELL_MIN:
        return mg[0]
    if c is not None:
        return c[0]
    if mg is not None:
        return mg[0]
    return global_mean


def _pitch_weight_vec(weights, name, df):
    # Coerce the count columns to numeric first. A sparse/MiLB feed can deliver
    # them as object dtype (the same null-out bug class _make_X guards), and
    # np.unique(axis=0) below rejects object arrays. Clean integer columns are a
    # no-op; out-of-list values become NaN and resolve to the global fallback,
    # matching _resolve_pitch_weight's dict-miss path.
    bs = pd.to_numeric(df["balls"], errors="coerce").to_numpy()
    ss = pd.to_numeric(df["strikes"], errors="coerce").to_numpy()
    os_ = pd.to_numeric(df["outs_when_up"], errors="coerce").to_numpy()
    out = np.empty(len(df), dtype="float64")
    if out.size == 0:
        return out
    # There are at most 36 distinct (balls, strikes, outs) combos. Resolve the
    # weight once per unique combo, then scatter back to every row via the
    # inverse index (no per-row Python loop).
    combos = np.stack([bs, ss, os_], axis=1)
    uniq, inverse = np.unique(combos, axis=0, return_inverse=True)
    inverse = inverse.ravel()
    resolved = np.empty(len(uniq), dtype="float64")
    for i in range(len(uniq)):
        b, s, o = uniq[i]
        resolved[i] = _resolve_pitch_weight(weights, name, b, s, o)
    np.copyto(out, resolved[inverse])
    return out


# ----------------------------------------------------------------------------
# Scoring -> per-pitch xRV arrays
# ----------------------------------------------------------------------------
def score(df, models, weights):
    """Score any df (train or held-out) with the given models + weights.
    Returns dict of np arrays: stuff_xrv, pitching_xrv, location_xrv.
    """
    Xp = _make_X(df, PITCH_FEATURES)
    Xs = _make_X(df, STUFF_FEATURES)

    # Stage probabilities
    p_swing = models["stage1"].predict(Xp)  # P(swing)
    p_swing = np.clip(p_swing, 0.0, 1.0)

    s2p = models["stage2_pitch"].predict(Xp)  # (n,3): whiff,foul,in_play
    s2s = models["stage2_stuff"].predict(Xs)
    s2t = models["stage2t"].predict(Xp)       # (n,3): cs,ball,hbp

    p_hr_pitch = np.clip(models["hr_pitch"].predict(Xp), 0.0, 1.0)
    p_hr_stuff = np.clip(models["hr_stuff"].predict(Xs), 0.0, 1.0)

    # ---- STUFF: scalar weights, drop non-HR in-play term ----
    Ww = weights["W_whiff_stuff"]
    Wf = weights["W_foul_stuff"]
    HR_RV = weights["HR_RV"]
    p_whiff_s = s2s[:, 0]
    p_foul_s = s2s[:, 1]
    p_ip_s = s2s[:, 2]
    stuff_xrv = p_whiff_s * Ww + p_foul_s * Wf + p_ip_s * p_hr_stuff * HR_RV

    # ---- PITCHING: per-count weights ----
    Ww_p = _pitch_weight_vec(weights, "whiff", df)
    Wf_p = _pitch_weight_vec(weights, "foul", df)
    Wcs = _pitch_weight_vec(weights, "cs", df)
    Wball = _pitch_weight_vec(weights, "ball", df)
    Whbp = _pitch_weight_vec(weights, "hbp", df)
    WHR = _pitch_weight_vec(weights, "HR", df)

    p_whiff_p = s2p[:, 0]
    p_foul_p = s2p[:, 1]
    p_ip_p = s2p[:, 2]
    swing_xrv = (
        p_whiff_p * Ww_p
        + p_foul_p * Wf_p
        + p_ip_p * p_hr_pitch * WHR
    )

    p_cs = s2t[:, 0]
    p_ball = s2t[:, 1]
    p_hbp = s2t[:, 2]
    take_xrv = p_cs * Wcs + p_ball * Wball + p_hbp * Whbp

    pitching_xrv = p_swing * swing_xrv + (1.0 - p_swing) * take_xrv

    location_xrv = pitching_xrv - stuff_xrv

    return {
        "stuff_xrv": stuff_xrv,
        "pitching_xrv": pitching_xrv,
        "location_xrv": location_xrv,
    }


# ----------------------------------------------------------------------------
# Plus scaling (pitcher-only buckets)
# ----------------------------------------------------------------------------
def to_plus(xrv, mean_xrv, std_xrv):
    # lower xRV is better -> elite (low) xRV gives plus > 100
    return 100.0 + (mean_xrv - np.asarray(xrv, dtype="float64")) / std_xrv * 10.0


# ----------------------------------------------------------------------------
# Pitcher-level (pitcher-season) aggregation and plus grade
# ----------------------------------------------------------------------------
# The plus is a pitcher-season GRADE, not a per-pitch value. These functions
# aggregate per-pitch xRV to (pitcher, season) means and grade those aggregates
# against a between-pitcher-season reference (built once at production refit).
_XRV_METRICS = ["stuff_xrv", "pitching_xrv", "location_xrv"]
_METRIC_TO_PLUS = {
    "stuff_xrv": "proStuff+",
    "pitching_xrv": "proPitching+",
    "location_xrv": "proLocation+",
}


def aggregate_xrv(scored_df, group_cols=("Pitcher", "game_year")):
    """Aggregate per-pitch xRV to group means plus a pitch count n.

    scored_df must carry the group_cols plus the xRV columns. foil_xrv is
    aggregated too when present. Returns one row per group.
    """
    group_cols = list(group_cols)
    metrics = [m for m in (_XRV_METRICS + ["foil_xrv"]) if m in scored_df.columns]
    g = scored_df.groupby(group_cols, dropna=True)
    agg = g[metrics].mean()
    agg["n"] = g.size()
    agg = agg.reset_index()
    # column order: group_cols, n, then the metric means
    return agg[group_cols + ["n"] + metrics]


def apply_pitcher_plus(agg_df, reference):
    """Add proStuff+/proPitching+/proLocation+ to an aggregate frame.

    Each plus grades the aggregate mean xRV against the reference mean/std via
    to_plus (lower xRV -> higher plus; sign not flipped).
    """
    out = agg_df.copy()
    for metric, short in (("stuff_xrv", "stuff"),
                          ("pitching_xrv", "pitching"),
                          ("location_xrv", "location")):
        plus_col = _METRIC_TO_PLUS[metric]
        ref = reference[short]
        out[plus_col] = to_plus(out[metric].values, ref["mean"], ref["std"])
    return out
