"""Single TrackMan cleaning core shared by training and serving.

Same code in equals same features out: the two training scripts
(build_eligible_parquet.py, build_model_features.py) and the serving entry
point process_frame() all call these functions, so a dragged-and-dropped
TrackMan report is cleaned by exactly the transforms that built the training
set. Pure transforms take and return a pl.DataFrame. This module has no
filesystem access of its own; the caller supplies the stadium map / park-factor
reference tables. ASCII only.
"""
import polars as pl

from . import physics

K5 = ["GameID", "Inning", "BatterTeam", "PAofInning", "PitchofPA"]

# (name, column, lo, hi): the 8 plausibility clips, authoritative here.
CLIPS = [
    ("RelSpeed", "RelSpeed", 65, 105),
    ("InducedVertBreak", "InducedVertBreak", -30, 30),
    ("HorzBreak", "HorzBreak", -30, 30),
    ("Extension", "Extension", 4, 8.5),
    ("RelHeight", "RelHeight", 0, 8),
    ("RelSide", "RelSide", -4, 4),
    ("PlateLocSide", "PlateLocSide", -4, 4),
    ("PlateLocHeight", "PlateLocHeight", -2, 6),
]

IDS = ["GameID", "Inning", "BatterTeam", "PAofInning", "PitchofPA", "game_year",
       "Pitcher", "Batter", "p_throws", "stand", "TaggedPitchType"]
FEATURES = [
    "release_speed", "induced_vertical_acceleration", "horizontal_acceleration_normalized",
    "release_spin_rate", "release_pos_z", "release_pos_x_normalized", "release_extension",
    "hand_split", "plate_x_normalized", "plate_z_normalized", "vra", "hra_normalized",
    "vra_diff", "hra_normalized_diff", "times_through_order", "times_seen_pitch_type",
    "balls", "strikes", "outs_when_up",
]


def _p_throws_expr() -> pl.Expr:
    """Per-pitch throwing hand. 'Both' (switch pitcher) resolves to the hand
    used on THIS pitch via the release-side sign (RHP positive, LHP negative),
    not the season label. Unknown handedness yields null (flagged downstream).
    """
    return (pl.when(pl.col("PitcherThrows") == "Both")
              .then(pl.when(pl.col("RelSide") > 0).then(pl.lit("R")).otherwise(pl.lit("L")))
            .when(pl.col("PitcherThrows") == "Right").then(pl.lit("R"))
            .when(pl.col("PitcherThrows") == "Left").then(pl.lit("L"))
            .otherwise(None))


def add_game_year(df: pl.DataFrame) -> pl.DataFrame:
    return df.with_columns(pl.col("GameID").str.slice(0, 4).cast(pl.Int32).alias("game_year"))


def dedup_exact(df: pl.DataFrame) -> pl.DataFrame:
    """Full-row dedup: 56 games log each pitch twice. Removing exact duplicates
    keeps the model from training on doubled pitches and keeps sequencing
    running counts from inflating about 2x."""
    return df.unique()


def derive_handedness(df: pl.DataFrame) -> pl.DataFrame:
    """Add per-pitch p_throws and stand ('R'/'L'); unknown handedness -> null."""
    return df.with_columns([
        _p_throws_expr().alias("p_throws"),
        pl.when(pl.col("BatterSide") == "Right").then(pl.lit("R"))
          .when(pl.col("BatterSide") == "Left").then(pl.lit("L"))
          .otherwise(None).alias("stand"),
    ])


DERIVED_NEW = [
    "horizontal_acceleration", "induced_vertical_acceleration",
    "magnus_horizontal_break", "magnus_vertical_break",
    "non_magnus_horizontal_break", "non_magnus_vertical_break",
    "magnus_horizontal_acceleration", "magnus_vertical_acceleration",
    "non_magnus_horizontal_acceleration", "non_magnus_vertical_acceleration",
]


def apply_park_correction_stage(df, smap, pf, scale_ivb=1.0, scale_hb=1.0):
    """Join the stadium map and park-factor calibration, then additively correct
    movement on the raw IVB and HB axes. Venues without a park factor get 0
    correction (corrected == raw). scale_* default 1.0 (full unbiased correction)."""
    df = (df.join(smap, left_on="Stadium", right_on="raw_stadium", how="left")
            .join(pf, on="canonical_venue", how="left")
            .with_columns([pl.col("ivb_calib").fill_null(0.0),
                           pl.col("hb_calib_v2").fill_null(0.0)]))
    return df.with_columns([
        (pl.col("InducedVertBreak") - scale_ivb * pl.col("ivb_calib")).alias("InducedVertBreak_corr"),
        (pl.col("HorzBreak") - scale_hb * pl.col("hb_calib_v2")).alias("HorzBreak_corr"),
    ])


def add_derived_features(df):
    """Add zoneTime, then recompute induced + Magnus/non-Magnus features from the
    CORRECTED movement by feeding the corrected columns under the raw names the
    feature functions expect. Mirrors build_eligible_parquet exactly."""
    df = physics.add_zone_time(df)
    feat_in = df.with_columns([
        pl.col("InducedVertBreak_corr").alias("InducedVertBreak"),
        pl.col("HorzBreak_corr").alias("HorzBreak"),
    ])
    feat_in = physics.add_induced_acceleration(feat_in)
    feat_in = physics.add_magnus_movement(feat_in)
    return df.hstack(feat_in.select(DERIVED_NEW))


def normalize_features(df):
    """Add the model's per-pitch features with RHP normalization. The L-flip plus
    two extra negations (hra, plate_x) are validated vs savant_data and Hawk-Eye.
    Lifted verbatim from build_model_features so train and serve are identical."""
    isL = pl.col("p_throws") == "L"
    return df.with_columns([
        pl.col("RelSpeed").alias("release_speed"),
        pl.col("induced_vertical_acceleration"),
        pl.when(isL).then(-pl.col("horizontal_acceleration")).otherwise(pl.col("horizontal_acceleration"))
          .alias("horizontal_acceleration_normalized"),
        pl.col("SpinRate").alias("release_spin_rate"),
        pl.col("RelHeight").alias("release_pos_z"),
        pl.when(isL).then(-pl.col("RelSide")).otherwise(pl.col("RelSide")).alias("release_pos_x_normalized"),
        pl.col("Extension").alias("release_extension"),
        pl.when(pl.col("p_throws") == pl.col("stand")).then(pl.lit("SHH")).otherwise(pl.lit("OHH")).alias("hand_split"),
        pl.when(isL).then(pl.col("PlateLocSide")).otherwise(-pl.col("PlateLocSide")).alias("plate_x_normalized"),
        pl.col("PlateLocHeight").alias("plate_z_normalized"),
        pl.col("VertRelAngle").alias("vra"),
        pl.when(isL).then(pl.col("HorzRelAngle")).otherwise(-pl.col("HorzRelAngle")).alias("hra_normalized"),
        pl.col("Balls").cast(pl.Int32).alias("balls"),
        pl.col("Strikes").cast(pl.Int32).alias("strikes"),
        pl.col("Outs").cast(pl.Int32).alias("outs_when_up"),
    ])


def compute_sequencing(df):
    """Sequencing features (vra_diff, hra_normalized_diff, times_through_order,
    times_seen_pitch_type) on the given pitch stream, keyed by K5.

    The caller selects the stream: train passes the eligibility-filtered, deduped
    whole-game stream; serve passes the full deduped report. This function never
    filters, so it runs on the complete stream before any pitch is excluded,
    keeping a dropped pitch from corrupting a neighbor's diff or running count.
    Lifted verbatim from build_model_features.build_sequencing."""
    df = df.select(K5 + ["Pitcher", "Batter", "PitcherThrows", "RelSide",
                         "VertRelAngle", "HorzRelAngle", "TaggedPitchType"])
    df = df.with_columns(_p_throws_expr().alias("p_throws"))
    isL = pl.col("p_throws") == "L"
    df = df.with_columns([
        pl.col("VertRelAngle").alias("vra"),
        pl.when(isL).then(pl.col("HorzRelAngle")).otherwise(-pl.col("HorzRelAngle")).alias("hra_normalized"),
        pl.concat_str([pl.col("GameID"), pl.col("Inning").cast(pl.Utf8),
                       pl.col("BatterTeam"), pl.col("PAofInning").cast(pl.Utf8)],
                      separator="|").alias("_pa_uid"),
        (pl.col("Inning") * 100 + pl.col("PAofInning")).alias("_ab"),
    ])
    # Tie-break on all available columns so K5-collision pitches (same PitchofPA
    # but different physical measurements) land in a deterministic order before
    # .diff() is computed; without this the diff values themselves vary across runs.
    df = df.sort(["GameID", "Inning", "BatterTeam", "PAofInning", "PitchofPA",
                  "VertRelAngle", "HorzRelAngle", "RelSide", "TaggedPitchType",
                  "Pitcher", "Batter"], nulls_last=True)
    df = df.with_columns([
        pl.col("vra").diff().over("_pa_uid").alias("vra_diff"),
        pl.col("hra_normalized").diff().over("_pa_uid").alias("hra_normalized_diff"),
        pl.col("_ab").rank("dense").over(["GameID", "Pitcher", "Batter"]).cast(pl.Int32).alias("times_through_order"),
        (pl.col("GameID").cum_count().over(["GameID", "Pitcher", "Batter", "TaggedPitchType"]).cast(pl.Int64) - 1)
            .cast(pl.Int32).alias("times_seen_pitch_type"),
    ])
    seq = df.select(K5 + ["vra_diff", "hra_normalized_diff", "times_through_order", "times_seen_pitch_type"])
    # deterministic tie-break among K5-collision pitches so the kept row is
    # reproducible across runs and identical in training and serving (the
    # source stream can carry multiple physical pitches under one K5 key).
    seq = seq.sort(
        K5 + ["vra_diff", "hra_normalized_diff", "times_through_order", "times_seen_pitch_type"],
        nulls_last=True,
    )
    return seq.unique(subset=K5, keep="first", maintain_order=True)


# features nulled (serve flag-and-keep) when a check fails. Extension also feeds
# zoneTime, so its failure nulls the zoneTime-derived accelerations too.
_GAMESTATE_NULLS = ["balls", "strikes", "outs_when_up"]
_HAND_NULLS = ["release_pos_x_normalized", "horizontal_acceleration_normalized",
               "plate_x_normalized", "hra_normalized", "hand_split"]
_CLIP_NULLS = {
    "RelSpeed": ["release_speed"],
    "InducedVertBreak": ["induced_vertical_acceleration"],
    "HorzBreak": ["horizontal_acceleration_normalized"],
    "Extension": ["release_extension", "induced_vertical_acceleration",
                  "horizontal_acceleration_normalized"],
    "RelHeight": ["release_pos_z"],
    "RelSide": ["release_pos_x_normalized"],
    "PlateLocSide": ["plate_x_normalized"],
    "PlateLocHeight": ["plate_z_normalized"],
}


def _gamestate_ok():
    return (pl.col("Balls").is_between(0, 3) & pl.col("Strikes").is_between(0, 2)
            & pl.col("Outs").is_between(0, 2))


def _call_ok():
    return pl.col("PitchCall") != "Undefined"


def _hand_ok():
    return (pl.col("PitcherThrows").is_in(["Right", "Left", "Both"])
            & pl.col("BatterSide").is_in(["Right", "Left"]))


def _clip_ok(col, lo, hi):
    return pl.col(col).is_between(lo, hi)


def _all_clips_ok():
    e = None
    for _, col, lo, hi in CLIPS:
        c = _clip_ok(col, lo, hi)
        e = c if e is None else (e & c)
    return e


def flag_plausibility(df, mode):
    """Apply the validity checks. mode='train' drops failing rows. mode='serve'
    keeps every row, adds `valid` and `invalid_reason` (first failing check in
    order: game state, undefined call, handedness, then the 8 clips), and nulls
    the features each failed check makes untrustworthy."""
    if mode == "train":
        return df.filter(_gamestate_ok() & _call_ok() & _hand_ok() & _all_clips_ok())
    if mode != "serve":
        raise ValueError(f"mode must be 'train' or 'serve', got {mode!r}")

    reason = (pl.when(~_gamestate_ok()).then(pl.lit("invalid count/outs"))
                .when(~_call_ok()).then(pl.lit("undefined PitchCall"))
                .when(~_hand_ok()).then(pl.lit("missing handedness")))
    for name, col, lo, hi in CLIPS:
        reason = reason.when(~_clip_ok(col, lo, hi)).then(pl.lit(f"{name} out of [{lo},{hi}]"))
    reason = reason.otherwise(None).alias("invalid_reason")  # type: ignore[assignment]  # Then rebound as Expr
    valid = (_gamestate_ok() & _call_ok() & _hand_ok() & _all_clips_ok()).alias("valid")
    # Serve-only companion flag: validity ignoring the PitchCall check. The call
    # is an OUTCOME, not a model input, so a pitch whose only defect is an
    # undefined call is fully scoreable; the dashboard keeps such pitches and
    # excludes them from call-based rate denominators instead of dropping them.
    # `valid` itself is unchanged (training parity).
    valid_except_call = (_gamestate_ok() & _hand_ok() & _all_clips_ok()).alias("valid_except_call")
    df = df.with_columns([valid, valid_except_call, reason])

    df = df.with_columns([pl.when(~_gamestate_ok()).then(None).otherwise(pl.col(f)).alias(f)
                          for f in _GAMESTATE_NULLS])
    df = df.with_columns([pl.when(~_hand_ok()).then(None).otherwise(pl.col(f)).alias(f)
                          for f in _HAND_NULLS])
    for name, col, lo, hi in CLIPS:
        bad = ~_clip_ok(col, lo, hi)
        df = df.with_columns([pl.when(bad).then(None).otherwise(pl.col(f)).alias(f)
                              for f in _CLIP_NULLS[name]])
    return df


# Some TrackMan exports report plate location in inches, others in feet; training
# used feet (the plausibility clips and the normalized plate features assume feet).
# A median PlateLocHeight above this many feet is physically impossible (no pitch
# crosses the plate at 8+ ft median) and is the signature of an inches export.
PLATE_INCHES_THRESHOLD = 8.0


def normalize_plate_units(df: pl.DataFrame) -> pl.DataFrame:
    """Auto-detect plate-location units and convert to feet (the training unit).

    Detection uses the median of PlateLocHeight: about 2.4 ft in a feet export,
    about 29 in an inches export. Above PLATE_INCHES_THRESHOLD ft flags inches,
    and both plate-location columns (they share one unit) are divided by 12.
    Serve-only: training data is known-feet, so this is never called from the
    training path and a feet report is returned unchanged.
    """
    h = df["PlateLocHeight"].drop_nulls()
    med = h.median()
    # Series.median() typed as wide scalar union float() rejects; None check is the only runtime narrowing.
    med_ft = None if med is None else abs(float(med))  # type: ignore[arg-type]
    if h.len() and med_ft is not None and med_ft > PLATE_INCHES_THRESHOLD:
        df = df.with_columns([
            (pl.col("PlateLocSide") / 12.0).alias("PlateLocSide"),
            (pl.col("PlateLocHeight") / 12.0).alias("PlateLocHeight"),
        ])
    return df


def process_frame(df, smap=None, pf=None, *, apply_park_correction=True):
    """Serving entry: clean a raw TrackMan polars frame into the 19 model
    features (flag-and-keep). Vendored from trackman_clean.process_report;
    takes an in-memory frame instead of a parquet path. The caller supplies
    the stadium map / park-factor tables (see LeagueModelRepository.venue_tables);
    this module has no filesystem access of its own.

    apply_park_correction: policy switch, ON by default (College behavior).
    When False, or when smap/pf are not supplied, the corrected movement
    columns pass through unchanged (corrected == raw), matching what a
    null-venue join would have produced."""
    df = add_game_year(df)
    df = dedup_exact(df)
    df = normalize_plate_units(df)
    seq = compute_sequencing(df)
    df = derive_handedness(df)
    if apply_park_correction and smap is not None and pf is not None:
        df = apply_park_correction_stage(df, smap, pf)
    else:
        df = df.with_columns([
            pl.col("InducedVertBreak").alias("InducedVertBreak_corr"),
            pl.col("HorzBreak").alias("HorzBreak_corr"),
        ])
    df = add_derived_features(df)
    df = normalize_features(df)
    df = flag_plausibility(df, mode="serve")
    df = df.join(seq, on=K5, how="left")
    return df.select(IDS + FEATURES + ["valid", "valid_except_call", "invalid_reason"])
