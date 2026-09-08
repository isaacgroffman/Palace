"""Vendored pitch-physics derivations for the college TrackMan pipeline.

Source: C:/Users/jerem/Desktop/Python/Baseball/OraclePitchProfiler/supabase_pipeline/V12/college-promodeling/features.py
Function bodies are copied verbatim. Only this module docstring and the import
line differ. No runtime import of the V12 tree is performed.
"""

import polars as pl

PLATE_FRONT_FT = 17 / 12  # front edge of home plate, distance from plate tip


def add_zone_time(df: pl.DataFrame) -> pl.DataFrame:
    """Add the production-parity `zoneTime` (release-to-plate flight time) column.

    Identical math to production `tf`; output is named `zoneTime` to match
    comprehensive TrackMan reports. Requires TrackMan columns: vy0, ay0,
    Extension. y0 is used as the null fallback for the release distance,
    matching production's release_pos_y use. Intermediate columns (yR, tR, vyR)
    are computed then dropped.
    """
    out = (
        df.with_columns(
            (60.5 - pl.col("Extension")).fill_null(pl.col("y0")).alias("yR")
        )
        .with_columns(
            (
                (-pl.col("vy0") - (pl.col("vy0") ** 2 - 2 * pl.col("ay0") * (50 - pl.col("yR"))).sqrt())
                / pl.col("ay0")
            ).alias("tR")
        )
        .with_columns((pl.col("vy0") + pl.col("ay0") * pl.col("tR")).alias("vyR"))
        .with_columns(
            (
                (-pl.col("vyR") - (pl.col("vyR") ** 2 - 2 * pl.col("ay0") * (pl.col("yR") - PLATE_FRONT_FT)).sqrt())
                / pl.col("ay0")
            ).alias("zoneTime")
        )
        .drop(["yR", "tR", "vyR"])
    )
    return out


def add_induced_acceleration(df: pl.DataFrame) -> pl.DataFrame:
    """Add induced horizontal/vertical acceleration (ft/s^2).

    Production parity: `database_updater_supabase.add_release_metrics_polars`,
        horizontal_acceleration       = 2 * (hb / 12) / tf**2
        induced_vertical_acceleration = 2 * (induced_vb / 12) / tf**2
    "Induced" = gravity-removed (InducedVertBreak already has gravity removed).
    Requires HorzBreak, InducedVertBreak (inches) and zoneTime (== production tf).
    """
    return df.with_columns([
        (2 * (pl.col("HorzBreak") / 12) / pl.col("zoneTime") ** 2).alias("horizontal_acceleration"),
        (2 * (pl.col("InducedVertBreak") / 12) / pl.col("zoneTime") ** 2).alias("induced_vertical_acceleration"),
    ])


def add_magnus_movement(df: pl.DataFrame, *, x_view: str = "pitcher") -> pl.DataFrame:
    """Add Magnus / non-Magnus break (inches) and acceleration (ft/s^2).

    Production parity: `database_updater_supabase.add_magnus_movement`, with
    TrackMan column names (HorzBreak->hb, InducedVertBreak->induced_vb,
    SpinAxis->spin_axis, zoneTime->tf). Decomposes the observed-movement vector
    into the component along the MEASURED spin axis's Magnus direction (magnus_*)
    and the residual (non_magnus_*, the seam-shifted-wake / drag-asymmetry part).

    x_view="pitcher" is VALIDATED for this TrackMan data: fastball
    non_magnus/observed is ~3.3% (near-pure Magnus, as physics requires);
    "catcher" gives ~87% and is wrong. Do not change without re-validating.
    """
    d = df.with_columns([
        (pl.col("HorzBreak").cast(pl.Float64) / 12.0).alias("obs_x_ft"),
        (pl.col("InducedVertBreak").cast(pl.Float64) / 12.0).alias("obs_z_ft"),
        (pl.col("SpinAxis").cast(pl.Float64) * (3.14159265358979 / 180.0)).alias("theta"),
    ])
    dir_x_cv = pl.col("theta").sin()
    dir_z = -(pl.col("theta").cos())
    dir_x = pl.when(pl.lit(x_view) == "pitcher").then(-dir_x_cv).otherwise(dir_x_cv)
    d = d.with_columns([dir_x.alias("mdx"), dir_z.alias("mdz")])
    d = d.with_columns(
        (pl.col("obs_x_ft") * pl.col("mdx") + pl.col("obs_z_ft") * pl.col("mdz")).alias("mag_ft")
    )
    d = d.with_columns([
        (pl.col("mag_ft") * pl.col("mdx")).alias("magnus_x_ft"),
        (pl.col("mag_ft") * pl.col("mdz")).alias("magnus_z_ft"),
        (pl.col("obs_x_ft") - pl.col("mag_ft") * pl.col("mdx")).alias("non_magnus_x_ft"),
        (pl.col("obs_z_ft") - pl.col("mag_ft") * pl.col("mdz")).alias("non_magnus_z_ft"),
    ])
    # breaks in inches
    d = d.with_columns([
        (pl.col("magnus_x_ft") * 12.0).alias("magnus_horizontal_break"),
        (pl.col("magnus_z_ft") * 12.0).alias("magnus_vertical_break"),
        (pl.col("non_magnus_x_ft") * 12.0).alias("non_magnus_horizontal_break"),
        (pl.col("non_magnus_z_ft") * 12.0).alias("non_magnus_vertical_break"),
    ])
    # accelerations (ft/s^2): a = 2*d / tf^2
    comps = [
        ("magnus_x_ft", "magnus_horizontal_acceleration"),
        ("magnus_z_ft", "magnus_vertical_acceleration"),
        ("non_magnus_x_ft", "non_magnus_horizontal_acceleration"),
        ("non_magnus_z_ft", "non_magnus_vertical_acceleration"),
    ]
    d = d.with_columns([
        pl.when((pl.col("zoneTime") > 0) & pl.col(src).is_not_null())
        .then(2.0 * pl.col(src) / (pl.col("zoneTime") ** 2))
        .otherwise(None)
        .alias(dst)
        for src, dst in comps
    ])
    d = d.drop([
        "obs_x_ft", "obs_z_ft", "theta", "mdx", "mdz", "mag_ft",
        "magnus_x_ft", "magnus_z_ft", "non_magnus_x_ft", "non_magnus_z_ft",
    ])
    return d
