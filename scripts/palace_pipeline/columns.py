"""Master TrackMan v3 parquet (dotted API names) -> TrackMan CSV names the
Pitch Profiler bundle expects, plus the value clean-up the raw feed needs.

Nothing here drops a row. Bad values are normalized or left for the bundle's
flag-and-keep plausibility checks.
"""
from __future__ import annotations

import re

import polars as pl

# master column -> TrackMan CSV column (the bundle's REQUIRED_RAW_COLUMNS + outcome columns)
MODEL_MAP = {
    "pitchUID": "PitchUID",
    "gameID": "GameID",
    "date": "Date",
    "sessionType": "SessionType",
    "taggerBehavior.pitchNo": "PitchNo",
    "gameState.inning": "Inning",
    "batter.team": "BatterTeam",
    "taggerBehavior.pAofinning": "PAofInning",
    "taggerBehavior.pitchofPA": "PitchofPA",
    "pitcher.name": "Pitcher",
    "pitcher.id": "PitcherId",
    "pitcher.team": "PitcherTeam",
    "batter.name": "Batter",
    "batter.id": "BatterId",
    "pitcher.throws": "PitcherThrows",
    "batter.side": "BatterSide",
    "pitchTag.taggedPitchType": "TaggedPitchType",
    "pitchTag.autoPitchType": "AutoPitchType",
    "pitch.release.relSpeed": "RelSpeed",
    "pitch.release.spinRate": "SpinRate",
    "pitch.release.relHeight": "RelHeight",
    "pitch.release.relSide": "RelSide",
    "pitch.release.extension": "Extension",
    "pitch.movement.inducedVertBreak": "InducedVertBreak",
    "pitch.movement.horzBreak": "HorzBreak",
    "pitch.movement.spinAxis": "SpinAxis",
    "pitch.location.plateLocSide": "PlateLocSide",
    "pitch.location.plateLocHeight": "PlateLocHeight",
    "pitch.release.vertRelAngle": "VertRelAngle",
    "pitch.release.horzRelAngle": "HorzRelAngle",
    "gameState.balls": "Balls",
    "gameState.strikes": "Strikes",
    "gameState.outs": "Outs",
    "pitchTag.pitchCall": "PitchCall",
    "pitch.nineP.v0.y": "vy0",
    "pitch.nineP.a0.y": "ay0",
    "pitch.nineP.x0.y": "y0",
    "korBB": "KorBB",
    "playResult.playResult": "PlayResult",
    "hitTag.taggedHitType": "TaggedHitType",
    "hit.launch.exitSpeed": "ExitSpeed",
    "hit.launch.angle": "Angle",
    "location.field.name": "FieldName",
    "location.venue.name": "Venue",
    "homeTeam.shortName": "HomeTeam",
    "awayTeam.shortName": "AwayTeam",
    "level.name": "Level",
    "league.name": "League",
}

# Full master -> snake_case names for the per-pitch Supabase table.
def snake(name: str) -> str:
    s = name.replace(".", "_")
    s = re.sub(r"(?<=[a-z0-9])([A-Z])", r"_\1", s)
    s = re.sub(r"_+", "_", s).lower()
    return s


SNAKE_OVERRIDES = {
    "pitchUID": "pitch_uid",
    "playID": "play_id",
    "sessionId": "session_id",
    "gameID": "game_id",
    "korBB": "kor_bb_raw",
    "pitcher.throws": "pitcher_throws_raw",
    "batter.side": "batter_side_raw",
    "taggerBehavior.pitchNo": "pitch_no",
    "taggerBehavior.pAofinning": "pa_of_inning",
    "taggerBehavior.pitchofPA": "pitch_of_pa",
    "gameState.topBottom": "top_bottom",
    "gameState.inning": "inning",
    "gameState.outs": "outs_raw",
    "gameState.balls": "balls_raw",
    "gameState.strikes": "strikes_raw",
    "pitchTag.taggedPitchType": "tagged_pitch_type_raw",
    "pitchTag.autoPitchType": "auto_pitch_type",
    "pitchTag.pitchCall": "pitch_call_raw",
    "hitTag.taggedHitType": "tagged_hit_type_raw",
    "hitTag.autoHitType": "auto_hit_type",
    "playResult.playResult": "play_result_raw",
    "playResult.outsOnPlay": "outs_on_play",
    "playResult.runsScored": "runs_scored",
    "pitch.nineP.pfxx": "pfxx",
    "pitch.nineP.pfxz": "pfxz",
    "pitch.nineP.x0.x": "x0",
    "pitch.nineP.x0.y": "y0",
    "pitch.nineP.x0.z": "z0",
    "pitch.nineP.v0.x": "vx0",
    "pitch.nineP.v0.y": "vy0",
    "pitch.nineP.v0.z": "vz0",
    "pitch.nineP.a0.x": "ax0",
    "pitch.nineP.a0.y": "ay0",
    "pitch.nineP.a0.z": "az0",
    "pitch.location.zoneTime": "zone_time",
    "pitch.location.zoneSpeed": "zone_speed",
    "pitch.location.vertApprAngle": "vert_appr_angle",
    "pitch.location.horzApprAngle": "horz_appr_angle",
    "pitch.location.plateLocHeight": "plate_loc_height",
    "pitch.location.plateLocSide": "plate_loc_side",
    "pitch.location.confidence": "location_confidence",
    "pitch.release.confidence": "release_confidence",
    "pitch.movement.confidence": "movement_confidence",
    "pitch.release.relSpeed": "rel_speed",
    "pitch.release.spinRate": "spin_rate",
    "pitch.release.extension": "extension",
    "pitch.release.vertRelAngle": "vert_rel_angle",
    "pitch.release.horzRelAngle": "horz_rel_angle",
    "pitch.release.relHeight": "rel_height",
    "pitch.release.relSide": "rel_side",
    "pitch.movement.horzBreak": "horz_break",
    "pitch.movement.vertBreak": "vert_break",
    "pitch.movement.inducedVertBreak": "induced_vert_break",
    "pitch.movement.spinAxis": "spin_axis",
    "pitch.movement.tilt": "tilt",
    "pitch.speedDrop": "speed_drop",
    "pitch.effectiveVelo": "effective_velo",
    "hit.lastTrackedDistance": "hit_last_tracked_distance",
    "hit.maxHeight": "hit_max_height",
    "hit.launch.exitSpeed": "exit_speed",
    "hit.launch.hitSpinRate": "hit_spin_rate",
    "hit.launch.angle": "launch_angle",
    "hit.launch.direction": "launch_direction",
    "hit.launch.hitSpinAxis": "hit_spin_axis",
    "hit.launch.confidence": "hit_confidence",
    "hit.launch.contactPosition.x": "contact_position_x",
    "hit.launch.contactPosition.y": "contact_position_y",
    "hit.launch.contactPosition.z": "contact_position_z",
    "hit.positionAt110Feet.x": "position_at_110_x",
    "hit.positionAt110Feet.y": "position_at_110_y",
    "hit.positionAt110Feet.z": "position_at_110_z",
    "hit.landingFlat.distance": "hit_distance",
    "hit.landingFlat.bearing": "hit_bearing",
    "hit.landingFlat.hangTime": "hit_hang_time",
    "hit.landingFlat.confidence": "landing_confidence",
    "location.field.name": "field_name",
    "location.venue.name": "venue_name",
    "homeTeam.name": "home_team_name",
    "homeTeam.shortName": "home_team",
    "awayTeam.name": "away_team_name",
    "awayTeam.shortName": "away_team",
    "level.name": "level",
    "league.name": "league",
    "league.shortName": "league_short",
    "strikeZone.top": "strike_zone_top",
    "strikeZone.bottom": "strike_zone_bottom",
    "strikeZone.right": "strike_zone_right",
    "strikeZone.left": "strike_zone_left",
    "strikeZone.type": "strike_zone_type",
    "strikeZone.decision": "strike_zone_decision",
    "externalSessionId": "external_session_id",
    "gameDateUtc": "game_date_utc",
    "gameDateLocal": "game_date_local",
    "localDateTime": "local_date_time",
    "utcDateTime": "utc_date_time",
    "date": "game_date",
}
DROP_FROM_PITCHES = {"notes", "version"}  # 100% null / constant


def snake_name(master_col: str) -> str:
    if master_col in SNAKE_OVERRIDES:
        return SNAKE_OVERRIDES[master_col]
    return snake(master_col)


# ---------------------------------------------------------------------------
# value normalization (typos / whitespace / case drift in the raw feed)
# ---------------------------------------------------------------------------
PITCH_CALL_FIX = {
    "StirkeCalled": "StrikeCalled", "Strikecalled": "StrikeCalled",
    "inPlay": "InPlay", "foulBallNotFieldable": "FoulBallNotFieldable",
    "FoulBall": "FoulBallNotFieldable", "BallInDirt": "BallinDirt",
}
PLAY_RESULT_FIX = {
    "error": "Error", "Sacrificie": "Sacrifice", ".": "Undefined", "Nothing": "Undefined",
}
HIT_TYPE_FIX = {"Groundball": "GroundBall", "PopUp": "Popup", ".": "Undefined"}
HAND_FIX = {"left": "Left", "right": "Right"}
KORBB_FIX = {".": "Undefined", "InPlay": "Undefined"}


def _fix(col: str, mapping: dict) -> pl.Expr:
    e = pl.col(col).cast(pl.Utf8).str.strip_chars()
    return e.replace(mapping).alias(col)


def normalize_values(df: pl.DataFrame) -> pl.DataFrame:
    exprs = []
    if "PitchCall" in df.columns:
        exprs.append(_fix("PitchCall", PITCH_CALL_FIX))
    if "PlayResult" in df.columns:
        exprs.append(_fix("PlayResult", PLAY_RESULT_FIX))
    if "TaggedHitType" in df.columns:
        exprs.append(_fix("TaggedHitType", HIT_TYPE_FIX))
    if "KorBB" in df.columns:
        exprs.append(_fix("KorBB", KORBB_FIX))
    for c in ("PitcherThrows", "BatterSide"):
        if c in df.columns:
            exprs.append(_fix(c, HAND_FIX))
    for c in ("Pitcher", "Batter", "PitcherTeam", "BatterTeam", "TaggedPitchType"):
        if c in df.columns:
            exprs.append(pl.col(c).cast(pl.Utf8).str.strip_chars().alias(c))
    return df.with_columns(exprs) if exprs else df


# ---------------------------------------------------------------------------
# five-column key: the bundle joins on it, so it must be non-null and unique
# ---------------------------------------------------------------------------
K5 = ["GameID", "Inning", "BatterTeam", "PAofInning", "PitchofPA"]


def fix_k5(df: pl.DataFrame) -> pl.DataFrame:
    """Make K5 non-null and unique without touching any physical column.

    1.2% of master rows (bullpen / adhoc sessions) carry no PA numbering. They
    get a synthetic PAofInning of 9000 + PitchNo so each becomes its own PA
    (their sequencing features are then null, as they should be). Residual
    exact collisions get PitchofPA bumped by 1000 per extra occurrence.
    `k5_adjusted` marks every touched row.
    """
    pa_null = pl.col("PAofInning").is_null()
    pp_null = pl.col("PitchofPA").is_null()
    df = df.with_columns([
        (pa_null | pp_null).alias("k5_adjusted"),
        pl.when(pa_null).then(9000 + pl.col("PitchNo").fill_null(0)).otherwise(pl.col("PAofInning")).cast(pl.Int64).alias("PAofInning"),
        pl.when(pp_null).then(pl.when(pa_null).then(1).otherwise(9000 + pl.col("PitchNo").fill_null(0)))
          .otherwise(pl.col("PitchofPA")).cast(pl.Int64).alias("PitchofPA"),
        pl.col("BatterTeam").fill_null("UNK"),
        pl.col("Inning").fill_null(0).cast(pl.Int64),
    ])
    occ = (pl.col("PitchNo").fill_null(0).rank("ordinal").over(K5) - 1).alias("_occ")
    df = df.with_columns(occ).with_columns([
        (pl.col("k5_adjusted") | (pl.col("_occ") > 0)).alias("k5_adjusted"),
        (pl.col("PitchofPA") + 1000 * pl.col("_occ")).alias("PitchofPA"),
    ]).drop("_occ")
    dup = df.select(pl.struct(K5).is_duplicated().sum()).item()
    if dup:
        raise RuntimeError(f"K5 still has {dup} duplicate rows after fix_k5")
    return df


# ---------------------------------------------------------------------------
# Stadium for park correction
# ---------------------------------------------------------------------------
def _norm_stadium(s: str | None) -> str | None:
    if s is None:
        return None
    out = re.sub(r"[^A-Za-z0-9]", "", s)
    return out or None


def add_stadium(df: pl.DataFrame, smap: pl.DataFrame) -> pl.DataFrame:
    """Stadium = master field name with punctuation stripped (matches the
    park table's raw_stadium spelling for ~34% of rows). For Standard games
    with no name match, fall back to the home team's owned venue. Unmatched
    venues simply get a zero park correction in the bundle.
    """
    df = df.with_columns(pl.col("FieldName").map_elements(_norm_stadium, return_dtype=pl.Utf8).alias("_stadium_norm"))
    raw = smap.select(pl.col("raw_stadium").alias("_stadium_norm"), pl.lit(True).alias("_name_hit")).unique(subset=["_stadium_norm"])
    own = (smap.filter(~pl.col("is_neutral")).sort("n_pitches", descending=True)
              .unique(subset=["owner_team"], keep="first")
              .select(pl.col("owner_team").alias("HomeTeam"), pl.col("raw_stadium").alias("_owner_stadium")))
    df = df.join(raw, on="_stadium_norm", how="left").join(own, on="HomeTeam", how="left")
    df = df.with_columns(
        pl.when(pl.col("_name_hit").fill_null(False)).then(pl.col("_stadium_norm"))
          .when((pl.col("SessionType") == "Standard") & pl.col("_owner_stadium").is_not_null()).then(pl.col("_owner_stadium"))
          .otherwise(pl.col("_stadium_norm")).alias("Stadium"),
        pl.when(pl.col("_name_hit").fill_null(False)).then(pl.lit("name"))
          .when((pl.col("SessionType") == "Standard") & pl.col("_owner_stadium").is_not_null()).then(pl.lit("home_team"))
          .otherwise(pl.lit("none")).alias("stadium_match"),
    ).drop(["_stadium_norm", "_name_hit", "_owner_stadium"])
    return df


def load_modeling_frame(master_path: str) -> pl.DataFrame:
    df = pl.read_parquet(master_path, columns=list(MODEL_MAP.keys())).rename(MODEL_MAP)
    df = normalize_values(df)
    return df
