"""Prove this copy of the bundle reproduces Pitch Profiler's production numbers.

    python verify_install.py

Scores the bundled sample TrackMan file (216 pitches, one Coastal Carolina
pitcher) and compares every output against golden values captured from the
production service. Exits non-zero on any mismatch.

Run this FIRST, before wiring the bundle into anything. If it passes, your
environment reproduces our numbers exactly and any later disagreement is in
your input data, not in the models.
"""

from __future__ import annotations

import json
import sys
from pathlib import Path

import polars as pl

HERE = Path(__file__).parent
sys.path.insert(0, str(HERE))

from pitchprofiler_models import load_bundle, run_all  # noqa: E402

# Tolerance: LightGBM is deterministic, so an exact match is expected. This
# leaves room only for platform float formatting, not for a real difference.
TOL = 1e-9


def main() -> int:
    golden = json.loads((HERE / "sample" / "golden.json").read_text(encoding="utf-8"))
    bundle = load_bundle(HERE / "models")
    raw = pl.read_parquet(HERE / "sample" / "sample_trackman_ccu.parquet")
    out = run_all(raw, bundle)

    failures: list[str] = []

    def check(label: str, got: float, want: float) -> None:
        delta = abs(got - want)
        ok = delta <= TOL
        print(f"  {'PASS' if ok else 'FAIL'}  {label:<28} got {got:>12.6f}   expected {want:>12.6f}")
        if not ok:
            failures.append(f"{label}: got {got}, expected {want} (delta {delta:.3e})")

    print(f"Rows in sample file: {len(raw)} (expected {golden['n_raw']})")
    if len(raw) != golden["n_raw"]:
        failures.append("sample file row count changed")
    n_scored = int(len(out["scored"]))
    print(f"Scoreable pitches:   {n_scored} (expected {golden['n_scored']})")
    if n_scored != golden["n_scored"]:
        failures.append("scoreable pitch count changed")

    print("\nPitcher-season grades")
    grades = out["grades"].iloc[0]
    for key, want in golden["grades"].items():
        check(key, float(grades[key]), want)

    print("\nMean per-pitch xRV")
    for key, want in golden["xrv_means"].items():
        check(key, float(out["scored"][key].mean()), want)

    print("\nExpected stats")
    xs = out["xstats"].iloc[0]
    for key, want in golden["xstats"].items():
        check(key, float(xs[key]), want)

    print()
    if failures:
        print(f"VERIFICATION FAILED ({len(failures)} mismatches)")
        for f in failures:
            print("  - " + f)
        return 1
    print("VERIFICATION PASSED: this bundle reproduces production exactly.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
