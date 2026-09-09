"""Pitch-type reclassification: Python port of R/12_pitch_reclassification.R.

Partition-before-naming (pitch-classification-methodology.md), one pitcher-
season at a time, geometry first and labels never consulted for grouping:

  Stage 0    spin-error detection (movement-cluster baseline; a tag may
             rescue a flagged pitch but never condemn one). The flagged spin
             value is imputed FOR CLUSTERING ONLY; the pitch is kept.
  Stage 1    velocity bands first (average linkage, cut at 6 mph), then
             movement-weighted Ward over-clustering on robust-scaled features
  Stage 2    agglomerate fragment centroids in a FIXED physical space
             (2.5 mph = 5.5 in = 1200 rpm = 1 unit), fold fragments < 3
  Stage 2.5  false-split repair: 1-D projection, seeded 2-comp vs 1-comp BIC
  Stage 2.6  misplaced-boundary repair on kept pairs (posterior reassignment)
  Stage 3    family-aware prototype naming; changeup + splitter are ONE
             family named by pooled spin, split only across a real gap

NO pitch is ever dropped by this module. There is no outlier / velocity
trimming anywhere: every pitch a pitcher threw keeps its row, and max velo is
untouched.

Deterministic: rows are lexicographically pre-sorted before every clustering,
cluster ids are numbered by first appearance (R cutree semantics), one merge
per iteration. Same pitches in a different row order -> same arsenal.
"""
from __future__ import annotations

import math

import numpy as np
from scipy.cluster.hierarchy import fcluster, linkage
from scipy.spatial.distance import pdist

RC_MIN_PITCHES = 25
RC_FRAGMENT_MIN = 3
RC_CUT_H = 1.55
RC_PAIR_RADIUS = 2.6
RC_PHYS_SCALE = np.array([2.5, 5.5, 5.5, 1200.0])  # velo, ivb, hb, spin
RC_OFFSPEED_SPIN_SPLIT = 1300.0
RC_OFFSPEED_SPIN_GAP = 700.0
RC_BAND_H = 6.0

TAG_MAP = {
    "Fastball": "Fastball", "FourSeamFastball": "Fastball", "FourSeamFastBall": "Fastball",
    "Four-Seam": "Fastball", "4-Seam": "Fastball", "Four-Seam Fastball": "Fastball",
    "4-Seam Fastball": "Fastball", "FF": "Fastball", "FA": "Fastball", "FastBall": "Fastball",
    "OneSeamFastBall": "Sinker",
    "Sinker": "Sinker", "TwoSeamFastBall": "Sinker", "TwoSeamFastball": "Sinker",
    "Two-Seam": "Sinker", "SI": "Sinker", "FT": "Sinker",
    "Cutter": "Cutter", "FC": "Cutter", "CT": "Cutter",
    "Slider": "Slider", "SL": "Slider", "Slurve": "Slider", "SV": "Slider",
    "Sweeper": "Sweeper", "ST": "Sweeper", "SW": "Sweeper",
    "Curveball": "Curveball", "CurveBall": "Curveball", "CU": "Curveball", "CB": "Curveball",
    "KC": "Curveball", "Knuckle Curve": "Curveball",
    "ChangeUp": "ChangeUp", "Changeup": "ChangeUp", "CH": "ChangeUp",
    "Splitter": "Splitter", "FS": "Splitter", "SP": "Splitter",
    "Knuckleball": "Knuckleball", "KN": "Knuckleball",
}
FAMILY_MAP = {
    "Fastball": "fastball", "Sinker": "fastball",
    "Cutter": "breaking", "Slider": "breaking", "Sweeper": "breaking", "Curveball": "breaking",
    "ChangeUp": "offspeed", "Splitter": "offspeed",
}
_RESCUE_TAGS = {"Curveball", "Slider", "Sweeper", "Cutter"}

PROTOTYPES = [  # name, vdiff, ivb, hb(arm side +), spin
    ("Fastball", 0.0, 16.0, 8.0, 2200.0),
    ("Sinker", 1.5, 9.0, 15.0, 2100.0),
    ("Cutter", 3.5, 10.0, -1.0, 2300.0),
    ("Slider", 8.0, 3.0, -5.0, 2350.0),
    ("Sweeper", 9.0, 2.0, -14.0, 2500.0),
    ("Curveball", 12.0, -8.0, -8.0, 2400.0),
    ("ChangeUp", 9.0, 7.0, 14.0, 1700.0),
    ("Splitter", 8.0, 4.0, 8.0, 950.0),
]
_PROTO_NAMES = [p[0] for p in PROTOTYPES]
_PROTO = np.array([p[1:] for p in PROTOTYPES], dtype=float)
_PROTO_SCL = np.array([2.5, 4.5, 4.5, 600.0])


def canon_tag(x) -> str:
    if x is None:
        return ""
    x = str(x)
    return TAG_MAP.get(x, x)


def family(x) -> str | None:
    return FAMILY_MAP.get(x)


# ----------------------------------------------------------------------------
# numeric helpers (exact R semantics)
# ----------------------------------------------------------------------------
def _robust_scale(x: np.ndarray) -> np.ndarray:
    med = np.nanmedian(x)
    q75, q25 = np.nanpercentile(x, [75, 25])  # numpy linear == R type 7
    iqr = q75 - q25
    if not np.isfinite(iqr) or iqr < 1e-6:
        sd = np.nanstd(x, ddof=1) if x.size > 1 else np.nan
        iqr = max(sd if np.isfinite(sd) else 0.0, 1e-6)
    return (x - med) / iqr


def _renumber_first_appearance(lab: np.ndarray) -> np.ndarray:
    """R cutree numbering: clusters numbered 1.. by first appearance."""
    out = np.empty(lab.shape, dtype=np.int64)
    seen: dict = {}
    for i, v in enumerate(lab):
        if v not in seen:
            seen[v] = len(seen) + 1
        out[i] = seen[v]
    return out


def _lex_order(M: np.ndarray) -> np.ndarray:
    """do.call(order, as.data.frame(M)): sort by col 1, ties by col 2, ..."""
    keys = tuple(M[:, j] for j in range(M.shape[1] - 1, -1, -1))
    return np.lexsort(keys)


def _ward(M: np.ndarray, k: int) -> np.ndarray:
    """Ward.D2 on a deterministic lexicographic pre-sort; labels in original order."""
    n = M.shape[0]
    k = min(k, n)
    if n == 1 or k == 1:
        return np.ones(n, dtype=np.int64)
    ord_ = _lex_order(M)
    Z = linkage(M[ord_], method="ward")
    lab_sorted = _renumber_first_appearance(fcluster(Z, t=k, criterion="maxclust"))
    lab = np.empty(n, dtype=np.int64)
    lab[ord_] = lab_sorted
    return lab


def _cut_height(X: np.ndarray, h: float, method: str) -> np.ndarray:
    """hclust(dist(X), method) + cutree(h): labels by first appearance."""
    n = X.shape[0]
    if n == 1:
        return np.ones(1, dtype=np.int64)
    Z = linkage(pdist(X), method=method)
    return _renumber_first_appearance(fcluster(Z, t=h, criterion="distance"))


def _bic1(x: np.ndarray) -> float:
    n = x.size
    s2 = max(np.var(x), 1e-9)  # population variance == var*(n-1)/n
    ll = np.sum(_dnorm_log(x, x.mean(), math.sqrt(s2)))
    return -2.0 * ll + 2.0 * math.log(n)


def _dnorm_log(x, mu, sd):
    return -0.5 * math.log(2 * math.pi) - math.log(sd) - 0.5 * ((x - mu) / sd) ** 2


def _dnorm(x, mu, sd):
    return np.exp(_dnorm_log(x, mu, sd))


def _gmm2(x: np.ndarray, seed1: np.ndarray):
    """Seeded 2-component 1-D Gaussian EM; returns (bic, posterior of comp 1)."""
    n = x.size
    r = seed1.astype(float)
    mu = [0.0, 0.0]
    s2 = [1.0, 1.0]
    pi1 = 0.5
    for _ in range(60):
        w1 = r.sum()
        w2 = n - w1
        if w1 < 1e-6 or w2 < 1e-6:
            break
        mu[0] = float((r * x).sum() / w1)
        mu[1] = float(((1 - r) * x).sum() / w2)
        s2[0] = max(float((r * (x - mu[0]) ** 2).sum() / w1), 1e-9)
        s2[1] = max(float(((1 - r) * (x - mu[1]) ** 2).sum() / w2), 1e-9)
        pi1 = w1 / n
        d1 = pi1 * _dnorm(x, mu[0], math.sqrt(s2[0]))
        d2 = (1 - pi1) * _dnorm(x, mu[1], math.sqrt(s2[1]))
        r_new = d1 / np.maximum(d1 + d2, 1e-300)
        if np.max(np.abs(r_new - r)) < 1e-7:
            r = r_new
            break
        r = r_new
    d1 = pi1 * _dnorm(x, mu[0], math.sqrt(s2[0]))
    d2 = (1 - pi1) * _dnorm(x, mu[1], math.sqrt(s2[1]))
    ll = float(np.sum(np.log(np.maximum(d1 + d2, 1e-300))))
    return -2.0 * ll + 5.0 * math.log(n), d1 / np.maximum(d1 + d2, 1e-300)


# ----------------------------------------------------------------------------
# Stage 0: spin-error detection
# ----------------------------------------------------------------------------
def spin_flags(ivb, hb, spin, tags) -> np.ndarray:
    n = spin.size
    flag = np.zeros(n, dtype=bool)
    ok = np.isfinite(ivb) & np.isfinite(hb) & np.isfinite(spin)
    n_ok = int(ok.sum())
    if n_ok < 12:
        return flag
    M = np.column_stack([_robust_scale(ivb[ok]), _robust_scale(hb[ok])])
    k = max(1, min(4, n_ok // 30))
    lab = _ward(M, k) if k > 1 else np.ones(n_ok, dtype=np.int64)
    sp = spin[ok]
    cl_med = np.empty(n_ok)
    for c in np.unique(lab):
        cl_med[lab == c] = np.median(sp[lab == c])
    cand = (sp > 3200) & (sp > 1.6 * cl_med)
    t = np.array([canon_tag(v) in _RESCUE_TAGS for v in np.asarray(tags, dtype=object)[ok]])
    rescue = t & (sp <= 3600)
    flag[ok] = cand & ~rescue
    return flag


# ----------------------------------------------------------------------------
# Stage 3: prototype namer
# ----------------------------------------------------------------------------
def name_clusters(n, velo, ivb, hb_as, spin) -> list[str]:
    n = np.asarray(n, float)
    feats = np.column_stack([velo.max() - velo, ivb, hb_as, spin])
    D = np.sqrt((((feats[:, None, :] - _PROTO[None, :, :]) / _PROTO_SCL) ** 2).sum(axis=2))
    W = np.exp(-D)
    W = W / np.maximum(W.sum(axis=1, keepdims=True), 1e-12)
    names = [_PROTO_NAMES[i] for i in D.argmin(axis=1)]
    ich, isp = _PROTO_NAMES.index("ChangeUp"), _PROTO_NAMES.index("Splitter")
    off_mass = W[:, ich] + W[:, isp]
    best_single = W.max(axis=1)
    is_off = np.array([nm in ("ChangeUp", "Splitter") for nm in names]) | (off_mass >= best_single)
    if is_off.any():
        off = np.where(is_off)[0]
        pooled = float((spin[off] * n[off]).sum() / n[off].sum())
        if off.size >= 2:
            m = spin[off]
            lo, hi = m.min(), m.max()
            if (hi - lo >= RC_OFFSPEED_SPIN_GAP and lo < RC_OFFSPEED_SPIN_SPLIT
                    and hi >= RC_OFFSPEED_SPIN_SPLIT):
                for j, i in enumerate(off):
                    names[i] = "Splitter" if m[j] < RC_OFFSPEED_SPIN_SPLIT else "ChangeUp"
            else:
                nm = "Splitter" if pooled < RC_OFFSPEED_SPIN_SPLIT else "ChangeUp"
                for i in off:
                    names[i] = nm
        else:
            nm = "Splitter" if pooled < RC_OFFSPEED_SPIN_SPLIT else "ChangeUp"
            for i in off:
                names[i] = nm
    return names


# ----------------------------------------------------------------------------
# the per-pitcher(-season) engine
# ----------------------------------------------------------------------------
def classify_one(velo, ivb, hb, spin_in, hand, tags):
    """Returns dict(type=list[str], spin_flag=bool array, n_clusters, n_named)."""
    velo = np.asarray(velo, float).copy()
    ivb = np.asarray(ivb, float).copy()
    hb = np.asarray(hb, float).copy()
    spin = np.asarray(spin_in, float).copy()
    n = velo.size
    # impute nulls from the pitcher's OWN distribution
    for arr in (velo, ivb, hb, spin):
        bad = ~np.isfinite(arr)
        if bad.any():
            arr[bad] = np.nanmedian(arr[~bad]) if (~bad).any() else np.nan
    if not np.isfinite(spin).any():
        spin = np.full(n, 2200.0)
    sflag = spin_flags(ivb, hb, spin, tags)
    if sflag.any():
        spin = spin.copy()
        spin[sflag] = np.median(spin[~sflag])

    # ---- Stage 1: velocity bands, then movement within band ----
    ordv = np.lexsort((spin, hb, ivb, velo))
    band_sorted = _cut_height(velo[ordv].reshape(-1, 1), RC_BAND_H, "average")
    band = np.empty(n, dtype=np.int64)
    band[ordv] = band_sorted

    frag = np.zeros(n, dtype=np.int64)
    base = 0
    for b in np.unique(band):
        idx = np.where(band == b)[0]
        m = idx.size
        k_fine = max(1, min(m, m // 8 + 2, 20))
        if m <= 2 or k_fine == 1:
            frag[idx] = base + 1
            base += 1
            continue
        M = np.column_stack([
            _robust_scale(velo[idx]),
            1.6 * _robust_scale(ivb[idx]),
            1.6 * _robust_scale(hb[idx]),
            0.5 * _robust_scale(spin[idx]),
        ])
        M[~np.isfinite(M)] = 0.0
        lab = _ward(M, k_fine)
        frag[idx] = base + lab
        base += int(lab.max())

    # ---- Stage 2: agglomerate fragment centroids in FIXED physical space ----
    phys = np.column_stack([velo, ivb, hb, spin]) / RC_PHYS_SCALE

    def cent(assign):
        ids = np.unique(assign)
        return ids, np.vstack([phys[assign == i].mean(axis=0) for i in ids])

    fr_ids, fr_cent = cent(frag)
    if fr_ids.size > 1:
        macro_of_frag = _cut_height(fr_cent, RC_CUT_H, "average")
    else:
        macro_of_frag = np.ones(1, dtype=np.int64)
    pos = {v: i for i, v in enumerate(fr_ids)}
    cl = np.array([macro_of_frag[pos[f]] for f in frag], dtype=np.int64)

    # fold fragments below the floor into the nearest surviving cluster
    while True:
        ids, counts = np.unique(cl, return_counts=True)
        small = ids[counts < RC_FRAGMENT_MIN]
        if small.size == 0 or ids.size <= 1:
            break
        s = small[0]
        ids, cc = cent(cl)
        si = int(np.where(ids == s)[0][0])
        d = np.sqrt(((cc - cc[si]) ** 2).sum(axis=1))
        d[si] = np.inf
        cl[cl == s] = ids[int(d.argmin())]

    # ---- Stage 2.5: false-split merge (one pair per iteration) ----
    while True:
        ids = np.unique(cl)
        if ids.size <= 1:
            break
        _, cc = cent(cl)
        best = None
        best_score = 0.0
        for a in range(ids.size):
            for b in range(a + 1, ids.size):
                axis = cc[b] - cc[a]
                dlen = math.sqrt(float((axis ** 2).sum()))
                if dlen > RC_PAIR_RADIUS:
                    continue
                sel = (cl == ids[a]) | (cl == ids[b])
                x = phys[sel] @ (axis / dlen)
                seed1 = cl[sel] == ids[a]
                score = _bic1(x) - _gmm2(x, seed1)[0]
                if score < best_score:
                    best_score = score
                    best = (ids[a], ids[b])
        if best is None:
            break
        cl[cl == best[1]] = best[0]

    # ---- Stage 2.6: misplaced-boundary refinement on kept pairs ----
    ids = np.unique(cl)
    if ids.size > 1:
        _, cc = cent(cl)
        for a in range(ids.size):
            for b in range(a + 1, ids.size):
                axis = cc[b] - cc[a]
                dlen = math.sqrt(float((axis ** 2).sum()))
                if dlen > RC_PAIR_RADIUS:
                    continue
                sel = np.where((cl == ids[a]) | (cl == ids[b]))[0]
                x = phys[sel] @ (axis / dlen)
                seed1 = cl[sel] == ids[a]
                post1 = _gmm2(x, seed1)[1]
                new_lab = np.where(post1 >= 0.5, ids[a], ids[b])
                if ((new_lab == ids[a]).sum() >= RC_FRAGMENT_MIN
                        and (new_lab == ids[b]).sum() >= RC_FRAGMENT_MIN):
                    cl[sel] = new_lab

    # ---- Stage 3: family-aware naming ----
    h1 = "R"
    for hv in hand:
        if hv is not None and str(hv).strip():
            h1 = str(hv).strip()
            break
    hb_as = -hb if h1[:1].upper() == "L" else hb
    ids = np.unique(cl)
    n_cl = np.array([(cl == i).sum() for i in ids])
    m_velo = np.array([velo[cl == i].mean() for i in ids])
    m_ivb = np.array([ivb[cl == i].mean() for i in ids])
    m_hb = np.array([hb_as[cl == i].mean() for i in ids])
    m_spin = np.array([spin[cl == i].mean() for i in ids])
    nm = name_clusters(n_cl, m_velo, m_ivb, m_hb, m_spin)
    name_of = dict(zip(ids.tolist(), nm))
    return {
        "type": [name_of[int(c)] for c in cl],
        "spin_flag": sflag,
        "n_clusters": int(ids.size),
        "n_named": len(set(nm)),
    }


def reclassify_group(velo, ivb, hb, spin, hand, tags):
    """One pitcher-season. Returns (types or None, spin_flag, usable_mask, info).

    types is None when the group is too small (< RC_MIN_PITCHES usable) or the
    engine failed; the caller keeps the original tags in that case. Rows that
    are not usable (velo/ivb/hb missing) keep their tag too.
    """
    velo = np.asarray(velo, float)
    ivb = np.asarray(ivb, float)
    hb = np.asarray(hb, float)
    spin = np.asarray(spin, float)
    usable = np.isfinite(velo) & np.isfinite(ivb) & np.isfinite(hb)
    if usable.sum() < RC_MIN_PITCHES:
        return None, np.zeros(velo.size, bool), usable, "too few usable pitches"
    idx = np.where(usable)[0]
    hand_u = [hand[i] for i in idx]
    tags_u = [tags[i] for i in idx]
    try:
        res = classify_one(velo[idx], ivb[idx], hb[idx], spin[idx], hand_u, tags_u)
    except Exception as e:  # noqa: BLE001 - keep the tag, report the failure
        return None, np.zeros(velo.size, bool), usable, f"failed: {e}"
    if any(t is None for t in res["type"]):
        return None, np.zeros(velo.size, bool), usable, "naming produced NA"
    types = [None] * velo.size
    flags = np.zeros(velo.size, bool)
    for j, i in enumerate(idx):
        types[i] = res["type"][j]
        flags[i] = bool(res["spin_flag"][j])
    return types, flags, usable, f"{idx.size} pitches -> {res['n_clusters']} clusters -> {res['n_named']} types"
