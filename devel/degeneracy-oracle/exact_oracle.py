"""Exact rational-arithmetic oracle for axes_reliability()'s two fitted-matrix
consumers (M89, RR18).

Why this exists: every quantity from Sigma-hat to `cval` and to the corrected
variances is a RATIONAL function of the matrix entries. The only irrationals
are the cos() values in Delta, which enter as exactly-representable doubles,
and the final sqrt(), taken after every comparison. So the whole pipeline can
be recomputed in exact arithmetic and the double-precision result compared
against truth -- which is what showed that `axes_corrected_se()` returns SEs
wrong by 3.4% with reason NULL at the committed counterexample, and that the
`"indefinite"` refusal there is a cancellation sign-flip rather than a
statement about the model.

This is offline dev tooling. It is NOT a package dependency: nothing in R/
or tests/ calls it, it uses only the Python standard library, and devel/ is
.Rbuildignore'd. Drive it from exact_oracle.R, which supplies the inputs.

Input: one argument, the path to a hex-dump written by exact_oracle.R. Every
value arrives as a C99 hex float (%a), which round-trips a double exactly --
decimal printing does not, and at these condition numbers the lost bits flip
the answer.
"""

import sys
from fractions import Fraction as F
import math


def _read(path):
    d = {}
    for line in open(path):
        if ":" not in line:
            continue
        k, v = line.split(":", 1)
        d[k.strip()] = v.strip()
    return d


def _mat(s, n):
    """Column-major (R's order) hex doubles -> exact rational matrix."""
    v = [F(float.fromhex(t)) for t in s.split()]
    return [[v[c * n + r] for c in range(n)] for r in range(n)]


def mm(A, B):
    n, k, m = len(A), len(B), len(B[0])
    return [[sum((A[i][t] * B[t][j] for t in range(k)), F(0)) for j in range(m)]
            for i in range(n)]


def inv(A):
    """Exact Gauss-Jordan. No pivoting tolerance: entries are rationals."""
    n = len(A)
    M = [row[:] + [F(int(i == j)) for j in range(n)] for i, row in enumerate(A)]
    for c in range(n):
        pv = next(r for r in range(c, n) if M[r][c] != 0)
        M[c], M[pv] = M[pv], M[c]
        d = M[c][c]
        M[c] = [x / d for x in M[c]]
        for r in range(n):
            if r != c and M[r][c] != 0:
                f = M[r][c]
                M[r] = [a - f * b for a, b in zip(M[r], M[c])]
    return [row[n:] for row in M]


def sum_prod_t(A, B):
    """sum(A * t(B)) -- the trace contraction both consumers use."""
    return sum((A[i][j] * B[j][i] for i in range(len(A)) for j in range(len(A[0]))),
               F(0))


def pipeline(S, mats, n_comp, n, df, baseline_df):
    """Returns (corrected SEs, FIML ratios, cval, baseline) exactly.

    Mirrors axes_se_pricing() and axes_scaling_factor() line for line. Both are
    evaluated at S: the caller passes cov2cor(Sigma-hat), which for a
    unit-diagonal input is Sigma-hat itself.
    """
    p, Q = len(S), len(mats)
    si = inv(S)
    sim = [mm(si, M) for M in mats]

    info = [[F(0)] * Q for _ in range(Q)]
    for s in range(Q):
        for t in range(s, Q):
            info[s][t] = info[t][s] = F(1, 2) * sum_prod_t(sim[s], sim[t])
    acov = inv(info)

    ses = []
    ratios = []
    # The pre-square-root quadratic forms themselves, kept as exact rationals.
    # They are what the certificate is computed from and what the packaged
    # bracket needs in order to price its own machine (M115): a relative error
    # is a statement about the matrix, so the exact v and v_naive travel, while
    # the doubles measured from them do not.
    vs = []
    vns = []
    for r in range(n_comp):
        acc = [[sum((acov[r][s] * mats[s][i][j] for s in range(Q)), F(0))
                for j in range(p)] for i in range(p)]
        w = [[F(1, 2) * x for x in row] for row in mm(mm(si, acc), si)]
        # W_c: off-diagonal unchanged, diagonal absorbs the correlation
        # Jacobian (a sample correlation's diagonal has zero sampling variance).
        wc = [row[:] for row in w]
        for i in range(p):
            wc[i][i] = F(0)
        rs = [sum((wc[i][j] * S[i][j] for j in range(p)), F(0)) for i in range(p)]
        for i in range(p):
            wc[i][i] = -rs[i]
        wcs = mm(wc, S)
        v_corrected = F(2) * sum_prod_t(wcs, wcs)
        ses.append(math.sqrt(float(v_corrected) / n))
        # The naive arm at this SAME matrix -- W itself, without the Jacobian
        # substitution above -- and the quotient the FIML path multiplies the
        # reported SE by (M113). n cancels out of the quotient exactly, which
        # is why it is formed here from the pre-root variances.
        ws = mm(w, S)
        v_naive = F(2) * sum_prod_t(ws, ws)
        ratios.append(math.sqrt(float(v_corrected / v_naive)))
        vs.append(v_corrected)
        vns.append(v_naive)

    up = [(i, j) for i in range(p) for j in range(p) if j > i]
    tr_vg = sum((F(1) - si[i][j] * S[i][j] * (F(1) - S[i][j] ** 2) for (i, j) in up),
                F(0))
    ys = []
    for s in range(Q):
        w = [[F(1, 2) * x for x in row] for row in mm(sim[s], si)]
        sw = mm(S, w)
        for i in range(p):
            w[i][i] = w[i][i] - sw[i][i]
        ys.append(mm(w, S))
    bmat = [[F(0)] * Q for _ in range(Q)]
    for s in range(Q):
        for t in range(s, Q):
            bmat[s][t] = bmat[t][s] = F(2) * sum_prod_t(ys[s], ys[t])
    proj = sum((acov[s][t] * bmat[s][t] for s in range(Q) for t in range(Q)), F(0))
    cval = (tr_vg - proj) / F(df)
    cb = sum(((F(1) - S[i][j] ** 2) ** 2 for (i, j) in up), F(0)) / F(baseline_df)
    return ses, ratios, cval, cb, tr_vg, proj, vs, vns, tr_vg - proj


def dd_hex(x):
    """Exact rational -> the (hi, lo) double pair whose unevaluated sum is x.

    hi is x correctly rounded to a double (float(Fraction) rounds to nearest in
    Python 3), and lo is the remainder x - hi rounded the same way, so hi + lo
    carries the exact value to about 106 bits -- ten decades finer than the
    double-precision errors the R side measures against it. Both words are
    printed as C99 hex floats, which round-trip through R's as.numeric() bit
    for bit; decimal printing does not.

    This is what lets the packaged bracket measure the RUNNING machine's own
    error (M115): a single frozen decimal error figure describes one machine,
    while the exact value it was measured against describes the matrix, and
    every machine can compare its own pricing to that.
    """
    hi = float(x)
    lo = float(x - F(hi))
    return hi.hex(), lo.hex()


def main(path):
    d = _read(path)
    p = int(d["P"])
    n = int(d["N"])
    df = int(d["DF"])
    bdf = int(d["BASELINE_DF"])
    n_comp = int(d["NCOMP"])
    Q = int(d["Q"])
    S = _mat(d["S"], p)
    mats = [_mat(d["M%d" % (i + 1)], p) for i in range(Q)]

    ses, ratios, cval, cb, tr_vg, proj, vs, vns, u = \
        pipeline(S, mats, n_comp, n, df, bdf)
    print("EXACT_CVAL: %.17g" % float(cval))
    print("EXACT_BASELINE: %.17g" % float(cb))
    print("EXACT_TR_VG: %.17g" % float(tr_vg))
    print("EXACT_PROJ: %.17g" % float(proj))
    for i, se in enumerate(ses):
        print("EXACT_SE%d: %.17g" % (i + 1, se))
    for i, rt in enumerate(ratios):
        print("EXACT_RATIO%d: %.17g" % (i + 1, rt))

    # The exact quadratic forms and the scaling factor's exact numerator, each
    # as a hi/lo hex pair. `HEX_` rather than `EXACT_` because the R driver
    # parses these as strings and the `EXACT_` lines as numbers.
    v_hex = [dd_hex(x) for x in vs]
    vn_hex = [dd_hex(x) for x in vns]
    u_hex = dd_hex(u)
    print("HEX_V_HI: %s" % " ".join(h for h, _ in v_hex))
    print("HEX_V_LO: %s" % " ".join(l for _, l in v_hex))
    print("HEX_VNAIVE_HI: %s" % " ".join(h for h, _ in vn_hex))
    print("HEX_VNAIVE_LO: %s" % " ".join(l for _, l in vn_hex))
    print("HEX_U_HI: %s" % u_hex[0])
    print("HEX_U_LO: %s" % u_hex[1])


if __name__ == "__main__":
    main(sys.argv[1])
