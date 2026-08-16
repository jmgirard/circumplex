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
    """Returns (corrected SEs, cval, baseline) exactly.

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
        ses.append(math.sqrt(float(F(2) * sum_prod_t(wcs, wcs)) / n))

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
    return ses, cval, cb, tr_vg, proj


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

    ses, cval, cb, tr_vg, proj = pipeline(S, mats, n_comp, n, df, bdf)
    print("EXACT_CVAL: %.17g" % float(cval))
    print("EXACT_BASELINE: %.17g" % float(cb))
    print("EXACT_TR_VG: %.17g" % float(tr_vg))
    print("EXACT_PROJ: %.17g" % float(proj))
    for i, se in enumerate(ses):
        print("EXACT_SE%d: %.17g" % (i + 1, se))


if __name__ == "__main__":
    main(sys.argv[1])
