from fractions import Fraction as F
import math, re
txt = open('bexp.txt').read().splitlines()
def grab(pfx):
    for L in txt:
        if L.startswith(pfx):
            return L[len(pfx):].strip()
    raise KeyError(pfx)
def mat(s, n):                       # column-major, exact from hex doubles
    v = [F(float.fromhex(t)) for t in s.split()]
    return [[v[c*n + r] for c in range(n)] for r in range(n)]
p = 3
S = mat(grab('S:'), p)
Q = int(grab('Q:'))
M = [mat(grab('M%d:' % (i+1)), p) for i in range(Q)]
NC, DFv, N = int(grab('NCOMP:')), 1, 600

def mm(A, B):
    n, m, k = len(A), len(B[0]), len(B)
    return [[sum((A[i][t]*B[t][j] for t in range(k)), F(0)) for j in range(m)] for i in range(n)]
def inv(A):
    n = len(A)
    M_ = [row[:] + [F(1) if i == j else F(0) for j in range(n)] for i, row in enumerate(A)]
    for c in range(n):
        pv = next(r for r in range(c, n) if M_[r][c] != 0)
        M_[c], M_[pv] = M_[pv], M_[c]
        d = M_[c][c]
        M_[c] = [x/d for x in M_[c]]
        for r in range(n):
            if r != c and M_[r][c] != 0:
                f = M_[r][c]
                M_[r] = [a - f*b for a, b in zip(M_[r], M_[c])]
    return [row[n:] for row in M_]
def sum_prod_t(A, B):                # sum(A * t(B))
    return sum((A[i][j]*B[j][i] for i in range(len(A)) for j in range(len(A[0]))), F(0))

si  = inv(S)
sim = [mm(si, Mk) for Mk in M]
info = [[F(0)]*Q for _ in range(Q)]
for s in range(Q):
    for t in range(s, Q):
        info[s][t] = info[t][s] = F(1,2)*sum_prod_t(sim[s], sim[t])
acov = inv(info)

print("EXACT corrected SEs (and the shipped doubles):")
dbl = [float(x) for x in grab('DOUBLE_corrected:').split()]
for r in range(NC):
    acc = [[sum((acov[r][s]*M[s][i][j] for s in range(Q)), F(0)) for j in range(p)] for i in range(p)]
    w  = mm(mm(si, acc), si)
    w  = [[F(1,2)*x for x in row] for row in w]
    wc = [row[:] for row in w]
    for i in range(p): wc[i][i] = F(0)
    rs = [sum((wc[i][j]*S[i][j] for j in range(p)), F(0)) for i in range(p)]
    for i in range(p): wc[i][i] = -rs[i]
    wcs = mm(wc, S)
    corrected = F(2)*sum_prod_t(wcs, wcs)
    se = math.sqrt(float(corrected)/N)
    print("  comp %d: EXACT %.12g   double %.12g   rel.err %.3e"
          % (r+1, se, dbl[r], abs(se-dbl[r])/se))

# ---- cval, the scaling factor (axes_scaling_factor) ----
up = [(i,j) for i in range(p) for j in range(p) if j > i]
tr_vg = sum((F(1) - si[i][j]*S[i][j]*(F(1) - S[i][j]**2) for (i,j) in up), F(0))
ys = []
for s in range(Q):
    w = [[F(1,2)*x for x in row] for row in mm(sim[s], si)]
    sw = mm(S, w)
    for i in range(p): w[i][i] = w[i][i] - sw[i][i]
    ys.append(mm(w, S))
bmat = [[F(0)]*Q for _ in range(Q)]
for s in range(Q):
    for t in range(s, Q):
        bmat[s][t] = bmat[t][s] = F(2)*sum_prod_t(ys[s], ys[t])
proj = sum((acov[s][t]*bmat[s][t] for s in range(Q) for t in range(Q)), F(0))
cval = (tr_vg - proj) / F(DFv)
print()
print("EXACT tr_vg = %.10g" % float(tr_vg))
print("EXACT proj  = %.10g" % float(proj))
print("EXACT cval  = %+.12g   (positive => the matrix IS priceable)" % float(cval))
print("cancellation amplification (|a|+|b|)/|a-b| = %.4g"
      % ((abs(float(tr_vg))+abs(float(proj)))/abs(float(tr_vg)-float(proj))))
