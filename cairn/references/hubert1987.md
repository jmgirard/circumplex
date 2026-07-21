# hubert1987 — the randomization order test behind `fit_structure()`'s RANDALL

**Provenance.** Ingested 2026-07-20 by M45 from
`cairn/references/sources/hubert1987.pdf` (gitignored). The shelf PDF is 7
pages and holds the whole article, printed pp. 172–178 on the pages themselves
(running heads verified). Anchors below cite the article's own page numbers.

Extraction: the PDF is an OCR scan (`Creator: ABBYY FineReader`, empty
Producer), so its `pdftotext` text layer is **OCR output of the page image, not
an independent witness** — and it is visibly garbled ("Wakeneld", "denned",
"m^ima"). Every load-bearing formula and numeral below (Eqs. 3 and 5, the
exact- and Monte-Carlo p-value definitions, the worked values .75 and
.88 − .13) was therefore verified 2026-07-20 against the `pdftoppm`-rendered
images of pp. 175–177 — the pages carrying every anchor this note cites (the
exact p-value and the reference-set symmetry on p. 175, Eqs. 3/5 and the worked
values on p. 176, the Monte-Carlo form on p. 177); the OCR text layer served
only to locate passages and cross-check running prose. Per M41-D1 the rendered-image read is independent of
the text layer but is **not** a human attestation, and both channels derive
from the same scan image — a defect in the scan itself would escape both. No
value here has been read by a human.

**Citation.** Hubert, L., & Arabie, P. (1987). Evaluating order hypotheses
within proximity matrices. *Psychological Bulletin*, 102(1), 172–178.
Transcribed from the article's own title block and running heads.

**Role.** The published method the package's RANDALL structure test implements:
the normalized order-agreement index and its randomization p-value computed by
`structure_randall()` / `structure_randall_test()` (`R/fit_structure.R`), the
fifth test `fit_structure()` runs. Tracey (1997) — `tracey1997.md` — is the
FORTRAN operationalization the package also cites; the statistical content is
Hubert & Arabie's.

## Extracted values

Notation is the source's: **P** is an n×n *proximity* matrix keyed as a
**dissimilarity** (larger p_ij = more dissimilar; for correlations the paper
uses 1 − r_ij, p. 173). An order hypothesis is a set of conjectured orderings
of proximity pairs. A conjecture is an **agreement** (A) when the data respect
it, a **violation** (D) when they contradict it, and a **tie** / null decision
(T) when the two proximities are equal (pp. 173, 176).

### The normalized agreement index — Eqs. (3) and (5), p. 176

The descriptive measure of correspondence between conjectured and observed
order, Eq. (3):

> (A − D) / (A + D + T)

"varies between the bounds of ±1" and, following Hubert (1978), "measures of
the form given in Equation 3 … have expectations of zero under the
random-labeling hypothesis" (p. 176). Eq. (5) writes the same quantity as a
cross-product with the {−1, 0, +1}-coded order functions,
`Σ T_H·T_D / Σ|T_H|`, T_H = +1 / −1 / 0 for a smaller / larger / no
conjecture (p. 176). Worked three-argument example (Holland data):
**(42 − 6)/48 = .88 − .13 = .75** (p. 176); equivalently `(A/24) − 1` since the
48 three-argument conjectures give `2A/48 − 1` (p. 177). Table 3 (p. 177) tabs
the three-argument index over all 6! = 720 relabelings — range −.50 to the
observed maximum **.75**.

### The exact randomization p-value — p. 175

"The p value associated with the observed number of agreements is the
proportion of the n! permutations that give agreement counts as large or
larger" (p. 175) — one-tailed, evaluated by relabeling the objects (a
one-to-one permutation of the object indices) and recomputing the statistic.

### The Monte-Carlo p-value — p. 177

For matrices too large to enumerate ("n ≤ 8" is called exact-feasible, p. 177):
"N permutations are chosen at random (with replacement), and if M of the
associated measures are as extreme or more so when compared with the observed
agreement measure, a p value (one-tailed) of **(M + 1)/(N + 1)** is reported.
(The 1 appears both in the numerator and the denominator because the observed
agreement measure is assumed to be another random draw under the null…)"
(p. 177).

### Symmetry of the reference set — p. 175

For a circular conjecture on 6 objects "there are really only 5!/2 = 60
‘distinct’ relabelings because all cyclic permutations and their complete
reversals are equivalent," and the uniform distribution over all 720
permutations "induces an equally likely distribution over the 60 distinct
relabelings" (p. 175) — so a reduced reference set yields the same p-value.

## Reconciliation with the shipped code

`R/fit_structure.R` works in **correlation (similarity)** space, not the paper's
1 − r dissimilarity: `structure_randall()` predicts `vals[ia] > vals[ib]` where
`randall_predictions()` sets `ia` to the pair at the *smaller* circular
distance — i.e. closer-in-order variables should be *more* correlated. This is
Hubert & Arabie's order relation with the sign flipped by the r ↔ (1 − r)
change of variable; the agreement counts are identical.

- **Statistic.** `2 * mean(vals[ia] > vals[ib]) - 1` = `(A − D − T)/(A + D + T)`
  (strict `>` scores a tie as a non-agreement). This equals Eq. (3)
  `(A − D)/(A + D + T)` **exactly when T = 0**. **Departure:** with tied
  proximities the code counts ties against the index where Eq. (3) excludes
  them from the numerator — numerically immaterial for continuous correlation
  matrices, where exact ties are measure-zero. At T = 0 the identity
  `2A/N − 1 = (A − D)/(A + D)` reproduces the paper's `(A/24) − 1` and its .75.
- **Exact p-value.** `structure_randall_test()` returns
  `mean(null_index >= observed)` — the paper's "proportion … as large or
  larger" (p. 175), on the index (monotone in A). It enumerates `cbind(1L,
  all_perms(2:nv))` = **(n−1)! relabelings** (variable 1 pinned to position 1),
  one representative per cyclic-rotation class; because the circulant
  prediction set makes the statistic rotation-invariant, each class has equal
  size n and constant value, so the (n−1)! p-value equals the full n! p-value.
  The code does **not** additionally collapse the reversal symmetry (the
  further ÷2 to (n−1)!/2 of p. 175) — harmless, as reversal pairs carry equal
  values and leave proportions unchanged.
- **Monte-Carlo p-value.** `(1 + sum(null_index >= observed)) / (n_perm + 1)`
  is the paper's **(M + 1)/(N + 1)** (p. 177) verbatim, with the same
  "observed is another draw under the null" rationale the code comment gives —
  **faithful, not a departure.**
- **Exact/MC threshold.** The code errors and requires `n_perm` for `nv > 9`;
  the paper calls exact enumeration feasible for "n ≤ 8" (p. 177). The code's
  cutoff (9) is one looser — an implementation choice, not a fidelity claim.

## What this source does and does not license

It licenses the RANDALL **statistic and p-value** only. It says nothing about
the circulant/circumplex *prediction pattern* itself (equal-spacing on a
circle) — that structure is the package's own `randall_predictions()`, justified
by the circumplex model, not by this paper, which illustrates with Holland's
six vocational types. The zero-expectation and ±1-bound properties are the
paper's; the package's interpretation of the index magnitude is not.

## Traces to

- `R/fit_structure.R` — `randall_predictions()`, `structure_randall()` (the
  Eq. 3/5 index), `structure_randall_test()` (the exact and (M+1)/(N+1)
  randomization p-values); surfaced through `fit_structure()` as the RANDALL
  test.
- `tests/testthat/test-fit_structure.R` — RANDALL statistic/p-value tests.
- `vignettes/evaluating-circumplex-structure.Rmd` — RANDALL exposition.
- `man/fit_structure.Rd` (`@references`).
- Companion: `tracey1997.md` (the FORTRAN operationalization the package
  co-cites).

## Open questions

- **No human read.** Both channels are machine (OCR text layer; a rendered-image
  read of pp. 176–177). A scan defect on an unrendered page would be caught by
  neither — dated 2026-07-20.
