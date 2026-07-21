# tracey1997 — RANDALL, the program operationalizing the order test

**Provenance.** Ingested 2026-07-20 by M45 from
`cairn/references/sources/tracey1997.pdf` (gitignored). The shelf PDF is 5
pages and holds the whole note, printed pp. 164–168 on the pages themselves
(running heads verified). Anchors cite the note's own page numbers.

Extraction: the PDF is an OCR scan (`Producer: Adobe Acrobat … Paper Capture
Plug-in`), so its `pdftotext` text layer is **OCR output of the page image, not
an independent witness** (garbled: "1ERENCE", "Im", "wasp= .0167"). The
load-bearing passages — the Correspondence Index definition and the worked
RIASEC example (69 confirmed / 2 not / 1 tie / CI .93 / p = .0167) — were
**verified 2026-07-20 against the `pdftoppm`-rendered image of p. 165**; the OCR
text layer served only to locate passages and read the reference list. Per
M41-D1 the rendered-image read is independent of the text layer but is **not** a
human attestation, and both derive from the same scan. No value here has been
read by a human.

**Citation.** Tracey, T. J. G. (1997). RANDALL: A Microsoft FORTRAN program for
a randomization test of hypothesized order relations. *Educational and
Psychological Measurement*, 57(1), 164–168. Transcribed from the note's own
title block, running heads, and citation line.

**Role.** The FORTRAN program that operationalizes Hubert & Arabie (1987) — the
statistical content is theirs (`hubert1987.md`), Tracey's note is the software
citation the package co-cites for its RANDALL test. It does **not** add a
statistic the package takes independently; it names the index and pins the
worked example that exercises the code's tie handling.

## Extracted values

### The Correspondence Index (CI) — p. 165

RANDALL "yields an exact significance level of the number of predictions met by
the data versus the null conjecture of random relabeling" and also "a
correspondence index (CI) … which is the proportion of predictions met minus
the proportion of predictions violated, can range from +1, indicating perfect
fit, to −1, indicating that not one prediction was met. A CI value of 0.0
indicates as many predictions were met as violated, and a CI value of 0.5
indicates that 75% of the predictions were met in the data set whereas 25% were
violated" (p. 165). So **CI = (proportion met) − (proportion violated) =
(A − D)/(A + D + T)** — Hubert & Arabie's Eq. 3 (`hubert1987.md`).

### The exact randomization p-value — pp. 165–166

"The number of times the number of predictions met in the original data set is
equaled or exceeded over all permutations divided by the total number of
permutations provides the exact probability of the hypothesized model occurring
by chance" (p. 166). Enumeration is over all relabelings of the matrix rows and
columns; for six variables that is 6! = 720 applications.

### Worked RIASEC example — pp. 165–166

For the Table 1 correlation matrix (ACT/UNIACT, 358 males), a circular order
model yields **72 unique order predictions**; "69 of the 72 order predictions
were confirmed, two were not confirmed, and there was one tie. The
correspondence index was .93" (p. 165) — i.e. (69 − 2)/72 = .9306. The exact
p-value was **p = .0167 (= 1/60**, the 12/720 tie for the maximum; p. 166).

### Program scope — p. 167

RANDALL "will only accept data matrices with four, six, or eight variables …
though it can easily be converted to other size variable sets," assumes a
**symmetric** input matrix, and reports the counts confirmed / not confirmed /
tied, the exact probability, and the CI. It enumerates exhaustively; the note
describes **no Monte-Carlo option**.

## Reconciliation with the shipped code

`structure_randall()` computes the CI, and `structure_randall_test()` the exact
p-value, over the package's circumplex prediction set (`randall_predictions()`).
Points specific to this source:

- **CI ↔ code index, and the tie departure — demonstrated on Tracey's own
  example.** The code's `2 * mean(vals[ia] > vals[ib]) - 1`
  = `(A − D − T)/(A + D + T)` equals Tracey's CI `(A − D)/(A + D + T)` **only
  when T = 0**. On the RIASEC example (A = 69, D = 2, **T = 1**) Tracey's CI is
  (69 − 2)/72 = **.93**, whereas the code's strict `>` scores the tie as a
  non-agreement and returns 2·(69/72) − 1 = **.92**. The gap is exactly T/N and
  vanishes for continuous correlation matrices (exact ties measure-zero), which
  is every real `fit_structure()` input — so the departure is real but never
  bites in practice. (Full derivation in `hubert1987.md`.)
- **The Monte-Carlo branch is not RANDALL's.** `structure_randall_test(n_perm=)`
  uses random relabelings with the `(M + 1)/(N + 1)` estimator; that path traces
  to Hubert & Arabie (1987, p. 177), **not** to Tracey, whose program enumerates
  exhaustively and offers no sampled option.
- **Scope generalized.** RANDALL is fixed to 4/6/8 variables; the package
  accepts `length(scales) >= 4`, enumerates exactly for `nv <= 9`, and switches
  to the sampled p-value beyond — a superset of RANDALL's fixed sizes.

## What this source does and does not license

It licenses the **name and the interpretive scale** of the CI (its +1/0/−1
anchors) and pins a worked example. It does not license the circumplex
prediction pattern itself (the package's own `randall_predictions()`), and its
statistical warrant is entirely Hubert & Arabie's.

## Traces to

- `R/fit_structure.R` — `structure_randall()` (the CI), `structure_randall_test()`
  (exact p-value), surfaced through `fit_structure()` as RANDALL.
- `tests/testthat/test-fit_structure.R` — RANDALL tests.
- `vignettes/evaluating-circumplex-structure.Rmd`; `man/fit_structure.Rd`
  (`@references`).
- Companion: `hubert1987.md` (the statistical source).

## Open questions

- **No human read.** Both channels are machine (OCR text layer; a rendered-image
  read of p. 165). A scan defect on an unrendered page would be caught by
  neither — dated 2026-07-20.
