# cheung2002 — the ΔGFI invariance criteria, and the direction its own text gets backwards

**Provenance.** Ingested 2026-07-24 by M57 from
`cairn/references/sources/cheung2002.pdf` (gitignored). The shelf PDF is 23
pages and holds the whole article, printed pp. 233–255 on the pages themselves
(PDF page = journal page − 232). Pagination: the article's own page numbers.
The PDF is **born-digital** (`Title: SEM0902.vp`, `Producer: iText 4.2.0` —
a post-processor over typeset output, not an OCR scan; the text layer carries
true en-dashes, `∆` glyphs, and Table 5's numerals), so its `pdftotext` text
layer is a faithful witness. It carries a "Downloaded by [University Of
Pittsburgh] at 13:09 13 June 2016" stamp on every page — a download artifact,
not article content.
Extraction: verified 2026-07-24 against the born-digital `pdftotext -layout` text layer for every page an anchor below names — article pp. 233 (abstract), 248–249 (Table 5 and its note), 250, and 251 — re-read against the source rather than carried over from the 2026-07-07 transcription; no value read by a second human channel — observed 2026-07-24.

**Citation.** Cheung, G. W., & Rensvold, R. B. (2002). Evaluating
Goodness-of-Fit Indexes for Testing Measurement Invariance. *Structural
Equation Modeling, 9*(2), 233–255. The article prints "Copyright © 2002,
Lawrence Erlbaum Associates, Inc." and **no DOI anywhere in the 23 pages** —
none is asserted here (a DOI recalled rather than read is exactly what the
primary-sources rule forbids). Title capitalization above is as printed;
it is commonly cited in sentence case.

**Role.** The published source for `ssm_sem()`'s **secondary** invariance
criterion: the ΔCFI cutoff, its published alpha, and — critically — the scope
the simulation actually covers. The gating verdict statistic is and stays the
nested Δχ² test, a computed quantity needing no literature constant; this page
backs only the reported-only ΔCFI label beside it.

**Two-channel history.** This page is the second record of these values. The
first is `devel/cr2002-transcription.md` (2026-07-07), which read the article
in full and stated the operational rule below. That record is *not* the
authority here: authoring a source note is itself a fresh extraction, so M57
re-read every anchored page against the PDF. The two agree on every value.
Two typographic differences, both in the paper's favor: the article prints
ΔCFI's cutoff as "**–0.01**" with a leading zero (the transcription rendered
it "–.01"), and the general-criterion sentence *begins* on p. 250 and
*finishes* on p. 251 rather than sitting wholly on p. 251.

## Extracted values

### The general criterion (pp. 250–251, verbatim across the page break)

> "Although the standard errors and critical values differ for the different
> invariance models, the between-model variations are so small that a general
> criterion for all hypotheses can be proposed. A value of ∆CFI smaller than
> or equal to –0.01 indicates that the null hypothesis of invariance should
> not be rejected. For ∆Gamma hat and ∆McDonald's NCI, the critical values
> are –.001 and –.02, respectively."

- **ΔCFI cutoff: −0.01** — p. 251, quoted above.
- ΔGamma hat cutoff: **−.001**; ΔMcDonald's NCI cutoff: **−.02** — p. 251,
  same sentence. Neither is wired into any shipped code; both are recorded
  here so a later milestone need not re-read the paper.

### What the critical values ARE (p. 250, introducing Table 5)

> "shown in Table 5 are the critical values for rejecting the null hypothesis
> of equivalence, with an alpha of 0.01 and assuming multivariate normal
> distributions."

- **alpha = 0.01** — p. 250, quoted above.

### ⚠️ The article contradicts itself on the direction, and Table 5 settles it

The p. 251 sentence as printed says ΔCFI ≤ −0.01 means invariance "should
**not** be rejected". That is backwards relative to the paper's own
construction, and **Table 5 (p. 248) is the decisive internal evidence**: its
ΔCFI columns are headed `M`, `SD`, and **`1%`**, and under the true null the
simulated means sit at zero (−.0001 to .0000) while every 1% entry is
**negative**:

- ΔCFI 1% critical values by invariance hypothesis (p. 248, Table 5):
  H2 **−.0085**, H3 **−.0039**, H4 **−.0094**, H5 **−.0082**, H6 **−.0056**,
  H7 **−.0048**, H8 **−.0055**.

These are the 1% **lower** tails of the null-hypothesis ΔGFI distributions, so
a ΔCFI at or below its critical value is precisely the 1%-level evidence
**against** invariance — and −.01 is a single conservative rounding covering
all seven. The abstract (p. 233) frames the values the same way: "We propose
critical values of these ∆GFIs that indicate measurement invariance."

**Operational rule this repo implements** (with ΔGFI = GFI(more constrained) −
GFI(less constrained) for adjacent rungs):

- ΔCFI **< −.01** → reject that invariance step; ΔCFI **≥ −.01** → the step is
  retained by this criterion. A value exactly at the cutoff retains.

`R/ssm_sem.R` cites this page, not the p. 251 sentence, and the user-facing
docs attribute the criterion with its published alpha and this direction.

### Scope the simulation covers — binding on every doc claim

From "Limitations of the Simulation" (p. 251) and the Discussion (p. 250):

- **Two groups only.** "Finally, this simulation is limited to measurement
  models with two groups. Suitability of the recommended GFIs for testing
  across three or more groups is an interesting topic for future study."
  (p. 251)
- **ML estimation only, multivariate normal data only.** "generalizability is
  limited by the fact that only ML estimation was used"; "this study
  stipulated that the data distributions were multivariate normal. Deviations
  from multivariate normality may affect the results." (p. 251) Robust CFI
  variants are not in the study at all — no cutoff here was validated for one.
- **Type I error only.** "we only examined the Type I error when testing for
  measurement invariance" — power was not examined (p. 251).
- Why this trio and not the other seventeen ΔGFIs: they are the ones
  uncorrelated with overall fit. "the only difference statistics not having
  this undesirable characteristic are ∆CFI, ∆Gamma hat, ∆McDonald's NCI,
  ∆NCP, ∆IFI, ∆RNI, and ∆critical N" (p. 250); the abstract adds that the
  recommended three "are independent of both model complexity and sample
  size" (p. 233).
- **Invariance-hypothesis labels in Table 5** (note, p. 249): H2 = metric
  (weak factorial); H3 = partial metric; H4 = metric + invariance of residual
  variance; H5 = strong factorial (metric + scalar); H6 = metric + invariance
  of construct variance; H7 = metric + invariance of construct covariance;
  H8 = strong factorial + invariance of latent means. The general criterion
  applies across all of them (pp. 250–251).
- **768,000 CFA models**, 96 combinations of model parameters and sample
  sizes (p. 251).

## Traces to

- `R/ssm_sem.R:759-771` (`sem_dcfi_cutoff`) — the −.01 cutoff and the
  direction, with this page named as the authority over the p. 251 sentence.
- `R/ssm_sem.R:773-790` (`sem_dcfi_flag`) — the ≥ −.01 retain boundary and the
  scope restriction, from the Limitations section.
- `R/ssm_sem.R:792-824` (`sem_dcfi_note`) — the user-facing attribution and
  published scope label printed beneath the invariance ladder, plus the reason
  named when the verdict is withheld.
- `R/ssm_sem.R:931-950` (`sem_fit_ladder`) — the scope test itself: two groups
  AND ML estimation AND a plain normal-theory CFI. All three are required and
  no two imply the third; the ML clause was added at the M57 review, which
  found that `GLS`/`WLS`/`ULS`/`DWLS` yield a plain-NAMED CFI while not being
  the estimator this source simulated (D-028).
- `tests/testthat/test-ssm_sem_groups.R` — the ΔCFI section: the deterministic
  pin of the ≥ −.01 boundary, both flag directions, and the robust-CFI and
  three-group scope gates.
- `vignettes/sem-based-ssm-analysis.Rmd` — teaches the criterion with these
  scope caveats.
- `man/ssm_sem.Rd` (`@references`, `invariance` parameter) — the
  user-facing attribution.
- `devel/cr2002-transcription.md` — the 2026-07-07 first-channel record; kept
  as the transcription-protocol artifact, superseded by this page as the
  citable record.

## Open questions

- ΔGamma hat (−.001) and ΔMcDonald's NCI (−.02) are transcribed here but not
  implemented; a ROADMAP candidate row carries them — observed 2026-07-24.
- Nothing in the article states how its criterion should behave under a robust
  (scaled) CFI or with more than two groups, and this page will not acquire
  such a value from anywhere else: it would take simulation Cheung & Rensvold
  never ran. `ssm_sem()` therefore reports the difference and withholds the
  verdict there — observed 2026-07-24.
