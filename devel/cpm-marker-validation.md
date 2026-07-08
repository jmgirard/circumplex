# CPM boundary-marker validation — results memo (B6 follow-up)

**What this is:** the measurement commissioned by
`devel/cpm-marker-validation-brief.md` (v2.0.0 pre-release item "B6
analytic-CI caution"): does the `cpm_boundary_markers()` set
(R/cpm_fit.R) actually separate analytic-CI mis-covering fits from
well-covered ones, *as a runtime-observable predictor*, in the N band
(2000 ≤ N < 50000) where `summary()`'s caution is marker-conditional?
The two ratified N thresholds (2000 / 50000) were not re-examined.

**Provenance:** `devel/cpm-marker-validation.R` (committed), 2026-07-08.
70,000 fits (7 truth configs × N ∈ {2000, 5000, 10000, 20000, 50000} ×
2000 reps), all on the literal `cormat` path (analytic Wald CIs, no
bootstrap machinery), deterministic per-replicate seeds off
`BASE_SEED = 20260708` (disjoint from the B6 oracle's). 0 errored fits;
99.6% met the acceptance criterion (tables condition on acceptance, as in
B6). Aggregates committed in `devel/cpm-marker-validation-summary.rds`;
the ~1.7 MB per-fit record is regenerable, not committed. MC intervals
are cluster-level (per-fit coverage proportions), so within-fit
dependence is respected.

**Truth configs.** Trailing-harmonic ladder at ζ = 0.75 with
β = (.50−t, .35, .15, t), t ∈ {0, .02, .05, .10, .15} — t = .05 *is* the
committed B6 "boundary" config — plus the B6 "interior" anchor
(.35, .30, .20, .15). **Scope addition, per the brief's provision:** under
ζ = 0.75 the Heywood and ill-conditioned-Hessian markers fired in ≤ 4% of
fits (N = 2000 only), far too rarely to estimate conditional coverage, so
one provocation config `zhigh` (ζ = 0.97, interior β) was added. It fires
`heywood`/`illcond` at 98%/95% at N = 2000, decaying to ~0 by N = 50000,
and never trips the small-weight marker, keeping the marker subsets
separable.

## Headline results

### Any-marker (what `summary()` actually gates on), by N

Angle coverage of nominal-95% analytic CIs, defined CIs only:

| N | fired: n, angle, ζ | quiet: n, angle, ζ |
|---|---|---|
| 2000 | 8276, .882, .921 | 5556, .907, .942 |
| 5000 | 8344, .882, .918 | 5579, .941, .947 |
| 10000 | 7867, .893, .920 | 6104, .948, .951 |
| 20000 | 7287, .909, .922 | 6706, .951, .949 |
| 50000 (control) | 6924, **.941**, .942 | 7071, .945, .939 |

The composite discriminates at every band N (fired ≈ .88–.91 vs quiet
≈ .94–.95 for angles above N = 2000), and the discrimination vanishes
exactly at the N = 50000 gate — the marker-conditional design is coherent
end-to-end.

### Per-marker conditional coverage, band pooled (N 2000–20000)

n = fits; coverage over defined CIs; **NA-CI rate** = fraction of fired
fits whose Wald CIs are NA. Mechanism (verified against
`cpm_analytic_se()`, R/cpm_fit.R:930–940): the SEs come back NA — all
three families at once — exactly when `solve()` rejects the
finite-difference Hessian as computationally singular; an
indefinite-but-invertible Hessian instead has negative asymptotic
variances *clamped to zero* (`pmax`), yielding zero-width CIs that score
as misses in the coverage columns. Both failure modes are real shipped
behavior and both concentrate where these markers fire (empirically, the
per-fit NA pattern is strictly all-or-nothing across families — 3454 of
70000 fits, every one carrying the ill-conditioned-Hessian marker):

| marker | n fired | NA-CI | angle fired/quiet | ζ fired/quiet | β fired/quiet |
|---|---|---|---|---|---|
| Heywood | 5199 | **.65** | .899 / .913 | **.836** / .936 | .919 / .930 |
| harmonic removed | 4107 | .00 | .948 / .910 | .948 / .931 | .953 / .928 |
| small weight (β<.10) | 26720 | .00 | **.890** / .936 | .926 / .940 | .919 / .941 |
| ill-cond. Hessian | 4337 | **.78** | .859 / .914 | **.757** / .936 | .910 / .930 |
| near-tied optima | 114 | .14 | .911 / .913 | **.815** / .933 | .936 / .930 |
| **any marker** | 31774 | .11 | .892 / .938 | .920 / .947 | .920 / .941 |

MC ± is ≤ .003 for the high-n rows (`small_beta`, `any marker`, and every
quiet row), ≤ .014 for the rarer fired rows (`heywood`, `removed`,
`illcond`), and ± .035 for `multimodal`'s ζ.

### β-cut sweep (judgment call #1) — **keep 0.10**

Shipped semantics (min over all β̂, polish zeros included), band pooled:

| cut | angle fired / quiet | false-alarm rate |
|---|---|---|
| 0.05 | **.951 / .893 — discriminates BACKWARDS** | .363 |
| **0.10 (shipped)** | **.890 / .936 — correct direction** | .481 |
| 0.15 | .908 / .936 — diluted | .810 |

The 0.05 reversal is the study's most instructive finding: analytic-CI
mis-coverage is **not** worst at the boundary itself but *near* it. Truths
with trailing β = 0 or .02 cover essentially nominally (angle .94–.955 at
every N — the polish absorbs the boundary), while the trailing-β = .05
regime is the disaster (angle .70–.82 across the band, ζ .85–.88). A 0.05
cut fires on the well-covered at-boundary fits (β̂ ≈ 0) and misses about
half of the mis-covering β̂ ≈ .05 fits; 0.15 buys almost no extra
sensitivity at nearly double the false alarms. 0.10 is the only cut of the
three that points the right way; the evidence supports the shipped
constant, not a change. (A retained-harmonics-only variant of the min is
marginally better — .879/.938, false alarm .40 — but the gain is small and
it never changes the any-marker gate, since removed ⇒ min β̂ = 0 fires the
shipped marker anyway. Not worth a code change now; note for the
simulation paper.)

### Multimodality (judgment call #2) — **keep**

`multimodal` fired 114 times in the band (all in the high-ζ config; it
never fired under ζ = 0.75). Given firing: ζ coverage **.815**
[.780, .850] vs .933 unfired, plus a 14% NA-CI rate; angles show no
signal (.911 vs .913). Its false-alarm rate is essentially zero (0.001 of
clean fits). So the reasoning that near-tied optima mark the same
weak-identification regime is *empirically supported for ζ*, on thin but
unambiguous evidence (the ζ interval excludes nominal decisively). Keep.
Caveat honestly: the estimate comes entirely from the ζ = 0.97 regime and
is near the ~100-firing power floor; the post-M4 simulation paper should
re-measure it on a wider factorial.

## Per-marker verdicts

- **Heywood communality — keep.** 65% of fired fits have *no analytic CI
  at all* (NaN); of the defined ones, ζ coverage .836. A marker that fires
  on CIs that either don't exist or mis-cover earns its caution line
  twice over.
- **Boundary harmonic removed — keep, but it is a null as a mis-coverage
  predictor.** Fired fits cover *better* than nominal (.948/.948/.953) —
  removal fires almost only when the truth is exactly at the boundary
  (t = 0, ~51% fire rate), where the polish does its job and the reduced
  model's Wald CIs behave. Dropping it would not change the caution's
  gating at all (removal forces min β̂ = 0, which fires the small-weight
  marker), so this null is inconsequential for behavior; keeping it costs
  nothing and preserves the more informative diagnostic name in the
  printed caution. Honest reading either way: no evidence it *predicts*
  mis-coverage.
- **Small correlation-function weight, β < 0.10 — keep the 0.10 cut**
  (sweep above; 0.05 is affirmatively wrong, 0.15 dominated).
- **Ill-conditioned Hessian — keep.** The strongest single predictor:
  78% NA-CI rate given firing; defined-CI ζ coverage .757.
- **Competing near-tied optima — keep** (above).

## Honest nulls and caveats

1. `removed` does not predict mis-coverage (see verdict; behaviorally
   inert to drop, kept for diagnostic wording).
2. `heywood`/`illcond`/`multimodal` conditional estimates come almost
   entirely from the ζ = 0.97 provocation config; under ζ = 0.75 they fire
   too rarely to test (≤ 4%, N = 2000 only). Their verdicts are "work
   where they fire," not "fire wherever CIs fail."
3. `heywood` at the N = 50000 control fired 15 times with angle coverage
   .762 — underpowered, but a hint that the rare fit still Heywood at huge
   N is genuinely pathological; nothing gates there today.
4. Residual unflagged mis-coverage at the band edge: at N = 2000,
   marker-quiet fits still cover angles at ~.907 (interior truth .915) —
   mild, inherent to the ratified 2000 threshold sitting at the edge of
   the Wald regime; recorded here, no change proposed.
5. The any-marker false-alarm rate is substantial (.499 of clean band
   fits — every angle and ζ CI defined and covering — fire something;
   the β marker alone accounts for .481, firing on near-boundary truths
   that happen to cover). That is the accepted cost of a regime marker
   with an advisory, one-line consequence; it buys fired-set coverage
   discrimination at every band N.

## Verdict (for Jeff)

**The shipped marker set is defensible as-is; no change is supported, let
alone required.** Both judgment calls survive measurement: 0.10 is the
only β cut of the three that discriminates in the right direction (0.05
is affirmatively backwards — mis-coverage peaks *near* the boundary, not
at it), and `multimodal` fits mis-cover ζ decisively when it fires while
flagging essentially no clean fits. The two markers that couldn't be
adjudicated by judgment now have measured teeth (`illcond`: ζ coverage
.76 and 78% nonexistent CIs given firing), one marker (`removed`) is a
predictive null that is behaviorally inert and harmless to keep, and the
composite's discrimination disappears exactly at the 50000 gate. Ship it
unchanged; hand the retained-β min refinement and a wider-factorial
re-measurement of `multimodal` to the post-M4 simulation paper.
