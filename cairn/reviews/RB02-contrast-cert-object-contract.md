# RB02: Contrast certification-conditional object contract (M15)

- **Date:** 2026-07-12
- **Output required:** write findings to `cairn/reviews/RR02-contrast-cert-object-contract.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

**Package.** `circumplex` is a CRAN R package for circumplex data analysis
via the Structural Summary Method (SSM). An `ssm_analyze()` fit yields, per
profile row, six parameters: elevation (e), x, y, amplitude (a), displacement
(d, an angle in degrees), and model fit (R²). With `contrast = TRUE` and a
two-level grouping, it appends a **contrast row** — the second-minus-first
difference of the two profiles' parameters. Its "amplitude" is Δa, a *signed
difference of amplitudes*, and its "displacement" is Δd, an angular contrast.

**The interpretability guardrail.** `print.circumplex_ssm()` gates a
**profile's** displacement on a certification rule: displacement is
interpretable only when the amplitude CI's lower bound, rounded at print
precision, exceeds zero (`ssm_certified()`, `R/ssm_oop.R:116-124`). Crucially,
`print.circumplex_ssm()` applies **no** certification gate to a **contrast**
row (`R/ssm_oop.R:172-190`): a contrast's amplitude/fit are differences, not
prototypicality measures, so the "amplitude CI includes zero → displacement
not interpretable" note is profiles-only.

**The trustworthiness diagnostic.** `ssm_ci_accuracy()` (an implementation of
the Zimmermann & Wright, 2017 CI-trustworthiness idea) is a parametric
coverage simulation: it takes a fitted `ssm_analyze()` object, re-simulates
from closed-form population truths along an amplitude "ladder," and tallies
how often the object's own CI procedure covers the truth — per Profile ×
Parameter × Condition. Because the shipped guardrail conditions
interpretation of a *profile's* displacement on certification, the diagnostic
also reports displacement coverage **conditional on certification**, and
measures the guardrail's operating characteristics (false-certification rate).

**The inconsistency M15 fixes.** For the **contrast** row, the diagnostic
currently reports its displacement coverage on the **certification-conditional**
column (`d_conditional`), conditioned on *joint* certification of both
underlying profile rows — even though `print.circumplex_ssm()` never exposes
any certification event for a contrast. So the diagnostic conditions the
contrast's displacement coverage on an event the package never shows the user
for that row. This split is a deliberate prior decision ("Milestone-close
review #3"), pinned at `tests/testthat/test-ci_accuracy.R:221-250`: it
suppresses the contrast's false-certification *verdict* (guardrail `Caution`
NA'd) but *keeps* the contrast's displacement coverage certification-conditional.

**Milestone M15 — the locked decision.** Direction A: `ssm_ci_accuracy()`
will report the contrast's displacement coverage **unconditionally**, matching
print's profiles-only certification stance. **This direction is fixed** (see
Constraints) — you are NOT asked whether to do it. The residual question this
review must settle is the **object contract**: reporting the contrast's
displacement unconditionally in *print* is clear, but the returned
`circumplex_ci_accuracy` object carries the certification-conditional
information across **three linked surfaces**, and we need a coherent rule for
all three.

## Materials

Read these regions (they are small and specific):

- `R/ssm_oop.R:116-193` — `ssm_certified()` (the single definition of the
  rule) and `print.circumplex_ssm()`; note the profiles-only gate at 172-190.
- `R/ssm_ci_oop.R:60-145` — `ssm_ci_verdict_blocks()`, the diagnostic's
  print/summary path. Note the loop over `c("e", "a", "d_conditional")` at
  ~line 84 (the contrast's displacement line is built from the
  **certification-conditional** parameter `d_conditional`), the `" when
  certified"` framing at ~line 106, and the profiles-only guardrail block at
  110-138 (with the comment explaining the contrast still uses joint-cert
  conditioning for its certified-displacement coverage line).
- `R/ssm_ci_accuracy.R:91-131` (roxygen `@return`) — the object contract: the
  `coverage` data frame carries per Profile × Parameter × Condition both the
  unconditional `Coverage` and the certification-conditional
  `Coverage_conditional` (with the count of certified replicates), and the
  `guardrail` data frame carries `Cert_rate` / `Caution` / `Benchmark`.
- `R/ssm_ci_accuracy.R:546-551, 650-695` — where coverage rows and the
  guardrail table are assembled; note the comment that the contrast's
  `Cert_rate` is "only the conditioning rate for certified-displacement
  coverage ... not a rule the package displays," and its `Caution` is NA'd.
- `tests/testthat/test-ci_accuracy.R:221-250` — the Milestone-close review #3
  test that M15 supersedes.
- `devel/m4-ci-accuracy-spec.md` §4.1 (Contrast objects) and §4.2 (What is
  evaluated on the ladder). §4.1: "Δa is a signed, unconstrained difference,
  so the boundary pathology motivating this module does not apply to it."
  §4.2: displacement coverage is evaluated "unconditional and
  certification-conditional; NA at c = 0."

The three object surfaces that carry certification-conditional info for the
contrast row:
1. `coverage$Coverage_conditional` (+ the certified-replicate count) for the
   contrast's displacement rows.
2. `verdict` — the printed displacement Class is computed on the
   `d_conditional` parameter; for the contrast that Class is a
   certification-conditional classification.
3. `guardrail$Cert_rate` for the contrast (the joint-certification
   conditioning rate; its `Caution` is already NA).

To run the diagnostic on a contrast:
```r
devtools::load_all()
data("jz2017"); jz <- jz2017[1:240, ]
set.seed(311)
obj <- ssm_analyze(jz, scales = PANO(), grouping = "Gender",
                   contrast = TRUE, boots = 60)
set.seed(312)
res <- ssm_ci_accuracy(obj, reps = 12, amplitude_factors = c(1, 0),
                       structure = "observed")
print(res); str(res$coverage); str(res$guardrail); str(res$verdict)
```

## Questions

1. **The object contract.** Direction A makes the contrast's *printed*
   displacement coverage unconditional. For the **returned object**, should
   the certification-conditional information for the contrast row be
   (a) left intact in the object and merely suppressed from print, or
   (b) also removed/NA'd from the object? Answer per surface — give a coherent
   rule covering all three of `coverage$Coverage_conditional`, the `verdict`
   Class, and `guardrail$Cert_rate` for the contrast row. Justify why the
   three should be treated the same or differently.

2. **The contrast's displacement Verdict/Class.** If the contrast's
   displacement is unconditional, should its Class be **recomputed on the
   unconditional coverage** column, or dropped/marked not-assessable? Which is
   statistically defensible for an angular contrast whose Δa is a signed
   difference, and what should print show for that line?

3. **`guardrail$Cert_rate` for the contrast.** It is currently "only a
   conditioning device, not a displayed rule." Under Direction A that
   conditioning no longer drives any displayed number. Should the contrast's
   `Cert_rate` be retained (as documented provenance), NA'd, or removed from
   the guardrail table? Weigh reproducibility/auditability against not
   shipping a figure with no consumer.

4. **What is lost by reversing Milestone-close review #3.** Is there a
   legitimate analytic use for the contrast's certification-conditional
   displacement coverage that unconditional-only reporting would discard? If
   yes, how could it be preserved without the print inconsistency (e.g., kept
   in the object but not printed) — and does that change your answer to Q1?

5. **Backward compatibility / least surprise.** The package is heading to a
   2.0.0 CRAN release. For each surface in Q1, classify the proposed change as
   silently altering an existing column's contents, adding a column, removing
   a column, or changing values to `NA`. Recommend the option that is least
   surprising to a downstream consumer reading `res$coverage` / `res$guardrail`
   programmatically, and state whether any change warrants a NEWS entry.

## Constraints

- **Direction A is fixed** (chosen at the M15 plan gate, 2026-07-12): the
  reconciliation is "`ssm_ci_accuracy()` matches `print.circumplex_ssm()`,"
  reporting the contrast's displacement unconditionally.
  `print.circumplex_ssm()` is **NOT** changed. Directions B (make print
  certify contrasts) and C (a bespoke contrast-certification semantic) were
  rejected — do not relitigate them. If you believe Direction A is wrong,
  flag it explicitly under "Beyond the brief" rather than answering a
  different question.
- **Fixed premise (M4 spec §4.1):** a contrast's Δa is a signed, unconstrained
  difference; the near-zero amplitude boundary pathology that motivates the
  certification guardrail does not apply to it. Treat this as settled.
- **`ssm_certified()` is the single definition of the rule** (`R/ssm_oop.R`);
  any object change must not alter how *profiles* are certified or reported.
- **Package doctrine:** base R + minimal deps (no tidyverse in package code);
  statistical correctness outranks all other concerns; do not propose new
  dependencies.
- This review decides the object contract only; it does not need to write code
  (the milestone's implement phase will), but concrete field-level
  recommendations are expected.

## Output format

In `RR02-contrast-cert-object-contract.md`: answer each question by number
with your reasoning and evidence; list any additional findings separately
under "Beyond the brief"; end with concrete recommendations, each marked
apply / consider / reject-with-reason.
