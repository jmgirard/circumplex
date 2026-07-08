# Transcription: Cheung & Rensvold (2002) ΔGFI invariance criteria

**Purpose:** resolves the M5 spec's TBT item for T4 (spec §6.2/§12.2: ΔCFI
cutoffs "offered only once transcribed... candidate source Cheung & Rensvold
2002"). Transcribed 2026-07-07 from the published article (read in full from
the PDF; page numbers are the journal's). House oracle rule (Brief A §6.1):
these values enter code/tests only from this record, never from memory.

**Source:** Cheung, G. W., & Rensvold, R. B. (2002). Evaluating
goodness-of-fit indexes for testing measurement invariance. *Structural
Equation Modeling, 9*(2), 233–255.

## The criteria (verbatim, with page cites)

From p. 250 (introducing Table 5's critical values):

> "...shown in Table 5 are the critical values for rejecting the null
> hypothesis of equivalence, with an alpha of 0.01 and assuming multivariate
> normal distributions."

From pp. 250–251 (the general criterion):

> "Although the standard errors and critical values differ for the different
> invariance models, the between-model variations are so small that a general
> criterion for all hypotheses can be proposed. A value of ΔCFI smaller than
> or equal to –.01 indicates that the null hypothesis of invariance should
> not be rejected. For ΔGamma hat and ΔMcDonald's NCI, the critical values
> are –.001 and –.02, respectively."

## ⚠️ Internal contradiction in the source, and the operational rule

The p. 251 sentence as printed says ΔCFI ≤ −.01 means invariance "should
**not** be rejected." That wording contradicts the paper's own construction:
the critical values are the **1% tails of the simulated null-hypothesis
ΔGFI distributions** (p. 250, quoted above; Table 5's "1%" columns), so a
ΔGFI at or below its critical value is precisely the 1%-level evidence
**against** invariance. It also contradicts the abstract's framing ("We
propose critical values of these ΔGFIs that indicate measurement
invariance") read together with the Discussion's account of ΔGFI < 0 as
fit degradation under added constraints (p. 250). The field's standard
operationalization of "the Cheung–Rensvold criterion" matches the
simulation logic, not the miswritten sentence.

**Operational rule for this package (T4), stated exactly:** with
ΔGFI = GFI(more constrained) − GFI(less constrained) for adjacent rungs of
the invariance ladder,

- ΔCFI **< −.01** → reject that invariance step (CFI dropped by more
  than .01); ΔCFI ≥ −.01 → the step is retained by this criterion;
- ΔGamma hat < −.001 → reject; ΔMcDonald's NCI < −.02 → reject.

Any implementation must cite this note, not the p. 251 sentence alone, and
the user-facing docs must attribute the criterion with its published alpha
(.01) and direction convention.

## Scope caveats stated by the source (binding on T4's doc wording)

Transcribed from "Limitations of the Simulation" (p. 251) and the Discussion
(p. 250):

- **Two groups only** ("this simulation is limited to measurement models
  with two groups. Suitability of the recommended GFIs for testing across
  three or more groups is an interesting topic for future study," p. 251).
- **ML estimation only** and **multivariate normal data only** (p. 251).
  Note for T4: the package's default fitting path is MLR (robust); the
  ΔCFI criterion was not simulated under robust estimation, and robust CFI
  variants did not exist in their study — the docs must not imply the
  cutoff was validated for robust indices.
- **Type I error only** — power to detect real non-invariance was not
  examined (p. 251).
- The recommended trio (ΔCFI, ΔGamma hat, ΔMcDonald's NCI) was selected
  for being **independent of model complexity and sample size and
  uncorrelated with overall fit** (abstract; p. 250: the only difference
  statistics without the undesirable overall-fit correlation are "ΔCFI,
  ΔGamma hat, ΔMcDonald's NCI, ΔNCP, ΔIFI, ΔRNI, and Δcritical N").
- Invariance-hypothesis labels in their Table 5: H2 = metric (weak
  factorial); H3 = partial metric; H4 = metric + residual variances;
  H5 = strong factorial (metric + scalar); H6 = metric + construct
  variance; H7 = metric + construct covariance; H8 = strong factorial +
  latent means (Table 5 note, p. 249). The general criterion applies
  across these (pp. 250–251).

## What T4 does with this

Per spec §6.2/§12.2 (decision: Δχ² default now): the scaled/robust nested
Δχ² test as lavaan's `anova()` computes it remains the **default verdict
statistic**; with this transcription in hand, T4 **may** additionally offer
the ΔCFI ≤/− .01-style flags as a labeled secondary criterion
("Cheung–Rensvold (2002) criterion, α = .01, two-group ML simulation
scope"), or continue printing indices without verdicts — Jeff's call at T4
(spec §12.2). Nothing in this note changes the default.
