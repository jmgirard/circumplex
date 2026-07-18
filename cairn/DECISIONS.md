# Decisions

Append-only. Never renumber; supersede with a new entry.

**Pre-migration decisions:** the full decision rationale predating cairn lives
in `cairn/DESIGN.md` (kept verbatim at migration — a living design doc with an
embedded decision log and statistical conventions) and in the entombed
`cairn/legacy/`. Only still-governing, cross-cutting decisions are re-recorded
below, each citing its legacy anchor (Compromise A); full decision-log
extraction is deferred to a later `/design-interview` run. The statistical
invariants in CLAUDE.md / DESIGN.md "Statistical conventions" are hard
constraints whose IP/GP formalization is likewise deferred to
`/design-interview`, not forced at migration time.

### D-001 (2026-07-07, re-recorded): v2.0.0 bundles M2–M5 as one CRAN submission

**Context:** Progress outran the original tiered submission train; CRAN
discourages churn (~1 submission / 1–2 months).
**Decision:** Fold M2 (inference), M3 (viz), M4 (Browne + CI trustworthiness),
M4.5 (structure tests), and M5 (SEM) into a single v2.0.0 release (target
~2026-08-02; code freeze ~2026-07-26). Scope is the variable, never the date or
the statistics. M6 (longitudinal) is excluded → its own ~v2.1.0.
**Consequences:** GitHub milestones decouple from CRAN submissions; work
accumulates on master until the release train leaves. Source: legacy ROADMAP
"CRAN release strategy".

### D-002 (2026-07, re-recorded): BCa CIs dropped for circular displacement

**Context:** BCa's bias-correction/acceleration are order-statistic concepts
defined on a line, not a circle.
**Decision:** No BCa CIs; the one real beneficiary (amplitude coverage near
zero) is handled by M4's CI-trustworthiness diagnostic instead.
**Consequences:** Percentile/basic bootstrap + Monte Carlo only. Source: legacy
ROADMAP "Milestone 2".

### D-003 (2026-07, re-recorded): 0°/360° pole reported as exactly 360.0, not canonicalized

**Context:** A profile peaking on the pole yields `atol2`≈−3e-17 →
`modu(·,2π)`=2π=360.0 (an fmod-at-the-edge float artifact, measure-zero for real
data).
**Decision:** Do not canonicalize; 360.0 matches the package's LM=360
convention. Tests at the boundary accept either ~0 or ~360.
**Consequences:** A pole-hugging CI endpoint may still print 0.0 (the opposite
label) — a parked cosmetic follow-up. Source: DESIGN.md "Statistical
conventions" (G2 decision).

### D-004 (2026-07-07, re-recorded): between-release working practice

**Context:** Real version numbers are bound to CRAN submissions only.
**Decision:** At each milestone close: archive to MILESTONES-ARCHIVE (now
`cairn/`), bump the DESCRIPTION dev suffix, add a lightweight git tag, and run a
milestone-close `/code-review` over the cumulative diff (`high`, or `max` for
statistically risky milestones).
**Consequences:** `install_github` users can identify milestone state; the
CRAN-release review verifies already-reviewed strata rather than making a first
deep pass. Source: legacy ROADMAP "Between releases".

### D-005 (2026-07-12): canonical reading of the `is_*()` validator rule (M10)

**Context:** The CLAUDE.md "prefer the `is_*()` helpers" rule was read two ways
across the codebase. Length was carried inconsistently: `is_num`/`is_char`/`is_var`
take an explicit `n=` length argument; scalar counts were validated either by
bolting `length(x) == 1` beside `is_count()` (`ssm_ci_accuracy`, `cpm_fit`), by an
inline `is.numeric && ceiling==floor` with no length guard at all (`ssm_sem`), or
by stacking `is_num(x, n = 1L), is_count(x)` (`ssm_sem_syntax`).
**Decision:** Length belongs *in the predicate name or argument*, never
hand-bolted at the call site. Two idioms are canonical: (a) `is_*(x, n = k)` for a
vector of known length `k`; (b) a named scalar predicate that fixes length-1 —
`is_flag()` (logical) and now `is_scalar_count()` (non-negative whole number,
`min` floor). `is_count()` is retained **only** as the vectorized
non-negative-integer test used as the internal `n=` guard inside
`is_char`/`is_var`/`is_num`; it is never a user-facing scalar-count validator.
**Superseded reading:** that `is_count()` alone (with or without a bolted
`length(x) == 1`) is the scalar-count validator. Callers now use
`is_scalar_count()`; the standalone `length == 1` companions are removed.
**Consequences:** Scalar count args gain a uniform, length-checked validator; the
`ssm_sem` and extra `cpm_fit` sites that lacked a length guard are now strictly
stricter (reject length>1). The `is_flag()` length-1-logical sibling
(`R/instrument_oop.R:68`) already conforms to idiom (b) and is out of scope.

### D-006 (2026-07-12): keep S3 for the angle classes — vctrs/S7 migration dropped

**Context:** A candidate proposed migrating `circumplex_degree`,
`circumplex_radian`, and `circumplex_contrast_radian` from S3 numeric
subclasses to vctrs or S7. Flagged `ip-touching` + `irreversible-api`;
escalated to independent Fable review (RB01 → RR01, 2026-07-12).
**Decision:** Keep S3. The classes are transient boundary dispatch tags, not
persistent data types — every consumer converts at the boundary
(`as_radian(as_degree(x))`) and both custom `quantile` methods `unclass()`
their input immediately, so no generic vctrs/S7 benefit (class-loss safety,
mixed-unit guarding, validators) is a live problem here. vctrs' strictness is
*anti-fit*: it errors on the transparent numeric arithmetic `angle_dist()`
(`R/utils.R:65–69`) and the unit converters rely on, and reaching behavioral
parity would triple the class code (`vec_arith`/`vec_math`/`vec_ptype2`/
`vec_cast`/`format`). S7 is the wrong tool for vector classes and is pre-1.0. A
direct vctrs Import breaches the minimal-deps / no-tidyverse-in-package-code
doctrine and raises the R floor. For code implementing the package's hardest
invariants (pole-snap, contrast continuous branch, branch alignment) the only
possible numeric outcome is "identical or worse."
**Re-trigger:** reopen only on a concrete, test-demonstrated defect traceable
to the S3 tag design (a real mixed-unit bug the convert-at-boundary idiom
missed, or a hard requirement for angle vectors as first-class tibble columns).
Modernization alone never re-triggers.
**Consequences:** D-003 (pole reported as exactly 360.0) is explicitly
unaffected and stands. Small S3-local follow-ups surfaced by RR01 are spun off
as a ROADMAP candidate (a `new_contrast_radian()` constructor for the two
inline `structure()` sites; deciding the export status of the internal
`as_degree`/`as_radian` generics; `NA_real_` all-NA return + CPM angle-CI
oracle path when the quantile methods are next touched). Source: RR01.

### D-007 (2026-07-12): displacement certification rule = scale-free lower-bound ratio (M16)

**Context:** The shipped displacement-interpretability guardrail
`ssm_certified(a_lci, digits) = round(a_lci, digits) > 0` (`R/ssm_oop.R:122`)
is print-dependent (moves with the display `digits`) and scale-dependent (its
implied threshold `0.5·10⁻ᵈⁱᵍⁱᵗˢ` is in amplitude units). The M4 CI-accuracy
spec (§12.5/§13) shipped it as-is and scheduled a principled replacement seeded
by the diagnostic's own output. The M16 seed
(`devel/m16-cert-rule-seed.{R,rds,md}`) showed it false-certifies a *truly
zero* amplitude ≈100% of the time in every metric (structural: a percentile
interval of positive amplitude replicates cannot contain 0). Escalated to
independent Fable review (RB03 → RR03, 2026-07-12).
**Decision:** Certify a **profile** row's displacement iff
`is.finite(r) && r >= 0.35`, where `r = a_lci / (a_uci − a_lci)` — a pure,
vectorized, base-R function of the amplitude CI **pair only** (`a_est` never
consulted). Print-independent (no `digits`), scale-free (numerator and
denominator carry the same scale factor). Equivalent to `a_lci >= 0.259·a_uci`.
Edge contract: `NA` lower bound → not certified; degenerate zero-width CI
(`Inf`/`NaN`) → not certified, fail-closed. Contrast rows stay ungated
(M15-D1). k = 0.35 is a pinned package constant
**calibrated to the 95% default interval**: it is ≈ the 97.5% point of the
statistic's asymptotically-pivotal c=0 null (Rayleigh) distribution, giving
false-certification ≈ 0.007–0.025 (≤ α/2) across n = 50–1166 while genuine
signals (seed 2.58, 6.24) pass by 7–18×.
**Acceptance target (AC4):** two-part gate — observed false-cert at the c=0
ladder rung ≤ 0.05 (point) AND the diagnostic's Wilson-LCI `Caution` not
firing — verified at reps = 1000 across COR_healthy / COR_nearzero / RAW_means
plus one small-n (≈100) config; ≥2 oracle types (simulation-coverage +
closed-form Rayleigh-tail cross-check `exp(−t*²/2)`, t* ≈ z(1+2k) ≈ 3.33).
**Rejected:** form (b) `a_lci/a_est` (null statistic diverges as amplitude→0,
no viable threshold); replicate-vector / ROPE rules (reparameterizations of the
same quantile info; the `circumplex_ssm` object stores no replicate matrix, so
they break legacy objects for zero gain); α/2 as a hard nominal-level claim
(the rule has no nominal level; α/2 survives only as the Caution benchmark).
**Consequences:** exported print behavior changes (a strictly-positive-but-
near-zero amplitude now prints the not-interpretable note; a v2.0.0
major-version change, NEWS-documented). The print note, verdict, guardrail, and
vignette wording must change from "the amplitude CI includes zero" (never
literally true of a positive-replicate percentile interval) to a
CI-lower-bound-relative-to-width phrasing. `ssm_ci_accuracy()`'s `digits`
argument and `Threshold` output column become vestigial and are **removed**
(not deprecated): the diagnostic is unreleased (new in the dev line toward
v2.0.0; latest CRAN is v1.2.0), so no lifecycle shim is owed — this corrects
RR03's contrary assumption. The k=0.35 calibration is pinned to interval=0.95;
a `k(interval)` generalization is documented as available but deferred (D-003
pole reporting unaffected). Source: RR03.

### D-008 (2026-07-12): CIRCUM free-scaling promoted into v2.0.0 scope; the release date is not a constraint

**Context:** D-001 bundled M2–M5 as v2.0.0, excluded new features (→ v2.1.0),
and targeted a CRAN cadence window (~2026-08-02 submission, ~07-26 freeze). Two
things changed. (1) Jeff asked to add the CIRCUM free-scaling covariance
estimation family (`Σ = D_σ P(γ) D_σ`, for exact reproduction of published
CIRCUM/CircE output; the legacy "decide post-M4" candidate) to v2.0.0. (2) Jeff
confirmed there is **no release-time pressure** — v2.0.0 ships when the
statistics are ready, not on a date.
**Decision:** (a) Supersede D-001's new-features-excluded clause **insofar as it
bars CIRCUM**: the CIRCUM free-scaling family enters v2.0.0 scope as M17
(Fable-reviewed design decision + spec) → M18 (implementation + oracle
validation). M17 may still decide *no-go*, retiring M18. (b) The ~2026-08-02
CRAN window and ~07-26 freeze are **not constraints**; v2.0.0 carries no target
date and ships when its bundle (now including M18, if built) is complete and
validated. D-001's core invariant — *scope is the variable, never the
statistics* — survives and is **extended to the date**: the date, too, yields to
the statistics, never the reverse.
**Scope of the supersession:** narrow. M6 (longitudinal) remains excluded from
v2.0.0 (→ its own ~v2.1.0); this promotes **only** CIRCUM. All other D-001
consequences stand.
**Consequences:** M7 loses its date-anchored block reason — it depends on M18
instead and stays a planned release milestone (no date language). The "CIRCUM
free-scaling compatibility mode" ROADMAP candidate is retired into M17/M18. The
stale "CPM convergence-acceptance vacuous 'reproduced'" candidate is struck: it
was already fixed in M4 review #1 (folding the g0/mirror pair into one start
group, `R/cpm_fit.R:548-549`; `reproduced` now requires ≥2 independent start
groups at min F, `:634`) and is regression-tested at
`tests/testthat/test-cpm_fit.R:595`. Source: Jeff, this session.

### D-009 (2026-07-12): GO on the CIRCUM free-scaling covariance family (M17)

**Context:** D-008 admitted the CIRCUM free-scaling family (`Σ = D_σ P(γ) D_σ`,
for exact reproduction of published CIRCUM/CircE output) into v2.0.0 scope as
M17 (design) → M18 (build), M17 free to decide *no-go*. M17 escalated the design
to an independent Fable review (RB04 → RR04, archived under
`cairn/reviews/archive/`); the central statistical risk was the free-family
analytic gradient (the current gradient's `diag P = 1` simplification no longer
holds once σ is free).
**Decision: GO.** Build the free-scaling family in M18 per the build-ready spec
`devel/circum-free-scaling-spec.md`. Fable-attested load-bearing findings:
(1) reproducing published output *requires* fitting σ — the diag-constrained
family provably cannot (B6); (2) `σ_i = e^{s_i}`, all p free, **no identification
pin** (map injective, F coercive in each σ_i); (3) gradient
`∂F/∂s_i = 2(1 − (Σ⁻¹R)_ii)`, γ blocks = design §3.4 with `A → Ã = D_σ A D_σ`
built from `Σ⁻¹` not `P⁻¹` — derived and FD-verified (worst err 3.6e-9);
(4) **df unchanged** (covariance moment count `p(p+1)/2`); (5) **no analytic σ
CIs ever**, bootstrap stays default, and a free-family coverage-oracle extension
is a **mandatory pre-ship gate** before any analytic-CI-trust claim;
(6) canonicalization untouched (σ invariant under rotation+reflection);
(7) validation anchor already green (OpenMx free-scaling oracle,
`test-cpm_oracles.R:329`) + Grassi et al. (2010) App. A at same-model tolerances.
**Consequences:** M18 stays `planned` (not retired); the flag is orthogonal to
variants A–D (8 combinations). Design-doc §3.2's "scale-invariance ⇒ χ²
validity" claim is refuted for the *diag* family (its true home is the free
family) and is rewritten at M18 doc time. Two items are deferred, **not**
committed to v2.0.0: bootstrap σ CIs, and a T_diag-vs-T_free calibration that
could make the free family the preferable inference default in a future major
version (measure in M18's coverage runs, decide later). D-008 stands. Source:
RR04 (Fable, 2026-07-12).

### D-010 (2026-07-13): free-scaling analytic CIs use the diag N-conditional caution, coverage-validated (M19)

**Context:** M18-D3 shipped an **unconditional** "not yet coverage-validated"
caution on the free-scaling family's analytic (Wald) CIs, because the
free-family coverage oracle was a deferred pre-ship gate (D-009 finding 5): the
diag family's N-conditional caution constants (`cpm_analytic_ci_n_caution =
2000`, `cpm_analytic_ci_n_boundary_caution = 50000`, boundary markers) were
calibrated on the diag family and **must not be silently reused** (spec §4). M19
built and ran that oracle (`devel/m4-coverage-oracle.R` stage 3;
`devel/m19-free-coverage-results.rds`, 500 reps).
**Decision:** Apply the **same** N-conditional caution to the free family,
**now coverage-validated for it** rather than silently reused. The M19 oracle
measured the free family's θ/ζ/β coverage regime to be the diag family's —
interior truths reach the [.90, .98] band at N = 2000 (angle .915), boundary
truths only near N = 50000 (.914) — because the correlation-input contract
forces σ_pop = 1 and σ̂ ≈ 1 at these truths (median max variance-ratio ≈ 1.00
every cell). The `summary()` free branch's placeholder unconditional caution is
removed; free and diag now share the N-conditional ladder, with the free family
additionally always printing the σ²-carries-no-interval note (D-009).
**Second finding (reinforces, not weakens):** the free family's bordered
information matrix (p extra σ nuisance parameters) is singular (NA SE) in 52–55%
of N = 250 fits and 13–14% at N = 1000, ~0% at N ≥ 2000 — an independent reason
its analytic CIs are untrustworthy below N = 2000.
**Oracle types (≥2 bar):** simulation-coverage (the M19 run) + a live
parametric-bootstrap SE cross-check at an interior cell
(`test-cpm_oracles.R`); registered there.
**Supersedes:** M18-D3's "unconditional, not-yet-coverage-validated" free
caution (the deferral is discharged, not a standing rejection). σ̂² still carries
no interval, ever (D-009 unaffected).
**Consequences:** a well-identified free fit at N ≥ 2000 now prints no θ/ζ/β
mis-coverage caution (only the σ² note) — exported `summary()` output change,
NEWS-documented at the v2.0.0 (M7) release. The T_diag-vs-T_free
inference-default decision stays deferred (T_free statistics were collected in
the same runs; ROADMAP candidate). Source: M19 coverage oracle, this session.

### D-011 (2026-07-16): unit family stays the CPM model-test inference default (M21)

**Context:** D-009 deferred whether a T_diag-vs-T_free calibration could make
the free-scaling family "the preferable inference default." M21 measured it
(paired design: both engines fit to the same `R = cor(X)` per replicate, 500
reps × 12 cells at the stage-1 circumplex correlation truths, N ∈
[250, 50000], df = 10, p = 8, m = 3, variant A;
`devel/m21-t-calibration.{R,md}` + results rds) and escalated the decision to
independent Fable review (RB05 → RR05, 2026-07-16; evidence re-verified
against the rds, worst-exclusion cells regenerated from seed).
**Decision:** The **unit family remains the CPM model-test inference
default**; the free family remains opt-in for exact reproduction of published
CIRCUM/CircE output. No conditional default. Grounds (RR05): (1) the measured
tie is decisive — no cell separates the families in any metric (paired ΔT̄ ≤
0.5% of df, paired cor ≥ .998; per-cell SE ≈ 0.013 T-units would have
detected a 1%-of-df difference); (2) the tie is structurally expected under
the correlation-input contract — `diag(R) = 1` makes the p added moments
degenerate and σ̂ ≈ 1, and the df bookkeeping exactly cancels, so neither
family can be better calibrated at correlation input; (3) against a null
benefit stand the free family's costs (bordered-Hessian NA SEs in ~52–55% of
N = 250 fits per D-010, p extra parameters, no σ² intervals per D-009).
**Scope of every equivalence claim** (docs and this entry): the *model test*
only, at *correlation input* only, within the measured envelope — both
families are mildly conservative at small/mid N (rejection .02–.04), reaching
nominal by boundary N ≈ 50000 / interior N ≈ 2000; never "identical"
(T_free ≤ T_unit by nesting, ≤ 0.5% of df).
**Re-trigger (gate, not revisit):** any future milestone shipping
covariance-matrix input (D-009 item 4) must re-run the paired T calibration
at non-unit σ truths **before** that feature ships; the reopened decision
covers both the default and the docs' equivalence wording — at genuine
covariance input T_free is a different, unmeasured statistic.
**Supersedes:** D-009 item 3's deferral (discharged). D-009's other holdings
and D-010 stand. Source: RR05 (Fable, 2026-07-16); M21 T1 analysis.

### D-012 (2026-07-16): longitudinal build merges are not gated behind the v2.0.0 submission (M23 plan gate)

**Context:** D-001 excluded longitudinal (legacy "Milestone 6") from v2.0.0 —
"its own ~v2.1.0" — and D-008 narrowed that exclusion once, for CIRCUM only.
At the M23 plan gate Jeff was asked whether longitudinal *build* milestones
should be merge-gated behind M7 (`Depends on: M7`), keeping the v2.0.0 bundle
frozen. Jeff chose no gate.
**Decision:** Longitudinal build milestones carry no dependency on M7;
longitudinal code merges to master as it becomes complete and validated.
Whichever release train is open at merge time carries it: a build merging
before the v2.0.0 submission expands the v2.0.0 bundle (D-001's exclusion
clause is superseded insofar as it gates merges); after submission, it rides
~v2.1.0. This extends D-008's doctrine — scope is the variable, never the
statistics or the date.
**Scope of the supersession:** merge gating only. The M23 design milestone is
docs-only regardless; every build still requires its own plan, oracle
validation per the validation doctrine, and the normal review gate before
merging — "not gated behind M7" never means "less validated".
**Consequences:** M7's check/win-builder validation surface at submission
time covers whatever has merged, including any longitudinal code that lands
first; the "v2.1.0 benefits from v2.0.0 field feedback" deferral rationale is
dropped. Source: Jeff, M23 plan gate, 2026-07-16.

### D-013 (2026-07-16): the RR06-reviewed longitudinal SSM spec is the binding build contract (M23)

**Context:** M23 turned Brief E's longitudinal/intraindividual directions
into `devel/longitudinal-ssm-spec.md` and escalated it to independent Fable
review (RB06 → RR06, 2026-07-16; archived under `cairn/reviews/archive/`).
Verdict: needs change (targeted), architecture confirmed; the spec was
revised per RR06 (§9 revision log — 12 applied, 3 rejections accepted).
**Decision:** Longitudinal build milestones implement the revised spec
without re-opening its reviewed decisions. Fable-attested load-bearing
holdings: (1) the paired occasion analysis rides the existing row resampler
(wide person-rows; case bootstrap = `boot::boot` over rows) and the stacked-
occasions MC covariance (sample covariance of stacked person vectors / n)
is the correct asymptotic object; (2) **the paired-efficiency claim is
conditional** — exact for Δe iff within-person elevation correlation > 0;
for Δa/Δd paired is narrower iff the gradient-projected cross-covariance
∇g₂ᵀC∇g₁ > 0 (∝ cos Δd under isotropic dependence), **reversing for
|Δd| > 90°** — docs may never print the unconditional claim; (3) growth
recipes must fit (x, y) **jointly** (independent univariate LMMs zero
Cov(x̂, ŷ) — wrong d(t) intervals), with a per-t D-007 amplitude
certification caution; (4) the draws adapter requires an explicit type in
the ncol = 3/no-angles cell; (5) cross-occasion column alignment is
stem-validated (the rotation channel); (6) listwise-only for occasions, on
estimand grounds; (7) `ssm_ci_accuracy()` errors informatively on occasions
objects.
**Re-trigger:** a build milestone may amend the spec only through its own
gate with a work-log line; challenges to reviewed holdings need a new RB.
**Consequences:** build candidates registered (spec §7 cut A/B/C; ROADMAP);
none merge-gated behind M7 (D-012). D-002/D-003/D-006/D-007 reinforced,
none superseded. Source: RR06 (Fable, 2026-07-16); M23 T3.

### D-014 (2026-07-16): tidyverse-style NSE stays out of the user API (M24)

**Context:** Pre-1.0 circumplex had rlang tidy-eval NSE; the v1.0.0
breaking release removed it ("streamline and reduce dependencies",
NEWS.md:412–416). M24 re-evaluated adoption on four evidence strata
(`devel/m24-nse-evaluation.md`).
**Decision:** The user-facing API remains standard evaluation — character
names / numeric indices via `is_var()` — with instrument helpers
(`PANO()`-family) as the ergonomic layer. **Full rejection**: bare-name
capture AND tidyselect-style select helpers, whether via a tidyselect
Import, bare-rlang `enquo()`, or an in-house parser (the datawizard route).
Grounds: (1) peer group — the modeling packages circumplex interoperates
with (lavaan engine, OpenMx oracle, psych) are SE/formula; tidy-eval NSE
marks tidyverse-identity packages (memo §1); (2) dependency delta — 6
net-new Imports incl. vctrs (refused by D-006); the closure's R floors are
neutral — circumplex's effective install floor is already 4.1 via
ggplot2/htmlTable (memo §2, corrected at review); (3) measured ergonomics — `PANO()` is already
shorter than the NSE form at the canonical call, and NSE's best case
(`starts_with()` for items) undermines `score()`'s ascending-order
contract, a silent mis-scoring channel (memo §3.1); (4) ambiguity spikes —
data-mask column/env collision silently selects wrong columns (a
statistical wrong-answer channel SE cannot produce) and user wrappers
require `{{ }}` (memo §3.2, runnable); (5) back-compat — a second
API-philosophy reversal for users who absorbed v1.0.0 (memo §4).
**Re-trigger:** reopen only on concrete evidence the SE interface fails
users — recurring user reports that name-vector/instrument helpers cannot
solve, or a hard interop requirement from a downstream tidyverse-native
consumer. Modernization or style advocacy alone never re-triggers. Any
reopening supersedes this entry and passes the `irreversible-api` RB gate
(Fable) before any build.
**Consequences:** DESIGN.md Dependency policy gains the one-line doctrine;
rlang stays imported solely for the ggplot2 `.data` pronoun; no NEWS entry
(no user-facing change). Confirms and evidences the v1.0.0 removal; D-006
reinforced. Source: M24 memo; plan-gate answers (Jeff, 2026-07-16).

### D-015 (2026-07-16): brms enters Suggests for the precomputed Bayesian vignette (M26)

**Context:** The D-013 spec (§5.3) finishes the longitudinal Build B with a
Bayesian SSM vignette; brms cannot run on CRAN builders, and the dependency
gate requires a D-entry for any dependency change. Pre-cleared at the M26
plan gate (2026-07-16).
**Decision:** brms is added to `Suggests` only. The vignette
(`vignettes/bayesian-ssm-analysis.Rmd`) is precomputed: its `brm()` chunk is
`eval = FALSE`, and the posterior draws it discusses ship as a committed
fixture (`vignettes/bayesian_ssm_draws.rds`) generated by the seeded
`data-raw/bayesian_ssm_draws.R` (provenance attribute embedded). brms is
never loaded by package code, tests, or vignette build.
**Consequences:** No Imports change; the minimal-deps doctrine (D-006/D-014
lineage) is untouched — a user without brms loses nothing but the ability to
re-run the frozen chunk. Regenerating the fixture requires a local Stan
toolchain.

### D-016 (2026-07-16): glmmTMB enters Suggests; the growth vignette's reference engine is glmmTMB, not nlme (M27)

**Context:** The D-013 spec (§4.1) named nlme for the growth vignette's
reference joint recipe (it ships with R). The RR06-reviewed *holding* is that
the model must be fit **jointly** on (x, y) — engine-agnostic. At the M27
plan gate (2026-07-16) Jeff chose glmmTMB: its `us(0 + dv | person)` syntax
expresses the correlated cross-outcome random-effects structure directly,
where nlme requires the error-prone `varIdent`/`corSymm` dummy-coding
contortion.
**Decision:** glmmTMB is added to `Suggests` only; the growth vignette's
reference joint recipe fits with glmmTMB (live conditional chunks — glmmTMB
is CRAN-hosted, no special toolchain), with nlme named in one line as the
base-R alternative. The package never Imports a mixed-model engine (spec
§4.1 minimal-deps holding unchanged); package code and tests stay
unconditional on glmmTMB. The spec is amended in place (§4.1/§4.2, marked
`[M27 amendment]`) per D-013's re-trigger clause — the joint-fitting holding
itself is untouched.
**Consequences:** CRAN builders need glmmTMB available to run the vignette
chunks (standard for Suggests; chunks are `eval`-guarded on availability).
The M27 coverage oracle simulates from and fits the glmmTMB family. A future
engine swap is a docs-level change gated the same way.

### D-017 (2026-07-17): the occasions `ssm_ci_accuracy()` plug-in population is the per-group observed stacked covariance — no CPM on the occasions path (M29)

**Context:** M29 removes `ssm_ci_accuracy()`'s occasions error guard and builds
an occasions-aware plug-in population. The construction was tagged `ip-touching`
("its own design", spec §1.4) and escalated to independent Fable review
(RB07 → RR07, 2026-07-17). Central risk: the classic diagnostic CPM-smooths a
single p×p circumplex correlation, but the occasions object's stacked k·p matrix
is not a single circumplex (only the k diagonal blocks are; the cross-occasion
blocks are arbitrary), and the diagnostic is most needed at the small n where a
raw k·p covariance is near-singular.
**Decision:** For occasions objects the diagnostic simulates persons from
`MVN(stacked occasion-profile means, per-group observed stacked k·p covariance)`
via the shared `mvn_root()`, at every n, with **no CPM anywhere on the occasions
path**. Fable-attested load-bearing findings: (1) everything the verdict is keyed
to (per-occasion parameters, the paired contrast, both replayed procedures)
depends on the k·p covariance only through a fixed 3k-dimensional harmonic
projection `A Σ A'`, whose estimation error is `O(sqrt(2/(n−1)))` regardless of
k·p (measured 28.7% at n=25) — so the "near-singular 16×16" objection targets the
wrong object; only the fit statistic escapes the projection. (2) Construction (b)
(CPM-diagonal + observed cross) is affirmatively broken: at n=25, ρ=0.6 the
reassembled matrix was non-PSD in 98% of replicates, median PSD-repair 0.020
(> the 0.01 realism bar), and the repair perturbs the cross-blocks under test
while destroying the CPM diagonals — an unvalidated hybrid; no coherent joint
Browne family exists (cross-occasion blocks are lagged auto-correlations the
single-circumplex family does not model). Shrinkage (c) attenuates the
cross-dependence toward independence (the very failure M29 guards) and is
unnecessary by (1). Both rejected. (3) Per-group `Σ̂_g`, not pooled: the diagnostic
replays the per-group MC engine; pooling had a Z&W/CPM-fidelity rationale that
does not transfer, and the projection argument removes the df motivation.
**Storage** (store-at-analysis-time was settled at the M29 gate; this fixes the
shape): the occasions object stores, per group, `n_g` + the stacked k·p mean +
the stacked k·p covariance (+ `occ_k`, occasion labels, stacked column names),
shape-tagged with `occ_k` so classic-path p×p consumers refuse it rather than
silently pool a k·p matrix. Not the raw person matrix (sufficiency); not a
correlation+SD decomposition (that existed only to interpose CPM). Rank
deficiency (`n_g ≤ k·p`) warns, never refuses (a singular Σ is a proper degenerate
MVN whose projected dependence rides through exactly); the fit-statistic caveat
is documented. An explicit `structure = "cpm"` or `cpm =` on an occasions object
is refused informatively (refuse-don't-coerce, M18 lesson); the default call runs
and records `details$structure = "observed"`.
**Consequences:** exported behavior change — `ssm_ci_accuracy()` errors → runs on
occasions objects (NEWS-documented at v2.0.0/M7). Assessing occasion-by-occasion
via `scales=` uses the CPM default and can give slightly different per-occasion
verdicts than the joint occasions run — a documented structure-sensitivity fact,
not a bug. The M29 acceptance bar is amended (see the M29 file): AC3 gains a
width-based discrimination arm + closed-form Δe width target (coverage alone is
provably blind to dependence-dropping — the discriminating observable is interval
width), and AC4's flat-occasion contract becomes informative refusal (not
"non-erroring"). D-013 reinforced (per-group stacked object; listwise-only), none
superseded. Source: RR07 (Fable, 2026-07-17); M29.

### D-018 (2026-07-17): the visualization expansion enters v2.0.0 scope; `ssm_plot_*` stay as convenience wrappers

**Context:** The "Plotting/visualization expansion on the `ggcircumplex()`
ggplot2 extension" ROADMAP candidate (2026-07-17) was promoted. At the plan gate
Jeff chose to (i) fold the whole expansion into v2.0.0 rather than ride ~v2.1.0,
and picked all four areas — the `CoordCircumplex`/carrier-scale architectural
rewrite, geom/layer ergonomics, longitudinal trajectory viz, and a plotting
vignette + pkgdown reorg; and (ii) asked what to do with the existing
`ssm_plot_circle/curve/contrast()`.
**Decision:**
**(a)** Supersede D-001's new-features-excluded clause **insofar as it bars the
visualization expansion** (narrowly, exactly as D-008 did for CIRCUM): the viz
overhaul enters v2.0.0 scope as M30 (coord-system design, Fable-reviewed) → M31
(coord-system build), M32 (geom/layer ergonomics), M33 (longitudinal trajectory
viz), and M34 (plotting vignette + pkgdown). This extends D-008/D-012's doctrine
— scope is the variable, the date yields to the statistics — there is no release
date pressure; v2.0.0 grows and ships when its bundle is complete and validated.
**Scope of the supersession:** narrow — promotes **only** the viz expansion; all
other D-001 consequences and exclusions stand (longitudinal deferrals stay
ROADMAP candidates; D-012 governs any late-merging build).
**(b)** `ssm_plot_circle/curve/contrast()` (and `ggcircumplex()`) are **retained
as thin convenience wrappers** over the improved composable coord/layers — no
deprecation, no breaking change. The two-tier API (one-liner wrappers + the
composable coord/geoms for power users) is the standing design; M31's back-compat
contract enforces it. The alternative (fold them into `plot()` S3 methods and
deprecate the standalone names) is rejected as a needless breaking change for no
user benefit.
**Consequences:** M7 gains `Depends on: M31, M32, M33, M34` — the v2.0.0
submission waits on the merged viz, and M7's check/win-builder surface covers
whatever merged (D-012 lineage). The coord rewrite is `irreversible-api` +
`ip-touching` (it re-owns the 0/360 polar transform), so M30 escalates to Fable
(RB→RR) and records its own GO/NO-GO D-entry before M31 builds; on a M30 NO-GO,
M31 is retired and only M32/M33/M34 remain (M7's dependency on M31 is then
dropped). D-001/D-008/D-012 lineage extended; D-006/D-014 (minimal-deps) untouched
— ggforce is already an Import (DESIGN.md V6). Source: Jeff, plan gate, 2026-07-17.

