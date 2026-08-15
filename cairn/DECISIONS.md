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

### D-019 (2026-07-17): GO on the `CoordRadial`-based coordinate system (M30); ggplot2 floor re-pinned to >= 4.0.0

**Context:** M30 turned the three DESIGN.md visualization known-limitations (no
owner for `amax`/the polar transform → silent point/ring misalignment; no
configurable center; theme-frozen canvas) into a build-ready design spec
(`devel/m30-coord-spec.md`) and escalated it to independent Fable review
(RB08 → RR08, 2026-07-17; archived under `cairn/reviews/archive/`). Tagged
`irreversible-api` (new exported coordinate system) + `ip-touching` (re-owns the
0/360 polar transform carrying the angle invariants). Verdict: **GO (Option A)**,
one blocking factual correction (the floor).
**Decision: GO.** Build `coord_circumplex()` in M31 as a `CoordRadial` subclass
per the spec's §11 authoritative punch-list. **Re-pin `DESCRIPTION` `ggplot2
(>= 3.3.0)` → `(>= 4.0.0)`** — the design's parameters (`thetalim`, `rlim`,
`reverse`, numeric `r.axis.inside`) are 4.0.0-only, verified by Fable against the
ggplot2 v3.5.2 source; the spec's original 3.5.0 claim was wrong. Fable-attested
load-bearing holdings M31 inherits: (1) the coord **hard-pins** `thetalim =
c(0,360)`, `expand = FALSE`, `start = pi/2`, `reverse = "theta"` internally
(range coord-side, never scale-limits which censor); (2) seam-straddling arcs
unwrap by **extension** (`xmax = xmin + span`, may exceed 360) in the arc geom's
`setup_data()` — one `GeomRect`, coord wraps periodically (I2); (3) the pole
(I3) draws identically for `d ∈ {0,360}` only under the `expand = FALSE` guard
(default `expand = TRUE` opens a 33° gap); (4) configurable center = `rlim =
c(center, amax)` alone, `inner.radius` decoupled and defaulting 0; (5) `amax`
and `geom_ssm_arc(n=)` become inert — unconditional soft-deprecation with a
sentinel default and a one-time note, **never** an error (that breaks the
package's own documented examples); (6) two hidden consumers the spec missed,
`plot.circumplex_cpm` and `plot.circumplex_fit_structure`, join the R4
keep-working set; the `repel = TRUE` branch needs a redesign (it hand-computes
canvas coordinates); (7) `ggforce` is likely fully removable but only via its
**own M31 D-entry superseding the DESIGN.md V6 KEEP holding**, after a
verification checklist — never a silent side effect.
**Dependency-gate record:** re-pinning ggplot2 up is a dependency change
(tracking-rules gate + D-entry); **user-approved 2026-07-17** (M30 T3 gate). It
adds **no new Import** (ggplot2 is already an Import); the effective install
floor is already R >= 4.1 (via ggplot2/htmlTable) → current ggplot2, so the
honestly-named excluded cohort is only environments *pinned* to pre-4.0 ggplot2
(e.g. an renv lock avoiding the S7 transition). The `DESCRIPTION` edit ships in
M31 with the code that needs it, not in docs-only M30.
**Rejected:** Option B (bespoke base-`Coord`, no floor bump) — higher total risk,
owns two churned coord/guide generations of internal API and re-derives the
annular tessellation (RR08 R-12); NO-GO (keep the drawn canvas) — the three
defects are structural and worth fixing. Supporting ggplot2 3.5.x *and* 4.0.x in
one subclass rejected (cross-version render drift in the invariant-carrying
layer).
**Re-trigger:** M31 may amend the spec only through its own gate with a work-log
line; challenges to the RR08 holdings need a new RB.
**Consequences:** M31 stays `planned` (not retired); D-018's M7→M31 dependency
stands. D-006/D-014 minimal-deps satisfied (re-pin, not a new dep). D-018
(wrappers retained) reinforced. Source: RR08 (Fable, 2026-07-17); Jeff, M30 T3
gate.

### D-020 (2026-07-17): ggforce dropped from Imports — the V6 KEEP holding is superseded (M31)

**Context:** DESIGN.md's V6 review held `ggforce: KEEP` — its
`StatArcBar`/`GeomArcBar` powered the CI-wedge geom and `geom_circle` drew the
canvas rings. M30/D-019 (RR08 R-8) flagged ggforce as "likely fully removable"
once the `coord_circumplex()` rewrite eliminated its call sites, but required a
verification checklist and its own D-entry — never a silent side effect.
**Decision:** Remove `ggforce` from `DESCRIPTION` Imports. M31 made the arc a
coord-bent `GeomRect` and the rings the coord's themed r-gridlines, so the
checklist (spec §11) is satisfied: (1) all three plot families
(`ssm_plot_circle`, `plot.circumplex_cpm`, `plot.circumplex_fit_structure`)
render off the coord with no `StatArcBar`/`geom_circle`; (2) the zero-width
wedge is re-owned by `GeomSsmArc$setup_data()` (T-arc0 regression); (3)
`grep -r ggforce` clean over `R/`, `tests/`, `vignettes/`, `NAMESPACE` (two
vignette `geom_circle` illustrations replaced with `annotate("path")`); (4)
`devtools::check()` clean (0 errors / 0 warnings; 1 transient top-level NOTE)
and the full suite passes with it dropped. The dead cartesian helpers
`ggrad()`/`ssm_to_cartesian()`/`ssm_radius()` are removed with it — the polar
transform now lives in exactly one place (the coord).
**Supersedes:** DESIGN.md V6's `ggforce: KEEP` (discharged, not a standing
rejection — KEEP was conditional on ggforce *simplifying* the geoms, which
Option A reversed). D-006/D-014 minimal-deps reinforced (one fewer Import).
**Consequences:** no user-facing behavior change (internal dependency); NEWS
notes the smaller dependency footprint at v2.0.0/M7. `ggplot2 (>= 4.0.0)`
(D-019) is the only remaining plotting Import. Source: M31 T7; RR08 R-8; Jeff,
M31 plan gate (Q2 "remove in M31").

### D-021 (2026-07-18): DESCRIPTION declares the true R floor, R (>= 4.1) (M7)

**Context:** `DESCRIPTION` has carried `Depends: R (>= 3.4)` since well before
the v2.0.0 bundle. Two later facts outran it. (1) D-015 added the precomputed
brms vignette's committed fixture `vignettes/bayesian_ssm_draws.rds`, a
version-3 serialized object; `R CMD build` therefore warns that the package
"now depends on R (>= 3.5.0)" and silently writes that floor into the built
tarball. (2) D-019 re-pinned `ggplot2 (>= 4.0.0)` and recorded in passing that
"the effective install floor is already R >= 4.1 (via ggplot2/htmlTable)" --
verified again at M7 T2: ggplot2 4.0.3 and htmlTable both declare
`Depends: R (>= 4.1)`. Neither fact was ever written back into `DESCRIPTION`,
so the declared floor understated the real one by two minor versions.
**Decision:** Declare `Depends: R (>= 4.1)`.
**Consequences:** No user who can install circumplex today is excluded -- the
4.1 floor is already enforced transitively by ggplot2 (>= 4.0.0), so this
records a constraint rather than adding one. The `R CMD build` serialization
warning is resolved as a side effect (4.1 > 3.5.0). The repo's declaration now
matches the analysis D-014 and D-019 already relied on. Shipped in v2.0.0 and
NEWS-documented alongside the ggplot2 re-pin; `cran-comments.md`'s dependency
note names it for the CRAN reviewer. Re-pinning an R floor is a dependency
change under the tracking-rules gate -- **user-approved 2026-07-18** at the M7
T2 gate. D-014/D-019 reinforced, neither superseded.


### D-022 (2026-07-19): `grid` joins Imports for grob-level label backdrops (M39)

**Context:** M39 draws a translucent plate behind each amplitude axis label so
the label stays readable where a data layer falls behind it. There is no theme
route to this: `element_text()` in ggplot2 4.0.3 has no `fill`, and ggplot2
ships no text-with-background element (checked at the M39 T1 gate). The plate
must therefore be a grob, which needs `unit()`, `gpar()`, `rectGrob()`,
`textGrob()`, `grobWidth()`/`grobHeight()`, `viewport()`, and `grobTree()` --
all in `grid`, which the package used nowhere before.
**Decision:** declare `grid` in `Imports` and call it as `grid::`.
**Consequences:** no user is affected and no install burden is added. `grid` is
a base R package shipping with every R installation, it cannot raise the R
floor, and ggplot2 -- already an Import -- depends on it, so it was loaded in
every session that used this package's plotting layer already. This records an
existing reality rather than adding a constraint, exactly as D-021 did for the
R floor. **No second dependency was taken for convenience:** the plate's fill is
written as the literal `#FFFFFFBF` rather than built with
`grDevices::adjustcolor()`, keeping `grDevices` out of `Imports` for what is
only a constant (D-006/D-014 minimal deps reinforced, neither superseded).
Adding a dependency is a question-gate item under the tracking rules --
**user-approved 2026-07-19** at the M39 T3 gate. This entry was written at
review: the gate was held but the D-entry was missed at the time, and review's
history lens caught the omission (M39 finding F1).

### D-023 (2026-07-20): a deliberately-shelved source is captured as a research prospect, not dismissed as "owes no page"

**Context:** The M41/M43 source-note sweeps dispositioned shelved PDFs the repo
does not cite as "owes no page" (the "consulted in passing owes nothing" rule;
`INDEX.md` ledger). At the M45/M46 plan gate Jeff added nagy2019, weide2021,
rogoza2021, and tracey2000 to the shelf **on purpose** — as candidate
references, oracles, or research material for future milestones — and rejected
the owes-no-page disposition as shortsighted: absence of a current citation is
not evidence a source is inert.
**Decision:** A shelf source no shipped code cites is dispositioned by *intent*,
not by citation count. One deliberately added as future research material is
captured as a **prospect** — a forward-looking brief (what it contains, what it
could seed, what a taking-up milestone must verify) plus a `candidate` ROADMAP
row — kept separate from the relied-upon reference pages and out of `INDEX.md`'s
committed-page list (it has no `Traces to`). The "owes no page" ledger is
retained only for sources genuinely consulted in passing (e.g. cited only as
other authors' citation of prior work), which stay listed with their reason.
**Consequences:** M46 authors the four prospect briefs and rows. Future source
sweeps must ask *why a PDF is on the shelf* before dispositioning it, never
dismiss on "nothing cites it." The full extraction + code-cross-reference bar
(the M45 relied-upon bar) is owed only when a milestone comes to rely on a
source. Refines the M41/M43 owes-no-page practice; no D-entry superseded.
Source: Jeff, M45/M46 plan gate.
_Mechanism superseded by D-024 (2026-07-20): the principle here stands, but the
per-source-brief-in-`prospects/` output is replaced by the supply-push doctrine's
survey synthesis note + candidate rows._

### D-024 (2026-07-20): forward-looking shelf sources are triaged into one survey synthesis note + candidate rows, not per-source prospect briefs (supersedes D-023's mechanism)

**Context:** D-023 (same M45/M46 plan gate) captured the four deliberately-shelved
sources (nagy2019, weide2021, rogoza2021, tracey2000) as prospects via a
per-source forward-looking *brief* in a new `cairn/references/prospects/` home,
held out of `INDEX.md`. The cairn supply-push exploration doctrine
(`tracking-rules.md` "Exploring prospective sources", shipped by cairn M103,
*after* D-023) governs exactly this activity and prescribes a different
mechanism: exploration adds no new write location, withholds per-source
`<citekey>.md` pages (they stay demand-pull), always emits `candidate` ROADMAP
rows, and commits at most one **survey synthesis note** when the triage outlives
the milestone.
**Decision:** D-023's *principle* stands — a deliberately-shelved source is
dispositioned by intent, not citation count, and captured rather than dismissed
as "owes no page." Its *mechanism* is replaced by the official supply-push
output: M46 authors one committed survey synthesis note
(`cairn/references/forward-source-prospects.md`, from `synthesis-note.md`,
INDEX-listed) triaging the four sources, plus four `candidate` rows. No
`prospects/` home; no per-source pages. A per-source `<citekey>.md` page is owed
only when a milestone comes to rely on a source (the M45 bar).
**Consequences:** The four sources gain one survey note and four candidate rows,
not four briefs; the note carries an `INDEX.md` line (reversing D-023's
out-of-INDEX instruction — correct for a synthesis note), and the stale
`INDEX.md` prospects comment is corrected. Supersedes D-023's mechanism; D-023
retained as historical record and cross-referenced. The owes-no-page ledger is
untouched (still valid for genuinely-in-passing sources).
Source: Jeff, M46 re-plan gate.

### D-025 (2026-07-23): the circumplex axes-reliability feature (Strack 2013) enters v2.0.0 scope as a design→build path

**Context:** The `strack2013` ROADMAP candidate (M48 supply-push capture) —
Strack, Jacobs & Grosse Holtforth (2013), *Reliability of Circumplex Axes* — is
a tau-equivalent CFA variance-decomposition model estimating circumplex **axes
reliability** (and SEm) from the axes variance component ξ1, contrasting with
`fit_structure()`'s RANDALL (sensitive to the axes/scale-specificity ratio;
Strack isolates both). At the M53 plan gate Jeff chose to (a) plan it
design-first, and (b) fold it into v2.0.0 rather than ride ~v2.1.0.
**Decision:** Supersede D-001's new-features-excluded clause **insofar as it
bars the axes-reliability feature** — narrowly, exactly as D-008 did for CIRCUM
and D-018 for the viz expansion. The feature enters v2.0.0 scope as **M53**
(design spec + Fable-reviewed GO/NO-GO, docs-only) → an axes-reliability
**build** (ROADMAP candidate, planned only post-GO). M53 may decide **NO-GO**,
in which case nothing axes-reliability ships in v2.0.0 and the build is
dropped/deferred. This extends D-008/D-012/D-018's doctrine — scope is the
variable, the date yields to the statistics; there is no release-date pressure,
v2.0.0 grows and ships when its bundle is complete and validated.
**Scope of the supersession:** narrow — promotes **only** the axes-reliability
feature; all other D-001 consequences and exclusions stand.
**Consequences:** M7 (v2.0.0) will gain `Depends on:` the build **at
build-planning time, on a GO** — not now, because the build has no ID until GO
(design-first). The build inherits M7's release linkage; M53 itself is
docs-only and does not gate M7. No new dependency: lavaan and OpenMx are already
`Suggests` (used by `ssm_sem` and its oracle), so the model reuses the existing
`lavaan::cfa` chokepoint — D-006/D-014 minimal-deps reinforced. The GO/NO-GO
verdict is a separate later D-entry (M53 T6, Fable-reviewed RB09→RR09), as
D-009/D-019 were for M18/M31 after D-008/D-018 admitted them. The `strack2013`
candidate row is promoted (M48 lineage). D-001/D-008/D-012/D-018 lineage
extended; none other superseded. Source: Jeff, M53 plan gate.

### D-026 (2026-07-23): GO on building the circumplex axes-reliability estimator (Strack 2013) in v2.0.0 (M53)

**Context:** D-025 admitted the axes-reliability feature to v2.0.0 as a
design→build path, M53 free to NO-GO. M53 designed the estimator
(`devel/m53-axes-reliability-spec.md`) and escalated to independent Fable review
(RB09→RR09, archived under `cairn/reviews/archive/`).
**Decision: GO.** Build `axes_reliability()` — an item-level restricted
tau-equivalent CFA reading circumplex axes reliability off the axes variance ξ1
(Spearman–Brown) — with the Layer-A (Table 3) + Layer-B (synthetic recovery +
cross-engine lavaan/OpenMx + deterministic population-matrix) oracles, per the
spec. Fable-attested load-bearing holdings: (1) **faithful** — the flat
implemented form is covariance-equivalent to Figure 2's hierarchical drawing
(every intermediate path fixed at +1 or the cosine); (2) **identified** — the
moment structure is linear in the components, a parameter-free rank condition
(rank 3 with ≥2 items/scale, collapsing to 2 at single-item scales;
`df = p(p+1)/2 − p − 3`), verified by exact population recovery; (3)
**`orthogonal = TRUE` is mandatory** (`lavaan::cfa` frees latent covariances by
default), and item errors stay **free** (constraining them equal changes df/fit
class — rejected); (4) the Layer-A Table-3 reliability/SEm oracle is a genuine
published-value oracle (four anchors reproduced independently), but the N–B
column (col 14) is **not** recomputable from printed values and needs its own
code-independent oracle; (5) analyzing a correlation matrix as covariance gives
correct point estimates but approximate SEs/χ² (Cudeck 1989; the paper's own
practice) — documented, with the lavaan `(N−1)/N` likelihood rescaling handled
in oracles; (6) weights route through `snap_trig` with pole tests (LM=360),
boundary fits (ξ̂1 ≤ 0) return NA reliability + warning (never clipped), and the
refuse contract uses a modular-angle check, ≥2 items/scale, N > p, and listwise
missing data.
**Binding:** RR09's **BC1–BC13** bind the **build** milestone verbatim (its
`Driving RR: RR09`), not the design milestone M53.
**Consequences:** the axes-reliability build candidate is cleared to plan; when
planned it sets `Driving RR: RR09`, ingests BC1–BC13 as acceptance criteria, and
M7 (v2.0.0) gains `Depends on:` it (D-025). Non-octant types (b–f),
quasi-circumplex weights, the secondary correlation-matrix input, and blockwise
ζ2 are deferred (build/candidate scope). D-025's design→build path is discharged
on its GO branch; D-006/D-014 minimal-deps reinforced (lavaan/OpenMx stay
`Suggests`, no new Import). Source: RR09 (Fable, 2026-07-23); M53 T6.

### D-027 (2026-07-24): ΔCFI enters `ssm_sem()` as a reported-only criterion, scope-gated to the envelope its source simulated (M57)

**Context:** the M5 SEM design left an open decision (§12.2 item 2): Δχ² was
the invariance verdict statistic, and alternative-index cutoffs would be
"offered only once transcribed". The Cheung & Rensvold (2002) transcription
landed 2026-07-07 but M5 T4 shipped without wiring it, leaving a candidate
row. M57 takes it up. Two things make the choice non-obvious: the article
states the direction of its own rule backwards, and the package's default
estimator (MLR) is outside the scope the article's simulation covers.
**Decision, three parts.** (1) **Reported, never gating.** The ladder table
carries `dcfi` = CFI(rung) − CFI(previous *fitted* rung); `comparable`, the
verdict string, and the fit the estimation layer consumes read the nested Δχ²
test and nothing else. Gating on ΔCFI was rejected: the two criteria answer
different questions and can legitimately disagree, and a package that gates on
whichever criterion is handier has no stable contract. (2) **The direction
comes from the simulation, not the printed sentence.** The p. 251 sentence says
ΔCFI ≤ −.01 means invariance "should not be rejected"; Table 5 (p. 248) shows
its critical values are the 1% *lower* tails of the simulated null
distributions (ΔCFI 1% entries −.0085 … −.0039, null means ≈ 0), so a value at
or below the cutoff is the 1%-level evidence *against* invariance. The repo
implements ΔCFI < −.01 → reject, with ≥ −.01 retaining, and cites
`cairn/references/cheung2002.md` for it rather than the sentence.
(3) **Scope-gated, and the gate keys on the statistic.** The retain/reject
label prints only for exactly two groups AND a plain normal-theory CFI (no
`cfi.robust`/`cfi.scaled` in `fitMeasures()`); elsewhere the value prints with
an explicit not-validated note and no verdict. Keying on the differenced
statistic rather than on `estimator == "ML"` ties the label to the quantity it
judges, so a robust index can never be labeled against a normal-theory cutoff.
**Rejected alternative:** extrapolating the cutoff to robust CFI or >2 groups.
Cheung & Rensvold simulated neither (p. 251: two groups, ML, multivariate
normal, Type I error only; robust variants postdate the study), so a cutoff
there would be invented, not extended. The package declines to flag rather
than fabricate a threshold — which is why the flag is OFF under `ssm_sem()`'s
own MLR default, an intended consequence and not a defect.
**Consequences:** an exported `print()` surface change inside unreleased
v2.0.0 (NEWS bullet, no deprecation cycle owed — the function is new in this
release). ΔGamma hat (−.001) and ΔMcDonald's NCI (−.02) stay transcribed in
`cheung2002.md` and unwired; a candidate row carries them. Any future
robust-CFI or multi-group flag needs new simulation evidence and a superseding
entry here.

### D-028 (2026-07-25): the ΔCFI scope gate requires ML estimation as well as a plain CFI — narrows D-027 part (3) (M57 review)

**Context:** D-027 part (3) set the ΔCFI scope gate to "exactly two groups AND
no `cfi.robust`/`cfi.scaled` in `fitMeasures()`", keying on the statistic
differenced rather than on the user's `estimator` argument. Its stated
rationale — "a robust index can never be labeled against a normal-theory
cutoff" — is sound but was **incomplete**: `GLS`, `WLS`, `ULS` and continuous
`DWLS` also return plain-named fit measures, so they passed the gate. The M57
review reproduced it end-to-end: a two-group `estimator = "GLS"` fit printed a
"retain" label with no not-validated caveat, contradicting AC2, the roxygen and
NEWS text, and `cairn/references/cheung2002.md`'s own binding scope block
("**ML estimation only**").
**Decision:** the gate is a **three-part conjunction** — exactly two groups AND
ML estimation AND a plain (non-robust) CFI. ML estimation is tested as
`identical(lavaan::lavInspect(fit, "options")$estimator, "ML")`, which reads the
**fit function**, not the argument: it returns `"ML"` for `ML`, `MLR` and `MLM`
(all ML estimation, differing in the test statistic and standard errors) and
`"GLS"`/`"ULS"`/`"DWLS"`/`"WLS"` otherwise. Verified against lavaan directly.
The two estimator clauses are both needed and neither implies the other: the
plain-CFI clause excludes MLR/MLM (ML fit, robust index), the ML clause excludes
GLS/WLS/ULS/DWLS (plain index, wrong fit function). D-027's admission of
`estimator = "ML", se = "robust.huber.white"` is **retained** — the fit function
is ML and the CFI is normal-theory, since CFI does not read the standard errors.
**What is superseded:** only D-027 part (3)'s two-part formulation. Parts (1)
reported-only and (2) direction-from-Table-5 stand unchanged, as does the whole
of D-027's refusal to invent a cutoff outside the simulated envelope — this
entry *strengthens* that refusal rather than qualifying it.
**Consequences:** `print()` now names the reason the verdict is withheld
("non-ML estimator: GLS", "robust CFI", "N groups", or a combination), so the
note is informative rather than merely negative. Roxygen, NEWS and the vignette
state the ML clause. `dcfi_scope` gains `estimator` and `ml` fields alongside
`cfi_plain`. Generalizable lesson: a scope gate keyed on a *proxy* for a
condition needs the condition's own test beside it — "the index is plain-named"
proxies for "the estimator is ML" and silently fails on the estimators nobody
thought to try.

### D-029 (2026-07-25): OpenMx and glmmTMB stay installed on the CI check job — the post-M52 install trim is declined on measured grounds (M58 plan gate)

**Context:** M51's scope note (2026-07-21) left a ROADMAP candidate holding the
remainder of the CI dependency-install trim after M52 took brms: dropping
OpenMx and/or glmmTMB from the `R-CMD-check` job. Its stated premise was that
"their `skip_on_cran` oracles never run under `R CMD check`, so excluding
OpenMx loses nothing there". Jeff declined it at the M52 plan gate; it was
recorded as deferred, not rejected. Re-investigated at the M58 plan gate.
**Decision:** both stay installed on both CI jobs. Declined outright, not
deferred again — the candidate row is closed.
**Why the premise no longer holds.** M54 landed
`tests/testthat/test-axes-reliability.R` on 2026-07-23, two days after the note
was written. That file contains **zero** `skip_on_cran()` calls, so its BC7
cross-engine oracle (line 292, "lavaan and OpenMx agree on the component
variances") is gated only by `skip_if_not_installed("OpenMx")` and does run
under `R CMD check` today. Removing OpenMx would convert it to a silent skip on
the check job — the exact "a step that doesn't run reports success" family
LESSONS records against M7, M31 and M38. The premise still holds for glmmTMB's
test (`test-growth_invariants.R:48` carries `skip_on_cran()`), but
`vignettes/growth-ssm-analysis.Rmd:113` gates its fitting chunks on
`has_glmmTMB`, so the check job would build that vignette with its model fits
unevaluated.
**Why the payoff does not justify it.** Per-step timings from the 2026-07-25
push runs (30162856789 / 30162856756 / 30162856759, all cache hits):
`setup-r-dependencies` is 0.9 min of a 14.5-min check job, 0.7 min of a
13.0-min coverage job, 0.9 min of a 4.3-min pkgdown job — under 7% of a run
before any trim. Removing two packages from an already-cached ~1-minute install
buys seconds. This is consistent with M52's own measurement (60s → 41s,
recorded there as "modest").
**Consequences:** the two workflow allowlists keep `any::OpenMx` and
`any::glmmTMB`; DESCRIPTION `Suggests` is untouched, so D-015 and D-016 stand.
Reopening needs a superseding entry and a materially different measurement —
a cold-cache regime, or a check job where the install is a real share of
runtime. The remaining install cost that *is* worth taking is pkgdown's
un-trimmed brms/Stan lockfile, which M58 addresses instead.

### D-030 (2026-07-25): the axes-reliability correlation-matrix input enters v2.0.0 — a narrow D-001 supersession that does not gate M7 (M59)

**Context:** M59 adds the `cormat` + `n` input path to `axes_reliability()`. At
its plan gate the milestone was scoped as post-v2.0.0, on the reading that this
needed no D-001 supersession. That reading was wrong about the mechanics: D-001
has work accumulate on master until the release train leaves, master is what
ships as v2.0.0 (`DESCRIPTION` already reads `Version: 2.0.0`, and `NEWS.md`'s
open section is `# circumplex 2.0.0`), and M59 merges to master well before the
release window M7 is blocked on. The feature therefore ships in v2.0.0 whether
or not a plan says otherwise; the honest choices were to record that or to hold
the finished branch unmerged until after the release.
**Decision:** Supersede D-001's new-features-excluded clause **insofar as it
bars the axes-reliability correlation-matrix input** — narrowly, exactly as
D-008 did for CIRCUM, D-018 for the visualization expansion, and D-025 for the
axes-reliability feature itself. The `cormat`/`n` path ships in v2.0.0 and its
NEWS text sits under the 2.0.0 heading, folded into the existing
`axes_reliability()` bullet rather than announced as a change to an unreleased
function.
**Scope of the supersession:** narrow — promotes **only** M59's
correlation-matrix input path. All other D-001 consequences and exclusions
stand, and the four extensions still parked on the ROADMAP's
"Axes-reliability deferred-in-spec extensions" candidate row (non-octant types
b–f, quasi-circumplex weights, blockwise ζ2 estimation, FIML on items) are
untouched — D-026's deferral of each, pending a concrete use case, is unchanged.
**Consequences:** M7 does **not** gain `Depends on: M59`. That is the part of
the plan-gate answer that stands unamended: the release never waits for M59, it
merely contains it if M59 lands first. Should the release window open before M59
merges, M59 ships in the following version and this entry is spent without
effect — it licenses inclusion, it does not require it. No new dependency
(lavaan is already `Suggests`; D-006/D-014 minimal-deps reinforced). M59's Scope
`Out:` clause is amended to match, with a work-log line. D-001/D-008/D-018/D-025
lineage extended; none other superseded. Source: Jeff, M59 implement gate.

### D-031 (2026-07-25): the axes-reliability non-octant and single-item extensions enter v2.0.0 — a narrow D-001 supersession that does not gate M7 (M60, M61)

**Context:** D-026 deferred four axes-reliability extensions ("Non-octant types
(b–f), quasi-circumplex weights, the secondary correlation-matrix input, and
blockwise ζ2 are deferred"), and D-030 left three of them parked, "pending a
concrete use case". At the M60/M61 plan gate Jeff took up two of the four. The
investigation found them much narrower than the deferral implied: Strack's type
b is eight equally spaced scales rotated 22.5° off the axes (p. 2), so the only
thing refusing it is the *phase* pinned by the `octants()` set-identity check at
`R/axes_reliability.R:495-506` — the weight and item_n math is already general.
Table 3 carries a **Type** column with printed %axes / item_n / reliability for
every type a–f, so each extension has a published-value oracle; all four type-b
rows and all six single-item rows reproduce by Spearman–Brown.
**Decision:** Supersede D-001's new-features-excluded clause **insofar as it
bars these two extensions** — narrowly, exactly as D-008 did for CIRCUM, D-018
for the visualization expansion, D-025 for the axes-reliability feature, and
D-030 for its correlation-matrix input. M60 (any equally spaced angle set, any
rotation, ≥ 2 items per scale) and M61 (single-item scale positions, ζ1 dropped)
ship in v2.0.0, their NEWS text folded into the existing `axes_reliability()`
bullet under the 2.0.0 heading. The accepted-input width is the plan-gate
answer: **any equally spaced set at any rotation**, because per-axis Σw² = k/2
holds at every rotation for k ≥ 3, so the equal-axis-variance restriction stays
as innocuous as it is for octants and pinning the phase would be arbitrary.
**Scope of the supersession:** narrow — promotes **only** M60 and M61. All other
D-001 consequences and exclusions stand. The two remaining parked extensions —
blockwise ζ2 estimation and FIML on items — are untouched, and D-026's deferral
of each, pending a concrete use case, is unchanged. RR09 §4's holding that
"Refusing unequal spacing in the MVP is therefore scope-correct, not merely
cautious" is likewise untouched: types b, c, e and f are all equally spaced, and
neither milestone goes near the quasi-circumplex refusal.
**Consequences:** M7 does **not** gain `Depends on: M60` or `M61` — on D-030's
reading, the release never waits for them, it merely contains them if they land
first; should the window open first, they ship in the following version and this
entry is spent without effect. No new dependency (lavaan and OpenMx are already
`Suggests`; D-006/D-014 minimal-deps reinforced). The ROADMAP's
"Axes-reliability deferred-in-spec extensions" candidate row is narrowed to its
two survivors. D-001/D-008/D-018/D-025/D-026/D-030 lineage extended; none other
superseded. Source: Jeff, M60/M61 plan gate.

### D-032 (2026-07-26): blockwise ζ2 enters v2.0.0 and takes up D-026's last-but-one deferral — a narrow D-001 supersession that does not gate M7 (M63)

**Context:** D-026 deferred four axes-reliability extensions pending a concrete
use case; D-030 and D-031 took up three of them (M59, M60, M61), leaving
blockwise ζ2 and FIML on items parked. At the M63 plan gate Jeff took up
blockwise ζ2. The use case is the caveat the package already ships: roxygen at
`R/axes_reliability.R:533-543` tells callers that a blockwise instrument folds
its block variance into the general and scale-specificity components, inflating
them and deflating the share attributed to the axes, and that Strack et al.
report block-specificity as high as 6.7% — so `axes_reliability()` currently
returns a knowably biased ξ1 for an entire class of instruments and says so.
Estimating ζ2 removes the caveat rather than documenting around it.
Investigation also corrected the candidate row's oracle premise: the row held
that the three blocked Table 3 rows plus the OCAI type-d rows "would be its
published oracle if memberships can be recovered", but recoverable memberships
were never the constraint — Strack et al. publish variance components, item_n
and reliability for those rows and no correlation matrix, so they cannot drive
an end-to-end fit at all (the conclusion `strack2013.md:160-170` already reached
for the type-f SYMLOG rows). Reliability is SB(ξ1, item_n) and never touches ζ2,
so those rows anchor the formula layer, which M63 does not change.
**Decision:** Supersede D-001's new-features-excluded clause **insofar as it
bars blockwise ζ2 estimation** — narrowly, exactly as D-008 did for CIRCUM,
D-018 for the visualization expansion, D-025 for the axes-reliability feature,
D-030 for its correlation-matrix input, and D-031 for the non-octant and
single-item extensions. M63 ships in v2.0.0, its NEWS text folded into the
existing `axes_reliability()` bullet under the 2.0.0 heading. Three gate answers
fix the design: block membership reaches the estimator through a **`blocks =`
argument only** (no bundled instrument records block structure, so an
`Instrument` field would ship empty on every one of them with nothing to
populate it from); an **unidentified ζ2 is dropped and flagged**, not refused,
mirroring the `axes_fits_zeta1()` contract M61 established so the emitted syntax
and the reported component set can never disagree; and the **acceptance bar is
synthetic** — known-ζ2 recovery, a demonstration that omitting ζ2 biases ξ1, and
lavaan/OpenMx/OLS-shadow agreement — with the blocked Table 3 rows added as
formula-layer Spearman–Brown and five-component sum anchors only.
**Scope of the supersession:** narrow — promotes **only** M63. All other D-001
consequences and exclusions stand. **FIML on items is untouched**, and D-026's
deferral of it, pending a concrete use case, is unchanged — it is now the last
survivor of the four. RR09 §4's holding that refusing unequal spacing is
"scope-correct, not merely cautious" is likewise untouched: blocks are a
grouping of items, not a respacing of scales, and M63 goes nowhere near the
quasi-circumplex refusal.
**Consequences:** M7 does **not** gain `Depends on: M63` — on D-030's reading
the release never waits for it, it merely contains it if it lands first; should
the window open first, M63 ships in the following version and this entry is
spent without effect. No new dependency (lavaan and OpenMx are already
`Suggests`; D-006/D-014 minimal-deps reinforced). Carrying blocks on the
`circumplex_instrument` class is deferred to the ROADMAP candidate row, not
rejected. D-001/D-008/D-018/D-025/D-026/D-030/D-031 lineage extended; none other
superseded. Source: Jeff, M63 plan gate.

### D-033 (2026-07-26): GO on FIML item-level missing data for `axes_reliability()`, on the FIML correlation metric — takes up D-026's last deferral, a narrow D-001 supersession that does not gate M7 (M64)

**Context:** D-026 deferred four axes-reliability extensions pending a concrete
use case; D-030, D-031 and D-032 took up three, leaving FIML on items as the
last survivor, whose deferral D-032 reaffirmed verbatim. The use case is
measured rather than asserted. Listwise deletion at **item** level retains a
respondent with probability (1 − rate)^p, so a realistic 64-item instrument
keeps 53% of respondents at 1% per-item MCAR and 3.8% at 5%; and at 15% on 24
items the shipped function does not degrade but **refuses** — "Complete-case N
(12) must exceed the number of items (24)" on 600 respondents
(`devel/m64-fiml-probe.R`, findings F1/F1b). The vignette currently advises
around this: "address the missingness before interpreting the estimate"
(`vignettes/axes-reliability.Rmd:156`). At the M64 plan gate Jeff chose
escalation over a direct build, so the design went to independent Fable review
as RB12 → RR12.
**Decision: GO.** Supersede D-001's new-features-excluded clause **insofar as it
bars FIML item-level missing data** — narrowly, exactly as D-008 did for CIRCUM,
D-018 for the visualization expansion, D-025 for the axes-reliability feature,
D-030 for its correlation-matrix input, D-031 for the non-octant and single-item
extensions, and D-032 for blockwise ζ2. `axes_reliability()` gains
`missing = c("listwise", "fiml")`, matching the sibling exported `ssm_sem()`
spelling, with `"listwise"` the default and its numbers bit-identical to shipped.
**The metric holding is load-bearing, and it overturns the position M64 put to
review.** Available-case `scale()` standardization is MCAR-honest but
**MAR-dishonest**: the standardized columns carry k_i·k_j·ρ_ij and the model has
no free per-item parameter *off* the diagonal to absorb an item-specific
multiplicative distortion, so it lands in the components. Measured **+0.0167**
above the FIML-metric estimate under a harsh same-scale-anchor MAR mechanism
(paired SE 0.0006 — about one full SE at N = 600), while the two metric-correct
routes agree to +0.0008. So the build standardizes by **saturated-FIML (EM)
moments** with a `sqrt(N_used/(N_used − 1))` convention that reproduces
`scale()` exactly on complete data, feeds a single **one-stage** structured FIML
fit, and retargets the OLS shadow and the positive-definiteness refusal to the
FIML correlation matrix R̂. Two-stage SEs and χ² never surface (`sample.nobs =
N_total` overstates information and no scalar effective N repairs it). Rejected
with reasons: the per-item unit-total-variance constraint (raises df and leaves
the paper's free-errors class — D-026's equal-errors rejection generalizes),
post-hoc component rescaling (does not touch the off-diagonal distortion), and
any scalar effective-N repair. A mandatory refusal is added for an item pair
never jointly observed: lavaan fabricates that moment (r = 0 against a
population 0.3475) and `axes_reliability()` fits inside `suppressWarnings()`, so
it is silent in this function.
**Binding:** RR12's **BC1–BC16** bind the **build** milestone verbatim (its
`Driving RR: RR12`), not M64, which ships no code. RR09 **BC13 is upheld, not
superseded** — R̂ is a saturated ML estimate, not a pairwise-deletion matrix.
RR09 §4's quasi-circumplex refusal is untouched.
**Scope of the supersession:** narrow — promotes **only** FIML item-level
missing data. All other D-001 consequences and exclusions stand. D-026's
deferral list is now empty; the one remaining axes-reliability candidate (block
membership on the `circumplex_instrument` class) was never part of it.
**Consequences:** the build is cleared to plan and sets `Driving RR: RR12`. M7
does **not** gain `Depends on:` it — on D-030's reading the release never waits
for it and merely contains it if it lands first. No new dependency (lavaan and
OpenMx are already `Suggests`; D-006/D-014 minimal-deps reinforced). Two
follow-ons are ROADMAP candidates rather than scope: planned-missingness designs
(three-form and similar), which the zero-joint-coverage refusal excludes even
though the structured model stays identified there, and lavaan's
`missing = "two.stage"` as a future SE-corrected alternative. The complete-data
implied-diagonal departure RB12 flagged is closed as expected restricted-ML
behavior rather than a defect, verified at the stationarity condition (M64-D3) —
no correction milestone. D-001/D-008/D-018/D-025/D-026/D-030/D-031/D-032 lineage
extended; none other superseded. Source: RR12 (Fable, 2026-07-26); M64 T4.

### D-034 (2026-07-26): three corrections to D-033's record of the FIML GO — annotates D-033, changes no decision (M64 review)

**Context:** M64's own review found three factual defects inside D-033.
`DECISIONS.md` is append-only history (IP4), so the entry is annotated here
rather than edited. **No decision changes:** D-033's GO, its narrow D-001
supersession, its takeup of D-026's FIML deferral, and BC1–BC16 binding the
build all stand exactly as recorded.
**Correction 1 — the reversal that did not happen (review F3).** D-033 says the
metric holding "overturns the position M64 put to review", and M64-D1 says it
overturns "the standardization the plan submitted with it". Neither is right.
M64's Scope names available-case z-standardization as "the one question this
session cannot settle" — an open question, not a position — and RB12's Q1 asks
it neutrally. The three positions M64 **did** fix were each **confirmed**:
one-stage FIML through `sem_fit_cfa` (RR12 §4), N–B and `sd = "raw"` reported
unavailable (§6), and a synthetic bar carrying a non-MCAR cell (§8, which
augmented the bar rather than overturning it). The accurate statement is that
RR12 **answered M64's open question** and, in answering it, ruled out the
mechanism the shipped code path happens to use. Identical consequence for the
build; a materially different answer to "how much of M64's own judgment survived
review", which is what a later session would come here to ask.
**Correction 2 — `sd = "raw"` is a hard error, not an NA (review F4).** D-033's
summary and the first ROADMAP build row lumped `sd = "raw"` together with
`nb_reliability` as "unavailable-with-reason". RR12 §6's Ruling and **BC9** set
two *different* contracts: `nb_reliability` becomes NA with `nb_reason` gaining
`"fiml"`, while `sd = "raw"` must be **refused with an informative error**
naming `"std"` and numeric SDs as the alternatives. BC9's verbatim text governs
the build; the ROADMAP row is corrected in place (current knowledge).
**Correction 3 — line anchor (review F5).** The vignette sentence "address the
missingness before interpreting the estimate" is at
`vignettes/axes-reliability.Rmd:157`; D-033 cites `:156`, which ends
"…discards a large share of". BC16 requires rewriting that paragraph, so the
anchor is load-bearing. The M43/M57 off-by-one anchor family, recurring.
**Also recorded, not a correction to D-033 (review F1, F8).** RB12 asserts the
committed probe "reproduces every figure quoted in this brief" and quotes
"|mean| ≤ 6e-17, |SD − 1| ≤ 9e-16" for available-case `scale()`. The check was
missing from the script and has been added (its new F5 section), but the quoted
**mean** bound is seed-specific and does not reproduce: the committed script
measures 7.76e-17 / 7.62e-17 / 7.9e-17 at 2/5/10% per-item MCAR. The SD bound
does reproduce (6.66e-16 to 8.88e-16). Both are machine precision and nothing
substantive turns on it — the claim RB12 rests on that figure, that `scale()`
standardizes exactly for the available cases, holds. It is the M59/M61 lesson
("a tolerance calibrated on one run is not a tolerance") recurring for the
second time inside this milestone, the first being AC1's own amended bound.
RB12's pasted transcript also omits one `message()` line the script emits
(F8). IP4 leaves both in place. Source: M64 review, findings F1/F3/F4/F5/F8.

### D-035 (2026-07-27): `axes_reliability()`'s component SEs will be corrected, not caveated — supersedes D-026 holding (5) and RR09 §2 (M65, RR13)

**Context:** D-026 holding (5), on RR09 §2's authority, ruled the
correlation-as-covariance issue **"document, don't fix"**: "analyzing a
correlation matrix as covariance gives correct point estimates but approximate
SEs/χ² (Cudeck 1989; the paper's own practice) — documented". RR09's grounds
were faithfulness to the source paper's own LISREL practice, and the SEs being
"approximate". **Neither RR09 nor D-026 ever measured the magnitude or the
direction of that approximation.** M65 measured it: the mean reported SE(ξ1) is
1.452× the estimator's empirical sampling SD over 200 replicates — on the new
FIML path and the shipped listwise path alike, to three decimals. Escalated as
RB13→RR13 (Fable, 2026-07-27).
**Decision: correct the SEs.** RR13 derived the ratio analytically rather than
simulating it — Σ is linear in the components, so the delta method gives both
sides in closed form — predicting 1.4412 against the measured 1.452 (0.2 MC
SEs), with the naive quantity reproducing lavaan's information-matrix value to
6 decimals. So the number is the exact textbook consequence for this design and
**not a defect** in the model, the extraction, or lavaan. What overturns
"document, don't fix" is not error but **size and sign**: across the accepted
input space the ratio runs [0.81, 1.97], and at Strack's own Table 3
configurations it spans 0.989 (weak-axes/strong-general instruments, reported
SEs slightly too *small*) to 1.300 (strong-axes instruments). An approximation
that is sign-unstable cannot be stated honestly by any static caveat — which is
the ground RR09 could not have weighed, because the sweep did not exist.
**Route:** the Browne/Cudeck corrected asymptotic covariance specialized to
this linear structure, ~40 lines of base R, no new dependency, identical code
on the raw, `cormat`, and FIML paths; validated at 1.005 (complete data) and
1.001/1.008/1.018 (FIML at 2/5/10% MCAR) against M65's committed fixture.
Measured rejections: lavaan `correlation = TRUE` (a different model class —
npar 3, determined errors, moves ξ̂1 by ≈5 empirical SDs, refuses missing data),
robust/sandwich SEs (measured no fix — blind to the in-sample standardization),
and unit-variance refitting (RR12 §9 stands).
**Consequences:** the correction takes **its own milestone**, bound by RR13
BC1–BC6, not M65 — it changes the shipped listwise and `cormat` paths, which is
strictly larger than M65's FIML scope, and M65's FIML path adds none of the
miscalibration. **M65 ships first** under RR13 BC7, recording the departure from
RR12 BC13 in its Deviations table with both printed caveats strengthened to
quantify. No deprecation cycle: the SEs were always documented as approximate,
point estimates/reliability/SEm/df are unchanged, and the maintainer's pre-1.0
waiver covers the formality. The global χ²/fit indices carry the same
approximation in the other direction (E[T] = 261.1 against df = 273, flattered
~4%) and keep their caveat. `ssm_sem()` is not implicated — it lives on the
covariance metric. Source: RR13 (Fable, 2026-07-27); derivation independently
re-run at ingestion and reproduced exactly.

### D-036 (2026-08-02): `axes_reliability()`'s global test statistic will be scaled, not caveated — supersedes D-035's fit-caveat holding (M68)

**Context:** D-035 corrected the component SEs and explicitly held the other
side: "The global χ²/fit indices carry the same approximation in the other
direction (E[T] = 261.1 against df = 273, flattered ~4%) and keep their
caveat." RR13 §3 set the same boundary — "a scaled test statistic from the same
Γ machinery is possible later but is not part of this recommendation" — and
B-1 filed it as a low-priority future milestone. Neither was a rejection; both
were scope boundaries on M66, whose Γ machinery now exists
(`R/axes_corrected_se.R`).
**Decision: scale the statistic.** `$fit$chisq`, `$pvalue`, `$rmsea` and `$cfi`
become Satorra–Bentler-type scaled values, `T_s = T / c` with
`c = tr(U Γ_R)/df` at the fitted Σ̂ and CFI additionally using the independence
model's factor `c_b`. What overturns "keep their caveat" is not new error but
the same argument D-035 made for the SEs, applied to the statistic M66 left
alone: a caveat that quantifies one number ("flattered by roughly 4%") is a
population-specific figure presented as a constant, and the machinery that
would replace it with the actual per-fit factor is now shipped and validated.
**Scope:** all three input paths, because a path-dependent `$fit$chisq` is the
failure the M65 SRMR fix cured (`R/axes_reliability.R:1635-1653`). `$fit$df`
and `$fit$srmr` are unchanged — SRMR is a residual summary, not a test
statistic. `ssm_sem()` remains unimplicated, on D-035's grounds.
**Release:** enters v2.0.0, a narrow D-001 supersession that does not gate M7,
following D-030/D-031/D-032/D-033. Shipping in the same release as M66 means no
released version ever carries the 4% caveat.
**Not decided here:** which `Γ_R` the FIML path uses — the complete-data form
at Σ̂ or RR13 §4's saturated observed-information acov delta-transformed. M68 T4
carries it as an (RB tripwire: no-oracle) open question, and AC4's simulation is
its only oracle. Source: RR13 B-1 (Fable, 2026-07-27); M68 plan gate.

### D-037 (2026-08-03): `axes_reliability()`'s FIML metric ratio is evaluated at `cov2cor(Σ̂)` — supersedes RR13 BC4's "evaluated at Σ̂" (M69, RR15)

**Context:** M66 shipped `axes_corrected_se()` pricing both its `naive` and
`corrected` branches at lavaan's raw `fitted(fit)$cov`, whose diagonal is
`(N−1)/N` rather than 1 (lavaan's `sample.cov.rescale`). M68-D2 had independently
chosen `cov2cor(Σ̂)` for the sibling scaled-fit surface. RR15 established that the
corrected branch's Jacobian fold compresses to `Σ_ij = ρ_ij` only at a unit
diagonal, so the raw evaluation is not the derived formula at any scale —
measured by non-homogeneity: scaling Σ̂ by 2 scales the corrected SEs by
1.538/2.009/2.114 where a coherent variance-metric quantity gives exactly 2.
RR13's own reproduction appendix derives both branches at the unit-diagonal
population matrix, so the shipped raw pricing was plug-in drift from RR13's
derivation, not a choice RR13 made.

**Decision:** The FIML path's corrected SE is the observed-information SE
multiplied by the per-parameter ratio of correlation-metric SE to normal-theory
SE, **both evaluated at `cov2cor(Σ̂)`** — superseding RR13 BC4's operative phrase
"the same per-parameter ratio evaluated at Σ̂", which no longer describes the
computation. The ratio is returned by `axes_corrected_se()` as `fiml_ratio`
rather than composed at the call site, so the same-matrix invariant is a property
of the helper. `naive` stays priced at the raw Σ̂: it is the only independent tie
of the derivative set to lavaan's own implementation, fenced at 1e-7.

**Consequences:** Reported component SEs move by ~0.1–0.2% at n = 600, growing
like 1/N — a coherence fix, not a material recalibration (RR15 B4). BC4's
missing-information rationale survives untouched: that pricing lives entirely in
the `se_uncorrected` factor. Same-matrix pricing *restores* agreement with RR13's
published constant 1.441229 (1/0.6938522 = 1.44124, against the shipped both-raw
1.44034). M68-D2 is affirmed on the same ground rather than merely left standing.
A mixed-matrix ratio was rejected: it inflates the reported FIML SE by N/(N−1)
(measured 1.0016694), is un-pinnable on the FIML path where the fitted diagonal
ranges 0.943–1.072, and would regress against the shipped both-raw ratio's
6.2e-4 fidelity. `sample.cov.rescale = FALSE` as a root-cause fix is refused
here (RR15 rec 8): it changes every shipped point estimate and would not obviate
`cov2cor()`, since the fitted diagonal departs from 1 under misspecification
regardless. Source: RR15 (Fable, 2026-08-03); M69 ingest gate.

### D-038 (2026-08-04): the design interview's principle set is adopted — six inviolable, seven guiding (IP1–IP6, GP1–GP7)

**Context:** The cairn migration deferred the IP/GP formalization of the
statistical invariants to `/design-interview` (this file's preamble). The
interview ran 2026-08-04 on Fable: Phase 1 elicited Purpose & scope (committed
42b4a36e) and banked twelve proto-principles; Phase 2 classified them plus
history-mined candidates (re-trigger discipline, fail-closed edges) and
domain-derived ones (norms provenance at correctness strength).
**Decision:** Adopt the set recorded in DESIGN.md "Design Principles":
inviolable — IP1 correctness outranks all; IP2 angle conventions (degrees
[0,360), LM=360, contrast second−first in (−180,180]); IP3 ≥2 independent
oracle types per shipped numeric result; IP4 the RNG contract; IP5
published-source provenance for shipped instrument data (binds forward; the
existing roster's debt is the norms-audit candidate row); IP6 boundary tests
for estimation changes. Guiding — GP1 circumplex constructs only; GP2 compute
well-defined/caution loudly/fail closed; GP3 minimal deps + SE-only API; GP4
post-2.0 API stability with deprecation cycle; GP5 teach the field with
statistically precise prose (venue tradeable — a future ebook may absorb the
didactic vignettes; precision is not tradeable); GP6 scope is the variable;
GP7 evidence reopens decisions. Classification choices made deliberately at
the gates: the contract boundary, guardrail stance, dependency posture, and
API posture were each offered at inviolable strength and set to guiding
(judgment-requiring lines); boundary tests and data provenance were elevated
to inviolable.
**Consequences:** Anything `ip-touching` now has a numbered referent for RB
tripwires. CLAUDE.md's invariants section stands as the operational summary of
IP2/IP6; DESIGN.md owns the principles. Four wart candidate rows (norms audit,
repel redesign, boundary-interval improvement, Heywood guidance) and the
vignettes→ebook row carry the interview's deferred work. Prior re-trigger
clauses (D-006, D-011, D-014) are instances of GP7, not superseded. Source:
Jeff, design interview gates, 2026-08-04.

### D-039 (2026-08-06): csiv's and csie's printed norms provenance is corrected under IP5, and the change is a factual correction rather than a GP4 break (M72)

**Context:** M72 audited the shipped norms of five instruments and found no
wrong number, but two wrong provenance records: csiv credited its norms to
Locke (2000), whose article publishes no octant statistics and reports a
different sample, and both csie's and csiv's `URL` pointed at retired
`webpages.uidaho.edu` paths that now resolve to a site homepage rather than the
cited norms table. Both fields print in `norms()` output, and GP4 makes printed
output a commitment whose break needs "statistical cause (a wrong number) or a
gated irreversible-api decision". The milestone changed them on IP5's
authority — published-source provenance for shipped instrument data — without
recording how the two principles compose, which the review's blame-history lens
surfaced as a traceability gap.
**Decision:** The change stands as made, and GP4's "statistical cause" clause is
read to cover it: a provenance field naming a source that does not publish the
values is wrong in the same sense a wrong number is wrong, and correcting it
restores the documented behavior rather than changing it. No deprecation cycle
is owed for a factual correction to a citation. The `Reference` and `URL` fields
joined the audited field set at the same gate, so a future drift here surfaces
in the comparison ledger rather than only in the regression pins.
**Consequences:** Later batches of the norms audit may correct provenance the
same way without a fresh gate; a change to a *numeric* norm value is a different
matter and does not inherit this. Users see the change in NEWS.md and in
`?csiv` / `?csie`, whose `@source` blocks now separate the instrument's article
from the norms table. Any future GP4 question about printed output that is not a
correction — a format change, a dropped field — still needs its own gate.

### D-040 (2026-08-08): a norm sample outside its instrument's response range is refused, not caveated (hotfix, PR #103)

**Context:** `norm_standardize()` computed z-scores from whichever normative
sample the caller named, never checking that the sample's moments were on the
same metric as the scores being standardized. One shipped sample is not: the
CAIS adult sample's PA, LM and NO means (5.19, 6.52, 6.14) exceed the CAIS's
own 1–5 range as declared in `cais$Anchors`, so standardizing against it
returned numbers in an undefined unit with nothing in the output to say so.
The M72–M75 provenance audit could not have caught it: that audit compares
shipped values against the published source, and here the two agree exactly —
the discrepancy originates in sodano2006's own Table 4. Three dispositions were
weighed at the gate: refuse, warn-and-compute, or rescale the sample onto the
1–5 metric.

**Decision:** Refuse. GP2's fail-closed clause governs — "undecidable edge
cases fail closed (not certified, not computed) rather than guessing" — and the
condition for it is met in the strong form: there is no metric under which the
returned values are correct, so refusing blocks no defensible analysis, which
is the interest GP2's "never block a defensible analysis" clause protects.
Warn-and-compute was rejected because a warning does not make a wrong number
right and the caller still receives it. Rescaling was rejected as premature: it
acts on an inference about what the source meant, and a numeric change to
shipped norms is exactly what D-039 carves out as *not* inheriting its
provenance-correction licence. **This is a behavior change rather than a pure
restoration** — callers who previously received numbers now receive an error —
and it is recorded here for that reason. GP4 does not bind: the package is at
1.3.0 and v2.0.0 has not shipped, which is what makes now the cheap moment.

**Consequences:** The refusal is roster-wide, not cais-specific: any shipped
sample whose means leave its instrument's anchor range is refused, and a
roster-wide invariant test pins the violation set exactly, so both a new
violation and the disappearance of this one fail the suite. The evidence that
Table 4's M and SD rows are transposed with its IAS block, what was ruled out
(sum scores; a published erratum), and the author query sent to Sodano and
Tracey on 2026-08-08 are in `cairn/references/sodano2006.md`. **The class of
evidence that reopens this:** a reply identifying the adult sample's metric, or
a second source printing those descriptives. Either would turn the refusal into
a repair — correcting the shipped values under a fresh gate per D-039's
numeric-change carve-out — or into a withdrawal of the sample. Users see the
change in NEWS.md and `?cais`. A future sample that is merely *unrepresentative*
rather than off-metric is a different question and does not inherit this;
that is the norms-fitness candidate row's territory.

### D-041 (2026-08-08): the reference-statistics vocabulary keeps its names — `norm_standardize()`, `norms()`, `$Norms`, `Population` (M76, M77, RR16)

**Context:** The M72–M75 provenance audit established what the 24 shipped
reference samples actually are: 11 carry a college- or undergraduate-student
label, 7 have n < 300, and only the 6 IIP samples are drawn to represent a
defined population. M74 then measured that the *choice* of reference sample
moves scores 0.44 SD on average and 0.78 at the extreme, against ~0.12 SD from
reference-moment sampling error at the worst shipped size. M76 and M77 were
planned to close that with a call-site disclosure and prose corrections, both
gates provisionally keeping the `norm*` vocabulary. The maintainer then raised
the `irreversible-api` tripwire: GP4 binds API stability only after v2.0.0,
which D-040 already relied on, so the rename's cost is at its lowest now and
expensive later. RB16 escalated the question; RR16 answered it.

**Decision:** Keep all four surfaces. "Norms" is the interpersonal-circumplex
field's own word for exactly these tables, convenience-sample ones included —
the instrument authors title their own single-study tables "norms", and the
*Standards* tradition qualifies the word ("local norms", "convenience norms")
rather than replacing it — so the identifiers report field usage rather than
assert representativeness. The hazard is real but runs through *claims* — the
definite article, silence about which sample was used, a header asked to carry
both who the sample was and what it represents — which M76's disclosure and
M77's prose address at the layer where they live. A rename carries at most one
bit, visible only where a call is written, and would understate the 6 genuine
standardization samples in the same motion that it stops overstating the other
18. `Population` was the one surface where the word does claim-work and was the
closest call; it stays, its residual hazard closed by M77's `?norms` hedge and
by the per-sample kind field below. A deprecation-cycle rename was rejected
too: mechanics mitigate cost, not pointlessness.

**Consequences:** M76's message prints the stored `Population` value as a plain
description, framed by neither "population" nor representativeness wording
(M76 AC8); a regression pin fails if any of the three surfaces is renamed or
dropped (M76 AC9). The distinction a rename would have flattened ships instead
as a machine-readable per-sample reference-kind column — standardization sample
/ identified published source / no identified source — printed by `norms()` and
carried by the disclosure, planned as its own milestone before the v2.0.0
submission while `Norms[[2]]` is cheap to touch. The ROADMAP's parked rename
item is closed on the merits, not deferred. **The class of evidence that
reopens this:** three or more documented instances, after a release carrying
the disclosure, of a user describing convenience-referenced z-scores as
locating respondents in a general population *with the disclosure in effect*;
or a field-level vocabulary shift deprecating "norms" for non-representative
reference tables. Explicitly insufficient: a reverse-dependency scan showing
few external callers (that lowers the cost, and the verdict rests on the
benefit), or modernization preference.

---

### D-042 (2026-08-15): the norms-audit abort apparatus is retired for a manifest check (M87)

**Context:** M81–M83 built, around the developer script `data-raw/audit-norms.R`,
an abort-site registry with per-site message matchers, build-time
discriminating-power floors, a cross-discrimination matrix and a denied-spelling
sweep — roughly 1500 lines of test machinery over a 1262-line script that is
`.Rbuildignore`d and ships to nobody; 77 of the suite's 90 blocks skipped under
`R CMD check`. Eight consecutive milestones (M79–M86) extended that script or
its machinery, and the two findings still open against it were both defects
*in the machinery*, latent, reachable only by adding a site shaped to trigger
them.

**Decision:** Delete the registry, the matchers and their build-time floors, the
acceptance matrix, the stack-capture machinery and the denylist. In their place
`tests/testthat/helper-norms-audit-manifest.R` carries a generated manifest of
the script's `stop()` calls and `stopifnot()` conditions, keyed
(kind, binding, key, ordinal); one test asserts set equality between it and a
fresh parse walk of the script; and `expect_audit_abort(expr, key)` resolves a
key to exactly one manifest site and checks the raised message against it under
that site's kind. The property kept is the one per-test regexps structurally
cannot provide: a regexp is quantified over the tests, so a guard added to the
script with no test at all is invisible to it, while the manifest is quantified
over the script. The branch removes several times what it adds; the figures are
whatever `git diff --numstat` reports for the squash-merge commit, and are not
restated here — a hand-copied count of a moving branch was this entry's own
first defect, caught at review after further commits had already stranded it.

**Consequences:** Three things are given up, named because nothing that replaces
them covers them. (1) The denylist's whole job: abort spellings other than
`stop()`/`stopifnot()` — `rlang::abort`, `cli_abort`, `do.call("stop", …)`, an
aliased or field-held handler — are no longer swept for at all, and the bound
M83 recorded, that a denied name passed as a value through a field access is not
seen, survives only as this sentence. (2) The acceptance matrix's build-time
sweep of every matcher against every site's message; `expect_audit_abort()`
folds the cross-site property in by requiring a raised message to be rendered by
exactly one manifest key, but only for sites some test exercises. (3) The guard
is opt-in — a site assertion written as a plain `expect_error()` receives none of
it, and no criterion detects the downgrade. That last is parity with the state
before M87 rather than a loss: the `expect_error()` calls matched by
`grep -c 'expect_error(' tests/testthat/test-norms-audit-roster.R
tests/testthat/test-norms-provenance.R` already asserted script aborts with no
site discrimination, and did so before this milestone as well as after it. **The class of evidence that reopens this:** an abort
site the manifest cannot see (a non-`stop()` spelling, or a call assembled at
runtime) appearing in the audit script, or a second identically-messaged pair
arising, which would make an existing key stop resolving to a single site. Any
of those argues for building the sweep the gap needs, not for restoring the
registry. Explicitly insufficient: a wish for symmetry with the pre-M87
machinery, or a review preferring more assertions to fewer.
