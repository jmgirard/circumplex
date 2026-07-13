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
