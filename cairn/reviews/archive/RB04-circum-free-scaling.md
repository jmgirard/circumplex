# RB04: CIRCUM free-scaling covariance family — go/no-go + spec review (M17)

- **Date:** 2026-07-12
- **Output required:** write findings to `cairn/reviews/RR04-circum-free-scaling.md`

You are performing an independent expert review as a psychometrician /
statistician with SEM and circumplex-modeling expertise. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions in order, and write
your findings to the output path above using the same numbering.

Your judgment here gates a v2.0.0 design decision and, if it is "go", the
derivation an implementer will build from without re-checking the math. The
single highest-stakes item is Q3 (the free-family analytic gradient): a
sign error or a dropped term there is exactly the class of
plausible-but-wrong statistics this review exists to catch.

## Background

**The package.** `circumplex` (CRAN, R) does circumplex data analysis. The
relevant subsystem is `cpm_fit()`, a native (from-scratch, base-R) estimator
for **Browne's (1992) circular stochastic process model** (CPM) — a
structural model for a `p × p` correlation matrix in which each manifest
variable sits at an angle `θ_i` on a circle and the model-implied correlation
between two variables is a Fourier function of their angular separation:

    P_ij(γ) = ζ_i ζ_j · ρ(θ_i − θ_j),   ρ(δ) = Σ_{k=0}^{m} β_k cos(kδ),
    with β_k ≥ 0, Σ_k β_k = 1  (Herglotz: guarantees P is PD),  diag P = 1.

Free parameters: item angles `θ_i` (one fixed for rotational identification),
communalities `ζ_i ∈ (0,1)`, and Fourier weights `β_0…β_m`. The estimator
fits the **correlation** structure `P(γ)` by ML (Wishart discrepancy), i.e.
the model-implied matrix has a unit diagonal by construction. This engine is
**already built, validated, and shipping toward v2.0.0** — it is not under
review here except where the extension forces a change.

**What "free-scaling" means and why it's wanted.** The published reference
programs CIRCUM (Browne) and CircE (Grassi et al., 2010) do **not** fit the
correlation structure. They fit the **covariance** structure

    Σ(γ, σ) = D_σ · P(γ) · D_σ,   D_σ = diag(σ_1,…,σ_p),  σ_i > 0,

adding `p` free scale factors `σ`. Our correlation family is the special case
`σ ≡ 1`; theirs is strictly larger and **nests** ours. Adding this family to
`cpm_fit()` ("CIRCUM-compatibility mode") would let the package **exactly
reproduce published CIRCUM/CircE output**, which our diag-constrained family
provably cannot (see below). The legacy ROADMAP recorded this as a "decide
post-M4" candidate; Jeff has now asked to decide it for v2.0.0 (D-008).

**Why this needs independent review — the crux.** The design doc
(`devel/m4-browne-design.md` §3.2) originally claimed the free-scaling family
is redundant: that ML fitted to a correlation matrix `R` would return
`σ̂ = 1` at the optimum, so the extra parameters buy nothing. **The B6
validation battery (2026-07-06) proved that claim false at finite N** (design
doc §11, change-log entry dated 2026-07-06): CIRCUM/CircE's published
vocational-interest solution (Grassi et al., 2010, Appendix A) has fitted
variance ratios of .963–1.042 (σ̂ ≠ 1), and its published discrepancy
`F̂ = 0.089815` sits *below* our diag-constrained optimum (0.09596) precisely
because our family is nested in theirs. So the free-scaling family is a
genuinely different, strictly-better-fitting model — reproducing published
output requires actually fitting `σ`, not assuming it away. Two things follow
that make this a real design question, not a mechanical port:

1. **The analytic gradient must be re-derived.** The current gradient
   (§3.4) is built on the simplification "the parameterization holds
   `diag P = 1` fixed, so only off-diagonal `∂P_ij` enter." Once `σ` is free,
   `diag Σ = σ²` varies, so the diagonal derivatives no longer vanish and the
   `∂/∂σ` block is entirely new. Getting this wrong is the dominant
   statistical risk.

2. **The analytic-CI validity argument may not survive.** §3.2's justification
   for trusting information-based (analytic Wald) CIs rests on the
   scale-invariance of the correlation model. It is unclear whether that
   argument transfers to the free covariance family, or whether bootstrap
   becomes the only trustworthy CI path. The package already has *measured*
   evidence that the diag-constrained analytic CIs under-cover badly at field
   N (DESIGN.md "CPM confidence intervals: measured coverage").

## Materials

Read these, in this order. Paths are repo-relative from the repo root.

**Design doc (primary):** `devel/m4-browne-design.md`
- §1 (lines 34–145) — the model, variants A–D, and their df.
- §2 (lines 146–232) — identification: rotation (§2.1), the Σβ=1 constraint
  (§2.2), **reflection canonicalization (§2.3, lines 165–188)**, the 0°/360°
  danger zone (§2.4).
- **§3.2 (lines 253–269)** — the (now half-wrong) scale-invariance argument
  for correlation-ML validity and analytic CIs. This is the claim the
  extension challenges.
- **§3.3 (lines 271–291)** — the current unconstrained parameterization
  (map + Jacobian table) for θ, ζ, β. `σ` is not in it yet.
- **§3.4 (lines 293–319)** — the current analytic gradient, including the
  "only off-diagonal ∂P_ij enter" simplification at line 296 and the
  mandatory finite-difference gradient unit test.
- §3.5 (lines 321–378) — optimizer (`nlminb`, unconstrained coords),
  deterministic multi-start, boundary polish, convergence acceptance.
- §5.2–5.3 (lines 456–524) — CI methods (analytic Wald + percentile
  bootstrap) and fit indices (T = n·F̂, df, RMSEA/RMSEA-CI, SRMR, etc.).
- §6.3 (lines 625–659) — tolerances and the CIRCUM mismatch-diagnosis
  checklist.
- **§11 change log, entry 2026-07-06 "B6 validation battery" (lines 806–839)**
  — the empirical refutation of the σ̂=1 claim, the Grassi et al. (2010)
  Appendix A targets, the model-difference allowances, and the OpenMx
  free-scaling attribution result. **Read this in full.**

**The existing free-scaling oracle (already in the test suite):**
`tests/testthat/test-cpm_oracles.R`
- `cpm_mx_model(...)` helper, lines 50–91 — builds an OpenMx model; the
  `free_scaling = TRUE` branch (lines 59–61, 84–89) fits exactly
  `Σ = D_s (D_ζ C D_ζ + I − D_ζ²) D_s` with free `s`. This is the reference
  parameterization you should compare any proposed spec against.
- Test "OpenMx oracle: the free-scaling model reproduces published CircE",
  lines 329–372 — the free-scaling OpenMx fit lands on published CircE output
  (ζ/β to 4 decimals, angles ~0.01°); the diag-constrained variant-B fit
  shows the nesting direction. This oracle **already exists** and is the
  named validation anchor for M18.
- Header comment lines 1–49 — provenance of the Grassi et al. (2010)
  transcription and the model-difference framing.

**The engine (for API/df/CI anchors):** `R/cpm_fit.R`
- `cpm_discrepancy()` ~line 88 (the ML `F`); `cpm_spec()` df derivation
  ~lines 93–141 (`df = p(p−1)/2 − q`); Hessian / analytic-CI machinery
  ~lines 657–699; `cpm_rmsea_ci()` ~lines 798–830. You do **not** need to
  propose code — these are so you can reason about how `σ` changes q, df,
  and the information matrix.

**Measured CI coverage (decides how much to trust analytic CIs):**
`cairn/DESIGN.md`, "CPM confidence intervals: measured coverage" (from
line 77). The diag-constrained analytic CIs already under-cover (angle
.76–.88, boundary β ~.77) at field N; bootstrap is the shipped default.

**Sources.** Primary source for the published targets is Grassi, Luccio &
Di Blas (2010), "CircE: An R implementation of Browne's circular stochastic
process model," *Behavior Research Methods*, 42(1), 55–73, **Appendix A
(pp. 70–72)** and Table 2. The relevant values are already transcribed into
`test-cpm_oracles.R` (see its header). Browne (1992), *Psychometrika* 57(4),
469–497 is the model's primary source. If you need a value not in the test
transcription and cannot access the primary source, say so — do not
substitute memory (primary-sources rule).

## Questions

Answer each by number in the RR, with reasoning and evidence.

1. **(a) Go/no-go.** Does the reproduction value of exact published
   CIRCUM/CircE output justify adding a second fitted covariance family to
   `cpm_fit()` for v2.0.0? Weigh: the demonstrated model difference (our
   family cannot reproduce published output; §11 B6 entry); the fact that an
   OpenMx free-scaling oracle already reproduces the published numbers, so
   the *validation* target exists; against the cost of a second estimation
   family (new parameters, new gradient, new identification, doubled CI
   surface). A reasoned **no-go is an acceptable outcome** — it retires M18
   and the CIRCUM candidate. State your recommendation and the decisive
   consideration.

2. **(b) σ parameterization + identification.** Propose the unconstrained
   map and Jacobian for the `p` scale factors `σ` (analogous to the §3.3
   table: e.g. `σ_i = e^{s_i}` giving `∂σ_i/∂s_i = σ_i`, or an
   alternative you argue is better-conditioned). Then resolve identification:
   given `diag P = 1` identically, is `Σ = D_σ P D_σ` identified with **all
   p** scale factors free, or does fitting a **correlation** input `R`
   (unit diagonal) leave a redundancy that must be pinned? The nesting anchor
   is `σ̂ = 1` at the correlation optimum, but B6 shows `σ` is free and
   `σ̂ ≠ 1` at finite N — reconcile this: what exactly identifies each σ_i
   when the input has a unit diagonal? Does the OpenMx oracle
   (`cpm_mx_model` free-scaling branch, all `p` of `s` free with `lbound
   = 0.1`) already answer this empirically, and is that sufficient?

3. **(c) The free-family analytic gradient — CENTRAL.** Derive the analytic
   gradient of the ML discrepancy `F(S, Σ)` for the free-scaling family.
   Specifically:
   - Give `∂F/∂σ_i` explicitly. With `Σ = D_σ P D_σ`, the diagonal now
     varies, so this block is entirely new.
   - Give the **corrected** `∂F/∂θ_i`, `∂F/∂ζ_i`, `∂F/∂β_k` for the free
     family — state precisely where the §3.4 "only off-diagonal ∂P_ij enter"
     simplification (line 296) breaks and what the diagonal terms contribute
     now that `diag Σ = σ²` is not held fixed. (Note `diag P = 1` still holds;
     the diagonal of *Σ* is what moves.)
   - Confirm or correct the chain-rule composition with the §3.3 Jacobians.
   - Specify the mandatory finite-difference gradient unit test for the
     extended parameter set (analog of §3.4's ≥20-random-point, rel ≤ 1e-7
     test): what to randomize (including σ), and any conditioning caveats.
   Show the derivation, not just the result — the implementer will not
   re-derive it.

4. **(d) df / χ² / CI treatment.** The free family adds `p` parameters
   (`q → q + p`, minus any identification constraint from Q2), so
   `df = p(p−1)/2 − q'` shrinks. (i) State the correct df and whether the
   input path matters (a covariance model fitted to `R` vs to a covariance
   `S`). (ii) **Does §3.2's scale-invariance argument that justifies the
   analytic Wald CIs still hold for the free covariance family, or is
   bootstrap the only trustworthy path?** The package already measures the
   diag-constrained analytic CIs under-covering at field N (DESIGN.md
   coverage record). Give a defensible position: analytic-CI-with-caveat,
   bootstrap-mandated, or conditional — and the reasoning. (iii) Any change
   to T = n·F̂ / RMSEA / SRMR conventions for the free family?

5. **(e) Canonicalization / identification interaction.** How does the free
   `σ` block interact with the existing **reflection** canonicalization
   (§2.3) and **angle** canonicalization (§2.3–§2.4)? Are `σ` invariant
   under reflection and rotation (so canonicalization is unaffected), or does
   the extra block introduce a new sign/permutation redundancy that
   canonicalization must resolve? Flag any interaction the spec must pin
   before implementation.

6. **Spec adequacy (if go).** Given your answers to 2–5, is there any
   remaining derivation gap that would force the M18 implementer to make an
   unreviewed statistical choice? List each gap, or confirm the design is
   complete enough to build from. Also: is the existing OpenMx free-scaling
   oracle (`test-cpm_oracles.R:329`) plus the Grassi et al. (2010) Appendix A
   published targets a sufficient validation battery for M18, or is an
   additional oracle type needed (per the ≥2-independent-oracle-types bar)?

## Constraints

Fixed — do not relitigate; flag disagreement explicitly rather than working
around it silently.

- **In v2.0.0 scope (D-008).** The CIRCUM free-scaling family is admitted into
  v2.0.0 (superseding D-001's new-features-excluded clause *only* for CIRCUM);
  there is no release date pressure — v2.0.0 ships when the statistics are
  ready. So a "go" is not blocked by scheduling, and a "no-go" must rest on
  statistical/design merit, not on timing.
- **The existing diag-constrained engine is settled and validated.** Do not
  reopen the correlation-family gradient, its convergence-acceptance rule
  (§3.5, scaled gradient norm; `nlminb` code advisory only), the boundary
  polish, or the multi-start design. The extension must sit *beside* the
  existing family, not replace it.
- **Angle handling is radians internally, degrees only at the API boundary**
  (§3.3); LM = 360 not 0 (package convention). σ does not touch this.
- **Bootstrap is the shipped default CI method** for the raw-data path
  (analytic only on the `cormat` path, with an N-conditional caution). A
  finding that free-family analytic CIs are untrustworthy is compatible with
  the existing posture; a finding that bootstrap is *mandatory* for the free
  family is a decision, so state it as one.
- **Primary-sources rule.** Any value you assert about the published
  CIRCUM/CircE solution must trace to Grassi et al. (2010) (via the
  `test-cpm_oracles.R` transcription or the primary PDF), never to memory.
- **This is a design/spec review, not implementation.** No `cpm_fit()` code,
  tests, or `R/`/`src/` changes are in scope (those are M18). Your product is
  the derivation and the decision.

## Output format

In `cairn/reviews/RR04-circum-free-scaling.md`: answer each question 1–6 by
number, with reasoning and evidence (show the Q3 derivation in full). List any
additional findings separately under "Beyond the brief." End with concrete
recommendations, each marked **apply / consider / reject-with-reason**, and a
clear one-line **GO / NO-GO** verdict on the second fitted family with its
single decisive reason.
