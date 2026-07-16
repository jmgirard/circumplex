# RR06: Review of the longitudinal & intraindividual SSM design spec (M23)

- **Reviewer:** independent Fable review (RB06), 2026-07-16
- **Materials:** as directed by RB06 (CLAUDE.md; cairn/DESIGN.md;
  devel/longitudinal-ssm-spec.md; devel/m5-m6-design-questions.md §M6;
  R/ssm_analysis.R; R/ssm_bootstrap.R; R/ssm_montecarlo.R; R/utils.R;
  src/parameters.cpp; src/circular.cpp; devel/bayesian_ssm.Rmd;
  cairn/DECISIONS.md D-002/D-003/D-006/D-010/D-012)

**Verdict: needs change (targeted).** The architecture is right — the case
bootstrap really is the existing resampler, the stacked-occasions MC
covariance is the correct asymptotic object, and adapter-not-engine is the
correct Bayesian scope. But the spec contains one statistically false claim
(§2.3/§8-5: the paired-efficiency statement is wrong for Δd — numerically
confirmed below), one missing danger cell that is the actual coverage risk of
§4 (low-amplitude/origin-proximal trajectories, not pole crossing), one
silent-corruption channel in the §1 API (unvalidatable cross-occasion column
order), and an underspecified §4 recipe requirement (joint bivariate LMM,
not two univariate fits) whose plausible-but-wrong shortcut the oracle plan
would not currently catch. All are fixable in the spec without touching any
ratified direction.

---

## Q1. API irreversibility (§1)

**`occasions` on `ssm_analyze()` vs a separate entry point: the right
commitment.** The paired analysis *is* the existing analysis with a different
column layout — same estimator, same resampler (`boot::boot` over rows,
`strata = Group`, R/ssm_analysis.R:383-398), same interval assembly. A
separate entry point would duplicate the dispatch, engine, and contrast
plumbing for no semantic gain, and the composition cells (occasions ×
grouping) would then need cross-entry-point validation. One argument on the
main entry point is the honest shape of the feature.

**Mutual exclusivity with `scales`: right rule, one unstated consequence.**
`scales` is currently a required positional argument with no default
(R/ssm_analysis.R:197). Making `occasions` an alternative spelling forces
`scales = NULL` (or missing-arg handling) into the signature — a real
exported-signature change the spec never states. It is backward-compatible
(positional callers unaffected), but the build must not discover this by
surprise; the spec should say it, and pin that `stopifnot(is_var(scales))`
(line 208) becomes conditional.

**Wide-format-only: defensible, but the spec's framing hides the real risk.**
Rejecting a long/id API is fine — the reshape argument is sound, and a later
`ssm_analyze_long()` sugar wrapper is additive, so nothing is irreversible
here. The real problem is what wide format *cannot validate*: the contract
"each occasion vector selects the same scales **in the same scale order**"
(§1.1) is enforceable only by documentation, because the column names differ
across occasions (`PA_1` vs `PA_2`). A user who supplies
`T2 = c("NO_2", ..., "PA_2")` in rotated order gets a silently rotated T2
profile — wrong displacement, wrong paired contrast, no error, no warning.
This is the single most likely silent-corruption channel in the whole spec,
it is aimed at exactly the boundary-sensitive quantity (d), and the spec is
silent on it. Mitigation is cheap: when all blocks share a
stem+suffix structure (strip a common per-block suffix/prefix; compare
stems), validate stem order across occasions and warn/error on mismatch;
when no stem structure is detectable, emit a one-time message naming the
assumed alignment ("PA_1 ~ NO_2 aligned by position"). See Recommendation 1.

**Composition matrix (§1.2): the two "out" cells are the right cuts.**
occasions × measures genuinely explodes the correlation draw core's key
space (R/ssm_montecarlo.R:79-93) for no demonstrated use case, and
occasions × contrast × grouping is a difference-of-differences — a new
estimand, correctly deferred to its own design pass. Both are reversible
narrowings (error → feature later). The occasions × grouping "in" cell is
*almost* fully specified: resampling unit and MC blocks are pinned, but the
**profile-row ordering convention** (occasion-major vs group-major in the
score matrix, the replicate columns, and the result table) is not, and
`ssm_by_group()`'s contrast slice is positional (`results[7:12]` vs
`[1:6]`, R/ssm_bootstrap.R:162). Underspecified ordering here is how a build
silently contrasts the wrong rows. Pin it (Recommendation 3).

**`Occasion` as a result-table dimension: right, but decide the
always-present question now.** `build_result_labels()` always emits
`Measure` (NA on the mean path; R/ssm_analysis.R:303-305). If `Occasion`
follows that precedent (always present, NA when unused), every existing
consumer of `results` sees a new column — a soft-breaking change to the
current exported shape that should be a deliberate, NEWS-documented
decision. If instead it appears only for occasions analyses, downstream code
(ssm_table, plot methods) must branch on its presence. Either is workable;
the spec must choose, because retrofitting the other choice later is the
breaking change Q1 asks about. Nothing else in §1 looks regret-prone: the
named-list spelling already generalizes to k occasions, and every "out" cell
can be opened compatibly.

## Q2. Missing waves (§1.3, spec §8-1)

**Listwise-only is the right narrowing, but the spec's stated justification
is technically wrong and should be fixed before it misleads the build.** The
spec claims that under pairwise the paired CI would be "computed over
resamples in which each person contributes to both occasions or neither per
replicate" while the point contrast uses all available data — an asymmetry.
That is not what the existing machinery would do: `bs_function` applies the
*same* `listwise` flag as the point estimate (R/ssm_analysis.R:374-380 with
`mean_scores(..., lwd = FALSE)` → `col_means`, src/parameters.cpp:104), so
each replicate would mirror the point statistic exactly — a person missing
T2 contributes to T1 only, in both. Pairwise-under-case-bootstrap is in fact
*internally coherent*; the real objection is different and stronger: under
pairwise, the "paired contrast" stops being a within-person contrast at all
— it compares partially overlapping subpopulations (T1-completers vs
T2-completers), and under outcome-related missingness it targets an estimand
nobody asked for. Ship listwise-only, but on those grounds.

**The error contract:** refusing pairwise + occasions outright (not just
pairwise + occasions + contrast) is slightly overbroad — pairwise per-occasion
*profiles* without contrast are as coherent as today's pairwise means — but
uniform refusal is simpler, matches the MC engine (which already stops on any
NA, R/ssm_montecarlo.R:18-25, keeping the two engines consistent for
occasions), and is reversible. Acceptable as specced; the error message
should say *why* (within-person contrast semantics), not just "unsupported".

**Selection-bias caution: yes, document it.** Complete-cases-across-waves
conditions on completing every wave; when dropout relates to the outcome
(entirely plausible for interpersonal-problems panels), the estimand is
"completers' change", biased for population change. Two additions: (a) the
docs caution the brief asks about; (b) an informative message reporting how
many persons listwise deletion dropped for occasions analyses —
`na.omit(bs_input)` is currently silent (R/ssm_analysis.R:345-347), and with
k·p columns per row the deletion rate grows with k, so silence is worse here
than in the cross-sectional case.

## Q3. Case bootstrap as specced (§2.1)

**Verified against the resampler: the central claim is true.** `boot::boot`
draws row indices per replicate (stratified by `Group`), `bs_function`
recomputes the statistic from `.data[index, ]` (R/ssm_analysis.R:374-380).
With persons as wide rows, every occasion block per replicate is computed
from the same drawn persons; within-person dependence is carried
nonparametrically. The claimed precedent is also real: the 2-measures/1-group
contrast already computes a dependent contrast from jointly resampled rows
(R/ssm_analysis.R:493-501) through the same `param_diff()` and contrast
quantile path. "No new resampling machinery" is accurate; the *statistic*
(bs_function computing k occasion blocks and stacking rows) is new code and
its row-order convention is the thing to pin (see Q1/Recommendation 3).

**One convention the spec must state explicitly: occasion contrast order is
list order, not level order.** Group contrasts take second-minus-first in
*factor level* (alphabetical) order; the occasion contrast must be
`names(occasions)` order (temporal, as supplied) — `param_diff(occ2, occ1)` =
T2 − T1. A builder mimicking the group path who factors/sorts occasion names
gets `T10 < T2`-style silent sign flips. State it and test it.

**Estimand under unbalanced/small strata:** the paired contrast is only
legal at 1 group (§1.2), so unbalanced-groups pathology cannot arise in this
build; for occasions × grouping (no contrast), stratified resampling
preserves each group's n and the per-group estimands are unchanged. If
occasions × contrast × grouping ever lands, this question reopens — record
that in the deferred cell's candidate note.

**Hidden assumptions in the percentile interval on `angle_dist(d2, d1)`
replicates: none new.** The Δd replicates live in (−π, π] exactly as for
independent-groups contrasts; `quantile.circumplex_contrast_radian()`
(R/ssm_bootstrap.R:194-201) centers on the circular mean of whatever
replicates it gets, and the branch alignment (:136-144) is
dependence-agnostic. Positive dependence just concentrates the replicates.
The standing concentration requirement (meaningless CI for diffuse
replicates) applies with a paired-specific reading: interpretability
requires *both* occasions' amplitudes reliably nonzero — worth one sentence
in the docs, since a user may certify only the T2 profile.

**Degenerate-replicate interactions: handled correctly, with one honest
caveat to document.** One occasion flat in a replicate → that occasion's
d (and fit) NA → Δd NA via `angle_dist` NA propagation (R/utils.R:65-69) and
`param_diff`; per-parameter `na.rm = TRUE` exclusion
(R/ssm_bootstrap.R:117-122) drops it from the Δd CI only, leaving Δe/Δa
intact — exactly the per-parameter conditioning DESIGN.md defends. Nothing
paired-specific is handled *wrongly*. The caveat: for a truth where one
occasion is near-flat, exclusion is correlated with resampled amplitude, so
the Δd CI is conditional on a selected subset — same as the existing
group-contrast behavior, but the paired docs inherit the existing
"conditional on estimability" language and the §2.3 boundary battery's
"one occasion degenerate" regression should assert the warning fires and the
Δe/Δa intervals are unaffected.

## Q4. Stacked-occasions Monte Carlo (§2.2, spec §8-2)

**The covariance is correct.** For person i, stack
v_i = (s_i^(1), …, s_i^(k)) ∈ R^(kp). The occasion-mean vector is v̄ =
n⁻¹Σv_i, and CLT gives Cov(v̄) = Σ_v/n with Σ_v = Cov(v_i), estimated by the
sample covariance of the stacked person vectors — the k = 1 case is
literally the shipped mean path (`stats::cov(cs_g) / n_g`,
R/ssm_montecarlo.R:105). The within-person cross-occasion covariance enters
through the off-diagonal p×p blocks, exactly as the spec says. Under
`grouping` with unbalanced sizes, the existing per-group loop
(:97-151) gives each group its own Σ̂_g/n_g — correct, since groups are
independent samples and each group's mean has its own n. The "same move as
measures" claim checks out structurally against :119-149 (joint draw, slice
per block, batched transform, `param_diff` on two blocks at :172).

**No-Fisher-z on the mean path: correct.** Fisher z is a
variance-stabilizing, range-respecting transform for correlations; means are
unbounded and the sample mean is the textbook asymptotically normal
statistic. Transforming would be wrong, not merely unnecessary.

**Regimes where MVN propagation is less trustworthy than the bootstrap:**

1. **Small n vs kp dimensions.** Σ̂_v has kp(kp+1)/2 free entries (k = 2,
   p = 8 → 136) estimated from n persons, and the draws treat Σ̂ as *known*
   (no t-style/Wishart correction) — anticonservative at small n in a way
   the bootstrap only partially shares. This is the existing single-occasion
   caveat with the dimension doubled (or k-tupled), not a new failure mode;
   `mvn_root()`'s PSD clamping (:185-189) keeps n < kp numerically safe
   (draws confined to the empirical span, much like resampling). The right
   response is measurement, not redesign: the §2.3 coverage grid should
   include a small-n cell (n ≈ 25–50) and, if k > 2 ships in Build A, a
   k = 3 cell.
2. **Near-singular within-person covariance** (high stability, T1 ≈ T2) is
   *not* a danger regime: the stacked Σ̂ is near-singular but clamped, and
   the contrast is a near-zero-variance linear image — the MVN draws
   concentrate correctly. If anything the MC engine behaves better here than
   a small bootstrap (fewer ties).
3. **Skewed scores at small n**: the empirical bootstrap carries third
   moments into the replicate distribution; MVN forces symmetry. For Δd near
   ±180° at moderate concentration this can shift coverage. Again: a grid
   cell, not a design change.
4. The `min(tabulate(grp)) < 2` guard (:28-34) remains formally sufficient
   but is very weak for kp dimensions; a documentation note that MC wants
   n_g comfortably above kp is worth adding.

None of these makes the MVN arm untrustworthy *in a way the bootstrap arm is
not* at field sample sizes; the two-engine invariant plus the coverage cells
above is the right adjudication.

## Q5. Intraindividual layer (§3)

**Flat estimates + honest NA is the right in-package product.** Per-person
SSM at one occasion is a deterministic transform with no within-person
inferential content; pooling genuinely is hierarchical-model territory, and
the adapter (§5) is the correct bridge. Reporting NA rates as a column
rather than silently dropping is exactly right for intensive data, where the
degenerate-profile branch (src/parameters.cpp:43-50) *will* fire.

**The §3.3 invariants are weaker than they look.** Invariant 2a
(identical profiles ⇒ per-person path reproduces the group mean-based path)
cannot catch a wrong per-person *transform*, because both paths share the
same C++ kernel (`group_parameters()` — the §3.2 wrapper reuses the
`ssm_score()` path, R/ssm_analysis.R:898). It only tests row plumbing. The
hand-computed fixtures (invariant 1) are the real independent check and are
well chosen (flat + pure-second-harmonic persons). Two cheap, genuinely
discriminating additions:

- **Linearity invariant (exact, heterogeneous profiles):** e, x, y are
  linear in scores, so the *mean over persons* of per-person (e_i, x_i, y_i)
  must equal the group-path (e, x, y) computed from the mean profile,
  exactly, for arbitrary heterogeneous data. This catches
  aggregation-order and row-misalignment bugs the identical-profiles case
  cannot.
- **Jensen inequality invariant:** group amplitude ≤ mean per-person
  amplitude, strictly when directions disperse — the aggregation caveat
  turned into an assertion.

Invariant 2b (circular mean of d_i matches `angle_mean()`) is discriminating
only if the summary layer is not literally `angle_mean()`; if it is, the
test is a tautology. Specify that the fixture recomputes the circular mean
by hand (atan2 of summed sines/cosines in the test, not via the package).

**Circular mean + resultant length is the right summary set, with one
documentation obligation the spec misses:** the circular mean of per-person
d_i (equal weight per person's *direction*) is a different quantity from the
displacement of the group mean profile (amplitude-weighted). They coincide
only in degenerate cases (e.g., identical profiles). The spec documents the
amplitude analog (mean resultant ≤ mean amplitude) but not the direction
analog; a user will compute both and file a bug when they disagree. One
paragraph in the §3 docs, and ideally a test asserting they *differ* on a
heterogeneous fixture (an anti-confusion regression).

**Build note:** `angle_mean()` (src/circular.cpp:8-26) has no `na.rm`; NA
persons propagate to NA. The summary layer must strip NA d_i (and report the
count) before calling it — spec should say so, since the NA-rates column
implies but does not state the exclusion.

## Q6. Growth-model support (§4, spec §8-3)

**MVN-from-frequentist-vcov through the (a, d) transform: defensible
coverage only in the concentrated regime — the spec needs the caution, and
the oracle needs the cell.** The device is the MC engine's own move and is
asymptotically valid wherever ‖(x(t), y(t))‖ is large relative to the draw
spread. It degrades in exactly one regime: **low amplitude at some t** —
when the mean trajectory passes near the origin (direction reversals,
crossovers, extrapolated t), the d(t) draw distribution becomes diffuse or
bimodal and circular quantiles are meaningless. This is the same regime the
package already guardrails for profiles, so yes: an explicit
amplitude-conditioned caution is required, analogous in spirit to the D-010
ladder but conditioned on the identifiable quantity here — per-t amplitude
relative to its own uncertainty. Concretely: at each t, apply the shipped
scale-free certification rule (`a_lci/(a_uci − a_lci) ≥ 0.35`, the D-007
rule `ssm_ci_accuracy()` already documents) to the a(t) draws and flag
uncertified t in the summary; the vignette states that d(t) intervals at
uncertified t are not interpretable. Secondary, vignette-level cautions:
fixed-effect vcov from REML ignores variance-component uncertainty
(anticonservative at small N; mention Kenward–Roger/t-scale as the user-side
remedy — the adapter cannot fix it and should not pretend to).

**A requirement the spec implies but never states, and whose omission the
oracles would not catch as written: the LMM must be fit *jointly* on (x, y).**
d(t) depends on the joint distribution of (x̂(t), ŷ(t)); the
plausible-but-wrong shortcut is two univariate LMMs with independent vcovs,
which zeroes Cov(x̂(t), ŷ(t)) and produces wrong d(t) intervals. In nlme this
means the stacked-outcome (dummy-coded multivariate) formulation. The spec
must name this requirement, the vignette recipe must be the joint fit, and
the §4.2 coverage grid must include a strong x–y fixed-effect-correlation
cell so that an independence-shortcut implementation *fails* the oracle
(making the oracle discriminating against this exact error).

**`angle_unwrap()` convention: sound.** Cumulative `angle_dist()` between
successive timepoints, +180 at the exact half-turn, matches the package's
half-turn atom convention (R/utils.R:56-64) and gives a deterministic,
fixture-testable helper; the 350→370→390 fixture is right. Its two failure
modes (near-180 steps, no common branch across heterogeneous persons) are
correctly documented-not-fixed. Two build details to specify: input domain
and units (degrees, any reals? wrap first?), and NA policy (a missing wave
makes all subsequent unwrapped values branch-ambiguous — propagate NA
thereafter or error; pick one and fixture it).

**§4.2 oracles: right set, wrong headline emphasis, one vague invariant.**
The pole-crossing cell (350°→10°) is the right *acceptance headline* for the
boundary machinery, but in the bivariate framing it is nearly guaranteed to
pass — (x, y) is boundary-free by construction, and the cell mostly
exercises the wrapping/summary code. The statistically hard cell is the
**low-amplitude/origin-proximal trajectory** (a(t) dipping toward 0 at an
interior t): that is where coverage actually degrades and where the caution
must demonstrably fire. Add it; keep both. The zero-slope invariant
("must reproduce the §2 paired-contrast machinery's answer for the
two-occasion special case") compares two different estimators (model-based
LMM draws vs nonparametric case bootstrap) that agree only asymptotically
under correct specification — as written it is a rubber ruler. Reframe as a
consistency check with a pre-registered tolerance at a large-n,
well-specified simulation cell, not an exact invariant.

## Q7. Draws adapter contract (§5.1, spec §8-4)

**Shape dispatch on `is.null(angles)` is not sound as the sole mechanism —
the spec's own worry (§8-4) is correct.** The ambiguity is confined to but
real at ncol = 3: profile draws from a p = 3 instrument (legal) passed
without angles are silently transformed as (e, x, y) — garbage with no
error; conversely (e, x, y) draws passed *with* a length-3 angles vector are
silently scored as a 3-scale profile. Auto-dispatch cannot distinguish these
by construction. The fix is cheap and preserves ergonomics: keep
angles-based inference where it is unambiguous, and make the ambiguous cell
explicit —

- `angles` supplied → shape B (profile), require `ncol(draws) == length(angles)`;
- `angles = NULL` and `ncol(draws) != 3` → error with a message explaining
  both shapes;
- `angles = NULL` and `ncol(draws) == 3` → require an explicit shape/type
  argument (e.g. `type = "parameters"`); error otherwise, naming the
  ambiguity.

This costs shape-A users one argument and eliminates both silent cells. A
second, independent silent channel: shape A assumes **column order**
(e, x, y). brms fixed-effect column order follows the user's formula; a
`sin + cos` formula silently swaps x/y, reflecting d about 45°. Document
loudly, and when `colnames(draws)` are present and unrecognizable as
(intercept, cos, sin)-like, message the assumed mapping (or accept an
explicit column-mapping argument).

**Reusing `ssm_replicate_intervals()` for posterior draws is statistically
honest.** Percentile quantiles of posterior draws are the equal-tailed
credible interval; the circular path (center on circular mean, unwrap,
quantile, re-wrap) is the correct circular analog for concentrated
posteriors, and the pole snap (D-003/M20) and radian classing apply
verbatim — that is the point of the reuse. Bootstrap-specific semantics that
leak, all manageable: (a) `replicate_label` must say "posterior draws" so
the degenerate warning (R/ssm_bootstrap.R:92-101) doesn't say "bootstrap
resamples"; (b) the per-parameter NA exclusion means a d credible interval
is conditional on a > 0 — for continuous shape-A posteriors this is
measure-zero and harmless, but for shape-B draws it can bind; document as
the same "conditional on estimability" semantics; (c) `t0` has no observed
estimate — the adapter feeds its own point summaries as t0, and the spec
should say so explicitly; (d) shape A has no `fit` — the adapter must
synthesize a 6-column layout (fit = NA) to satisfy `ssm_param_names()`
order (R/ssm_bootstrap.R:74-81), a build detail worth one spec line since
jamming a 5-column matrix in would misalign every parameter.

**Medians + circular mean are the right point summaries**, with one
documentation obligation: marginal summaries are not jointly coherent —
median(a) ≠ √(median(x)² + median(y)²), and the reported d is not the
direction of the reported (x, y). Unlike the bootstrap path (whose t0 is the
coherent observed estimate), a user recomputing a from the reported x, y
will get a different number. One sentence in the docs prevents the bug
report. Median for e is harmless (near-symmetric); circular mean for d is
standard; `angle_mean()`'s NA-at-zero-resultant behavior is the right
diffuse-posterior outcome (and should be documented as such, not "fixed").

## Q8. Oracle plan sufficiency (§2.3, §3.3, §4.2, §5.5, §6)

Judged by derivation and type independence, per the oracle discipline:

- **§2.3 (paired contrasts): meets the bar**, with one caveat and two gaps.
  Coverage (primary, correctly so for a CI method) + closed-form for Δe +
  invariants + boundary regressions. Caveat: the two-engine agreement
  invariant is *not* independent for the shared downstream code — both
  engines flow through the same `param_diff()`, `ssm_replicate_intervals()`,
  and quantile methods, so a branch-handling bug passes both engines
  identically; the coverage oracle carries that weight, and its listed cells
  (Δd near ±180°, near 0°, pole-straddling truths) are the right ones for
  it. The degenerate-dependence (random re-pairing) invariant is good and
  genuinely discriminating for the dependence handling. Gaps: (1) no
  **ρ ≤ 0 / large-Δd efficiency cell** — required anyway once the §8-5 claim
  is corrected (see Q9); (2) no small-n cell for the MC arm (see Q4). The
  "flat occasion / one occasion degenerate" danger zones are present as
  behavioral regressions — right type for a behavior, since coverage is
  undefined for an NA-truth parameter.
- **§3.3 (per-person layer): adequate type count for deterministic code**
  (closed-form + invariant), but invariant 2a is near-vacuous for the
  transform (shared kernel — see Q5) and 2b risks tautology. Add the
  linearity and Jensen invariants (Q5); with those, the plan is sharp.
- **§4.2 (growth support): type count fine; discriminating power not.** As
  written, a wrong implementation that fits x and y independently (no
  cross-covariance) would plausibly *pass* all three oracles: the
  pole-crossing cell has high amplitude throughout (coverage barely
  sensitive to the cross term when the truth is a pure rotation at constant
  a — and if mildly low, nothing in the plan localizes it), the concentrated-
  regime unwrap agreement tests point trajectories, not interval width, and
  the closed-form fixtures test only `angle_unwrap()`. Add the
  strong-x–y-correlation cell and the low-amplitude cell (Q6) to make
  coverage discriminating. Also: simulate from the same model family the
  reference recipe fits, so a coverage failure indicts the adapter, not
  model misspecification.
- **§5.5 (adapter): meets the bar and is well designed.** The
  reproduce-the-bootstrap-run-exactly invariant is the decisive plumbing
  oracle (byte-level, catches any quantile-path divergence); closed-form
  4-row fixtures with a pole-straddling pair and the all-NA contract cover
  the danger zones; D-003 pole snap asserted. One cheap addition: a
  **shape-A/shape-B consistency invariant** — for a profile-draws matrix,
  shape B must equal shape A applied to the per-row (e, x, y) computed from
  those profiles; exact by construction, catches shape-dispatch and
  column-mapping bugs (the Q7 channels) that nothing else in §5.5 exercises.
- **Vacuity / unchecked-claim sweep:** the §5.2 Rayleigh-induced-prior
  statement is a documentation claim with no oracle — acceptable for a doc,
  but a 10-line prior-predictive simulation in the vignette would make it
  shown-not-asserted. The §2.3 "agreement in expectation" invariant needs a
  pre-registered tolerance (SE-based criterion) or it becomes a judgment
  call at build time. The §6 oracle-registry gap is correctly self-flagged
  and correctly deferred to the first build that adds an oracle.

## Q9. The paired-efficiency claim (§2.3, spec §8-5)

**Derive-and-state — but the statement in the spec is false as written, and
this is the review's most important statistical finding.** "Paired contrast
CIs are narrower than independent-groups CIs at positive within-person
correlation" is:

- **Exact for Δe** (finite-sample variance identity): Var(ē₂ − ē₁) =
  (σ₁² + σ₂² − 2ρ_e σ₁σ₂)/n vs (σ₁² + σ₂²)/n for two independent groups of
  the same n — narrower iff ρ_e > 0, where ρ_e is specifically the
  within-person correlation of the *profile elevations* (mean-of-scales),
  not of scale scores generally. Textbook paired-design result; cite any
  design text and state ρ_e precisely.
- **Asymptotic and conditional for Δa and Δd** (delta method). Writing C for
  the cross-occasion covariance of the estimated (x̂, ŷ) blocks and ∇g_j for
  the parameter gradient at occasion j, the paired asymptotic variance is
  Var₁ + Var₂ − 2∇g₂ᵀC∇g₁: narrower than independent **iff the
  gradient-projected cross-covariance ∇g₂ᵀC∇g₁ is positive** — not iff
  within-person correlation is positive. Under an isotropic cross-covariance
  C = cI₂ (c > 0), ∇d_j ∝ tangent/a_j and ∇a_j ∝ radial, so the cross term
  for both Δd and Δa is proportional to **c·cos(Δd)**: the claim holds only
  when the true angular change is less than a quarter turn, and **reverses —
  paired CIs asymptotically *wider* than independent — when |Δd| > 90°**
  despite strongly positive within-person correlation.
- **Numerically confirmed** (tiny sanity script per the brief's allowance;
  n = 200, 20k sims, isotropic bivariate model, ρ = 0.6): Var(Δd̂)
  paired/independent ratio = **0.49 at Δd = 30°** and **1.41 at
  Δd = 135°** — matching the theory's 1 − ρcos(Δd) prediction (0.48, 1.42)
  to two decimals.

Consequences: (1) the docs must never print the unconditional claim; state
the exact Δe result, the projected-covariance condition for Δa/Δd with the
cos(Δd) special case, and note the |Δd| > 90° reversal; (2) the §2.3
efficiency measurement must include a large-Δd cell (e.g., 135°) at ρ > 0
*expecting* the reversal — which simultaneously turns the oracle from a
confirmation into a discrimination. This does not challenge any ratified
direction — the paired design is still correct and still typically more
efficient in the small-change regime that motivates it; only the claim's
scope was wrong.

## Q10. Beyond the brief

1. **`ssm_ci_accuracy()` × occasions objects.** The diagnostic replays an
   object's own CI procedure from `details$suff_stats`
   (R/ssm_analysis.R:408-419; R/ssm_ci_accuracy.R). An occasions object will
   carry mean-path suff_stats computed over the flattened k·p scale columns;
   passed to the diagnostic, it would plausibly *run* and simulate from a
   population with the wrong dependence structure — silent wrong output on a
   shipped diagnostic. The spec must state the contract for Build A:
   `ssm_ci_accuracy()` on an occasions object errors informatively
   (extension is a later candidate — the stacked-MC machinery makes it
   natural, but it is its own design).
2. **Plot/table surface unaddressed.** `ssm_table()`, `ssm_plot_circle()`,
   `ssm_plot_curve()` consume `results`/`scores`; an `Occasion` dimension
   changes labeling and row structure. Not statistical, but Build A's
   acceptance should include at least "existing plot functions either
   support or cleanly reject occasions objects" — silence risks a plot that
   draws k occasions as if they were groups with wrong labels.
3. **New `circumplex_ssm` details fields.** `details` gains occasions
   metadata (names, k); `print.circumplex_ssm` and snapshot tests must know.
   One spec line.
4. **Grouping must be time-invariant** — wide format enforces this
   structurally (one Group cell per person-row), which is worth one doc
   sentence so users with time-varying grouping don't shoehorn it in as
   occasion-specific columns.
5. **RNG-contract bookkeeping**: the stacked MC draw consumes one rnorm
   block per group of size boots × kp; adding an occasion is a "structural
   edit" that changes the draw sequence — already the documented DESIGN.md
   semantics, but the DESIGN.md reproducibility table should gain the
   occasions row at build time.
6. **`devel/bayesian_ssm.Rmd` contains a live argument-order confusion the
   vignette rewrite must not inherit.** The sketch's derivation comment
   (line 43: "b = arctan(b1/b2) or atan2(b1, b2)") has the atan2 arguments
   swapped relative to its own correct code (line 47: `atan2(b2, b1)` with
   b1 = cos coefficient, b2 = sin coefficient — i.e., atan2(y, x), correct).
   A builder transcribing the comment instead of the code reflects every
   displacement about 45°. This is the Q7 column-order hazard demonstrated
   live in the package's own devel file; the §5.3 vignette should treat the
   sketch as untrusted and derive the mapping fresh (x = cos coefficient,
   y = sin coefficient, d = atan2(y, x)), with the §5.5 fixtures pinning a
   known-direction case.

## Recommendations (prioritized)

1. **apply — Correct the §2.3/§8-5 efficiency claim.** Replace with: exact
   for Δe (paired-variance identity, narrower iff within-person elevation
   correlation > 0); asymptotic for Δa/Δd, narrower iff the
   gradient-projected cross-occasion covariance ∇g₂ᵀC∇g₁ > 0, which under
   isotropic dependence is ∝ cos(Δd) — reverses for |Δd| > 90°. Docs state
   the derived conditional claim; the oracle measures it, including a
   ρ > 0, Δd ≈ 135° reversal cell. (Mathematical form above, Q9.)
2. **apply — Add cross-occasion alignment validation to §1.1.** Stem-based
   order check across occasion blocks (strip common per-block
   suffix/prefix; error or warn on mismatched stems), with a one-time
   message naming the assumed positional alignment when no stem structure
   exists. This closes the spec's largest silent-corruption channel.
3. **apply — Pin the ordering conventions in §1/§2.** (a) Occasion contrast
   order = `names(occasions)` list order (temporal), never factor/alphabetical
   sorting — state it beside the second-minus-first rule and regression-test
   a `T10`-style name. (b) Profile-row order for occasions × grouping
   (choose and state, e.g. occasion-major within group to parallel the
   measure path). (c) Decide whether `Occasion` is an always-present result
   column (NA when unused, matching the `Measure` precedent) or conditional
   — and state the compatibility consequence either way.
4. **apply — §4: require the joint bivariate LMM and add the two missing
   oracle cells.** State that the reference recipe fits x and y jointly
   (stacked-outcome nlme formulation); add to §4.2 a strong x–y
   fixed-effect-correlation cell (so an independent-fits shortcut fails
   coverage) and a low-amplitude/origin-proximal trajectory cell (the true
   coverage danger; keep pole-crossing as the boundary-machinery headline).
   Add the amplitude-conditioned caution: per-t certification via the
   shipped D-007 rule on the a(t) draws, uncertified t flagged in the
   summary and stated non-interpretable in the vignette.
5. **apply — §5.1: close the shape-dispatch ambiguity.** Keep angles-based
   inference where unambiguous; make ncol = 3 with `angles = NULL` require
   an explicit type argument (error naming the ambiguity otherwise), and
   require ncol(draws) == length(angles) for shape B. Document the shape-A
   column-order assumption (e, x, y) loudly, with a message or explicit
   column-mapping when colnames are present but unrecognizable.
6. **apply — §1.3: fix the pairwise justification and the error surface.**
   Replace the "asymmetry" rationale (incorrect — the replicate statistic
   mirrors the point statistic under the existing `listwise` plumbing) with
   the estimand rationale (pairwise makes the paired contrast a comparison
   of partially overlapping subpopulations); keep listwise-only. Add the
   dropped-n message for occasions listwise deletion and the
   outcome-related-missingness caution to the docs.
7. **apply — state the `ssm_ci_accuracy()` contract for occasions objects**
   (informative error in Build A; extension a recorded candidate). (Q10-1.)
8. **apply — §3.3: strengthen the invariants.** Add the exact linearity
   invariant (mean of per-person (e, x, y) over heterogeneous profiles
   equals group-path (e, x, y) of the mean profile) and the Jensen amplitude
   inequality; require the circular-mean fixture to recompute by hand rather
   than via `angle_mean()`; document that circular mean of d_i ≠ direction
   of the mean profile (with a fixture asserting they differ); specify NA
   stripping (with count) before `angle_mean()`.
9. **consider — §2.3/§2.2 grid additions:** a small-n cell (n ≈ 25–50) and,
   if k > 2 ships in Build A, a k = 3 cell, to measure the known-Σ̂
   anticonservatism of the stacked MC arm; a doc note that the MC engine
   wants n_g comfortably above k·p. Low cost, bounded benefit — the
   two-engine invariant already partially covers it.
10. **consider — §1.1 signature note:** one spec line that `scales` becomes
    optional (`NULL` default) with conditional validation, so the build
    treats the exported-signature change as deliberate.
11. **consider — §5.5: add the shape-A/shape-B consistency invariant**
    (shape B equals shape A on the per-row (e, x, y) of the same profile
    draws; exact, and the only oracle that exercises the Q7 dispatch
    channels).
12. **consider — Build A acceptance line for the output surface:** plot/table
    functions support or cleanly reject occasions objects; `print` +
    snapshot updates; details fields listed. (Q10-2/3.)
13. **reject — long/id-format API for the build** (the wide-only narrowing
    stands): the reshape argument is sound, `ssm_analyze_long()` sugar
    remains an additive later candidate, and Recommendation 2 addresses the
    real risk wide format creates. Reversing this would also challenge a
    plan-gate-adjacent framing without a concrete failure the alignment
    validation doesn't already fix.
14. **reject — replacing `is.null(angles)` dispatch with two exported
    functions** (`ssm_draws_parameters()`/`ssm_draws_profiles()`): doubles
    the exported surface the milestone flagged as irreversible for no gain
    over Recommendation 5's explicit-type-when-ambiguous rule.
15. **reject — a Wishart/t-style correction for the stacked MC covariance**:
    it would diverge from the shipped single-occasion mean path (which has
    the identical property), fragment the engine's semantics, and the
    percentile bootstrap default is the package's answer to small-n
    non-normality; measure it (Rec. 9) instead of redesigning it.
