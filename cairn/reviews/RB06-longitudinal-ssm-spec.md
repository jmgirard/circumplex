# RB06: Adversarial review of the longitudinal & intraindividual SSM design spec (M23)

- **Date:** 2026-07-16
- **Output required:** write findings to `cairn/reviews/RR06-longitudinal-ssm-spec.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

circumplex is a CRAN R package for circumplex data analysis whose core is
the Structural Summary Method (SSM): profiles of p circumplex scale scores
are summarized by elevation e, amplitude a, and displacement d (an angle,
degrees [0, 360), LM = 360), with percentile-bootstrap or asymptotic
Monte Carlo confidence intervals. Angular/boundary behavior is where bugs
hide; statistical correctness outranks every other concern.

Milestone M23 is a docs-only design-gate milestone: it produces a
build-ready spec for the package's next major capability family —
repeated-measures/longitudinal SSM (paired timepoint contrasts,
person-level dependent resampling), a per-person (intraindividual) scoring
layer, growth-model support for displacement trajectories, and a thin
Bayesian draws adapter. The spec under review turns an earlier scoping memo
(Brief E, `devel/m5-m6-design-questions.md` §M6) into concrete decisions.
Build milestones will implement it verbatim, so plausible-but-wrong design
here corrupts everything downstream. Two escalation tripwires fired:
`no-oracle` (circular growth-model / dependent-resampling design has no
published reference implementation to validate against) and
`irreversible-api` (the repeated-measures API on the package's main entry
point is an exported-interface commitment).

Your mandate is adversarial: attack the spec, don't rubber-stamp it. The
spec's authors chose its framings; the author's blind spots are the point.

## Materials

Read in this order:

1. `CLAUDE.md` (repo root) — the binding statistical invariants (angle
   conventions, contrast conventions, boundary danger zones, minimal-deps
   doctrine).
2. `cairn/DESIGN.md` — architecture and statistical conventions.
3. `devel/longitudinal-ssm-spec.md` — **the spec under review** (all 8
   sections; §8 lists the authors' own open questions for you).
4. `devel/m5-m6-design-questions.md` §M6 (Q6.1–Q6.3) — the settled scoping
   memo the spec builds on. Its recommended *directions* (bivariate (x, y)
   growth framing; person-level case bootstrap; Bayesian = draws adapter)
   were plan-gate ratified; the spec's job was the concrete design, and
   your review may fault the concretization freely. Fault a ratified
   direction itself only with explicit flagging (see Constraints).
5. Code the spec extends (verify its claims against the code, not its
   paraphrase):
   - `R/ssm_analysis.R:197-292` (`ssm_analyze()` signature + validation +
     dispatch), `:329-431` (`ssm_analyze_means()`: `bs_input` assembly, the
     row-resampling `bs_function`, `strata = Group`).
   - `R/ssm_bootstrap.R` (all 201 lines): `ssm_replicate_intervals()`
     (shared interval assembly; contrast branch alignment at lines
     136-144), `quantile.circumplex_radian` (pole handling, lines
     170-190), `quantile.circumplex_contrast_radian` (192-201).
   - `R/ssm_montecarlo.R` (all 195 lines): the asymptotic engine; the
     joint-across-measures draw device at lines 119-149 is the device the
     spec's §2.2 generalizes to stacked occasions.
   - `R/utils.R:60-75` (`angle_dist()` — contrast displacement convention,
     (−180°, 180°] as +π at the exact half-turn).
   - `devel/bayesian_ssm.Rmd` — the brms sketch the spec's §5.3 finishes
     (note its line ~114 `#TODO: Account for 360 boundary`).
6. Decisions that bind (in `cairn/DECISIONS.md`): D-002 (BCa dropped for
   circular displacement — stands), D-003 (0/360 pole reported as 360),
   D-006 (angle classes stay S3), D-012 (longitudinal builds not
   merge-gated behind the v2.0.0 submission — a release-process fact, not
   reviewable here).

## Questions

Answer each by number. The spec's §8 questions are folded in below as Q2,
Q4, Q6, Q7, Q9.

1. **API irreversibility (§1).** Is `occasions` as a named-list argument on
   `ssm_analyze()` the right exported commitment, versus a separate entry
   point? Attack: the mutual-exclusivity rule with `scales`; wide-format-only
   (is rejecting a long/id API defensible, or does it push a error-prone
   reshape onto exactly the users most likely to get within-person structure
   wrong?); the composition matrix in §1.2 (are the two "out" cells the
   right cuts; is the occasions×grouping "in" cell fully specified?);
   `Occasion` as a new result-table dimension. Would any of these choices be
   regretted in a way that forces a breaking change later?
2. **Missing waves (§1.3, spec §8-1).** Is listwise-only for occasions the
   right narrowing, or is there a coherent pairwise semantics worth
   specifying now? If listwise-only ships, is the stated error (pairwise +
   occasions refused) the correct contract, and does complete-cases-across-
   waves introduce a selection bias worth a documented caution when
   missingness is outcome-related?
3. **The case bootstrap as specced (§2.1).** With persons as rows,
   row-resampling within group strata is claimed to implement the paired
   (person-level cluster) bootstrap with no new machinery. Verify against
   the actual resampler (`bs_function` + `boot::boot(strata=)`). Attack:
   what does the paired contrast estimand become when group sizes are
   unbalanced or when a stratum is small; does the percentile interval on
   `angle_dist(d2, d1)` replicates inherit any hidden assumption the
   independent-groups contrast doesn't have; are there degenerate-replicate
   interactions (one occasion flat, the other not) the exclusion warning at
   `ssm_replicate_intervals()` handles wrongly for paired data?
4. **Stacked-occasions Monte Carlo (§2.2, spec §8-2).** Is the sample
   covariance of stacked person-level score vectors divided by n the
   correct asymptotic covariance for the stacked occasion mean vector —
   including under `grouping` with unbalanced group sizes (each group gets
   its own stacked block, per the existing per-group loop)? Is the
   no-Fisher-z choice on the mean path correct? Name any regime (small n,
   near-singular within-person covariance, k > 2 occasions) where the MVN
   propagation is untrustworthy in a way the bootstrap arm is not.
5. **Intraindividual layer (§3).** Is flat-estimates + honest NA reporting
   the right in-package product, and are the two invariants in §3.3
   actually discriminating (would they catch a wrong per-person transform)?
   Is the circular mean + resultant length the right summary set for
   per-person d, given the aggregation caveat?
6. **Growth-model support (§4, spec §8-3).** The package will ship the
   coordinate table + draws adapter + vignette recipes, never fitting the
   LMM itself. Attack the statistical core: do MVN draws from a frequentist
   fixed-effect vcov, pushed through the (a, d) transform, give pointwise
   d(t) intervals with defensible coverage — or does the nonlinearity at low
   amplitude demand an explicit N/amplitude-conditioned caution (analogous
   to the package's CPM analytic-CI caution ladder, D-010)? Is
   `angle_unwrap()`'s convention (cumulative `angle_dist`, +180 at the exact
   half-turn) sound for its stated recipe use? Are the §4.2 oracles the
   right ones, and is the pole-crossing trajectory cell correctly framed as
   the headline test?
7. **Draws adapter contract (§5.1, spec §8-4).** Is shape dispatch on
   `is.null(angles)` sound, or does it invite silent misuse (e.g., 3-column
   profile draws passed without angles, silently transformed as (e, x, y))?
   Should shape be an explicit argument instead? Also: is reusing
   `ssm_replicate_intervals()` for posterior draws statistically honest
   (percentile quantiles of draws = credible interval; any place the
   bootstrap-specific semantics leak)? Are medians + circular mean the right
   point summaries (§5.1–5.2)?
8. **Oracle plan sufficiency (§2.3, §3.3, §4.2, §5.5, §6).** For each
   component: do the named oracles meet the bar of ≥2 *independent* oracle
   types per numeric result (types: frozen / live / invariant / closed-form
   / simulation-coverage; a CI method's primary oracle is coverage)? Flag
   any oracle that is vacuous (passes for wrong implementations), any
   danger zone untested (pole straddling, ±180 contrasts, flat profiles,
   all-NA), and any claim in the spec that no listed oracle actually
   checks.
9. **The paired-efficiency claim (§2.3, spec §8-5).** "Paired contrast CIs
   are narrower than independent-groups CIs at positive within-person
   correlation": state-as-measured, or derive-and-cite? If derivable, give
   the correct statement (it is presumably exact for Δe, asymptotic for Δa,
   and what, exactly, for Δd?).
10. **Beyond the brief.** Any plausible-but-wrong design element not
    covered above — especially anything that would silently corrupt the
    build milestones or embed a boundary bug in a new exported surface.

## Constraints

- Plan-gate decisions (2026-07-16, milestone M23) are fixed: one unified
  spec; Bayesian scope = thin draws adapter + brms vignette only (Stan
  companion out per §5.4 criteria); D-012 (merge gating) is not reviewable.
  Brief E's ratified *directions* (bivariate (x, y) growth framing;
  person-level case bootstrap; adapter-not-engine Bayesian) may be faulted
  only with an explicit "this challenges a ratified direction" flag and a
  concrete failure argument — never silently redesigned around.
- Package conventions bind: minimal deps (no new Imports; Suggests only,
  via a later dependency gate), no tidyverse in package code, S3 angle
  classes (D-006), pole = 360 (D-003), contrasts second-minus-first in
  (−180°, 180°], BCa stays dropped for circular displacement (D-002).
- **Oracle discipline:** never state a coverage/accuracy number as known;
  no expected values from memory. `devel/g2xx1.txt` is banned as a
  reference. Judge oracle plans by derivation and independence of types,
  not by predicting their numeric outcomes.
- Review only: do not edit the spec, do not write package code, do not
  commit. Tiny throwaway numeric sanity scripts are permitted; nothing that
  fits models for hours.

## Output format

In `cairn/reviews/RR06-longitudinal-ssm-spec.md`: open with a one-line
verdict — **sound / needs change (targeted) / needs redesign** — then
answer each question by number with reasoning and evidence (cite spec
sections and file:line). List additional findings under "Beyond the brief".
End with concrete, prioritized recommendations, each marked
**apply / consider / reject-with-reason**, precise enough that a
non-author session can apply them without guessing (where a fix is
mathematical, state the correct form).
