# M111: Shrink the ill-conditioning refusal to what the certificate cannot certify

- **Status:** review
- **Priority:** normal
- **Depends on:** M108
- **Driving RR:** —
- **Principles touched:** IP1, IP3, GP2
- **Branch/PR:** `m111-certificate-refusal-region` / https://github.com/jmgirard/circumplex/pull/142

## Goal

Replace the a-priori condition-number refusal at both surfaces with M108's
per-fit certificate, so a fit computes whenever its own certificate passes.

## Scope

Surface tier: **user-facing** — it changes what `axes_reliability()` returns
for a class of fitted matrices.

This is the remedy D-048 recorded and D-050 made due rather than conditional.
The contract is fixed by RR19 and RR20 and is mechanism-agnostic: the refusal
becomes computed-with-certificate where the certificate passes, with refusal
retained for indefiniteness, exact singularity, and certificate failure. The
2.0.0 line is unreleased, so this ships as an exported-behaviour change on the
dev line as D-048's own threshold move did, not through a deprecation cycle.

**In:**
- Both consuming surfaces, `R/axes_corrected_se.R:302` and `R/axes_scaled_fit.R:165`.
- A refusal literal for certificate failure, joining the existing vocabulary.
- The `WHY THE LIMB EXISTS AT ALL` section and the exported doc sites.
- `NEWS.md`.

**Out:**
- The certificate itself → M108.
- The `"indefinite"` and `"singular"` limbs, which keep refusing unchanged.
- Moving the constants → refused by D-048 and D-049.
- The calibration-domain sentence → M110.

## Acceptance criteria

- [x] AC1: `axes_sigma_degenerate()` keeps its condition-number floor only as
      the partition between `"indefinite"` and `"singular"`; neither surface
      refuses on that floor alone.
- [x] AC2: Each of the five reachable-geometry cases the M106/RR19 family in
      `devel/degeneracy-oracle/exact_oracle.R` enumerates, injected at the
      `axes_fitted_cov()` seam, yields finite corrected component SEs and a
      finite `cval`, with `details$se_correction_failed` and
      `details$fit_scaling_failed` both NULL.
- [x] AC3: Each of two matrices whose accuracy certificate fails — one on each
      of the certificate's two failure routes — is refused at both surfaces
      under one shared literal distinct from `"indefinite"` and `"singular"`,
      both surfaces reporting that same literal for it (M89's nestedness
      contract, holding across the new literal). The graded route is the
      committed counterexample at
      `tests/testthat/fixtures/rb18-counterexample-b.rds`, priced directly by
      `axes_corrected_se()` and `axes_scaling_factor()` because at p = 3 it
      cannot ride the `axes_fitted_cov()` seam — `axes_reliability()` refuses
      fewer than four scales. The sentinel route is a p = 24
      near-duplicate-item matrix injected at that seam, where
      `axes_reliability()` itself reports the literal in
      `details$se_correction_failed` and `details$fit_scaling_failed`.
- [x] AC4: The `"indefinite"` and `"singular"` limbs still refuse with their
      own literals at both surfaces, on both sides of the partition boundary,
      at two values of p and two spectral forms.
- [x] AC5: The span from the `WHY THE LIMB EXISTS AT ALL` heading to the end of
      the derivation block in `R/axes_corrected_se.R` is rewritten, and no line
      within that span states that the package has no shipped means of
      certifying a computed answer past the floor, or that the reopening
      trigger is only partly met.
- [x] AC6: The new refusal path is mutation-proved: with the certificate's
      comparison inverted, and separately with its threshold removed, an AC2 or
      AC3 assertion reddens in each case.
- [x] AC7: `NEWS.md` records the change in what the ill-conditioning refusal
      does; `Rscript -e 'devtools::document()'` produces no diff;
      `Rscript -e 'devtools::test()'` and
      `Rscript -e 'devtools::check(args = "--no-manual")'` clean.

## Coverage

- AC1 → T1
- AC2 → T1, T3
- AC3 → T1, T2, T3
- AC4 → T4
- AC5 → T6
- AC6 → T5
- AC7 → T7

## Tasks

- [x] T1: Wire M108's certificate into both surfaces, keeping the floor as the
      indefinite/singular partition only.
- [x] T2: Add the certificate-failure literal to the refusal vocabulary and to
      the enumeration test at `tests/testthat/test-axes-corrected-se.R:1179`.
- [x] T3: Write the AC2/AC3 tests at the `axes_fitted_cov()` seam, using the
      builders in `tests/testthat/helper-m106-degeneracy.R`.
- [x] T4: Re-fence the partition (AC4). The pinned tests at
      `test-axes-scaled-fit.R:1906` and `:1944` assert the criterion function's
      own return, which T1 leaves intact; assert the surfaces' behaviour beside
      them rather than editing them.
- [x] T5: Mutation-prove the new path (AC6), recording which assertion each
      mutant reddens.
- [x] T6: Rewrite the `WHY THE LIMB EXISTS AT ALL` span. Two claims in it are
      falsified by this milestone and by D-050: "the package has no shipped
      means of certifying the number to delta_star" (`:574`) and "its first
      trigger is partly met on the record already" (`:581`), the latter already
      stale since D-050 recorded that trigger met in full.
- [x] T7: NEWS entry, document, verify and check.
- [x] T8: Correct the stale `6.5e-6` corner figure in the accuracy-target
      block (`R/axes_corrected_se.R`), which derives from a = 0.046 while the
      block states a = 0.045 (0.1*0.045/sqrt(5e5) = 6.36e-6). Comment-only;
      routed here by the M110 review, outside its own diff.

## Work log

- 2026-08-24: created by /milestone-plan.
- 2026-08-24: plan chose keeping the criterion's floor as an indefinite/singular partition over removing it outright, because removing it fails the two pinned partition tests and the partition is independent of the accuracy question the certificate answers; falsified by a matrix the partition misclassifies once the floor no longer gates refusal.
- 2026-08-24: plan chose asserting the five reachable cases at the `axes_fitted_cov()` seam over routing them through `axes_reliability()`, because the oracle enumerates matrices while the exported path refits and would price a different matrix than the one enumerated; falsified by a fit whose refitted Sigma-hat lands on the other side of the certificate from the matrix it was built from.
- 2026-08-24: criteria audit findings for this milestone are recorded in M108's work log, which covers the joint audit run.
- 2026-08-24: RR21 (M108's mechanism review) routes four items here: re-key the `"ill_conditioned"` refusal to the certificate and carry the fit's own estimate in the warning (rec 4, the shape D-051's consequences state); surfacing the estimate on computed fits as well as refused ones (rec 5, a design call); emitting the exact oracle's values as hex double pairs so a reference route can be pinned below double resolution (B3); and extending the certificate's worst-component maximum to the FIML ratio vector, which the same pricing call already computes (B4). None is adopted by M108.
- 2026-08-24: M108's review routes its remainder here, headed by the packaged bracket asserting on macOS only (all six cases skip on ubuntu and windows) with nothing failing when that domain empties; thirteen lower-ranked findings ride with it. Text and disposition for each are in the M108 archive's Review; the ROADMAP degeneracy candidate row carries the promotion clause.
- 2026-08-24: implement started; branch `m111-certificate-refusal-region` cut from master.
- 2026-08-24: gate chose one shared refusal predicate at both surfaces (the worse of the certificate's two estimates against delta_star) over per-surface fields, the literal `"uncertified"`, the certificate's estimate carried in the refusal warning, and folding the M110-review stale-figure correction in as T8.
- 2026-08-24: AC3 amended (mini gate). Its "injected at the same seam" step is unsatisfiable: the p = 3 fixture carries three item names and `axes_reliability()` refuses fewer than four scales, so the seam's realignment errors `subscript out of bounds`. Amended text prices the fixture directly and adds a p = 24 near-duplicate matrix at the seam so the exported surface is pinned.
- 2026-08-24: criteria audit of the amended AC3 ran in full mode (user-facing tier), two fresh-context [O] readers, neither an author of the wording. Findings disposed: the first draft's `naive_reason` clause was unfalsifiable and misattributed (dropped); the second draft's opening clause quantified over all certificate failures while the certificate gates only the ill-conditioned arm (narrowed to two enumerated matrices); the exported surface was named only as coverage prose (rewritten as the promise it carries).
- 2026-08-24: T1 wired the certificate in at both surfaces through one shared decision helper, `axes_degeneracy_refusal()`; the floor now selects which fits are checked, and only a fit whose worse certificate estimate exceeds delta_star refuses, as `"uncertified"`.
- 2026-08-24: T2 added `"uncertified"` to the refusal vocabulary and to the BC5 enumeration test, which now also reads the list-field shape the shared decision helper returns its literal in.
- 2026-08-24: T3 added `tests/testthat/test-axes-certificate-refusal.R` (AC2, AC3). AC2's five geometries are fit on well-conditioned siblings of the same design and injected at the seam: lavaan does not converge on the p = 8 kappa 1e5 anchor itself. Two of the five (a5, b9b) were refused before this milestone and now compute.
- 2026-08-24: T1/T2/T3 moved 28 assertions in four existing test files onto the new behaviour. Two classes: eleven sites where the surfaces still refuse under the new literal, and seventeen where the fit now computes -- the M106 floor-bracket tests (AC4 case 2 at p = 8, case 3 at p = 24, AC5 radius 2) and the two diagnostic-hint controls, whose refusing exemplars moved down to kappa 2.0e8 and 7.2e8 where the certificate's reference route fails.
- 2026-08-24: T4 fenced the two limbs the certificate is never asked about at the SURFACES (AC4), two p and two spectral forms (rotated planted eigenvalue, rank-one projector), both sides of the M90 partition boundary plus the non-finite route.
- 2026-08-24: T5 mutation-proved the new path (AC6). Comparison inverted (`<=` to `>=`): 20 assertions redden, including AC2's `se_correction_failed`/`fit_scaling_failed` NULL checks at a5 and b9b and AC3's `"uncertified"` checks at both routes. Threshold removed (the certified branch taken unconditionally): 10 redden, all in AC3 -- the sentinel route's two failure fields and the graded route's six.
- 2026-08-24: T6 rewrote the `WHY THE LIMB EXISTS AT ALL` span (AC5); T8 corrected the stale `6.5e-6` corner to 6.36e-6 and pinned every figure in that paragraph to its own 0.1*a/sqrt(n) derivation.
- 2026-08-24: T7 landed the NEWS and exported-doc rewrites and closed the verify slot: `devtools::document()` no diff and no unresolved-link warning, `devtools::test()` 8755 pass / 0 fail / 1 pre-existing skip, `devtools::check(args = "--no-manual")` 0 errors, 0 warnings, 0 notes. Status to review.
- 2026-08-24: review gate triaged 17 findings; eight documentation, comment and test-assertion defects fixed on the branch (F5-F11, F14), five routed to the ROADMAP degeneracy row as follow-ups (F1+F2, F3, F4, F12, F13), three rejected with reasons in the Review section.

## Decisions

## Review

Evidence gathered 2026-08-24 on branch `m111-certificate-refusal-region` at
8e6ecd01, PR #142, master at 74fde7d2 (branch already contained it; no merge
needed). Full suite: FAIL 0, WARN 5, SKIP 1 (pre-existing, a fixture generated
under another lavaan version), PASS 8755. The new file
`test-axes-certificate-refusal.R` runs 112 assertions with 0 skips.

### Acceptance criteria

- **AC1 — met.** `axes_sigma_degenerate()` is unchanged and still returns the
  three-way partition; its only refusing callers are gone. Both surfaces now
  call `axes_degeneracy_refusal()` (`R/axes_corrected_se.R:340`,
  `R/axes_scaled_fit.R:241`), which forwards `"indefinite"`/`"singular"`
  untouched and consults the certificate on `"ill_conditioned"`. The one
  surviving bare call is the raw-arm `naive_reason` line
  (`R/axes_corrected_se.R:363`), which refuses nothing a user is shown. Proved
  behaviourally by AC2's a5 and b9b, both below the floor and both computing.
- **AC2 — met.** `test-axes-certificate-refusal.R:47` runs all five geometries
  (a4, a5, c4, b9a, b9b) through `axes_reliability()` with the oracle's matrix
  injected at `axes_fitted_cov()`, and asserts the case list has length 5 so
  the domain cannot silently empty. Each case: `se_correction_failed` and
  `fit_scaling_failed` both NULL, no refusal warning, all non-epsilon
  component SEs finite, `scaling_factor[["model"]]` finite and positive,
  `fit$chisq` finite. The two cases the milestone moved are named rather than
  counted (`expect_setequal(refused_before, c("a5", "b9b"))`). Green in the
  full run and in a targeted run of the file.
- **AC3 — met.** Two matrices, one per failure route, both refused at both
  surfaces under `"uncertified"`, distinct from `"indefinite"` and
  `"singular"`. Sentinel route (`:113`): a p = 24 near-duplicate matrix at the
  `axes_fitted_cov()` seam, `details$se_correction_failed` and
  `details$fit_scaling_failed` both `"uncertified"`, two warnings naming the
  literal and two naming the estimate, everything the refusal covers NA
  together while `df` and `srmr` report. Graded route (`:172`): the committed
  `rb18-counterexample-b.rds` priced directly by `axes_corrected_se()` and
  `axes_scaling_factor()`, both returning `"uncertified"`, with the fixture's
  `"ill_conditioned"` precondition asserted first. The route split itself is
  measured but not asserted by the test — see finding F4.
- **AC4 — met.** `test-axes-certificate-refusal.R:214` loops p = 4 and p = 8
  across two spectral forms (a planted eigenvalue in a rotated basis, and the
  identity minus a rank-one projector). Past the partition boundary both
  surfaces return `"indefinite"`; inside the band, the other side of the same
  boundary, neither returns `"indefinite"` and the two agree. A non-finite
  matrix returns `"singular"` at both surfaces at each p. The criterion's own
  near-threshold probes in `test-axes-scaled-fit.R` are untouched.
- **AC5 — met.** The span is rewritten: the heading is now `WHAT THE FLOOR NOW
  DOES, AND WHY IT NO LONGER REFUSES (M111)` (`R/axes_corrected_se.R:622`),
  running to the constant definitions. Grepped over the rewritten span:
  no line says the reopening trigger is partly met, and no line asserts in the
  present tense that the package has no shipped means of certifying past the
  floor. The phrase survives only inside an explicitly past-tense report of
  what the two earlier escalations rested on ("both kept it on ONE ground:
  past the floor there **was** no shipped means"), immediately followed by
  "That ground is gone." Recorded so the reading is visible rather than
  assumed.
- **AC6 — met, re-run at review.** Both mutants applied to
  `axes_degeneracy_refusal()` and the new file re-run against each, then the
  source restored and verified clean. Comparison inverted (`<=` to `>=`):
  FAIL 24 / PASS 88, the first failure AC2's "no refusal warning" check.
  Threshold removed (the certified branch taken unconditionally): FAIL 10 /
  PASS 102, all in AC3. Unmutated: FAIL 0 / PASS 112.
- **AC7 — met.** `NEWS.md` rewrites the degeneracy entry to the two-step
  design, names `"uncertified"` and its warning's estimate, and carries the
  reason-string change under the breaking-change entry.
  `devtools::document()` at pinned cli width: no diff, zero `resolve link`
  lines. `devtools::test()`: FAIL 0 / PASS 8755. `devtools::check()` (the full
  form, manual included, stricter than the criterion's `--no-manual`):
  0 errors, 0 warnings, 0 notes, 7m46s.

### Consistency gate

Universal: `cairn_validate.py` exit 0, all 16 checks PASS including
`coverage complete` and `scaffold present`; 47 advisory work-log-format warns,
every one a pre-existing multi-line entry in M7. No `DESIGN.md` principle
changed, so `cairn_impact.py` does not apply. Toolchain (`r-package`
consistency-gate slot): `document()` no diff and no unresolved-link warning;
generated files untouched; README.md in sync; `pkgdown::check_pkgdown()` no
problems; NEWS entry present; no new top-level files; full `check()` clean;
master watches both green on the newest push run reaching a verdict
(R-CMD-check and test-coverage, run 32798426467/32798426425 at d666ed14);
`check-master-red-alert.R`, `master-red-alert-dryrun.R` and
`check-branch-protection.R` all exit clean.

### Independent review

Three fresh-context reviewers, distinct evidence bases, none an author of the
implementation: [O] diff-bug over the full diff against the criteria, DESIGN
and DECISIONS; [S] blame-history over the modified and deleted lines; [S]
prior-review over `milestones/archive/` (the GitHub PR-comment probe returned
empty, so the thread walk was skipped). The blame lens found no assertion whose
guarantee was weakened rather than relabeled, and confirmed the three
constants untouched. Seventeen findings, ranked and triaged below.

### Findings and triage

Ranked as the reviewers ranked them. Dispositions recorded at the gate.

- **F1 [O] The FIML path's reported SE is composed from a quantity the
  certificate never prices.** `axes_corrected_se.R:376` builds
  `fiml_ratio = std$corrected / std$naive`, both at `cov2cor(Sigma-hat)`, and
  `axes_reliability.R:1824` reports `se_uncorrected * fiml_ratio` whenever
  `missing == "fiml"`. The certificate replays the corrected arm only, on the
  stated ground that the naive arm is never user-reported — false for the
  cov2cor naive, which is that ratio's denominator. Before M111 the floor
  refused every such fit; now the gate releases them on an estimate covering
  neither the ratio nor its denominator, while the exported doc promises the
  check estimates the error the reported numbers carry. The reviewer replayed
  the naive forms in double-double: at family-A kappa 1e5/1e6/1e7 the naive
  arm's error is 1.1e-11/5.8e-11/4.1e-10 against certified
  3.0e-11/1.2e-10/4.5e-10 — the same decade, so measured risk is low, but
  unpriced and untested. RR21 B4 (extend the certificate to the FIML ratio)
  was routed here and neither adopted nor declined in writing.
- **F2 [prior-review] The diff adds a second, near-duplicate copy of a comment
  M108's review already flagged as false, instead of fixing the original.**
  `axes_certificate.R:342` (untouched here) claims only the corrected arm is
  replayed because the naive arm is never user-reported; both M108 review
  rounds flagged this as false and it stood at merge. M111 adds
  `axes_corrected_se.R:752` in nearly identical language about a *different*
  naive quantity (the raw-Sigma-hat arm, genuinely unreported), which
  entrenches the conflation rather than isolating the false claim for its fix.
  Same root as F1.
- **F3 [O] The uncertified naive arm can break the shared-literal nestedness
  contract in the region M111 newly admits.** `axes_se_pricing()`
  (`axes_corrected_se.R:239`) returns `"indefinite"` if any entry of the naive
  or corrected SE rows is non-finite; the certificate checks positivity of the
  corrected double-double forms only. A certified below-floor fit whose
  `v$naive` cancels negative would refuse `"indefinite"` at the SE surface
  while the scaling surface, which prices no naive arm, computes. The guard's
  own comment records it has never fired across 3822 indefinite matrices and
  no reviewer constructed an instance; M111 is what first lets ill-conditioned
  matrices reach it end-to-end.
- **F4 [O] Nothing tests the shared `max(cert$se, cert$cval)` predicate against
  the per-surface alternative it was chosen over.** Every matrix in the suite
  has both estimates on the same side of the target (fixture 0.335 and 48.9;
  sentinels 1 and 1; computing cases ~1e-11 and ~1e-12), so replacing the max
  with each surface's own field would pass the whole suite while
  reintroducing the split the design exists to prevent. The two estimates are
  two decades apart on real inputs, so a straddling matrix is not
  hypothetical.
- **F5 [O] AC3's "one on each of the certificate's two failure routes" is
  asserted nowhere.** The tests assert the literal and that the phrase
  "estimated relative error" appears, never its value. Both route claims hold
  today (p = 24 gives 1 and 1; the fixture 0.3355 and 48.90, matching the
  comments), but a drift of the p = 24 case to a graded estimate would
  collapse the AC to one route with every assertion still green.
- **F6 [O] `details$naive_reason` can still return `"ill_conditioned"`, which
  the rewritten exported docs no longer define.** `axes_corrected_se.R:363`
  still calls the criterion directly on the raw matrix; the Rd now enumerates
  the vocabulary as `"indefinite"`/`"singular"`/`"uncertified"` and then says
  `naive_reason` carries the same vocabulary. Introduced by this diff: before
  it, the two vocabularies genuinely matched.
- **F7 [O] The two internal reason enumerations in `axes_reliability.R` are
  stale.** Lines 1962 and 2005 still attribute the field to the criterion's
  M90 partition and omit `"uncertified"`; line 2011 still says "All eight are
  reachable" where the list is now nine. M71 audited these against the source,
  so they are load-bearing for the next auditor.
- **F8 [O] A doc and NEWS generalization not supported by what was measured.**
  "Most fits below the floor do compute: over the reachable geometries
  measured, their estimated errors run around `1e-11`"
  (`axes_reliability.R:724`, NEWS). Exactly two of the five reachable
  geometries sit below the floor, and the one matrix drawn from a real refusal
  estimates 3.4e-1. "Most" is a population claim from a handful of constructed
  matrices.
- **F9 [O] `"singular"` is described as non-finite entries only**
  (`axes_reliability.R:748` and the Rd). The literal also fires from the
  nonpositive-diagonal door on a perfectly finite matrix — verified at
  `axes_corrected_se.R:292` and `axes_scaled_fit.R:222`. The sentence is new
  in this diff; the text it replaced made no such claim.
- **F10 [O] Stale comment at `test-axes-scaled-fit.R:2524`:** "kappa 7.2e5
  against a floor of 4.33e4: strictly above, refused" — the assertions eight
  lines below now expect it to compute. The rest of the case was updated.
- **F11 [O] `axes_degeneracy_hint()`'s header still calls itself "the
  actionable half of an `"ill_conditioned"` refusal"**
  (`axes_corrected_se.R:790`) and its scope block reasons about a literal that
  no longer refuses at either surface. The precondition it relies on still
  holds, so this is wording, not behaviour.
- **F12 [O] The AC8 nestedness gate still lists `"ill_conditioned"`**
  (`test-axes-scaled-fit.R:1409`). At the scaling surface that literal now
  reaches only the `cval <= 0` cancellation door, which is outside the
  nestedness contract, so the test demands agreement on a literal the SE
  surface can no longer produce. Unreachable today, so it will not fail; it is
  the wrong set.
- **F13 [O] The certificate call is the only refusal path here with no error
  guard.** `axes_corrected_se.R:761` calls it bare on a path contractually
  obliged to return a named-reason NA rather than error, where the sibling
  `solve()` calls are wrapped. No throw could be constructed, but a raised
  condition anywhere in the double-double arithmetic would reach the user as
  an error instead of a refusal.
- **F14 [O] The certificate runs twice per checked fit** — once per surface,
  on the same matrix with the same derivative set; measured 0.20 s each at
  p = 24, so roughly 0.4 s added to a below-floor `axes_reliability()` call.
  The comment's "16-108x replay" understates it by a factor of two and there
  is no memoization seam.
- **F15 [blame] The shared predicate means a surface's warning can carry the
  other quantity's estimate.** With the SE estimate fine and the cval estimate
  bad, the SE surface still refuses and its "estimated relative error" text
  prints the cval number. Deliberate per the gate decision and fail-closed,
  but a change from each warning describing its own surface's arithmetic.
- **F16 [O] AC4's inside-band assertions cannot fail on the new path** — they
  assert only that the literal is not `"indefinite"` and that the two surfaces
  agree, both satisfied whether the fit computes or refuses. Sound as a
  partition fence; contributes nothing to the mutation proof.
- **F17 [O] The milestone's `## Decisions` section is empty** while the work
  log records four gate decisions and the change alters exported behaviour.

**Dispositions** (maintainer's triage at the gate, 2026-08-24: "fix the
documentation and comment defects, then merge"). Fixed on the branch before
approval: F5 (both AC3 cases now assert which route they take — the sentinel's
`1` and the graded case's `49` — so the two cannot silently collapse onto one
route), F6 (`naive_reason`'s enumeration names `"ill_conditioned"` and says
why the raw arm keeps it), F7 (both internal enumerations name `"uncertified"`
and the scaling list's count moves 8 to 9, with the surviving
`"ill_conditioned"` attributed to the cval cancellation door), F8 (the
population claim narrowed to the geometries actually measured), F9
(`"singular"` also names the nonpositive-diagonal door, verified at
`axes_corrected_se.R:292` and `axes_scaled_fit.R:222`), F10 (the case-3 header
comment now says the case computes), F11 (the hint's header re-keyed to the
refusal it actually serves, precondition noted unchanged), F14 (the cost
comment records the two calls per checked fit). Re-verified after the fixes:
`document()` no diff and no link warnings, `test()` FAIL 0 / PASS 8758,
`check()` 0/0/0.

Follow-up, absorbed into the ROADMAP's degeneracy candidate row: F1 and F2 as
one item (the cov2cor naive arm is a reported number's denominator, uncertified
and misdescribed at two sites; RR21 B4 is the remedy), F3 (the naive-arm
`"indefinite"` backstop as a nestedness hazard M111 first makes reachable), F4
(no test discriminates the shared predicate from the per-surface alternative),
F12 (the AC8 nestedness gate's literal set), F13 (the unguarded certificate
call on a refusal path).

Rejected: F15 — the shared predicate reporting the worse estimate at both
surfaces is the gate decision recorded in the work log, and the fail-closed
direction is the intended one (GP2); an intentional change the plan called for.
F16 — the inside-band assertions are a partition fence and were never meant to
carry the mutation proof, which AC6's own mutants do. F17 — the four gate
decisions are in the work log, which is where a milestone-local decision that
supersedes nothing belongs; D-051 pre-authorized the re-keying, so no D-entry
is owed.
