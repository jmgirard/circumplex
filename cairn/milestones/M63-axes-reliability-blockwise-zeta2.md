# M63: Blockwise instruments for `axes_reliability()` — the ζ2 component

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m63-axes-reliability-blockwise-zeta2` · https://github.com/jmgirard/circumplex/pull/89

## Goal

Estimate Strack's block-specificity component ζ2 so a blockwise instrument's
axes variance ξ1 is recovered unbiased instead of carrying the documented
"treat as approximate" caveat.

## Scope

**In:** A `blocks =` argument on `axes_reliability()` carrying a per-item block
map; block-specificity latents sharing one `zeta2` label in `axes_syntax()`; a
fourth same-block design column in `axes_ols_shadow()`; ζ2 in
`axes_population_cor()`/`axes_simulate()` and the boundary guard; ζ2 dropped
with a `details` flag when blocks coincide with scales; the synthetic recovery
+ omitted-ζ2 bias + cross-engine oracle; the blocked/type-d Table 3 rows banked
into `strack2013.md` as formula-layer anchors; every surface enumerating the
component set updated.

**Out:** Carrying blocks on the `circumplex_instrument` class → a candidate row
(no bundled instrument records block membership, so the field would ship empty).
Quasi-circumplex/unequal spacing → stays refused on RR09 §4's standing holding.
FIML on items → the surviving candidate row from D-031. An end-to-end fixture
built from Table 3's blocked rows → impossible, not deferred: the paper prints
components and reliability but no correlation matrix, so those rows anchor the
formula layer only (`strack2013.md:160-170` reached the same conclusion for the
type-f rows).

## Acceptance criteria

- [x] AC1 `blocks =` accepts a per-item block map on the raw-data and `cormat`
      paths, is validated with the house `stopifnot()`/`is_*()` idiom, and is
      refused with a named error on a length mismatch, an unmappable item, or a
      non-finite/missing block label.
- [x] AC2 ζ2 is fitted and reported when identified; when every block coincides
      with a scale it is dropped from the emitted syntax and flagged in
      `details$zeta2_fitted`, with the emitted model and the reported component
      set never disagreeing (the `axes_fits_zeta1()` contract, extended).
- [x] AC3 On a synthetic population with known ζ2 > 0, the fit recovers ξ1 and
      ζ2 within a stated **absolute** bound (stated absolutely, per the M61
      relative-tolerance trap), set from the discrimination required rather than
      from one machine's printed value (M59).
- [x] AC4 The omitted-ζ2 bias in ξ1 is characterized as a **conditional on block
      geometry**, not a blanket claim, with both branches asserted against the
      exact population matrix and agreeing with a closed-form omitted-variable-bias
      prediction: under an **angle-balanced** layout (each block drawing one item
      per scale) ξ1 is unbiased — asserted at < 1e-4, a bound set from the 240×
      separation against the clustered branch rather than from one machine's
      printed value (M59), with the orthogonality derivation recorded — while
      under an **angle-clustered** layout (blocks
      spanning contiguous arcs) ξ1 carries ≥ 10% relative bias. ξ2 is never
      deflated — inflated under most layouts, exactly unchanged under a few.
- [x] AC5 lavaan, OpenMx, and the OLS shadow agree on ξ1/ξ2/ζ1/ζ2 within stated
      bounds on the population matrix, and all six banked blocked/type-d Table 3
      rows (CSIV S7, TRC-g S10, TRC-t S11, OCAI S15 Self/Other/Meta) reproduce
      their printed reliability by Spearman–Brown within ±.01 and their printed
      SEm within ±.02. The five-component sum guard binds only the four rows the
      source prints self-consistently; CSIV (102.9) and OCAI S15 Meta (100.6) are
      pre-existing source defects, pinned with their printed sums rather than
      swept, per RR10's ruling for the IIP S6 erratum.
- [x] AC6 No NaN, negative, or infinite SEm on any ζ2 path: a negative ζ2 trips
      the boundary flag, and the never-NaN block covers the new component.
- [x] AC7 Every surface that enumerates the component set names ζ2 — roxygen
      (including the replaced `# Blockwise instruments` section), `print()`,
      `summary()`, the `@return` component-row count, the vignette, and NEWS —
      verified by grepping for the enumeration, not the changed lines (M56/M62).
- [x] AC8 `devtools::test()` clean and `devtools::check()` 0/0/0, with the PDF
      manual actually built (`checking PDF version of manual` present in the
      log, per the M7/M57 lesson) since roxygen changes here.

## Coverage

- AC1 → T1
- AC2 → T2, T3
- AC3 → T5, T7
- AC4 → T7
- AC5 → T4, T7, T8
- AC6 → T6
- AC7 → T9
- AC8 → T9

## Tasks

- [x] T1 `blocks =` argument on `axes_reliability()` (`R/axes_reliability.R:603`)
      + resolution to a per-item block index beside `axes_resolve_map()`
      (`:394`); validation and error tests first.
- [x] T2 `axes_fits_zeta2()` identification predicate mirroring
      `axes_fits_zeta1()` (`:123`). Open design question for implement: whether
      the predicate is structural ("some block spans ≥2 scales") or a rank check
      on the OLS design — the structural form can miss a block map collinear
      with the cosine column. Settle it against a numeric identifiability probe.
- [x] T3 `axes_syntax()` emits `BS<m>` latents with a shared `zeta2` label and
      `start()` seeding (`:146`), dropped whole when T2's predicate is false.
- [x] T4 `axes_ols_shadow()` fourth same-block design column (`:228`), with the
      rank-drop path that already handles the ζ1-dropped case extended.
- [x] T5 `axes_population_cor()` + `axes_simulate()` carry ζ2 and a block map
      (`:328`, `:342`). Build the fixture from ONE population (M61 lesson (h)).
- [x] T6 `axes_is_boundary()` negative-ζ2 disjunct (`:314`) + the never-NaN
      block; assert the condition on the unmocked path (M62 lesson (i)).
- [x] T7 Oracle: known-ζ2 recovery, the omitted-ζ2 bias demonstration, and
      lavaan/OpenMx/OLS-shadow agreement. Prove each new guard by mutation, and
      record any null mutation with why the model is invariant (M60 lesson).
- [x] T8 Bank the blocked type-a rows (CSIV S7, TRC-g S10, TRC-t S11) and the
      OCAI type-d rows into `cairn/references/strack2013.md`, two channels on
      p. 7, extraction status dated and unbolded (M45 format trap).
- [x] T9 Docs: replace `R/axes_reliability.R:533-543`, update `print()`/
      `summary()`/`@return`/vignette/NEWS, `devtools::document()`, then grep
      every enumerating surface and build the PDF manual.

## Work log

- 2026-07-26: gated AC amendment at the merge gate, at Jeff's direction — status review→in-progress→review. AC5's "sum to 100% across five components" was false for two of the six banked rows, so it now binds the sum guard to the four self-consistent rows and requires CSIV (102.9) and OCAI S15 Meta (100.6) pinned with their printed sums — the text now describes what the test asserts, rather than the test being read charitably against it. AC4's closing clause was amended in the same pass because review finding F3 falsified it: it claimed ξ2 inflation "does hold unconditionally", now "never deflated — inflated under most layouts, exactly unchanged under a few". Both re-verified against fresh evidence before re-ticking; suite 903 passed / 0 failed.
- 2026-07-26: status in-progress→review. All 9 tasks done. Full `devtools::check(manual = TRUE)` is 0/0/0 with BOTH steps that habitually skip actually running — `checking tests ... OK` (431s, not SKIPPED) and `checking PDF version of manual ... OK` — which is the M7/M57 lesson's requirement, since this milestone changed roxygen. `devtools::test()` 4032+ passes, 0 failures. Branch is 8 commits over 8 files.
- 2026-07-26: T9 done — the caveat this milestone existed to remove is gone from every surface, replaced by M63-D2's conditional in the roxygen, the vignette, and NEWS; `@return` now names the five-row case and `zeta2_fitted`; `example_data.R` says explicitly that the bundled dataset's fifth component is zero, since it claimed a "five-component population" while enumerating four. The enumeration sweep grepped for the OLD assertion's keywords rather than the changed lines (M56/M62): "as approximate", "deflating the share", "no such component" now appear on no surface. The AC7 doc guard was FALSE COVERAGE on first writing and measured to be so — `expect_match(rd, "blocks")` passed with `@param blocks` deleted, because the prose says "administered in blocks" (the M39/M40 trap); re-pinned to `\item{blocks}{`, which only the \arguments entry emits, and both it and the reverted-caveat mutation now redden. The guard also reads man/ in the dev tree and Rd_db() once installed, the dual-source pattern test-rd-latex-safe.R uses, because a man/-only guard skips under R CMD check and an Rd_db()-only guard errors under load_all().
- 2026-07-26: T8 done — the six Table 3 rows carrying a col-8 value (CSIV S7, TRC-g S10, TRC-t S11, OCAI S15 Self/Other/Meta) banked in `strack2013.md`, two channels on p. 7 (text layer + 300-dpi page image) agreeing on every value, including the source's own `(1.2))` misprint. All six reproduce printed Rel by Spearman–Brown within .01 and printed SEm within .02. Deviation from AC5 as written, following existing repo precedent rather than a new gate: AC5 says the rows "sum to 100%% across five components", but two do not — CSIV 102.9 and OCAI Meta 100.6, both defects this page already recorded — so the test sweeps the four self-consistent rows and PINS the two with their printed sums, which is RR10's standing ruling for the IIP S6 erratum. Also recorded on the page: these rows can never be an end-to-end fixture, since no correlation matrix is published and reliability never touches ζ2.
- 2026-07-26: T7 done, and it changed a plan claim. AC4 as planned asserted that omitting ζ2 biases ξ1; measurement showed that is FALSE for the canonical crossed layout — ξ1 is provably unbiased there because same-block is orthogonal to cos(θi−θj), confirmed three ways (OLS shadow at -7.5e-16, closed-form omitted-variable bias at 9.9e-17, lavaan CFA at 2.9e-8). It is true for angle-clustered layouts (+.024 on a truth of .20, +12%). Jeff approved amending AC4 to the conditional at a mini gate; M63-D2 records the finding and the shipped roxygen caveat is now known to be wrong in two of three parts, which T9 must rewrite. Minor follow-on amendment: AC4's stated bound moved 1e-8 → 1e-4 after both drafted tolerances failed on the machine that wrote them — recalibrated from the 240x separation against the clustered branch rather than from a printed value (M59). AC3 recovery within 1e-4 on the exact population; AC5 lavaan/OpenMx agree to 1e-3 and the OLS shadow to .02, OpenMx actually running (0 skips). Mutation: BS latents loading 0.9 instead of 1 reddens 5 tests. `devtools::test()` 0 failures / 4032 passes.
- 2026-07-26: T4/T5/T6 done, and the estimator is now end-to-end — `axes_reliability()` takes `blocks =`, fits ζ2 when the map identifies it, and reports it as a fifth component row with `details$zeta2_fitted`. Correcting my own record: T1's tick covered the resolver and its validation only; the `axes_reliability()` signature and wiring it names landed here, and T4's shadow column had already landed inside T2. On a 3000-row blockwise draw ξ1 and ζ2 both recover within .02 of truth, and an unblocked call on the same data reproduces the pre-M63 result to 1e-10. Mutations: negative-ζ2 boundary disjunct, the population's ζ2 term, and crossed-vs-scale-aligned blocks all redden (2, 6, 7+2 respectively). One NULL mutation recorded rather than chased (M60 lesson): reading ζ2 off lavaan's parameter table instead of the design predicate cannot redden, because `BS1` is in the table exactly when the emitter wrote it and the emitter reads the same predicate — they are equal by construction, and only a two-point mutation could separate them. `devtools::test()` 0 failures / 4011 passes.
- 2026-07-26: T3 done — `axes_syntax()` emits one `BS<m>` latent per block sharing a single `zeta2` label, dropped whole when the design says unidentified; `item_block` threaded through `axes_fit()`/`axes_fit_cormat()` (minor task refinement: the fit-seam plumbing landed here so the emitter is reachable, rather than waiting for T6). Two defects caught by the tests before commit: the drop comment named the `zeta2` token, which would have silently defeated a "no such component anywhere" assertion the way M61's comment deliberately avoids (reworded, with the reason recorded in the code); and a test regex pinned digits `fmt()` never prints, since .06 expands to 0.0599999… — reseeded on a dyadic value. Three mutations red, including block latents loading scale items instead of block items. `devtools::test()` 0 failures / 3973 passes.
- 2026-07-26: T2 done — the component set now lives in one place, `axes_design()`, which the OLS shadow and `axes_fits_zeta2()` both read; ζ2 joins only when the same-block column raises the design's rank. Task refined during work (minor): the shadow's move onto the shared design landed here rather than in T4, since the predicate needed it first; T4 keeps the shadow's zeta2 return. The gate's rank-check answer is now evidenced, not just asserted — a block map pairing OPPOSITE scales spans two scales each (so the rejected structural rule calls it identified) yet same-block equals -cos exactly and adds no rank; substituting the structural rule into the code turns 4 tests red. M61's shadow tests pass untouched as the refactor's fence. `devtools::test()` 0 failures / 3956 passes.
- 2026-07-26: T1 done — `axes_resolve_blocks()` maps a list of per-block item vectors onto the `unlist(item_cols)` order, or NULL for the pre-M63 path. Implement gate settled two open choices: `blocks` is a list of item vectors mirroring `items` (a flat label vector was refused as a silent-misalignment channel, M25 family), and identification will be a rank check on the OLS design rather than a structural rule. M63-D1 records the partition contract. Four guards proven by mutation (orphan, duplicate, index alignment, empty block); `devtools::test()` 0 failures / 3936 passes.
- 2026-07-26: created by /milestone-plan. Four gate answers from Jeff: into v2.0.0 as a narrow D-001 supersession (D-032); `blocks =` argument only, no Instrument field; unidentified ζ2 drops and flags per the M61 precedent; oracle bar is synthetic recovery + omitted-ζ2 bias + cross-engine. Investigation corrected the candidate row's premise — Table 3's blocked rows can never be an end-to-end fixture (no published correlation matrix), so they anchor the formula layer only and the estimator's real oracle is synthetic.

## Decisions

- **M63-D1 (2026-07-26): `blocks` must partition the items.** Every item belongs
  to exactly one block; an item in no block and an item in two are both refused,
  naming the offending item. A blockwise instrument administers every item in
  some block, so a partial map is one the model has no reading for — the
  alternatives were to invent a catch-all block for the remainder or to fit
  block latents over part of the item set, and both answer a question the caller
  never asked. Refusing keeps `blocks` the exact shape-mate of `items`, which
  partitions the items into scales the same way. Ruled at implement, not plan:
  the plan settled the argument's *shape* (list of item vectors, M63 gate) and
  left its *completeness* open.

- **M63-D2 (2026-07-26): the omitted-ζ2 bias in ξ1 is conditional on block
  geometry, and the shipped caveat overstates it.** Measured three independent
  ways on the exact population — the OLS shadow, a closed-form omitted-variable-bias
  computation, and the lavaan CFA, agreeing to five decimals — omitting ζ2 leaves
  ξ1 EXACTLY unbiased when each block draws one item per scale, because the
  same-block indicator is then orthogonal to cos(θi − θj): within-block pairs are
  all cross-scale and span every scale pair uniformly, so the auxiliary regression
  of same-block on the design has a zero cosine coefficient (measured 9.9e-17).
  Under angle-clustered blocks that coefficient is 0.398 and ξ1 carries +.024 on a
  truth of .20 (+12%). ξ2 is inflated under every geometry tested; ζ1's bias
  changes sign with geometry. The roxygen caveat at `R/axes_reliability.R:533-543`
  therefore ships an unconditional claim — block variance inflating general and
  scale-specificity "and, in turn, deflating the share attributed to the axes" —
  that is wrong in two of three parts for the canonical crossed layout. T9
  rewrites it as the conditional; AC4 is amended to assert both branches rather
  than a single direction. Same family as M23: write a bias claim as a conditional
  with the condition derived.

## Review

2026-07-26, PR #89. All evidence re-run fresh at review, never recalled.

**Criterion evidence** (20 M63 tests, 120 assertions, 0 failed / 0 errored /
0 skipped; whole file 886 assertions green):

- AC1 — `M63 T1: axes_resolve_blocks() maps blocks onto the item order` (5) and
  `M63 T1: blocks must partition the items, and says which item broke it` (6).
  Six refusals each name the offending item: non-list, empty list, empty block,
  unknown name, item in two blocks, item in no block. Four guards proven red by
  mutation at implement (orphan, duplicate, index alignment, empty block).
- AC2 — `M63 T2: axes_fits_zeta2() keeps zeta2 only where it is identified` (5),
  `M63 T3: axes_syntax() emits BS latents sharing one zeta2 label` (7),
  `M63 T3: an unidentified block map emits no BS latents at all` (6). Emitted
  syntax and reported component set are one decision (`axes_design()`), so they
  cannot disagree; `details$zeta2_fitted` verified both ways end to end.
- AC3 — `M63 T7 (AC3): the fit recovers zeta2 on the exact population` (7). All
  four components within 1e-4 of truth on the exact population matrix; bound
  stated absolutely and set four orders below the .06 signal.
- AC4 — `M63 T7 (AC4): the omitted-zeta2 bias in xi1 is conditional on
  geometry` (8) and `... closed-form omitted-variable bias predicts the fitted
  bias` (2). Angle-balanced: |bias| < 1e-4 (observed 2.9e-8). Angle-clustered:
  |bias| > 10% of truth (observed +12%). ξ2 inflated under both. The OVB algebra
  and the CFA agree within 1e-3 (observed 1.04e-5).
- AC5 — `M63 T7 (AC5): lavaan, OpenMx and the OLS shadow agree on zeta2` (4;
  OpenMx ran, 0 skips) and `M63 T8 (AC5): the blocked Table 3 rows reproduce Rel
  and SEm (Layer A)` (6). Two SEM engines within 1e-3; OLS shadow within .02.
  All six banked rows reproduce printed Rel within .01 and SEm within .02.
  **AC5 was amended at the merge gate** (Jeff's call) to say what the evidence
  supports: its earlier "sum to 100% across five components" was false for two
  of the six rows. The amended text binds the sum guard to the four
  self-consistent rows and requires the other two pinned with their printed
  sums; that is exactly what the test does, so the amendment describes the
  evidence rather than the evidence chasing the text. **AC4's closing clause was
  amended in the same pass** — it claimed ξ2 inflation "does hold
  unconditionally", which review finding F3 disproved; now "never deflated —
  inflated under most layouts, exactly unchanged under a few".
- AC6 — `M63 T6: axes_is_boundary() catches a negative zeta2` (6) on the
  unmocked predicate, and `M63 T6: axes_reliability() fits and reports zeta2 end
  to end` (16). No NaN, negative, or infinite SEm on any ζ2 path.
- AC7 — **failed on first review evidence; fixed on the branch and re-verified.**
  The implement-time sweep grepped the OLD claim's keywords ("as approximate",
  "deflating the share", "no such component" — absent from every surface) and
  was blind in the other direction: it never swept for positive enumerations
  needing EXTENDING. The prior-review lens caught it, and a positive sweep then
  found five stranded lists, not the three it reported — `@description`'s
  component enumeration, `@details`'s loading structure, and three vignette
  passages (the four-pieces sentence, the population description, the
  components-are-isolated list). All five fixed. The guard was strengthened from
  presence to COMPLETENESS: it now slices the description's own enumeration and
  requires the block member inside it, and deleting that member reddens the
  suite (verified by mutation). `M63 T9 (AC7)` now 8 assertions.
  Fourth recurrence of the M56/M62 lesson; the LESSONS line is updated to name
  the asymmetry rather than repeat the general warning.
- AC8 — `devtools::check(manual = TRUE)`: 0 errors, 0 warnings, 0 notes, with
  `checking tests ... OK` (431s, not SKIPPED) and `checking PDF version of
  manual ... OK` both confirmed present in the log.

**Independent review — three lenses + scorer.** Five findings; three actioned
(scored 92/90/90), two below the 80 threshold and logged.

- **F1 (92) — the corrected conditional's *condition* was itself wrong. FIXED.**
  The shipped rule said ξ1 is unaffected "when each block draws about evenly
  from around the circle". Disproved: blocks pairing diametrically opposite
  scales are maximally dispersed — every block's angles average to the circle's
  centre, mean resultant length 0 — yet at eight scales ξ1 comes back 9% BELOW
  truth. Even spread is not the test; carrying information about angular
  distance is. Rewritten in roxygen, vignette and NEWS around the case that is
  actually safe and checkable (each block draws one item from every scale), with
  the counterexample stated. M63-D2's derivation was right; the plain-language
  gloss put on it was not.
- **F2 (90) — a worked example true only at k = 4. FIXED.** The docs offered
  opposite-scale blocks as a map the rank check refuses. That holds at four
  scales; at six, eight and twelve it is identified — and eight is this
  package's canonical layout, so the example was wrong exactly where most users
  are. Example removed; the behaviour is pinned by test instead.
- **F3 (90) — "ξ2 is inflated in every configuration" overstated. FIXED.** A
  layout exists whose ξ2 bias is exactly zero (auxiliary intercept coefficient
  0 to float precision) while ξ1 carries −0.25·ζ2. Now "inflated under most
  layouts, never deflated".
- **F4 (58, below threshold — fixed anyway).** `axes_is_boundary()`'s header
  still read "Four disjuncts … the remaining two" after M63 added a fifth.
  Actioned despite the score because it is a factual error in a comment on code
  this milestone changed and the fix is one line.
- **F5 (52, below threshold — not actioned).** Supplying blocks that don't
  identify ζ2 is invisible on `print()`/`summary()`. The scorer's own check
  weakened it: `zeta1_fitted` behaves identically and has since M61, so the
  silent drop is the established precedent the plan asked for, not a deviation.

Three tests were added to fence the **condition** rather than instances of it —
the earlier AC4 test exercised only crossed and contiguous layouts, so restating
the rule wrongly reddened nothing. Reinstating either false claim now reddens
the suite (verified by mutation). Test count 20 → 23, assertions 120 → 137.

**Consistency gate.** `cairn_validate` exit 0, all 16 checks PASS (two
advisories, neither a gate failure: M63's 8 criteria over the 7 tripwire, and
M7's 47 legacy hard-wrapped work-log lines). No principle change, so
`cairn_impact` does not apply. Profile `consistency-gate` slot: `document()`
produces no diff; generated files consistent; README current; NEWS carries the
user-visible entry with no milestone numbers; no new top-level files;
`pkgdown::check_pkgdown()` reports no problems.
