# M63: Blockwise instruments for `axes_reliability()` — the ζ2 component

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m63-axes-reliability-blockwise-zeta2`

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

- [ ] AC1 `blocks =` accepts a per-item block map on the raw-data and `cormat`
      paths, is validated with the house `stopifnot()`/`is_*()` idiom, and is
      refused with a named error on a length mismatch, an unmappable item, or a
      non-finite/missing block label.
- [ ] AC2 ζ2 is fitted and reported when identified; when every block coincides
      with a scale it is dropped from the emitted syntax and flagged in
      `details$zeta2_fitted`, with the emitted model and the reported component
      set never disagreeing (the `axes_fits_zeta1()` contract, extended).
- [ ] AC3 On a synthetic population with known ζ2 > 0, the fit recovers ξ1 and
      ζ2 within a stated **absolute** bound (stated absolutely, per the M61
      relative-tolerance trap), set from the discrimination required rather than
      from one machine's printed value (M59).
- [ ] AC4 Fitting that same population **without** ζ2 biases ξ1 by a margin the
      test asserts — the component demonstrably earns its place, and the
      caveat at `R/axes_reliability.R:533-543` is shown to describe a real error.
- [ ] AC5 lavaan, OpenMx, and the OLS shadow agree on ξ1/ξ2/ζ1/ζ2 within stated
      bounds on the population matrix, and the blocked type-a rows (CSIV S7,
      TRC-g S10, TRC-t S11) plus the OCAI type-d rows reproduce their printed
      reliability by Spearman–Brown and sum to 100% across five components.
- [ ] AC6 No NaN, negative, or infinite SEm on any ζ2 path: a negative ζ2 trips
      the boundary flag, and the never-NaN block covers the new component.
- [ ] AC7 Every surface that enumerates the component set names ζ2 — roxygen
      (including the replaced `# Blockwise instruments` section), `print()`,
      `summary()`, the `@return` component-row count, the vignette, and NEWS —
      verified by grepping for the enumeration, not the changed lines (M56/M62).
- [ ] AC8 `devtools::test()` clean and `devtools::check()` 0/0/0, with the PDF
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
- [ ] T7 Oracle: known-ζ2 recovery, the omitted-ζ2 bias demonstration, and
      lavaan/OpenMx/OLS-shadow agreement. Prove each new guard by mutation, and
      record any null mutation with why the model is invariant (M60 lesson).
- [ ] T8 Bank the blocked type-a rows (CSIV S7, TRC-g S10, TRC-t S11) and the
      OCAI type-d rows into `cairn/references/strack2013.md`, two channels on
      p. 7, extraction status dated and unbolded (M45 format trap).
- [ ] T9 Docs: replace `R/axes_reliability.R:533-543`, update `print()`/
      `summary()`/`@return`/vignette/NEWS, `devtools::document()`, then grep
      every enumerating surface and build the PDF manual.

## Work log

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

## Review
