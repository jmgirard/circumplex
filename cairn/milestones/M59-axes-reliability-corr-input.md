# M59: Correlation-matrix input path for `axes_reliability()`

- **Status:** review
- **Priority:** normal
- **Depends on:** M54
- **Driving RR:** —
- **Principles touched:** —
- **Branch/PR:** `m59-axes-reliability-corr-input` / https://github.com/jmgirard/circumplex/pull/85

## Goal

Let `axes_reliability()` estimate from a published item correlation matrix plus
its sample size, so a reanalysis needs no raw data.

## Scope

**In:** a secondary input path on the existing exported function —
`cormat` + `n` take an item correlation matrix instead of raw items, fit
through `lavaan`'s `sample.cov`/`sample.nobs` surface; its own refuse contract;
Nunnally–Bernstein reported `NA`-with-reason there (RR09 §7.4); the
oracle battery (raw-path round-trip, population-matrix recovery, cross-engine);
docs, vignette, NEWS. Plus the outstanding RR09 §7.8 blockwise-ζ2 doc note
(M54 review F3, deferred sub-threshold).

**Out:** non-octant types b–f, quasi-circumplex weights, blockwise ζ2
estimation, FIML on items → they stay on the ROADMAP candidate row
"Axes-reliability deferred-in-spec extensions", each still gated on a concrete
use case (D-026). Covariance-matrix (non-unit-diagonal) input → not planned;
the model assumes unit-variance items. Gating the release → out: M7 gains **no**
`Depends on: M59` and never waits for it (M59 plan gate). Amended at the
implement gate: the milestone *does* enter v2.0.0's contents, because master
ships as v2.0.0 and M59 merges first, so D-030 records the narrow D-001
supersession the plan wrongly thought unnecessary.

## Acceptance criteria

- [x] AC1 (surface). `axes_reliability()` gains `cormat = NULL` and `n = NULL`,
      following `cpm_fit()`'s house pattern (`R/cpm_fit.R:1559`): exactly one of
      `data` or `cormat`; `n` is required with `cormat` and refused with `data`.
      `devtools::document()` produces no diff. *(RB tripwire: irreversible-api)*
- [x] AC2 (round-trip oracle). On ≥2 datasets, `axes_reliability(cormat =
      cor(x), n = nrow(x), …)` equals `axes_reliability(x, …)` on ξ1/ξ2/ζ1, both
      reliabilities, SEm at `sd = "std"`, `df` and χ² within 1e-6 — with
      lavaan's `(N−1)/N` likelihood rescaling explicitly handled, not absorbed
      into tolerance (RR09 BC5's trap).
- [x] AC3 (two independent oracle types on the corr path). **(a)
      Deterministic population matrix** — the exact `axes_population_cor()`
      matrix fed through the public `cormat` path recovers (ξ1, ξ2, ζ1) within
      1e-4 with χ² < 1e-6. **(b) Cross-engine** — lavaan and OpenMx fits of the
      identical model on the identical correlation matrix agree on every free
      component within 1e-3; that test **skips**, never passes, when OpenMx is
      absent; no new Imports (D-006/D-014). Both halves need their own evidence.
- [x] AC4 (N–B and SEm). `nb_reliability` is `NA` on the `cormat` path and
      `print`/`summary` state why — RR09 §7.4: "N–B must be `NA`-with-reason
      there, not dropped silently". `sd = "raw"` errors informatively (no raw
      scores exist); `"std"` and numeric `sd` work.
- [x] AC5 (refuse contract). Each errors informatively, with a regression test:
      `data` and `cormat` both supplied, or neither; `cormat` non-square,
      asymmetric, non-unit-diagonal, non-finite, or non-PD; `cormat` dimnames
      absent or mismatched with `items`; `n` absent with `cormat`, supplied
      with `data`, non-numeric, non-finite, or ≤ number of items.
- [x] AC6 (docs). Roxygen and `vignettes/axes-reliability.Rmd` document the
      corr path (including the Cudeck SE approximation already stated for the
      raw path) and carry the RR09 §7.8 note that a blockwise-administered
      instrument analyzed without ζ2 folds block variance into the general and
      scale components; NEWS entry added.
- [x] AC7 (profile verify). `devtools::test()` clean and
      `devtools::check()` OK — with the PDF-manual step confirmed to have
      **run** by grepping the log for `checking PDF version of manual`, since
      this milestone touches roxygen (M7/M57 lesson).

## Coverage

- AC1 → T2, T8
- AC2 → T1, T2, T3
- AC3 → T5 (a), T6 (b)
- AC4 → T4
- AC5 → T2, T7
- AC6 → T8, T9
- AC7 → T9, T10

## Tasks

- [x] T1. Test-first: write the AC2 round-trip test against the not-yet-added
      `cormat`/`n` arguments and watch it fail. Prove each new guard by
      mutation, not by eye, and scope every probe to the surface it claims to
      check (M57).
- [x] T2. Add `cormat`/`n` and the AC5 refuse contract in `axes_reliability()`
      (`R/axes_reliability.R:366`), mirroring `cpm_fit()`'s validation block
      (`R/cpm_fit.R:1583`), reusing the existing PD/eigenvalue guard at `:456`
      and bypassing the listwise block at `:427`.
- [x] T3. Route the fit to `sample.cov`/`sample.nobs`. `sem_fit_cfa()`
      (`R/ssm_sem.R:745`) is the single `lavaan::cfa` chokepoint and takes
      `data`; extend it or add a sibling seam without disturbing its SEM
      callers. `axes_ols_shadow()` (`:137`) already takes `R` directly.
- [x] T4. N–B → `NA` with a stated reason; refuse `sd = "raw"`; update
      `print`/`summary`. Grep every consumer of `nb_reliability` and the
      `details` list before changing their contract (M18).
- [x] T5. AC3(a) population-matrix oracle through the public `cormat` path.
- [x] T6. AC3(b) cross-engine OpenMx oracle on the correlation matrix,
      patterned on M54's existing BC7 test.
- [x] T7. AC5 refuse-contract regression tests.
- [x] T8. Roxygen: `@param cormat`, `@param n`, the corr-path `@details`
      paragraph, the blockwise-ζ2 note; `devtools::document()`.
- [x] T9. Vignette section + NEWS. Check the tail bytes of any wholesale-written
      file (M34) and confirm no echoed chunk depends on a hidden one (M50).
- [x] T10. Full `devtools::test()` + `devtools::check()`; verify the PDF-manual
      step actually ran (AC7); fix fallout.

## Work log

- 2026-07-25: review — all 7 ACs verified on final code; 3 of 4 diff-bug findings actioned and fixed (F1/F3/F4), 3 sub-threshold logged. One blocking `gh pr checks --watch` timed out at 10m; fresh state at that point: pkgdown pass, ubuntu-latest (release) + test-coverage pending. Nothing left watching.
- 2026-07-25: created by /milestone-plan.
- 2026-07-25: start — status in-progress, branch `m59-axes-reliability-corr-input` cut from master.
- 2026-07-25: T1 — AC2 round-trip test written first; fails with `unused arguments (cormat, n)`, the intended pre-implementation failure.
- 2026-07-25: T10 — `devtools::check(manual = TRUE)` → `Status: OK` (0 errors/warnings/notes); `checking PDF version of manual ... OK` present at log line 119, so the step the M7/M57 lesson names as silently skipped genuinely ran. Suite inside check OK (197s). Status → review.
- 2026-07-25: T9 — vignette section 4 ("Starting from a published correlation matrix") + a fourth caveat carrying the RR09 §7.8 blockwise note; NEWS folded into the existing unreleased `axes_reliability()` bullet. Vignette knits; no echoed chunk depends on a hidden one (M50 check clear).
- 2026-07-25: amended Scope at the implement gate — the plan's "v2.0.0 not entered" was wrong on mechanics (master ships as v2.0.0; `DESCRIPTION` already reads 2.0.0), so D-030 records the narrow D-001 supersession. M7 still gains no dependency. Jeff's call at the gate.
- 2026-07-25: T8 — roxygen: `@param cormat`/`@param n`, a "Supplying a correlation matrix" section, a "Blockwise instruments" section discharging RR09 §7.8 (M54 F3), and a cormat example. `document()` idempotent; only `man/axes_reliability.Rd` changed. The `cpm_gradient` link warning is pre-existing (present on the unmodified tree).
- 2026-07-25: T5–T7 — AC3(a) population oracle pins the convention exactly (recovered = truth × (n−1)/n to 1e-8 relative at n = 500/5e3/5e4, and a permuted cormat gives an identical answer); AC3(b) cross-engine OpenMx agrees to ~2e-5 against a 1e-3 bar; AC4/AC5 regressions. Both novel guards proven by mutation: dropping `is.finite(n)` and dropping the cormat reordering each turn the suite red. Suite FAIL 0 / PASS 3309.
- 2026-07-25: T2–T4 — `cormat`/`n` path, refuse contract, `axes_fit_cormat()` seam, N–B `NA`-with-reason, `sd="raw"` refusal, print/summary. Round-trip agrees to 1e-15 (not merely inside the 1e-6 bar); the wishart/normal ratio measured exactly 499/500, confirming the `(N−1)/N` mechanism is matched rather than tolerated. Suite FAIL 0 / PASS 3263 (baseline 3247 + the 16 new).
- 2026-07-25: amended AC1/AC2/AC3a/AC4/AC5 + Scope + T1/T2/T8 at the implement gate — the planned `nobs`-switches-`data` surface is replaced by `cormat` + `n`, matching `cpm_fit()`'s existing correlation-matrix path (`R/cpm_fit.R:1559`), a house precedent the plan's collision sweep missed. Jeff's call at the gate.

## Decisions

## Review

Reviewed 2026-07-25 on `m59-axes-reliability-corr-input` @ PR #85. Evidence is
fresh (re-run at review, never recalled from implementation).

### Acceptance-criteria evidence

- **AC1 (surface).** `args(axes_reliability)` prints
  `function (data = NULL, items, angles = NULL, instrument = NULL, cormat = NULL, n = NULL, sd = "std")`.
  The exactly-one and `n`-with-`data` refusals are fenced in the AC5 block. A
  fresh `devtools::document()` left `git status` clean over `man/`, `NAMESPACE`,
  and `R/` — no diff.
- **AC2 (round-trip oracle).** Test block `AC2: the cormat path reproduces the
  raw path exactly` — 16 assertions, 0 fail, 0 skip, across two datasets (the
  bundled 32-item `simulated_items`; a 16-item draw at ξ1 = .22). Measured
  max |diff|: components 1.44e-15, SEs 2.69e-17, reliability 1.11e-16,
  χ² 3.58e-12, `df` identical (493) — three to nine orders inside the 1e-6 bar.
  The `(N−1)/N` convention is *matched*, not tolerated: the wishart/normal ξ1
  ratio measured 0.9979999 against 499/500 = 0.998 exactly.
- **AC3 (two independent oracle types).** (a) 17 assertions: recovery equals
  truth × (n−1)/n to 1e-8 relative at n = 500 / 5000 / 50000, χ² < 1e-6 at each;
  at n = 50000 outright recovery within 1e-4 (measured error 3e-6); a permuted
  `cormat` reproduces the unpermuted answer to 1e-10. (b) 2 assertions (seeds
  7, 8): OpenMx vs the public path max |diff| ≈ 1.95e-5 against the 1e-3 bar.
  Skips-never-passes proven mechanically: a probe applying
  `skip_if_not_installed()` to an absent package reports `Skip`, and the
  `expect_true(FALSE)` following it never executed. No new Imports —
  `DESCRIPTION` is not in the diff; OpenMx stays `Suggests` (D-006/D-014).
- **AC4 (N–B and SEm).** 9 assertions: the `nb_reliability` column is present
  and wholly `NA`; `print()` and `summary()` both emit "Nunnally-Bernstein
  comparison needs the raw item"; the header reads `Input: correlation matrix`
  and `Sample N:`; numeric `sd` yields sem = sd·sqrt(1 − rel) to 1e-10;
  `sd = "raw"` errors with "needs the raw scale scores".
- **AC5 (refuse contract).** 18 assertions, one per listed condition: both or
  neither of `data`/`cormat`; `n` with `data`; non-square; absent dimnames;
  asymmetric; non-unit diagonal; NA/non-finite; singular (non-PD); an item
  absent from the matrix; `n` missing, character, length-2, fractional, NA, Inf,
  = p, and < p. The Inf case is load-bearing rather than decorative: removing
  `!is.finite(n)` turns the suite red (mutation run at T5–T7).
- **AC6 (docs).** `man/axes_reliability.Rd` carries `cormat` (8 occurrences) and
  the "Blockwise instruments" section, whose 6.7% figure roxygen correctly
  escaped to `6.7\%` (an unescaped `%` is an Rd comment character; the manual
  builds clean). `vignettes/axes-reliability.Rmd` adds §4 "Starting from a
  published correlation matrix" plus a fourth caveat carrying the same note;
  Cudeck (1989) is cited on both surfaces. NEWS is folded into the existing
  unreleased `axes_reliability()` bullet. No milestone numbers appear in any
  user-facing text (grep clean over NEWS, Rd, vignette).

- **AC7 (profile verify).** Re-run on the FINAL post-fix code:
  `devtools::test()` → FAIL 0 / WARN 4 / SKIP 0 / PASS 3318 (the 4 warnings are
  pre-existing, confirmed against a stashed tree at implementation time; the
  axes file alone is 159 pass / 0 fail / 0 skip). `devtools::check(manual = TRUE)`
  → `Status: OK`, zero ERROR/WARNING/NOTE lines, with
  `checking PDF version of manual ... OK` at log line 119 — the step
  `devtools::check()` skips by default and which the M7/M57 lesson names as the
  repeat offender, confirmed to have actually run rather than inferred from
  `Status: OK`.

### Consistency gate

- `cairn_validate` exit 0 — 16 checks PASS (including `coverage complete` and
  `binding criteria`); `record density`, `sizing`, `dangling id tokens`,
  `references staleness`, `release window` all OK. The 47 `work-log format`
  advisories are M7's pre-existing wrapped entries — history, never edited
  (IP4); none belong to M59.
- `cairn_impact` correctly skipped: `Principles touched: —`, no IP/GP changed.
- Profile (`r-package`) `consistency-gate` slot: `document()` no diff ✓;
  `NAMESPACE`, `data/`, `README` untouched by the diff ✓; `man/` limited to the
  one regenerated Rd ✓; `pkgdown::check_pkgdown()` → "No problems found" ✓;
  NEWS entry present with no milestone numbers ✓; no new top-level files, so no
  `.Rbuildignore` addition owed ✓.

### Independent review — three lenses

- **[S] prior-PR-comments lens: no findings.** GitHub inline-comment probe
  returned `[]` (consistent with M33's recorded finding that this repo reviews
  through cairn, not PR threads), so archived `## Review` sections were the
  evidence base. It confirmed the diff *discharges* rather than regresses each
  targeted obligation: M54 F3's blockwise note present in both roxygen and
  vignette with RR09 §7.8's own 6.7% figure; RR09 §7.4's NA-with-reason honored
  in `print()` and `summary()` independently; BC5's rescaling trap handled by
  matching the convention, not widening tolerance; BC4's mandatory
  `orthogonal = TRUE` set on the new fit path. No `references/` page touched, so
  the M40/M47 provenance-status family does not apply.
- **[S] blame-history lens: no findings.** The raw path's listwise block and its
  `n <= p` refusal are byte-identical to `master` (moved inside `if (has_data)`,
  logic unchanged); the PD/eigenvalue guard and its 1e-8 tolerance remain
  unconditional on both paths; the boundary (BC11) block is path-agnostic and
  untouched; `suppressWarnings` scope is functionally unchanged; `sem_fit_cfa()`
  has an empty diff, so the SEM chokepoint was bypassed rather than disturbed
  (as T3 intended); `details` only gained `input`. It separately judged the
  zero-variance refusal's unreachability on the `cormat` path legitimate — a
  unit diagonal encodes non-zero variance by definition, and a zero-variance
  item yields `NA` correlations that the finiteness check catches.
- **[O] diff-bug lens: four findings, three actioned.** It independently
  confirmed the core statistics — round-trip xi1 diff 5.6e-17, the `(N−1)/N`
  claim true (traced to `sample.cov.rescale`, so attributing it to `likelihood`
  is loose wording not an error), guards firing on the new path (it forced a
  ξ1 = 0 population through `cormat` and got the correct warning + NA), and no
  NULL dereference of `mat`/`scale_scores`.

**Scored by a fresh [S] scorer** (did not generate the findings). Actioned
(≥ 80), all three fixed on the branch and each proven by mutation:

- **F1 (95) — `R/axes_reliability.R`: dimnames guard checked one dimension, the
  subset indexes two.** A matrix with colnames and NULL rownames passed
  validation, then failed on `cormat[all_cols, all_cols]` with a bare
  `subscript out of bounds` rather than the refusal AC5 promises for absent
  dimnames. That shape is the *default* result of `as.matrix(read.csv(...))` —
  the transcribe-a-published-matrix workflow this path exists to serve.
  Reproduced verbatim. Fixed: the guard requires dimnames present and identical
  on both dimensions, in the same order; three cases added (rownames-NULL,
  colnames-NULL, scrambled order). Mutation: reverting to colnames-only → red.
- **F3 (85) — the permuted-cormat test could not fence what it claimed.** Its
  comment said the permutation proves loadings are matched by name; the
  assertion read `components$Estimate`, which lavaan makes invariant by matching
  `sample.cov` on dimnames itself. Measured with the reorder removed:
  max |diff| exactly **0**. The reorder actually determines
  `details$ols_shadow` (built positionally), which collapses from .15 to
  ~2.6e-4 without it — and that was asserted only on the raw path. Fixed:
  `ols_shadow` now asserted on the `cormat` path. Mutation: dropping the
  reorder now fails *on the `ols_shadow` assertion* rather than incidentally
  through ~1e-8 optimizer jitter.
- **F4 (85) — the cross-engine oracle's comment misattributed its residual, and
  a systematic offset sat inside the bar.** The comment blamed the engines'
  likelihood normalization; correcting for `(N−1)/N` measurably *worsens*
  agreement, and pairing OpenMx with a wishart lavaan fit is looser still
  (5.5e-5 / 6.5e-5 vs 1.9e-5 across seeds 7, 8) — so OpenMx's `type="cov"`
  convention sits nearer lavaan's `"normal"`, the shipped pairing is the
  tightest available, and the residual is plain optimizer disagreement. Fixed:
  comment states what is true; bar tightened 1e-3 → 2e-4, plus an assertion
  that the disagreement is below the `(N−1)/N` offset (xi1/n = 7.5e-5), which
  *falsifies* the old explanation instead of leaving it untested. This was the
  same "absorbed into tolerance" practice AC2 forbids, one screen away.

Sub-threshold (< 80) — logged, not actioned (IP3: surfaced, never dropped):

- **F2 (65)** — the `cormat` path validates finiteness/symmetry/unit-diagonal on
  the whole supplied matrix before subsetting, while the raw path subsets first
  and validates only selected columns. So a superset matrix carrying the items
  plus unrelated variables is refused if anything *outside* the item block is
  NA, though the raw-data analogue is accepted and a clean superset works.
  Reproduced. Arguable design gap, no criterion promises block-only validation.
- **F5 (65)** — `sample(nrow(sigma))` had no preceding `set.seed()`. Fixed
  incidentally (`set.seed(59)`) because F3's fix rewrote those exact lines;
  recorded here rather than claimed as an actioned finding.
- **F6 (50)** — the `sd = "raw"` refusal sits after the lavaan fit, so that
  combination pays a full fit before erroring, and on a boundary matrix emits
  the boundary warning before the `sd` error. Cost/ordering nit; AC5's
  informative error is still delivered.
