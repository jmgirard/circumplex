# M92: Teach the boundary regime the structure vignette's own example is in

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP5
- **Branch/PR:** `m92-boundary-regime-guidance`

## Goal

Teach a reader whose CPM fit prints a boundary or weak-identification note what
it means for each reported number and what to do next, on the fit the structure
vignette already displays.

## Scope

Surface tier: **user-facing** — the deliverable is a shipped vignette section
and the help page that points at it.

**In:** a new subsection of `vignettes/evaluating-circumplex-structure.Rmd` §2
that glosses the marker vocabulary, reads the boundary behaviour of the fit
already displayed there (`jz2017`, seed 12345: NO at ζ̂ = 1.000 with a
zero-width interval, the printed Heywood note, and the ill-conditioning
warning the same chunk emits), adds one seeded analytic-path fit whose printed
summary names its fired markers, and gives an action ladder. Two pre-existing
passages in the same section are reconciled with it. One internal change:
the marker labels move into a catalog the marker function indexes, so the
label test checks the contract rather than the function's source text.

**Out:**
- Changing the marker set, the β = 0.10 cut, or the N thresholds — measured and
  ratified (`devel/cpm-marker-validation.md`); the retained-β refinement is
  handed to the CPM simulation-paper track (`devel/cpm-simulation-paper-plan.md`).
- Printing the fired-marker list on the bootstrap path as the analytic path
  does → new ROADMAP candidate row (an inference-caution change, not teaching).
- Better intervals for this regime → the standing "Boundary-regime interval
  improvement" ROADMAP candidate.
- Printing the validation-simulation figures as reader-citable results → the
  CPM simulation-paper track; they do not ship with the package.

## Acceptance criteria

- [ ] AC1 The new subsection glosses every marker label the fitted-solution
      marker function can return. A test enumerates that domain from a new
      internal label catalog that `cpm_boundary_markers()` indexes, extracts
      the subsection's own text between its heading and the next heading of
      the same level, and fails if any catalogued label is absent from it.
      Teeth shown: with one label removed from the subsection, the test fails.
- [ ] AC2 The subsection contains an executed chunk whose printed `summary()`
      output includes the note naming fired markers, and the prose reads that
      output. A test reproduces the chunk's fit from the same seed and asserts
      the marker set it fires is exactly the set the prose names.
- [ ] AC3 For each of the three reported parameter families (scale angles,
      communality indices ζ, correlation-function weights β) the subsection
      states what a fired marker implies for that family's **interval**, on the
      estimation path where the package measured it, and says where the record
      does not speak: point-estimate bias, the bootstrap path, and the two
      markers the record found predictively null or power-limited
      (`devel/cpm-marker-validation.md`, "Per-marker verdicts" and "Honest
      nulls and caveats").
- [ ] AC4 A claims ledger in this file lists **every** sentence of the new
      subsection, in order, built by walking it sentence by sentence, each
      carrying one verdict: `derived` (naming the artifact and anchor, the
      anchor stating what the sentence claims), `on-page` (naming the chunk
      whose printed output shows it), or `exempt` (framing, no factual claim).
      A sentence absent from the ledger is a gate failure.
- [ ] AC5 The subsection's action ladder states, at minimum, what to re-fit or
      re-check, what to report alongside a flagged fit, and which reported
      quantities stay interpretable — the last as a closed list. The
      already-shipped sentence preferring the bootstrap on the raw-data path
      (`vignettes/evaluating-circumplex-structure.Rmd`, "Prefer the bootstrap
      (the raw-data default) when you have raw data.") does not count toward
      any of the three.
- [ ] AC6 Two pre-existing passages are reconciled. (a) The paragraph reading
      the estimated angles no longer describes them as landing near their
      theoretical positions, and its final wording is quoted verbatim in AC4's
      ledger for the review gate to read against the printed table. A test pins
      the figures the new wording rests on — the smallest and largest of the
      eight gaps between circularly adjacent estimates (estimates sorted around
      the circle, wrap-around gap included), and the largest departure of an
      estimated angle from its theoretical position with PA held as the fixed
      reference at 90° — and asserts the estimates' circular order matches the
      scale order, the claim the old bullet got right. The prose does not name
      which pair of scales carries the widest gap (the two widest are within 2°
      of each other). (b) The bullet matched by the
      text "**Boundary solutions are common at realistic sample sizes.**" is
      reduced to a pointer into the new subsection, and every fact it asserted
      appears in AC4's ledger.
- [ ] AC7 `?summary.circumplex_cpm` cross-references the new section by its
      heading, and a test asserts that heading string appears in the vignette
      source. `devtools::check(args = "--no-manual")` is 0/0/0 with vignettes
      rebuilt, the built vignette grepped clean of tool-call scaffolding, and
      `NEWS.md` gains one Documentation entry.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T2, T4
- AC3 → T3
- AC4 → T6
- AC5 → T4
- AC6 → T2, T5
- AC7 → T7

## Tasks

- [ ] T1 Add the internal marker-label catalog in `R/cpm_fit.R` and have
      `cpm_boundary_markers()` (`R/cpm_fit.R:1416`) index it; the existing
      marker tests (`tests/testthat/test-cpm_api.R:555-610`) still pass, and
      no user-visible output changes.
- [ ] T2 Write the tests first and watch each fail: catalog-vs-subsection label
      sweep (scoped extraction), demo-chunk marker-set equality, the angle
      figures and ordering, the roxygen heading string. Break one guarded line
      per test.
- [ ] T3 Draft the subsection — marker glossary, the on-page reading of the
      displayed fit (NO ζ̂ = 1.000 and its zero-width interval, the printed
      Heywood note, the ill-conditioning warning), and the per-family interval
      implications with their explicit not-measured statements.
- [ ] T4 Add the seeded analytic-path demonstration chunk (base-R `chol` +
      `rnorm` draw from the fitted implied matrix at N = 2500 — no new
      dependency) and write the action ladder from what it prints.
- [ ] T5 Reconcile the angle-reading paragraph and the "Boundary solutions are
      common" bullet against the new subsection.
- [ ] T6 Author the claims ledger in this file, sentence by sentence.
- [ ] T7 Roxygen pointer + `document()`; NEWS entry; full check with vignettes
      rebuilt; grep the built vignette for scaffolding.

## Work log

- 2026-08-16: created by /milestone-plan, promoting the ROADMAP "Heywood-regime user guidance" candidate (2026-08-04 design interview, wart routing).
- 2026-08-16: criteria audit ran in **full** mode (user-facing tier), fresh-context [O] reader over the drafted wording; returned 14 findings — 10 fixed autonomously before writing (subsection-scoped label search; interval-side claims restricted to the measured path with explicit not-measured verdicts; anchor-adequacy governing the whole ledger; every sentence ledgered; example pin extended; on-page reading restated over all three fired markers; shipped text excluded from the ladder's minimum; interpretability closed to a list; roxygen heading tested; old bullet anchored by quoted text), 4 posed at the gate.
- 2026-08-16: plan gate chose an added analytic-path demonstration fit over teaching only what the bootstrap example surfaces, because the fired-marker list prints only on the analytic path and the vignette otherwise names labels no reader can reproduce; falsified by evidence that the simulated demo misteaches practice or that readers cannot follow the two-fit structure.
- 2026-08-16: plan gate chose a seeded base-R simulation from the fitted implied matrix over passing an inflated `n` to the correlation-matrix path, because the marker list needs N ≥ 2000 and `jz2017` has n = 1166; falsified by evidence that the simulated correlation matrix leaves the boundary regime the section teaches.
- 2026-08-16: plan gate chose to fix the angle-reading paragraph here over deferring it, because it misdescribes the same printed table the new subsection teaches from; falsified by a reading showing the sentence is about spacing alone and the table supports it.
- 2026-08-16: plan gate chose an internal label catalog over enumerating labels from `deparse()`, because the deparse probe breaks under a behaviour-preserving refactor; the alternative of also printing markers on the bootstrap path lost as an inference-caution design change, not teaching, and became a candidate row; falsified by the catalog forcing a user-visible output change.
- 2026-08-16: plan gate chose qualitative guidance anchored on printed output over quoting the validation-simulation figures, because those files do not ship and a reader cannot trace them (the M77 precedent); falsified by the simulation paper publishing them citably.
- 2026-08-16: step 2 chose docs guidance over a new runtime diagnostic, because the runtime already names fired markers at `summary()` and the gap the candidate row records is teaching, not detection; falsified by a user report that the printed notes are missed rather than misunderstood.

- 2026-08-16: branch cut, status in-progress.
- 2026-08-16: amendment gate — AC6(a) rewording taken (user approved): pinned quantities become the eight circular adjacent-estimate gaps (min/max), the largest theory departure *with PA named as the fixed reference at 90°*, and an ordering assertion; the paragraph's final wording is ledgered for the gate to read. Rationale correction: the retired "arc the eight estimates span" figure was not vacuous but redundant — it is 360 minus the largest gap (281.3°) and its linear reading depends on where the wrap falls. Fresh-context [O] audit of the amended wording ran in full mode and returned five findings, all folded in before writing.

## Decisions

## Review
