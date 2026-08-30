# M113: Certify the ratio the reported FIML standard error is multiplied by

- **Status:** planned
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP3, GP2
- **Branch/PR:** —

## Goal

Extend the per-fit accuracy certificate to `fiml_ratio`, so every quantity a
reported standard error is composed from is priced by the check whose estimate
the refusal warning names.

## Scope

Surface tier: **user-facing** — the certificate gates a refusal the user sees,
and its estimate is printed in that refusal's warning.

**In:** a third certificate field estimating the committed relative error of
`fiml_ratio = corrected / naive` at `cov2cor(Σ̂)` (both arms are already
computed by the `axes_v_pricing()` call the certificate makes and one is
discarded, `R/axes_certificate.R:412`); `axes_degeneracy_refusal()` taking its
max over three fields; the exact-rational oracle extended to bracket the new
field; a second independent oracle type for it (IP3); the false
"never user-reported" claim at `R/axes_certificate.R:343` corrected without
disturbing the true raw-arm claim at `R/axes_corrected_se.R:757`; a `tryCatch`
at the bare certificate call in `axes_degeneracy_refusal()`; and a recorded
characterisation of the naive-only non-finiteness divergence between the two
surfaces (M111 review F3).

**Out:** the shared refusal predicate's straddle fence and the nestedness
literal set → M114. The packaged bracket's platform reach → M115. Surfacing
the estimate on computed fits (RR21 rec 5), the certificate's double
evaluation per checked fit, and the M108/M111 cosmetic residue → the ROADMAP
degeneracy candidate row.

## Acceptance criteria

- [ ] AC1 `axes_accuracy_certificate()` returns a third element estimating the
      committed relative error of `fiml_ratio`, computed from the same
      double-double replay as the other two, and `axes_degeneracy_refusal()`
      refuses `"uncertified"` on the max over all three against
      `axes_degeneracy_delta_star`.
- [ ] AC2 At every geometry `devel/degeneracy-oracle/exact_oracle.R`'s
      certificate case list holds, the new field is at least the oracle's
      measured relative error of `fiml_ratio` there; the script prints that
      ratio per geometry, records the ceiling at its measured value, and exits
      non-zero if any ratio falls below 1 or if it formed fewer ratios than
      `CERT_EXPECTED`.
- [ ] AC3 The new field is validated against a second independent oracle type,
      recorded at the asserting test, sharing no code and no pipeline with the
      exact-rational oracle (IP3). `(RB tripwire: ip-touching)`
- [ ] AC4 Two planted defects, run one at a time, each redden at least one
      assertion in `exact_oracle.R`'s certificate flag: one substituting the
      corrected arm's error for the ratio's (a location plant), one scaling the
      ratio's estimate down by a factor of 10 (a magnitude plant).
- [ ] AC5 The comment sites `R/axes_certificate.R:343`,
      `R/axes_corrected_se.R:311`, `:359`, `:757` and
      `R/axes_reliability.R:1989` each state the arm relationship correctly:
      none claims the certificate omits an arm because that arm is never
      user-reported, except where the arm it names is the raw-`Σ̂` one.
- [ ] AC6 Two conditions raised inside `axes_accuracy_certificate()`, run one
      at a time — a `stop()` and a non-error route failure — each reach
      `axes_corrected_se()` and `axes_scaling_factor()` as
      `reason = "uncertified"` with exactly one warning and no error.
- [ ] AC7 `devtools::test()` clean; `devtools::document()` no diff and no
      unresolved-link warning at pinned `cli.width`;
      `devtools::check(args = "--no-manual")` 0 errors / 0 warnings / 0 notes.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T3
- AC3 → T4
- AC4 → T5
- AC5 → T6
- AC6 → T7
- AC7 → T9

## Tasks

- [ ] T1 Measure the cancellation: replay `corrected`, `naive` and their ratio
      in double-double at each of the oracle's certificate cases and record the
      ratio's relative error against each arm's. Settles whether the ratio needs
      its own replay or is bounded by the arms.
- [ ] T2 Add the `fiml_ratio` field to `axes_dd_pricing()` and
      `axes_accuracy_certificate()` (`R/axes_certificate.R`); extend
      `axes_degeneracy_refusal()`'s max to three fields
      (`R/axes_corrected_se.R:765`).
- [ ] T3 Extend `exact_oracle.py` / `exact_oracle.R` to emit and bracket the
      ratio's true relative error; record the ceiling at its measured value.
- [ ] T4 Second independent oracle type for the new field, recorded at the
      asserting test. `(RB tripwire: ip-touching)`
- [ ] T5 Plant the two AC4 defects one at a time; record per-defect results,
      revert each and verify the tree clean.
- [ ] T6 Correct the five AC5 comment sites; sweep `man/`, `NEWS.md`,
      `vignettes/` and `tests/` for paraphrases of the same claim.
- [ ] T7 `tryCatch` the certificate call at `R/axes_corrected_se.R:764`,
      returning the sentinel; test both condition routes.
- [ ] T8 Characterise the naive-only non-finiteness divergence (M111 review
      F3): exhibit an admitted matrix whose naive row alone is non-finite at
      `R/axes_corrected_se.R:239`, or record that none was found. No behaviour
      change here — a change to which literal a user sees goes to a D-entry.
- [ ] T9 NEWS entry for the widened refusal basis; profile verify and
      consistency-gate slot.

## Work log

- 2026-08-30: created by /milestone-plan.
- 2026-08-30: criteria audit ran in FULL mode (declared user-facing tier), one fresh-context [O] reader that authored none of the criteria, jointly over M113, M114 and M115; it returned eleven findings. Nine with one clear right answer were fixed before writing and reported in chat: an unbounded "anywhere it is false" grep promise narrowed to five enumerated comment sites (AC5); an ambiguous ratio count pinned to `CERT_EXPECTED` (AC2); the `[1, 1e3]` window made a measured output rather than imported before measurement (AC2); recording, reversion and tree-clean clauses moved out of criteria into task procedure; a magnitude plant added beside the location plant (AC4) and a non-`stop()` route added beside the `stop()` plant (AC6); and the gate command pinned to `check(args = "--no-manual")`. Two findings became gate questions.
- 2026-08-30: plan gate chose certifying `fiml_ratio` in its own field over RR21 B4's max over the two arms' standalone errors, because the arms' errors partially cancel in the ratio and B4's bound would refuse fits whose reported SE is accurate — the false-refusal failure D-048/D-049's history is about; falsified by a geometry at which the ratio's measured relative error exceeds both arms'.
- 2026-08-30: plan gate chose demoting M111 review F3 to a recorded characterisation (T8) over an acceptance criterion requiring the two surfaces to agree, because agreement is reachable only by the SE surface reporting a non-finite `fiml_ratio` on the FIML path, against GP2's fail-closed clause; falsified by an admitted matrix whose naive row alone is non-finite and whose reported FIML SE is nonetheless finite.
- 2026-08-30: plan gate chose three milestones over folding M114's predicate fences into this one, because the straddle fixture may not be constructible and that uncertainty should not ride in the same PR as the estimand change; falsified by the fixture proving trivial to construct.
- 2026-08-30: OPEN — the criteria the gate changed (AC1, AC2, AC3, AC4) were sent back through the audit's questions in FULL mode, to a second fresh-context [O] reader, but that re-audit had not returned when this plan was committed. Its findings are to be disposed, and this line replaced with the result, BEFORE /milestone-implement starts on this milestone. An absent replacement means the re-audit was never read, not that it was silent.

## Decisions

## Review
