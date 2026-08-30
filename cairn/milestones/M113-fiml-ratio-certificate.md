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
      double-double replay as the other two and finite and non-negative on
      every input — its sentinel gaining a third element, so no route failure
      leaves the field `NaN` for `axes_degeneracy_refusal()`'s comparison to
      raise on. `axes_degeneracy_refusal()` refuses `"uncertified"` on the max
      over all three fields against `axes_degeneracy_delta_star`, and
      `axes_degeneracy_note()` prints that same max as the warning's estimated
      relative error, so no fit is refused against a target its warning reports
      it inside.
- [ ] AC2 At every geometry `devel/degeneracy-oracle/exact_oracle.R`'s
      certificate case list holds, the new field is at least the oracle's
      measured relative error of `fiml_ratio` there and at most a ceiling of
      its own, measured once on a clean tree and thereafter frozen as a
      constant no plant run re-measures; `CERT_CEILING`, which bounds the `se`
      and `cval` lines, is unchanged. The script emits eighteen certificate
      lines — six geometries by three fields — and exits non-zero if it emits
      fewer, if any ratio falls below 1, or if any exceeds its own field's
      ceiling.
- [ ] AC3 The new field is validated against a second independent oracle type,
      recorded at the asserting test, sharing no code, no library and no
      pipeline with `exact_oracle.py` or with the route under test (IP3) — the
      independence M108 already records at
      `tests/testthat/test-axes-certificate.R:29-30`.
      `(RB tripwire: ip-touching)`
- [ ] AC4 Three planted defects, run one at a time against the frozen ceilings,
      each redden the new field's own ratio line at a named geometry in
      `exact_oracle.R`: the field's safety factor dropped from 10 to 1 (an
      under-report, against the floor), the ratio replayed at the raw `Σ̂`
      rather than `cov2cor(Σ̂)` (the D-044 metric split), and the double-double
      replay of the naive arm truncated to double precision (a collapse of the
      reference route). The three vary magnitude, metric and route, not one
      axis three times.
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
      `axes_accuracy_certificate()` (`R/axes_certificate.R`), with its own
      denominator guard and a third sentinel element; extend
      `axes_degeneracy_refusal()`'s max to three fields
      (`R/axes_corrected_se.R:765`) and `axes_degeneracy_note()`'s printed
      estimate to the same max (`R/axes_corrected_se.R:786-793`).
- [ ] T3 Extend `exact_oracle.py` / `exact_oracle.R` to emit and bracket the
      ratio's true relative error; measure the new field's ceiling once on a
      clean tree and freeze it as its own constant beside `CERT_CEILING`.
- [ ] T4 Second independent oracle type for the new field, recorded at the
      asserting test. `(RB tripwire: ip-touching)`
- [ ] T5 Plant the three AC4 defects one at a time against the frozen
      ceilings; record per-defect results naming the reddened line and its
      geometry, revert each and verify the tree clean.
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
- 2026-08-30: the criteria the gate changed (AC1, AC2, AC3, AC4) went back through the audit's questions in FULL mode, to a second fresh-context [O] reader that authored none of them; it returned nine findings. Eight were fixed here: the AC4 location plant could not redden, because under the plan gate's own cancellation hypothesis substituting an arm's error OVER-estimates and AC2 let the ceiling be re-measured — the ceiling is now frozen and the plant set replaced by three varying magnitude, metric and route; `axes_degeneracy_note()` still printed `max(se, cval)`, so a fit refused on the ratio field would have warned a number below the target it was refused against; AC1 permitted a non-finite third field, whose `NaN` would raise in `axes_degeneracy_refusal()`'s comparison OUTSIDE T7's tryCatch, against GP2; AC3's "no code and no pipeline" was unsatisfiable as worded and is narrowed to M108's own phrasing; AC2's count was pinned to `CERT_EXPECTED`, a constant the implementer sets after the fact, and to `cert_n`, which counts lines rather than ratios — now eighteen lines stated in the criterion; the single shared `CERT_CEILING` could have been loosened for the two already-audited fields and is now per-field; and AC4's redden is tied to the new field's own line at a named geometry. The ninth (F8 — AC3 is satisfiable by extending the zero-committed-error dyadic oracle, which would validate the new field only where there is no error to catch) is a genuine two-way call and is open at the user gate.

## Decisions

## Review
