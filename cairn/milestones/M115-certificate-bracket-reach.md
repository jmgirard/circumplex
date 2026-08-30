# M115: Make the packaged accuracy bracket assert where the shipped pricing differs

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M113
- **Driving RR:** —
- **Principles touched:** IP3
- **Branch/PR:** `m115-certificate-bracket-reach`

## Goal

Replace the frozen macOS pricing yardstick with a committed exact value, so
the packaged bracket measures the running machine's own error instead of
skipping wherever that machine is not the one the figures were frozen on.

## Scope

Surface tier: **internal** — the deliverable is the validation apparatus, a
test file and a `devel/` script, on which no external consumer of the package
relies.

**In:** RR21 B3 — `exact_oracle.py` emitting each case's exact `v` and `u` as
hi/lo double pairs, committed in `tests/testthat/test-axes-certificate.R` — so
the bracket's bit-identity precondition
(`tests/testthat/test-axes-certificate.R:202-234`) can drop its
shipped-pricing half and keep only the anchor-matrix half, which reproduces
everywhere; a non-empty-domain assertion in the packaged file; a
safety-factor sensitivity plant; `cert_n` counting ratios rather than
`cert_line()` calls; and `sweep_ok` and `reach_ok` simplified so neither can
report PASS having checked nothing.

**Out:** extending the bracket to M113's third certificate field → M113. The
shared-predicate and nestedness fences → M114. The recorded cost figure, the
`dd_*` namespace prefixing, the certificate's double evaluation per fit and
the rest of the M108/M111 cosmetic residue → the ROADMAP degeneracy candidate
row.

## Acceptance criteria

- [ ] AC1 `exact_oracle.py` emits, for each certificate case, the exact `v` and
      `u` as hi/lo double pairs via `%a`, and those pairs are committed in
      `tests/testthat/test-axes-certificate.R`.
- [ ] AC2 The bracket's precondition no longer reads the shipped double
      pricing: on this machine, with that pricing perturbed so it no longer
      matches the figures the frozen values were measured on,
      `devtools::test(filter = "axes-certificate")` reports zero skips and the
      bracket assertions still run.
- [ ] AC3 `tests/testthat/test-axes-certificate.R` reddens, rather than passing
      green, when its certificate case list is emptied.
- [ ] AC4 Raising `axes_certificate_safety_factor` from 10 to 100 reddens at
      least one assertion in the packaged suite.
- [ ] AC5 `exact_oracle.R` reports the number of ratios it formed rather than
      the number of `cert_line()` calls it made, and `sweep_ok` and `reach_ok`
      are each replaced by a form derived from the values their own loop
      collects, so neither can report PASS on an empty domain; each of the two
      is shown to fail with its domain emptied.
- [ ] AC6 `devtools::test()` clean; `devtools::document()` no diff and no
      unresolved-link warning at pinned `cli.width`;
      `devtools::check(args = "--no-manual")` 0 errors / 0 warnings / 0 notes.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6

## Tasks

- [x] T1 Extend `exact_oracle.py` to emit each case's exact `v` and `u` as
      hi/lo double pairs; commit them in the test file beside the existing
      frozen figures.
- [x] T2 Rewrite `cert_skip_unless_reproduced()` to compute each case's true
      relative error on the running machine from the committed exact pair, and
      drop the shipped-pricing half of the precondition; run the AC2
      perturbation and record the skip count.
- [x] T3 Add the non-empty-domain assertion; prove it by emptying the case
      list, then restore.
- [x] T4 Run the safety-factor plant against the packaged suite; record which
      assertions redden, revert and verify the tree clean.
- [x] T5 Fix `cert_n` to count formed ratios; replace `sweep_ok` and
      `reach_ok` with forms derived from their loops' collected values; empty
      each domain in turn and record the failure.
- [ ] T6 Run the profile's verify and consistency-gate slot. Then, at the
      review gate — where the PR and therefore the CI run exist — read that run
      and record which platforms the bracket asserted on: a gate observation,
      not a criterion.

## Work log

- 2026-08-30: created by /milestone-plan.
- 2026-08-30: criteria audit ran in REDUCED mode (declared internal tier), one fresh-context [O] reader that authored none of the criteria, as part of the joint M113/M114/M115 run. Two findings fixed before writing: AC4's "packaged suite **or** the oracle run" disjunction let the devel script carry the promise this milestone exists to restore, and was narrowed to the packaged bracket; AC3's recording and restoration clauses moved to T3 as instrument bookkeeping. Its note that no criterion verifies the title's off-macOS claim is deliberate and recorded below.
- 2026-08-30: the internal-tier criteria standard is why AC2 is scoped to this machine rather than to the three CI platforms: a demonstration family spanning environment boundaries is itself a finding at that tier. The cross-platform observation is T6's gate step. Residual risk recorded: a green local AC2 with the CI run unread would leave the milestone's own goal unverified.
- 2026-08-30: plan gate chose retiring the precondition's shipped-pricing half via RR21 B3's exact emission over widening the frozen floor by a platform tolerance, because M108's mini gate measured a 100x slack as exceeding the 10x the dropped-safety-factor plant moves, so the tolerance would retire that plant's coverage; falsified by the exact hi/lo pair proving insufficient to recover a case's true error.
- 2026-08-30: plan gate chose simplifying `sweep_ok` and `reach_ok` to forms derived from their loops' collected values over adding a `cert_ok`-style accumulator count, on the user's answer at the checker-regress question, which took the subtractive option; falsified by the derived form proving unable to express either flag's condition.
- 2026-08-30: T1 — `exact_oracle.py` emits each case's exact `v`, `v_naive` and `u` as hi/lo hex double pairs; `exact_oracle.R` parses them and, under `CERT_EMIT=1`, prints a paste-ready `cert_frozen` block carrying matrix and exact values from one construction, so the two cannot describe different matrices. `v_naive` travels beside `v` because it is the `fiml_ratio` field's denominator and AC2 brackets all three fields.
- 2026-08-30: T2 — the precondition's shipped-pricing half is gone; `cert_true_error()` measures the running machine's own relative error against the committed exact pairs, and `cert_bracket()` asserts on both branches (at the floor: the machine's error is below what the floor certifies; above it: the [1, 1e3] bracket), so no case can report PASS having asserted nothing. The M113 closed-form case's `fiml_ratio` skip is replaced by the same two-branch form, closing the second surface of the platform-reach gap.
- 2026-08-30: AC2 demonstration — with `axes_v_pricing()`'s corrected arm perturbed by a relative 3e-13, the pre-M115 test file skipped all six cases naming the shipped pricing, while the new file reported zero skips and every bracket assertion ran (its four failures are the two dyadic closed-form cases, which the plant makes non-exact by construction). Plant reverted, `devtools::test()` clean at FAIL 0 / SKIP 1, that skip pre-existing in `test-axes-scaled-fit.R`.
- 2026-08-30: T3 — the non-empty-domain assertion (`AC3: the anchor case list is not empty`) shipped in the T1-T2 commit, since it lives in the same helper block; proved here by replacing `cert_anchors()`'s body with `list()`: it reddens with two failures naming the expected length and the expected ids, where before the change an emptied list generated no per-case tests at all and the file reported PASS. Two further errors surfaced in the n-invariance and planted-perturbation tests, which index the list directly. List restored, file green.
- 2026-08-30: T4 — the safety-factor plant. Before this milestone nothing reddened: every assertion mentioning the factor computed its expectation from `axes_certificate_safety_factor` itself, so estimate and expectation moved together. Writing 10 down at the three sites (`cert_floor`, and `f` in the planted-perturbation test) makes `axes_certificate_safety_factor <- 100` fail 13 assertions in 3 tests, all in `test-axes-certificate.R`: 8 in the planted-perturbation test (the `se` and `cval` upper bounds at each of the four deltas), 3 in the dyadic closed-form test and 2 in the quotient closed-form test (each field asserted at the floor). Plant reverted, tree clean against HEAD, file green.
- 2026-08-30: T5 — `cert_n` now increments where the ratio is formed, not on entry to `cert_line()`, so a case going exact would report 17 of 18 and redden instead of printing eighteen comparisons it had not made; `sweep_ok` and `reach_ok` are derived from the ratio vectors their own loops collect, checked against written-down counts (3 and 5) rather than against the domain's own length, which would go on matching with both emptied. The two verdict lines now name how many ratios they compared. Measured: with both domains emptied the pre-M115 accumulator form printed SWEEP PASS and REACHABLE PASS having compared nothing, while the new form prints `0 of 3` FAIL and `0 of 5` FAIL and the script exits 1. Emptying the reachable domain alone also drops the certificate count to 3 of 18, FAIL. Domains restored, script exits 0.
- 2026-08-30: minor amendment to T6 — the CI read is moved to the review gate. `R-CMD-check.yaml` triggers only on push to the default branch or on `pull_request`, and `/milestone-review` is what opens the PR, so no CI run exists while implement is running. Nothing in AC6 or any other criterion moves; the CI read was already recorded as a gate observation rather than a criterion.
- 2026-08-30: `Depends on: M113` because M113 extends the oracle's certificate case list, which T1 and T2 both read.

## Decisions

## Review
