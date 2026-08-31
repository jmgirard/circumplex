# M118: Close three ways the certificate suite can pass without checking

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP3
- **Branch/PR:** `m118-certificate-suite-pass-without-checking`

## Goal

Make three currently-green states of `tests/testthat/test-axes-certificate.R`
red: a run that priced no case at all, a shipped pricing regression at the
dyadic configuration, and a drift between the harness's written-down safety
factor and the package's own.

## Scope

**Surface tier: internal.** The deliverable is the discrimination of an in-repo
test suite; no exported function, return structure or printed output changes,
and no external consumer of the package relies on any of it.

**In:**
- A detector that fails a run of `test-axes-certificate.R` in which every
  priced case skipped. `cert_true_error()` skips a case whose anchor matrix
  this machine does not build bit for bit, and six such skips today leave the
  file green with nothing asserted; only the review gate's read of CI catches
  it.
- Shipped-route assertions at the closed-form dyadic configuration
  (`test-axes-certificate.R:757-806`), at a tolerance written down as a
  literal. M116 deleted three `expect_identical()` checks there because they
  pinned every measured error to exactly zero and so reduced both bracket
  branches to the identities they replaced; a tolerance-based pin catches a
  shipped regression without demanding bit-identity.
- An assertion that `axes_certificate_safety_factor` is 10, which is what the
  harness's `cert_floor` (`:270`) and `cert_ceiling` (`:291`) are written
  against. Nothing asserts it today, so the two can diverge silently.
- Recording, in `cairn/DESIGN.md`, the items this milestone accepts rather
  than fixes, split between **Known fragilities** (keeps a ROADMAP row) and
  **Accepted limitations** (no row), and dispositioning the candidate row that
  has carried findings from M108, M110, M111, M113, M115 and M116 so it is not
  extended a seventh time.

**Out:**
- A hand-derived exact `cval` for the quotient configuration
  (`:809-939`), which would let that field be checked there too → recorded as
  an accepted limitation at T4; the field is already priced against exact
  values at the five anchors and counterexample B, so the gap is one
  configuration wide.
- RR21 recommendation 5, surfacing the certificate's estimate on computed
  fits rather than only refused ones → stays a ROADMAP candidate; it adds a
  field to an exported return under GP4 and wants its own gate.
- Any change to the certificate's mechanism, estimand or constants, which
  D-051, D-053 and D-054 fix → not reopened here; this milestone changes only
  what the suite can distinguish.
- The cosmetic and latent residue the candidate row carries → relocated at T4,
  not fixed.

## Acceptance criteria

- [ ] **AC1** — A run of `tests/testthat/test-axes-certificate.R` in which no
      priced case is asserted fails. A test iterating the cases
      `cert_anchors()` enumerates, plus counterexample B, reads each case's
      recorded disposition and fails when the number priced is zero.
- [ ] **AC2** — At the closed-form dyadic configuration,
      `test-axes-certificate.R` carries one assertion per shipped value —
      `axes_v_pricing()$corrected`, `axes_v_pricing()$naive` and
      `axes_u_pricing()` — against its hand-derived fraction (97/128, 2, 5/8)
      at a relative tolerance of `4 * 2^-53` written as a literal, and each of
      the three reddens when its own shipped value alone is perturbed by
      `8 * 2^-53` relative.
- [ ] **AC3** — `test-axes-certificate.R` carries an assertion that
      `axes_certificate_safety_factor` is identical to 10, and that assertion
      reddens when the constant is set to 100.
- [ ] **AC4** — `Rscript -e 'devtools::test()'` clean (the profile's verify
      slot).

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T1, T2, T3

## Tasks

- [x] **T1** — Record each priced case's disposition as `cert_true_error()`
      (`:296-351`) runs — priced, or skipped with its reason — in a
      file-local environment, and add a test after the per-case tests that
      fails when zero were priced. The per-case tests are one `test_that()`
      each (`:440-443`) precisely so they skip independently, so the registry
      is written from inside `cert_true_error()` on both paths. Prove it by
      forcing the matrix check at `:311` to mismatch for every case and
      observing the new test red while the per-case tests skip.
- [x] **T2** — Add the three tolerance-based shipped-route assertions at
      `:797-800`, against 97/128, 2 and 5/8 at a literal `4 * 2^-53` relative.
      Do not read the tolerance from `cert_floor`: an expectation defined by
      the harness constant it sits beside weakens whenever that constant is
      raised. Verify each assertion reddens under an `8 * 2^-53` perturbation
      of its own shipped value alone, and that the other two stay green.
- [x] **T3** — Assert `axes_certificate_safety_factor`
      (`R/axes_certificate.R:430`) is identical to 10, beside the comment at
      `:264-270` that explains why `cert_floor` writes the factor down instead
      of reading it. Verify by setting the package constant to 100 and
      observing this assertion red.
- [ ] **T4** — Write the accepted items into `cairn/DESIGN.md`: the missing
      `cval` check at the quotient configuration and any residue that could
      still justify work go under **Known fragilities** (`:71`); residue with
      no fix planned goes under **Accepted limitations** (`:80`).
- [ ] **T5** — Disposition the candidate row: delete the items T1–T3 fixed and
      the items T4 relocated, leaving only still-open promotion conditions and
      cross-references into the M108/M110/M111/M113/M115/M116 archives.
      `cairn/ROADMAP.md` is at 23,821 of its 24,000-byte budget (measured
      2026-08-31 by `wc -c`), so this task must leave it smaller.

## Work log

- 2026-08-31: created by /milestone-plan, promoting the behavior-guarding items out of the finding-absorbing candidate row per the records-hygiene rule that such a row is dispositioned rather than extended again.
- 2026-08-31: criteria audit ran in REDUCED mode (internal tier) in a fresh [O] reader that authored none of the criteria. It returned three findings, all fixed here before the gate: AC2 defined both its tolerance and its perturbation size by the harness's own `cert_floor`, so the promise would weaken whenever that constant rose (now a stated literal); AC3 promised reddening for "any other value" of the safety factor, an unbounded domain (now one stated alternative, 100); and a fourth criterion made two "every item" claims over a domain fixed by recollection and aimed them at tracking prose (descoped to T4/T5). It also corrected the DESIGN.md heading names — **Known fragilities** and **Accepted limitations**, not "Known issues".
- 2026-08-31: plan gate chose fixing the all-skip detector, the dyadic pin and the safety-factor pin over fixing only the all-skip detector, over also hand-deriving `cval`, and over planning nothing and closing the row outright, because the three chosen items each make a currently-green regression red at a cost that needs no new derivation; falsified by any of the three proving to need a hand derivation of its own.
- 2026-08-31: plan gate chose a written-down literal tolerance for the dyadic pin over a tolerance read from `cert_floor` and over leaving the configuration unpinned, because an expectation reading the constant it sits beside moves with that constant and notices nothing — the failure M115 AC4 already recorded for the floor itself; falsified by a machine committing a legitimate error above `4 * 2^-53` at a configuration where every intermediate is dyadic.
- 2026-08-31: plan gate chose accepting the quotient configuration's missing `cval` check over hand-deriving an exact `u` there and over an indirect worst-of assertion, because the field is already priced against exact values at six other configurations and a new hand derivation is its own correctness surface; falsified by a `cval` regression that the six priced cases miss.
- 2026-08-31: T1 — each case records its disposition (priced / skipped-with-reason / refused) into a file-local environment from inside `cert_true_error()`, and a test after the per-case tests fails when none was priced. Discrimination proved by forcing the matrix precondition to mismatch for every case: 6 skips, and the new test the only failure, its label naming each case's reason. `devtools::test()` 0 failures, 9194 passes.
- 2026-08-31: T2 — the dyadic configuration now pins `axes_v_pricing()$corrected`, `$naive` and `axes_u_pricing()` against 97/128, 2 and 5/8 at a literal `4 * 2^-53` relative tolerance, beside the brackets rather than in place of them. Each pin proved to redden alone under an `8 * 2^-53` relative perturbation of its own shipped value: one failure per run, at the perturbed value's own assertion, the other two green.
- 2026-08-31: T3 — `test-axes-certificate.R` now asserts `axes_certificate_safety_factor` is identical to 10, in a test placed beside the `cert_floor`/`cert_ceiling` comments that explain why neither reads it. Proved by setting the package constant to 100: the new assertion is among the failures, at its own line. `devtools::test()` 0 failures, 9198 passes.
- 2026-08-31: plan gate chose splitting the unfixed residue between DESIGN.md's Known fragilities and Accepted limitations over moving all of it to Accepted limitations and over leaving the row untouched, because the two headings already encode exactly the keeps-a-row / no-row distinction the residue splits on; falsified by an item under Accepted limitations later needing a row.

## Decisions

## Review
