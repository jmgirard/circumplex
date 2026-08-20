# M98: Close the parked norms-audit findings by subtraction

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP5, GP2, GP6, GP7
- **Branch/PR:** —

## Goal

Dispose of the four findings parked against the norms-audit test apparatus so
that the apparatus ends smaller than it started.

## Scope

Surface tier: **internal** — `data-raw/audit-norms.R` is `.Rbuildignore`d and
ships to nobody, and its test apparatus verifies repo-internal artifacts (the
`data-raw/` norms tables and source notes), so no external consumer relies on
the deliverable.

**In:**
- M88 F12: `expect_setequal()` on a walked site's field set is multiplicity-
  blind (`test-norms-audit-manifest.R:73`) — repaired in place.
- M88 F4: `NORMS_AUDIT_VERDICT`'s `are not all TRUE` alternative
  (`helper-norms-audit-script.R`) is unreachable from every shipped site, all
  three positional `stopifnot()` conditions being scalar — deleted, not tested.
- M88 F11 (manifest-side assertions skipping under `R CMD check`) and M80 F1
  (the note-only emitter dropping the `anchor` its dedupe key discriminates on,
  `audit-norms.R:927-945`) — declined, with rationale and reopening evidence
  recorded.
- One `cairn/DECISIONS.md` entry carrying all four dispositions.

**Out:**
- Splitting F11's constant-only assertions out of the skip → declined here, in
  the AC3 entry; reopens on that entry's stated evidence.
- Carrying the note row's `anchor` into `COVERAGE_COLUMNS` → declined here, in
  the AC3 entry; the ROADMAP row keeps its promotion condition.
- The six parked M75-family findings against the instrument-tagged-block
  machinery → stay ROADMAP candidates on their stated condition (the audit
  extending to a new instrument).
- The same-binding-twin conflation and the other M88-row findings → untouched,
  their rows left as they stand.

## Acceptance criteria

- [ ] AC1 — `tests/testthat/test-norms-audit-manifest.R` no longer asserts a
      walked site's field set with `expect_setequal()`, and the assertion
      replacing it is sensitive to a repeated name. Evidence: with the
      replacement in place, a planted `norms_audit_abort_sites()` whose returned
      site carries a fourth element named `key` reddens that assertion, while
      the unplanted file is green. The plant is applied to a committed file, one
      mutant per invocation, restored by copy, with the file's blob hash
      re-verified after restore.
- [ ] AC2 — `NORMS_AUDIT_VERDICT` in `tests/testthat/helper-norms-audit-script.R`
      no longer carries the `are not all TRUE` alternative, and the deletion is
      measured to fail closed rather than open: for the message R raises from
      the stated call `stopifnot(c(TRUE, FALSE))` under `LANGUAGE=C`,
      `audit_key_matches("stopifnot", "c(TRUE, FALSE)", msg)` returns FALSE
      after the deletion, with the message and the verdict recorded verbatim in
      the work log.
- [ ] AC3 — one `cairn/DECISIONS.md` entry records all four dispositions of this
      milestone — the F12 repair, the F4 deletion, the F11 decline and the M80-F1
      decline — each with its rationale and its class of reopening evidence, and
      it states its relation to D-042 and D-043.
- [ ] AC4 — the two ROADMAP candidate rows carrying these findings are rewritten
      so that F12, F4, F11 and the M80 note-only `sample`-cell finding — those
      four and no others — are each struck as closed by this milestone or left
      standing with a pointer to the AC3 entry; every other finding named in
      either row is left as it stands.
- [ ] AC5 — the milestone subtracts rather than adds on the test surface: over
      the files listed by `git diff --name-only master...HEAD -- tests data-raw`,
      the total count of `expect_` occurrences reported by
      `grep -o 'expect_' | wc -l` is no higher at HEAD than at `master`, with
      both counts recorded in the work log.
- [ ] AC6 — `Rscript -e 'devtools::test()'` is green, and the number of skips
      reported for the files listed by `ls tests/testthat/test-norms-audit-*.R`
      is unchanged from the same command run at the branch point, with both
      runs' skip lines recorded in the work log.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T5

## Tasks

- [ ] **T1** — Replace the `expect_setequal(names(s), ...)` assertion at
      `tests/testthat/test-norms-audit-manifest.R:73` with an order-normalised
      exact one, and update the comment above it that credits the field-set
      assertion. Then plant the fourth-element mutant in
      `tests/testthat/helper-norms-audit-script.R`'s `norms_audit_abort_sites()`,
      run the one file, record red; restore by copy and re-verify the blob hash
      (M82/M88: `git checkout --` restores from the index, so never use it).
- [ ] **T2** — Delete `|are not all TRUE` from `NORMS_AUDIT_VERDICT`
      (`helper-norms-audit-script.R`), rewriting the comment above it to state
      why the alternative is gone and that an added vectorized guard fails
      closed. Measure AC2's fail-closed call and record its message verbatim.
- [ ] **T3** — Write the single `cairn/DECISIONS.md` entry (D-045) with the four
      dispositions, each carrying rationale and reopening evidence, and its
      relation to D-042 (whose "explicitly insufficient" clause is the ground
      for two of them) and D-043.
- [ ] **T4** — Rewrite the four named findings' text in the M88 and M80 ROADMAP
      candidate rows to point at D-045, leaving every other finding in those
      rows untouched.
- [ ] **T5** — `Rscript -e 'devtools::test()'` plus the AC5 `expect_` counts and
      the AC6 skip-line comparison against the branch point; record all of it.

## Work log

- 2026-08-20: created by /milestone-plan.
- 2026-08-20: [O] reduced criteria audit (internal tier), round 1 over the pre-gate draft: two findings — AC2's "in both its untruncated and its truncated form" was a per-rendering enumeration, AC4's anchor-pair promise a proxy for all anchor-differing pairs; both had one clear answer and were narrowed before the gate.
- 2026-08-20: [O] reduced criteria audit, round 2 over the final post-gate wording: one finding — AC4's "no finding named in either row is left with no disposition" quantified past its own hand-list, the M88 row also naming the same-binding-twin conflation and pointing at a fuller scored list; narrowed to the four named findings, AC1/AC2/AC3/AC5/AC6 clean.
- 2026-08-20: plan gate chose closing F4 by deleting the unreachable `are not all TRUE` alternative over adding a test that exercises it, because no shipped site can raise it and its removal fails closed (an unstripped plural verdict makes `startsWith()` fail); falsified by a vectorized `stopifnot()` condition entering `data-raw/audit-norms.R`, which would want the alternative back.
- 2026-08-20: plan gate chose declining M88 F11 over splitting the two constant-only assertions out of the `skip_if_not()`, because they read the committed `NORMS_AUDIT_MANIFEST` constant, whose verdict cannot vary by machine, so running them under `R CMD check` adds coverage optics and no detection; falsified by the manifest ceasing to be a committed constant — generated at test time, or read from a source `R CMD check` can also see.
- 2026-08-20: plan gate chose declining M80 F1 over carrying the `anchor` into `COVERAGE_COLUMNS` (and over an emit-time refusal), because none of the 14 committed note-only rows has the shape and the fix widens an internal checker's promise for a case that has never occurred; falsified by a source note citing one sample to two different tables actually arriving.
- 2026-08-20: plan gate chose subtraction over hardening for the milestone as a whole, on the checker-regress shape plus D-042's "explicitly insufficient" clause; falsified by an abort site the manifest cannot see appearing in the audit script (D-042's own reopening class).

## Decisions

## Review
