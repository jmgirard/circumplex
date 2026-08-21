# M98: Close the parked norms-audit findings by subtraction

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP5, GP2, GP6, GP7
- **Branch/PR:** `m98-norms-audit-parked-findings` / [PR #127](https://github.com/jmgirard/circumplex/pull/127)

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

- [x] AC1 — `tests/testthat/test-norms-audit-manifest.R` no longer asserts a
      walked site's field set with `expect_setequal()`, and the assertion
      replacing it is sensitive to a repeated name. Evidence: with the
      replacement in place, a planted `norms_audit_abort_sites()` whose returned
      site carries a fourth element named `key` reddens that assertion, while
      the unplanted file is green. The plant is applied to a committed file, one
      mutant per invocation, restored by copy, with the file's blob hash
      re-verified after restore.
- [x] AC2 — `NORMS_AUDIT_VERDICT` in `tests/testthat/helper-norms-audit-script.R`
      no longer carries the `are not all TRUE` alternative, and the deletion is
      measured to fail closed rather than open: for the message R raises from
      the stated call `stopifnot(c(TRUE, FALSE))` under `LANGUAGE=C`,
      `audit_key_matches("stopifnot", "c(TRUE, FALSE)", msg)` returns FALSE
      after the deletion, with the message and the verdict recorded verbatim in
      the work log.
- [x] AC3 — one `cairn/DECISIONS.md` entry records all four dispositions of this
      milestone — the F12 repair, the F4 deletion, the F11 decline and the M80-F1
      decline — each with its rationale and its class of reopening evidence, and
      it states its relation to D-042 and D-043.
- [x] AC4 — the two ROADMAP candidate rows carrying these findings are rewritten
      so that F12, F4, F11 and the M80 note-only `sample`-cell finding — those
      four and no others — are each struck as closed by this milestone or left
      standing with a pointer to the AC3 entry; every other finding named in
      either row is left as it stands.
- [x] AC5 — non-comment `expect_` occurrences do not increase in the files this
      branch touches under `tests/` and `data-raw/`: over the files listed by
      `git diff --name-only master...HEAD -- tests data-raw`, the count from
      `grep -v '^[[:space:]]*#' | grep -o 'expect_' | wc -l` is no higher at HEAD
      than at `master`, with both counts recorded in the work log alongside the
      whole-file counts including comment lines.
- [x] AC6 — `Rscript -e 'devtools::test()'` is green, and the number of skips
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

- [x] **T1** — Replace the `expect_setequal(names(s), ...)` assertion at
      `tests/testthat/test-norms-audit-manifest.R:73` with an order-normalised
      exact one, and update the comment above it that credits the field-set
      assertion. Then plant the fourth-element mutant in
      `tests/testthat/helper-norms-audit-script.R`'s `norms_audit_abort_sites()`,
      run the one file, record red; restore by copy and re-verify the blob hash
      (M82/M88: `git checkout --` restores from the index, so never use it).
- [x] **T2** — Delete `|are not all TRUE` from `NORMS_AUDIT_VERDICT`
      (`helper-norms-audit-script.R`), rewriting the comment above it to state
      why the alternative is gone and that an added vectorized guard fails
      closed. Measure AC2's fail-closed call and record its message verbatim.
- [x] **T3** — Write the single `cairn/DECISIONS.md` entry (D-045) with the four
      dispositions, each carrying rationale and reopening evidence, and its
      relation to D-042 (whose "explicitly insufficient" clause is the ground
      for two of them) and D-043.
- [x] **T4** — Rewrite the four named findings' text in the M88 and M80 ROADMAP
      candidate rows to point at D-045, leaving every other finding in those
      rows untouched.
- [x] **T5** — `Rscript -e 'devtools::test()'` plus the AC5 `expect_` counts and
      the AC6 skip-line comparison against the branch point; record all of it.

## Work log

- 2026-08-20: created by /milestone-plan.
- 2026-08-20: implement started on `m98-norms-audit-parked-findings`, cut from master at `f57d0127`.
- 2026-08-20: branch-point baseline `devtools::test()`: `[ FAIL 0 | WARN 5 | SKIP 3 | PASS 8395 ]`; all three skips are `test-axes-scaled-fit.R:536/922/1241`, so the eight `test-norms-audit-*.R` files skip nothing at the branch point (AC6 comparison figure).
- 2026-08-20: T1 — `expect_setequal(names(s), ...)` replaced by `expect_identical(sort(names(s)), c("binding", "key", "kind"))` in `test-norms-audit-manifest.R`; sorted rather than order-pinned so a harmless reordering of the walk's own `list()` cannot redden a test about the field set.
- 2026-08-20: T1 mutation — committed `helper-norms-audit-script.R` (blob `50fedd23`) planted with a fourth element `key = "MUTANT"` in `norms_audit_abort_sites()`'s `stop` branch (blob `9656d7b0`); the file went red on the AC1 assertion alone, exceeding testthat's default 10-failure cap, and no other assertion of the file failed. Restored by copy, blob re-verified `50fedd23`, tree clean, file green at `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 44 ]`.
- 2026-08-20: T2 — `NORMS_AUDIT_VERDICT` is now `"is not TRUE"`; the `are not all TRUE` alternative is deleted, with the comment above it rewritten to record why and what restoring it would cost.
- 2026-08-20: T2 fail-closed measurement (AC2), under `LANGUAGE=C`/`LC_MESSAGES=C` after the deletion. `tryCatch(stopifnot(c(TRUE, FALSE)), error = conditionMessage)` gives verbatim `c(TRUE, FALSE) are not all TRUE`; `norms_audit_stopifnot_stem()` returns that message unchanged as its stem with `truncated = FALSE` (the pattern no longer strips the plural verdict), and `audit_key_matches("stopifnot", "c(TRUE, FALSE)", msg)` returns **FALSE** — the matcher refuses a site's own genuine message rather than accepting a stranger's.
- 2026-08-20: full `devtools::test()` after T1+T2: `[ FAIL 0 | WARN 5 | SKIP 3 | PASS 8395 ]`, identical to the branch-point baseline in every field.
- 2026-08-20: T3 — D-045 appended to `cairn/DECISIONS.md`: one entry, four numbered dispositions, each with rationale and its own reopening class, applying D-042 rather than superseding it and leaving D-043's identity unchanged.
- 2026-08-20: T4 — the M88 and M80 candidate rows rewritten to strike F11/F12/F4 and the M80 note-only finding and point each at D-045; the same-binding-twin conflation, the M84/M85 graduations and every other finding in both rows left byte-untouched.
- 2026-08-20: T4 correction — the M80 row described the finding as "the `note-only-sample` emitter leaves the report's `sample` column `NA`". Measured against `data-raw/norms-audit-coverage.csv`: all 14 note-only rows carry `sample = "—"`, and the cells `NA` across all 14 of them are `field`, `scale` and `tag` — not `field` alone (`coverage_rows()` defaults every cell the emitter does not pass, and `tag_or_na()` returns `NA` for an untagged block). The mechanism is that the emitter (`audit-norms.R:927-945`) writes no cell at all for the `anchor` its dedupe key discriminates on, so an anchor-differing pair emits identically. The row and D-045 finding 4 both state it that way now. `cairn/milestones/archive/M80-norms-audit-report-schema.md:24` carries the original wording and is left as written — it is history (IP4), not a live claim.
- 2026-08-20: AC5 amended at a mini gate (substantive; Jeff chose the amendment over rewording the comment or dropping the criterion). As written it counted `expect_` anywhere in the file, so the new comment naming `expect_setequal()` — the record of what was wrong — read as an added assertion: 14 vs 13, while assertions were 12 vs 12. The procedure now excludes comment lines and both numbers are recorded.
- 2026-08-20: AC5 amendment — the fresh-context [O] reader (reduced mode, internal tier) returned a second finding against the wording I proposed: the headline "the milestone subtracts rather than adds on the test surface" quantified over the whole test surface, which no `expect_` count enumerates (assertions can be added in helpers, in loops, or in files this branch does not touch, while the token count falls). The headline is narrowed to what the count settles. The same overclaim was in the pre-gate AC5 and round 2 of the plan-time audit passed it.
- 2026-08-20: AC5 amendment — narrowed wording re-entered the reader once, per the one-fix rule: clean on both questions.
- 2026-08-20: T5/AC5 — files in scope per `git diff --name-only master...HEAD -- tests data-raw`: `tests/testthat/helper-norms-audit-script.R` and `tests/testthat/test-norms-audit-manifest.R`. Non-comment `expect_` occurrences: master 12, HEAD 12 — no increase. Whole-file counts including comment lines: master 13, HEAD 14; the one added occurrence is the comment naming `expect_setequal()` as the assertion that was wrong.
- 2026-08-20: T5/AC6 — final `devtools::test()`: `[ FAIL 0 | WARN 5 | SKIP 3 | PASS 8395 ]`, every field identical to the branch-point baseline. The three skips are `test-axes-scaled-fit.R:536/922/1241`; the eight `test-norms-audit-*.R` files skip nothing, unchanged from the branch point.
- 2026-08-20: pre-review `devtools::check(args = "--no-manual")`: `Status: OK`, 0 errors / 0 warnings / 0 notes, 7m35s; `checking tests ... OK` (377s). `--no-manual` is CLAUDE.md's documented form, so the PDF-manual step did not run — no roxygen changed on this branch, so nothing on that surface moved.
- 2026-08-20: all tasks complete; status → review.
- 2026-08-20: [O] reduced criteria audit (internal tier), round 1 over the pre-gate draft: two findings — AC2's "in both its untruncated and its truncated form" was a per-rendering enumeration, AC4's anchor-pair promise a proxy for all anchor-differing pairs; both had one clear answer and were narrowed before the gate.
- 2026-08-20: [O] reduced criteria audit, round 2 over the final post-gate wording: one finding — AC4's "no finding named in either row is left with no disposition" quantified past its own hand-list, the M88 row also naming the same-binding-twin conflation and pointing at a fuller scored list; narrowed to the four named findings, AC1/AC2/AC3/AC5/AC6 clean.
- 2026-08-20: plan gate chose closing F4 by deleting the unreachable `are not all TRUE` alternative over adding a test that exercises it, because no shipped site can raise it and its removal fails closed (an unstripped plural verdict makes `startsWith()` fail); falsified by a vectorized `stopifnot()` condition entering `data-raw/audit-norms.R`, which would want the alternative back.
- 2026-08-20: plan gate chose declining M88 F11 over splitting the two constant-only assertions out of the `skip_if_not()`, because they read the committed `NORMS_AUDIT_MANIFEST` constant, whose verdict cannot vary by machine, so running them under `R CMD check` adds coverage optics and no detection; falsified by the manifest ceasing to be a committed constant — generated at test time, or read from a source `R CMD check` can also see.
- 2026-08-20: plan gate chose declining M80 F1 over carrying the `anchor` into `COVERAGE_COLUMNS` (and over an emit-time refusal), because none of the 14 committed note-only rows has the shape and the fix widens an internal checker's promise for a case that has never occurred; falsified by a source note citing one sample to two different tables actually arriving.
- 2026-08-20: plan gate chose subtraction over hardening for the milestone as a whole, on the checker-regress shape plus D-042's "explicitly insufficient" clause; falsified by an abort site the manifest cannot see appearing in the audit script (D-042's own reopening class).

## Decisions

## Review

Fresh evidence, gathered at review on the post-fix branch; every measurement
re-run rather than carried over from implement.

- **AC1 ✓** — `expect_setequal()` is gone from the field-set assertion
  (`test-norms-audit-manifest.R:83`). Plant against the committed helper
  (`2ff55f7a` → `f73fd47e`), a fourth element whose name repeats `key`:
  `[ FAIL 30 | WARN 0 | SKIP 0 | PASS 14 ]`, all 30 failures on
  `sort(names(s), na.last = TRUE)` and no other assertion of the file failing;
  30 is the site count the walk yields for the `stop` branch. Run with
  `set_max_fails(Inf)` so the later blocks actually ran — the implement-time
  run hit the default 10-failure cap and could not establish that (review F3).
  Restored by copy, blob re-verified `2ff55f7a`, tree clean, unplanted file
  green at `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 44 ]`.
- **AC2 ✓** — `NORMS_AUDIT_VERDICT` is `(is not TRUE)`; the plural alternative
  is gone. Under `LANGUAGE=C`, `stopifnot(c(TRUE, FALSE))` raises
  `c(TRUE, FALSE) are not all TRUE`, the stem comes back as that message
  unchanged with `truncated = FALSE`, and
  `audit_key_matches("stopifnot", "c(TRUE, FALSE)", msg)` is **FALSE** — fails
  closed. Scalar control: `is.data.frame(1)` still matches its key (TRUE), so
  the deletion cost the shipped sites nothing.
- **AC3 ✓** — D-045 carries 4 numbered dispositions and 4 `*Reopens:*` clauses,
  cites D-042 three times and D-043 once, and states the relation to each
  ("applied, not superseded"; D-043's identity unchanged).
- **AC4 ✓** — both rows rewritten, F12/F4/F11 and the M80 note-only finding each
  struck with a D-045 pointer. Neighbours intact: the same-binding-twin clause
  and its own promotion condition survive byte-for-byte, as do the M84/M85
  graduations.
- **AC5 ✓** — files in scope: `helper-norms-audit-script.R`,
  `test-norms-audit-manifest.R`. Non-comment `expect_` master 12, HEAD 12 — no
  increase. Whole-file including comments: master 13, HEAD 15; the three added
  occurrences are all comment text naming the assertions this milestone
  reasoned about.
- **AC6 ✓** — `[ FAIL 0 | WARN 5 | SKIP 3 | PASS 8395 ]`, every field identical
  to the branch-point baseline. The 3 skips are `test-axes-scaled-fit.R`
  536/922/1241; the eight `test-norms-audit-*.R` files skip nothing. The diff
  adds and removes zero `skip_if_not` lines, so no skip gate moved.

**Consistency gate.** `cairn_validate`: all 16 checks PASS, advisories 47 (M7's
pre-M28 work-log WARNs, IP4 history). Toolchain slot: `document()` no diff and
zero `resolve link` warnings at `cli.width = 500`; `pkgdown::check_pkgdown()`
"No problems found"; `devtools::check(args = "--no-manual")` `Status: OK`,
0 errors / 0 warnings / 0 notes, `checking tests ... OK`; master watches — newest
push run concluding a verdict on `R-CMD-check.yaml` is success (2026-08-19, M97;
the M96 run is `cancelled`, not a verdict) and likewise `test-coverage.yaml`;
both alert audits exit 0. NEWS: no entry owed — `git diff --name-only` over
`R/ src/ man/ NAMESPACE DESCRIPTION vignettes/` is empty, so nothing
user-visible changed. No principle changed, so `cairn_impact` was skipped.

**Independent review — three lenses, 11 findings, 6 actioned, 5 logged.**
No finding met the return floor: none demonstrates an acceptance criterion
failing inside its named procedure's domain, and none is a defect in what the
package does for its users.

- **F1 ([O], most severe) — fixed.** `sort()` defaults to `na.last = NA`, which
  *discards* NA names, so a fourth field named `NA` sorted back to the same
  three-element vector and passed — strictly weaker than the `expect_setequal()`
  it replaced, which rejected that shape. Measured both ways before fixing.
  `na.last = TRUE` now keeps it; mutation-proved with an NA-named fourth field
  (30 failures, all on the assertion, restore hash-verified). AC1 as written
  promised sensitivity to a *repeated* name, which held throughout — this was a
  new weakness introduced beside the repair, not the repair failing.
- **F2 ([O]) — fixed.** The constant lost its grouping parentheses. Both readers
  interpolate it into a larger `$`-anchored pattern, so restoring the deleted
  alternative the obvious way (`is not TRUE|are not all TRUE`) would bind the
  alternation across the whole pattern: measured, a plain untruncated plural
  message then matches the *truncation* detector, which waives the stem
  floor — the 2026-08-14 incident recorded directly above it. The comment
  recommending restoration was walking a maintainer into it. Parens restored
  around the single alternative, with the trap named in the comment.
- **F3 ([O]) — fixed.** The implement-time mutation run hit testthat's default
  10-failure cap, which aborts the file, so "no other assertion failed" was
  argued rather than measured. Re-run under `set_max_fails(Inf)`; the claim now
  holds as a measurement (30 failures, all on the one assertion, 14 passes).
- **F4 ([O]) — fixed.** The M80 row's promotion condition ("promote into
  whichever milestone next opens `audit_norms()`") had been replaced by D-045's
  narrower reopening clause, while the milestone's own Scope says the row keeps
  its condition. The original condition is restored, with D-045's clause added
  beside it rather than in place of it.
- **F5 ([O]) — fixed.** The T4 correction named `field` as the NA cell; measured,
  `field`, `scale` **and** `tag` are NA across all 14 note-only rows. The
  reviewer's own correction named two of the three; the work log now names all
  three and why.
- **F6 ([O]) — fixed.** D-045 and the ROADMAP row said "a `unique()` over the
  CSV drops one", attributing an operation the audit never performs (the only
  dedupe is `duplicated(key)` at emit time). Reworded to attribute it to a
  downstream reader. Wording inherited from M80, but it now sat in a decision
  entry.
- **F7 ([O]) — rejected, out of scope.** The empty `## Decisions` heading is the
  milestone template's own stub; the archive summary replaces the file entirely.
- **[S] history lens — no findings.** Independently reproduced the fail-closed
  behaviour, confirmed the plural alternative was added in M81 as general
  grammar accommodation and never exercised by any site, confirmed the M83/M88
  truncation-floor measurements are unrelated to the singular/plural
  alternative and their comment text survives verbatim, and confirmed the M80
  wording correction against the code. Recorded one under-articulation, already
  covered by F2's fix.
- **[S] prior-review lens — 2 findings, both logged, not actioned.** (i) F11's
  decline sits in tension with the LESSONS line "an always-skipping
  `skip_if_not(file.exists(...))` is the same trap — split so a runtime half
  still runs on CRAN (M69, M70)", and D-045 declines without citing it. The
  distinction is real — M69/M70's skipped guard exercised computation that could
  vary by environment, while these two read a committed constant — but it is
  worth the maintainer's explicit sign-off rather than silent drift.
  **Signed off at the approval gate; D-045 finding 3 now cites the lesson and
  states why its shape does not reach this case.** (ii) M80
  F1's decline rests on "no committed note has the shape today", the
  inference-from-current-data shape the M86 lesson warns about; mitigated by
  D-045 recording a reopening trigger, and the blast radius is a maintainer's
  report, not a shipped value. GitHub probe returned `[]` — no inline review
  threads exist, so that surface contributed nothing, as in M91.
