<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M112: Withdraw the CAIS adult normative sample

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP5, GP2, GP4, GP7
- **Branch/PR:** m112-cais-adult-sample-withdrawal

## Goal

Remove `cais` sample 2 from the shipped roster before v2.0.0, on the ground
that an unusable sample should not ship — not on the metric question being
settled — leaving the refusal machinery and the transcription record intact.

## Scope

**Surface tier: user-facing** — the deliverable is shipped instrument data and
the documentation describing it.

**In:** Delete sample 2's rows from `data-raw/cais.R` and rebuild
`data/cais.rda`; keep D-040's roster-wide refusal and the disclosure's
usable-count clause working with their positive controls moved to constructed
off-metric objects; update `?cais`, NEWS.md, the two `data-raw/norms-audit-*`
CSVs and `cairn/references/norms-audit.md`; record the withdrawal in
`cairn/references/sodano2006.md` without touching its extracted-values block;
append the D-entry annotating D-040's withdrawal clause.

**Out:** Correcting the sample's values to sodano2006's IAS block → a fresh
gate under D-039's numeric-change carve-out, on a reply from Sodano or a second
source (ROADMAP row, reopening condition rewritten by T6). The `iis32` and
`ipipipc` unsourced-norms question → stays on its ROADMAP row, unchanged.
Any change to `norm_standardize()`'s refusal predicate itself → out; D-040
stands.

## Acceptance criteria

- [x] AC1: `cais$Norms[[1]]` carries rows for sample 1 only, and
      `cais$Norms[[2]]` carries exactly one row, with `Sample == 1`.
- [x] AC2: `norm_standardize(jz2017, scales = 2:9, instrument = cais,
      sample = 2)` errors with the unmatched-sample message — naming sample 2
      and reporting that the CAIS carries sample 1 — and not with the
      anchor-range message.
- [x] AC3: No shipped norm sample's octant mean falls outside its instrument's
      declared anchor range, over the sweep `anchor_range_violations()`
      performs (every name `shipped_instruments()` returns × every `Sample`
      value in that instrument's `Norms[[1]]`); the sweep's domain is asserted
      non-empty in the same test, so an empty roster cannot pass it (M108).
- [x] AC4: `norm_standardize()` still refuses any normative sample whose means
      leave its instrument's anchor range, and the standardization message's
      other-samples clause still counts only samples that would be accepted —
      both holding when no shipped sample exercises either path.
- [x] AC5: `git grep -inE 'adult sample|sample ?= ?2|sample 2|5\.19|6\.52|6\.14'
      -- R man NEWS.md vignettes README.md` returns no site describing the CAIS
      adult sample as shipped; `?cais` and NEWS.md's 2.0.0 section each state
      that the sample was withdrawn, why, and where its transcription survives.
- [x] AC6: The record of the withdrawn sample survives and agrees with the
      roster: `cairn/references/sodano2006.md`'s `<!-- audit-values-begin -->`
      … `<!-- audit-values-end -->` block is byte-identical to its
      pre-milestone content and the file records the withdrawal (date, ground,
      what a reply would reopen); and a fresh `data-raw/audit-norms.R` run over
      the post-removal roster reproduces the committed
      `norms-audit-ledger.csv` and `norms-audit-dispositions.csv`, neither of
      which contains a cais sample-2 row.
- [x] AC7: `devtools::test()` passes and `devtools::check(args =
      "--no-manual")` is clean (PROFILE.md verify slot), including a
      warning-free `devtools::document()`.

## Coverage

- AC1 → T2
- AC2 → T1, T2
- AC3 → T1, T3
- AC4 → T1, T3
- AC5 → T4, T6
- AC6 → T5
- AC7 → T6

## Tasks

- [x] T1: Tests first. Rewrite `tests/testthat/test-norms-anchor-range.R`:
      violation set expected empty, sweep domain asserted non-empty, and the
      two shipped-cais cases (refusal message, usable-count) rebuilt on
      constructed off-metric instrument objects — each shown to fail when its
      predicate is inverted, the inversion evidence recorded in the work log.
      Add the AC2 unmatched-sample case. Red before T2.
- [x] T2: Drop sample 2 from `cais_norms` and `cais_norms_src` in
      `data-raw/cais.R:95`-ish, recording the withdrawal and its ground in the
      script's comment block (IP5); rerun the script; verify the artifact by
      `load()`ing `data/cais.rda` directly, not via `load_all()` (LESSONS).
- [x] T3: Update the shipped-roster pins: `test-norms-kind.R:17` (16 → 15
      samples) and `:189`, `test-norms-audit-roster.R:46`,
      `test-norms-provenance.R:216`, `test-norms-disclosure.R:285`.
- [x] T4: Rewrite `?cais`'s adult-sample note (`R/instrument_data.R:5`) as a
      withdrawal note, and NEWS.md:193-202's "will be corrected or withdrawn"
      sentence as what happened; `devtools::document()`.
- [x] T5: Rerun `data-raw/audit-norms.R` to regenerate both CSVs; update
      `cairn/references/norms-audit.md`'s roster, citekey-map and
      reference-kind tables; append the withdrawal note to
      `cairn/references/sodano2006.md` leaving its extracted block untouched.
- [x] T6: Run AC5's grep; full `devtools::test()` and
      `devtools::check(args = "--no-manual")`; rewrite the ROADMAP cais candidate
      row's reopening
      condition from "promote on a reply" to "a reply re-adds corrected values
      under a fresh gate (D-039)", per D-052 (written at plan time).

## Work log

- 2026-08-29: created by /milestone-plan.
- 2026-08-29: AC6 and AC7 (provenance-record survival; audit-run/roster agreement) merged into one criterion after the >7-AC sizing tripwire fired — one promise about the post-removal records, re-read through the audit's bounded-promise, instrument and proportionality questions before being written. No scope was dropped.
- 2026-08-29: criteria audit ran in FULL mode (user-facing tier), self-read rather than by a fresh-context [O] reader — this session is instructed not to spawn subagents, and the deviation was disclosed at the gate. Three findings, all fixed before the questions: AC4 bound a test property (constructed controls, inversion evidence) and was narrowed to the deliverable behavior with the construction moved to T1; AC5 promised "every shipped surface" over an author-recalled pattern list and was narrowed to what its named grep returns plus two named sites; AC1's "rebuilt by running data-raw/cais.R" was a process claim and moved to T2. AC7's audit-run/roster agreement was considered under the instrument question and kept: IP5 makes roster-record agreement part of the shipped data's contract.
- 2026-08-29: plan gate chose withdrawal-on-unusability over keeping the sample refused until Sodano replies, because v2.0.0 has not shipped and D-040 itself names that as the cheap moment, while a "swapped" reply needs a fresh numeric gate whether or not the wrong values are still shipped; falsified by a reply or second source identifying the adult sample's metric, which would make this a correction rather than a withdrawal.
- 2026-08-29: plan gate chose keeping D-040's refusal with its controls moved to constructed objects over deleting the now-unexercised sweep, because the sweep is what would catch a future off-metric sample entering the roster; falsified by evidence that no path adds norms without passing through the audit.
- 2026-08-29: T1 tests written red. The violation set now expects `character(0)` with the sweep's domain asserted non-empty, the refusal and offending-scale cases are rebuilt on constructed off-metric objects (both the `Scale`- and `Abbrev`-labelled column names), and the AC2 unmatched-sample case is added. Inversion evidence, each run against the current tree: an empty sweep domain fails `expect_gt`; an in-range constructed object raises no error, failing `expect_error`; a message naming only the pushed scale (DE) fails an assertion demanding a non-offending one (BC). The violation-set and unmatched-sample assertions are red now against the still-shipped sample 2, which is their inversion.
- 2026-08-30: T2 done. `data-raw/cais.R` now builds one sample; the withdrawal, its ground and the pointer to `cairn/references/sodano2006.md` are in the script's comment block, with no second copy of the withdrawn numbers (gate choice: one place holds them, so nothing can drift). Artifact verified by `load()`ing `data/cais.rda` directly: `Norms[[1]]` carries eight `Sample == 1` rows, `Norms[[2]]` one.
- 2026-08-30: gate choice — `test-norms-kind.R`'s `expect_false("cais:2" %in% ...)` was rebuilt as a constructed off-metric control rather than deleted, because the setequal above it passes against an always-TRUE predicate and that line is what stopped it.
- 2026-08-30: T3 done, over more sites than the plan named. Beyond the five listed: `test-norms-kind.R:94`-ish (24 -> 23 samples, published 16 -> 15), `test-norms-audit-roster.R:119` (roster 24 -> 23) and its four `omits N` message pins, `test-norms-audit-coverage.R:346`-ish, `test-norms-provenance.R:482`, and `data-raw/audit-norms.R`'s `AUDIT_BATCH`, which named cais sample 2 and aborted the audit run without the edit. Minor amendment: discovered sub-tasks, no criterion changed.
- 2026-08-30: T4 done. `?cais`'s note is now a withdrawal note naming neither `sample = 2` nor the three off-range means; NEWS.md leads the 2.0.0 norms items with the withdrawal and rewrites the refusal item as a check no shipped sample now exercises. One further NEWS repair: the M72-M75 audit item's "all nine normative samples" now says "the nine those four instruments carried at the time", which the withdrawal would otherwise have contradicted two items above it. `devtools::document()` ran warning-free, rewriting `man/cais.Rd`.
- 2026-08-30: T5 done. Audit rerun regenerates `norms-audit-ledger.csv` (cais rows 4 -> 2) and `norms-audit-coverage.csv`; the two orphaned cais sample-2 rows were removed from `norms-audit-dispositions.csv`, which the script reads rather than writes. `norms-audit.md`'s roster verdict, citekey map and reference-kind table updated; `derive-norms-kind.R` reports 23 audit-table rows against 23 shipped samples, zero disagreements. `sodano2006.md` records the withdrawal, its ground and what a reply reopens; its extracted-values block is byte-identical to master (md5 03f6e573bffe88f299c4657c3eddd71c both sides). D-040 gained a forward annotation naming D-052.
- 2026-08-30: gate choice — the audit's coverage sweep now reports one standing non-exempt gap, because sodano2006.md still tables the withdrawn sample and no batch pass claims it. Pinned by identity (side, instrument, citekey, sample) rather than exempted, keeping AC6's byte-identity and the record's per-scale granularity; a call-site withdrawal exemption in `audit-norms.R` and relabelling the block's rows `note-only` were the alternatives. Falsified by a second standing gap appearing, which the identity pin would catch.
- 2026-08-30: the standing gap made two guards vacuous and both were repaired, not just re-pinned: `test-norms-audit-roster.R`'s M79 drop-every-batch-row fence counted raw gaps, so every drop would have looked noticed, and `test-norms-audit-coverage.R`'s M80 unaudited-note-sample assertions would have had to loosen into membership tests. Both now assert the standing row by identity and subtract it.
- 2026-08-30: T6 done. AC5's grep returns one hit, NEWS.md:203, which says a call passing `sample = 2` for the CAIS now errors saying the sample does not exist — a site describing it as absent, not shipped. `devtools::test()`: FAIL 0 | WARN 5 | SKIP 1 | PASS 8768, warnings and skip unchanged from master. `devtools::check(args = "--no-manual")`: 0 errors, 0 warnings, 0 notes (7m52s). ROADMAP's cais candidate row rewritten: it is no longer promoted by a reply arriving; a "swapped" reply enters under D-039's numeric-change gate whether or not the wrong values were still shipped, a "correct as printed" reply closes the row.
- 2026-08-30: AC6's audit half verified by re-running `data-raw/audit-norms.R` against the committed CSVs — identical except the three stamp columns (`generated`, `script_commit`, `data_commit`), which every run rewrites from the current date and HEAD, so the committed copy is always one commit behind by construction. Not a milestone effect; noted for review.

## Decisions

## Review
