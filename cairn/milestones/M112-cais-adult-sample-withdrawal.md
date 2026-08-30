<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M112: Withdraw the CAIS adult normative sample

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP5, GP2, GP4, GP7
- **Branch/PR:** —

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

- [ ] AC1: `cais$Norms[[1]]` carries rows for sample 1 only, and
      `cais$Norms[[2]]` carries exactly one row, with `Sample == 1`.
- [ ] AC2: `norm_standardize(jz2017, scales = 2:9, instrument = cais,
      sample = 2)` errors with the unmatched-sample message — naming sample 2
      and reporting that the CAIS carries sample 1 — and not with the
      anchor-range message.
- [ ] AC3: No shipped norm sample's octant mean falls outside its instrument's
      declared anchor range, over the sweep `anchor_range_violations()`
      performs (every name `shipped_instruments()` returns × every `Sample`
      value in that instrument's `Norms[[1]]`); the sweep's domain is asserted
      non-empty in the same test, so an empty roster cannot pass it (M108).
- [ ] AC4: `norm_standardize()` still refuses any normative sample whose means
      leave its instrument's anchor range, and the standardization message's
      other-samples clause still counts only samples that would be accepted —
      both holding when no shipped sample exercises either path.
- [ ] AC5: `git grep -inE 'adult sample|sample ?= ?2|sample 2|5\.19|6\.52|6\.14'
      -- R man NEWS.md vignettes README.md` returns no site describing the CAIS
      adult sample as shipped; `?cais` and NEWS.md's 2.0.0 section each state
      that the sample was withdrawn, why, and where its transcription survives.
- [ ] AC6: The record of the withdrawn sample survives and agrees with the
      roster: `cairn/references/sodano2006.md`'s `<!-- audit-values-begin -->`
      … `<!-- audit-values-end -->` block is byte-identical to its
      pre-milestone content and the file records the withdrawal (date, ground,
      what a reply would reopen); and a fresh `data-raw/audit-norms.R` run over
      the post-removal roster reproduces the committed
      `norms-audit-ledger.csv` and `norms-audit-dispositions.csv`, neither of
      which contains a cais sample-2 row.
- [ ] AC7: `devtools::test()` passes and `devtools::check(args =
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

- [ ] T1: Tests first. Rewrite `tests/testthat/test-norms-anchor-range.R`:
      violation set expected empty, sweep domain asserted non-empty, and the
      two shipped-cais cases (refusal message, usable-count) rebuilt on
      constructed off-metric instrument objects — each shown to fail when its
      predicate is inverted, the inversion evidence recorded in the work log.
      Add the AC2 unmatched-sample case. Red before T2.
- [ ] T2: Drop sample 2 from `cais_norms` and `cais_norms_src` in
      `data-raw/cais.R:95`-ish, recording the withdrawal and its ground in the
      script's comment block (IP5); rerun the script; verify the artifact by
      `load()`ing `data/cais.rda` directly, not via `load_all()` (LESSONS).
- [ ] T3: Update the shipped-roster pins: `test-norms-kind.R:17` (16 → 15
      samples) and `:189`, `test-norms-audit-roster.R:46`,
      `test-norms-provenance.R:216`, `test-norms-disclosure.R:285`.
- [ ] T4: Rewrite `?cais`'s adult-sample note (`R/instrument_data.R:5`) as a
      withdrawal note, and NEWS.md:193-202's "will be corrected or withdrawn"
      sentence as what happened; `devtools::document()`.
- [ ] T5: Rerun `data-raw/audit-norms.R` to regenerate both CSVs; update
      `cairn/references/norms-audit.md`'s roster, citekey-map and
      reference-kind tables; append the withdrawal note to
      `cairn/references/sodano2006.md` leaving its extracted block untouched.
- [ ] T6: Run AC5's grep; full `devtools::test()` and
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

## Decisions

## Review
