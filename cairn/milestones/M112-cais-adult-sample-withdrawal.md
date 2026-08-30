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
- **Branch/PR:** m112-cais-adult-sample-withdrawal / https://github.com/jmgirard/circumplex/pull/143

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
- 2026-08-30: T5 done. Audit rerun regenerates `norms-audit-ledger.csv` (cais rows 4 -> 2) and `norms-audit-coverage.csv`; the two orphaned cais sample-2 rows were removed from `norms-audit-dispositions.csv`, which the script reads rather than writes. `norms-audit.md`'s roster verdict, citekey map and reference-kind table updated; `derive-norms-kind.R` reports 23 audit-table rows against 23 shipped samples, zero disagreements. `sodano2006.md` records the withdrawal, its ground and what a reply reopens; its extracted-values block is byte-identical to master (md5 03f6e573bffe88f299c4657c3eddd71c on both sides, over the span from the
  begin marker line through the end marker line inclusive). D-040 gained a forward annotation naming D-052.
- 2026-08-30: gate choice — the audit's coverage sweep now reports one standing non-exempt gap, because sodano2006.md still tables the withdrawn sample and no batch pass claims it. Pinned by identity (side, instrument, citekey, sample) rather than exempted, keeping AC6's byte-identity and the record's per-scale granularity; a call-site withdrawal exemption in `audit-norms.R` and relabelling the block's rows `note-only` were the alternatives. Falsified by a second standing gap appearing, which the identity pin would catch.
- 2026-08-30: the standing gap made two guards vacuous and both were repaired, not just re-pinned: `test-norms-audit-roster.R`'s M79 drop-every-batch-row fence counted raw gaps, so every drop would have looked noticed, and `test-norms-audit-coverage.R`'s M80 unaudited-note-sample assertions would have had to loosen into membership tests. Both now assert the standing row by identity and subtract it.
- 2026-08-30: T6 done. AC5's grep returns one hit, NEWS.md:203, which says a call passing `sample = 2` for the CAIS now errors saying the sample does not exist — a site describing it as absent, not shipped. `devtools::test()`: FAIL 0 | WARN 5 | SKIP 1 | PASS 8768, warnings and skip unchanged from master. `devtools::check(args = "--no-manual")`: 0 errors, 0 warnings, 0 notes (7m52s). ROADMAP's cais candidate row rewritten: it is no longer promoted by a reply arriving; a "swapped" reply enters under D-039's numeric-change gate whether or not the wrong values were still shipped, a "correct as printed" reply closes the row.
- 2026-08-30: AC6's audit half verified by re-running `data-raw/audit-norms.R` against the committed CSVs — identical except the three stamp columns (`generated`, `script_commit`, `data_commit`), which every run rewrites from the current date and HEAD, so the committed copy is always one commit behind by construction. Not a milestone effect; noted for review.

## Decisions

## Review

### Acceptance criteria — fresh evidence (2026-08-30, at ba2a531a)

All seven boxes arrived at this gate already ticked by the T6 implement commit
(ba2a531a), with no evidence recorded. Under AC fencing that is an unverified
state, not a pass: every box was reset and re-ticked below only as its own
fresh evidence landed.

- **AC1 — verified.** `load("data/cais.rda")` read directly, not through
  `load_all()`: `Norms[[1]]` carries 8 rows, every one `Sample == 1`;
  `Norms[[2]]` carries exactly 1 row, `Sample == 1`.
- **AC2 — verified.** `norm_standardize(jz2017, scales = 2:9, instrument =
  cais, sample = 2)` errors with "No normative data for sample 2. The CAIS
  carries sample 1; see norms() for what each one is." — names sample 2,
  reports the one sample the CAIS carries, and contains no "response range"
  text, so it is the unmatched-sample message and not the anchor-range one.
- **AC3 — verified.** The sweep re-run outside the test harness over every
  name `shipped_instruments()` returns × every `Sample` in that instrument's
  `Norms[[1]]`: domain 23 (instrument, sample) pairs, 0 violations. The
  domain's non-emptiness is asserted in the same test
  (`test-norms-anchor-range.R:74`, `expect_gt(..., 0L)`) ahead of the
  emptiness assertion at `:75`.
- **AC4 — verified, both halves, each demonstrated live rather than only via
  a green test.** Refusal: a constructed `cais` object with one octant mean
  pushed to anchor-max + 1 errors "The CAIS normative sample 1 cannot be used
  for standardization. Its mean score for DE falls outside the instrument's 1
  to 5 response range…". Usable-count clause: shipped `iipsc` sample 1
  discloses "1 other sample is available; see norms()."; with `iipsc` sample 2
  pushed off-metric, the same call's message carries no "other sample" clause
  at all. `test-norms-anchor-range.R` 22 passed / 0 failed,
  `test-norms-disclosure.R` 471 passed / 0 failed.
- **AC5 — verified.** The criterion's grep verbatim over `R man NEWS.md
  vignettes README.md` returns exactly one hit, `NEWS.md:203`, which reads
  "Code that passed `sample = 2` to `norm_standardize()` for the CAIS was
  already erroring and now errors saying the sample does not exist" — a site
  describing the sample as absent, not as shipped. `?cais` (`man/cais.Rd:23`,
  section "Note on the withdrawn second normative sample") and NEWS.md's
  2.0.0 section each state the withdrawal, its ground (three octant means
  above the 5-point scale's maximum, so the sample is off the metric it would
  standardize), and where the transcription survives
  (`cairn/references/sodano2006.md`).
- **AC6 — verified, with one stated exception on the ledger's stamp columns.**
  Record survival: the `<!-- audit-values-begin -->` … `<!-- audit-values-end
  -->` block of `cairn/references/sodano2006.md` is byte-identical to master
  (md5 03f6e573bffe88f299c4657c3eddd71c on both sides, over the span from the
  begin marker line through the end marker line inclusive), and the file's new
  "Withdrawn (2026-08-30, M112)" section records the date, the ground
  (unusability, not the metric question settled), and what a reply reopens (a
  "swapped" reply enters under D-039's numeric-change gate; "correct as
  printed" leaves the withdrawal standing). Roster agreement: a fresh
  `data-raw/audit-norms.R` run over the post-removal roster reproduces
  `norms-audit-dispositions.csv` and `norms-audit-coverage.csv` byte for byte,
  and `norms-audit-ledger.csv` byte for byte across all 192 rows once the three
  run-stamp columns are removed — those columns record the run's own date and
  HEAD (committed copy `c917e48b`, fresh run `ba2a531a`), so a file naming its
  producing commit can never be byte-reproduced by a later run. Neither CSV
  contains a cais sample-2 row (the two surviving cais ledger rows are sample
  1's Population and URL dispositions). Surfaced at the gate rather than
  silently read as a pass.

### Consistency gate (2026-08-30)

- `cairn_validate.py` — exit 0, all checks pass. 47 advisory WARNs, every one
  a wrapped work-log line in M7's file, pre-existing and outside this diff.
  `coverage complete` and `binding criteria` both PASS. The `release window`
  advisory did not fire.
- `cairn_impact.py` — skipped: `git diff master..HEAD --name-only` shows
  `cairn/DESIGN.md` untouched, so no principle changed.
- Toolchain checks, from `PROFILE.md`'s `consistency-gate` slot:
  `devtools::document()` leaves no diff in `NAMESPACE`, `man/` or `R/` and
  emits zero `resolve link` warnings at `cli.width = 500`; `build_readme()`
  leaves README.md unchanged; `pkgdown::check_pkgdown()` reports no problems;
  NEWS.md carries the user-visible entry with no milestone number in it; the
  diff adds no new top-level file (`NEWS.md` is the only top-level path it
  touches, already tracked). Master watches: the newest push run on master to
  reach a verdict is `success` for both `R-CMD-check.yaml` and
  `test-coverage.yaml` (2026-08-25, 15909d47). `tools/check-master-red-alert.R`,
  `tools/master-red-alert-dryrun.R` (4 cases ok) and
  `tools/check-branch-protection.R` (both rulesets match the committed pin) all
  exit clean.
- **AC7 — verified.** `devtools::test()`: FAIL 0 | WARN 5 | SKIP 1 | PASS 8768.
  `devtools::check(args = "--no-manual")`: Status OK, 0 errors, 0 warnings,
  0 notes, 8m 1.9s. `devtools::document()` emitted zero `resolve link`
  warnings and left no diff. The 5 warnings and 1 skip are lavaan
  estimation warnings and the one pre-existing skip, unchanged from master.

### Independent review (2026-08-30)

Surface tier is user-facing and the diff touches R/, tests/ and data-raw/, so
the full three-lens fan-out ran, each reviewer fresh-context and none having
seen the implementation. The session's standing no-subagent instruction is
satisfied by the user's invocation of this skill (tracking-rules
freshness-spawns clause), so the review is not degraded.

- **[S] blame-history** — no findings. It traced each modified guard back to
  its introducing commit and reports that the M108 empty-domain pattern, M79's
  drop-every-batch-row fence and M80's unaudited-note-sample assertions are
  each repaired rather than loosened, and that the roster pin arithmetic
  (24 -> 23, 23 -> 22) is right against the one removed pair.
- **[S] prior-PR-comments** — no findings. Archived `## Review` sections for
  M72-M87, M98, M109 and M111 on the touched files, plus `LESSONS.md` and
  `test-craft.md`; it names M76 F1 (the other-samples clause counting refused
  samples) and M85 F1 (a blank sample cell) as the two prior points nearest
  this diff and finds both still fenced.
- **[O] diff-bug** — nine findings, ranked; text and disposition below.

#### [O] diff-bug findings, ranked, with verification and proposed disposition

Return floor: none of the nine demonstrates an acceptance criterion failing,
and none is a defect in what the package computes for a user — the refusal,
the disclosure and the shipped data are all verified correct above. So none
returns the milestone; each takes fix-now / follow-up / reject at the gate.

- **F1. The offending-scale assertions do not discriminate the defect they
  claim to fence.** `test-norms-anchor-range.R:172` and `:203` assert only
  that the refusal message contains the offending scale; neither asserts a
  non-offending scale is absent. **Verified by mutation**: changing
  `R/tidying_functions.R:265` from `paste(labels[outside], collapse = ", ")`
  to `paste(labels, collapse = ", ")` — the refusal then blames all eight
  octants for one out-of-range mean — leaves the file 22 passed / 0 failed.
  The Abbrev case has the same shape and is pre-existing; T1 added the
  Scale-labelled twin carrying it. The reviewer also notes the T1 work-log
  line describes inversion evidence ("a message naming only the pushed scale
  (DE) fails an assertion demanding a non-offending one (BC)") for an
  assertion that was a one-off probe, not committed. *Proposed: fix now* —
  add the absent-non-offender assertion to both cases.
- **F2. The audit's new permanent coverage gap is recorded nowhere durable.**
  Every `data-raw/audit-norms.R` run now reports one non-exempt
  `note-sample-not-audited` row (`norms-audit-coverage.csv:17`, cais /
  sodano2006 / sample 2), because the source note still tables the withdrawn
  sample and no batch pass claims it. This is explained only in the M112 work
  log, which archives on merge, and in three test comments;
  `cairn/references/norms-audit.md` — the standing record — does not say the
  clean state of a run is now one gap, not zero. *Partly corrected*: the
  reviewer reads norms-audit.md:102 as unchanged, but the diff did update that
  sentence to carry the 24 -> 23 withdrawal; what is genuinely missing is the
  expected-gap note. *Proposed: fix now* — one short paragraph in
  norms-audit.md, so a maintainer meeting a non-clean run does not repair it
  by deleting the transcription AC6 exists to preserve.
- **F3. `test-norms-audit-roster.R:65-66` states a stale present-tense count.**
  "Six of the 24 rows are the `scales = TRUE` entry of a multi-sample
  instrument". **Re-measured at HEAD**: the batch is 23 rows, 5 of which are
  the `scales = TRUE` entry of a multi-sample instrument (iei, igicr, iip32,
  iip64, iipsc), and 10 rows are single-sample. The diff made it stale. The
  block's dated "Measured 2026-08-08" figures below it stay correct as the
  dated observations they are. *Proposed: fix now* — one sentence.
- **F4. `test-tidying_functions.R:176` claims a shipped violation that no
  longer exists.** "see test-norms-anchor-range.R for the invariant and the
  one shipped violation" — there is none, and the `expect_error(...,
  "response range")` arm at `:200` is now unreachable for every shipped
  instrument. The expectation is derived from the predicate so it stays
  correct; the comment and the coverage it implies do not. Outside AC5's grep
  scope, which is why T4/T6 did not reach it. *Proposed: fix now* — comment
  only; the dead arm is kept deliberately, since it is what makes the check
  correct if an off-metric instrument is ever added.
- **F5. The sweep's domain guard is weaker than the pin it replaced.**
  `test-norms-anchor-range.R:74` asserts only `expect_gt(..., 0L)`; a
  regression shrinking `shipped_instruments()` or widening the
  `is.null(obj$Anchors)` skip would leave the test green while its own comment
  claims "every shipped sample of every shipped instrument". AC3 demanded only
  non-emptiness, so this is a strengthening, not a contract miss; sibling
  tests already pin 23 exactly. *Proposed: fix now* — pin the domain to 23.
- **F6. `data-raw/audit-norms.R:181` and `:193` say "the shipped roster
  reports 23" where the mirrored test comments were updated to 22.** Both
  audit-norms.R sites carry "measured 2026-08-15", so neither is false as a
  dated observation; the two files simply now present the same measurement
  differently. *Proposed: fix now* — add the same one-clause historical gloss
  the test mirror already carries.
- **F7. `test-norms-anchor-range.R:193-194` couples an absolute row index to a
  within-sample index.** `offender` is read as `Scale[[3]]` of the unmodified
  object while the mutation pushes `which(values$Sample == 1)[[3]]`; identical
  only because cais now ships one sample starting at row 1. If a reply from
  Sodano re-adds the adult sample listed first, the two indices diverge and
  the test fails for a reason unrelated to the refusal. *Proposed: fix now* —
  derive `offender` from the same index expression.
- **F8. `?cais` and NEWS.md point at `cairn/references/sodano2006.md`, which
  `.Rbuildignore:22` excludes from the tarball.** A user wanting the withdrawn
  numbers to reproduce a prior analysis has no route from an installed copy.
  Both texts say "the package's source repository", so the statement is
  honest. *Proposed: reject* — deliberate: AC5's grep forbids those numerals
  on the shipped surfaces, and naming the source repository is the accurate
  pointer.
- **F9. `?cais` freezes a dated fact**: "The query to the authors is still
  open" (`R/instrument_data.R:16`, `man/cais.Rd:33`). A reply arriving after
  release leaves shipped documentation asserting something false for that
  release's life. *Proposed: fix now* — drop the sentence; the correspondence
  state is tracked in `cairn/references/sodano2006.md`, which the surrounding
  text already points at.

### Work-log note carried to the gate

The seven acceptance-criterion boxes arrived ticked by the T6 implement
commit. AC fencing gives review the tick; an implement-side tick with no
evidence is an unverified state. No criterion turned out to be wrong — all
seven verify — so this is a process note, not a return.
