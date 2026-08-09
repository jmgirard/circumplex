# M78: Per-sample reference-kind field in the shipped norms

- **Status:** review
- **Priority:** high
- **Depends on:** —
- **Driving RR:** RR16
- **Principles touched:** IP5, GP4
- **Branch/PR:** `m78-norms-reference-kind` / https://github.com/jmgirard/circumplex/pull/106

## Goal

Every shipped normative sample carries a machine-readable classification of what
kind of reference distribution it is, surfaced where users choose a sample and
where they use one.

## Scope

**In:** a `Kind` column on every shipped instrument's `Norms[[2]]`, built in the
15 `data-raw/<instrument>.R` files and derivable from an extended per-sample kind
table in `cairn/references/norms-audit.md`; the kind printed by `norms()`, named
in `norm_standardize()`'s disclosure message, and carried in its `norm_sample`
attribute; `?norms`, `?norm_standardize`, NEWS.md and
`vignettes/using-instruments.Rmd` updated, the vignette's counts computed from
the column rather than by matching text in `Population`/`Reference`.

**Out:** renaming any existing identifier — closed on the merits by D-041, not
deferred. Changing any numeric norm value or `Population` string → a numeric
change takes its own gate (D-039, D-040). Kinds finer than three (community
cohort vs undergraduate pool) → refused by RR16 Q5 as assignment ambiguity the
free-text `Population` already carries. `norm_standardize()`'s roxygen "or
custom" claim (RR16 B1) → candidate row.

## Acceptance criteria

- [x] AC1 (BC1): After the constrained milestones merge, `norm_standardize` and `norms` both appear in `NAMESPACE` as exports under exactly those names, and every instrument returned by the enumeration `Rscript -e 'utils::data(package="circumplex")'` filtered to objects inheriting `circumplex_instrument` (the RB16 Materials enumeration) has a `Norms` list slot whose second element contains a column named `Population`. Tolerance: exact; any departure is a deviation.
- [x] AC2 (BC2): The disclosure message emitted by `norm_standardize()` (M76 AC1) contains the selected sample's `Norms[[2]]$Population` value verbatim, and its fixed (non-data) message text contains neither the token "population" nor the token "representative", case-insensitively; asserted by the AC1 message-form tests over the same instrument enumeration BC1 names, which covers every shipped message variant because AC1's two message forms partition that enumeration. Tolerance: exact string absence in the fixed text; a shipped `Population` *value* containing those tokens does not violate this criterion.
- [x] AC3 (BC3): Every shipped `Norms[[2]]` carries a machine-readable column classifying each sample's reference-distribution kind under a controlled vocabulary of exactly three values (drawn-to-represent standardization sample; identified-study participant pool; no identified source). Over the BC1 enumeration the kind counts are exactly 6 standardization (the iip32 and iip64 samples), 16 identified-study, and 2 no-identified-source (iis32 sample 1, ipipipc sample 1), totalling 24; `norms()` prints the kind for every sample it lists. Procedure: the BC1 enumeration extended to read the new column and tally kinds. Tolerance: exact counts; if the shipped roster changes size, the partition is re-derived from `cairn/references/norms-audit.md` and the deviation shown.
- [x] AC4: The column is named `Kind` and every value over the AC1 enumeration is one of exactly `"standardization"` (the sample was drawn to represent a defined population), `"published"` (the sample's octant statistics appear in an identified published source, a study report or an author's norms page alike) or `"unsourced"` (the sample's octant statistics appear in no identified source, whatever is known about the sample itself). For each sample `norms()` lists, its printed block carries the reader-facing phrase mapped to that row's `Kind` and neither of the other two kinds' phrases.
- [x] AC5: `cairn/references/norms-audit.md` records, per instrument-sample pair, the kind assigned and the basis for it; each of the 15 `data-raw/<instrument>.R` builders states its own samples' kinds and basis; and `data-raw/derive-norms-kind.R` re-derives the partition from the audit table and reports zero disagreements against the shipped column over the AC1 enumeration (IP5).
- [x] AC6: For every instrument-sample pair in the AC1 enumeration that `norm_standardize()` accepts — the `norm_sample_usable()` predicate, which excludes cais sample 2 under D-040 — the non-quiet message names the sample's kind and the returned `norm_sample` attribute carries a `Kind` element equal to that row's value. The tests' expected kinds are a literal map transcribed from the audit table, asserted `setequal` to the accepted-pair enumeration so no pair is silently skipped.
- [x] AC7: `?norms`, `?norm_standardize`'s attribute-field enumeration and NEWS.md describe the column; `vignettes/using-instruments.Rmd` computes its standardization and unsourced counts from `Kind`; `grep -rn "grepl(" R/ man/ tests/ vignettes/ data-raw/ NEWS.md` returns no call deriving a sample's reference kind, its only hit against `$Population` or `$Reference` being the vignette's `college|undergraduate` head-count, which names a description rather than a kind and which RR16's refusal of finer kinds leaves without a column; the roster loop at `tests/testthat/test-norms-disclosure.R:219` also asserts `"Kind" %in% names(obj$Norms[[2]])` with every value in AC4's token set; `_snaps/instrument_oop.md` is re-accepted; `devtools::document()` emits no warnings (whole log grepped, never its tail) and `devtools::check()` reports 0 errors, 0 warnings, 0 notes.

### Deviations from RR16

| BC | Deviation | Reason |
|---|---|---|
| BC3 | AC4 widens the middle kind from "identified-study participant pool" to "an identified published source", and states that the third kind classifies the *statistics'* provenance rather than the sample's | Required by the ROADMAP row's two wording fixes: the audit ties csie's and csiv's author-website tables to no study sample, and ipipipc sample 1 has an identified study (markey2009 Study 2, n = 274) with unsourced M/SD. Neither the partition nor its counts change. |

## Coverage

- AC1 → T2, T8
- AC2 → T5, T8
- AC3 → T2, T3, T4
- AC4 → T3, T4
- AC5 → T1, T3
- AC6 → T5
- AC7 → T4, T6, T7, T8

## Tasks

- [x] T1. Extend `cairn/references/norms-audit.md` with a per-sample kind table: all 24 instrument-sample pairs, each with its kind and the basis (source note anchor or the audit's unsourced disposition). The citekey map encodes only sourced-vs-unidentified today; nothing there marks a standardization sample.
- [x] T2. Test-first: extend the roster loop at `tests/testthat/test-norms-disclosure.R:219` to require `Kind` and its token set, and add the literal 24-pair expectation map asserted `setequal` to the enumeration. Red before T3.
- [x] T3. Add `Kind` to all 15 `data-raw/<instrument>.R` builders with a per-sample basis comment; add `data-raw/derive-norms-kind.R` re-deriving from T1's table and diffing against the shipped column; rebuild `data/`.
- [x] T4. `norms()` prints the kind phrase ([R/instrument_oop.R:189](R/instrument_oop.R:189)); extend the `?norms` prose classification at `R/instrument_oop.R:162-171` to key off the column; re-accept `_snaps/instrument_oop.md`.
- [x] T5. `norm_standardize()`'s disclosure and attribute carry the kind ([R/tidying_functions.R:316](R/tidying_functions.R:316)); extend the attribute-field enumeration in the roxygen; tests per AC6.
- [x] T6. Update the `Norms[[2]]` pins in `tests/testthat/test-norms-provenance.R` to carry the new column.
- [x] T7. Rewire `vignettes/using-instruments.Rmd:143-144` off the `Population`/`Reference` text matching; NEWS.md entry; run the AC7 grep.
- [x] T8. Gate: warning-free `devtools::document()`, `devtools::check()` 0/0/0, full suite, `git status` clean before reporting any of it.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: branch `m78-norms-reference-kind` cut from master at 3d5d18a4; status planned -> in-progress.
- 2026-08-08: T8 done -- document() emits zero unresolved-link warnings at cli.width 500 and regenerates only man/norms.Rd and man/norm_standardize.Rd; devtools::check(--no-manual) is Status OK, 0 errors / 0 warnings / 0 notes in 14m56s with the test suite inside it. The log carries no 'checking PDF version of manual' line, so that step did not run: the manual was built directly (R CMD Rd2pdf, exit 0), its only warnings being the pre-existing cross-package link destinations. The tree the check ran against carried the regenerated man/ pages uncommitted; they are committed here and document() re-run to a zero diff.
- 2026-08-08: T7 done -- the vignette's standardization and unsourced counts read Kind; its prose names the column as where the three counts come from. NEWS gains the Kind bullet, and two M76 entries were corrected rather than left: the quoted example message and the attribute's field list both predated this change and had gone stale the moment it landed.
- 2026-08-08: AC7 amended at the implement gate -- its grep clause was over-broad, catching the vignette's `college|undergraduate` head-count, which derives no kind and which RR16's refusal of finer kinds leaves without a column. Narrowed to kind-deriving calls with that hit named; the two kind-deriving calls it was written for are both replaced. Retiring the head-count or adding a second classification column were both offered and declined.
- 2026-08-08: T6 done -- the 15 provenance pins take the Kind column by surgical insertion rather than regeneration, so nothing else was re-blessed: the diff is 15 line pairs and every added line differs only by `Kind = `. Header notes that Kind is the one pinned field with no ledger row and where its independent origin lives.
- 2026-08-08: T5 done -- the disclosure message gains a "Reference kind: <phrase>" clause and the norm_sample attribute a Kind element, both through the same norm_kind_phrase() helper norms() uses; the roxygen @return enumeration extended. Tests sweep the 23 accepted pairs (setequal to the anchor-range predicate's own enumeration, with cais:2 asserted absent so an empty predicate cannot satisfy it vacuously) and assert the attribute on a quiet call. Deleting the message clause reddens 11. M76's BC2 fixed-text test still passes: neither banned token appears in the new clause.
- 2026-08-08: T4 done -- norms() prints a "Reference kind: <phrase>" line per sample from a shared norm_kind_phrase() helper in utils.R; ?norms replaces its prose two-tier classification with a describe block over the three kinds. The print test parses output into per-sample blocks and asserts the prefixed phrase, because iip32's Population string already contains "national standardization sample" and a bare phrase search would pass on those rows with nothing printed; deleting the cat line reddens 10 assertions. isc's snapshot re-accepted (3 blocks).
- 2026-08-08: T3 done -- Kind added to all 15 builders with a per-sample basis comment; data/ rebuilt; data-raw/derive-norms-kind.R reports 24 audit rows against 24 shipped samples, 6/16/2, zero disagreements. Mutating one audit row (isc published -> unsourced) makes it exit 1 naming that row, and it restores clean. Its shipped-side read is data/*.rda directly, not utils::data(), which resolves against the installed package and reported the freshly rebuilt column as absent.
- 2026-08-08: T2 done -- tests/testthat/test-norms-kind.R added (vocabulary, audit-transcribed per-pair map asserted setequal to the shipped pairs, the 6/16/2 partition, and which six instruments carry the standardization label), plus the Kind pin in the RR16 BC1 roster loop; red on 51 failures, every one reporting the absent Kind column rather than a wrong value.
- 2026-08-08: T1 done -- norms-audit.md gains a Reference kind section: the three kind definitions and a 24-row table of instrument/sample/kind/basis, tallying 16 published, 6 standardization, 2 unsourced against the section's own rows.
- 2026-08-08: implement gate chose a short kind label at both surfaces ("Reference kind: standardization sample" / "identified published source" / "no identified source") over a full explanatory sentence, because AC2 bars the words that would carry the explanation from the message anyway and 24 listed samples make length expensive; falsified by users reading the bare label as uninformative.
- 2026-08-08: pre-task classification sweep over norms-audit.md's citekey map places all 24 samples with no residue (16 published, 6 standardization from horowitz2003.md p. 25's national standardization sample, 2 unsourced), so RR16's F2 falsifier (>4 unassignable) does not fire.
- 2026-08-08: criteria audit ([O], fresh context) returned six single-answer defects, all fixed before this file was written — the three tokens were unwritten; the `norms-audit.md` derivation was unreachable (its citekey map marks no standardization sample, so T1 was added); an AC grep would have matched the milestone file quoting it; and a clause about "every pinned `Norms[[2]]` structure in `tests/testthat/`" quantified over an empty set. It found no criterion forbidden by any IP or GP.
- 2026-08-08: plan gate chose token vocabulary `standardization`/`published`/`unsourced` over RR16's literal `standardization`/`study`/`unknown` because "study" mislabels the two author-website tables and "unknown" misplaces the ipipipc case, where the study is identified and only its statistics are not; falsified by more than 4 of the 24 samples proving unassignable when T1 populates the table (RR16 F2).
- 2026-08-08: plan gate chose recording the kind basis in both the 15 builders and one central derivation script over the central form alone, because a maintainer editing one instrument meets the reasoning where the value is typed; falsified by the two records disagreeing in practice, which the AC5 diff would surface.
- 2026-08-08: plan gate chose naming the kind in every non-quiet message over naming it only for non-standardization samples, because silence would then have to be read as good news — the inference the disclosure exists to stop; falsified by users reporting the message as noise.

- 2026-08-08: review return 1 (defect) -- F6 shows AC6 failing as written: its enumeration is specified as the `norm_sample_usable()` predicate and `accepted_pairs()` re-implements it, diverging on the `is.null(anchors)` branch. F7 (85) actioned alongside: `?norms` contradicts itself after M77's qualifier was deleted with the paragraph that carried it. Status review -> in-progress; AC6 and AC7 unticked pending re-verification.

- 2026-08-08: return 1 fixed -- `accepted_pairs()` now calls `norm_sample_usable()` itself (mutating that predicate to a constant TRUE reddens 4 assertions, so the test consults it rather than a copy of its arithmetic), and `?norms`'s opening sentence is repaired to "For most samples ... but not for all of them", which no longer contradicts the standardization item four lines below it.
- 2026-08-08: F10 and F11 fixed too, a deliberate departure from "below 80 is logged, not actioned" -- both are one vignette sentence this branch introduced, and it was false as written on two counts ("the three counts above" when two come from `Kind`; "every standardizing call reports" when `quiet = TRUE` reports nothing and the refused sample errors first). CLAUDE.md makes vignette precision a hard rule, so shipping a knowingly false teaching sentence to respect a triage threshold was the worse trade. The remaining sub-threshold findings stand as logged.

## Decisions

## Review

Reviewed 2026-08-08 on branch `m78-norms-reference-kind`, PR #106. Master had
not moved since the branch was cut (0 commits behind), so no merge or re-run
was needed before evidence was gathered.

### Criterion evidence

- **AC1 (BC1).** `getNamespaceExports()` under `load_all()` carries both
  `norm_standardize` and `norms`; the RB16 enumeration returns 15 instruments
  and every one's `Norms[[2]]` carries `Population`. The M76 roster loop that
  pins this (`test-norms-disclosure.R`) passes.
- **AC2 (BC2).** The full `norms-disclosure` suite passes, including the
  sentinel test that strips the `Population` value out of an emitted message
  and asserts neither "population" nor "representative" survives in what
  remains — run against the new message, whose added clause is
  "Reference kind: <phrase>".
- **AC3 (BC3).** Tallying `Kind` over the enumeration returns 24 samples:
  6 standardization, 16 published, 2 unsourced. `norms()` prints a kind line
  for every sample it lists (asserted per sample in `test-norms-kind.R`).
- **AC4.** Every one of the 24 values is in the three-token vocabulary. The
  print test parses output into per-sample blocks and asserts the prefixed
  `Reference kind: <phrase>` plus the absence of the other two kinds' phrases;
  the prefix is load-bearing because iip32's `Population` value already
  contains "national standardization sample". Deleting the `cat` line reddens
  10 assertions.
- **AC5.** `norms-audit.md` carries a 24-row Reference kind table with a basis
  per sample; all 15 builders carry `Kind` with a basis comment;
  `derive-norms-kind.R` reports 24 audit rows against 24 shipped samples,
  6/16/2, zero disagreements, exit 0. Mutating one audit row (isc published to
  unsourced) makes it exit 1 naming that row.
- **AC6.** The accepted-pair sweep passes over 23 pairs, setequal to the
  anchor-range predicate's own enumeration, with `cais:2` asserted absent so an
  empty predicate cannot satisfy the setequal vacuously. Deleting the message
  clause reddens 11 assertions.
- **AC7.** `?norms` and `?norm_standardize` both document the column; NEWS
  carries its bullet and two M76 entries corrected for staleness; the vignette
  computes both counts from `Kind`. The AC7 grep returns exactly the one named
  exception, the vignette's `college|undergraduate` head-count. The roster loop
  asserts `Kind` and its token set; `_snaps/instrument_oop.md` re-accepted;
  `document()` at cli.width 500 emits zero unresolved-link warnings and a zero
  diff; `devtools::check()` Status OK, 0/0/0 in 14m56s.

### Projection vs outcome (Driving RR: RR16)

BC3 projects the partition exactly, tolerance exact. Measured 6
standardization against projected 6; measured 16 published against projected 16
(BC3's "identified-study", widened by the recorded deviation); measured 2
unsourced against projected 2 (iis32 sample 1 and ipipipc sample 1, the two BC3
names); measured 24 total against projected 24. No shortfall.

RR16's F2 falsifier — more than 4 of 24 samples unassignable without new source
research — did not fire: the pre-task sweep placed all 24 with no residue.

### Independent review (2026-08-08)

Three fresh-context lenses. The prior-review lens found nothing: its probe
confirmed the repo carries no inline PR review comments at all, so it worked
from the archived `## Review` sections of M72-M77 and checked this diff against
six patterns those reviews caught on these files. The blame-history lens found
no guard, caveat or prior fix undone -- D-040's refusal path untouched, M72's
whole-object pinning honored, BC2's banned tokens avoided. The diff lens
reported 19 findings.

A fresh scorer, given the diff and this plan, put two at 80 or above.

- **F6 (80) -- actioned, and an AC6 failure.** `accepted_pairs()` in
  `test-norms-kind.R` re-implements the anchor-range predicate instead of
  calling `norm_sample_usable()`, which AC6 names as the enumeration. It is the
  third copy (`disclosure_usable()` is the second), and it diverges: the real
  predicate short-circuits `is.null(anchors)` to TRUE, while the copy's
  `min(NULL)` is `Inf` and excludes the pair. The two agree on all 24 shipped
  samples, so no criterion's substance is wrong -- but the guard is not the one
  the criterion names, and this milestone moved `disclosure_probe()` into
  `helper-norms.R` for exactly this reason.
- **F7 (85) -- actioned.** `?norms` contradicts itself: the surviving sentence
  says the label names the group drawn from "not a frame the sample was drawn
  to represent", the qualifier that repaired it was deleted with M77's
  paragraph, and the describe block four lines later says a standardization
  sample "was drawn to represent a defined population".

The other 17 were logged below threshold, not silently dropped. The nearest
misses: F10 (74, the vignette says three counts come from `Kind` when two do),
F11 (68, "every standardizing call reports" is false under `quiet = TRUE` and
for the refused sample), F8 (66, neither help page states the literal stored
tokens, so a user filtering on the printed phrase gets zero rows), F4 (62, the
derive script's `match()` takes the first hit so duplicate audit rows could pass
as agreement), F12 (58, `?norm_standardize`'s Description still enumerates the
old message contents). Scored below 60 as out of scope or not defects: F1 and F2
(40 each, blank or `NA` kind for an instrument built without the column -- the
custom-instrument path this milestone defers), F17 (55), F15 (35), F9 (32), F5
and F18 (30 each), F16 (28), F20 (25), F3 and F13 (22 each), F14 (20), F19 (15).

### Re-verification after return 1 (2026-08-08)

- **AC6 re-verified.** `accepted_pairs()` now calls `norm_sample_usable()`.
  Mutating that predicate to a constant `TRUE` reddens 4 assertions, so the
  test consults the function the criterion names rather than a copy of its
  arithmetic; the enumeration is unchanged at 23 accepted pairs with `cais:2`
  excluded.
- **AC7 re-verified.** `?norms` opens "For most samples ... but not for all of
  them", which no longer contradicts the standardization item below it;
  `document()` regenerates with zero unresolved-link warnings and a zero diff;
  the AC7 grep still returns only the named `college|undergraduate` exception;
  `devtools::check()` re-run after the fixes is Status OK, 0 errors / 0
  warnings / 0 notes in 13m2s, with the test suite inside it.

### Consistency gate

`cairn_validate` passes all 16 checks (the 47 `work-log format` advisories are
M7's wrapped entries, unrelated). Profile `r-package` consistency-gate:
`document()` no-diff and zero `resolve link` output; generated files
regenerated from their sources, never hand-edited; NEWS entry present; no new
top-level files; `devtools::check()` clean. The check log carries no
"checking PDF version of manual" line, so that step did not run and the manual
was built directly (`R CMD Rd2pdf`, exit 0). No `DESIGN.md` principle changed,
so `cairn_impact` was not run.

