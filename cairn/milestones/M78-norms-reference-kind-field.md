# M78: Per-sample reference-kind field in the shipped norms

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** RR16
- **Principles touched:** IP5, GP4
- **Branch/PR:** `m78-norms-reference-kind`

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

- [ ] AC1 (BC1): After the constrained milestones merge, `norm_standardize` and `norms` both appear in `NAMESPACE` as exports under exactly those names, and every instrument returned by the enumeration `Rscript -e 'utils::data(package="circumplex")'` filtered to objects inheriting `circumplex_instrument` (the RB16 Materials enumeration) has a `Norms` list slot whose second element contains a column named `Population`. Tolerance: exact; any departure is a deviation.
- [ ] AC2 (BC2): The disclosure message emitted by `norm_standardize()` (M76 AC1) contains the selected sample's `Norms[[2]]$Population` value verbatim, and its fixed (non-data) message text contains neither the token "population" nor the token "representative", case-insensitively; asserted by the AC1 message-form tests over the same instrument enumeration BC1 names, which covers every shipped message variant because AC1's two message forms partition that enumeration. Tolerance: exact string absence in the fixed text; a shipped `Population` *value* containing those tokens does not violate this criterion.
- [ ] AC3 (BC3): Every shipped `Norms[[2]]` carries a machine-readable column classifying each sample's reference-distribution kind under a controlled vocabulary of exactly three values (drawn-to-represent standardization sample; identified-study participant pool; no identified source). Over the BC1 enumeration the kind counts are exactly 6 standardization (the iip32 and iip64 samples), 16 identified-study, and 2 no-identified-source (iis32 sample 1, ipipipc sample 1), totalling 24; `norms()` prints the kind for every sample it lists. Procedure: the BC1 enumeration extended to read the new column and tally kinds. Tolerance: exact counts; if the shipped roster changes size, the partition is re-derived from `cairn/references/norms-audit.md` and the deviation shown.
- [ ] AC4: The column is named `Kind` and every value over the AC1 enumeration is one of exactly `"standardization"` (the sample was drawn to represent a defined population), `"published"` (the sample's octant statistics appear in an identified published source, a study report or an author's norms page alike) or `"unsourced"` (the sample's octant statistics appear in no identified source, whatever is known about the sample itself). For each sample `norms()` lists, its printed block carries the reader-facing phrase mapped to that row's `Kind` and neither of the other two kinds' phrases.
- [ ] AC5: `cairn/references/norms-audit.md` records, per instrument-sample pair, the kind assigned and the basis for it; each of the 15 `data-raw/<instrument>.R` builders states its own samples' kinds and basis; and `data-raw/derive-norms-kind.R` re-derives the partition from the audit table and reports zero disagreements against the shipped column over the AC1 enumeration (IP5).
- [ ] AC6: For every instrument-sample pair in the AC1 enumeration that `norm_standardize()` accepts — the `norm_sample_usable()` predicate, which excludes cais sample 2 under D-040 — the non-quiet message names the sample's kind and the returned `norm_sample` attribute carries a `Kind` element equal to that row's value. The tests' expected kinds are a literal map transcribed from the audit table, asserted `setequal` to the accepted-pair enumeration so no pair is silently skipped.
- [ ] AC7: `?norms`, `?norm_standardize`'s attribute-field enumeration and NEWS.md describe the column; `vignettes/using-instruments.Rmd` computes its standardization and unsourced counts from `Kind`; `grep -rn` over `R/ man/ tests/ vignettes/ data-raw/ NEWS.md` finds no `grepl(` call matching a pattern against `Population` or `Reference`; the roster loop at `tests/testthat/test-norms-disclosure.R:219` also asserts `"Kind" %in% names(obj$Norms[[2]])` with every value in AC4's token set; `_snaps/instrument_oop.md` is re-accepted; `devtools::document()` emits no warnings (whole log grepped, never its tail) and `devtools::check()` reports 0 errors, 0 warnings, 0 notes.

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
- [ ] T5. `norm_standardize()`'s disclosure and attribute carry the kind ([R/tidying_functions.R:316](R/tidying_functions.R:316)); extend the attribute-field enumeration in the roxygen; tests per AC6.
- [ ] T6. Update the `Norms[[2]]` pins in `tests/testthat/test-norms-provenance.R` to carry the new column.
- [ ] T7. Rewire `vignettes/using-instruments.Rmd:143-144` off the `Population`/`Reference` text matching; NEWS.md entry; run the AC7 grep.
- [ ] T8. Gate: warning-free `devtools::document()`, `devtools::check()` 0/0/0, full suite, `git status` clean before reporting any of it.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: branch `m78-norms-reference-kind` cut from master at 3d5d18a4; status planned -> in-progress.
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

## Decisions

## Review
