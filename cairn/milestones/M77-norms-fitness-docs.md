# M77: Say precisely what the shipped reference statistics are

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M76
- **Driving RR:** —
- **Principles touched:** GP5
- **Branch/PR:** `m77-norms-fitness-docs`

## Goal

Stop the docs implying a population referent the reference samples do not have.

## Scope

**In:** the four passages in `vignettes/using-instruments.Rmd` that import a
population referent or resolve sample choice on size (`:122`, `:124`, `:126`,
`:132`); a chunk-computed characterization of what the shipped samples are; an
addition to `?norms`'s existing hedge; and a keyword sweep of the vignettes and
roxygen so the disposition of every hit is on the record.

**Out:** the call-site disclosure itself → M76 (this milestone describes the
behavior M76 ships, which is why it depends on it). Renaming any identifier or
the `Population` column → declined at M76's plan gate, ROADMAP candidate row.
Moving the teaching content to an ebook → existing ROADMAP candidate row,
untouched. Correcting `cairn/references/norms-audit.md` itself → not needed;
this milestone cites it, it does not restate it.

## Acceptance criteria

- [ ] AC1 The four passages are rewritten: the nationality-matching claim at
      `:124` goes (no shipped instrument carries nationality-matched samples —
      the matched sets are gender for igicr/iip32/iip64 and age for cais); the
      definite-article "the normative average" at `:122` names a specific study
      sample instead; and the size-resolves-it framing at both `:132` ("so much
      larger and therefore subject to less sampling error") and `:126` ("a
      rather large sample … a rather small sample") resolves on appropriateness,
      consistent with the M74 measurement that reference *choice* moves scores
      0.44 SD on average and 0.78 at the extreme, against ~0.12 SD contributed
      by reference-moment sampling error at the worst shipped sample size
      (iipsc sample 2, n = 106).
- [ ] AC2 The vignette states, where sample choice is first taught, what the
      shipped reference samples are, and every *quantified* claim in that
      passage is either computed by a visible chunk from the shipped instrument
      objects or cited to `cairn/references/norms-audit.md` — never
      hand-written. The chunk-computed figures are 24 samples across 15
      instruments, 11 whose `Population` label matches college or undergraduate
      students, 7 with `Size` < 300, and 6 labelled a national standardization
      sample (iip32 and iip64, the exception the passage must name). Claims
      about study *design* — that most are single-study convenience samples —
      are not computable from any shipped field and are cited, not computed.
- [ ] AC3 `?norms`'s hedge, which already cautions against treating a sample as
      representative (`R/instrument_oop.R:158-160`), additionally states that
      `Population` names the group the sample was drawn from rather than a
      frame it was sampled to represent, and points at the vignette passage
      AC2 adds.
- [ ] AC4 Every site returned by `grep -n -iE
      'representative|normative average|sampling error|the population'
      vignettes/*.Rmd R/*.R` is dispositioned in this milestone's work log as
      rewritten or as confirmed correct as written. This criterion claims what
      that command sweeps and nothing wider; it makes no claim about passages
      the pattern does not match.
- [ ] AC5 `NEWS.md` carries a user-facing entry for the documentation
      correction, and `Rscript -e 'devtools::check()'` is clean (0 errors, 0
      warnings; NOTEs justified) — which is what rebuilds the edited vignette.

## Coverage

- AC1 → T2
- AC2 → T3
- AC3 → T4
- AC4 → T1, T5
- AC5 → T6

## Tasks

- [x] T1 Run the AC4 sweep and record every hit with a proposed disposition
      before editing anything, so the edit set is decided by the sweep rather
      than by recall.
- [ ] T2 Rewrite the four passages at `vignettes/using-instruments.Rmd:122`,
      `:124`, `:126`, `:132`. Where a synonym for "normative samples" is wanted,
      gloss them "reference samples"; avoid "comparison sample", which in the
      psychological literature connotes a study-internal control group (RR16 R4,
      B3, reaching `:122`'s own "normative or comparison sample").
- [ ] T3 Add the characterization passage and its computing chunk; verify each
      printed figure against a separate run rather than against the chunk that
      produced it.
- [ ] T4 Extend the `?norms` hedge at `R/instrument_oop.R:153-160`; run
      `document()`.
- [ ] T5 Close out the AC4 dispositions in the work log after the edits land,
      re-running the sweep so the record describes the post-edit tree.
- [ ] T6 NEWS entry; full `check()` including the vignette rebuild.

## Work log

- 2026-08-08: created by /milestone-plan.
- 2026-08-08: criteria audit ([O], fresh context) covered this file's criteria in the same pass as M76's; of the 10 findings, 4 were M77's — the `:126` omission, the uncomputable college-student count, the compute-it rule exempting the unquantified convenience-sample claim, and a false characterization of what `?norms` already says. All 4 fixed before the gate.
- 2026-08-08: plan gate chose to fix the prose and leave every identifier alone over renaming the docs' vocabulary away from "norms"; renaming the prose alone puts the docs and the API out of step for readers, and the identifiers are not the claim. Falsified by evidence that readers take the word itself, rather than the surrounding prose, as the representativeness claim.
- 2026-08-08: blocked on RB16 (norms vocabulary rename). The maintainer raised the irreversible-api tripwire immediately after planning: GP4 binds API stability only after v2.0.0, which D-040 already relied on, so the cost-of-change ground the gate decided on is at its weakest now and expensive later. The gate's keep-the-names choice is provisional pending RR16.
- 2026-08-08: at the RB16 approval gate the maintainer chose to run the review manually rather than have the session spawn it, so RB16 sits open awaiting `cairn/reviews/RR16-norms-vocabulary-rename.md`; ingestion runs at the next session start once that file appears.
- 2026-08-08: the manual review run wrote `cairn/reviews/RR16-norms-vocabulary-rename.md`. Verdict: no rename (all four identifier surfaces kept); binds a per-sample reference-kind field and M76 message-prose wording via BC1–BC3. Awaiting ingestion.
- 2026-08-08: RR16 ingested; unblocked, back to planned. No binding criterion constrains this file — BC1 and BC2 land in M76, BC3 routes to a new milestone — so the amendment here is T2's wording constraint (RR16 R4/B3) only, and the four passages, the characterization chunk and the AC4 sweep stand as planned. RR16 endorsed this milestone's docs-only scope explicitly (its Q3(e)), finding the docs-vs-API mismatch worry does not arise because the prose keeps the field's vocabulary and corrects only the claims. RB16/RR16 archived; ingest audit detail in M76's work log.
- 2026-08-08: RR16 B1 (`norm_standardize()`'s roxygen promising "normative data (from the package or custom)" where the signature admits only a `circumplex_instrument`) went to M76 AC5/T7 rather than here — M76 already rewrites that help page, and this milestone's AC4 sweep pattern does not match the phrase.
- 2026-08-08: T1 pre-edit AC4 sweep (the AC4 `grep -n -iE` command, run at 9e9fff31 + this branch's tracking commits) returns 23 matched lines; proposed dispositions — REWRITE 4: `using-instruments.Rmd:122` (definite-article "the normative average"), `:132` (size-resolves-it), `R/instrument_oop.R:153` and `:159` (the `?norms` hedge, AC3). CONFIRMED CORRECT AS WRITTEN 19, all naming an estimation-theoretic population or sampling error rather than a reference sample: `evaluating-circumplex-structure.Rmd:35,298`; `sem-based-ssm-analysis.Rmd:246,247`; `R/ssm_ci_oop.R:298`; `R/tidying_functions.R:310,328` (M76's own comments stating the ground this milestone documents); `R/axes_reliability.R:261,430,459`; `R/cpm_fit.R:405,1806`; `R/ssm_ci_accuracy.R:15,16,126,151,257,431,554`.
- 2026-08-08: implement question gate chose inline computed figures (a visible chunk assigns the counts, prose interpolates them) over a printed summary block or a 24-row table, and placed the characterization passage before the IIP-SC demo so the reader knows what the samples are before making the worked choice.

## Decisions

- 2026-08-08 (RR16 R4, B3): where the rewritten passages want a synonym for "normative samples", they say "reference samples" and not "comparison sample" — the latter connotes a study-internal control group, inviting the reading that the user's data are compared to a matched design element rather than to an external published table.

## Review
