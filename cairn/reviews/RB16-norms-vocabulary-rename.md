# RB16: Should the reference-statistics vocabulary be renamed before v2.0.0? (M77)

- **Date:** 2026-08-08
- **Output required:** write findings to `cairn/reviews/RR16-norms-vocabulary-rename.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`circumplex` is an R package on CRAN (currently 1.3.0, preparing a v2.0.0) for
circumplex data analysis — the Structural Summary Method and related models.
One of its subsystems bundles 15 interpersonal-circumplex instruments as data
objects. Each carries a two-frame `Norms` slot: `Norms[[1]]` holds per-octant
means and standard deviations keyed by `Sample`, `Scale`/`Abbrev` and `Angle`;
`Norms[[2]]` describes each sample with columns `Sample`, `Size`, `Population`,
`Reference`, `URL`. The exported function `norm_standardize()` converts a
user's scale scores to z-scores against a chosen sample; the exported
`norms()` prints the samples available for an instrument.

**The problem under review is a naming question, not a numeric one.** The
package calls these statistics "norms" — in the exported function names, in
the data slot, in help pages and vignettes, and in a shipped column literally
named `Population`. A norm, in psychometrics, implies a referent: a
distribution estimated from a sample drawn to represent some population, such
that a z-score against it locates a respondent within that population.

Measured over the shipped data on 2026-08-08 (you should re-derive these; the
command is in Materials):

- 24 samples across 15 instruments.
- 11 of the 24 carry a `Population` label naming college or undergraduate
  students.
- 7 of the 24 have `Size` < 300; the smallest is 106.
- 23 of the 24 are labelled American; the exception is a single sample of
  "MTurkers from US, Canada, and India".
- 6 of the 24 — the IIP-32 and IIP-64 sets, three each — are labelled a
  "national standardization sample", drawn to represent the U.S. adult
  population. These are the only shipped samples for which "norms" is
  literally accurate.
- The remaining 18 are, as far as the package's own provenance audit
  established, the participant pools of single validation studies.

A separate measurement (recorded in `cairn/references/norms-audit.md` and
summarized in `cairn/ROADMAP.md`) found that **which** sample of the same
instrument a user standardizes against moves scores by 0.44 SD on average and
0.78 SD at the extreme, unevenly across octants — so it changes Structural
Summary Method amplitude and displacement, not merely elevation. By contrast,
sampling error in the reference moments themselves contributes only ~0.12 SD
at the worst shipped sample size. Reference *choice* dominates reference
*precision* by roughly 4–9×.

Two milestones were planned on 2026-08-08 in response. **M76** adds call-site
disclosure to `norm_standardize()` — a `quiet` argument, a message naming the
sample used with its size and population, and an attribute on the returned
frame. **M77** corrects what the documentation claims these statistics are.
Neither renames anything. That was decided at M77's plan gate on
cost-of-change grounds — identifiers are not themselves claims, and renaming
breaks user code and stored data structures.

**Why this needs independent review.** The maintainer observed that the
cost-of-change argument is at its weakest right now: v2.0.0 has not shipped,
the package's own GP4 principle binds API stability only *after* 2.0, and
D-040 already relied on exactly that reasoning to justify a behavior change.
If a rename is right, this is the moment; after v2.0.0 it becomes expensive
for a decade. The session that planned M76/M77 should not author the verdict
on its own provisional decision, and an exported-API rename is irreversible in
the sense that matters — a deprecation cycle can soften it, but the vocabulary
choice propagates into published analysis scripts, teaching materials and
citations.

## Materials

Read these:

- `R/tidying_functions.R:124-250` — `norm_standardize()`: roxygen, signature,
  sample selection (`:179-180`), the anchor-range refusal added in 2026-08
  (`:184-205`), and the two return paths (`:241-245`).
- `R/instrument_oop.R:146-191` — `norms()` and its roxygen, including the
  existing hedge at `:153-160` about what the `Population` label means.
- `R/instrument_data.R` — the instrument data documentation blocks; note the
  `@source` sections that distinguish an instrument's article from its norms
  table, and the "source unconfirmed" blocks for `iis32` and `ipipipc`.
- `vignettes/using-instruments.Rmd:110-150` — the passage that teaches norm
  standardization. Lines 122, 124, 126 and 132 are the ones M77 rewrites.
- `cairn/DESIGN.md` — the whole file, but especially the numbered principles:
  IP1–IP6 and GP1–GP7. GP4 (post-2.0 API stability) and GP5 (teach the field,
  precisely) are the two that collide here; GP3 constrains dependencies and
  favors a standard-evaluation API.
- `cairn/DECISIONS.md` — scan the `### D-` headings, then read **D-039** and
  **D-040** whole. D-039 concerns correcting norms provenance under IP5 and
  how it composes with GP4; D-040 records the refusal of an off-metric norm
  sample and states explicitly why GP4 does not bind pre-2.0.0.
- `cairn/milestones/M76-norms-call-site-disclosure.md` and
  `cairn/milestones/M77-norms-fitness-docs.md` — the two planned milestones,
  including M77's `Out:` clause excluding the rename.
- `cairn/references/norms-audit.md` — the per-instrument provenance record
  from the four-batch audit completed 2026-08-07.
- `NEWS.md` — search for "norm" to see the vocabulary's history in
  user-facing release notes, including the earlier rename of `standardize()`
  to `norm_standardize()`.

Re-derive the sample characterization rather than trusting the figures above:

```
Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); nms <- utils::data(package="circumplex")$results[,"Item"]; insts <- sort(Filter(function(nm){e<-new.env(); utils::data(list=nm,package="circumplex",envir=e); inherits(get(nm,envir=e),"circumplex_instrument")}, nms)); rows <- do.call(rbind, lapply(insts, function(nm){e<-new.env(); utils::data(list=nm,package="circumplex",envir=e); o<-get(nm,envir=e); s<-o$Norms[[2]]; data.frame(inst=nm, sample=s$Sample, n=s$Size, pop=s$Population, stringsAsFactors=FALSE)})); print(rows, row.names=FALSE)'
```

## Questions

1. **Is the word wrong?** Given what the 24 shipped samples are, does calling
   them "norms" in user-facing documentation constitute a false or misleading
   claim about their referent, or is "norms" defensible as the field's
   ordinary term for any published reference distribution regardless of
   sampling design? Answer with reference to how the interpersonal-circumplex
   and broader psychometric literature actually uses the term — including
   whether "norms" carries a representativeness commitment in practice or
   only in textbook definitions.

2. **Does the naming change user behavior?** Is there a defensible mechanism
   by which the vocabulary — as against the prose M77 is already fixing —
   leads a user to a wrong inference or a wrong analytic choice? Specifically:
   does calling the column `Population` and the function `norm_standardize()`
   plausibly cause users to skip the sample-choice question that the 0.44 SD
   measurement shows dominates? If you judge that the prose fix alone
   addresses the mechanism, say so plainly — a finding that no rename is
   warranted is as useful as the opposite.

3. **If a rename is warranted, what is the minimal sufficient scope?** Rank
   these five surfaces independently, since they have very different costs,
   and say for each whether it should change and why:
   (a) the exported `norm_standardize()` function name;
   (b) the exported `norms()` function name;
   (c) the `$Norms` list slot on the instrument objects;
   (d) the `Population` column in `Norms[[2]]`;
   (e) the documentation vocabulary alone, leaving all identifiers intact.
   Note that (e) is what the plan gate provisionally chose, and that choosing
   (e) plus nothing else leaves the docs and the API using different words for
   the same thing — assess whether that mismatch is itself a cost worth
   avoiding.

4. **What would you rename it to?** If any of (a)–(d) should change, propose
   concrete replacements and assess each against the alternative readings it
   invites. "Reference sample" and "comparison sample" are the obvious
   candidates; consider whether either imports its own false implication, and
   whether a term of art already exists in the psychometric literature for a
   published non-representative reference distribution.

5. **Is the sample-level distinction more useful than a blanket rename?**
   Six of the 24 samples genuinely are norms and 18 are not. Would it be
   better to keep the vocabulary and instead mark, per sample, what kind of
   reference distribution it is — a machine-readable field alongside
   `Population` — so that the disclosure M76 prints can say which kind the
   user is standardizing against? Assess this against a blanket rename on
   both accuracy and usability, and say which you would choose if forced.

6. **Deprecation mechanics, if a rename is warranted.** The package uses
   `lifecycle` for deprecation cycles. Given that v2.0.0 has not shipped and
   GP4 does not yet bind, is a deprecation cycle owed at all, or should a
   pre-2.0 rename be clean? What is the concrete cost to existing users —
   published scripts, the CRAN-visible API surface — and does it change your
   answer to question 3?

7. **What would falsify your recommendation?** State the class of evidence
   that should reopen whatever you conclude. This is recorded and used later,
   so make it concrete and observable rather than a hedge.

## Constraints

Fixed; flag disagreement explicitly rather than working around it.

- **The shipped numeric values are not under review.** A four-batch provenance
  audit (2026-08 through `cairn/references/norms-audit.md`) verified the
  shipped moments against their published sources. One sample — `cais`
  sample 2 — is refused at standardization time because its means fall outside
  the instrument's own response range; that refusal is settled by **D-040**
  and is not to be relitigated.
- **Provenance-record corrections are settled by D-039** and follow IP5, not
  GP4. Do not reopen how a wrong citation is corrected.
- **M76's call-site disclosure is not under review** — the `quiet` argument,
  the message, and the returned attribute are decided. You may recommend
  changes to what the message *says* if a naming verdict requires it; do not
  re-argue whether it should exist.
- **No new package dependency** may be assumed by a recommendation without
  saying so explicitly; GP3 holds dependencies to a minimum and the repo
  requires a question gate plus a recorded decision for any addition.
- **Do not assume the release schedule.** Release timing is the maintainer's
  to declare. You may reason about "before v2.0.0" as a window; do not
  recommend when to ship.

## Output format

In `RR16-norms-vocabulary-rename.md`: answer each question by number with your
reasoning and evidence; list any additional findings separately under "Beyond
the brief"; end with concrete recommendations, each marked apply / consider /
reject-with-reason. Where findings bind implementation, also emit a
`## Binding criteria` section: numbered `BC1…`, each a measurable assertion
checkable against evidence, with any numeric projection stating its tolerance.
These are ingested VERBATIM into the constrained milestone's acceptance
criteria and mechanically diffed against this file; departures are legal only
through that milestone's shown "Deviations from RR16" table.

Note on binding criteria: prefer few and load-bearing. A criterion that makes
a universal claim ("no surface says X") must name the procedure that
enumerates its domain, and that procedure must enumerate the domain the claim
itself quantifies over rather than a proxy for it — a hand-list of sites is
not such a procedure.
