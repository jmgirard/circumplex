# RR16: Should the reference-statistics vocabulary be renamed before v2.0.0? (M77)

- **Date:** 2026-08-08
- **Brief:** `cairn/reviews/RB16-norms-vocabulary-rename.md`
- **Reviewer basis:** all Materials read; sample characterization re-derived
  from the shipped data (command in the brief, run 2026-08-08) and confirmed:
  24 samples across 15 instruments; 11 labelled college or undergraduate
  students; 7 with `Size` < 300 (minimum 106, iipsc sample 2); 6 labelled a
  national standardization sample (iip32 and iip64, three each); 23 American,
  the exception csig's "MTurkers from US, Canada, and India".

## Verdict in one paragraph

Keep every identifier. "Norms" is the interpersonal-circumplex field's own
word for exactly these tables — including the convenience-sample ones — so the
identifiers are not false claims, and the behavioral hazard the 0.44 SD
measurement exposes is addressed at the right layer by M76's call-site
disclosure and M77's prose corrections. The one thing a blanket rename cannot
do and the current plan does not yet do is preserve the true distinction the
data contains: 6 of the 24 samples genuinely are standardization samples and
18 are not. That distinction should ship as a machine-readable per-sample
field that `norms()` and M76's message surface, which is more accurate and
more usable than renaming everything to a word ("reference") that flattens it.
One wording constraint on M76's message follows from the same reasoning:
identifiers are not claims, but message prose is, so the message should print
the `Population` value without framing it with the word "population".

## Answers

### 1. Is the word wrong?

No — loose, but not false, and the looseness is the field's, not the
package's.

The representativeness commitment attached to "norms" lives mainly in
textbook definitions and in reporting standards (the AERA/APA/NCME *Standards*
require that norms be accompanied by a clear description of the norming
population and sampling procedure — a disclosure requirement, not a ban on
non-representative reference data; the same tradition recognizes "local
norms," "convenience norms," and "research norms" as qualified forms). In
practice — and specifically in the interpersonal-circumplex literature this
package serves — "norms" is the ordinary term for any published octant-level
reference table:

- The instrument authors themselves title their convenience-sample tables
  "norms": kennethlocke.org publishes `CSIE_Norms.html`, `CSIV_Norms.html`,
  and `IEI_Norms.html` (shelf manifest in `cairn/references/norms-audit.md`),
  each carrying single-study college or crowdworker samples.
- The IIP manual (Horowitz et al., 2003) uses "norms" for its genuinely
  representative standardization samples — the same word, for the one case
  where the textbook sense holds.
- The package's own vignette teaches that the operation "is often called
  'norm standardizing'" (`vignettes/using-instruments.Rmd:122`), which is an
  accurate report of field usage, and the SSM methods literature the package
  implements (e.g., Zimmermann & Wright, 2017) instructs standardizing
  against "normative data" without a representativeness audit.

So a user who knows the field's usage is not deceived by the word; a user who
knows only the textbook definition can be. That residual hazard is real but it
attaches to *claims* — prose that says "the normative average" with a definite
article, a column header reading `Population`, silence about which sample was
used — not to the noun itself. Those claims are precisely what M76 and M77
already target. The single sharpest surface is the `Population` column,
because "American college students" under a header named `Population` is the
closest thing in the package to an unqualified representativeness assertion;
but the `?norms` hedge (`R/instrument_oop.R:153-160`) already states the label
names the group drawn from, and question 5's mechanism closes the remaining
gap better than a rename would (see Q5).

One asymmetry deserves note: renaming away from "norms" would itself carry a
cost in accuracy, because for 6 of the 24 shipped samples "norms" is
literally, textbook-correct. A blanket retreat to "reference sample" would
understate the IIP standardization samples in the same motion that it stops
overstating the other 18.

### 2. Does the naming change user behavior?

The defensible mechanism runs through *silence plus a definite article*, not
through the identifier. The hazard scenario is: a user calls
`norm_standardize(..., sample = 1)` — or takes the default — gets z-scores
with nothing in the output saying what they are relative to, and the
surrounding vocabulary ("the normative average", "norm standardizing")
licenses reading them as location in a general population. The user never
confronts the sample-choice question that the M74 measurement shows dominates
(0.44 SD mean, 0.78 SD extreme, versus ~0.12 SD from reference-moment
sampling error).

M76 attacks that mechanism at its strongest point: every non-quiet call names
the sample, its size, and its description, at the moment of use, regardless of
whether the user ever read a help page. That is strictly stronger than a
rename, because a rename is seen only when the call is written (often copied
from someone else's script) and carries at most one bit — "this is not a
norm" — where the message carries the actual facts. M77 then removes the
definite-article prose and the size-resolves-it framing from the teaching
vignette. After both, I can construct no plausible chain in which a user who
reads `norm_standardize` in their own script, past the printed disclosure and
the corrected vignette, is thereby led to skip the sample-choice question. The
prose fix plus the disclosure addresses the mechanism; **no rename of (a)–(c)
is warranted on behavioral grounds.**

Two residual leaks, both fixable without renaming:

- **M76's message can re-import the claim.** If the message prints its facts
  as "population: American college students", the package's own most-visible
  surface (console output on every call) frames a convenience sample with the
  one word that asserts a referent. The message should print the stored value
  as a description ("N = 872, American college students"), not under a
  "population" frame. The brief permits this recommendation ("what the
  message says"); BC2 binds it.
- **The 6-vs-18 distinction is invisible at choice time.** `norms()` output
  is where users choose a sample, and today nothing in it distinguishes a
  national standardization sample from a single-study pool except the free-text
  `Population` string. See Q5.

### 3. If a rename is warranted, what is the minimal sufficient scope?

Ranked by (benefit of renaming) net of (cost). None of (a)–(d) clears the bar.

- **(a) `norm_standardize()` — keep.** Highest cost, least benefit. It is the
  package's most-taught function; it appears in published scripts, teaching
  materials, and the vignette family; the package already spent a breaking
  change at 1.0.0 to arrive at this name (from `standardize()`,
  `NEWS.md:894`), deliberately, to distinguish norm-referenced from
  self/sample-referenced standardization (`self_standardize()`). The verb
  "norm-standardize" is the field's name for the operation (Q1), so the
  identifier is field-accurate even where a given sample is not a norm in the
  textbook sense. Renaming would also break the symmetry with
  `self_standardize()` for no informational gain that the disclosure doesn't
  already deliver.
- **(b) `norms()` — keep.** Same grounds, lower stakes. Its printed output and
  help page are exactly where the qualification belongs (and partly already
  lives); the name is how users find that qualification.
- **(c) `$Norms` slot — keep.** Not a prose surface; users meet it as a data
  structure, and renaming breaks stored instrument objects, `data-raw/`
  builders, and any user code reading `instrument$Norms` — cost without any
  claim-correction benefit. Identifiers are not claims, and this one is barely
  even read.
- **(d) `Population` column — keep, narrowly.** This is the only identifier
  where the word itself does claim-work (a column header is
  documentation-in-data), and if any rename were to happen this would be the
  one. But: M76's AC1/AC2 are specified against this column name; user code
  reading `Norms[[2]]$Population` breaks; and the two cheaper instruments
  already cut the same hazard — the `?norms` hedge saying the label names the
  group drawn from (extended by M77 AC3), and the Q5 field, which puts the
  *kind* beside the *description* so `Population` stops being asked to carry
  both. With those in place the header reads naturally as "who they were",
  which is true. Rejected with reason rather than ranked for renaming.
- **(e) documentation vocabulary alone — adopt, as M77 already scopes it.**
  On the mismatch worry the brief raises: it presupposes the docs abandoning
  the word "norms" while the API keeps it. M77 as planned does not do that —
  it corrects specific false claims (the nationality-matching claim, the
  definite article, size-resolves-it) and characterizes what the samples are,
  while continuing to use the field's vocabulary. Docs and API therefore stay
  in the same vocabulary, precisely qualified, and the mismatch cost the
  brief flags does not arise. What (e)-alone genuinely leaves open is the
  choice-time visibility gap, which is Q5's territory, not a vocabulary
  mismatch.

### 4. What would you rename it to?

Answered for completeness since Q3 concludes no identifier changes.

- **"Reference sample" / `reference_standardize()`** — the best candidate.
  Accurate for all 24 samples, no false import. Costs: it is nobody's search
  term (users of these instruments look for "norms"); it severs the docs from
  the literature the package teaches (GP5 argues for teaching the field's own
  vocabulary, precisely qualified — not a private vocabulary); and it
  flattens the 6-vs-18 distinction, mislabeling the IIP standardization
  samples downward.
- **"Comparison sample"** — imports its own false implication: in the
  psychological literature a "comparison sample/group" connotes a
  study-internal control or contrast group, inviting the reading that the
  user's own data are being compared to a matched design element rather than
  to an external published table. Weaker than "reference sample".
- **Term of art:** there is no single established noun for a published
  non-representative reference distribution. The field marks the distinction
  by *qualifying* "norms" — "local norms", "convenience norms", "research
  norms" (the *Standards*' own treatment of local norms is the canonical
  case). That the field solves this problem with qualification rather than a
  different noun is independent support for the keep-and-qualify verdict:
  the package should do what the field does, i.e., keep "norms" and mark the
  kind per sample.

### 5. Is the sample-level distinction more useful than a blanket rename?

Yes, and it is the one genuinely missing piece. If forced to choose between a
blanket rename and per-sample kind marking, **choose the marking** — on both
axes the brief names:

- **Accuracy.** A blanket rename applies one word to 24 samples that are not
  one kind of thing; it fixes the overstatement of 18 by understating 6. A
  per-sample field states, for each sample, what the provenance audit
  actually established: the iip32/iip64 sets (6) are national standardization
  samples; 16 are the participant pools of identified single studies; 2
  (iis32 sample 1, ipipipc sample 1) have no identified source at all, which
  is a fact users choosing a sample deserve to meet *before* the help page
  fine print.
- **Usability.** The choice among samples happens while reading `norms()`
  output and is consumed at the `norm_standardize()` call. A kind field
  printed by `norms()` and included in M76's message and attribute puts the
  decision-relevant fact at both moments. A rename puts one bit at neither
  moment — it is visible only in the function's spelling.

Concretely: add a column to `Norms[[2]]` beside `Population` with a small
controlled vocabulary — something like `"standardization"` (drawn to
represent a defined population), `"study"` (participant pool of an identified
study), `"unknown"` (no identified source) — populated from
`cairn/references/norms-audit.md`, printed by `norms()`, and carried by M76's
message and attribute. It is additive (no existing identifier changes, no new
dependency), it is cheap inside the pre-2.0 window because the `Norms[[2]]`
schema is being touched by nothing else, and it converts the `?norms` hedge
from a global caution into per-row data. The exact vocabulary and its
labeling in printed output are milestone-plan decisions; BC3 binds the
partition, which is the load-bearing part. Fine-grained kinds beyond three
(e.g., distinguishing community cohorts from undergraduate pools) are not
worth the assignment ambiguity; the free-text `Population` already carries
that texture.

### 6. Deprecation mechanics, if a rename were warranted

Moot under the Q3 verdict, but answering as asked. GP4 does not bind before
v2.0.0 and D-040 has already used exactly that ground for a behavior change,
so no deprecation cycle is *owed* as a matter of principle — a pre-2.0 rename
could be clean. But "not owed" is not "free": `norm_standardize()` has been
the exported name since 1.0.0, sits in published scripts and course
materials, and the package already broke this exact call once
(`standardize()` → `norm_standardize()`), so a second break re-teaches the
same users the same lesson with less cause. Since the package already uses
`lifecycle`, the marginal cost of a one-release deprecated alias is near
zero, and I would recommend one for (a)/(b) in any rename scenario; a data
column (d) cannot be aliased cleanly (a duplicated column is its own
confusion), which is a further reason (d) should change only for decisive
cause. None of this changes the Q3 answer; the cost side was never the
decisive term — the benefit side is small once M76's disclosure exists, and
it stays small at any price.

### 7. What would falsify this recommendation?

Concrete and observable; each reopens the named part.

- **F1 (reopens the rename of (a)/(d)):** after a release carrying M76's
  disclosure and M77's prose, three or more independent documented instances —
  GitHub issues, support mail, or published papers citing circumplex ≥ 2.0.0 —
  in which a user describes convenience-referenced z-scores as locating
  respondents in a general or national population, *with the disclosure
  demonstrably in effect* (non-quiet call, or the attribute present in their
  workflow). This is the M76/M77 plan-gate falsifier ("users read the
  column/word itself as the claim") made countable.
- **F2 (reopens Q5's design):** the kind-field taxonomy proves unassignable —
  if, when populating it from the audit record, more than 4 of the 24 samples
  cannot be placed in one of the three kinds without new source research, the
  three-kind vocabulary is wrong and the field should be redesigned (or
  reduced to standardization-vs-other) rather than shipped half-empty.
- **F3 (reopens Q1's keep-the-word verdict):** a field-level vocabulary
  shift — a revision of the AERA/APA/NCME *Standards*, or guidance in a major
  assessment journal adopted by the interpersonal-circumplex literature, that
  deprecates "norms" for non-representative reference tables. The package
  should follow the field's vocabulary when the field moves, not before.
- **Explicitly not sufficient to reopen:** a reverse-dependency scan showing
  few external callers of `norm_standardize()` (that lowers the rename's
  cost, but the verdict rests on the benefit being near zero post-M76, which
  cheapness does not change); style or modernization preference (GP7); and a
  single anecdote of misreading without the disclosure in effect.

## Beyond the brief

- **B1.** `norm_standardize()`'s roxygen opens with "normative data (from the
  package or custom)" (`R/tidying_functions.R:127`), but the signature offers
  no custom-norms path — `instrument` must be a `circumplex_instrument`, so
  "custom" is reachable only by hand-constructing an instrument object,
  which no documentation teaches. M77's AC4 sweep will not catch this (its
  pattern matches none of the phrase). Either drop "or custom" or say what it
  actually means. Small, but it is a claim about the API in the very help
  page under review.
- **B2.** M76 AC2's attribute is specified to record the sample's
  `Population`. If BC3's kind field is adopted, the attribute (and message)
  should carry the kind as well — otherwise the attribute preserves the
  description while dropping the classification that Q5 argues is the
  decision-relevant fact. Noted here rather than as a BC because it follows
  mechanically from adopting R3.
- **B3.** The vignette's own phrase "normative or comparison sample"
  (`using-instruments.Rmd:122`) will survive M77's rewrite of that line's
  *claims*; given Q4's finding that "comparison sample" carries a
  control-group connotation, the rewrite should prefer "reference sample" as
  the in-prose gloss where a synonym is wanted. Prose only; no identifier
  involved.

## Recommendations

- **R1 — apply.** Keep all four identifier surfaces unchanged:
  `norm_standardize()`, `norms()`, `$Norms`, `Norms[[2]]$Population`. The
  plan gates' provisional keep-the-names choice is confirmed, now on the
  merits rather than provisionally: the identifiers are field-accurate usage
  (Q1), the behavioral mechanism is closed by M76+M77 (Q2), and the rename's
  benefit is near zero at any cost (Q3, Q6). The ROADMAP candidate row for
  the rename can be closed rather than deferred.
- **R2 — apply.** Constrain M76's message prose: print the stored
  `Population` value as a plain description, without the framing token
  "population" and without representativeness wording (BC2). This is the one
  place the naming verdict requires a change to what the message says.
- **R3 — apply.** Add the per-sample reference-kind field to `Norms[[2]]`
  (Q5): machine-readable, three-kind controlled vocabulary, populated from
  the audit record, printed by `norms()`, carried by M76's message and
  attribute (B2). Additive, no identifier renamed, no new dependency. Cheapest
  inside the pre-2.0 window while `Norms[[2]]` documentation and disclosure
  are already open; scheduling is the maintainer's.
- **R4 — consider.** In M77's rewritten passages, gloss the samples as
  "reference samples" where a synonym for "normative samples" is wanted, and
  avoid "comparison sample" (B3, Q4).
- **R5 — reject (with reason): rename any of (a)–(d)** — including the
  otherwise-tempting `Population` column: its residual hazard is closed more
  cheaply and more accurately by R2+R3 plus M77's AC3 hedge than by a rename
  that breaks M76's spec and user code (Q3d).
- **R6 — reject (with reason): a deprecation-cycle rename as a compromise.**
  A soft rename (new name + lifecycle alias) still forks the vocabulary
  users search and cite, for the same near-zero benefit; deprecation
  mechanics mitigate cost, not pointlessness (Q6).

## Binding criteria

- BC1. After the constrained milestones merge, `norm_standardize` and `norms`
  both appear in `NAMESPACE` as exports under exactly those names, and every
  instrument returned by the enumeration `Rscript -e
  'utils::data(package="circumplex")'` filtered to objects inheriting
  `circumplex_instrument` (the RB16 Materials enumeration) has a `Norms` list
  slot whose second element contains a column named `Population`. Tolerance:
  exact; any departure is a deviation.
- BC2. The disclosure message emitted by `norm_standardize()` (M76 AC1)
  contains the selected sample's `Norms[[2]]$Population` value verbatim, and
  its fixed (non-data) message text contains neither the token "population"
  nor the token "representative", case-insensitively; asserted by the AC1
  message-form tests over the same instrument enumeration BC1 names, which
  covers every shipped message variant because AC1's two message forms
  partition that enumeration. Tolerance: exact string absence in the fixed
  text; a shipped `Population` *value* containing those tokens does not
  violate this criterion.
- BC3. Every shipped `Norms[[2]]` carries a machine-readable column
  classifying each sample's reference-distribution kind under a controlled
  vocabulary of exactly three values (drawn-to-represent standardization
  sample; identified-study participant pool; no identified source). Over the
  BC1 enumeration the kind counts are exactly 6 standardization (the iip32
  and iip64 samples), 16 identified-study, and 2 no-identified-source (iis32
  sample 1, ipipipc sample 1), totalling 24; `norms()` prints the kind for
  every sample it lists. Procedure: the BC1 enumeration extended to read the
  new column and tally kinds. Tolerance: exact counts; if the shipped roster
  changes size, the partition is re-derived from
  `cairn/references/norms-audit.md` and the deviation shown.
