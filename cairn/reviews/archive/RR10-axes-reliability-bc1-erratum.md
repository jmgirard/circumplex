# RR10: BC1 Layer-A oracle handling for the IIP S6 Self paper erratum — Review Report

- **Date:** 2026-07-23
- **Reviewer:** Independent expert review (Fable), per `cairn/reviews/RB10-axes-reliability-bc1-erratum.md`
- **Scope:** BC1's handling of the IIP Sample 6 Self row of Strack et al. (2013) Table 3 only; the RR09 GO and BC2–BC13 are fixed (D-026) and untouched.
- **Verdict:** The erratum diagnosis is **correct with one documented nuance**; adopt **option (a)** with a change-detector assertion; **BC1 is revised** (exact replacement text in the Binding criteria section below).

Materials read: the brief; `cairn/references/strack2013.md` (Table 3 section and
erratum paragraph); `cairn/reviews/archive/RR09-axes-reliability-strack.md`
(BC1 and §6); the shelf PDF `cairn/references/sources/strack2013.pdf` p. 7
(Table 3 read directly via `pdftotext`, both `-layout` and `-raw` modes,
agreeing). I independently recomputed every Spearman–Brown value in the brief's
twelve-row table (all reproduce exactly as stated), the rounding-band analysis
for the competing hypotheses, the SEm bands for the erratum row, and the
component sums for **every** row of Table 3 — the last two checks use printed
values (cols 12–13 for IIP S6; cols 5–9 table-wide) that the brief's materials
did not bank, and both produced findings reported below.

---

## 1. Is the erratum diagnosis correct?

**Yes — IIP S6 Self is best treated as a single-digit source typo in col 6
(%axes printed 13.0, true value 12.0, true ξ1 ≈ .12), with one nuance from the
SEm column that slightly complicates but does not overturn the diagnosis.**

The competing explanations, examined in turn:

**(i) "The paper computed reliability from an unrounded ξ1 that legitimately
rounds to 13.0" — impossible.** SB(x, 32) is strictly increasing in x, and a
printed reliability of .81 requires SB(x, 32) ∈ [.805, .815), i.e.
x ∈ [.1143, .1210) (computed by root-finding; verified). The largest ξ1
consistent with a printed .81 prints as %axes 12.1; no value rounding to 13.0
can round to .81. Conversely, SB over the entire 13.0 rounding band
[12.95, 13.05) is [.8264, .8277), which rounds to .83 throughout. The rounding
hypothesis is arithmetically excluded, not merely unlikely.

**(ii) "The reliability .81 is the typo (true .83, ξ1 = .13 correct)" — fits
one column better but requires two independent errors.** This hypothesis has
one genuine piece of support that the brief's materials did not surface: the
printed SEm. Reading Table 3 directly, IIP S6 Self prints Raw variance 0.30
(col 12) and SEm 0.23 (col 13). `SEm = √var · √(1 − Rel)` gives:

- under the reliability-typo hypothesis (Rel band [.8264, .8277] from
  ξ1 = .13): SEm ∈ [.2255, .2301] → rounds to **.23 across the whole band**;
- under the %axes-typo hypothesis (Rel band [.8128, .8143] from ξ1 ≈ .12):
  SEm ∈ [.2341, .2389] → central value .2365 rounds to **.24**; only the lower
  corner of the joint rounding band (unrounded ξ1 near .1205 and var near
  .295) rounds to .23.

So the printed SEm .23 mildly favors the reliability-typo reading. **But that
reading cannot explain the 101.0% component sum**: the components print
17.7 + 13.0 + 2.4 + 67.9 = 101.0, and rounding of four one-decimal components
can drift at most ±0.2 — a 1.0 excess is a genuine error of magnitude ~1.0 in
some component. The reliability-typo hypothesis therefore needs **two
independent print errors** in one row (a reliability digit and an unidentified
~1.0 component error), whereas the %axes-typo hypothesis needs **one** (a
single digit, 12→13), explains both anomalies exactly, and remains consistent
with the printed SEm within legitimate rounding bands (SEm .23 is attainable
under it, just not central). Under the reliability-typo hypothesis the
erroneous component is also unidentifiable (general 17.7→16.7? item
67.9→66.9? scale 2.4→1.4? — each fixes the sum, none is singled out), while
the %axes correction is uniquely pinned by two independent constraints.

**(iii) "Some other single component is the typo" (e.g. general 17.7 → 16.7)
— fixes the sum but not the reliability.** SB(.13, 32) = .8270 still misses
the printed .81 by .017. Only the %axes correction satisfies both constraints
simultaneously.

**Conclusion.** The %axes-typo diagnosis is the unique single-error
explanation and is over-determined: %axes = 12.0 is fixed by the sum identity
*alone* (100.0 − 17.7 − 2.4 − 67.9 = 12.0), and then independently reproduces
the printed reliability (SB(.12, 32) = .8136 → .81, |Δ| = .0036). Parsimony
decisively favors it. The SEm nuance means the diagnosis is "almost certain,"
not certain — which is one more reason the oracle handling (Q2) should consist
of assertions that are true *numeric facts about the printed and corrected
values* under either hypothesis, rather than an assertion that the true ξ1 was
.12.

## 2. How should the Layer-A oracle handle this row?

**Option (a), with one addition: an explicit inconsistency assertion that
turns the erratum into a change-detector.** Concretely, the oracle should:

1. **Sweep the eleven self-consistent rows strictly at ±.01** (they pass with
   max |Δ| = .0053; verified all eleven).
2. **Assert the printed IIP S6 Self pair is inconsistent:**
   `|SB(.130, 32) − .81| > .01` (actual .0170). This assertion is what option
   (b) throws away: it pins the banked printed values *as banked*, so if a
   future re-extraction, a corrected publisher PDF, or a well-meaning "fix" to
   `strack2013.md` silently changes 13.0 or .81, the test fails and forces a
   human look rather than silently passing a now-different oracle.
3. **Assert the sum-restoring correction reproduces print:**
   `|SB(.120, 32) − .81| ≤ .005` (actual .0036, anchor-row tightness).

Why this best preserves BC1's intent: BC1's purpose (RR09 §6) is that the SB
implementation reproduces published reliabilities against values the
implementation does not control. Option (a) keeps all twelve rows exercising
the SB code path; the erratum row still tests SB against a printed output
(.81) from an input the source itself determines via its own sum identity —
the corrected 12.0 is *not* fitted to make the test pass, it is forced by the
paper's printed components independently of the reliability match, so the
circularity worry does not bite. Assertions 2 and 3 are hypothesis-robust:
they are true arithmetic facts about the printed and corrected values whether
the underlying truth is ξ1 = .12 (diagnosed) or ξ1 = .13 with a misprinted
reliability (the residual alternative), so the oracle never asserts a claim
that the Q1 nuance could falsify.

Rejected options:

- **(b) exclude entirely** — loses a twelfth SB exercise and, more
  importantly, loses the change-detection of assertion 2; an excluded row with
  a prose rationale can rot silently.
- **(c) keep all twelve at ±.01** — fails as a matter of arithmetic, and would
  "pass" only by widening the tolerance to ±.02, which would blunt the sweep
  for all rows to accommodate one known source defect. An oracle must not
  assert against a value the source itself got wrong; tolerances should
  reflect rounding propagation, not errata.

## 3. Does BC1 need a clarified/superseding binding criterion?

**Yes.** BC1 as written ("**every** non-blocked type-a row … within ±.01") is
not literally satisfiable against the printed table, M54 ingests it verbatim,
and `cairn_validate` string-matches it — so the handling cannot live only in
oracle documentation; the criterion text itself must change, and per the
brief's constraint that change comes from this review. The exact replacement
text is in the **Binding criteria** section at the end of this report. It
narrows the ±.01 sweep to the eleven internally self-consistent rows (defined
by the component-sum property *and* enumerated by exclusion, so the criterion
is checkable without re-deriving the property), and folds in the three
erratum-row assertions of Q2 plus the component-sum guard of Q4 with stated
tolerances.

## 4. Adjacent oracle strengthening (component-sum guard)?

**Apply — but scoped to the twelve banked rows only, not Table 3 at large.**
Asserting each banked row's printed components sum to 100.0 (±0.1; all eleven
self-consistent rows sum to exactly 100.0) and that IIP S6 Self sums to 101.0
(±0.1) is cheap, catches future mis-transcription of any single component
digit (a 0.1-level slip in one component moves the sum outside ±0.1), and
documents the erratum in executable form. It requires banking Table 3's
component columns (cols 5–9) for the twelve rows in `strack2013.md` — a small
transcription from the already-extracted page (the full rows are in the text
layer read for this review).

**The scoping matters because the guard would be false table-wide.** Checking
every row of Table 3 from the text layer (both `pdftotext` modes agree), the
rows not summing to 100.0 ± 0.1 are:

- **IIP S6 Self: 101.0** — the erratum under review;
- **CSIV S7 Self (blocked, excluded from the sweep): 102.9**
  (13.5 + 14.8 + 4.2 + 2.8 + 67.6) — a second apparent source anomaly; its SB
  still reproduces its printed reliability (SB(.148, 32) = .8475 → printed
  .84, |Δ| = .0075), so if anything its *component* columns carry the defect;
- **OCAI S15 Meta: 100.6** (48.2 + 7.3 + 3.4 + 5.2 + 36.5);
- **MEIL S14 Self: 74.4** (4.3 + 5.5 + 27.9 + 36.7) — so far off that an
  extraction artifact (a dropped value in the text layer) is plausible;
  needs a page-image read before being called a source error.

None of these three additional rows is in the BC1 sweep population (blocked
type-a, type-d Meta, type-c respectively), so BC1 is unaffected — but a guard
naively phrased as "all rows of Table 3 sum to 100" would fail, and the
source note's claim that IIP S6 Self is "the only row in Table 3 that does not
sum to 100.0%" is an overclaim (see B-1).

---

## Beyond the brief

- **B-1.** The erratum paragraph in `cairn/references/strack2013.md` states
  IIP S6 Self is "the only row in Table 3 that does not sum to 100.0%." Per
  the table-wide check in Q4 this is true only of the twelve-row sweep
  population; table-wide, CSIV S7 Self (102.9), OCAI S15 Meta (100.6), and
  possibly MEIL S14 Self (74.4 in the text layer; unverified against the page
  image) also miss. The paragraph should be narrowed to "the only
  **non-blocked type-a** row…" and may note the others. This is a
  documentation fix to the source note, not a BC.
- **B-2.** The printed SEm for IIP S6 Self (col 13 = .23, col 12 = .30) is the
  one printed value in mild tension with the %axes-typo diagnosis (Q1 nuance:
  central prediction .2365 → .24). BC2 is untouched (it names only the IAL,
  OCAI, and COC anchor rows), but the build should not add IIP S6 Self to any
  SEm cross-check, and the source-note erratum paragraph could record the
  nuance in one sentence for the next reader who tries to adjudicate.
- **B-3.** The Table 3 caption says "28 sub-samples" while the table has 29
  data rows (and the paper's p. 5 fit summary says 29 models, as banked). A
  curiosity confirming this table saw imperfect copy-editing; no action.

## Recommendations

1. **Apply.** Replace BC1 with the revised criterion below (Q2 option (a) +
   the Q4 sum guard, scoped to the twelve banked rows).
2. **Apply.** Bank Table 3 cols 5–9 for the twelve sweep rows in
   `cairn/references/strack2013.md` so the sum guard runs against banked
   values (Q4).
3. **Apply.** Narrow the source-note erratum paragraph's "only row in
   Table 3" claim to the sweep population, noting CSIV 102.9 and OCAI-Meta
   100.6 (B-1), and add one sentence recording the SEm nuance (B-2).
4. **Consider.** A page-image read of the MEIL S14 Self row to settle whether
   its 74.4 text-layer sum is an extraction artifact or a further source
   error; only relevant if the oracle ever extends beyond type a (Q4/B-1).
5. **Reject (widening the sweep tolerance to ±.02 to keep all twelve rows,
   option (c)).** It accommodates a known source defect by blunting the test
   for the eleven good rows; tolerances must reflect rounding propagation,
   not errata (Q2).
6. **Reject (excluding IIP S6 Self outright, option (b)).** Loses the
   change-detector on the banked printed values and a twelfth exercise of the
   SB path (Q2).

## Binding criteria

The following supersedes RR09's BC1 in full. BC2–BC13 stand unchanged.

- **BC1 (Layer A, reliability).** Spearman–Brown on Table 3's printed col 6
  (/100) and col 10 must reproduce col 11 for the four anchor rows (IAL S1
  Self; IPI-A S9 Self; OCAI S15 Self; COC S16 Self) within ±.005, and for
  every internally self-consistent non-blocked type-a row of Table 3 within
  ±.01 — the eleven rows whose printed variance components sum to 100.0,
  i.e. all non-blocked type-a rows except IIP S6 Self. IIP S6 Self is a
  documented source erratum and is handled by three assertions instead of the
  sweep: (i) its banked printed components sum to 101.0 (±0.1);
  (ii) the printed pair is inconsistent: |SB(.130, 32) − .81| > .01;
  (iii) the sum-restoring single-digit correction reproduces print:
  |SB(.120, 32) − .81| ≤ .005. The oracle must additionally assert, for each
  of the eleven self-consistent rows, that its banked printed components sum
  to 100.0 (±0.1); this sum guard applies only to the twelve banked
  non-blocked type-a rows, not to Table 3 at large, and requires Table 3's
  component columns (cols 5–9) for these twelve rows to be banked in
  `cairn/references/strack2013.md`.

## Conclusion

The IIP S6 Self row is a source erratum, best explained by a single-digit
%axes typo (13.0 for 12.0; true ξ1 ≈ .12) — the unique one-error account,
over-determined by the sum identity and the printed reliability jointly, with
a mild and documented SEm nuance that does not overturn it. The Layer-A
oracle should sweep the eleven self-consistent rows at ±.01 and pin the
erratum row with hypothesis-robust assertions (printed pair inconsistent;
corrected pair reproduces), plus a component-sum guard over the twelve banked
rows. BC1 is revised accordingly; the exact replacement text above is ready
for verbatim ingestion. Nothing outside BC1's scope is implicated beyond the
source-note documentation fixes flagged in B-1/B-2.
