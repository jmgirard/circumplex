# RB10: BC1 Layer-A oracle handling for the IIP S6 Self paper erratum (M54)

- **Date:** 2026-07-23
- **Output required:** write findings to `cairn/reviews/RR10-axes-reliability-bc1-erratum.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`circumplex` is an R package for circumplex data analysis (Structural Summary
Method). Milestone **M54** is building `axes_reliability()`, a standalone
estimator of the reliability of the circumplex axes via a restricted
tau-equivalent CFA, per Strack, Jacobs & Grosse Holtforth (2013), *Reliability
of Circumplex Axes* (SAGE Open 3(2)). The build was GO'd by an earlier Fable
review, **RR09**, subject to thirteen binding criteria BC1–BC13, which M54
ingests verbatim as acceptance criteria.

**BC1** (RR09, verbatim): *"Spearman–Brown on Table 3's printed col 6 (/100)
and col 10 must reproduce col 11 for the four anchor rows (IAL S1 Self; IPI-A
S9 Self; OCAI S15 Self; COC S16 Self) within ±.005, and for every non-blocked
type-a row of Table 3 within ±.01."*

The Spearman–Brown "list-length" reliability is
`Rel = (item_n · ξ1) / (1 + (item_n − 1) · ξ1)`, with `ξ1 = %axes/100` (col 6)
and `item_n` the per-axis Σwᵢ² (col 10). Implementing the Layer-A oracle
(BC1), we banked all twelve non-blocked type-a rows of Table 3 from the source
PDF (two channels: the born-digital `pdftotext -layout` text layer and an AI
read of the page-image render; both agree on every value). SB reproduces the
printed reliability for **11 of the 12** rows within ±.01. The twelfth,
**IIP Sample 6 Self**, misses by **.017** and appears to be a **paper
erratum**: its printed variance components sum to **101.0%** (the only row in
Table 3 not summing to 100.0%), and a single-digit correction of its printed
`%axes` from 13.0 to **12.0** both restores the 100.0% sum and reproduces the
printed reliability .81 (`SB(.12, 32) = .8136`). Because BC1 says "**every**
non-blocked type-a row … within ±.01", it is not literally satisfiable against
the printed value for this one row. The M54 implementer will not soften a
Fable-authored binding criterion unilaterally; hence this brief.

## Materials

- `cairn/references/strack2013.md` — the source note. Read the section
  "### Table 3 — the published-value oracle (p. 7)", including the banked
  twelve-row type-a table, the BC2 SEm inputs, and the **"Erratum (IIP Sample 6
  Self)"** paragraph. If the shelf PDF is available at
  `cairn/references/sources/strack2013.pdf` (gitignored), Table 3 is on p. 7;
  otherwise rely on the banked values in the source note.
- `cairn/reviews/archive/RR09-axes-reliability-strack.md` — the prior review;
  BC1 is in its "## Binding criteria" section; §6 explains the Layer-A oracle
  rationale and the ±.005/±.01 tolerance choice.
- The twelve banked non-blocked type-a rows (col 6 %axes, col 10 item_n,
  col 11 Reliability), with SB and |SB − printed|:

  | Instrument | Sample | Persp. | %axes | item_n | Rel | SB | Δ |
  |---|---|---|---|---|---|---|---|
  | IAL | 1 | Self | 26.0 | 32 | .92 | .9183 | .0017 |
  | IAL | 1 | Other | 26.1 | 32 | .92 | .9187 | .0013 |
  | IAL | 2 | Self | 23.0 | 32 | .90 | .9053 | .0053 |
  | IAS-R | 3 | Self | 22.9 | 32 | .90 | .9048 | .0048 |
  | IAS-R | 3 | Other | 21.5 | 32 | .90 | .8976 | .0024 |
  | IIP | 4 | Self | 11.8 | 32 | .81 | .8106 | .0006 |
  | IIP | 5 | −t1 | 13.2 | 32 | .83 | .8295 | .0005 |
  | IIP | 5 | −t2 | 11.8 | 32 | .81 | .8106 | .0006 |
  | **IIP** | **6** | **Self** | **13.0** | **32** | **.81** | **.8270** | **.0170** |
  | IMI | 6 | Other | 27.9 | 32 | .92 | .9253 | .0053 |
  | SAS-C | 8 | Self | 17.8 | 32 | .87 | .8739 | .0039 |
  | IPI-A | 9 | Self | 13.4 | 16 | .71 | .7123 | .0023 |

  Component sums (general + axes + scale + item): IIP S6 Self =
  17.7 + 13.0 + 2.4 + 67.9 = **101.0**; its siblings (e.g. IIP S5 −t1 =
  16.6 + 13.2 + 1.5 + 68.7) = **100.0**. With %axes = 12.0 the S6 Self sum is
  100.0 and `SB(.12, 32) = .8136 → .81`.

- Reproduce in R: `sb <- function(x,n) (n*x)/(1+(n-1)*x)`.

## Questions

1. **Is the erratum diagnosis correct?** Given the unique 101.0% component sum,
   the sibling rows summing to 100.0%, and that the sum-restoring correction
   (%axes 13.0 → 12.0) also reproduces the printed reliability, is IIP S6 Self
   best treated as a source typo in `%axes` (true ξ1 ≈ .12)? Or is there a
   more likely explanation (e.g., the reliability itself is the typo, or the
   paper computed reliability from an unrounded ξ1 that legitimately differs
   from the rounded print)?

2. **How should the Layer-A oracle (BC1) handle this row** so that it validates
   the Spearman–Brown implementation faithfully without asserting against a
   value the source itself got wrong? Options the implementer sees: (a) sweep
   the 11 self-consistent rows strictly at ±.01 and additionally pin IIP S6
   Self against the corrected %axes 12.0; (b) sweep the 11 and exclude IIP S6
   Self entirely with a documented rationale; (c) keep all 12 at ±.01 (fails);
   (d) some other handling. Which best preserves BC1's *intent* (that the SB
   implementation reproduces the published reliabilities)?

3. **Does BC1 need a clarified/superseding binding criterion?** Because M54
   ingests BC1 verbatim and `cairn_validate` string-matches it, any change to
   how BC1 reads must come from you as a revised binding criterion. If you
   judge a clarification warranted (e.g. "every non-blocked type-a row that is
   internally self-consistent (components sum to 100.0 ± rounding)"), provide
   the **exact replacement text** for BC1 in a `## Binding criteria` section so
   it can be ingested verbatim. If BC1 should stand unchanged with the handling
   living only in the oracle's documentation, say so explicitly.

4. **Any adjacent oracle strengthening?** Should the Layer-A oracle also assert
   the component-sum self-consistency (each banked row's five components sum to
   100.0 ± a rounding tolerance) as a guard that would catch a future
   mis-transcription or a similar source slip? Consider, not required.

## Constraints

- The RR09 **GO** and its BC2–BC13 are fixed (D-026) and not under review here;
  only BC1's handling of this one erratum row is in scope. Flag explicitly if
  you believe anything outside this scope is implicated.
- The Spearman–Brown formula and the four anchor rows are **not** in question —
  all four anchors reproduce within ±.005 and all self-consistent type-a rows
  within ±.01; the SB implementation is correct.
- No new package dependency; this is a test-oracle/documentation question.
- The source values are as banked in `strack2013.md`; both extraction channels
  read "%axes 13.0" and "Reliability .81" for IIP S6 Self, so this is a source
  defect, not a transcription error to re-check.

## Output format

In `RR10-axes-reliability-bc1-erratum.md`: answer each question by number with
your reasoning and evidence; list any additional findings under "Beyond the
brief"; end with concrete recommendations, each marked apply / consider /
reject-with-reason. If your answer to Q3 revises BC1, emit a `## Binding
criteria` section containing the exact criterion text to be ingested verbatim
(numbered `BC1`), with tolerances stated; otherwise state that BC1 stands
unchanged and no `## Binding criteria` section is needed.
