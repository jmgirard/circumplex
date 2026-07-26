# RB11: Fractional `item_n` and the SYMLOG Table 3 rows (M61)

- **Date:** 2026-07-26
- **Output required:** write findings to
  `cairn/reviews/RR11-axes-reliability-fractional-item-n.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`circumplex` is a CRAN R package for circumplex data analysis. One of its
estimators, `axes_reliability()`, implements the item-level restricted
tau-equivalent CFA of Strack, Jacobs & Grosse Holtforth (2013, *SAGE Open* 3(2),
doi:10.1177/2158244013486115), which decomposes each circumplex item's variance
into orthogonal components — a general factor (ξ2), the two circumplex axes
(ξ1), scale specificity (ζ1), block specificity (ζ2, not modelled here), and
item specificity (ε) — and reads each axis's reliability off the isolated axes
component with the Spearman–Brown "list-length" formula:

```
Rel_axis = (item_n * xi1) / (1 + (item_n - 1) * xi1)
```

`item_n` is the axis's **effective test length**: the sum of squared item
weights on that axis, `sum_i n_i * cos^2(theta_i)` for the X axis and
`sum_i n_i * sin^2(theta_i)` for the Y axis, where `theta_i` is scale *i*'s
angular position and `n_i` its item count. Strack's Table 3 prints it as
column 10, headed "Items/axis".

The estimator has shipped in three stages. **M54** built it for the canonical
eight octant scales. **M59** added a correlation-matrix input path. **M60**
relaxed the accepted input from "the octant set" to **any equally spaced set of
scale angles at any rotation**, on the identity that for `k` equally spaced
scales carrying `n` items each, both axes get `item_n = n * k / 2` *at every
rotation* (for `k >= 3`) — which is what keeps the model's equal-axis-variance
restriction ("no preferred rotation") as innocuous for a rotated or non-octant
set as it is for octants. Unequal spacing (a quasi-circumplex) stays refused.

**M61**, the milestone this brief serves, is the next step: allow an instrument
with **one item per scale position**, which forces the scale-specificity
component ζ1 to be dropped (with one item per position, no same-scale item pair
exists, so ζ1 is unidentified and its design column is empty). This is Strack's
circumplex **types e and f**, and the paper's Table 3 publishes six such rows —
three for the COC instrument and three for SYMLOG — which M61 planned to use as
its Layer-A published-value oracle.

### The finding that triggered this brief

M61's acceptance criterion AC4 reads, verbatim:

> - [ ] AC4: fractional item_n works end to end (8.67), and the OLS shadow
>   returns a two-component seed instead of erroring when the same-scale design
>   column is empty.

The `8.67` is SYMLOG's printed Table 3 column-10 value, and the implementing
session found it **appears unreachable end to end under the package's own input
contract**. The reasoning, which this brief asks you to confirm or refute:
with equal spacing and exactly one item per position, per-axis
`item_n = k / 2`, so `item_n = 8.67` would require `k = 17.34` scale positions.
Meanwhile Table 1 lists SYMLOG (type f) with **Scales `—`, Blocks `—`, Items
26**, and `26 / 3 = 8.667`, which suggests SYMLOG's item weights are direction
cosines in a **three**-dimensional space rather than the model's two — SYMLOG
(Bales) is conventionally described as a three-dimensional system. By contrast
the type-e instrument is clean and reachable: COC has 16 items and no scales,
so 16 single-item positions give `item_n = 16 / 2 = 8`, exactly as printed.

If that reading holds, three things follow that need an independent opinion
before M61 proceeds: whether the SYMLOG rows are a legitimate oracle at all,
what fixture should stand in for "fractional `item_n`, end to end", and how AC4
must be re-worded. The implementing session's own inclination is stated in the
questions below and should be treated as a proposal to test, not a conclusion
to ratify.

## Materials

Read these. Paths are repo-relative from the repository root.

**The milestone plan and the tracking record**

- `cairn/milestones/M61-axes-reliability-single-item.md` — the whole file: Goal,
  Scope, all eight acceptance criteria, the Coverage map, tasks T1–T9, the work
  log, and the milestone-local decision M61-D1.
- `cairn/DECISIONS.md` — entries **D-031** (what M60/M61 are permitted to
  change), **D-030**, and **D-026** (what stays deferred). Read each entry
  whole, not only its heading.

**The source and its committed note**

- `cairn/references/strack2013.md` — the committed source note. Note what it
  already banks (the twelve non-blocked type-a rows, the four type-b rows, the
  type-c row) and, at the end of its Provenance block, its explicit statement
  that the **type-e and type-f rows are not yet banked and carry no
  verification claim** — they are M61's to bank.
- `cairn/references/sources/strack2013.pdf` — the article itself. It is
  gitignored but present in this working tree. It is born-digital, so
  `pdftotext -layout` gives the typeset text faithfully. The load-bearing pages
  are **p. 2** (Figure 1's circumplex types a–f, and Table 1 "Instruments
  Examined", whose columns are Type / Instrument / Developers / **Scales** /
  **Blocks** / **Items** / Rating), **p. 3** (the item weights on the axes, and
  the Nunnally–Bernstein formula), **p. 5** (the sentence beginning "The
  Nunnally–Bernstein formula was not applied for analyzing instruments with a
  single item per spatial position"), and **p. 7** (Table 3, whose column 1 is
  the circumplex Type and whose column 10 is "Items/axis"). Extract with e.g.
  `pdftotext -layout -f 7 -l 7 cairn/references/sources/strack2013.pdf -`.
  Read whatever else in the paper bears on SYMLOG's geometry.

**The code**

All in `R/axes_reliability.R` unless stated:

- **lines 60–79** — `axis_item_n()`, the effective-test-length computation,
  with the comment block explaining why octant sets give exact integers while
  non-octant sets carry ~1e-14 float error and must be compared with a
  tolerance.
- **lines 19–22** — `axis_weights()` and its `snap_trig()` pole handling.
- **lines 89–91** — `axis_reliability_sb()`, the Spearman–Brown formula.
- **lines 98–100** — `axis_sem()`.
- **lines 182–192** — `axes_ols_shadow()`, the SEM-independent least-squares
  estimate of the three components; its third design column
  (`outer(item_scale, item_scale, "==")`) is the one that goes empty at one
  item per position, which is the second half of AC4.
- **lines 549–611** — the refuse contract: the finite-angle gate, the
  four-scale identification floor, the equal-spacing `switch()`, and (line 609)
  the `>= 2 items per scale` refusal that M61 relaxes.
- **lines 760–766, 828–844** — where `item_n` reaches the results frame and the
  four-row components frame.

**The tests**

In `tests/testthat/test-axes-reliability.R`:

- **lines 1143–1172** — `"M60: per-axis item_n is n * k/2 at any rotation"`,
  including its explicit tolerance rationale and its unbalanced-set case, which
  already produces per-axis `item_n` values that differ between the two axes.
- **lines 1174–1211** — `"M60: Spearman-Brown reproduces the non-octant Table 3
  rows (Layer A)"`, the pattern M61's Layer-A sweep is meant to follow,
  including its discrimination check (the sweep must fail at the wrong
  `item_n`).
- **lines 3–13** — `BC3`, which asserts `expect_identical()` on the octant
  `item_n`, i.e. exactness that must not be weakened.

**Prior review**

- `cairn/reviews/archive/RR09-axes-reliability-strack.md` — the Fable review
  that shaped the original build. Its **§4** holds that refusing unequal
  spacing is "scope-correct, not merely cautious"; its **§7.4** sets the
  NA-with-reason discipline. Both are constraints below, not questions.

## Questions

1. **Is 8.67 reachable?** Under the package's accepted-input contract — a set
   of equally spaced scale angles at any rotation, at least four positions,
   with a per-scale item count — can *any* configuration produce a per-axis
   `item_n` of 8.67 (to within the ±.01 tolerance M61's Layer-A sweep uses)?
   Consider both the single-item case and the general unbalanced case where
   `n_i` varies across scales. Give the reasoning, not just the verdict; if a
   configuration does exist, state it explicitly.

2. **What is SYMLOG's geometry in Strack's analysis?** From the paper alone
   (Table 1's `Scales —`, `Items 26`; Figure 1's type f; Table 3's
   `item_n 8.67`; and anything else in the text), what can be established about
   how SYMLOG's 26 items were weighted? Is `8.67 = 26/3` sound evidence of a
   three-axis weight set, or is a two-dimensional but unequally spaced
   arrangement (or some other account) at least as consistent with the printed
   numbers? Mark clearly which parts of your answer are established by the text
   and which are inference.

3. **Are the three SYMLOG rows a legitimate Layer-A oracle?** M61's AC5 plans
   to sweep all six single-item rows through `axis_reliability_sb()` and
   require agreement within ±.01. Two readings are in tension. In favour: the
   Spearman–Brown expression is a scalar identity in `(item_n, xi1)` and is
   indifferent to how many axes produced that `item_n`, so the three SYMLOG
   rows are three genuine published `(xi1, item_n, Rel)` triples — and the only
   published ones at a fractional `item_n`, which is exactly the arithmetic
   regime nothing else in the suite exercises. Against: they come from an
   instrument that this estimator would refuse as input, so banking them
   alongside reachable rows risks a later reader treating them as a
   configuration the package supports. Which reading should govern, and why?

4. **If they are banked, what must carry the caveat, and in what words?** The
   candidates are the committed source note
   (`cairn/references/strack2013.md`), the test that sweeps them, and the
   estimator's own documentation. Say which of the three need it and propose
   the wording. Note that the repo's tracking rules distinguish a **standing
   fact** about the source (durable) from a **dated observation** about the
   repo's own state (must carry `— observed YYYY-MM-DD` inline); classify each
   claim you propose.

5. **What is the right end-to-end fixture for "fractional `item_n`"?** The
   candidates the implementing session sees are: (a) an odd number of
   single-item positions, giving `item_n = k/2` — `k = 5` → 2.5, `k = 17` →
   8.5; (b) a *mixed* configuration with unequal item counts per scale, which
   gives per-axis `item_n` that are fractional *and* differ between the X and Y
   axes; (c) both. Which discriminates best against the defect class AC4
   exists to catch — a silent rounding, integer coercion, or `expect_identical`
   over-tightening somewhere between `axis_item_n()` and the results frame?
   Name any *other* fixture that would discriminate better.

6. **Propose exact replacement wording for AC4.** It must remain
   script-measurable, must keep the OLS-shadow half of the current criterion
   intact, and must not silently narrow what the criterion demands. Supply the
   full replacement line in the repo's criterion format
   (`- [ ] AC4: …`). If you conclude AC4 should be split into two criteria,
   say so and supply both, noting that the milestone's Coverage section maps
   AC4 → T3, T7 and would need the corresponding update.

7. **Is there a numerical hazard specific to fractional `item_n`?** M60's own
   comment records that non-octant `item_n` sums are inexact (16 scales at
   22.5° measure 32.000000000000000 and 31.999999999999996 on the two axes).
   Does a fractional `item_n` introduce any hazard beyond that — in
   `axis_reliability_sb()`, in `axis_sem()`, or in the results frame — and is
   any existing or planned assertion tighter than the arithmetic can support
   (or, conversely, so loose it would not catch a real defect)? Recommend
   specific tolerances where you flag one.

8. **What else in the M61 plan does this destabilize?** Re-read the eight
   acceptance criteria, the Coverage map, and tasks T1–T9 in light of your
   answers. Name anything that no longer holds, is now unreachable, or is
   quietly assuming SYMLOG is a reachable configuration. In particular assess
   AC5's "all six single-item Strack (2013) Table 3 rows reproduce within ±.01"
   and T8's Layer-B cells.

## Constraints

These are fixed. Flag disagreement with any of them explicitly in your report
rather than silently working around it.

- **D-031** promotes M60 and M61 into v2.0.0 and fixes the accepted-input width
  as *any equally spaced set at any rotation*. The width itself is not
  relitigated here.
- **RR09 §4** holds that refusing unequal spacing (a quasi-circumplex) is
  scope-correct rather than merely cautious. Not relitigated. Note that if your
  answer to Q2 is that SYMLOG is two-dimensional but unequally spaced, that
  makes SYMLOG *refused input*, which is a finding about the oracle, not an
  invitation to reopen the refusal.
- **D-026** keeps blockwise ζ2 (Strack's type d) and FIML-on-items deferred.
  Out of scope.
- **The ζ1 drop rule is settled** by the M61 plan: ζ1 is dropped exactly when no
  same-scale item pair exists anywhere, and a mixed configuration carrying at
  least one multi-item scale still fits ζ1. **M61-D1** settles that the
  Nunnally–Bernstein comparison returns `NA` with a stated reason whenever any
  scale has fewer than two items. Neither is a question here.
- **No new package dependencies.** lavaan and OpenMx are already `Suggests`;
  nothing may be added.
- **Angle convention:** degrees in `[0, 360)` in the user API, with the LM
  position reported as 360 rather than 0; the axes sit at communion 0° and
  agency 90°.
- The estimator's numeric behaviour is validated against **independent
  oracles**, never against its own output. Published-value (Table 3),
  exact-population-matrix, Monte-Carlo, and cross-engine (lavaan vs OpenMx) are
  the four already in use.

## Output format

In `cairn/reviews/RR11-axes-reliability-fractional-item-n.md`: answer each
question by number with your reasoning and evidence; list any additional
findings separately under "Beyond the brief"; end with concrete
recommendations, each marked apply / consider / reject-with-reason. Where
findings bind implementation, also emit a `## Binding criteria` section:
numbered `BC1…`, each a measurable assertion checkable against evidence, with
any numeric projection stating its tolerance. These are ingested VERBATIM into
M61's acceptance criteria and mechanically diffed against this file; departures
are legal only through that milestone's shown "Deviations from RR11" table.
