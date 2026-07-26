# RR11: Fractional `item_n` and the SYMLOG Table 3 rows (M61)

- **Date:** 2026-07-26
- **Brief:** `cairn/reviews/RB11-axes-reliability-fractional-item-n.md`
- **Reviewer:** independent Fable review (this report)
- **Materials actually read:** M61 milestone file; DECISIONS D-026/D-030/D-031;
  `cairn/references/strack2013.md`; the shelf PDF pp. 2–3, 5–9 via
  `pdftotext -layout` (born-digital text layer); `R/axes_reliability.R`
  (weights, `axis_item_n()`, SB/SEm, OLS shadow, refuse contract, results
  frame); `R/axes_reliability_oop.R` (print path);
  `tests/testthat/test-axes-reliability.R` lines 1–20, 1143–1214; RR09 §4, §7.
  All arithmetic below was recomputed in R during this review, not transcribed.

## Answers

### Q1. Is 8.67 reachable?

**On the ζ1-dropped (all-single-item) path: no. On the general unbalanced
path: yes — and it is accepted input already, under the shipped M60
contract.** The two halves:

**Single-item, and balanced generally.** For k equally spaced positions at any
rotation φ, `Σ cos²(φ + 360j/k) = k/2` exactly (the second-harmonic sum
`Σ e^{i(2φ+720j/k)}` vanishes for every k ≥ 3). With one item per position
both axes therefore get `item_n = k/2` — a half-integer. `8.67 ± .01` requires
k ∈ [17.32, 17.36]: no integer. Balanced n items per scale gives `n·k/2`, a
half-integer multiple: `n·k = 17.34` has no integer solution. Unreachable.

**Unbalanced counts at a non-octant rotation.** Per-axis
`item_n_x = Σ nᵢ cos²θᵢ` is continuous in the rotation. Take k = 4 at rotation
φ: positions φ, φ+90, φ+180, φ+270 give
`item_n_x = (n₁+n₃)cos²φ + (n₂+n₄)sin²φ`, which sweeps the whole interval
between the two pair-sums as φ varies. With counts (4, 5, 4, 4) — every scale
≥ 2 items, so this is legal input **today** — and angles
{55, 145, 235, 325} (equally spaced, k = 4 ≥ floor):

```
item_n = (8.671010, 8.328990), sum = 17          # verified in R
|8.671010 − 8.67| = .00101  →  inside the ±.01 window
```

The exact 26/3 value sits at φ = asin(√(2/3)) ≈ 54.7356°. So a configuration
producing 8.67 per axis exists — but only on the ζ1-fitted mixed path, never
where all scales are single-item. **AC4's implicit conjunction — item_n 8.67
*and* the empty same-scale design column — is unreachable in any one
configuration**, which confirms the implementing session's finding. The
reachable 8.67 configuration above is numerology (a rotation tuned to hit a
number), not a SYMLOG analogue, and should not be dressed up as one.

### Q2. SYMLOG's geometry in Strack's analysis

**Established by the text (all page numbers from the shelf PDF's own
pagination):**

1. p. 2: Figure 1 "depicts other types of circumplexes and extensions to
   three-dimensional content models, that is, spheres (e.g., Bales & Cohen,
   1979 …)" — Bales & Cohen 1979 *is* SYMLOG's source (Table 1, type f row).
2. p. 5 (Instruments): "the System for Multi-Level Observation of Groups
   (SYMLOG) instrument realizes a sphere."
3. p. 9 (Discussion): "More than 20% of the item variance can be accounted for
   by the axes of a 'good' IPC (IAL, IAS-R, and IMI; **the SYMLOG for a sphere
   model**)" — this establishes that the *fitted model*, not just the
   instrument's design intent, was spherical.
4. Table 1: type f, Scales `—`, Blocks `—`, Items 26.
5. Table 3: all three SYMLOG rows print item_n 8.67; scale- and
   block-specificity are `—`; components sum to exactly 100.0
   (14.4+27.2+58.4, 11.8+30.3+57.9, 15.2+28.1+56.7 — verified).
6. pp. 5–6: "The Nunnally–Bernstein formula was not applied for analyzing
   instruments with a single item per spatial position (i.e., the COC and
   SYMLOG instrument)" — SYMLOG is single-item-per-position.

**Inference (marked as such):** 26/3 = 8.6667 matches the printed 8.67 to
its two decimals. In a 3-D unit-vector weight set each item's squared
direction cosines sum to 1 across the three axes, so total item_n across axes
= 26; the sphere's no-preferred-rotation symmetry splits it 26/3 per axis.
**External corroboration (outside the paper, flagged as such):** Bales's
standard 26-vector SYMLOG space (6 pole, 12 edge, 8 corner directions) gives
per-axis `Σcos² = 2·1 + 8·(1/2) + 8·(1/3) = 26/3` *exactly* — recomputed in R
during this review.

**The two-dimensional-but-unequally-spaced account is not "at least as
consistent"; it is contradicted.** (i) p. 5: "All instruments are designed to
be perfectly circumplex structured (equal spacing of scales or items)" and
quasi-circumplex instruments "were not included." (ii) A 2-D account must put
the two axes' item_n at 8.67 and 26 − 8.67 = 17.33, yet Table 3 prints one
item_n per row and the model forces equal axis variances — the paper's own
machinery has no place for that asymmetry. (iii) 2-D equal spacing with 26
single items would print 13, not 8.67. The sphere reading is the only account
consistent with every printed number and every quoted sentence.

### Q3. Are the three SYMLOG rows a legitimate Layer-A oracle?

**Yes — as a formula-layer oracle only. The "in favour" reading governs, but
re-scoped, and the tension in the brief is smaller than it looks** because of
a fact about the existing suite: the M60 Layer-A sweep
(`test-axes-reliability.R:1174–1211`) already calls `axis_reliability_sb()`
**directly** with printed `(xi1, item_n)` pairs — Layer A has never been an
end-to-end path through `axes_reliability()`. The SB expression is a scalar
identity in `(item_n, xi1)`, indifferent to how many axes produced the
item_n; the three SYMLOG rows are three genuine published triples — verified:
SB(.272, 8.67) = .7641, SB(.303, 8.67) = .7903, SB(.281, 8.67) = .7721
against printed .76/.79/.77, all within ±.005 (and within ±.0002 of the same
sweep at 26/3, so the printed two-decimal value is a fine sweep input). They
are the **only** published anchors in the fractional-item_n regime, exactly
the arithmetic nothing else in the suite exercises. Discarding the only
published values in the regime under test would violate the
independent-oracle doctrine's own preference for published values over
self-constructed fixtures.

The "against" risk — a later reader treating SYMLOG as supported input — is
real but is a labelling problem, and the repo already has the precedent and
the pattern for it: the type-c MEIL row is banked "as a reliability anchor
only, never as a component-sum guard." Bank the SYMLOG rows the same way:
**scalar-identity anchors only, never an `axes_reliability()` fixture**, with
the caveats of Q4. Per the brief's constraint, this is a finding about the
oracle, not a reopening of the unequal-spacing refusal (RR09 §4 stands; the
sphere is refused for a different reason anyway — it is not two-dimensional).

### Q4. What carries the caveat, and in what words

All three candidates need it, in different registers:

**(1) `cairn/references/strack2013.md` — yes, and it is the primary home.**
Proposed block, to be added where the type-e/f rows are banked:

> **Type-f rows (SYMLOG) are sphere-model values, not a two-axis
> configuration.** Strack et al. fit SYMLOG as a *sphere* — a
> three-dimensional extension of the circumplex: Figure 1's 3-D types are
> "spheres (e.g., Bales & Cohen, 1979)" (p. 2), the instrument "realizes a
> sphere" (p. 5), and the fitted model is named "the SYMLOG for a sphere
> model" (p. 9). Table 1 lists 26 items and no scales; each item's squared
> direction cosines sum to 1 over *three* orthogonal axes, so per-axis
> item_n = 26/3 = 8.667, printed 8.67 (Table 3 col 10). In any two-axis
> equally spaced single-item set item_n = k/2, a half-integer, so 8.67 is
> unreachable under `axes_reliability()`'s accepted input (D-031 width;
> RR09 §4) — the three rows are formula-layer Spearman–Brown anchors only,
> never an end-to-end fixture — observed 2026-07-26.

Classification: the sphere geometry, the 26/3 identity, and the printed
values are **standing facts about the source** (durable, page-cited, no
stamp). The final sentence — what the *package* accepts and how the rows may
be used in *this repo's* suite — is a **dated observation about repo state**
and carries the `— observed 2026-07-26` stamp (anchoring D-031 gives it a
durable citation even after the stamp ages).

**(2) The sweep test — yes**, as a comment (code comments version with the
code; no stamp discipline applies). Proposed wording above the SYMLOG block:

```r
# Type f -- SYMLOG. NOT a package-supported configuration: Strack fits SYMLOG
# as a SPHERE (three orthogonal axes; pp. 2, 5, 9), and its item_n
# 8.67 = 26/3 is unreachable in any two-axis equally spaced set (single-item
# sets give k/2). These rows are the paper's only published fractional-item_n
# triples, so they anchor the scalar identity axis_reliability_sb() -- and
# only that. Never promote them to an end-to-end axes_reliability() fixture.
```

**(3) The estimator's roxygen — yes, but as a user-facing scope sentence, not
an oracle caveat** (users do not read oracle notes; what they need is scope).
Durable, no stamp. Proposed sentence for the Details section, where M61
documents the single-item extension:

> The model is two-dimensional. Instruments whose items span three dimensions
> — spherical designs such as SYMLOG (Strack et al.'s type f) — are out of
> scope, even though Strack et al. (2013) analyze one; their Table 3 SYMLOG
> rows arise from a three-axis sphere model, not from any configuration this
> function accepts.

### Q5. The right end-to-end fixture for fractional `item_n`

**(c) both — the two shapes catch disjoint defect halves — plus frame-level
type/inequality assertions that discriminate harder than either shape
alone.**

- (a) odd single-item k is the **only** shape that puts a fractional item_n
  on the ζ1-dropped path — the path M61 actually rebuilds (two-column OLS
  shadow, variable-length components frame, generalized `SS1` extraction). A
  mixed fixture never reaches that code. Use k = 5 → 2.5/2.5 (cheap; k = 17
  works but buys nothing extra). Note the half-integer is *not* reliably
  exact: measured this review, k = 5 at rotation 13.7° gives
  2.4999999999999996 — so (a) also discriminates against
  `expect_identical` over-tightening, despite 2.5 being representable.
- (b) mixed unequal counts at a non-octant rotation is the **only** shape
  whose per-axis item_n are fractional *and unequal*, so it alone catches
  x/y conflation (recycling one axis's item_n into both results rows) — a
  defect class (a) is structurally blind to, since single-item sets always
  give equal axes. Concrete fixture: angles {22.5, 112.5, 202.5, 292.5},
  counts (2, 3, 2, 2) → item_n (4.14645, 4.85355), fractional-part .146/.854,
  large enough that any `round()`/`as.integer()` mangling fails a 1e-8
  comparison by seven orders of magnitude. Do **not** use the
  8.67-hitting configuration from Q1 — it invites exactly the
  SYMLOG-is-reachable misreading this review exists to prevent.

**The better-discriminating additions** (name-any-other): assert directly on
the results frame, against an *independently coded* analytic expression
(`sum(n * cos(th)^2)` written out in the test, not a call back into
`axis_item_n()`): (i) `expect_equal(..., tolerance = 1e-8)`, (ii)
`expect_true(is.double(res$results$item_n))` — catches integer coercion that
rounding-tolerant comparisons at half-integers would miss, (iii) for (b),
assert the two rows differ. Silent rounding, coercion, and over-tightening
are each caught by at least two of these; no single fixture without the frame
assertions catches all three.

### Q6. Replacement wording for AC4

**Split it.** The two halves have different fixtures, different code paths,
and after this review different rationales; fusing them is what let "8.67"
contaminate the OLS-shadow half. Full replacement lines:

```
- [ ] AC4a: fractional item_n works end to end on both reachable shapes:
      (i) an odd all-single-item configuration (k = 5 → item_n 2.5/2.5) on
      the ζ1-dropped path, and (ii) a mixed unequal-count configuration at a
      non-octant rotation (e.g. angles 22.5/112.5/202.5/292.5, counts
      2/3/2/2 → item_n 4.14645/4.85355) on the ζ1-fitted path; in both, the
      results-frame item_n equals an independently coded analytic per-axis
      Σ nᵢwᵢ² within 1e-8 and is stored as double, and in (ii) the two axes
      differ. SYMLOG's printed 8.67 = 26/3 is a three-axis (sphere-model)
      value unreachable under the input contract and is asserted only at
      the formula layer (AC5).
- [ ] AC4b: the OLS shadow returns a two-component seed instead of erroring
      when the same-scale design column is empty.
```

Coverage update: `AC4 → T3, T7` becomes **`AC4a → T7; AC4b → T3`**. This does
not narrow the criterion: the OLS-shadow half is verbatim-preserved, and the
fractional-item_n half is *widened* from one unreachable number to two
reachable shapes plus type/inequality assertions.

### Q7. Numerical hazards specific to fractional `item_n`

**No new hazard in the arithmetic itself; two calibration findings on
assertions.**

- `axis_reliability_sb()` is a smooth rational function of item_n with
  `∂Rel/∂item_n = ξ1(1−ξ1)/(1+(item_n−1)ξ1)² < ξ1`, so the ~1e-14 float noise
  M60 documented propagates to < 1e-14 in reliability. Nothing about a
  fractional value changes this.
- `axis_sem()` takes `sqrt(1 − rel)`; under the accepted contract per-axis
  `item_n = Σ nᵢcos²θᵢ ≥ Σ cos²θᵢ = k/2 ≥ 2` (nᵢ ≥ 1, k ≥ 4), and
  SB < 1 strictly for ξ1 ∈ (0, 1), so the fractional regime sits nowhere near
  a domain edge. (A ξ1 ≥ 1 estimate would make rel ≥ 1 and SEm NaN, but that
  is not fractional-specific — see Beyond the brief.)
- The results frame (`:828–837`) stores the raw double; the print method
  (`axes_reliability_oop.R:77`) passes item_n through unformatted — no
  rounding anywhere on the path. Verified by reading, to be pinned by AC4a.
- **Too tight:** any `expect_identical()` on a non-octant item_n — even the
  "exact-looking" half-integers come out as 2.4999999999999996 (measured,
  k = 5). Use `expect_equal(tolerance = 1e-8)`, M60's discrimination-derived
  rationale (smallest meaningful error is one item; 1e-8 fences that at
  1e8× while sitting ~6 orders above the float noise). BC3-the-test's octant
  `expect_identical` stays — octant exactness is a snap_trig guarantee and
  must not be weakened.
- **Too loose, in one specific sense:** AC5's ±.01 reliability sweep cannot
  discriminate item_n 8.67 from its nearest reachable neighbours at SYMLOG's
  ξ1 values — |SB(ξ1, 8.5) − SB(ξ1, 26/3)| ≈ .0032–.0036 for all three rows
  (measured). ±.01 is still right for the sweep (printed values carry two
  decimals; input rounding of %axes contributes ≲ .0005), but its
  discrimination check must use a *distant* wrong item_n — at 32 the three
  rows miss by .143–.159 — and nobody may cite AC5 as having "verified 8.67".
  (At item_n 8 the misses are .0107/.0133/.0122 — technically > .01 but with
  no margin; do not use 8 as the discriminator.)
- If anyone is tempted to bank SYMLOG **SEm** cross-checks (the BC2 pattern):
  don't. The Self row gives `sqrt(32.3)·sqrt(1 − .76) = 2.784` vs printed
  2.80 (and 2.761 using unrounded rel .7641) — a miss of up to ~.04, beyond
  the existing ±.02 input-rounding slack. The Other/Meta rows pass, but a
  cross-check that needs per-row exemptions is not worth banking; COC's SEm
  row (already banked, exact) covers type e.

### Q8. What else in the M61 plan this destabilizes

- **Scope bullet** "Fractional item_n end to end (SYMLOG's 8.67)" — must be
  reworded, e.g. "Fractional item_n end to end (odd single-item k/2; mixed
  unequal counts — SYMLOG's 8.67 is a sphere-model value, formula layer
  only)."
- **AC4** — replaced per Q6.
- **AC5** — the number pairs are all correct as printed and the sweep holds
  (verified: all six rows reproduce within ±.005), but the criterion should
  name the layer so no one implements it end to end. Proposed rewording:

  ```
  - [ ] AC5: Layer A — all six single-item Strack (2013) Table 3 rows
        reproduce within ±.01 through direct axis_reliability_sb() calls
        (the formula layer, M60's pattern): COC %axes 2.8/3.2/1.9 at
        item_n 8 → .19/.21/.13, SYMLOG 27.2/30.3/28.1 at item_n 8.67 →
        .76/.79/.77 (p. 7); each row's components sum to 100.0 (±.05); the
        sweep fails at a distant wrong item_n (32). The SYMLOG rows are
        sphere-model anchors banked with the not-package-input caveat (AC7).
  ```
- **T7** — "the ±.01 sweep test, which is also the fractional-item_n fence"
  is no longer true: the sweep is formula-layer; the end-to-end fence is
  AC4a's fixtures. T7 should read "…add the ±.01 formula-layer sweep plus the
  AC4a end-to-end fractional-item_n fence (odd-k single-item and mixed
  unequal-count fixtures)."
- **T8 Layer B** — sound as planned (nothing in it assumes SYMLOG is
  reachable), but as specified its single-item cells would naturally use an
  even k (COC-like, integer item_n), leaving the ζ1-dropped fractional case
  end-to-end-only. Recommend one population-matrix cell at **k = 5**
  (item_n 2.5) so the fractional ζ1-dropped regime is also oracle-backed.
- **AC1–AC3, AC6, AC8, M61-D1** — unaffected. AC7 is affected only in that
  the banking it requires must carry the Q4 caveat.

## Beyond the brief

1. **Two comment mislabels attribute SYMLOG's fractional item_n to
   two-axis unbalance.** `R/axes_reliability.R:73–75` ("Computed per axis so
   an unbalanced set degrades gracefully (Table 3 col. 10 is per axis, and
   fractional for SYMLOG at 8.67)") and
   `tests/testthat/test-axes-reliability.R:1166–1168` ("a fractional value
   (the SYMLOG shape, Table 3 col. 10 = 8.67)" over a 2-D unbalanced
   4-scale set). Both imply SYMLOG is an unbalanced two-axis instrument; it
   is a balanced three-axis one. Fix the wording while M61 is in the file.
   (RR09 §4 contains the same gloss but is an archived review — do not edit
   archives.)
2. **`axis_sem()` can return NaN on a ξ1 ≥ 1 fit.** The boundary guard
   (`:751`) catches ξ1 ≤ 0 and negative variances but not rel ≥ 1
   (`sqrt(1 − rel)` → NaN). On the correlation metric ξ1 ≥ 1 means a
   grossly misspecified fit, so this is remote — but it is the same
   "never NaN" doctrine as M61-D1. Worth a one-line guard or a follow-up
   candidate, not an M61 requirement.
3. **Print cosmetics:** `print.circumplex_axes_reliability()` shows item_n
   unformatted while its neighbours go through `axes_fmt()`; a mixed fixture
   will print `4.146447` beside 3-digit columns. Cosmetic only; harmless to
   leave.
4. Table 3's caption says "28 sub-samples" while the text says 29 — a source
   quirk, no action for this repo.

## Recommendations

1. **Apply** — replace AC4 with the AC4a/AC4b split of Q6 and update the
   Coverage map (AC4a → T7; AC4b → T3).
2. **Apply** — bank all six type-e/f rows in `strack2013.md` with the Q4
   block: sphere standing facts (pp. 2, 5, 9), the 26/3 identity, and the
   dated repo-contract observation; keep the SYMLOG rows formula-layer only.
3. **Apply** — the sweep-test comment and the roxygen two-dimensional-scope
   sentence of Q4.
4. **Apply** — reword AC5 per Q8 (formula layer named, 100.0 sum guard,
   distant-item_n discrimination at 32) and reword the Scope bullet and T7.
5. **Apply** — AC4a's frame assertions: tolerance 1e-8 against an
   independently coded analytic sum, `is.double`, axes-differ on the mixed
   fixture; no `expect_identical` on non-octant item_n.
6. **Consider** — a T8 population-matrix cell at k = 5 (fractional item_n on
   the ζ1-dropped path, oracle-backed).
7. **Consider** — fixing the two SYMLOG-shape comment mislabels (Beyond 1)
   and the ξ1 ≥ 1 NaN guard (Beyond 2) inside M61's touched files.
8. **Reject** — banking SYMLOG SEm cross-checks: the Self row misses by up to
   ~.04 depending on rounding path (Q7); COC already covers type e exactly.
9. **Reject** — any end-to-end fixture tuned to hit 8.67 (the Q1
   construction): legal input but numerology, and it manufactures the exact
   misreading the caveats exist to prevent.

## Binding criteria

- BC1: M61's AC4 is replaced verbatim by the two criteria in RR11 Q6 (AC4a,
  AC4b), and the Coverage map reads `AC4a → T7; AC4b → T3`.
- BC2: no test anywhere in M61 calls `axes_reliability()` (either input path)
  with a configuration presented as SYMLOG or asserted to yield per-axis
  item_n 8.67; the three SYMLOG Table 3 rows enter the suite only through
  direct `axis_reliability_sb()` calls.
- BC3: the Layer-A sweep asserts, each within ±.01: SB(.028, 8) → .19,
  SB(.032, 8) → .21, SB(.019, 8) → .13, SB(.272, 8.67) → .76,
  SB(.303, 8.67) → .79, SB(.281, 8.67) → .77; asserts each of the six rows'
  printed components sum to 100.0 within ±.05; and asserts the three SYMLOG
  rows each miss their printed reliability by MORE than .01 at item_n 32.
- BC4: the end-to-end fixtures of AC4a compare the results-frame `item_n`
  against an analytic per-axis sum coded independently in the test (not via
  `axis_item_n()`), with `expect_equal(tolerance = 1e-8)`, assert
  `is.double()` on the frame column, and on the mixed fixture assert the two
  axes' item_n differ; no new `expect_identical()` on any non-octant item_n,
  and the existing octant-exactness assertions are not weakened.
- BC5: `cairn/references/strack2013.md` banks the six type-e/f rows carrying
  (i) the sphere standing fact quoting pp. 2, 5, and 9, (ii) the
  `8.67 = 26/3` identity, and (iii) a dated (`— observed YYYY-MM-DD`)
  observation that the configuration is unreachable under the accepted input
  contract (citing D-031); the sweep test carries the never-promote comment;
  the `axes_reliability()` roxygen states the two-dimensional scope naming
  spherical designs (SYMLOG) as out of scope.
