# M7 T3 — second independent human re-read checklist

**Purpose.** The Grassi et al. (2010) and Zimmermann & Wright (2017)
transcriptions were each extracted through two *mechanical* channels (a
rendered-page visual read and a `pdftotext` text-layer extraction, diffed
against each other). Both records mark the remaining step as
`second independent human re-read: pending (Jeff)`. This checklist is that
step's worksheet. It gates the v2.0.0 submission (M7 AC3).

**How to use it.** Open the primary source at the anchor named in each row and
read the published value off the page, *then* compare it to the row. Read
source-first — comparing in the other direction invites confirmation of what is
already written. Tick the box when the two agree; note any discrepancy inline
and resolve it before T4.

That recipe fits section A, where nearly every row is a printed number. **It
does not fit all of section B**, whose rows come in three kinds — printed
values, asserted findings, and our own paraphrases, the last of which no page
can settle. Section B rows are tagged accordingly; read
*"How to check section B"* before starting that half.

This checklist was assembled from the repo, so it inherits any error the repo
already has. It tells you **where to look**, never what you should find.

---

> **Section A completed 2026-07-19 by Jeff**, against the primary source.
> **Every transcribed value confirmed — no fixture changed.** Three
> corrections, all to records *about* the values (a page anchor, a column
> label, a comment's wording), applied the same day; see the A2–A6 notes.
> **Section B started 2026-07-19, partly complete.** Confirmed: B2, B3's
> Eq. A7 (including the leading ½ — closing the record's one inferential
> resolution), B4, B5. Two worksheet defects found and fixed in B1a and B3,
> both the same one: presenting *our computed* values as though printed
> (hence the new **[DERIVED]** tag). **Still open — AC3 is not met until
> these are:** B0's fidelity judgments, the remaining B1a/B3 value rows,
> and two decisions for Jeff — the Study 2 threshold-source conflict (B1)
> and whether the vignette's table header should say *bootstrap* CI.

## A. Grassi, Luccio & Di Blas (2010)

*CircE: An R implementation of Browne's circular stochastic process model.*
**Behavior Research Methods, 42**(1), 55–73. doi:10.3758/BRM.42.1.55

Repo locations: `tests/testthat/helper-cpm-oracles.R` (fixtures),
`tests/testthat/test-cpm_oracles.R` (provenance header + assertions).

### A1. Input correlation matrix — Table 1 (p. 58), N = 175

`helper-cpm-oracles.R:12-30`. Seven vocational-interest scales (Health,
Science, Technology, Trades, BusinessOperations, BusinessContact, Social).
Lower triangle, read row-wise:

- [ ] Row 2 (Science): `.654`
- [ ] Row 3 (Technology): `.453 .644`
- [ ] Row 4 (Trades): `.251 .440 .757`
- [ ] Row 5 (BusinessOperations): `.122 .158 .551 .493`
- [ ] Row 6 (BusinessContact): `.218 .210 .570 .463 .754`
- [ ] Row 7 (Social): `.496 .264 .366 .202 .471 .650`
- [ ] N = 175
- [ ] Scale names and their **order** match Table 1

> Cross-check available: the paper states this reanalyzes Browne (1992)
> Table 2. The matrix is also printed in the paper's Listing 1.

### A2. Appendix A full-precision m = 1 output (pp. 70–72)

`helper-cpm-oracles.R:33-55`. These are the frozen published-program values
the free-scaling oracle asserts against.

- [ ] Angles θ (deg): `0, 305.35328, 247.82980, 237.38218, 168.30615, 149.83787, 91.25973`
- [ ] Angle SEs: `0, 9.01111, 7.35838, 9.44904, 9.08050, 7.95016, 8.72929`
- [ ] v (communality parameter): `0.15438, 0.51654, 0.03945, 0.63153, 0.54550, 0.13449, 0.44771`
- [ ] v SEs: `0.13759, 0.12755, 0.04238, 0.13854, 0.12125, 0.05959, 0.13865`
- [ ] ζ: `0.91358, 0.81222, 1.00102, 0.79058, 0.79269, 0.92497, 0.84376`
- [ ] Communality indices: `.93, .81, .98, .78, .80, .94, .83`
- [ ] Communality 95% CIs: `(.73,.99) (.74,.87) (.87,1) (.71,.84) (.74,.86) (.87,.97) (.74,.90)`
- [ ] Variance ratios (reproduced/input): `0.963, 1.000, 1.042, 1.020, 0.971, 0.971, 1.031`
- [ ] β (correlation-function weights): `0.6378, 0.3622`
- [ ] MCSC ρ(180°): `0.276`
- [ ] F̂ (iteration trace, "final value"): `0.089815`

> **Re-read result (2026-07-19).** All values confirmed. **Order correction:**
> Appendix A prints these blocks in its own variable order — Health, Social,
> BusinessContact, BusinessOperations, Trades, Technology, Science (ascending in
> its mirrored angle) — not Table 1's. The rows above (and the fixtures) are in
> **Table-1 order**; re-map by scale before comparing. Mapped that way the
> communality indices and all seven CIs agree exactly.
>
> **Direction caution.** The angle vector is the Appendix A direction, which is
> the *mirror* of the Table 2 model-1a start values (`th_start = 0, 55, 112,
> 123, 192, 210, 269` at `helper-cpm-oracles.R:29`). The paper prints both and
> labels one "360 − ang. pos." Confirm you are reading the Appendix A column,
> not Table 2, for this block — and confirm the start values separately.

### A3. Fit measures — Appendix A (pp. 70–71)

> **Re-read result (2026-07-19).** Values confirmed, **anchor corrected**: these
> unconstrained m = 1 fit measures come from Appendix A (pp. 70–71), not Table 3
> (p. 60). Table 3 is the anchor for the *constrained*-model F values (A4).
> The provenance header in `test-cpm_oracles.R` now splits the two.

`helper-cpm-oracles.R:44-48`.

- [ ] T (χ²): `15.63`, df `7`, p `0.029`
- [ ] F₀: `0.049`, 90% CI `(0.005, 0.139)`
- [ ] RMSEA: `0.084`, 90% CI `(0.026, 0.141)`
- [ ] Null χ²: `747.663`, null df `21`
- [ ] TLI: `0.964` · CFI: `0.988` · SRMR: `0.04`

### A4. Constrained-model rows — Table 2 / Table 3 (p. 60)

Asserted inline in `test-cpm_oracles.R`.

> **Re-read result (2026-07-19).** Values confirmed, **label corrected**: the
> table lists `.87` as ρ̂₁ (the communality index), not ζ. The assertion is
> still right — that column *is* our `Zeta` per design sec. 6.5 — but the code
> comments said "zeta" and now say so. Applied to the m = 2 rows on the same
> reading.

- [ ] Model 2b (equal communality): β `(.628, .372)`, ρ̂₁ `.87`, F̂ `.299`, ρ(180) `.26` — `test-cpm_oracles.R:196-201`
- [ ] Model 1a m = 2: β `(.608, .355, .038)`; F̂ `.067` (Table 3) — `test-cpm_oracles.R:229-239`
- [ ] The m = 3 statement that β₃ attains "the lower bound of zero" (p. 59) — `test-cpm_oracles.R:251`
- [ ] Model 3c (equal spacing, free scaling) — `test-cpm_oracles.R:347, 564-565`

### A5. Quoted textual claims

- [ ] p. 59: CircE's m = 1..3 results "coincide precisely with the ones obtained by CIRCUM" — this is what makes the fixtures transitively cover Browne's own program (`test-cpm_oracles.R:18-20`)
- [ ] p. 57, **as published**: "The nonsymmetric confidence intervals for the communality index estimates, ρ(x_i, c_i) (Browne, 1992, Eq. 4), are obtained from symmetric confidence intervals on ln v_ii (Browne, 1982, pp. 95–96)." The checklist's earlier paraphrase ("communality CIs are symmetric Wald intervals on ln(v)") put the symmetry on the wrong quantity; the comment at `test-cpm_oracles.R:122-127` has been reworded to the published statement. The decoding arithmetic was already correct.
- [ ] Appendix A prints variance ratios spanning `.963–1.042` (the free-scaling model difference)

### A6. Secondary fixture — Listing 7–8 (pp. 67–68)

`helper-cpm-oracles.R:59-72`. Verbal-ability matrix (Guttman, 1954, p. 282;
also Browne, 1992, p. 470), used for input-refusal behavior only, so an error
here cannot move a numeric result — verify last, or skip.

> **Re-read result (2026-07-19).** Correlations, N, and all six scale names
> confirmed. *(An earlier note here reported the sixth scale as
> "ForeignLanguage"; Jeff retracted that the same day as his own slip —
> the source reads **ForeignLiterature**, which is what the fixture has
> always said. No change was kept; corrected 2026-07-19.)*

- [ ] Six scale names — Spelling, Punctuation, Grammar, Vocabulary, Literature, ForeignLiterature — and lower triangle: `.621 / .564 .742 / .476 .503 .577 / .394 .461 .472 .688 / .389 .411 .429 .548 .639`
- [ ] N = 1046

---

## B. Zimmermann & Wright (2017)

*Beyond description in interpersonal construct validation: Methodological
advances in the circumplex Structural Summary Approach.*
**Assessment, 24**(1), 3–23. doi:10.1177/1073191115621795

Repo locations: `devel/m4-zw-transcription.md` (the full 211-line record,
already page-anchored), `vignettes/evaluating-circumplex-structure.Rmd`
(the user-facing claims).

**The full record is the authority for this half** — it is structured for
exactly this pass and every entry already carries its page anchor. Work
through it directly. The rows below are the subset that reaches **shipped,
user-facing output**, and so are the ones where an error would do the most
damage.

## How to check section B (read this first)

Section A was uniform: nearly every row was "read this number off this table."
Section B is not, and that is why it reads as harder to verify. Its rows are
**three different kinds of claim**, and each needs a different move. Every row
below is now tagged with which one it is.

| Tag | What it is | What settles it |
|---|---|---|
| **[VALUE]** | A number or formula printed in the paper | Read it off the named table/equation. Same as section A. |
| **[FINDING]** | A threshold the authors *assert in a sentence* | Find the sentence — page and section given below — and check the vignette says the same thing. Not in any table. |
| **[DERIVED]** | A number *we* computed; the paper prints a rounder one, or none | Don't hunt for it on the page. Check the arithmetic and check that we didn't present it as published. Added 2026-07-19 after two rows did exactly that. |
| **[FIDELITY]** | *Our* paraphrase of what they found | **No page settles this.** The paper never says "essentially unbiased"; we do. The question is whether our wording is faithful and doesn't overclaim — a judgment, not a lookup. Collected separately in **B0**, with the published numbers beside each. |

If a row felt unverifiable, it was almost certainly a [FIDELITY] row sitting in
a checklist of [VALUE] rows. Those are now pulled out into B0, where the
question asked matches the work.

> **Locator pass, 2026-07-19.** Each [FINDING] and [VALUE] row below now names
> the printed page its claim actually sits on, confirmed page-by-page against
> the shelf copy (`cairn/references/sources/zimmermann2017Description.pdf`;
> PDF page numbering and printed page numbering coincide, so a page anchor is
> unambiguous). **This confirms the anchors, not the values.** It used the same
> extraction family as the original channel 2, so it is not an independent
> read of any number — it only means that when a row says p. 10, p. 10 is
> where you should be looking.

> **What section A suggests to watch for.** Every one of its findings was an
> error in a *record about* a value — a page anchor, a column label, a
> variable order, a comment's wording — and **not one transcribed number was
> wrong**. Section B's analogue is that same label/anchor layer, and B1 below
> now carries a live instance of it.

### B0. [FIDELITY] The vignette's characterizations — judgment, not lookup

These are the accuracy table's "Point estimate" column
(`vignettes/evaluating-circumplex-structure.Rmd:190-199`). Each is our
compression of a paragraph of theirs. Don't hunt for a matching sentence;
there isn't one. Ask instead: **does this overclaim, and would a user acting
on it be misled?** The published basis is given so the judgment is informed.

- [ ] "essentially unbiased" (elevation, X, Y) — their basis: average bias −.0007, most extreme −.013 (p. 6)
- [ ] "**biased upward**, strongly so when population amplitude is small" (amplitude) — their basis: bias .0002–.136, average .021; relative bias average 15.5%, range 0–135.8%; direction consistently positive (p. 6)
- [ ] "unbiased but imprecise at low amplitude" (displacement) — their basis: bias average −.03°, range −2.38° to 3.03° (p. 6); SE grows as amplitude shrinks (p. 8)
- [ ] "biased downward … unsuited near 1" (fit) — their basis: average −.081, range −.382 to .027 (p. 6); coverage exactly 0 at population R² = 1, inaccurate at R² ≥ .9 (p. 10). Note "unsuited" is *their* word about the method for R² CIs (p. 11); "near 1" is ours.
- [ ] Table header "95% CI accurate when…" — accuracy always means the Bradley band, never statistical significance. Confirm no cell reads as a significance claim.

### B1. [FINDING] The sample-size thresholds the vignette's table prints

`vignettes/evaluating-circumplex-structure.Rmd:190-199`. This column is read
by users as guidance about their own sample sizes, so it is the highest-stakes
block in section B. **All four thresholds live in the Study 2 Results section,
p. 10** — not in a table. Read that section top to bottom once; the four
sentences appear in this order.

> **Scope, per Jeff 2026-07-19 — say this on every row below.** These
> thresholds are about **the empirical coverage accuracy of 95% percentile
> bootstrap CIs**, and nothing else. They are not statements about the point
> estimates, not about CIs in general, and not about statistical power or
> significance. "Accurate at n ≥ 50" means: at that n, the bootstrap
> interval's coverage lands inside the Bradley band. A reader who takes these
> as "the estimate is trustworthy at n ≥ 50" has been misled, and the whole
> column is one paraphrase away from reading that way — which is why B0
> exists.
>
> Check on the vignette side: the prose at `:184-188` does say "95% percentile
> bootstrap intervals", but the table header itself reads only
> `95% CI accurate when…`. Decide whether the header should say **bootstrap
> CI**, since tables get read without their preamble.

- [ ] Elevation / affiliation / dominance accurate at **n ≥ 50** — end of the elevation paragraph, p. 10
- [ ] Amplitude accurate at **n ≥ 75** (general factor) and **n ≥ 150** (no general factor) — end of the amplitude paragraph, p. 10
- [ ] Displacement accurate at **n ≥ 100** (general factor) and **> 200** (no general factor) — end of the displacement paragraph, p. 10. Note the asymmetry is theirs: "at least 100" but "greater than 200", which is why the vignette prints `n ≥ 100` and `n > 200`. Confirm we kept that distinction rather than tidying it.
- [ ] Fit: inaccurate at population R² ≥ .9, coverage 0 at R² = 1 — p. 10, last paragraph, continuing onto p. 11
- [ ] The band is Bradley's (1978) liberal band, 92.5%–97.5% (p. 10; vignette `:187-188`)
- [ ] The amplitude ≥ .10 precondition — Study 2 Discussion, p. 10. The vignette attaches it to the amplitude and displacement rows only; confirm that is where they scope it.

> ⚠ **The paper states these thresholds twice, and the two statements do not
> agree.** The Study 2 **Results** (p. 10) gives amplitude 75/150 and
> displacement 100/200, as above. The Study 2 **Discussion** on the same page
> summarizes amplitude *and* displacement together as 100/200 — a rounder,
> more conservative claim that does not reproduce the 75/150 figures.
>
> The vignette follows the **Results**.
>
> **✅ Figure 5 resolves this in the Results' favour (checked 2026-07-19).**
> Read each panel as "the smallest n at which *every* amplitude curve,
> including the worst case A = .10, sits below the 2.5% dotted line" — the
> operative reading, and the one that matches the A ≥ .10 precondition:
>
> | Panel | without GF | with GF |
> |---|---|---|
> | **B — amplitude** | A = .10 curve ≈ 4.2% at n = 100, ≈ 2.45% at n = 150 → first below the line at **150** | ≈ 3.5% at n = 50, ≈ 2.1% at n = 75 → first below at **75** |
> | **C — displacement** | ≈ 4.1% at n = 150 and **still ≈ 2.65% at n = 200** → needs **more than 200** | ≈ 4.0% at n = 75, ≈ 2.55% at n = 100 — essentially *on* the line |
>
> Two things follow. **(1) The 75/150 amplitude thresholds are what the figure
> shows**; they are not a loose reading of the Results. **(2) The Discussion's
> 100/200 is Panel C's displacement thresholds applied to both parameters** —
> a conservative simplification that takes the worse of the two, not an
> independent measurement that contradicts Panel B.
>
> The figure also explains two bits of the authors' careful wording: why
> displacement without GF is "greater than 200" rather than "at least 200"
> (at n = 200 the A = .10 curve has not yet crossed), and why the displacement
> sentence hedges with "close to or dropped below" (with GF at n = 100 the
> curve is sitting on the line, not under it).
>
> **Recommendation: keep 75/150.** The vignette is reporting the
> parameter-specific finding the figure supports, and switching to 100/200
> would import the Discussion's collapse of two parameters into one number.
>
> ⚠ **Precision caveat.** These are values read off a rendered figure, not
> published numbers — the per-condition deviances were never published (the
> record's no-supplement finding), so Figure 5 is the finest evidence that
> exists. The clear calls (amplitude 150 without GF, 75 with GF; displacement
> still above at 200) are well clear of eyeball error; the with-GF displacement
> point at n = 100 is genuinely on the line and should not be read as precise.

### B1a. [VALUE] The three "implications" bullets the vignette prints (`:203-222`)

Prose, not a table, so it escaped the first cut — but every number in it is
transcribed and shipped. All are printed values; each page below was confirmed
in the locator pass.

- [ ] Relative amplitude bias averaged `15.5%`, reached `135.8%` — **p. 6** (the record said pp. 6–7; both numbers are on p. 6)
- [ ] At n = 50, no general factor, population amplitude 0: expected sample amplitude `.153` — **p. 6**. The vignette rounds this to "about .15" (`:205`); confirm you're comfortable with the rounding, since .15 is also the "marked amplitude" figure the same bullet argues against.
- [ ] SE of displacement ≈ `50°` at n = 100 for a weakly differentiated profile — **p. 8**
- [ ] Eq. 3 constants `2.95` and `n^(−0.587)` — **p. 12**
- [ ] The vignette's f_a list (`:219`) — `.55` IIP-C, `.63` IIP-SC, `.85` IAS — **no single page prints all three**, which is why this row previously read as unverifiable. They are: IIP-C `.545` on **p. 9** (and again on p. 12), IAS `.845` on **p. 9**, IIP-SC `.625` on **p. 14**. The two-decimal forms are the vignette's own rounding; the paper prints three decimals in all three places.
- [ ] At n = 100 with an IIP-C-like instrument the frontier is `.11`, and at n = 1,000 it is `.03` — **p. 12**, printed to two decimals in the text

> **Corrected 2026-07-19** (Jeff, on the page): the earlier version of the
> f_a row implied the unrounded values were findable near Eq. 3. They are
> not — p. 12 prints only `f_a = .545`. Each value now carries its own page.

> ⚠ **`15.5%` occurs twice in the paper for two different quantities** —
> Study 1's average relative amplitude bias (**p. 6**) and Study 4's mean
> deviance when the second profile's affiliation is 0 (**p. 13**). Both
> occurrences were confirmed independently in the original two channels, and
> the locator pass re-confirmed both pages. It is a genuine coincidence, not a
> paste error: do not "fix" one into the other. The vignette means the p. 6
> sense.
>
> A third near-miss to be aware of while reading: `.153` appears on **p. 6** as
> the expected sample amplitude above, and again on **p. 15** as a Table 4 CI
> bound for an unrelated scale. Different quantities, same digits.

### B2. [VALUE] Population octant matrices — Note 3 (p. 18)

`devel/m4-zw-transcription.md`. These define the simulation conditions the
diagnostic was validated against.

> **✅ Confirmed 2026-07-19 by Jeff.** Both matrices correct as transcribed.

- [x] Without a substantial general factor (IAS, 2,988 students; Gurtman & Pincus, 2000; Wiggins, 1995): ρ1 `.430`, ρ2 `.030`, ρ3 `−.360`, ρ4 `−.740`
- [x] With a substantial general factor (IIP-C, 1,981 students; Gurtman & Balakrishnan, 1998): ρ1 `.683`, ρ2 `.500`, ρ3 `.345`, ρ4 `.288`

### B3. [VALUE] Scaling-factor formulas — Eq. A6, A7 (p. 18) and Eq. 3 (p. 12)

> **✅ Confirmed 2026-07-19 by Jeff against the page: Eq. A7 carries both the
> leading ½ and the √2 radicand.** This closes the record's one open channel
> discrepancy — previously settled by reasoning (only the √2 form reproduces
> the published scaling factors) rather than by a clean second read — and
> checks the ½, which no pass had ever verified. `m4-zw-transcription.md`
> updated.

- [x] Eq. A7 in full: `f_a = ½·√( √2(ρ1−ρ3) + (1−ρ4) )` — radicand `√2(ρ1−ρ3)+(1−ρ4)`, **not** `2(ρ1−ρ3)+(1−ρ4)`. Confirmed on the page, ½ included.
- [ ] Eq. A6: `f_e = √((2ρ1+2ρ2+2ρ3+ρ4+1)/8)`
- [ ] Eq. 3 (p. 12): `|AFF_min| = 2.95 · f_a · n^(−0.587)` — confirm both constants
- [ ] Values printed on **p. 9**: f_e → IIP-C `.737`, IAS `.240`; f_a → IIP-C `.545`, IAS `.845`
- [ ] **p. 14**: IIP-SC f_a `.625`; `|.029|` at N = 1,166
- [ ] **[DERIVED]** p. 12's worked values: the page prints **`.11`** (n = 100) and **`.03`** (n = 1,000), to two decimals. `.108` and `.028` are *our* recomputation from Eq. 3, not published figures — check the arithmetic, don't look for them on the page.

> **Corrected 2026-07-19** (Jeff): this last row previously listed `.108`/`.028`
> as though they were on p. 12, with the published `.11`/`.03` in parentheses —
> exactly backwards. The page prints two decimals. Same defect as B1a's f_a
> row, and the reason the **[DERIVED]** tag now exists.

> **Numeric self-check, re-run 2026-07-19.** Feeding the B2 matrices through
> the formulas as transcribed reproduces all eight published constants:
> f_e → .7369 / .2398 (pub .737 / .240); f_a → .5454 / .8452 / .6246
> (pub .545 / .845 / .625); Eq. 3 → .1077, .0279, .0292 (pub .11, .03, .029).
> The no-√2 variant gives .589 / .911 / .675 and misses every one.
>
> **What this does and does not settle.** It is a *consistency* check, not an
> independent oracle: B2's ρ values and these formulas were transcribed by the
> same pass, so a compensating pair of errors would still close. It does mean
> the √2 reading, the ½, and the four IIP-C ρ values are mutually pinned — an
> error in any one of them alone would break the identity. Read the pages
> anyway; this only tells you the set is internally coherent.

### B4. [VALUE] Study 5 reproduction (pp. 13–16) and Table 4 (p. 15)

The vignette reproduces these analyses live on the bundled `jz2017` data, so a
transcription error here would surface as a mismatch between the vignette's
prose and its own computed output. **The first cut's single row here
conflated two different Table 4 values** — OCPD's *amplitude* and PARPD's
*elevation* — and cited a stale line for each; split and re-anchored below.

- [ ] The Study 5 CircE fit pattern the vignette narrates (`:151-157`), including the printed indices: equal spacing + equal communality CFI `.824`, TLI `.795`, RMSEA `.169`; unequal spacing CFI `.958`, TLI `.931`, RMSEA `.098` (p. 14)
- [ ] Table 4, **obsessive–compulsive** PD: amplitude `.012` at full sample size — the vignette's stated reason for picking this scale as the near-flat case (`:237-238`)
- [ ] Table 4, **paranoid** PD: elevation `.250`, the value the ipsatizing section says the raw-score analysis matches (`:572-573`)
- [ ] N = 1,166 undergraduates, IIP-SC octants + PDQ-4+ target measures (`:41-42`)

> **✅ Confirmed 2026-07-19 by Jeff.** All four rows correct, **and the absent
> OCPD brackets are real** — a deliberate omission by the authors, not a
> transcription gap. **The trigger is its low `Prob` (.130), not its low R²**
> (.117). Table 4's own note defines Prob as "probability of accurate
> confidence intervals for amplitude and angular displacement" — exactly the
> two parameters withheld — and the authors' rule is not to interpret a/δ CIs
> when that estimate is < .50 (pp. 12–13, 16). R² is a separate quantity and
> does not govern which intervals print.
>
> *(An earlier version of this note gave R² as the reason; corrected the same
> day. The distinction matters here more than most: Prob is a statement about
> whether the interval is trustworthy, R² about how well the model fits, and
> conflating them is precisely the error the vignette's own guidance warns
> against.)*
>
> Both Table 4 rows are transcribed in full in `devel/m4-zw-transcription.md`.

### B5. [VALUE] Octant angles — Figure 1A (p. 3)

> **✅ Confirmed 2026-07-19 by Jeff.**

- [x] LM 0°, NO 45°, PA 90°, BC 135°, DE 180°, FG 225°, HI 270°, JK 315°

> Note the package's own convention reports LM as **360**, not 0. That is a
> deliberate package convention (D-003), not a transcription discrepancy —
> do not "correct" it.

---

## Attestation

When complete, record the outcome in the M7 work log: the date, that the
re-read was done against the primary sources, and either "no discrepancies"
or each discrepancy and its resolution. Then the two `pending (Jeff)` markers
in `tests/testthat/test-cpm_oracles.R:10-11`,
`tests/testthat/helper-cpm-oracles.R:6-7`, and
`devel/m4-zw-transcription.md` should be updated to record the completed
re-read and its date.
