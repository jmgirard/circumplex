# M38: Guaranteed rim ring for the circumplex canvas — done

**Goal:** guarantee `coord_circumplex()` draws an amplitude ring at `amax`, so
every circumplex canvas closes at its rim. PR #63, merged 2026-07-18.

**Outcome:** the break algorithm places a break at `amax` only by coincidence --
over [0, 1.75] it proposes 2, correctly censored -- so the outermost ring sat
below the rim and the circle rendered open. `rim_view_scale()` appends the rim to
the radial `ViewScale`'s breaks, and `guide_grid()` draws rings from
`r$mapped_breaks()`, so ring and guide follow from one patched break set, on top
of PR #62's ULP headroom. The vignette's `coord-bare` chunk gained a corrected
counterpart; all 15 figures inspected. Suite 2986; check 0/0/0; 9/9 CI green.

**Key decisions:** M38-D1 -- the rim is unlabeled unless `amax` is a generated
break, and no generated break is removed; reverses the plan gate's labeled-rim-
plus-suppression design on render evidence (crowding tracks label width, not
break spacing, so a clearing threshold fires on the commonest case). M38-D2
(review) corrects M38-D1 on trained canvases: `amax = NULL` is the default and a
trained rim is the data max, so unlabeled is the common case.

**Review:** 5 findings, all >=80, all fixed -- two regressions M38 introduced
(appending the rim desynchronized the break set from a scale's explicit `labels`,
a hard build error; blanking by index into `NULL` labels gave literal `NA`
labels), a toothless trained-`amax` assertion, M38-D1's overstated rationale, and
wrong vignette prose. Other two lenses clean.
