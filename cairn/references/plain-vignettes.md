# Plain-English vignettes: reader, rules, sweep and ledger (M123)

**Provenance.** Ingested 2026-09-14 by M123 from the plan of M123 to M125 and from `tools/prose-sweep.R` as written in M123.
Pagination: —.
Extraction: first-hand record, nothing to re-verify against — observed 2026-09-14.

**Scope.** This page defines the plain-English pass over the vignettes. It holds the reader profile, the writing rules, the precision list, the definition of the prose sweep, and one ledger per milestone. It is not a style guide for help pages or the README. It is a reference, not an authority. Status lives in `ROADMAP.md`, decisions in `DECISIONS.md`, and architecture in `DESIGN.md`.

## Reader profile

The reader is an applied researcher. The reader knows R, data frames, correlation, linear regression and confidence intervals. The reader does not know the circumplex terms (elevation, amplitude, displacement, octant) until a page defines them or links the introduction vignette. The reader reads each paragraph once, in order, and does not reread to parse a sentence.

## Rules

1. A sentence has 25 words or fewer. A code span or a math span counts as one word.
2. Use active voice and simple tenses where the claim allows it.
3. Define a term at its first use on the page, or link `vignette("introduction-to-ssm-analysis")` where that page defines it.
4. Change the form, not the claims. This is rule 6 of the tidymedia plain-English pass: a rewrite moves, splits and rewords sentences, and it adds, drops or changes no claim.
5. Keep every statistical qualifier. A word that limits a claim ("approximately", "for equally spaced angles", "not a significance test", "posterior") stays in the text. A longer sentence is an acceptable cost. If a qualifier does not fit in 25 words, split the sentence and keep the qualifier.
6. No dash and no semicolon in prose. Use a period, a comma, parentheses or a connecting word ("because", "but", "so").
7. No milestone, decision or review id (`M123`, `D-015`, `RR22`) in a vignette.
8. Code chunks, chunk options, figure captions and alt text do not change in this pass.
9. A gloss is a claim. Check each new gloss against `R/` or the chunk, and give a batch of fix-round glosses its own fresh re-read. At M123's review, 5 of 16 new glosses were wrong or added a claim (added M123 review).

## Precision list

The `--inventory` mode of `tools/prose-sweep.R` reads the terms below. It matches each term case-insensitively at the start of a word, so `interval` also matches "intervals".

- interval
- credible
- confidence
- significant
- contrast
- displacement
- amplitude
- elevation
- fit
- posterior
- prior
- bootstrap
- circular
- quantile
- median
- mean
- pooling
- random
- fixed
- boundary
- wrap
- undefined
- deprecated
- approximate
- significance
- hypothesis
- unwrap
- nominal
- certif
- simultaneous

## The prose sweep

`tools/prose-sweep.R` is the definition in code. This section and the script change together.

Swept prose is a page minus five parts: the YAML header, every HTML comment (multi-line ones too), the References section, every fenced block, and every knitr table. A knitr table is a line that starts with `Table:`, followed after any blank lines by pipe-table rows. `knitr::kable(caption = ...)` writes this form into a knitted `.Rmd`, and chunk code wrote its text. The caption line and the rows up to the first line that is not a table row are removed. A `Table:` line with no table rows under it stays prose (added M124). A fenced block opens with three or more backticks or tildes, with or without an info string. It closes with at least as many of the same character. The References section runs from a heading named "References" to the next heading of the same or a higher level.

A sentence is a run of words in swept prose that ends at one of these points:

- a period, question mark or exclamation mark before a space or the end of the unit (closing quotes, parentheses and emphasis marks can come between).
- the end of a heading, list item, blockquote line, table cell or paragraph.

A period after a single capital letter ("J.") does not end a sentence. A period in `e.g.`, `i.e.`, `et al.`, `vs.` or `cf.` does not end one either, because those abbreviations usually run on. A code span (backticks) and a math span (`$...$` or `$$...$$`) each count as one word. If a token holds a letter or a digit, it counts as a word. Table alignment rows and horizontal rules hold no sentences.

A dash is any of U+2014 (the em dash), `---`, ` -- ` (two hyphens with a space on each side) and `&mdash;`. The sweep does not search code spans or math spans for a dash or a semicolon. HTML entities such as `&amp;` do not count as semicolons. The sweep searches for ids everywhere in swept prose, code spans, link text and URLs included. The id pattern is `\b(M[0-9]{2,3}|D-[0-9]{3}|RR[0-9]{2})\b`.

The sweep exits 0 on a clean page and 1 on a finding. If a file has no sentences, it exits 2. On a usage error, it exits 3. The `--chunks` mode reads no prose, so it never exits 1 or 2. The sweep reads its input as UTF-8 in any locale. If a line is not valid UTF-8, it exits 3 and names the line (corrected M123 review: the page said a non-UTF-8 locale missed the em dash, but the script crashed with exit 1):

```
Rscript tools/prose-sweep.R vignettes/<name>.Rmd.orig
```

`--prose` prints one sentence per line. `--chunks` prints every fenced block with its opening and closing lines and without its `#>` output lines. `--inventory` prints, sorted and once each, every number, degree value, code-span content and precision-list term in the swept prose. A minus sign (a hyphen or U+2212) after a space, an opening bracket or the start of a unit stays on its number. The inventory records whether an item appears, not how often.

`tests/testthat/test-prose-sweep.R` plants each finding kind in several forms and places and asserts which finding each plant produces.

### Known sweep gaps

The M123 review found these gaps and deferred them to the candidate row "The prose sweep as a merge gate". No page in M123 hits one.

- A hard-wrapped line that starts with `1. `, `- `, `+ ` or `* ` starts a new unit, so it splits a sentence the renderer keeps whole.
- `etc.`, `approx.`, `Fig.`, `Eq.` and `resp.` end a sentence.
- Each blockquote line is its own unit, so a long sentence wrapped across `>` lines passes.
- `<!--` inside a code span opens a comment and drops prose up to the next `-->`.
- A line such as "```r x``` is odd." opens a fence that swallows the rest of the page.
- `&#8212;`, `&#x2014;`, a spaced en dash and a ` --` at the end of a unit are not flagged. `R&D;` hides its semicolon.
- Ids are searched only in swept prose, not in chunks, comments, YAML or References.
- `--inventory` reads the precision list relative to the working directory.
- A pandoc heading `# References {-}` and setext headings are not recognized.
- A re-knit that adds or removes a chunk's output adds or removes a fence pair in `--chunks` output.
- The tests plant no References section followed by more prose, no id in a code span and no dash in math. They also plant no inline comment with prose on both sides, no tilde fence with an info string, no fence over three characters and no `-` (stdin) input.
- The em dash test fails, not skips, on a machine without the `en_US.UTF-8` locale.
- A hand-written pipe table with a `Table:` caption line above it is skipped as a knitr table. A `kable()` table without a caption is still swept, so its generated cells can produce findings (added M124).
- A skipped knitr table is in neither swept prose nor `--chunks` output, so a changed table passes both. Only `vignette-precompute` compares it. The rule also misses a pandoc `: caption` line, a caption under the table, and an indented table. Prose right after the last row, with no blank line, can join the prose before the table (added M124 review).

### A vignette re-knit and the chunk comparison

A pre-computed vignette ships as a knitted `.Rmd`, and `tools/precompute-vignettes.R` regenerates it from the `.Rmd.orig`. A re-knit rewrites each chunk's opening line (`{r name, ...}` becomes ` r`) and its `#>` output lines. `--chunks` drops the `#>` lines, so new output alone does not change the comparison. The opening lines of the shipped `.Rmd` stay the same across re-knits of unchanged chunk options. So a base-to-head difference in `--chunks` output of a shipped `.Rmd` means chunk source or chunk options changed. A re-knit also rewrites the `<img>` lines that knitr writes for figures. Those lines are prose, not chunks, so they do not affect the chunk comparison. The `vignette-precompute` workflow compares the shipped prose to the re-knit byte for byte. So a prose edit that you copy into the `.Rmd` by hand must match the `.Rmd.orig` exactly.

## Ledger

Each row records an item that a reader or the inventory comparison listed and that the pass kept as it is, with its evidence. A fixed item needs no row. The milestone's work log gives the count of fixed items per reader.

### M123 (bayesian-ssm-analysis, growth-ssm-analysis, advanced-visualization)

Reader reports, numbered as the rows cite them: claims reader C1 to C20, one-read reader B1 to B10 (Bayesian), G1 to G16 (growth), V1 to V11 (visualization).

| Page | Item | Disposition | Evidence |
|---|---|---|---|
| bayesian, growth, visualization | Inventory: `vignette("introduction-to-ssm-analysis")` added (C1, C6, V1) | Kept | Rule 3 asks for this link. The introduction vignette exists in `vignettes/`. |
| bayesian, growth, visualization | Inventory: "45°" added | Kept | The gloss "eight scales placed 45° apart" matches `octants()`, whose eight angles are the multiples of 45 from 45 to 360. |
| growth | Inventory: `ssm_analyze(method = "montecarlo")` added (G7) | Removed at review (corrected M123 review) | The review's claims reader found "the same one" stronger than the base "the same asymptotic move". The head now says "the same large-sample (asymptotic) step". |
| bayesian | C5: "Convert them to ..., or pass profile draws" is an instruction | Kept | The claim is the same: both routes make the draws summarizable. |
| growth | C15: "multivariate normal (MVN)" gloss | Kept | The expansion is correct. |
| growth | C16: "exact posterior inference" for projected-normal regression | Kept as a base claim | The base text makes the claim. The fix moved the claim back onto the method, not the package. Whether "exact" fits MCMC output is outside a form-only pass. |
| bayesian | B1: "derived quantities" has no example | Kept | The phrase is general on purpose. Partial pooling is now glossed. |
| bayesian | B4: "group-level" can clash with "groups" | Kept | "Group-level" is the standard name for fixed effects in a mixed model. Octant is now glossed. |
| growth | G2: "circular-correct summaries" not explained | Kept | Section 4 names them (circular means, wrapped intervals). $x$, $y$ and $(a(t), d(t))$ are now defined. |
| growth | G7: "equal-tailed intervals" | Kept | This is a standard statistics term for the reader profile. |

### M124 (axes-reliability, sem-based-ssm-analysis)

Reader reports, numbered as the rows cite them: claims reader A1 to A2 (axes) and S1 (SEM), one-read reader R1 to R28 (axes) and E1 to E29 (SEM), and gloss re-read G1 to G7. Items not in a row were fixed.

| Page | Item | Disposition | Evidence |
|---|---|---|---|
| axes, SEM | Inventory: `vignette("introduction-to-ssm-analysis")` added | Kept | Rule 3 asks for this link. The introduction vignette defines elevation, amplitude, displacement and fit. |
| axes | Inventory: `sd` added (R7) | Kept | "pass numeric axis SDs to `sd`" names the argument that the base text used. `R/axes_reliability.R` accepts numeric SDs. |
| SEM | Inventory: `lx`, `ly` added (E4) | Kept | The gloss names the loadings in the constraint. The generated syntax writes `cx =~ lx*` and `cy =~ ly*`. |
| axes | A2: "`zeta2` row" became "`block_specificity` row (symbol `zeta2`)" | Kept | The component table rows are built with `Component = "block_specificity", Symbol = "zeta2"` in `R/axes_reliability.R`. |
| axes | R19: "Under MAR" became "Under MAR that is not MCAR" | Kept | The base sentence contrasts MAR with the MCAR case just before it. MCAR is a special case of MAR, so the unqualified base contradicted itself. |
| SEM | E12, G2: "linear parameters get percentile intervals" became "elevation, X value, Y value and amplitude" | Kept | Amplitude is nonlinear but gets a percentile interval, and fit gets none (`R/ssm_bootstrap.R`, `ssm_replicate_intervals`). |
| axes | R6, R9, R10 (calibrated uncertainty), R14 (asymptotically exact), R15, R16, R20, R21, R23, R24, R27 | Rejected | Technical notes for a reader who checks the numbers. A gloss would need facts the page and `?axes_reliability` do not state, such as the band width or the FIML standardization details. |
| axes | R8: "Four properties" | Rejected | Section 5 has four bold properties. The calibration question is a sub-point of the first. |
| axes | R12: lavaan variant names | Rejected | The paragraph addresses readers who cross-check in lavaan. It is now split in two. |
| SEM | E2, E3, E7, E10, E11, E13, E14, E15, E20, E23 | Rejected | The terms are explained by the sentences around them, or the text addresses readers who fit their own lavaan models. |
| SEM | E18: table cells | Rejected | A code chunk writes the table. Chunks do not change in this pass. |
| SEM | E19: configural and scalar rungs | Rejected | Standard SEM terms. A gloss of scalar invariance needs a claim about intercepts that the page does not make. |
| SEM | E24, E25: "branch" | Rejected | The next sentence says what the branch means for the interval endpoints. |
| SEM | E26: "plane factors are fixed isotropic and orthogonal" | Kept as a base claim | The generated syntax fixes them only under the scaled tier, and the strict tier frees the factor covariance. The `ssm_plot_trajectory()` doc-bug candidate row now holds the fix, because a form-only pass does not change the claim. |
| SEM | E29: "point" then "boundary" | Rejected | The point is where the families meet. The boundary divides fixed from free angles. |

The review readers read `53d94913`, and their items carry a V prefix. The claims readers gave VA1 to VA12 (axes) and VS1 to VS10 (SEM). The one-read readers gave VR1 to VR30 (axes) and VE1 to VE33 (SEM). The diff reviewer gave F1 to F7. The review fixed six items: VA1, VA2, VA4, VS1, VS3 and VS9. For VA1, the five components now count the axes as one and block specificity as the fifth (`strack2013.md` p. 4). For VA2, the gloss is gone and the base wording is back, because the mean match is a large-sample result. For VS1, the clause is gone, because the package uses robust SEs against misspecification. A fresh re-read checked the six fixes. It found problems in VA2, VS3 and VS9, and those three now use base wording or the re-reader's wording.

| Page | Item | Disposition | Evidence |
|---|---|---|---|
| axes, SEM | VA3, VS2, VS8 | Matched | Rows R19, E12 and the inventory row above. |
| axes | VA5 "misprices" became "comes out wrong", VA8 "direct read-out" became "shows" | Rejected | The scope clause stays, and the quantities named are the same. |
| axes | VA6 "oracle" became "simulated test data" | Rejected | "Known by construction" keeps the known-truth sense. |
| axes | VA7, F3: "item specificity (the item error component)" | Rejected | `strack2013.md` p. 4 names the free error term item specificity. The page calls the same component item error. |
| axes | VA9 exact-fit and approximate-fit classes, VA10, VA11, VA12 | Rejected | Standard terms or a claim the base implied, and each is consistent with `R/axes_reliability.R` and `R/axes_scaled_fit.R`. VA11 adds the missing Hu and Bentler reference. |
| SEM | VS4, VS5 (F5), VS6, VS10 | Rejected | Each names a referent or glosses a standard term correctly, and the reader found each accurate. |
| SEM | VS7: categorical and hybrid glosses | Rejected | `wendt2019.md` names the categorical model (LCA) and the hybrid model (SP-FA). |
| axes | VR1, VR2, VR3, VR5, VR6, VR7, VR11, VR12, VR13, VR14, VR17, VR19, VR20 | Rejected | Technical notes for a reader who checks the numbers or cross-checks in lavaan. A gloss needs facts the page and `?axes_reliability` do not state. |
| axes | VR15, VR16, VR22, VR23, VR24, VR25 | Rejected | Base wording or base structure (VR16 as R8 above). An added reason why is a new claim. |
| axes | VR4, VR8, VR9, VR10, VR18, VR21, VR26, VR27, VR28, VR29, VR30 | Rejected | A later paragraph, the linked introduction vignette or the instruments vignette defines the term. Otherwise the term is standard for the reader profile. |
| SEM | VE1, VE3, VE4, VE7, VE8, VE9, VE11, VE12, VE13, VE14, VE17, VE18, VE19, VE21, VE23, VE24, VE25, VE26, VE27, VE28, VE30, VE32, VE33 | Rejected | Technical notes for a reader who fits their own lavaan models, as E2 and the related rows above. |
| SEM | VE2 (the article's ΔCFI direction), VE5 (the E26 bullet), VE20 | Rejected | Base claims that a form-only pass keeps. VE5 is in the doc-bug candidate row. |
| SEM | VE6, VE10, VE15, VE16, VE22, VE29, VE31 | Rejected | The term is defined in the introduction vignette that the page links, or in a later section that the text names. |
| SEM | F6, F7: knitr tables are in neither the sweep nor `--chunks`, and the table rule misses other caption forms | Follow-up | Listed under "Known sweep gaps". `vignette-precompute` still compares the table output. |

### M125 (evaluating-circumplex-structure)

| Page | Item | Disposition | Evidence |
|---|---|---|---|
