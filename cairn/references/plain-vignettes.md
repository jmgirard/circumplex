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

## The prose sweep

`tools/prose-sweep.R` is the definition in code. This section and the script change together.

Swept prose is a page minus four parts: the YAML header, every HTML comment (multi-line ones too), the References section, and every fenced block. A fenced block opens with three or more backticks or tildes, with or without an info string. It closes with at least as many of the same character. The References section runs from a heading named "References" to the next heading of the same or a higher level.

A sentence is a run of words in swept prose that ends at one of these points:

- a period, question mark or exclamation mark before a space or the end of the unit (closing quotes, parentheses and emphasis marks can come between).
- the end of a heading, list item, blockquote line, table cell or paragraph.

A period after a single capital letter ("J.") does not end a sentence. A period in `e.g.`, `i.e.`, `et al.`, `vs.` or `cf.` does not end one either, because those abbreviations usually run on. A code span (backticks) and a math span (`$...$` or `$$...$$`) each count as one word. If a token holds a letter or a digit, it counts as a word. Table alignment rows and horizontal rules hold no sentences.

A dash is any of U+2014 (the em dash), `---`, ` -- ` (two hyphens with a space on each side) and `&mdash;`. The sweep does not search code spans or math spans for a dash or a semicolon. HTML entities such as `&amp;` do not count as semicolons. The sweep searches for ids everywhere in swept prose, code spans, link text and URLs included. The id pattern is `\b(M[0-9]{2,3}|D-[0-9]{3}|RR[0-9]{2})\b`.

The sweep exits 0 on a clean page and 1 on a finding. If a file has no sentences, it exits 2. On a usage error, it exits 3. If the locale is not UTF-8, the em dash does not match, so set it:

```
LC_ALL=en_US.UTF-8 Rscript tools/prose-sweep.R vignettes/<name>.Rmd.orig
```

`--prose` prints one sentence per line. `--chunks` prints every fenced block with its opening and closing lines and without its `#>` output lines. `--inventory` prints, sorted and once each, every number, degree value, code-span content and precision-list term in the swept prose.

`tests/testthat/test-prose-sweep.R` plants each finding kind in several forms and places and asserts which finding each plant produces.

### A vignette re-knit and the chunk comparison

A pre-computed vignette ships as a knitted `.Rmd`, and `tools/precompute-vignettes.R` regenerates it from the `.Rmd.orig`. A re-knit rewrites each chunk's opening line (`{r name, ...}` becomes ` r`) and its `#>` output lines. `--chunks` drops the `#>` lines, so new output alone does not change the comparison. The opening lines of the shipped `.Rmd` stay the same across re-knits of unchanged chunk options. So a base-to-head difference in `--chunks` output of a shipped `.Rmd` means chunk source or chunk options changed. A re-knit also rewrites the `<img>` lines that knitr writes for figures. Those lines are prose, not chunks, so they do not affect the chunk comparison. The `vignette-precompute` workflow compares the shipped prose to the re-knit byte for byte. So a prose edit that you copy into the `.Rmd` by hand must match the `.Rmd.orig` exactly.

## Ledger

Each row records an item that a reader or the inventory comparison listed and that the pass kept as it is, with its evidence. Dispositions that fix the page need no row.

### M123 (bayesian-ssm-analysis, growth-ssm-analysis, advanced-visualization)

| Page | Item | Disposition | Evidence |
|---|---|---|---|

### M124 (axes-reliability, sem-based-ssm-analysis)

| Page | Item | Disposition | Evidence |
|---|---|---|---|

### M125 (evaluating-circumplex-structure)

| Page | Item | Disposition | Evidence |
|---|---|---|---|
