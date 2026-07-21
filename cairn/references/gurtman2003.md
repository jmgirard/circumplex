# gurtman2003 — the vignette-cited structural-summary methods reference

**Provenance.** Ingested 2026-07-20 by M47 from
`cairn/references/sources/gurtman2003.pdf` (gitignored).
Pagination: the handbook chapter's own pp. 407–428; the 22-page shelf PDF is one
page per printed page, so printed page = PDF page + 406 (Eq. 16.7 on p. 417 =
PDF p. 11).
The shelf PDF is a born-digital typeset PDF (font-encoded text with `ﬁ`/`ﬂ`
ligatures and `θ`/`δ` glyphs — not an OCR scan), so its text layer is a reliable
digital channel.
Extraction: verified 2026-07-20 against the source by the implementing session's own read of the born-digital text layer — the authoritative channel for a born-digital source — with the rendered p. 417 image spot-checked; per M41-D1 this is the session's own read, not a human attestation, and no value on this page has been read by a human — observed 2026-07-20.

**Citation.** Gurtman, M. B., & Pincus, A. L. (2003). The circumplex model:
Methods and research applications. In J. A. Schinka & W. F. Velicer (Eds.),
*Handbook of psychology. Volume 2: Research methods in psychology* (pp. 407–428).
Hoboken, NJ: John Wiley & Sons.

**Role.** The Structural Summary Method methods reference the package's SSM
vignettes cite. It restates the same structural-summary model the estimator
implements (`gurtman1998.md` is its defining source); it is not itself the
estimator's originating source, and it does not carry the numeric R² adequacy
cutoffs sometimes attributed to it (see Open questions).

## Extracted values

From p. 417 (born-digital text layer), quoted verbatim:

- **Structural-summary model** — `Si = e + a × cos(θi − δ) + di`, Eq. (16.7),
  p. 417, where `Si` is "the person's score on scale, i, of a circumplex
  measure; e is the elevation, or mean level, of the profile; a is the amplitude
  of the cosine curve model (the distance from its mean level to its peak
  value); θi is the angle of scale, i; δ is the angular displacement, or peak
  shift, of the cosine curve; and di is the deviation, generally assumed to be
  random and pairwise independent. This model then has three parameters—e, a,
  and δ."
- **Goodness of fit** — "a goodness-of-fit index, R2, can be calculated to
  indicate how well the cosine model fits the actual profile data, in a sense,
  quantifying the extent to which the profile can be reduced to its summary
  features", p. 417. No R² formula or numeric cutoff is given on this page.
- **Attribution of the summary** — "In an earlier work, Gurtman (1994) noted
  that elevation, amplitude, angular displacement, and goodness-of-fit constitute
  what amounts to a structural summary of the individual's circular profile",
  p. 417.

## Traces to

- `vignettes/introduction-to-ssm-analysis.Rmd:443`,
  `vignettes/intermediate-ssm-analysis.Rmd:280` — cite Gurtman & Pincus (2003)
  as an SSM methods reference.
- `R/ssm_analysis.R:1183` — Eq. (16.7) is the same cosine model the estimator
  fits; a secondary exposition of the estimator `gurtman1998.md` defines.
- `cairn/references/gurtman1998.md`, `cairn/references/wright2009.md` — companion
  estimator sources.

## Open questions

- gurtman2003 does **not** print the ".80 adequate / .70 inadequate" R² cutoffs
  that `wright2009.md` (p. 315) attributes to "Gurtman & Pincus, 2003": a full-text
  search finds `.80`/`.70` only as table cell values, never as an adequacy
  threshold. The cutoffs trace to Wright et al.'s rendering, not to a verbatim
  Gurtman & Pincus (2003) statement — observed 2026-07-20.
