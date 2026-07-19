# M40: Source notes for the two shelved primary sources (done)

**Goal.** Retire the pre-migration deferral in `cairn/references/INDEX.md` by
authoring source notes for the two sources already on the shelf, setting the
conventions the remaining seven follow.

**Outcome.** `cairn/references/` is a working reference system: `grassi2010.md`
(the published CPM oracle) and `zimmermann2017.md` (estimator accuracy behind
the vignette's sample-size guidance), page-anchored with `Traces to` lists,
plus an `INDEX.md` that no longer points at `devel/`. Both were re-derived from
the shelf PDFs rather than trusted from the existing transcriptions; every
value reproduced and an independent Opus reviewer found no numeric
disagreement. Newly recorded: Grassi's Appendix A uses 90% CIs for fit measures
but 95% for communality indices, and Eq. A7 rests on a single human read
because `pdftotext` drops its √2 and leading ½. Docs-only — `devel/`
byte-untouched (M7 cites it); tarball excludes `cairn/`.

**Decisions.** M40-D1 citekeys follow the shelf's `author+year` filenames
(binds M41); M40-D2 an `Extraction:` status claims only what actually ran;
M40-D3 the Browne & Cudeck 1992-vs-1993 mismatch at `R/ssm_ci_oop.R:415` → M41.

**Review.** Three findings fixed (90/88/80): a dated absence claim false when
written (Browne 1982 pp. 95–96 landed a minute before that commit); a status
claiming two channels for eight values only `pdftotext` saw; undated repo-state
claims. One logged below threshold (58). CI 9/9. PR #66, `60fc20b2`.
