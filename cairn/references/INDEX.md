# References index

_One line per committed page: `citekey — title — traces to`._

- [grassi2010.md](grassi2010.md) — Grassi, Luccio & Di Blas (2010), *CircE: An R implementation of Browne's circular stochastic process model* — the published CPM oracle; traces to `tests/testthat/helper-cpm-oracles.R` and `tests/testthat/test-cpm_oracles.R`.
- [zimmermann2017.md](zimmermann2017.md) — Zimmermann & Wright (2017), *Beyond description in interpersonal construct validation* — SSM estimator accuracy and sample-size guidance; traces to `vignettes/evaluating-circumplex-structure.Rmd` and the `jz2017` sample.
- [browne1992a.md](browne1992a.md) — Browne & Cudeck (1992), *Alternative ways of assessing model fit* — the RMSEA cutoffs (0.08 reasonable, greater than 0.1 would-not-employ) AND eqs. 13/14, which `cpm_fit()` implements natively; traces to `R/cpm_fit.R:1011-1028,1049`, `R/ssm_ci_accuracy.R:1014-1023`, `R/ssm_ci_oop.R` and `vignettes/evaluating-circumplex-structure.Rmd`.
- [hu1999.md](hu1999.md) — Hu & Bentler (1999), *Cutoff criteria for fit indexes in covariance structure analysis* — the SRMR .08 and CFI/TLI .95 cutoffs; traces to `R/ssm_ci_accuracy.R:1023`, `R/ssm_ci_oop.R`, `R/cpm_oop.R:187-188` and `vignettes/evaluating-circumplex-structure.Rmd`.

<!-- Entry format note: cairn_validate's _INDEX_LINE regex matches the first
     [\w./-]+\.md token after the bullet, so the link TEXT must be the
     filename ("[grassi2010.md](grassi2010.md)"). A bare citekey as link text
     ("[grassi2010](grassi2010.md)") is silently not counted as a catalog
     entry and the page reads as unindexed. Caught by mutation at M40 T5.

     Citekey trap: browne1992a.md is Browne & CUDECK (1992); browne1992.md
     (owed, M42) is Browne ALONE. The `a` suffix marks a different author set,
     not a second work by one author. Both pages state this.

     Which sources still owe a page is the milestone files' ledger, not this
     file's. As of the M41 merge the remaining sources owing a page are
     Browne (1992) and Browne (1982), both M42; and Acton & Revelle (2004)
     and Wendt et al. (2019), both M43 — observed 2026-07-19. The shelf is a
     live directory that changed twice during M40 alone, so treat any list
     here as a snapshot and re-inventory rather than trusting it.

     Shelved sources that owe NO page, each with its reason — all observed
     2026-07-19:
     - sources/acton2002.pdf — Acton & Revelle (2002); the repo cites it only
       as other authors' citation of prior work, which the "consulted in
       passing owes nothing" rule excludes.
     - sources/cudeck1983.pdf — Cudeck & Browne (1983), Cross-validation of
       covariance structures, MBR 18(2) 147-167; a different paper from
       "Alternative ways of assessing model fit", with the author order
       reversed. The repo neither computes nor asserts a cross-validation
       index (Grassi's Appendix A merely prints an ECVI). Assessed at M41.
     - sources/cheung2002.pdf — Cheung & Rensvold (2002); no shipped code,
       vignette, or test cites it. The ssm_sem() invariance gate ships the
       chi-square difference test only, a computed quantity needing no
       literature constant, and the delta-CFI option was left as an
       unexercised offer (devel/m5-sem-design.md:751-759, section 12.2
       item 2). Assessed at M41; a ROADMAP candidate row carries the feature
       question, and the milestone that ever takes it up authors the page. -->
