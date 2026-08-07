# References index

_One line per committed page: `citekey — title — traces to`._

- [grassi2010.md](grassi2010.md) — Grassi, Luccio & Di Blas (2010), *CircE: An R implementation of Browne's circular stochastic process model* — the published CPM oracle; traces to `tests/testthat/helper-cpm-oracles.R` and `tests/testthat/test-cpm_oracles.R`.
- [zimmermann2017.md](zimmermann2017.md) — Zimmermann & Wright (2017), *Beyond description in interpersonal construct validation* — SSM estimator accuracy and sample-size guidance; traces to `vignettes/evaluating-circumplex-structure.Rmd` and the `jz2017` sample.
- [browne1992a.md](browne1992a.md) — Browne & Cudeck (1992), *Alternative ways of assessing model fit* — the RMSEA cutoffs (0.08 reasonable, greater than 0.1 would-not-employ) AND eqs. 13/14, which `cpm_fit()` implements natively; traces to `R/cpm_fit.R:1039-1060,1085`, `R/ssm_ci_accuracy.R:1014-1023`, `R/ssm_ci_oop.R` and `vignettes/evaluating-circumplex-structure.Rmd`.
- [browne1992.md](browne1992.md) — Browne (1992), *Circumplex models for correlation matrices* — the full specification of the CPM `cpm_fit()` estimates (eqs. 1–8, 30–48), plus a parameter map, five printed errata, and three departures the code takes from the paper; traces to `R/cpm_fit.R`, `R/cpm_oop.R:123,162`, `R/ssm_ci_accuracy.R:169-170`, `tests/testthat/helper-cpm-oracles.R` and both vignettes.
- [browne1982.md](browne1982.md) — Browne (1982), *Covariance structures*, pp. 95–96 only — the general transform-and-invert CI method (eqs. 1.6.29–1.6.41), whose log instance the communality-CI oracle reconstructs; traces to `tests/testthat/test-cpm_oracles.R:131-137` and `cairn/references/grassi2010.md`. No `R/` estimator uses the method (the page dates and scopes that claim).
- [acton2004.md](acton2004.md) — Acton & Revelle (2004), *Evaluation of ten psychometric criteria for circumplex structure* — the four criteria `fit_structure()` implements (Fisher eq. 6, Gap eq. 2, VT2 eq. 8, RT eq. 9), the Eq. 6 printed-vs-prose split, and the published nv = 64/128 cutoffs the shipped nv = 8 constants are **not** taken from; traces to `R/fit_structure.R`, `R/fit_structure_oop.R`, `data-raw/structure-test-cutoffs.R` and `vignettes/evaluating-circumplex-structure.Rmd`.
- [wendt2019.md](wendt2019.md) — Wendt et al. (2019), *The latent structure of interpersonal problems* — the real-data benchmark for the fixed-angle circumplex CFA (RMSEA .075–.111, g–agency ≈ −.29 across four samples); traces to `vignettes/sem-based-ssm-analysis.Rmd:44,114,368,394-400,428`. Context for the strict tier, never an oracle.
- [hu1999.md](hu1999.md) — Hu & Bentler (1999), *Cutoff criteria for fit indexes in covariance structure analysis* — the SRMR .08 and CFI/TLI .95 cutoffs; traces to `R/ssm_ci_accuracy.R:1023`, `R/ssm_ci_oop.R`, `R/cpm_oop.R:187-188` and `vignettes/evaluating-circumplex-structure.Rmd`.
- [hubert1987.md](hubert1987.md) — Hubert & Arabie (1987), *Evaluating order hypotheses within proximity matrices* — the randomization order test behind `fit_structure()`'s RANDALL: the normalized agreement index (A−D)/(A+D+T) and its exact / (M+1)/(N+1) randomization p-value; traces to `R/fit_structure.R` (`structure_randall()`, `structure_randall_test()`), `tests/testthat/test-fit_structure.R` and `vignettes/evaluating-circumplex-structure.Rmd`.
- [tracey1997.md](tracey1997.md) — Tracey (1997), *RANDALL: A Microsoft FORTRAN program…* — the program operationalizing Hubert & Arabie (1987), naming the Correspondence Index; traces to the same RANDALL implementation and `man/fit_structure.Rd` (`@references`). Companion to `hubert1987.md`; its Table 1 pins the tie case where the code's index (.92) departs from RANDALL's CI (.93).
- [wright2009.md](wright2009.md) — Wright, Pincus, Conroy & Hilsenroth (2009), *Integrating methods to optimize circumplex description and comparison of groups* — the published closed-form SSM estimator (Eqs. 7–13) and the source of the `aw2009` example data (its Table A); traces to `R/ssm_analysis.R:1183`, `R/example_data.R:4`, `man/aw2009.Rd`, `tests/testthat/test-ssm_sem_syntax.R:6` and the three SSM vignettes.
- [gurtman1998.md](gurtman1998.md) — Gurtman & Balakrishnan (1998), *Circular measurement redux* — the originating definition of the "conventional Gurtman estimator": the structural-summary decomposition (Eq. 1) and `R² = 1 − Σd²/SS_Total` (Eq. 2, p. 349, the page Wright 2009 cites); traces to `R/ssm_analysis.R:1183`, `tests/testthat/test-ssm_sem_syntax.R:6` and `cairn/references/wright2009.md`.
- [gurtman2003.md](gurtman2003.md) — Gurtman & Pincus (2003), *The circumplex model: Methods and research applications* — the vignette-cited SSM methods reference restating the structural-summary model (Eq. 16.7); traces to `vignettes/introduction-to-ssm-analysis.Rmd:443`, `vignettes/intermediate-ssm-analysis.Rmd:280` and `R/ssm_analysis.R:1183`. Does NOT carry the .80/.70 R² cutoffs Wright attributes to it.
- [strack2013.md](strack2013.md) — Strack, Jacobs & Grosse Holtforth (2013), *Reliability of Circumplex Axes* — the tau-equivalent CFA variance-decomposition model, the Spearman–Brown axes-reliability / SEm / Nunnally–Bernstein formulas, and Table 3 as a published-value oracle for the formula layer; traces to `devel/m53-axes-reliability-spec.md` (code traces land on a build GO).
- [cheung2002.md](cheung2002.md) — Cheung & Rensvold (2002), *Evaluating goodness-of-fit indexes for testing measurement invariance* — the ΔCFI −.01 cutoff with its published alpha (.01), the ΔGamma-hat/ΔMcDonald's-NCI cutoffs, Table 5's per-hypothesis 1% tails, and the binding simulation scope (two groups, ML, multivariate normal, Type I only); traces to `R/ssm_sem.R:759-771,773-790,794-816,921-932`, `tests/testthat/test-ssm_sem_groups.R`, `man/ssm_sem.Rd` and `vignettes/sem-based-ssm-analysis.Rmd`. Settles the direction the article's own p. 251 sentence states backwards.
- [satorra1994.md](satorra1994.md) — Satorra & Bentler (1994), *Corrections to test statistics and standard errors in covariance structure analysis* — the scaled statistic T̄ = c⁻¹T with c = trace{UΓ/r} (eqs. 16.21/16.22, p. 407), U = H − HΔ(Δ′HΔ)⁻¹Δ′H (eq. 16.18, p. 406), and p. 401's sentence licensing Γ as the acov of any moments; traces to `R/axes_scaled_fit.R` and `tests/testthat/test-axes-scaled-fit.R`. States the fix, not the problem — pair with `cudeck1989.md`.
- [cudeck1989.md](cudeck1989.md) — Cudeck (1989), *Analysis of correlation matrices using covariance structure models* — the three errors of fitting a covariance structure to a correlation matrix, the scale-invariance definition (p. 319), and Table 4's published 48% standard-error discrepancy (p. 323); traces to `R/axes_corrected_se.R:1-29` (anchoring its unpaged `(Cudeck, 1989)` at line 22) and `R/axes_scaled_fit.R`. States the problem, not the fix; its Error (b) is **not** the error M68 corrects — the page says why.
- [norms-audit.md](norms-audit.md) — Norms provenance audit (M72) — a synthesis note carrying the per-instrument audit status for all 15 shipped instruments, the instrument→citekey map the audit script joins on, and the sha256 + scan-verdict shelf manifest; batch 1 audited csie, csig, csip, csiv and iitc.
- [locke2007.md](locke2007.md) — Locke & Sadler (2007), *Self-efficacy, values, and complementarity in dyadic interactions* — the CSIE's validation study and the origin of its normative sample (n = 367, Table 1 note p. 97), which publishes **no** octant M/SD; the shipped values come from the author's CSIE norms page, and the item mapping from his scoring page. Traces to `data/csie.rda`, `data-raw/csie.R`.
- [locke2014.md](locke2014.md) — Locke (2014), *Circumplex scales of intergroup goals* — the published source for every shipped CSIG norm value, printed inside Figure 2 (p. 436) rather than in a table, with the item mapping from the CSIG Items & Scales page. Records the source's identical PA/NO statistics. Traces to `data/csig.rda`, `data-raw/csig.R`.
- [boudreaux2018.md](boudreaux2018.md) — Boudreaux, Ozer, Oltmanns & Wright (2018), *Development and validation of the Circumplex Scales of Interpersonal Problems* — the published source for every shipped CSIP norm value (Table 1 overall column, p. 600, n = 712), with the documented `/8` sum-to-item-mean conversion. Traces to `data/csip.rda`, `data-raw/csip.R`.
- [locke2000.md](locke2000.md) — Locke (2000), *Circumplex Scales of Interpersonal Values* — supplies the CSIV **angle** assignment (Figure 2, p. 255, degrees printed) but publishes no octant M/SD and never reports the shipped N = 1200; those come from the author's CSIV norms page, a different sample from the article's N = 588. Traces to `data/csiv.rda`, `data-raw/csiv.R`.
- [bliton2019.md](bliton2019.md) — Bliton & Pincus (2019), *Construction and validation of the Interpersonal Influence Tactics Circumplex (IIT-C) Scales* — the published source for every shipped IIT-C norm value (Table 1, p. 7, N = 862); item numbering is in an online appendix, not the article. Traces to `data/iitc.rda`, `data-raw/iitc.R`.
- [hatcher2009.md](hatcher2009.md) — Hatcher & Rogers (2009), *Development and validation of a measure of interpersonal strengths* — the published source for every shipped IIS-64 norm value (Table 1, p. 558, N = 684) and for its octant item grouping (Appendix, p. 569); records the article's own 684-vs-686 sample-size inconsistency. Traces to `data/iis64.rda`, `data-raw/iis64.R`.
- [hatcher2012.md](hatcher2012.md) — Hatcher & Rogers (2012), *The IIS–32: A brief inventory of interpersonal strengths* — supplies the IIS-32 **item grouping** (Appendix, p. 646) but publishes **no** octant M/SD and never reports the shipped N = 1380, so those shipped values have no identified source; records the bounded sweep behind that absence. Traces to `data/iis32.rda`, `data-raw/iis32.R`.
- [markey2009.md](markey2009.md) — Markey & Markey (2009), *A brief assessment of the interpersonal circumplex: The IPIP-IPC* — supplies the IPIP-IPC **item assignment and anchors** (Appendix, p. 360) and the shipped N = 274 (Study 2, p. 357), but publishes octant M/SD only for a *different* sample (Study 1 combined, p. 353), so the shipped M/SD have no identified source. Traces to `data/ipipipc.rda`, `data-raw/ipipipc.R`.
- [hopwood2011.md](hopwood2011.md) — Hopwood, Ansell, Pincus, Wright, Lukowitsky & Roche (2011), *The circumplex structure of interpersonal sensitivities* — the published source for every shipped ISC norm value, printed inside Figure 2 (p. 717) rather than in a table, for Study 1's derivation half (n = 649); the article prints no item list. Traces to `data/isc.rda`, `data-raw/isc.R`.
- [forward-source-prospects.md](forward-source-prospects.md) — Forward-looking source prospects (M46) — a supply-push survey synthesis note triaging four shelved sources (nagy2019, weide2021, rogoza2021, tracey2000), each mapped to a `candidate` ROADMAP row; traces to nothing yet — per-source pages owed only on graduation (D-024).

<!-- Entry format note: cairn_validate's _INDEX_LINE regex matches the first
     [\w./-]+\.md token after the bullet, so the link TEXT must be the
     filename ("[grassi2010.md](grassi2010.md)"). A bare citekey as link text
     ("[grassi2010](grassi2010.md)") is silently not counted as a catalog
     entry and the page reads as unindexed. Caught by mutation at M40 T5.

     Citekey trap: browne1992a.md is Browne & CUDECK (1992); browne1992.md
     is Browne ALONE. The `a` suffix marks a different author set, not a
     second work by one author. Both pages state this. (This note read
     "browne1992.md (owed, M42)" until M42 authored it; corrected in place
     2026-07-19.)

     Which sources still owe a page is the milestone files' ledger, not this
     file's. As of the M45 merge every source on the shelf that the repo
     relies on has a page — the Hubert & Arabie (1987) and Tracey (1997)
     RANDALL pair this note recorded as owed after M43 are discharged by M45
     (both PDFs were shelved 2026-07-20, clearing the "not on the shelf" gate),
     as the Acton & Revelle (2004) / Wendt (2019) and Browne (1992) / Browne
     (1982) entries were before them — observed 2026-07-20. The shelf is a
     live directory that changed twice during M40 alone, so treat any list
     here as a snapshot and re-inventory rather than trusting it.

     Forward-looking shelf sources (nagy2019, weide2021, rogoza2021,
     tracey2000) were added on purpose as future research material, not
     because anything cites them; per D-023 (mechanism superseded by D-024)
     they are captured as prospects by M46, NOT dismissed as "owes no page."
     M46 triaged all four into the ONE committed survey synthesis note listed
     above (forward-source-prospects.md) plus four candidate ROADMAP rows; a
     per-source <citekey>.md page is still owed only once a milestone comes to
     rely on one (D-024). So the survey note carries its INDEX line here; the
     four sources individually do not, until they graduate.

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
     (This block listed sources/cheung2002.pdf as owing no page — "no shipped
     code, vignette, or test cites it", the delta-CFI option "an unexercised
     offer" — from M41 until M57 exercised it. Corrected in place 2026-07-24:
     cheung2002 graduated to a committed page, listed above, and its entry is
     removed from this list rather than left readable.)

     Gurtman corpus — dispositioned at M47 while identifying the estimator's
     defining source (gurtman1998); each uncited by shipped code, none a
     standalone seedable prospect — all observed 2026-07-20:
     - sources/gurtman1991.pdf — Gurtman (1991), Evaluating the
       interpersonalness of personality scales; a scale-evaluation method the
       repo does not cite or implement. Consulted in passing.
     - sources/gurtman1993.pdf — Gurtman (1993), Constructing personality tests
       to meet a structural criterion; test-construction methodology, uncited.
     - sources/gurtman1994.pdf — Gurtman (1994), The circumplex as a tool…: a
       methodological primer. Its one relevant contribution — the "slight
       scaling correction" behind the vector-averaging estimator — is captured
       transitively by gurtman1998.md's citation of "Gurtman, 1994"; the repo
       cites 1994 nowhere directly. Consulted as another source's citation. -->
