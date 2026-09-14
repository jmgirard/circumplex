# M125: The circumplex-structure vignette reads as plain English

**Status:** done (2026-09-14, PR #158 https://github.com/jmgirard/circumplex/pull/158)

**Goal:** The vignette `evaluating-circumplex-structure` reads on one pass for an applied researcher, with every statistical claim kept.

**Outcome:** The page's prose is rewritten under the M123 rules, and each `.Rmd.orig` edit is copied by hand into the shipped `.Rmd`. Chunks and output are unchanged. The checklist's ipsatizing cross-reference now points to Section 5, and "(next section)" for the cutoffs now says subsection. `tests/testthat/test-cpm_boundary_vignette.R` pins phrases of this page, so a rewrite must keep them word for word. NEWS extends the plain-English bullet to six vignettes. Ledger rows are in `cairn/references/plain-vignettes.md` under M125.

**Decisions:** none.

**Review:** three-lens fan-out plus a fresh claims reader and one-read reader at the head. The claims reader listed 17 items with no dropped claim or lost qualifier. RK2 ("For example" added a relation) was fixed, and the rest were verified or matched ledger rows. The diff-bug lens listed 10. Five were fixed at the gate: the analytic scope of the large-N caution, the three parameter families, the "$f_a$" referent, "next subsection", and the ledger's "items not in a row" line. Two base doc bugs ("weak" label, `cpm_fit()` "commits to the theoretical angles") went to the doc-bug candidate row at the maintainer's choice. One was kept and two cosmetic ones rejected. The one-read reader listed 38 paragraphs, all rejected under grouped reasons at the maintainer's choice. Blame lens: nothing undone. Prior-review lens: no findings. The first CI wait hit the 590 s ceiling while R CMD check ran, and the resumed session merged on green. `vignette-precompute` passed on the PR head. The PR URL and AC3 evidence commits stayed unpushed, so the merged milestone file shows AC3 unticked, and this summary holds that evidence.
