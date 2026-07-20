# M43: Source notes for the structure criteria and the validity source

**Status:** done · **PR:** [#69](https://github.com/jmgirard/circumplex/pull/69) (squash `920b76fd`) · **Depends on:** M40

**Goal/outcome.** Source notes for the criteria `R/fit_structure.R` implements and the
validity source the SEM vignette cites; documentation-only. Both shipped, closing the
M40–M43 series — every shelved source the repo relies on now has a committed page.

`acton2004.md` records **two departures, not a transcription.** (1) Eq. 6 as printed
defines the Fisher Test on communalities (`X_v = Σ_f φ_fv²`) while the p. 6 prose
describes vector lengths `√h²`; `structure_fisher()` ships the prose reading — both
recorded, neither resolved in the other's favour. (2) None of the 24 shipped cutoffs
come from the paper; all are re-derived at nv = 8, because **A&R announce an
nv = 8/16/32 follow-up twice (pp. 10, 18) and never report its results** (full-text
sweep). Gap/VT2/RT (eqs. 2, 8, 9) verified as printed; a third erratum found beyond the
two in `devel/ar2004-transcription.md`. `wendt2019.md` verifies all four vignette claims
(`:394` is if anything conservative) and finds one wrong range in
`devel/m5-wendt-discrepancies.md` §1, recorded not settled. **Decision:** RANDALL's
sources (Hubert & Arabie 1987; Tracey 1997) back shipped code but are unshelved — a
ROADMAP candidate, not a scope expansion.

**Review.** 6/6 criteria; `cairn_validate` 15/15, 3082 tests pass,
`check(--no-manual)` clean, CI 7 of 7 green. The diff-bug lens caught a **false provenance claim
in M43's own work** — the page said `pdftotext` drops the display equations; it does not
(`-layout` scatters them). Fixed in place, work-log superseded per IP4; LESSONS as case (e).