# M163: Small-sample coverage in the trajectory docs

**Status:** done (2026-10-19, PR #197 https://github.com/jmgirard/circumplex/pull/197)

**Goal:** The growth vignette, the `ssm_trajectory()` help page and NEWS.md state what the M161 oracle measured about the shipped intervals at 10 to 50 persons.

**Outcome:** Section 7 of `vignettes/growth-ssm-analysis.Rmd.orig` (and the rendered `.Rmd`) now names both oracles' design: the `ssm_growth_formula()` model by glmmTMB REML, five balanced waves, M27 at 200 persons and 500 samples, and M161 at 10 to 200 persons and 1000 samples under the crossing (`pole`) and correlated (`xycor`) truths. It reports R0 mean coverage among converged fits of .938 to .944 at 20 to 50 persons, which moves by under .001 when non-PD fits' draws are added. It reports non-PD shares of 27.4%/17.7% at 10 persons and 3.2%/1.0% at 20, and at 10 persons .906/.903 from non-PD fits' draws and .939/.920 from converged fits, with a caveat that the converged figures run high. It also says the helper refuses an invalid `vcov` and that the package ships no correction. The Section 4 and Section 6 coverage results name their 200-person sample. The `ssm_trajectory()` roxygen (and Rd) states the 20-to-50 range with its basis and the 10-person shares. The dev NEWS entry on Section 7 names the new figures. No code changed.

**Decisions:** none. The plan gate dropped M162 (see its archive), stated figures only with no minimum-N advice, and kept the peer-review row parked. D-066 holds.

**Review:** One review return (AC1: Section 7 did not name the `ssm_growth_formula()` model), then fixed. The pre-review claim audit read 38 claims and corrected 3, the main one being that the oracle drew non-PD fits through `draws =`, bypassing the helper's `vcov` checks. The four-lens fan-out found 24 findings: 17 fixed on the branch (selection caveat at 10 persons, definition of converged, averaging basis, truth labels, wording), 7 rejected (3 false, 2 planned no-threshold choice, 1 all-replicate premise, 1 style), and 0 follow-up. Local check and PR CI green on six checks. Hygiene extended the M90 lesson; no row unparked.
