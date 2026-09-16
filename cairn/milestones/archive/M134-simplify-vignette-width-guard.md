# M134: Simplify the vignette width guard

**Status:** done (2026-09-16, PR #167 https://github.com/jmgirard/circumplex/pull/167)

**Goal:** The vignette width guard holds its one exemption as a named list inside the checker, in place of region markers in the vignette source.

**Outcome:** `tools/check-vignette-width.R` holds an `EXEMPT` list of (vignette, pattern, reason) entries. Two entries exempt the `#> cx =~` and `#> cy =~` loading lines (86 columns) in `sem-based-ssm-analysis`. A pattern must start with `^`, and each entry applies only to its own vignette. An entry that matches no over-wide `#>` line stops the checker with an error that names it. `expand_tabs()` expands tabs to 8-column stops before `nchar(type = "width")` measures a line. The header limits its promise to `#>` lines. It names three output kinds the guard does not read: another knitr `comment` prefix, `results = "asis"`, and indented output. The `vignette-width:exempt` markers left `sem-based-ssm-analysis.Rmd.orig` and its render. The workflow comment in `vignette-precompute.yaml` was reworded. `tools/m132-planted-defects.R` was deleted, and five plants ran once instead, recorded in the milestone's work log.

**Decisions:** The plan simplified the guard instead of hardening it or deleting it. A one-off plant run replaced a committed plant script, because CI never ran the M132 script.

**Review:** One pass. All five criteria passed with fresh evidence, and the five plants re-ran. `devtools::check()` gave 0 errors, 0 warnings and 0 notes, and all six PR checks passed. A three-lens fan-out found 14 weaknesses, and none broke a criterion. O1 (an exempt line has no width cap) and O2 (an exemption covers its whole vignette) went to follow-up. At hygiene the width-guard candidate row was promoted for planning as "Bound the width guard's exemptions". The M132 byte-versus-width finding was refuted by a fresh Unicode plant. The other findings were rejected as pre-existing on master, planned, cosmetic, or latent with no instance today. No lesson added.
