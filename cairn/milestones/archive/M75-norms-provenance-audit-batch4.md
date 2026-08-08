# M75: Norms provenance audit, batch 4 (IIP family)

**Status:** done (2026-08-07, PR #102 https://github.com/jmgirard/circumplex/pull/102)

**Goal:** Verify the shipped `iip32` and `iip64` norms against the IIP
professional manual and record their provenance, closing the norms-audit family.

**Outcome:** All 96 shipped M/SD reconcile exactly against `horowitz2003` —
IIP-64 Table 4.4 (pp. 27–29) at divisor 8, IIP-32 Table F.5 (p. 91) at divisor 4
— as do both item maps, the six Sizes and the six Reference credits; no value
changed, and all fifteen instruments now carry a verdict. `parse_source_note()`
gained instrument-tagged blocks (one source, two instruments) plus a block-level
coverage sweep; both help pages cite the 2003 3rd edition and carry the Mind
Garden credit line; `Population` now names the national standardization sample.

**Decisions:** None milestone-local. Two gated amendments sit in the work log:
AC2 rewritten from an Appendix F T-table recovery to an ordinary comparison once
Table F.5 was found, and AC1/AC2 rescoped to the fields the manual publishes.

**Review:** Three lenses plus a scorer, 22 findings. Two actioned — an unclaimed
source-note block was invisible to the coverage sweep (88), NEWS overstated the
audit's reach (80) — plus three sub-threshold defects in new content; 14 logged,
six absorbed into the `data-raw/audit-norms.R` robustness candidate row. Two
lessons captured; the M18 consumers-semantics and M20 pole-float-label lines
pruned as stalest to fit the cap (D-015), neither retired by coverage.
