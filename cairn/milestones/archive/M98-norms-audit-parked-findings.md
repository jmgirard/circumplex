# M98: Close the parked norms-audit findings by subtraction

**Status:** done (2026-08-21, PR #127 https://github.com/jmgirard/circumplex/pull/127)

**Goal:** Dispose of the four parked norms-audit findings so the apparatus ends smaller.

**Outcome:** `test-norms-audit-manifest.R`'s walked-site field-set assertion is
`expect_identical(sort(names(s), na.last = TRUE), c("binding", "key", "kind"))` —
sensitive to a repeated name (M88 F12) and to an `NA` name, both mutation-proved.
`NORMS_AUDIT_VERDICT` drops its unreachable `are not all TRUE` alternative, keeping
its grouping parentheses; the deletion fails closed, measured — R's
`c(TRUE, FALSE) are not all TRUE` leaves the verdict in the stem, so
`audit_key_matches()` refuses the site's own message. M88 F11 and M80 F1 declined
on the record; no package surface moved.

**Decisions:** D-045 — four dispositions, each with rationale and reopening class;
applies D-042 rather than superseding it, leaves D-043's identity unchanged, and
cites the M69/M70 CRAN-split lesson to state why its shape does not reach F11.

**Review:** three lenses, 11 findings, 6 actioned, 5 logged. F1 found the
replacement assertion *weaker* than what it replaced on one shape — `sort()`
discards `NA` names by default — fixed and re-proved. F2 found the constant's lost
grouping would make the restoration its own comment recommends fail open, reviving
the 2026-08-14 truncation-floor incident. F3–F6 corrected evidence and record
defects; F7 rejected.
