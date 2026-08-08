# M74: Norms provenance audit, batch 3 (multi-sample instruments)

**Status:** done (2026-08-07, PR #101 https://github.com/jmgirard/circumplex/pull/101)

**Goal:** Verify the shipped norms of the four multi-norm-sample instruments
(cais, iei, igicr, iipsc — nine samples across five sources) against their
published sources.

**Outcome:** `shipped_values()` and the `audit_norms()` join key by
`(Sample, scale)`, and `AUDIT_BATCH` is an 18-row per-(instrument, sample) map
carrying citekey/divisor/scales, so iipsc's two samples join to two sources.
Source notes added for sodano2006, horner2024, trucco2013, hopwood2008 and
soldz1995. All 144 shipped means and SDs matched. Four corrections: cais's item
key rekeyed to sodano2006 Table 1's 5/5/5/4/5/3/5/5 blocks (7 of 8 octants
change, items 33–37 had been unscored), cais sample-1 `Size` 213→204, iipsc
sample-1 `Reference` 2011→2008, iei `URL` split per sample. An item-key
partition sweep over all fifteen bundled instruments is now test-enforced.

**Decisions:** M74-D1 — iei's norms `URL` points at the author's IEI norms page
rather than the article DOI or the OSF project, that page shelved as evidence.

**Review:** Two passes. Pass 1: 12 findings, F2 (92) returned it — NEWS said the
cais key changed three octants where seven change. Pass 2: 22 findings, F5 (82)
actioned, plus four sub-threshold record repairs. Extended LESSONS' stale-prose
line to its eighth recurrence.
