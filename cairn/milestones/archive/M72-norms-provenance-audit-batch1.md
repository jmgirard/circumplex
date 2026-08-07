# M72: Norms provenance audit, batch 1 (CSI family + IITC)

**Status:** done (2026-08-06, PR #98 https://github.com/jmgirard/circumplex/pull/98)

**Goal:** Re-verify every shipped norm value, angle, and item map for csie, csig, csip, csiv and iitc against their primary published sources.

**Outcome:** No shipped M, SD or sample size was wrong; none changed. Two provenance
records were: csiv's `Reference` credited Locke (2000), which publishes no octant
statistics and a different sample, and the csie/csiv `URL`s pointed at retired
uidaho.edu paths. Both corrected; `?csiv`/`?csie` now separate the instrument's
article from the norms table, and `?norms` discloses that `Population` is a
package-standardized label broader than each source's own. Ships
`data-raw/audit-norms.R` (parses each source note's machine-readable table, compares
angles mod 360, cross-checks the two angle copies, enforces the IP2 LM=360 convention
the modulo comparison cannot see), five `references/<citekey>.md` notes with anchored
value tables plus note-only rows, `norms-audit.md` (15-instrument status, sha256 shelf
manifest), a dispositioned pre/post-fix ledger pair with coverage report, and
`test-norms-provenance.R` pinning both objects whole.

**Decisions:** D-039 (printed provenance correction is factual, not a GP4 break).

**Review:** Two rounds. Round 1 failed AC2/AC4/AC5/AC6 plus 12 findings, closed by
T7–T12. Round 2 actioned 2 of 39 — verdicts omitting `Population` (82), and csig's
single-channel Figure 2 read (80), resolved by a 400-dpi plus OCR re-read confirming
every value. AC5 took an amendment return: it named `instruments()`, which returns `NULL`.
