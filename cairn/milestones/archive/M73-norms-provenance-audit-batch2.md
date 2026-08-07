# M73: Norms provenance audit, batch 2 (single-sample instruments)

**Status:** done (2026-08-07, PR #100 https://github.com/jmgirard/circumplex/pull/100)

**Goal:** Verify the shipped norms of the four single-norm-sample instruments (iis32, iis64, ipipipc,
isc) against their published sources, and repair the four M72 audit defects needing no multi-sample support.

**Outcome:** iis64 and isc match their sources exactly (hatcher2009 Table 1 p. 558; hopwood2011 Figure 2
p. 717). iis32 and ipipipc ship octant M/SD that **no identified source publishes** — hatcher2012 prints no
descriptive statistics at all, markey2009 only Study 1's combined sample against a shipped Study 2 n = 274 —
so both now ship "Norms source unconfirmed" in `Reference` and `?instrument`, under a new
`source-not-identified` disposition kind separating an unfindable venue from a field the source does not
publish. Four item texts corrected against their own appendices; three `.rda` rebuilt. Also ships the
anchored-`Reference` rule (each Reference row quotes a page anchor or carries a `constructed-credit` token
that `audit_norms()` emits as an exempt coverage row), `stamp_ledger()` with the zero-row crash fixed and
fenced, `AUDIT_BATCH` at nine instruments, four source notes, the test pins bound two-directionally to
`norms-audit.md`'s verdicts, and published iitc/iei citations replacing "(in press)".

**Decisions:** none milestone-local; D-039 covers the provenance-only corrections.

**Review:** Three rounds. R1 failed AC6 (a verdict omitting compared fields) and took an amendment return on
AC5, whose "page printing that credit" test no journal article can satisfy. R2 failed AC5 again on anchor
accuracy — a false constructed-credit reason and two anchors citing an invisible text-layer production code
as a printed running head. R3 verified all eight; 2 of 18 findings actioned and fixed at the gate.
