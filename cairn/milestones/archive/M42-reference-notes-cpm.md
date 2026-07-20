# M42: Source notes for the CPM model and its communality CIs

- **Status:** done · **PR:** https://github.com/jmgirard/circumplex/pull/68 (squash `c27c74ed`)

**Goal.** Source notes for Browne (1992) — the full CPM model specification — and for the Browne (1982) communality-CI derivation the CPM oracle path relies on.

**Outcome.** Two committed pages, documentation only, no package file changed. `browne1992.md` (409 lines) carries the model `cpm_fit()` estimates — eqs. 1–8, the six §5.2 Requirements, all of §6 (eqs. 30–48) — plus a parameter map, five errata printed in the paper, and three departures the code takes from it (§6.7's start recipe unimplemented, the m cap, variant C). `browne1982.md` (214 lines) scopes pp. 95–96 to what they state.

**The finding that mattered.** Browne (1982) pp. 95–96 contain **no** communality-CI derivation — they state a general transform-and-invert CI method and never mention communalities, vᵢᵢ, or the circumplex. The chain has three links and only the first is Browne's: (1.6.38)–(1.6.40) → `browne1992.md` eq. (4) → Grassi's reporting choice. `grassi2010.md`'s attribution was wrong and is corrected in place; the arithmetic was not. Also found: Browne's Table 11 (p. 494) prints the m = 1 estimates digit for digit as Grassi's Table 2, so §8 independently corroborates the oracle.

**Verified, not asserted.** The engine's `zeta` is Browne's ζ*ᵢᵢ (agreement to 2.22e-16 against eq. 3); both scaling families reproduce eq. (6) across all 10 legal (p, m) pairs. No numeric departure.

**Decisions.** M42-D1: on an OCR-scanned source the page image is authoritative and `pdftotext` is not an independent witness (refines M41-D1, supersedes nothing).

**Review.** 6 findings from the diff-bug lens, 0 from blame-history, 0 from prior-PR-comments. F1 (95, ζ CIs mischaracterized as symmetric when the default path is percentile bootstrap) and F4 (90, anchor on comment lines) actioned; F3 (75) and F5 (70) fixed anyway; F2 (40) rejected with reason; F6 (28) led to the Table 11 discovery. `check(--no-manual)` 0/0/0, `test()` 3082 passing, CI 7/7.

**Open.** No value on either page has been read by a human — both channels are machine reads of one OCR scan. Browne (1982)'s citation is transcribed from Grassi's reference list, not the source. `R/cpm_fit.R` still implements published equations with no per-line citation (the M41 finding, still unaddressed).
