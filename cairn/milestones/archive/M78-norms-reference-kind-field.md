# M78: Per-sample reference-kind field in the shipped norms

**Status:** done (2026-08-08, PR #106 https://github.com/jmgirard/circumplex/pull/106)

**Goal:** Every shipped normative sample carries a machine-readable classification of what kind of reference distribution it is, surfaced where users choose a sample and where they use one.

**Outcome:** `Norms[[2]]` gains a `Kind` column over all 15 instruments and 24 samples — `standardization` (6, the iip32/iip64 rows), `published` (16), `unsourced` (2, iis32 s1 and ipipipc s1). `norms()` prints a `Reference kind:` line per sample and `norm_standardize()` names the kind in its message and its `norm_sample` attribute, both through one `norm_kind_phrase()` helper in `R/utils.R` so the surfaces cannot drift. The assignment is recorded twice and diffed: a per-sample kind+basis table in `cairn/references/norms-audit.md` and a basis comment in each builder, compared by `data-raw/derive-norms-kind.R` (reads `data/*.rda`, exit 1 on disagreement). `?norms` defines the kinds; the vignette's counts read the column instead of `Population`/`Reference` text.

**Decisions:** none milestone-local. Implements D-041's chosen alternative to renaming the vocabulary; RR16 BC3 ingested verbatim, with one recorded deviation widening the middle kind past "identified-study participant pool".

**Review:** three lenses; prior-review and blame-history found nothing to action, diff lens found 19. Scorer actioned F6 (80, an AC6 failure — the test re-implemented `norm_sample_usable()` instead of calling it; one defect return) and F7 (85, `?norms` contradicted itself after M77's qualifier was deleted). F10/F11 fixed against the sub-80 rule as false vignette prose, logged. Measured 6/16/2/24 against RR16's projected 6/16/2/24, no shortfall.
