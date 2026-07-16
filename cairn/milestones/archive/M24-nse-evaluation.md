# M24: Tidyverse NSE in the user API — evaluation + standing decision (done 2026-07-16)

- **Goal:** decide on recorded evidence whether user-facing functions should
  support tidyverse-style NSE; record the outcome as a standing D-entry.
- **Outcome: NO — D-014 full rejection** (bare-name capture AND
  tidyselect-style helpers, via tidyselect Import, bare-rlang `enquo()`, or
  in-house parser), with a re-trigger clause (concrete user-facing evidence
  + `irreversible-api` RB gate to reopen). DESIGN.md Dependency policy
  gained the standard-evaluation-by-design doctrine line.
- **Deliverable:** `devel/m24-nse-evaluation.md` — four strata: 7-package
  prior-art survey (peer modeling packages are SE/formula); dependency
  delta (6 net-new Imports incl. D-006-refused vctrs; R floors neutral —
  effective floor already 4.1 via ggplot2/htmlTable); ergonomics on real
  vignette sites (`PANO()` beats the NSE form; `starts_with()` undermines
  `score()`'s ascending-order contract) + runnable ambiguity spikes
  (silent data-mask collision; `{{ }}` embracing requirement); back-compat
  vs the v1.0.0 NSE removal (NEWS.md:412–416), which the verdict confirms.
- **Review:** all 4 ACs passed with fresh evidence; check() 0/0/0; CI 7/7.
  One diff-bug finding (scored 84): the draft's "R-floor jump 3.4 → 4.1"
  claim was false by the memo's own method — corrected pre-merge in memo +
  D-014; verdict unaffected. Other lenses: zero findings.
- **PR:** https://github.com/jmgirard/circumplex/pull/48 (squash 0067ac9).
