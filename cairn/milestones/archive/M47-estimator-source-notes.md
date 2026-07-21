# M47: SSM estimator source notes (Wright 2009 + defining Gurtman)

**Status:** done (2026-07-21, PR #73 https://github.com/jmgirard/circumplex/pull/73)

**Goal:** Author `cairn/references/` source notes for the shelf sources the
package's closed-form SSM estimator relies on, banking each formula with anchors.

**Outcome:** Three pages (docs-only; INDEX +3). `wright2009.md` banks the
estimator (Eqs. 7–13) from the p.315 image, confirms `aw2009` = Wright's Table A
(all 40 cells), and records the Appendix printed typo (SS_Total term missing its
square) + unreconciled scalars. `gurtman1998.md` is the estimator's **defining
source** (Eqs. 1–2, p.349 — the SS_Total page Wright cites; vector-averaging
closed form, 2/p scaling, matching `test-ssm_sem_syntax.R:6`). `gurtman2003.md`
banks Eq. 16.7 and records that 2003 does **not** print the .80/.70 R² cutoffs
Wright attributes to it. gurtman 1991/1993/1994 → owes-no-page ledger.

**Decisions:** M47-D1 — both gurtman1998 (definer) and gurtman2003
(vignette-cited) get pages; the estimator-identity attribution proceeded on the
triangulated Opus finding (Wright's explicit p.349 citation + code match), Fable
offered on the no-oracle tripwire and declined.

**Review:** 3 lenses + scorer. F1 (92) fixed — all three pages had dropped the
M41-D1 not-a-human-attestation caveat; re-added, each Extraction status made one
physical line. F2 (78) fixed — M47-D1 no longer restates the .80/.70
misattribution. F3/F4 (45/45) logged. CI 7/7; validate green.
