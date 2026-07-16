# M20: 0-vs-360 pole CI-endpoint alignment — done 2026-07-16

**Goal:** displacement-CI endpoints denoting the 0/360 pole report 360
(value-level), matching LM = 360 (D-003's parked cosmetic follow-up).

**Outcome:** `quantile.circumplex_radian` snaps pole-denoting endpoints
(both float representations, 16*eps ≈ 4-ulp-of-2π window) to 2π; flows
through `ssm_analyze()` bootstrap CIs and `cpm_fit()` bootstrap angle CIs.
Gated amendment (AC6/M20-D1): CPM's *reported* `Angle` also labels the pole
360 (a reference item at theory 360 printed `Angle = 0`, CI `[0, 0]`);
computational radians untouched, fitted matrices byte-identical. Consumer
audit: every `*_lci`/`*_uci` reader arithmetic-invariant to the relabel;
doc surfaces updated to [0, 360]/pole = 360 wording.

**Key decisions:** M20-D1 (snap reported degrees only). Review F1 (85,
fixed): snap window widened from 2*eps (thinner than 1 ulp of 2π) to
16*eps; estimate-path e2e assertion relaxed to G2's either-label rule.
F2 (72) / F3 (65) sub-threshold, logged; F2's roxygen sentence scoped.

**Evidence:** red-first teeth (6 pre-fix failures; re-proven at review via
in-memory reversion); suite 0 fail / 2095 pass; `devtools::check()` 0/0/0
post-fix; CI 7/7 green; boundary-coverage.md M20 cells.

**PR:** https://github.com/jmgirard/circumplex/pull/45 (squash 34e6704)
