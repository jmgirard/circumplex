# M108: Build and validate a per-fit accuracy certificate

**Status:** done (2026-08-24, PR #138 https://github.com/jmgirard/circumplex/pull/138)

**Goal:** Build an a-posteriori per-fit accuracy certificate for the corrected component standard errors and the
scaling factor, validated against the exact-rational oracle, without changing what any exported function returns.

**Outcome:** `axes_accuracy_certificate()` (`R/axes_certificate.R`) replays the shipped pricing in vectorized double-double
arithmetic, returning the estimated relative error of the corrected component SE vector (worst component) and of the
scaling factor as `10 * max(delta/2, 2*eps)`, sentinel 1 ("no digits certified") on any route failure; n-free
by construction, 16-108x the cost of the double pricing. The shipped pre-square-root pricing split into
`axes_pricing_core()` / `axes_v_pricing()` / `axes_u_pricing()`, called by both paths, so the certificate prices what
ships. `exact_oracle.R` reads `tests/testthat/fixtures/`, emits the certificate beside each measured error and fails if
any of its 12 ratios leaves [1, 1e3]; the `cairn/reviews/` fixture duplicate and its byte-identity fence
`test-fixture-drift.R` are gone. Refusal rewiring is M111.

**Decisions:** D-051 (mechanism, sentinel contract, rejected alternatives). Milestone-local: the estimand and its scope;
a hand-derived dyadic-rational closed-form oracle as the second independent type; the planted-defect set; the cost envelope.

**Review:** Two rounds. Round 1 returned it on a master-watch red (cleared by the M90 backstop hotfix, PR #139) and on AC2
falsified on windows-latest, taking one gated amendment — the frozen error is a macOS measurement, so the packaged bracket
runs behind a bit-identity precondition. Round 2's three fresh-context reviewers returned 24 findings; three fixed at the
gate, each proved able to fail first (`dd_solve()` raising a condition where its contract promises the sentinel; a
shipped-pricing refusal skipping instead of reddening; counterexample B asserting nothing outside its gate), one declined,
two recorded without edit, the rest routed to follow-up — headed by the bracket asserting on macOS only, not on ubuntu or windows.
