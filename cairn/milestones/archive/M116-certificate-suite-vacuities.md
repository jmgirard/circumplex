# M116: Make three passing-without-checking assertions in the certificate suite redden

**Status:** done (2026-08-31, PR #147 https://github.com/jmgirard/circumplex/pull/147)

**Goal:** Three assertions in `tests/testthat/test-axes-certificate.R` that pass without
checking what they name are replaced by assertions that redden on the failure each was
written to catch.

**Outcome:** The bracket ceiling is a named `cert_ceiling <- 100` (was 1e3), the measured
ratio range 9.829339–10.0025192 recorded beside it; the closed-form sites'
`expect_identical(cert$…, floor_est)` identities became `cert_bracket()` calls fed by
errors measured against hand-derived exact values (97/128, 2, 5/8; `v_exact`), the
quotient `cval` identity deleted (no exact `u` committed); the shape test pins every
case's matrix `dim()`, counterexample B included; `cert_bracket()`'s upper bound floors
at `cert_floor`. The first return also removed the dyadic site's shipped-route
`axes_v_pricing()` exactness identities — retained at the plan gate, they pinned the
brackets' measured error to zero. Every repair proved able to fail by a planted defect.

**Decisions:** none promoted; the return gate's scope amendment superseded the plan-gate retention of the exactness identities (work-log lines carry it).

**Review:** Two rounds, three-lens fan-out each. First gate returned on F1 (brackets
vacuous under retained identities), F2/F4/F5 fixed in the return, F3 filed. Second gate:
five criteria fresh-verified; [O] R1 rejected as the certificate's contract (the ×50 plant
reds all four brackets); R9/R10/B1 fixed at the gate; R4/R5/R6 deferred to the degeneracy
row; rest rejected. One AC2 amendment return on its own track.
