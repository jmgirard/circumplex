# M52: Trim brms/Stan from CI dependency installs

**Status:** done (2026-07-21, PR #78 https://github.com/jmgirard/circumplex/pull/78)

**Goal:** Exclude brms and its Stan toolchain from both CI workflows' dependency
installs, cutting per-run install cost with no loss of check/coverage surface.

**Outcome:** Both CI workflows switch `setup-r-dependencies` from whole-Suggests
resolution (`needs: check`/`coverage`) to `dependencies: '"hard"'` + an explicit
allowlist = DESCRIPTION Suggests minus brms (check adds rcmdcheck; coverage adds
xml2), with `_R_CHECK_FORCE_SUGGESTS_: false` on the check job so the absent brms
doesn't error `R CMD check`. The Stan stack (brms/rstan/StanHeaders/RcppParallel/
BH) is no longer installed; brms stays a declared Suggest (D-015), glmmTMB/lavaan/
OpenMx kept. Check `Status: OK` 0/0/0, all 8 vignettes build, coverage unchanged
(codecov pass), install 60s->41s (modest: pak uses RSPM binaries, <1min of ~12min).

**Decisions:** none — no dependency change (DESCRIPTION Suggests untouched).

**Review:** three lenses (diff-bug/blame-history/prior-review) zero findings;
none reached the scorer. Allowlist verified = Suggests minus brms; M51's
concurrency/paths-ignore/matrix preserved. Graduated to LESSONS: force-suggests
false makes allowlist/DESCRIPTION drift fail silently.
