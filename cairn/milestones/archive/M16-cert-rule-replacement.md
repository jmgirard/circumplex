# M16: Print-independent, scale-free displacement-certification rule — done 2026-07-12

- **Outcome:** replaced the v1.2.0 guardrail `round(a_lci, digits) > 0`
  (print- and scale-dependent; certified a truly-zero amplitude ~100% of the
  time) with the scale-free rule `ssm_certified(a_lci, a_uci, k = 0.35) =
  is.finite(r) & r >= k`, `r = a_lci/(a_uci − a_lci)`. Some near-zero-amplitude
  profiles previously certified are now correctly flagged uninterpretable.
  PR #40 (squash `cd0c140`); CI green all platforms; `check()` 0/0/0.
- **Form decided by Fable review** (RB03→RR03) seeded by `ssm_ci_accuracy()`'s
  own false-certification output → **D-007**. A lower-bound ratio is
  asymptotically pivotal at zero amplitude (Rayleigh null), so `k = 0.35` (the
  ~97.5% null point at the 95% interval) holds false-cert near α/2.
- **Verified (AC4):** two-part gate (false-cert@c=0 ≤ 0.05 AND Wilson-LCI
  Caution off) passes at reps=1000 across 5 configs (measured 0.008–0.023);
  closed-form Rayleigh oracle (0.0039) as the 2nd oracle type
  (`devel/m16-cert-rule-{seed,verify}.*`).
- **Also:** removed the vestigial `digits` arg + `Threshold` column from
  `ssm_ci_accuracy()` (unreleased → clean removal, not deprecation); rewrote the
  print note, guardrail/verdict wording, and both vignettes; k pinned to the 95%
  interval (a `k(interval)` generalization deferred).
- **Review:** 3 lenses + scorer. F1 (88) fixed — dropped NHST framing ("not
  reliably distinguishable from zero") from the print note. F2 (20, k/interval
  coupling) logged, not actioned (deferred by D-007). Contrasts stay ungated
  (M15-D1); D-003 pole reporting unaffected.
