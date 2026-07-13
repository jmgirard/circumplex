# M16 T5 verification — the scale-free rule meets AC4's two-part gate

Evidence for M16 AC4 (D-007). Generator: `devel/m16-cert-rule-verify.R`
(reproducible; `set.seed(2026)` per config). Data: `devel/m16-cert-rule-verify.rds`.
`ssm_ci_accuracy(reps = 1000)` measures the **new** rule automatically (it calls
`ssm_certified()`; single-definition doctrine).

## Two-part acceptance gate (D-007 / RR03 Q5)

1. **Hard gate:** false-certification at the `c = 0` ladder rung ≤ 0.05 (point).
2. **Caution gate:** the diagnostic's Wilson-LCI `Caution` must not fire at `c = 0`.

| Config | metric / n | false-cert @ c=0 | Wilson LCI | Caution | Gate 1 | Gate 2 |
|---|---|---|---|---|---|---|
| COR_healthy | correlation, n=1166 | 0.012 | 0.007 | off | ✓ | ✓ |
| COR_nearzero | correlation, n=1166 | 0.011 | 0.006 | off | ✓ | ✓ |
| RAW_means | raw, n=1166 | 0.008 | 0.004 | off | ✓ | ✓ |
| RAW_smalln | raw, n=100 | 0.013 | 0.008 | off | ✓ | ✓ |
| COR_smalln | correlation, n=100 | 0.023 | 0.015 | off | ✓ | ✓ |

All five ≤ 0.05 (and below the α/2 = 0.025 benchmark); Caution never fires.
**Both gates PASS.** The superseded rule sat at `Cert_rate = 1.000` on every one
of these configs.

## Power curve (certification rate by ladder rung `c`)

- COR_healthy: c1=1.00, c0.5=1.00, c0.25=0.56 — a genuine signal certifies.
- RAW_means: c1=1.00, c0.5=1.00, c0.25=1.00.
- RAW_smalln: c1=1.00, c0.5=0.88, c0.25=0.24.
- COR_smalln: c1=0.75, c0.5=0.14, c0.25=0.04.
- **COR_nearzero: c1=0.02** — the OCPD noise fit (amplitude ≈ 0.012) is now
  correctly *refused* at the as-estimated condition. This is the intended
  user-visible behavior change (a fit the old rule certified is no longer
  certified), covered by the `test-ssm_oop.R` regression test and NEWS.

## Second oracle (validation doctrine ≥2 types)

The simulation-coverage measurements above are cross-checked against an
independent **closed-form** oracle: under the isotropic zero-amplitude null the
statistic's tail gives false-cert ≈ `exp(−t*²/2)` with `t* ≈ z·(1 + 2k)`. At
k = 0.35, 95% CIs: `t* = 1.96·1.70 = 3.332`, so `exp(−3.332²/2) = 0.0039`. The
measured rates (0.008–0.023) sit *above* this idealized floor exactly as RR03
predicted — bootstrap bias and finite-n fatten the tail — and within RR03's
stated 0.007–0.025 band. Two independent oracle types agree in magnitude and
direction.
