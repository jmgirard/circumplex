# M26: Longitudinal Build B — per-person layer + draws adapter + Bayesian vignette (done 2026-07-16)

- **Goal:** intraindividual layer + Bayesian draws adapter + precomputed brms
  vignette, per the binding D-013 spec (§§3, 5; Build B).
- **Outcome:** `ssm_parameters_id(data, scales, angles, id = NULL)` — per-person
  SSM table (within-person means by id, degenerate-NA semantics, `na_rate`
  column, reserved-name id guard) with `summary()` circular group statistics
  (circular mean + resultant length, NA-d strip counted). `ssm_draws(draws,
  angles, interval, type)` — two-shape adapter (parameter/profile draws;
  explicit `type` required in the ambiguous ncol = 3 cell), summarized through
  `ssm_replicate_intervals()` with honest posterior wording (new t0_warning /
  interval_label / structural_na args, byte-identical defaults); standalone
  `circumplex_ssm_draws` class (gate decision: not a `circumplex_ssm`
  subclass — additive to add later, breaking to retract).
  `bayesian-ssm-analysis.Rmd` precomputed (only `brm()` frozen; executable
  atan2 pin; Rayleigh exhibit); brms → Suggests (D-015); seeded fixture.
- **Oracles:** a run's bootstrap replicate matrix reproduces its intervals
  bit-exactly; shape B ≡ shape A; pole-straddle wrap (hand type-7 arithmetic);
  all-flat all-NA contract; exact-pole 360; §3.3 invariants (exact linearity,
  strict Jensen, differ-fixture anti-confusion regression).
- **Found en route:** R `%%` ≠ kernel modu() at tiny-negative angles (LESSONS);
  single-correction wrap shipped (pole parity, regression-tested).
- **Review:** 3-lens + scorer; F1 (id-name collision, 93) + F3 (pole-360 doc
  range, 82) fixed; F2/F4 (72/76, machine-eps corners) logged sub-threshold.
- **PR:** #50 (squash 0813d05); check 0/0/0; CI green on all platforms.
