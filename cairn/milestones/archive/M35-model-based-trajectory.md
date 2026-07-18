# M35: Model-based trajectory plotting (`ssm_draws()` tables)

**Status:** done · **PR:** [#58](https://github.com/jmgirard/circumplex/pull/58) · merged 2026-07-18 · depends on M33

**Goal.** Extend M33's trajectory plot to model-based trajectories from
`ssm_draws()` over continuous time; retire the growth vignette's hand-rolled figure.

**Outcome.** `ssm_plot_trajectory()` is now an S3 generic (`circumplex_ssm`,
`data.frame`, `default`). The `data.frame` method takes a *trajectory table* — one
row per time point, a numeric time column named by the new `time` argument,
required `a_*`/`d_*` triples, optional `e_*`/`x_*`/`y_*` and a logical `certified`
column — on a continuous axis, drawing only the panels it can fill; absent
`certified`, no interpretability claim is made. `ssm_trajectory_long()` is the one
shared implementation of the unwrap, interval placement, and certification carry:
M33's test file stayed byte-identical as the fence. Vignette Section 4 is now one
call (inline `rel()`/`%% 360` gone); a new Section 5 figure surfaces `certified`.

**Decisions.** M35-D1 generic over constructor (settled at the gate; the
`irreversible-api` tripwire did not escalate). M35-D2 base `stop()` over
`cli::cli_abort()` — `cli` is not an Import, so it needs its own dependency gate.

**Review.** 3 findings, all reproduced; 2 fixed with red-first regression tests —
a non-finite estimate silently blanking the rest of the series (93), a time column
naming a parameter column clobbered not refused (94). Below threshold (76): mixed
per-row `NA` in `certified` drops that marker. Suite 2886 passes, `check()` OK.
