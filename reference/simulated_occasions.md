# Simulated octant scores at three occasions

A simulated dataset for demonstrating
[`ssm_analyze_long()`](http://circumplex.jmgirard.com/reference/ssm_analyze_long.md)
and
[`ssm_plot_trajectory()`](http://circumplex.jmgirard.com/reference/ssm_plot_trajectory.md).
The data are simulated, not collected from people. 200 persons have
scores on the eight octant scales at three waves. At each wave the group
profile is a cosine curve with elevation 0 and amplitude 0.6. Its
displacement is 330 degrees at `T1`, 355 degrees at `T2` and 20 degrees
at `T3`, so the profile crosses the 0/360 degree boundary between `T2`
and `T3`. Each person has one offset, added to all eight scales at every
wave, drawn from a normal distribution with standard deviation 0.5. Each
score also has its own noise, drawn from a normal distribution with
standard deviation 0.5.

## Usage

``` r
simulated_occasions
```

## Format

A data frame with 600 rows (one per person per wave) and 10 columns:

- id:

  Person identifier, 1 to 200.

- wave:

  Occasion, a factor with levels `T1`, `T2` and `T3` in that order.

- PA, BC, DE, FG, HI, JK, LM, NO:

  Scores on the eight octant scales, at the angles given by
  [`octants()`](http://circumplex.jmgirard.com/reference/octants.md).

## Details

The data are written by a seeded script,
`data-raw/simulated_occasions.R`, which is not in the installed package.
Read it on GitHub:
<https://github.com/jmgirard/circumplex/blob/master/data-raw/simulated_occasions.R>.
