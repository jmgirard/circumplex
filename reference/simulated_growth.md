# Simulated octant scores for growth models

Two simulated datasets for demonstrating growth models on SSM parameters
with
[`ssm_growth_data()`](http://circumplex.jmgirard.com/reference/ssm_growth_data.md),
[`ssm_growth_formula()`](http://circumplex.jmgirard.com/reference/ssm_growth_formula.md)
and
[`ssm_trajectory()`](http://circumplex.jmgirard.com/reference/ssm_trajectory.md).
The data are simulated, not collected from people. In both datasets, 150
persons have scores on the eight octant scales at waves 0 to 4.

## Usage

``` r
simulated_growth

simulated_growth_origin
```

## Format

Each is a data frame with 750 rows (one per person per wave) and 10
columns:

- person:

  Person identifier, 1 to 150.

- wave:

  Wave number, an integer from 0 to 4.

- PA, BC, DE, FG, HI, JK, LM, NO:

  Scores on the eight octant scales, at the angles given by
  [`octants()`](http://circumplex.jmgirard.com/reference/octants.md).

An object of class `data.frame` with 750 rows and 10 columns.

## Details

A person's score on a scale at a wave is the elevation 0.5, plus a
person elevation effect, plus \\(x + v_x)\cos\theta + (y +
v_y)\sin\theta\\, plus noise. Here \\\theta\\ is the scale angle and
\\(x, y)\\ is the group coordinate at that wave. The person elevation
effect has standard deviation 0.30, the person effects \\v_x\\ and
\\v_y\\ have standard deviation 0.15, and the noise has standard
deviation 0.40. All of them are drawn from normal distributions. The
person effects stay the same at every wave, and the two datasets share
them, so each person has the same effects in both.

The two datasets differ in the group coordinates:

- In `simulated_growth`, \\(x, y)\\ moves in a straight line, in equal
  steps, from the point with amplitude 0.6 at 350 degrees to the point
  with amplitude 0.6 at 10 degrees. The displacement crosses the 0/360
  degree boundary.

- In `simulated_growth_origin`, \\x\\ moves in equal steps from 0.5 to
  -0.5, and \\y\\ stays at 0.02. The group passes near the origin at
  wave 2.

Both datasets are written by one seeded script,
`data-raw/simulated_growth.R`, which is not in the installed package.
Read it on GitHub:
<https://github.com/jmgirard/circumplex/blob/master/data-raw/simulated_growth.R>.

## See also

[`vignette("growth-ssm-analysis")`](http://circumplex.jmgirard.com/articles/growth-ssm-analysis.md),
which fits a growth model to both datasets.
