# Package index

## Instrument Functions

Functions for importing and examining circumplex instruments

- [`instruments()`](http://circumplex.jmgirard.com/dev/reference/instruments.md)
  : List all available instruments
- [`scales()`](http://circumplex.jmgirard.com/dev/reference/scales.md) :
  Display the scales of a circumplex instrument
- [`anchors()`](http://circumplex.jmgirard.com/dev/reference/anchors.md)
  : Display the anchors of a circumplex instrument
- [`items()`](http://circumplex.jmgirard.com/dev/reference/items.md) :
  Display the items of a circumplex instrument
- [`norms()`](http://circumplex.jmgirard.com/dev/reference/norms.md) :
  Display the norms for a circumplex instrument

## Tidying Functions

Functions for preparing data for analysis

- [`ipsatize()`](http://circumplex.jmgirard.com/dev/reference/ipsatize.md)
  : Ipsatize circumplex items using deviation scoring across variables
- [`score()`](http://circumplex.jmgirard.com/dev/reference/score.md) :
  Score circumplex scales from item responses
- [`norm_standardize()`](http://circumplex.jmgirard.com/dev/reference/norm_standardize.md)
  : Standardize circumplex scales using normative data
- [`self_standardize()`](http://circumplex.jmgirard.com/dev/reference/self_standardize.md)
  : Standardize circumplex scales using sample data

## Primary SSM Functions

Functions for implementing the Structural Summary Method

- [`ssm_analyze()`](http://circumplex.jmgirard.com/dev/reference/ssm_analyze.md)
  : Perform analyses using the Structural Summary Method
- [`ssm_table()`](http://circumplex.jmgirard.com/dev/reference/ssm_table.md)
  : Create HTML table from SSM results or contrasts
- [`ssm_plot_circle()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_circle.md)
  : Create a Circular Plot of SSM Results
- [`ssm_plot_contrast()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_contrast.md)
  : Create a Difference Plot of SSM Contrast Results
- [`ssm_plot_curve()`](http://circumplex.jmgirard.com/dev/reference/ssm_plot_curve.md)
  : Create a Curve Plot of SSM Results
- [`ssm_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters.md)
  : Calculate Structural Summary Method parameters for a set of scores
- [`ssm_parameters_id()`](http://circumplex.jmgirard.com/dev/reference/ssm_parameters_id.md)
  : Calculate SSM parameters for each person
- [`summary(`*`<circumplex_ssm_id>`*`)`](http://circumplex.jmgirard.com/dev/reference/summary.circumplex_ssm_id.md)
  : Summarize per-person SSM parameters at the group level
- [`ssm_draws()`](http://circumplex.jmgirard.com/dev/reference/ssm_draws.md)
  : Summarize posterior draws as SSM parameters
- [`ssm_score()`](http://circumplex.jmgirard.com/dev/reference/ssm_score.md)
  : Calculate SSM parameters by row and add results as new columns

## SEM-Based SSM Functions

Latent-variable (disattenuated) SSM via structural equation modeling

- [`ssm_sem()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem.md)
  : Perform SEM-based (latent-variable) SSM analyses
- [`ssm_sem_parameters()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_parameters.md)
  : Calculate latent SSM parameters from a fitted lavaan measurement
  model
- [`ssm_sem_syntax()`](http://circumplex.jmgirard.com/dev/reference/ssm_sem_syntax.md)
  : Generate lavaan syntax for a fixed-angle circumplex measurement
  model

## Structure Evaluation Functions

Functions for evaluating circumplex structure and CI trustworthiness

- [`cpm_fit()`](http://circumplex.jmgirard.com/dev/reference/cpm_fit.md)
  : Fit Browne's circular stochastic process model (circumplex fit
  statistics)
- [`cpm_simulate()`](http://circumplex.jmgirard.com/dev/reference/cpm_simulate.md)
  : Simulate data from a fitted circular process model
- [`print(`*`<circumplex_cpm>`*`)`](http://circumplex.jmgirard.com/dev/reference/print.circumplex_cpm.md)
  : Print a circular process model fit
- [`summary(`*`<circumplex_cpm>`*`)`](http://circumplex.jmgirard.com/dev/reference/summary.circumplex_cpm.md)
  : Summarize a circular process model fit
- [`plot(`*`<circumplex_cpm>`*`)`](http://circumplex.jmgirard.com/dev/reference/plot.circumplex_cpm.md)
  : Plot a circular process model fit
- [`fit_structure()`](http://circumplex.jmgirard.com/dev/reference/fit_structure.md)
  : Evaluate circumplex structure (Acton & Revelle, 2004)
- [`print(`*`<circumplex_structure>`*`)`](http://circumplex.jmgirard.com/dev/reference/print.circumplex_structure.md)
  : Print circumplex-structure test results
- [`summary(`*`<circumplex_structure>`*`)`](http://circumplex.jmgirard.com/dev/reference/summary.circumplex_structure.md)
  : Summarize circumplex-structure test results
- [`plot(`*`<circumplex_structure>`*`)`](http://circumplex.jmgirard.com/dev/reference/plot.circumplex_structure.md)
  : Plot a circumplex-structure configuration
- [`ssm_ci_accuracy()`](http://circumplex.jmgirard.com/dev/reference/ssm_ci_accuracy.md)
  : Assess the accuracy of SSM confidence intervals by simulation
- [`summary(`*`<circumplex_ci_accuracy>`*`)`](http://circumplex.jmgirard.com/dev/reference/summary.circumplex_ci_accuracy.md)
  : Summarize the accuracy of SSM confidence intervals
- [`plot(`*`<circumplex_ci_accuracy>`*`)`](http://circumplex.jmgirard.com/dev/reference/plot.circumplex_ci_accuracy.md)
  : Plot SSM CI accuracy across the amplitude ladder

## Visualization Layer

Composable ggplot2 components for building circumplex figures

- [`ggcircumplex()`](http://circumplex.jmgirard.com/dev/reference/ggcircumplex.md)
  : Create a circumplex plotting canvas
- [`geom_ssm_point()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_point.md)
  : Draw SSM profile points in circumplex space
- [`geom_ssm_arc()`](http://circumplex.jmgirard.com/dev/reference/geom_ssm_arc.md)
  : Draw SSM confidence-region arcs in circumplex space
- [`scale_x_circumplex()`](http://circumplex.jmgirard.com/dev/reference/scale_x_circumplex.md)
  : Angle-labeled x-axis scale for circumplex plots

## Secondary SSM Functions

Functions for customizing SSM results

- [`html_render()`](http://circumplex.jmgirard.com/dev/reference/html_render.md)
  : Format and render data frame as HTML table

## Convenience Functions

Functions that make your life easier

- [`angle_unwrap()`](http://circumplex.jmgirard.com/dev/reference/angle_unwrap.md)
  : Unwrap a sequence of angles onto a continuous branch
- [`octants()`](http://circumplex.jmgirard.com/dev/reference/octants.md)
  : Angular displacements for octant circumplex scales
- [`poles()`](http://circumplex.jmgirard.com/dev/reference/poles.md) :
  Angular displacements for pole circumplex scales
- [`quadrants()`](http://circumplex.jmgirard.com/dev/reference/quadrants.md)
  : Angular displacements for quadrant circumplex scales
- [`PANO()`](http://circumplex.jmgirard.com/dev/reference/PANO.md) :
  Two-letter abbreviations for octant circumplex scales

## Example Data

- [`aw2009`](http://circumplex.jmgirard.com/dev/reference/aw2009.md) :
  Standardized octant scores on hypothetical circumplex scales
- [`jz2017`](http://circumplex.jmgirard.com/dev/reference/jz2017.md) :
  Raw octant scores on real circumplex scales with covariates
- [`raw_iipsc`](http://circumplex.jmgirard.com/dev/reference/raw_iipsc.md)
  : Raw item responses on real circumplex scales
