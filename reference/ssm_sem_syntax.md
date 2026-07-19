# Generate lavaan syntax for a fixed-angle circumplex measurement model

Emit lavaan model syntax for a structural-equation-model formulation of
the Structural Summary Method, with the circumplex scale angles held
fixed at their theoretical values. This is the syntax-generation layer
of the SEM-based SSM (see the package's design notes); it is a pure
function of its arguments and does **not** require lavaan to be
installed (only fitting the emitted model does).

## Usage

``` r
ssm_sem_syntax(
  instrument = NULL,
  scales = NULL,
  angles = NULL,
  measures = NULL,
  model = c("scaled", "strict"),
  n_groups = 1,
  invariance = c("configural", "metric", "scalar", "strict_residuals"),
  include_defined = NULL
)
```

## Arguments

- instrument:

  Optional. A `circumplex_instrument` object; its scale abbreviations
  and angles are used. Supply this or both `scales` and `angles`.

- scales:

  Optional. A character vector of circumplex scale (column) names.
  Ignored when `instrument` is supplied.

- angles:

  Optional. A numeric vector of scale angles in degrees, the same length
  as `scales`. Ignored when `instrument` is supplied.

- measures:

  Optional. Either `NULL` or a character vector of external measure
  (column) names to relate to the circumplex factors.

- model:

  Optional. The measurement-model tier: `"scaled"` (default) or
  `"strict"`. See Details.

- n_groups:

  Optional. A single positive whole number: the number of groups for a
  multi-group model. `1` (default) emits the single-group model. When
  `n_groups >= 2` the generator emits multi-group syntax with a mean
  structure and the invariance constraints selected by `invariance`;
  fitting it requires
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) called with
  `group = ` a grouping variable whose number of levels equals
  `n_groups`. Group ordering follows the factor-level order of that
  variable: the **first** level is the reference group, whose factor
  metric (and, from the scalar rung, whose latent means) is fixed.

- invariance:

  Optional. The cross-group invariance rung to emit when
  `n_groups >= 2`: `"configural"` (default), `"metric"`, `"scalar"`, or
  `"strict_residuals"`. Must be left at its default when `n_groups == 1`
  (supplying it there is an error). Under the `"strict"` tier the
  `"metric"` rung is vacuous (all loadings are fixed) and emits the
  configural structure with an explanatory comment. See the package
  design notes (section 6.2) for the adapted fixed-angle invariance
  ladder.

- include_defined:

  Optional. (Single-group models only: for `n_groups >= 2` the lines are
  unavailable and an explicit `TRUE` is ignored with a warning.) Whether
  to append inspection-only `:=` definitions of the covariance-metric
  `(e, x, y)` coordinates for each measure. `NULL` (default) emits them
  automatically under the `"strict"` tier when at least one measure is
  present (there they are linear), and omits them otherwise. `TRUE`
  forces emission (an error under `"scaled"`, where they would be
  nonlinear); `FALSE` always suppresses them.

## Value

A single character string of lavaan model syntax, with attributes
`angles` (a `circumplex_degree` vector), `scales`, `model`, `weights`
(the 3-by-p OLS projection matrix, rows `e`, `x`, `y`), `n_groups`, and
(when `n_groups >= 2`) `invariance`.

## Details

Two model tiers are available. The `"scaled"` tier frees a general
saturation and a circumplex saturation per scale while fixing each
scale's angle; the circumplex plane is held isotropic and orthogonal,
and the general factor is held orthogonal to the plane (freeing the
general-plane covariances alongside free saturations makes the model
locally unidentified exactly at zero covariance, so they cannot be
estimated in this tier). The `"strict"` tier fixes every loading to the
unit cosine pattern `(1, cos(angle), sin(angle))` and frees the 3x3
factor covariance matrix – including the general-plane covariances,
making it the tier that can model a general factor leaning into the
plane. The angles enter only as evaluated cosine and sine constants; no
angle is ever a free parameter.

The returned string always carries a `weights` attribute: the
ordinary-least-squares projection matrix that maps a profile vector to
the structural summary coordinates `(e, x, y)`. For equally spaced
angles this equals the conventional closed-form estimator; for unequally
spaced angles it is the least-squares projection and can differ. The
emitted syntax never contains `:=` definitions for amplitude or
displacement: those are nonlinear functions whose confidence intervals
must be constructed in-package, not by lavaan's delta method (which
ignores the angular branch cut).

## See also

[`ssm_analyze()`](http://circumplex.jmgirard.com/reference/ssm_analyze.md)
for the observed-data SSM.

## Examples

``` r
# Octant instrument, default scaled tier
syn <- ssm_sem_syntax(scales = paste0("s", 1:8), angles = octants())
attr(syn, "weights")
#>             s1         s2            s3         s4            s5         s6
#> e 1.250000e-01  0.1250000  1.250000e-01  0.1250000  1.250000e-01  0.1250000
#> x 1.941169e-17 -0.1767767 -2.500000e-01 -0.1767767 -5.696675e-17  0.1767767
#> y 2.500000e-01  0.1767767  2.998201e-17 -0.1767767 -2.500000e-01 -0.1767767
#>              s7        s8
#> e  1.250000e-01 0.1250000
#> x  2.500000e-01 0.1767767
#> y -4.672039e-17 0.1767767
cat(syn)
#> # circumplex SSM measurement model (generated by ssm_sem_syntax())
#> # scales: s1, s2, s3, s4, s5, s6, s7, s8
#> # angles (degrees): 90, 135, 180, 225, 270, 315, 360, 45
#> # model tier: scaled
#> 
#> # general factor: free per-scale saturations
#> g =~ NA*s1 + a1*s1 + a2*s2 + a3*s3 + a4*s4 + a5*s5 + a6*s6 + a7*s7 + a8*s8
#> # circumplex plane: loadings free but with each scale's angle fixed
#> cx =~ NA*s1 + lx1*s1 + lx2*s2 + lx3*s3 + lx4*s4 + lx5*s5 + lx6*s6 + lx7*s7 + lx8*s8
#> cy =~ NA*s1 + ly1*s1 + ly2*s2 + ly3*s3 + ly4*s4 + ly5*s5 + ly6*s6 + ly7*s7 + ly8*s8
#> # fixed-angle direction constraints: sin(a)*lx - cos(a)*ly == 0
#> 0 == 1*lx1 - 0*ly1
#> 0 == 0.70710678118654757*lx2 - -0.70710678118654746*ly2
#> 0 == 0*lx3 - -1*ly3
#> 0 == -0.70710678118654746*lx4 - -0.70710678118654768*ly4
#> 0 == -1*lx5 - 0*ly5
#> 0 == -0.70710678118654768*lx6 - 0.70710678118654735*ly6
#> 0 == 0*lx7 - 1*ly7
#> 0 == 0.70710678118654746*lx8 - 0.70710678118654757*ly8
#> # isotropic orthonormal plane metric (plane scale absorbed by loadings)
#> g ~~ 1*g
#> cx ~~ 1*cx
#> cy ~~ 1*cy
#> cx ~~ 0*cy
#> # general-plane covariances fixed to zero: with free per-scale
#> # saturations, freeing these is locally unidentified exactly at
#> # phi_g = 0 (the trade a_i +/- d*c_i*cos/sin(angle_i) <-> phi_g is
#> # first-order flat there), so they cannot be estimated. To model a
#> # general factor leaning into the plane, use the strict tier, whose
#> # fixed loadings leave the full factor covariance matrix free.
#> g ~~ 0*cx
#> g ~~ 0*cy
#> 
#> # NOTE: amplitude (a) and displacement (d) are deliberately NOT defined
#> # here. They are nonlinear (sqrt / atan2) and their intervals must be
#> # built in-package through circular quantiles, never via lavaan := or
#> # delta-method CIs (which ignore the angular branch cut).
```
