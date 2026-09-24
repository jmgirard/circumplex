# The joint growth model on SSM coordinates as an engine's formulas

Return the package's fixed joint growth model in the formula pieces one
mixed-model engine takes. The model reads the long table
[`ssm_growth_data()`](http://circumplex.jmgirard.com/reference/ssm_growth_data.md)
builds: one intercept and one linear time slope per coordinate (`e`,
`x`, `y`), a person-level random intercept per coordinate with the three
intercepts free to correlate, and a separate residual variance per
coordinate. Printing the object shows the complete fit call for the
engine and, below it, the lines that keep the fixed effects and their
covariance from the fit, or the posterior draws for brms. Those are the
inputs of the recipe's next step, the trajectory.

## Usage

``` r
ssm_growth_formula(
  engine = c("glmmTMB", "nlme", "brms"),
  time = "wave",
  id = "person"
)

# S3 method for class 'circumplex_growth_formula'
print(x, ...)
```

## Arguments

- engine:

  Optional. One of `"glmmTMB"` (default), `"nlme"` or `"brms"`. The
  engine is named only; it is not loaded or called.

- time:

  Optional. The name of the time column in the long table (default
  `"wave"`), the same `time` given to
  [`ssm_growth_data()`](http://circumplex.jmgirard.com/reference/ssm_growth_data.md).

- id:

  Optional. The name of the person column in the long table (default
  `"person"`), the same `id` given to
  [`ssm_growth_data()`](http://circumplex.jmgirard.com/reference/ssm_growth_data.md).
  `time` and `id` must each be one non-empty name, different from each
  other and from `dv` and `value`, the columns the long table reserves.
  A name that is not syntactic, such as `"my wave"`, is backticked in
  the formulas, so every name reads as one column and never as formula
  syntax.

- x:

  An object of class `"circumplex_growth_formula"`.

- ...:

  Ignored (S3 consistency).

## Value

A list of class `"circumplex_growth_formula"` with attributes `engine`,
`time` and `id`. Its elements are formula objects, one per argument or
formula part the engine's fit call takes. For `"glmmTMB"`: `formula`,
`value ~ 0 + dv + dv:<time> + us(0 + dv | <id>)`, and `dispformula`,
`~ 0 + dv`. For `"nlme"`: `fixed`, `value ~ 0 + dv + dv:<time>`,
`random`, `~ 0 + dv | <id>`, and `weights`, `~ 1 | dv`, the form that
[`nlme::varIdent()`](https://rdrr.io/pkg/nlme/man/varIdent.html) takes.
For `"brms"`: `formula`, `value ~ 0 + dv + dv:<time> + (0 + dv | <id>)`,
and `sigma`, `sigma ~ 0 + dv`, the two parts of a `brms::bf()` call.

## Details

The model has no options. Adding a covariate, a quadratic time term or
another random-effects structure changes the model whose interval
coverage the package validated, so the pieces are built for the user to
paste and, if they choose, to edit by hand with that in mind. That
validation ran the frequentist REML fit. The brms dialect fits the same
likelihood under brms's default priors, and its posterior intervals were
not part of it. The nlme call sets `na.action = na.omit`, since
[`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) otherwise stops
on a row whose `value` is `NA`; glmmTMB and brms drop such rows by
default.

## See also

Other growth functions:
[`ssm_growth_data()`](http://circumplex.jmgirard.com/reference/ssm_growth_data.md)

## Examples

``` r
ssm_growth_formula("glmmTMB", time = "wave", id = "person")
#> Joint growth model on SSM coordinates, glmmTMB dialect.
#> Fit on the long table from ssm_growth_data(), then keep the fixed effects:
#> 
#> fit <- glmmTMB::glmmTMB(
#>   value ~ 0 + dv + dv:wave + us(0 + dv | person),
#>   dispformula = ~ 0 + dv,
#>   data = long,
#>   REML = TRUE
#> )
#> coef <- glmmTMB::fixef(fit)$cond
#> vcov <- as.matrix(vcov(fit)$cond)
ssm_growth_formula("nlme")
#> Joint growth model on SSM coordinates, nlme dialect.
#> Fit on the long table from ssm_growth_data(), then keep the fixed effects:
#> 
#> fit <- nlme::lme(
#>   fixed = value ~ 0 + dv + dv:wave,
#>   random = ~ 0 + dv | person,
#>   weights = nlme::varIdent(form = ~ 1 | dv),
#>   data = long,
#>   na.action = na.omit,
#>   method = "REML"
#> )
#> coef <- nlme::fixef(fit)
#> vcov <- as.matrix(vcov(fit))
ssm_growth_formula("brms")
#> Joint growth model on SSM coordinates, brms dialect.
#> Fit on the long table from ssm_growth_data(), then keep the draws:
#> 
#> fit <- brms::brm(
#>   brms::bf(
#>     value ~ 0 + dv + dv:wave + (0 + dv | person),
#>     sigma ~ 0 + dv
#>   ),
#>   data = long
#> )
#> draws <- as.matrix(fit)
```
