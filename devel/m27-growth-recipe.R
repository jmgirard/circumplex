# M27 T3: reference joint growth-model recipe on SSM displacement -------------
#
# Prototype for the growth-ssm-analysis vignette (spec sec. 4.1, as amended
# at the M27 gates: reference engine glmmTMB, nlme named as the base-R
# alternative; D-016). Since M151 to M153 the recipe runs on the package's
# growth helpers. Demonstrates the full pipeline:
#
#   per-person-per-wave (e, x, y) coordinates, stacked long
#     -> ONE joint mixed model on the stacked outcomes (glmmTMB), its call
#        from ssm_growth_formula()
#     -> ssm_trajectory(): MVN draws from the joint fixed-effect vcov (the MC
#        engine's own asymptotic move), per-t (e, x, y) draws, and
#        ssm_draws(type = "parameters") at each t
#     -> circular-correct a(t), d(t) summaries + D-007 certification per t
#
# HARD REQUIREMENT (spec sec. 4.1 / RR06 R4): the model is fit *jointly* on
# (x, y). Two univariate LMMs have independent vcovs, zero Cov(x_hat, y_hat),
# and produce wrong d(t) intervals; the M27 coverage oracle includes a cell
# where that shortcut demonstrably fails (devel/m27-coverage-oracle.R).
# ssm_trajectory() refuses a vcov whose implied Cov(x(t), y(t)) is exactly
# zero at every time, which is that shortcut's signature.
#
# Run: Rscript devel/m27-growth-recipe.R   (requires glmmTMB; ~seconds)

# Load the source tree (the installed release may predate the helpers)
devtools::load_all(".", quiet = TRUE)
stopifnot(requireNamespace("glmmTMB", quietly = TRUE))

set.seed(20260716)

# --- 1. Simulate person-level coordinate trajectories -------------------------
# Same family the model fits (well-specified): per-dv fixed intercept + slope,
# correlated person random intercepts across (e, x, y), independent residuals.
# The coordinates are simulated directly, so the long table is built here
# rather than by ssm_growth_data(), which scores scale scores; the columns
# are the ones that helper produces (person, wave, dv, value).
n_person <- 120
waves <- 0:4

# True fixed effects: displacement drifting from ~65 deg toward ~35 deg with
# amplitude ~0.55-0.75 (comfortably certified at every wave)
b0 <- c(e = 0.60, x = 0.25, y = 0.55) # intercepts at t = 0
b1 <- c(e = 0.02, x = 0.08, y = -0.03) # slopes per wave

# Person random-intercept covariance (correlated across dvs -- the joint
# structure the univariate shortcut throws away)
sd_u <- c(e = 0.25, x = 0.20, y = 0.20)
R_u <- rbind(
  c(1.0, 0.3, 0.2),
  c(0.3, 1.0, 0.5),
  c(0.2, 0.5, 1.0)
)
Sigma_u <- diag(sd_u) %*% R_u %*% diag(sd_u)
sd_resid <- c(e = 0.30, x = 0.25, y = 0.25)

mvn <- function(R, mu, sigma) {
  # Eigen-root MVN sampler (same convention as the package's MC engine)
  eig <- eigen(sigma, symmetric = TRUE)
  root <- eig$vectors %*% (sqrt(pmax(eig$values, 0)) * t(eig$vectors))
  sweep(matrix(rnorm(R * length(mu)), nrow = R) %*% root, 2, mu, "+")
}

u <- mvn(n_person, c(0, 0, 0), Sigma_u)
dat <- expand.grid(person = seq_len(n_person), wave = waves)
dvs <- c("e", "x", "y")
long <- do.call(rbind, lapply(seq_along(dvs), function(j) {
  dv <- dvs[j]
  data.frame(
    person = dat$person,
    wave = dat$wave,
    dv = dv,
    value = b0[dv] + b1[dv] * dat$wave + u[dat$person, j] +
      rnorm(nrow(dat), 0, sd_resid[dv])
  )
}))
long$dv <- factor(long$dv, levels = dvs)
long$person <- factor(long$person)

# --- 2. ONE joint mixed model on the stacked outcomes -------------------------
# The fit call is the one ssm_growth_formula() prints for glmmTMB:
# 0 + dv + dv:wave = per-outcome intercepts and slopes;
# us(0 + dv | person) = correlated person intercepts across outcomes;
# dispformula = ~ 0 + dv = per-outcome residual variances.
f_glmmTMB <- ssm_growth_formula("glmmTMB", time = "wave", id = "person")
print(f_glmmTMB)
fit <- glmmTMB::glmmTMB(
  f_glmmTMB$formula,
  dispformula = f_glmmTMB$dispformula,
  data = long,
  REML = TRUE
)

coef <- glmmTMB::fixef(fit)$cond
vcov <- as.matrix(vcov(fit)$cond)
stopifnot(identical(names(coef), colnames(vcov)))

# The x/y cross block of vcov (intercept and slope terms) is not structurally
# zero in a joint fit. Independent univariate fits assembled block by block
# put exact zeros throughout it. ssm_trajectory() below refuses that
# structure itself, and says nothing about whether the joint model is right.
cat("x/y cross block of vcov (intercept and slope terms):\n")
print(vcov[c("dvx", "dvx:wave"), c("dvy", "dvy:wave")])

# --- 3. Fixed effects -> per-t (e, x, y) draws -> SSM summaries --------------
# ssm_trajectory() draws 4000 coefficient vectors from MVN(coef, vcov),
# forms mu_dv(t) = b_dv + b_dv:wave * t for each draw and each t, and hands
# each t's draws to ssm_draws(type = "parameters").
trajectory <- ssm_trajectory(coef, vcov, times = waves, n_draws = 4000)
print(trajectory, digits = 3)

# Truth for eyeballing: d(t) of the true fixed-effect trajectory
d_true <- (atan2(b1["y"] * waves + b0["y"], b1["x"] * waves + b0["x"]) *
             180 / pi) %% 360
cat("true d(t):", format(d_true, digits = 4), "\n")

# Sanity: every wave certified in this comfortably-away-from-origin design,
# and every true d(t) inside its credible interval (loose smoke check, not
# the oracle -- coverage is measured properly in devel/m27-coverage-oracle.R)
stopifnot(all(trajectory$certified))
inside <- d_true >= trajectory$d_lci & d_true <= trajectory$d_uci
cat("true d(t) inside CrI:", all(inside), "\n")
