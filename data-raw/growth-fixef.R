# Generator for tests/testthat/fixtures/growth-fixef.rds ----------------------
# Provenance: the glmmTMB fit of the package's joint growth model on the two
# bundled growth datasets, `simulated_growth` and `simulated_growth_origin`.
# The tests read the fixture so that they need no glmmTMB: the nlme parity
# test compares its own fit to these fixed effects, and M152's trajectory
# tests start from them. Regenerate from scratch (from the package root,
# requires glmmTMB) with:
#   Rscript data-raw/growth-fixef.R
# The fit is deterministic given the data: glmmTMB draws no random start,
# and the fixture regenerates identically under any seed (measured
# 2026-09-24, glmmTMB 1.1.15). set.seed(20260716), the growth vignette's
# seed, is kept so that a stochastic step added here later is pinned.

devtools::load_all(".", quiet = TRUE)
stopifnot(requireNamespace("glmmTMB", quietly = TRUE))

gf <- ssm_growth_formula("glmmTMB", time = "wave", id = "person")

fit_one <- function(dataset) {
  set.seed(20260716)
  long <- ssm_growth_data(get(dataset), scales = PANO(),
                          id = "person", time = "wave")
  fit <- glmmTMB::glmmTMB(
    gf$formula,
    dispformula = gf$dispformula,
    data = long,
    REML = TRUE
  )
  list(
    coef = glmmTMB::fixef(fit)$cond,
    vcov = as.matrix(vcov(fit)$cond),
    logLik = as.numeric(stats::logLik(fit))
  )
}

data("simulated_growth")
data("simulated_growth_origin")
fixture <- list(
  simulated_growth = fit_one("simulated_growth"),
  simulated_growth_origin = fit_one("simulated_growth_origin"),
  glmmTMB_version = as.character(utils::packageVersion("glmmTMB")),
  TMB_version = as.character(utils::packageVersion("TMB")),
  provenance = paste(
    "data-raw/growth-fixef.R (seed 20260716, unused by the deterministic",
    "fit) on simulated_growth and",
    "simulated_growth_origin; joint model from ssm_growth_formula('glmmTMB')",
    "fit by REML; glmmTMB", as.character(utils::packageVersion("glmmTMB")),
    "with TMB", as.character(utils::packageVersion("TMB"))
  )
)

out <- file.path("tests", "testthat", "fixtures", "growth-fixef.rds")
saveRDS(fixture, out, version = 2)
cat("Wrote", out, "with glmmTMB", fixture$glmmTMB_version, "\n")
print(fixture$simulated_growth$coef)
print(fixture$simulated_growth_origin$coef)
