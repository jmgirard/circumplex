# Generator for vignettes/growth_bootstrap_draws.rds --------------------------
# Provenance: parametric-bootstrap replicates of the fixed effects for Section 7
# of the "Growth Models on SSM Parameters" vignette. The fit is the one
# ssm_growth_formula("glmmTMB") prints, on the long table that
# ssm_growth_data() builds from simulated_growth, by REML as the vignette fits
# it. Each replicate simulates a new response vector from the fitted model
# (random effects and residuals both redrawn) and refits the model to it; the
# replicate is that refit's fixed effects. One thousand refits take about three
# minutes, so the vignette reads this file rather than running the loop.
# Regenerate from scratch (from the package root, requires glmmTMB) with:
#   Rscript data-raw/growth-bootstrap-draws.R
# Seeded: set.seed(20260716), the growth vignette's seed, runs before the
# unseeded simulate() batches. A refit
# that errors, or whose fixed effects are not all finite, is dropped and
# replaced: the loop simulates in batches until 1000 replicates are kept, and
# the count dropped is printed and recorded in the provenance attribute.

library(circumplex)
data("simulated_growth")

long <- ssm_growth_data(
  simulated_growth,
  scales = PANO(),
  id = "person",
  time = "wave"
)
f_glmmTMB <- ssm_growth_formula("glmmTMB", time = "wave", id = "person")

fit <- glmmTMB::glmmTMB(
  f_glmmTMB$formula,
  dispformula = f_glmmTMB$dispformula,
  data = long,
  REML = TRUE
)
coef <- glmmTMB::fixef(fit)$cond

n_keep <- 1000L
seed <- 20260716L
set.seed(seed)
kept <- matrix(NA_real_, nrow = 0, ncol = length(coef),
               dimnames = list(NULL, names(coef)))
n_dropped <- 0L
while (nrow(kept) < n_keep) {
  # simulate() with no seed argument reads the session RNG, so the batches
  # follow one seeded stream.
  ys <- simulate(fit, nsim = n_keep - nrow(kept))
  for (y in ys) {
    b <- tryCatch(
      glmmTMB::fixef(glmmTMB::refit(fit, y))$cond,
      error = function(e) NULL
    )
    if (is.null(b) || !all(is.finite(b)) ||
        !identical(names(b), names(coef))) {
      n_dropped <- n_dropped + 1L
      next
    }
    kept <- rbind(kept, b)
  }
}
draws <- kept[seq_len(n_keep), , drop = FALSE]
rownames(draws) <- NULL
stopifnot(nrow(draws) == n_keep, !anyNA(draws),
          identical(colnames(draws), names(coef)))

attr(draws, "provenance") <- list(
  generator = "data-raw/growth-bootstrap-draws.R",
  data = "simulated_growth",
  seed = seed,
  replicates = n_keep,
  dropped = n_dropped,
  glmmTMB = as.character(utils::packageVersion("glmmTMB")),
  R = R.version.string
)
saveRDS(draws, file.path("vignettes", "growth_bootstrap_draws.rds"),
        compress = "xz")
cat("Wrote vignettes/growth_bootstrap_draws.rds:", nrow(draws),
    "replicates x", ncol(draws), "columns;", n_dropped, "refits dropped\n")
