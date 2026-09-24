# Generator for vignettes/growth_brms_draws.rds ------------------------------
# Provenance: posterior fixed-effect draws for the brms section of the "Growth
# Models on SSM Parameters" vignette. brms cannot run on CRAN builders, so the
# vignette's brm() chunk is eval = FALSE and its draws step reads this file.
# The fit is the one ssm_growth_formula("brms") prints, on the long table that
# ssm_growth_data() builds from simulated_growth. Regenerate from scratch
# (from the package root, requires brms with a working Stan backend) with:
#   Rscript data-raw/growth-brms-draws.R
# Seeded: the sampler seed is 20260716, the growth vignette's seed.

library(circumplex)
data("simulated_growth")

long <- ssm_growth_data(
  simulated_growth,
  scales = PANO(),
  id = "person",
  time = "wave"
)
f_brms <- ssm_growth_formula("brms", time = "wave", id = "person")

# The call the vignette shows, with the sampler settings the vignette states.
fit <- brms::brm(
  brms::bf(f_brms$formula, f_brms$sigma),
  data = long,
  chains = 4,
  iter = 2000,
  cores = 4,
  seed = 20260716,
  refresh = 0
)

# Only the six fixed-effect columns ship: the trajectory helper reads those
# and ignores the sd_, cor_, b_sigma_ and lp__ columns a full as.matrix(fit)
# carries, so the file holds nothing the page does not use. The names are
# selected exactly, since a "^b_" pattern would also catch the three
# b_sigma_ columns of the dispersion model.
b_cols <- c("b_dve", "b_dvx", "b_dvy", "b_dve:wave", "b_dvx:wave", "b_dvy:wave")
all_draws <- as.matrix(fit)
missing_cols <- setdiff(b_cols, colnames(all_draws))
if (length(missing_cols)) {
  stop("fit has no column ", paste(missing_cols, collapse = ", "),
       "; its columns are ", paste(colnames(all_draws), collapse = ", "))
}
draws <- all_draws[, b_cols, drop = FALSE]
attr(draws, "provenance") <- paste(
  "data-raw/growth-brms-draws.R (seed 20260716) on simulated_growth;",
  "brms", as.character(utils::packageVersion("brms"))
)
saveRDS(draws, file.path("vignettes", "growth_brms_draws.rds"),
        compress = "xz")
cat("Wrote vignettes/growth_brms_draws.rds:",
    nrow(draws), "draws x", ncol(draws), "columns\n")
