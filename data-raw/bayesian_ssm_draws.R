# Generator for vignettes/bayesian_ssm_draws.rds -------------------------------
# Provenance: posterior fixed-effect draws for the "Bayesian SSM Analysis"
# vignette. brms cannot run on CRAN builders, so the vignette is precomputed:
# this script fits the model once locally and commits the resulting draws;
# the vignette's live chunks only read the rds. Regenerate from scratch (from
# the package root, requires brms with a working Stan backend) with:
#   Rscript data-raw/bayesian_ssm_draws.R
# Seeded end to end (subsample seed and sampler seed both 12345).

library(circumplex)
data("jz2017")

# A seeded subsample keeps the committed fixture and the one-time fit light;
# the vignette states this choice.
set.seed(12345)
n_sub <- 200
sub <- jz2017[sample(nrow(jz2017), n_sub), ]

scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
theta <- as.numeric(octants()) * pi / 180

# Long format: one row per person-scale observation, column-major stacking
# (all persons' PA, then BC, ...) matched by the id/cos/sin repetitions.
dat <- data.frame(
  id = rep(seq_len(n_sub), times = length(scales)),
  cos_theta = rep(cos(theta), each = n_sub),
  sin_theta = rep(sin(theta), each = n_sub),
  score = unlist(sub[scales], use.names = FALSE)
)

# Random-intercept cosine regression: fixed effects (Intercept, cos_theta,
# sin_theta) are the group-level (e, x, y); the random intercept absorbs
# within-person dependence across a person's eight scale scores. The
# normal(0, 1) prior on the (x, y) coefficients is the one whose induced
# amplitude prior (Rayleigh) the vignette exhibits.
fit <- brms::brm(
  score ~ cos_theta + sin_theta + (1 | id),
  data = dat,
  prior = brms::set_prior("normal(0, 1)", class = "b"),
  chains = 4,
  iter = 2000,
  cores = 4,
  seed = 12345,
  refresh = 0
)

draws <- as.matrix(fit,
                   variable = c("b_Intercept", "b_cos_theta", "b_sin_theta"))
attr(draws, "provenance") <- paste(
  "data-raw/bayesian_ssm_draws.R (seeds 12345/12345) on jz2017 (n = 200",
  "subsample); brms", as.character(utils::packageVersion("brms"))
)
saveRDS(draws, file.path("vignettes", "bayesian_ssm_draws.rds"),
        compress = "xz")
cat("Wrote vignettes/bayesian_ssm_draws.rds:",
    nrow(draws), "draws x", ncol(draws), "columns\n")
