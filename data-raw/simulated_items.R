# Generator for data/simulated_items.rda ---------------------------------------
# Provenance: a SIMULATED item-level dataset for the axes_reliability() help
# example and tests (there is no public raw-data oracle for Strack, Jacobs &
# Grosse Holtforth, 2013). 500 respondents answer 32 items -- four items on each
# of the eight octant circumplex scales (PA .. NO at octants()) -- drawn from the
# paper's five-component population: a general factor, the two equal circumplex
# axes, one shared scale-specificity component, and free item error, with
#   xi1 (axes) = .18, xi2 (general) = .06, zeta1 (scale specificity) = .10,
# so each item's error variance is .66 and the axes reliability is about .78
# (Spearman-Brown at item_n = 16). Continuous latent responses are rescaled to a
# 1-7 Likert metric (mean 4, SD 1.3) and rounded, so the columns read like real
# item scores. Fully seeded (seed 486115, the paper's DOI suffix). Regenerate
# from the package root with:
#   Rscript data-raw/simulated_items.R

library(circumplex)

angles <- as.numeric(octants()) # PA=90, BC=135, ..., LM=360, NO=45
scale_abbrev <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
k <- 4L # items per scale
n <- 500L
xi1 <- .18
xi2 <- .06
zeta1 <- .10

item_scale <- rep(seq_along(angles), each = k)
item_angle <- rep(angles, each = k)
th <- item_angle * pi / 180
p <- length(item_angle)

# The exact population item-correlation matrix (spec section 2 / Figure 2).
sigma <- xi2 + xi1 * outer(th, th, function(a, b) cos(a - b)) +
  zeta1 * outer(item_scale, item_scale, `==`)
diag(sigma) <- 1

# Symmetric eigen square root (the package's mvn_root convention).
mvn_root <- function(s) {
  e <- eigen(s, symmetric = TRUE)
  e$vectors %*% (sqrt(pmax(e$values, 0)) * t(e$vectors))
}

set.seed(486115)
z <- matrix(stats::rnorm(n * p), nrow = n, ncol = p) %*% mvn_root(sigma)

# Rescale the standardized latent responses to a rounded 1-7 Likert metric.
likert <- round(4 + 1.3 * z)
likert[likert < 1] <- 1
likert[likert > 7] <- 7

simulated_items <- as.data.frame(likert)
colnames(simulated_items) <- unlist(lapply(scale_abbrev, function(s) {
  paste0(s, "_", seq_len(k))
}))

usethis::use_data(simulated_items, overwrite = TRUE)
