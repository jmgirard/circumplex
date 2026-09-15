# Generator for data/simulated_growth.rda and data/simulated_growth_origin.rda -
# Provenance: two SIMULATED five-wave datasets for the "Growth Models on SSM
# Parameters" vignette. In both, 150 persons answer the eight octant scales
# (PA .. NO at octants()) at waves 0 to 4. A person's score on a scale at a wave
# is
#   0.5 + u_e + (x(t) + v_x) cos(angle) + (y(t) + v_y) sin(angle) + noise,
# with elevation 0.5, person effects u_e (SD 0.30), v_x and v_y (SD 0.15 each)
# that stay the same at every wave, and noise with SD 0.40. All draws are
# normal. The person effects are drawn once and used for both datasets, so the
# same persons appear in each. The group coordinates (x(t), y(t)) differ:
#   simulated_growth:        (x, y) moves linearly from the amplitude-0.6
#                            point at 350 degrees to the one at 10 degrees,
#                            so the displacement crosses the 0/360 boundary.
#   simulated_growth_origin: x moves linearly from 0.5 to -0.5 with y at 0.02,
#                            so the group passes near the origin at wave 2.
# Seeded (20260716). Regenerate from the package root with:
#   Rscript data-raw/simulated_growth.R

library(circumplex)

n <- 150L
waves <- 0:4
theta <- as.numeric(octants()) * pi / 180

set.seed(20260716)
u_e <- stats::rnorm(n, 0, 0.30)
v_x <- stats::rnorm(n, 0, 0.15)
v_y <- stats::rnorm(n, 0, 0.15)

simulate_waves <- function(x_t, y_t) {
  out <- do.call(rbind, lapply(seq_along(waves), function(k) {
    mu <- 0.5 + (x_t[[k]] + v_x) %o% cos(theta) + (y_t[[k]] + v_y) %o% sin(theta)
    scores <- mu + u_e + matrix(stats::rnorm(n * length(theta), 0, 0.40), n)
    colnames(scores) <- PANO()
    data.frame(person = seq_len(n), wave = waves[[k]], scores)
  }))
  rownames(out) <- NULL
  out
}

xy_start <- 0.6 * c(cos(350 * pi / 180), sin(350 * pi / 180))
xy_end <- 0.6 * c(cos(10 * pi / 180), sin(10 * pi / 180))
simulated_growth <- simulate_waves(
  x_t = seq(xy_start[[1]], xy_end[[1]], length.out = length(waves)),
  y_t = seq(xy_start[[2]], xy_end[[2]], length.out = length(waves))
)

simulated_growth_origin <- simulate_waves(
  x_t = seq(0.5, -0.5, length.out = length(waves)),
  y_t = rep(0.02, length(waves))
)

usethis::use_data(simulated_growth, simulated_growth_origin, overwrite = TRUE)
