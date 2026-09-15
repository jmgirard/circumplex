# Generator for data/simulated_occasions.rda -----------------------------------
# Provenance: a SIMULATED three-wave dataset for the "Advanced Circumplex
# Visualization" vignette, where ssm_analyze_long() estimates one profile per
# wave and ssm_plot_trajectory() draws them. 200 persons answer the eight octant
# scales (PA .. NO at octants()) at waves T1, T2 and T3. Each wave's group
# profile is a cosine of amplitude 0.6 whose displacement is 330, 355 and then
# 20 degrees, so the profile rotates counterclockwise across the 0/360 degree
# boundary between T2 and T3. Each person has one offset added to all eight
# scales at every wave (normal, SD 0.5), and each score has its own noise
# (normal, SD 0.5). Seeded (12345). Regenerate from the package root with:
#   Rscript data-raw/simulated_occasions.R

library(circumplex)

angles <- as.numeric(octants())
n <- 200L
displacement <- c(T1 = 330, T2 = 355, T3 = 20)

set.seed(12345)
offset <- stats::rnorm(n, sd = 0.5)

make_wave <- function(wave) {
  signal <- 0.6 * cos((angles - displacement[[wave]]) * pi / 180)
  scores <- matrix(signal, n, length(angles), byrow = TRUE) + offset +
    matrix(stats::rnorm(n * length(angles), sd = 0.5), n)
  colnames(scores) <- PANO()
  data.frame(id = seq_len(n), wave = wave, scores)
}

simulated_occasions <- do.call(rbind, lapply(names(displacement), make_wave))
simulated_occasions$wave <- factor(simulated_occasions$wave, levels = names(displacement))
rownames(simulated_occasions) <- NULL

usethis::use_data(simulated_occasions, overwrite = TRUE)
