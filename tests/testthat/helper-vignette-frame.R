# The vignette level map and reading order, defined once. test-vignette-frame.R
# reads them here (testthat sources every helper-*.R before the tests), and
# tools/check-pkgdown-vignettes.R sources this file to diff _pkgdown.yml
# against the same map. A new page is added here and in _pkgdown.yml together.

# Each page's level. Within a level the order is the reading order the pkgdown
# articles index follows.
frame_levels <- c(
  "using-instruments" = "Introductory",
  "introduction-to-ssm-analysis" = "Introductory",
  "intermediate-ssm-analysis" = "Intermediate",
  "evaluating-circumplex-structure" = "Intermediate",
  "ci-accuracy" = "Intermediate",
  "structure-tests" = "Intermediate",
  "cpm-boundary-fits" = "Advanced",
  "advanced-visualization" = "Advanced",
  "sem-based-ssm-analysis" = "Advanced",
  "sem-latent-contrasts" = "Advanced",
  "axes-reliability" = "Advanced",
  "axes-reliability-caveats" = "Advanced",
  "bayesian-ssm-analysis" = "Advanced",
  "growth-ssm-analysis" = "Advanced"
)

# Pages that teach a method the package proposes with no peer-reviewed source.
# Each opens with a "Not yet peer reviewed" notice under its Level paragraph,
# and no other page carries one (test-vignette-frame.R).
frame_notice <- c(
  "sem-based-ssm-analysis",
  "sem-latent-contrasts",
  "bayesian-ssm-analysis",
  "growth-ssm-analysis"
)

# Reading order: each row is one page and the page that follows it.
frame_next <- rbind(
  c("using-instruments", "introduction-to-ssm-analysis"),
  c("introduction-to-ssm-analysis", "intermediate-ssm-analysis"),
  c("intermediate-ssm-analysis", "evaluating-circumplex-structure"),
  c("evaluating-circumplex-structure", "cpm-boundary-fits"),
  c("evaluating-circumplex-structure", "ci-accuracy"),
  c("ci-accuracy", "structure-tests"),
  c("structure-tests", "advanced-visualization"),
  c("structure-tests", "sem-based-ssm-analysis"),
  c("sem-based-ssm-analysis", "sem-latent-contrasts"),
  c("sem-latent-contrasts", "axes-reliability"),
  c("axes-reliability", "axes-reliability-caveats"),
  c("intermediate-ssm-analysis", "bayesian-ssm-analysis"),
  c("bayesian-ssm-analysis", "growth-ssm-analysis")
)
