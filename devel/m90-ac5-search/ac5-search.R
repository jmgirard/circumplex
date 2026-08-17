# M90 AC5 search: can a criterion-accepted matrix reach the cval <= 0 branch?
# Provenance: authored and run 2026-08-16 on the m90-degeneracy-reason-
# vocabulary branch (R 4.6.1, macOS); seed 20260816 below. Recorded result
# (M90 work log): 10,000 accepted draws per p in {3, 8, 24}, branch reached
# 0 times; min cval +1.2e-5 (p = 3, df = 1), +0.813 (p = 8), +0.956 (p = 24).
# Runtime ~6 minutes. Re-run from the repo root with:
#   Rscript devel/m90-ac5-search/ac5-search.R
#
# Reach detection, post-relabel (M90 review round 2): the recorded run
# happened before the backstop's relabel, when the branch's literal was
# "indefinite" and unambiguous. Since the relabel the branch says
# "ill_conditioned" -- but on a draw accept() has already cleared, the
# criterion cannot be the source of that literal, so on ACCEPTED draws
# "ill_conditioned" identifies the backstop exactly; "unidentified" is a
# distinct upstream door and is counted separately.
# Families per p: Wishart correlation draws at three concentration levels
# (conditioning spread), spectrum-surgery near-floor draws (lambda-ratio
# pinned just above the refusal floor), and an adversarial hill-climb that
# perturbs the minimum-cval draw toward smaller cval. A draw "reaches" the
# branch iff the criterion accepts it and axes_scaling_factor() still
# refuses with the backstop's literal (see the reach-detection note above).
suppressMessages(devtools::load_all(quiet = TRUE))
set.seed(20260816)

maps <- list(
  list(p = 3L,  ang = c(0, 0, 90), scl = c("A", "A", "B"), z1 = FALSE),
  list(p = 8L,  ang = as.numeric(octants()), scl = LETTERS[1:8], z1 = FALSE),
  list(p = 24L, ang = rep(as.numeric(octants()), each = 3L),
       scl = rep(LETTERS[1:8], each = 3L), z1 = TRUE)
)

for (m in maps) {
  p <- m$p
  d <- axes_se_derivs(m$ang, m$scl, NULL, m$z1, FALSE)
  q <- length(d$mats)
  df <- p * (p + 1) / 2 - q
  bdf <- p * (p - 1) / 2
  stopifnot(df > 0)
  nm <- sprintf("i%02d", seq_len(p))
  floor_ <- sqrt(p * .Machine$double.eps / axes_degeneracy_tau)

  sf <- function(S) {
    dimnames(S) <- list(nm, nm)
    suppressWarnings(axes_scaling_factor(S, nm, m$ang, m$scl,
                                         fit_zeta1 = m$z1, fit_zeta2 = FALSE,
                                         df = df, baseline_df = bdf))
  }
  accept <- function(S) is.null(axes_sigma_degenerate(stats::cov2cor(S)))

  draw <- function(kind) {
    if (kind <= 3L) {                       # Wishart at 3 concentrations
      dfw <- c(p + 1L, 2L * p, 10L * p)[kind]
      S <- stats::cov2cor(drop(stats::rWishart(1L, dfw, diag(p))))
    } else {                                 # spectrum surgery near the floor
      S0 <- stats::cov2cor(drop(stats::rWishart(1L, 2L * p, diag(p))))
      e <- eigen(S0, symmetric = TRUE)
      tgt <- runif(1L, 1.05, 3) * floor_     # ratio just above the floor
      v <- e$values - min(e$values)
      v <- v / max(v) * (1 - tgt) + tgt      # lambda in [tgt, 1]
      S <- e$vectors %*% diag(v) %*% t(e$vectors)
      S <- stats::cov2cor((S + t(S)) / 2)
    }
    S
  }

  n_acc <- 0L; n_reach <- 0L; min_cval <- Inf; min_S <- NULL; n_try <- 0L
  while (n_acc < 10000L && n_try < 60000L) {
    n_try <- n_try + 1L
    S <- draw(1L + (n_try %% 4L))
    if (!accept(S)) next
    n_acc <- n_acc + 1L
    r <- sf(S)
    if (is.null(r$reason)) {
      if (r$scale < min_cval) { min_cval <- r$scale; min_S <- S }
    } else if (r$reason == "ill_conditioned") {
      n_reach <- n_reach + 1L   # backstop: accept() already cleared the criterion
    }
  }

  # Adversarial hill-climb from the minimum-cval accepted draw.
  hc <- 0L
  S <- min_S
  for (i in seq_len(1000L)) {
    E <- matrix(rnorm(p * p, sd = 0.02), p, p); E <- (E + t(E)) / 2
    S2 <- stats::cov2cor(S + E)
    if (!isTRUE(accept(S2))) next
    r <- sf(S2)
    if (is.null(r$reason) && r$scale < min_cval) {
      min_cval <- r$scale; S <- S2; hc <- hc + 1L
    } else if (!is.null(r$reason) && r$reason == "ill_conditioned") {
      n_reach <- n_reach + 1L   # backstop (see above)
    }
  }
  cat(sprintf(
    "p=%2d df=%3d: tried %d, accepted %d, cval-branch reached %d, min cval %.6f (hill-climb accepted %d)\n",
    p, df, n_try, n_acc, n_reach, min_cval, hc))
}
