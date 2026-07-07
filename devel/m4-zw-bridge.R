# m4-zw-bridge.R -- the O5 bridge (spec devel/m4-ci-accuracy-spec.md sec. 10):
# run ssm_ci_accuracy() at transcribed Zimmermann & Wright (2017) Study 3
# conditions and compare its coverage estimates to the published record.
#
# Transcription provenance and the documented re-scope of this gate:
# devel/m4-zw-transcription.md. In brief: the article has no supplemental
# materials, so per-condition coverage values were never published; the
# comparison targets are therefore the published accurate/inaccurate
# classifications (Bradley band [.925, .975]) and the Eq. 3 accuracy
# frontier |AFF_min| = 2.95 * f_a * n^(-0.587) (p. 12), evaluated at
# conditions chosen safely away from the frontier. The generating process
# is MVN (p. 5-6), so the simulator-compatibility gate passes.
#
# Design (all transcribed, Study 3, pp. 11-13): octant matrices from
# Note 3 (p. 18); elevation and dominance fixed to 0; goodness of fit
# fixed to 1; affiliation = amplitude; 95% percentile bootstrap CIs with
# 2,000 replicates (Study 2, p. 9-10 -- also this package's defaults).
# The target profile peaks at LM, i.e. this package's 0/360 pole.
#
# Each condition seeds its own data construction and diagnostic run, so
# results are scheduling-independent and rerun to the stored values.
# Runtime: ~10 min serial. Output: devel/m4-zw-bridge-results.rds.

devtools::load_all(".", quiet = TRUE)

scales <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
angles <- as.numeric(octants()) # degrees, LM = 360 (Z&W's LM = 0; same pole)

# Note 3 (p. 18): model-based circulant parameters rho_1..rho_4
rho_gf <- c(.683, .500, .345, .288)   # with general factor (IIP-C)
rho_nf <- c(.430, .030, -.360, -.740) # without general factor (IAS)

# Eq. A7 (p. 18): amplitude scaling factor of an octant matrix
f_a <- function(rho) sqrt(sqrt(2) * (rho[1] - rho[3]) + (1 - rho[4])) / 2

# Eq. 3 (p. 12): minimum population affiliation for accurate a/delta CIs
aff_min <- function(rho, n) 2.95 * f_a(rho) * n^(-0.587)

zw_octant_R <- function(rho) {
  p <- length(angles)
  R <- diag(p)
  for (i in seq_len(p)) {
    for (j in seq_len(p)) {
      if (i == j) next
      d <- abs(angles[i] %% 360 - angles[j] %% 360)
      R[i, j] <- rho[round(min(d, 360 - d) / 45)]
    }
  }
  dimnames(R) <- list(scales, scales)
  R
}

# Joint population: octant circulant plus a target row on the exact cosine
# curve r_i = E + AFF*cos(theta_i) + DOM*sin(theta_i) (Eq. 1 / appendix)
zw_population <- function(rho, E, AFF, DOM) {
  R <- zw_octant_R(rho)
  th <- angles * pi / 180
  rt <- E + AFF * cos(th) + DOM * sin(th)
  J <- rbind(cbind(R, TGT = rt), TGT = c(rt, 1))
  rownames(J) <- colnames(J)
  J
}

# Data whose *sample* correlation matrix equals J exactly, so that
# ssm_ci_accuracy(structure = "observed") simulates from exactly the Z&W
# population (and the c = 1 ladder truth is exactly the Z&W condition)
exact_cor_data <- function(J, n) {
  p <- ncol(J)
  Z <- matrix(rnorm(n * p), n, p)
  Z <- scale(Z, center = TRUE, scale = FALSE)
  S <- crossprod(Z) / (n - 1)
  X <- Z %*% solve(chol(S)) %*% chol(J)
  colnames(X) <- colnames(J)
  as.data.frame(X)
}

conditions <- data.frame(
  id        = c("gf_aff18", "gf_aff05", "nf_aff28", "nf_aff05"),
  matrix    = c("gf", "gf", "nf", "nf"),
  AFF       = c(.18, .05, .28, .05),
  n         = 100,
  # Published classification: accurate iff AFF > AFF_min(n) (Eq. 3);
  # all four sit well away from the frontier (ratios 1.67, .46, 1.68, .30)
  published = c("accurate", "inaccurate", "accurate", "inaccurate")
)

reps <- 5000 # matches Z&W's own 5,000 samples per condition (p. 6)

results <- list()
for (i in seq_len(nrow(conditions))) {
  cc <- conditions[i, ]
  rho <- if (cc$matrix == "gf") rho_gf else rho_nf
  J <- zw_population(rho, E = 0, AFF = cc$AFF, DOM = 0)

  # Machinery asserts (not oracle values): population is valid and the
  # constructed sample reproduces it exactly
  stopifnot(min(eigen(J, symmetric = TRUE, only.values = TRUE)$values) > 0)
  set.seed(2000 + i)
  df <- exact_cor_data(J, n = cc$n)
  stopifnot(max(abs(cor(df) - J)) < 1e-12)

  set.seed(3000 + i)
  fit <- ssm_analyze(df, scales = scales, angles = angles, measures = "TGT")
  est <- fit$results
  stopifnot(abs(est$a_est - cc$AFF) < 1e-12,
            abs(est$e_est) < 1e-12,
            # circular distance to the 360/0 pole (atan2 of a ~1e-17
            # y-component can wrap the report to 360 - eps)
            abs(((est$d_est - 360 + 180) %% 360) - 180) < 1e-6)

  set.seed(4000 + i)
  acc <- ssm_ci_accuracy(fit, reps = reps, amplitude_factors = 1,
                         structure = "observed")

  am <- aff_min(rho, cc$n)
  results[[cc$id]] <- list(
    condition = cc, aff_min = am,
    coverage = acc$coverage, guardrail = acc$guardrail,
    verdict = acc$verdict, population = acc$population,
    details = acc$details[c("elapsed", "conditions")]
  )
  cat(sprintf("[%s] AFF = %.2f (frontier %.3f, published %s)\n",
              cc$id, cc$AFF, am, cc$published))
  print(acc$verdict, digits = 3)
}

saveRDS(results, "devel/m4-zw-bridge-results.rds")

# Bridge check: at the as-estimated (c = 1) rung, the diagnostic's Bradley
# classification of amplitude and conditional displacement must match the
# published classification. Three-way rule: any "inadequate" -> inaccurate;
# all "adequate" -> accurate (Study 3 judged a and delta jointly, p. 11);
# a "borderline" class (the Wilson interval straddles a band edge) or an NA
# class (no certified replicates) is consistent with either published
# classification -- reported as CONSISTENT and flagged rather than forced
# to MATCH/MISMATCH. Elevation must be band-consistent everywhere
# (accurate from n >= 50, Study 2 p. 10).
cat("\n== Bridge summary ==\n")
for (id in names(results)) {
  r <- results[[id]]
  v <- r$verdict[r$verdict$Parameter %in% c("e", "a", "d_conditional"), ]
  ad <- v$Class[v$Parameter != "e"]
  ours <- if (any(ad == "inadequate", na.rm = TRUE)) {
    "inaccurate"
  } else if (!anyNA(ad) && all(ad == "adequate")) {
    "accurate"
  } else {
    "borderline"
  }
  status <- if (ours == "borderline") {
    "CONSISTENT (borderline -- rerun with more reps)"
  } else if (ours == r$condition$published) "MATCH" else "MISMATCH"
  cat(sprintf("%s: published %-10s ours %-10s (e %s, a %s, d|cert %s) %s\n",
              id, r$condition$published, ours,
              v$Class[v$Parameter == "e"],
              v$Class[v$Parameter == "a"],
              v$Class[v$Parameter == "d_conditional"],
              status))
}
