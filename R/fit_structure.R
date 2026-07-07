# Shared infrastructure for the circumplex structure tests of Acton & Revelle
# (2004). Every one of their exploratory criteria (Fisher, Gap, Variance, and
# Rotation tests) operates on the first two *unrotated principal-axis* factors
# of the scales' correlation matrix (A&R p. 13), so the extraction lives here
# once rather than in each test.

# Principal-axis factor extraction of the first two factors --------------------
# Iterated principal-axis factoring (PAF): communalities are initialized from
# the squared multiple correlations (SMCs), then refined by repeatedly placing
# the current communalities on the diagonal of the correlation matrix, taking
# the leading two eigenpairs as loadings, and updating the communalities from
# those loadings until they stabilize. No rotation is applied -- the A&R
# criteria are defined on the unrotated solution. Replaces the drafts' single
# `psych::fa(nfactors = 2, rotate = "none", fm = "pa")` call so `psych` is only
# a Suggests-level test oracle.
paf2 <- function(r, max_iter = 100L, tol = 1e-4) {
  p <- ncol(r)
  # SMC starting communalities: 1 - 1/diag(R^-1). A singular R (e.g. deviation-
  # scored scales, whose correlation matrix is rank-deficient) has no inverse;
  # fall back to a unit (identity) start, then let ridge repair supply a proper
  # matrix upstream.
  smc <- tryCatch(1 - 1 / diag(solve(r)), error = function(e) rep(1, p))
  smc[smc < 0] <- 0
  smc[smc > 1] <- 1
  comm <- smc

  loadings <- matrix(0, nrow = p, ncol = 2)
  for (i in seq_len(max_iter)) {
    reduced <- r
    diag(reduced) <- comm
    e <- eigen(reduced, symmetric = TRUE)
    vals <- e$values[1:2]
    # A non-positive-definite reduced matrix can yield negative leading
    # eigenvalues; clip to zero so sqrt() is defined (a zero-variance factor
    # simply contributes nothing to the loadings).
    vals[vals < 0] <- 0
    loadings <- e$vectors[, 1:2, drop = FALSE] %*% diag(sqrt(vals), 2, 2)
    new_comm <- rowSums(loadings^2)
    # Cap Heywood cases at the theoretical communality maximum of one.
    new_comm[new_comm > 1] <- 1
    if (max(abs(new_comm - comm)) < tol) {
      comm <- new_comm
      break
    }
    comm <- new_comm
  }

  # Factor orientation is arbitrary; sign each factor so its loadings sum to a
  # positive number (the psych::fa convention) for a reproducible solution.
  signs <- sign(colSums(loadings))
  signs[signs == 0] <- 1
  loadings <- loadings %*% diag(signs, 2, 2)
  colnames(loadings) <- c("PA1", "PA2")
  rownames(loadings) <- rownames(r)
  loadings
}

# Two-factor loadings for a set of circumplex scales ---------------------------
# Selects the scales from `data`, forms their correlation matrix, optionally
# repairs a non-positive-definite matrix with a ridge, and returns the first two
# unrotated principal-axis factors. Extraction is *always* principal axis: the
# A&R thresholds were calibrated under PA, so (unlike the drafts) ridge does not
# switch the estimator to maximum likelihood -- it is an orthogonal correlation-
# matrix repair only.
structure_loadings <- function(data, scales, ridge = 0) {
  stopifnot(is_var(scales))
  stopifnot(length(scales) >= 2)
  stopifnot(is_num(ridge, n = 1), ridge >= 0)

  mat <- as.matrix(data[scales])
  r <- stats::cor(mat, use = "pairwise.complete.obs")

  if (ridge > 0) {
    # Add the ridge to the diagonal of the *correlation matrix* (not the data,
    # as the buggy draft did) and rescale back to a unit diagonal. This lifts
    # the smallest eigenvalue by `ridge` before rescaling, restoring positive
    # definiteness while keeping a correlation matrix.
    diag(r) <- diag(r) + ridge
    r <- stats::cov2cor(r)
  }

  paf2(r)
}
