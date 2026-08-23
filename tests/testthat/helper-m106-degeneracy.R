# M106 / RR19 section 3a: the reachable near-duplicate geometry.
#
# Every matrix the degeneracy criterion prices in production is
# `lavaan::fitted(fit)$cov` -- exactly on the model manifold. RR19 measured the
# criterion's error attainment over matrices of that form and found the
# committed RR18 fixture (p = 3, df = 1, 25 units off the manifold) to be
# unreachable through the exported API, which requires four scales. These
# builders reproduce RR19's families from its stated parameters so the tests
# and the exact oracle exercise geometries the package can actually produce.
#
# Deterministic by construction -- a closed-form model-implied matrix, no RNG
# and no committed fixture, so there is nothing whose bit-exactness could go
# unfalsifiable on the authoring machine.

# RR19 family B: p = 9, eight octant scales plus a ninth item duplicating
# scale 1's angle. Sigma = xi1*C + xi2*J + zeta1*B + diag(eps), cov2cor'd,
# with the near-duplicate pair's item errors driven down together. Shrinking
# `pair_eps` walks the pair's correlation toward 1 and kappa up by a decade
# per decade: 7e-5 gives r = .9999 (kappa 2.87e4), 7e-6 gives r = .99999
# (kappa 2.87e5).
m106_family_b <- function(pair_eps, xi1 = 0.3, xi2 = 0.2, zeta1 = 0.2,
                          other_eps = 0.30) {
  oct <- as.numeric(octants())
  # The ninth item shares SCALE 1's angle, whatever that is -- octants() starts
  # at 90, not 0, so a hard-coded duplicate angle would pair the ninth item
  # with a different scale and the driven-down errors would sit on two items
  # that are not the near-duplicate pair.
  ang <- c(oct, oct[1L])
  rad <- ang * pi / 180
  cmat <- outer(rad, rad, function(u, v) cos(u - v))
  scale_id <- c(seq_along(octants()), 1L)
  bmat <- outer(scale_id, scale_id, "==") * 1
  evar <- rep(other_eps, length(ang))
  evar[c(1L, length(ang))] <- pair_eps
  sigma <- xi1 * cmat + xi2 * matrix(1, length(ang), length(ang)) +
    zeta1 * bmat + diag(evar)
  dimnames(sigma) <- list(paste0("i", seq_along(ang)), paste0("i", seq_along(ang)))
  stats::cov2cor(sigma)
}

# The condition number the criterion actually prices.
m106_kappa <- function(r) {
  ev <- eigen(r, symmetric = TRUE, only.values = TRUE)$values
  ev[1L] / ev[length(ev)]
}

# RR19 family A generalised: `per_scale` items on each octant scale, one
# item-error variance throughout. Sigma = xi1*C + xi2*J + zeta1*B + eps*I,
# cov2cor'd. Shrinking `eps` raises kappa without touching the model form.
# per_scale = 1 is RR19's p = 8 family A; per_scale = 3 gives p = 24.
m106_family_a <- function(eps, per_scale = 1L, xi1 = 0.3, xi2 = 0.3,
                          zeta1 = 0.0) {
  oct <- as.numeric(octants())
  ang <- rep(oct, each = per_scale)
  scale_id <- rep(seq_along(oct), each = per_scale)
  rad <- ang * pi / 180
  cmat <- outer(rad, rad, function(u, v) cos(u - v))
  bmat <- outer(scale_id, scale_id, "==") * 1
  sigma <- xi1 * cmat + xi2 * matrix(1, length(ang), length(ang)) +
    zeta1 * bmat + eps * diag(length(ang))
  nms <- paste0("i", seq_along(ang))
  dimnames(sigma) <- list(nms, nms)
  stats::cov2cor(sigma)
}

# RR19 family C: p = 4, the minimum design the API accepts -- four scales, one
# item each, at the four cardinal angles.
m106_family_c <- function(eps, xi1 = 0.3, xi2 = 0.3) {
  ang <- c(90, 180, 270, 360)
  rad <- ang * pi / 180
  cmat <- outer(rad, rad, function(u, v) cos(u - v))
  sigma <- xi1 * cmat + xi2 * matrix(1, 4L, 4L) + eps * diag(4L)
  nms <- paste0("i", 1:4)
  dimnames(sigma) <- list(nms, nms)
  stats::cov2cor(sigma)
}

# The criterion's own floor, for locating a construction relative to it.
m106_floor_kappa <- function(p) sqrt(axes_degeneracy_tau / (p * .Machine$double.eps))
