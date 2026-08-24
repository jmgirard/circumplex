# A-posteriori per-fit accuracy certificate (M108; D-051, RR21) ---------------
#
# WHAT THIS IS. The degeneracy criterion in R/axes_corrected_se.R refuses a
# fitted matrix a-priori, on p*kappa^2*eps -- a conservative envelope that
# measurement puts 5 to 8 decades above the error actually committed in
# reachable geometry. This file is the other instrument: given the matrix a fit
# is priced at, it ESTIMATES the relative error that fit's own corrected
# component SEs and its `cval` carry, after the fact, from that fit's own
# numbers. Nothing consumes it yet -- re-keying the "ill_conditioned" refusal
# to it is M111's work -- so no exported behaviour depends on this file.
#
# THE MECHANISM (D-051). The shipped pricing pipeline is replayed once in
# compensated double-double arithmetic -- two doubles carrying an unevaluated
# sum, about 31 significant digits, base R and no dependency -- and the
# certificate is the relative disagreement between the shipped values and the
# replayed ones, times a stated safety factor. Both routes run the SAME
# algorithm on the SAME data, at precisions 16 decades apart (eps_dd ~ 6e-32
# against eps ~ 2.2e-16), so the disagreement IS the shipped route's committed
# error rather than a proxy for it. That precision separation is the whole
# decision: two routes at EQUAL precision were measured at RR21 section 1 and
# rejected -- their disagreement is dominated by whichever route is less
# accurate, overstating the shipped SE error by 3 to 4 decades on every
# reachable case while UNDER-reporting the cval error at two of six anchor
# geometries, which is the licensing failure this instrument exists to prevent.
# Runtime exact-rational arithmetic was rejected too (base R has no bignum; a
# hand-rolled one is a larger correctness surface than the certificate it
# serves, and an optional package cannot decide what the package refuses).
#
# WHAT IT ESTIMATES, exactly (RR21 section 2). With P the priced matrix -- the
# realigned cov2cor(Sigma-hat) both surfaces consume -- and se_r(P), cval(P)
# the exact-real-arithmetic values of the shipped formulas at P:
#
#   E_SE   = max over fitted components r of |se_r_hat - se_r(P)| / se_r(P)
#   E_cval = |cval_hat - cval(P)| / |cval(P)|
#
# The SE side is aggregated by MAX because the reported vector refuses as a
# unit (the M62/M66 contract): a gate must protect the worst component, and a
# per-component certificate would license nothing the max does not.
#
# These are estimates of the COMMITTED ERROR, NOT proven upper bounds -- no
# theorem converts the disagreement into a bound. The safety factor F = 10 and
# the additive floor of two machine epsilons are what make the estimate behave
# as an upper estimate, and that behaviour is pinned empirically rather than
# assumed: at six anchor geometries against the exact-rational oracle
# (tests/testthat/test-axes-certificate.R, AC2/AC3) and by planted-perturbation
# sensitivity invariants elsewhere. F is load-bearing, not decorative: at F = 1
# the raw disagreement sits just BELOW the true error at two of those anchors
# (ratios 0.997 and 0.983, RR21 section 3), because dropping from v to sqrt(v)
# is a first-order conversion.
#
# NEITHER n NOR df ENTERS, structurally. The reported SE is sqrt(v_r / n), so
# the relative SE error is |sqrt(v_r_hat / v_r) - 1|: n cancels EXACTLY, not
# merely to first order. The certificate is therefore computed from the
# pre-square-root quadratic forms v_r and from the pre-division numerator of
# cval, in which neither the sample size nor the degrees of freedom appear, and
# takes no `n` and no `df` argument at all. What the n-touching tail (one
# division, one square root) and the df division can add is ~2 ulp regardless
# of their values, which is what the 2*eps additive floor covers. This is
# D-048's and D-049's requirement met by construction: a certificate that moved
# with the typed n would rebuild the yardstick-dependence they refused.
#
# SCOPE, stated so it cannot be overclaimed. The certificate prices the
# arithmetic FROM P TO THE REPORTED NUMBERS. The optimizer's error in Sigma-hat
# and cov2cor()'s own rounding are upstream of P and common to both routes --
# out of scope, exactly as they were for the a-priori bound, and the
# indefiniteness band prices optimizer error separately (M90).
#
# THE SENTINEL (RR21 section 5). Where either route fails to produce finite
# values, or a quadratic form is nonpositive, or a denominator is zero, both
# estimates are 1 -- "no digits certified". It is finite and non-negative like
# any other return, and it sits four decades above the accuracy target, so a
# gate keyed to the certificate fails closed on it (GP2). Measured along the
# degradation path (RR21 section 5): past kappa ~ 3e8 the shipped pricing's own
# solve() refuses "unidentified" before the certificate matters -- there is no
# reported number left to certify. The sentinel is returned as a unit for both
# quantities, never one field at a time, so the two surfaces cannot disagree
# about whether this fit is certified (M89's nestedness contract).


# ---- compensated (double-double) arithmetic ---------------------------------
#
# A dd value is list(hi, lo) with the value hi + lo unevaluated and
# |lo| <= ulp(hi)/2. hi and lo are ordinary numeric vectors or matrices of the
# same shape, so every operation below is elementwise and vectorized: the
# arithmetic is 10 to 30 R-level calls per operation, and doing it a scalar at
# a time measured 6x slower at p = 24 (RR21 section 7).
#
# The error-free transforms are Knuth's two-sum and Dekker's two-product. Both
# are theorems about IEEE-754 double rounding, and both are defeated by an
# arithmetic that does NOT round each operation: a fused multiply-add
# contracting `ahi * bhi - p`, or x87 80-bit intermediates. Each line below is
# a separate R-level operation on a materialized numeric vector, so R's
# evaluator forces the rounding a C compiler could contract away -- and
# axes_dd_selftest() checks the transforms against committed known answers at
# every certificate call rather than trusting that argument.

dd_of <- function(hi, lo = NULL) {
  if (is.null(lo)) lo <- hi * 0
  list(hi = hi, lo = lo)
}

# Knuth two-sum: hi = fl(a + b) and lo = the exact rounding error, for any a, b.
dd_two_sum <- function(a, b) {
  s <- a + b
  bb <- s - a
  list(hi = s, lo = (a - (s - bb)) + (b - bb))
}

# The cheap variant, valid only where |a| >= |b| -- every use below has that
# from the branch it sits in, which is what the extra argument would otherwise
# have to assert.
dd_quick_two_sum <- function(a, b) {
  s <- a + b
  list(hi = s, lo = b - (s - a))
}

# Dekker two-product via the 2^27+1 splitting constant: hi = fl(a * b) and
# lo = the exact rounding error. No FMA is assumed (see the note above).
dd_two_prod <- function(a, b) {
  p <- a * b
  ca <- 134217729 * a
  ahi <- ca - (ca - a)
  alo <- a - ahi
  cb <- 134217729 * b
  bhi <- cb - (cb - b)
  blo <- b - bhi
  list(hi = p, lo = ((ahi * bhi - p) + ahi * blo + alo * bhi) + alo * blo)
}

dd_add <- function(x, y) {
  s <- dd_two_sum(x$hi, y$hi)
  t <- dd_two_sum(x$lo, y$lo)
  s <- dd_quick_two_sum(s$hi, s$lo + t$hi)
  dd_quick_two_sum(s$hi, s$lo + t$lo)
}

dd_neg <- function(x) list(hi = -x$hi, lo = -x$lo)

dd_sub <- function(x, y) dd_add(x, dd_neg(y))

dd_mul <- function(x, y) {
  p <- dd_two_prod(x$hi, y$hi)
  dd_quick_two_sum(p$hi, p$lo + (x$hi * y$lo + x$lo * y$hi))
}

# Three-step division: two corrections bring the quotient to full dd precision.
dd_div <- function(x, y) {
  q1 <- x$hi / y$hi
  r <- dd_sub(x, dd_mul(y, dd_of(q1)))
  q2 <- r$hi / y$hi
  r <- dd_sub(r, dd_mul(y, dd_of(q2)))
  q3 <- r$hi / y$hi
  s <- dd_quick_two_sum(q1, q2)
  dd_add(s, dd_of(q3))
}

# Scaling by a power of two is exact in both words, so the 0.5 and 2 factors
# the pricing carries cost nothing and introduce no rounding at all.
dd_scale2 <- function(x, f) list(hi = x$hi * f, lo = x$lo * f)

dd_to_double <- function(x) x$hi + x$lo

# Tree (pairwise) summation over every element: log2(m) rounding steps instead
# of m, and it is also the fewest R-level calls, since each level is one
# vectorized dd_add over half the remaining terms.
dd_sum <- function(x) {
  h <- as.vector(x$hi)
  l <- as.vector(x$lo)
  if (length(h) == 0L) return(list(hi = 0, lo = 0))
  while (length(h) > 1L) {
    n <- length(h)
    m <- n %/% 2L
    s <- dd_add(list(hi = h[seq_len(m)], lo = l[seq_len(m)]),
                list(hi = h[m + seq_len(m)], lo = l[m + seq_len(m)]))
    if (n %% 2L == 1L) {
      h <- c(s$hi, h[[n]])
      l <- c(s$lo, l[[n]])
    } else {
      h <- s$hi
      l <- s$lo
    }
  }
  list(hi = h[[1L]], lo = l[[1L]])
}

# Row sums, tree-reduced over columns for the same reason.
dd_rowsums <- function(x) {
  h <- x$hi
  l <- x$lo
  while (ncol(h) > 1L) {
    n <- ncol(h)
    m <- n %/% 2L
    s <- dd_add(list(hi = h[, seq_len(m), drop = FALSE],
                     lo = l[, seq_len(m), drop = FALSE]),
                list(hi = h[, m + seq_len(m), drop = FALSE],
                     lo = l[, m + seq_len(m), drop = FALSE]))
    if (n %% 2L == 1L) {
      h <- cbind(s$hi, h[, n])
      l <- cbind(s$lo, l[, n])
    } else {
      h <- s$hi
      l <- s$lo
    }
  }
  list(hi = as.vector(h), lo = as.vector(l))
}

dd_t <- function(x) list(hi = t(x$hi), lo = t(x$lo))

dd_diag <- function(x) {
  i <- seq_len(nrow(x$hi))
  ij <- cbind(i, i)
  list(hi = x$hi[ij], lo = x$lo[ij])
}

dd_set_diag <- function(x, v) {
  i <- seq_len(nrow(x$hi))
  ij <- cbind(i, i)
  x$hi[ij] <- v$hi
  x$lo[ij] <- v$lo
  x
}

# Matrix product, accumulated one rank-one term at a time so that each term is
# a single vectorized dd_mul over the whole n x m result.
dd_matmul <- function(a, b) {
  n <- nrow(a$hi)
  k <- ncol(a$hi)
  m <- ncol(b$hi)
  acc <- dd_of(matrix(0, n, m))
  for (j in seq_len(k)) {
    term <- dd_mul(
      list(hi = matrix(a$hi[, j], n, m), lo = matrix(a$lo[, j], n, m)),
      list(hi = matrix(b$hi[j, ], n, m, byrow = TRUE),
           lo = matrix(b$lo[j, ], n, m, byrow = TRUE))
    )
    acc <- dd_add(acc, term)
  }
  acc
}

# Inverse by Gauss-Jordan elimination with partial pivoting on [A | I].
# Pivoting rather than a Cholesky-shaped factorization deliberately: the
# admitted domain includes matrices whose smallest eigenvalue is negative at
# roundoff level (the M90 band), where a positive-definiteness assumption is
# not available. Returns NULL on a zero pivot or a non-finite result -- the
# sentinel path, never a guess.
dd_solve <- function(a) {
  n <- nrow(a$hi)
  hi <- cbind(a$hi, diag(n))
  lo <- cbind(a$lo, matrix(0, n, n))
  for (k in seq_len(n)) {
    # which.max() returns integer(0) on an all-NA column, and the finiteness
    # test below would then evaluate to NA and error -- the sentinel path
    # raising a condition instead of returning, which is what this guard
    # stops. A column with no finite entry has no usable pivot, so it is the
    # sentinel either way.
    cand <- abs(hi[k:n, k])
    if (!any(is.finite(cand))) return(NULL)
    piv <- which.max(cand) + k - 1L
    if (!is.finite(hi[piv, k]) || hi[piv, k] == 0) return(NULL)
    if (piv != k) {
      hi[c(k, piv), ] <- hi[c(piv, k), ]
      lo[c(k, piv), ] <- lo[c(piv, k), ]
    }
    rowk <- dd_div(list(hi = hi[k, ], lo = lo[k, ]),
                   list(hi = hi[k, k], lo = lo[k, k]))
    hi[k, ] <- rowk$hi
    lo[k, ] <- rowk$lo
    other <- seq_len(n)[-k]
    nr <- length(other)
    nc <- ncol(hi)
    # n = 1 leaves nothing to eliminate. Guarded rather than left to recycling:
    # matrix(row, 0, nc, byrow = TRUE) warns about non-empty data in a
    # zero-extent matrix, and a warning from inside a certificate would reach
    # the user through a surface that is supposed to be silent.
    if (nr > 0L) {
      upd <- dd_sub(
        list(hi = hi[other, , drop = FALSE], lo = lo[other, , drop = FALSE]),
        dd_mul(
          list(hi = matrix(hi[other, k], nr, nc), lo = matrix(lo[other, k], nr, nc)),
          list(hi = matrix(rowk$hi, nr, nc, byrow = TRUE),
               lo = matrix(rowk$lo, nr, nc, byrow = TRUE))
        )
      )
      hi[other, ] <- upd$hi
      lo[other, ] <- upd$lo
    }
  }
  out <- list(hi = hi[, n + seq_len(n), drop = FALSE],
              lo = lo[, n + seq_len(n), drop = FALSE])
  if (!all(is.finite(out$hi)) || !all(is.finite(out$lo))) return(NULL)
  out
}

# Known-answer self-test of the two error-free transforms, on operand pairs
# whose exact errors are known in closed form: fl(1 + eps/2) is 1 with error
# eps/2 (ties-to-even), and (1 +/- eps)^2 differs from its rounded product by
# exactly +/- eps^2. If an arithmetic mode defeats the transforms, these are
# what notice: the certificate then returns its sentinel, so the a-priori
# criterion remains the operative gate instead of a silently wrong estimate.
axes_dd_selftest <- function() {
  e <- .Machine$double.eps
  s <- dd_two_sum(1, e / 2)
  p <- dd_two_prod(1 + e, 1 + e)
  m <- dd_two_prod(1 + e, 1 - e)
  identical(s$hi, 1) && identical(s$lo, e / 2) &&
    identical(p$hi, 1 + 2 * e) && identical(p$lo, e^2) &&
    identical(m$hi, 1) && identical(m$lo, -(e^2))
}


# ---- the reference route ----------------------------------------------------
#
# The same pipeline axes_v_pricing() and axes_u_pricing() run, expression for
# expression, in dd arithmetic. Returns the corrected quadratic forms `v` (one
# per fitted component) and the scaling factor's numerator `u`, both as dd
# values, or NULL where the route cannot produce them.
axes_dd_pricing <- function(sigma, d) {
  ss <- dd_of(sigma)
  si <- dd_solve(ss)
  if (is.null(si)) return(NULL)
  mats <- lapply(d$mats, dd_of)
  sim <- lapply(mats, function(m) dd_matmul(si, m))
  q <- length(sim)

  info_hi <- matrix(0, q, q)
  info_lo <- matrix(0, q, q)
  simt <- lapply(sim, dd_t)
  for (s in seq_len(q)) {
    for (t in s:q) {
      e <- dd_scale2(dd_sum(dd_mul(sim[[s]], simt[[t]])), 0.5)
      info_hi[s, t] <- info_hi[t, s] <- e$hi
      info_lo[s, t] <- info_lo[t, s] <- e$lo
    }
  }
  acov <- dd_solve(list(hi = info_hi, lo = info_lo))
  if (is.null(acov)) return(NULL)

  # The component quadratic forms. Only the CORRECTED arm is replayed: the
  # `naive` arm is never user-reported (it exists as the tie to lavaan's own
  # SE, D-037), so there is nothing about it to certify.
  v_hi <- numeric(d$n_comp)
  v_lo <- numeric(d$n_comp)
  for (r in seq_len(d$n_comp)) {
    acc <- dd_of(matrix(0, nrow(sigma), ncol(sigma)))
    for (k in seq_len(q)) {
      acc <- dd_add(acc, dd_mul(mats[[k]],
                                list(hi = acov$hi[r, k], lo = acov$lo[r, k])))
    }
    w <- dd_scale2(dd_matmul(dd_matmul(si, acc), si), 0.5)
    wc <- dd_set_diag(w, dd_of(numeric(nrow(sigma))))
    wc <- dd_set_diag(wc, dd_neg(dd_rowsums(dd_mul(wc, ss))))
    wcs <- dd_matmul(wc, ss)
    vv <- dd_scale2(dd_sum(dd_mul(wcs, dd_t(wcs))), 2)
    v_hi[[r]] <- vv$hi
    v_lo[[r]] <- vv$lo
  }

  # The scaling factor's numerator, tr_vg - sum(acov * bmat).
  up <- upper.tri(sigma)
  rho <- dd_of(sigma[up])
  si_up <- list(hi = si$hi[up], lo = si$lo[up])
  one <- dd_of(rep(1, length(rho$hi)))
  tr_vg <- dd_sum(dd_sub(one, dd_mul(dd_mul(si_up, rho),
                                     dd_sub(one, dd_mul(rho, rho)))))
  ys <- lapply(sim, function(sm) {
    w <- dd_scale2(dd_matmul(sm, si), 0.5)
    w <- dd_set_diag(w, dd_sub(dd_diag(w), dd_diag(dd_matmul(ss, w))))
    dd_matmul(w, ss)
  })
  yst <- lapply(ys, dd_t)
  b_hi <- matrix(0, q, q)
  b_lo <- matrix(0, q, q)
  for (s in seq_len(q)) {
    for (t in s:q) {
      e <- dd_scale2(dd_sum(dd_mul(ys[[s]], yst[[t]])), 2)
      b_hi[s, t] <- b_hi[t, s] <- e$hi
      b_lo[s, t] <- b_lo[t, s] <- e$lo
    }
  }
  u <- dd_sub(tr_vg, dd_sum(dd_mul(acov, list(hi = b_hi, lo = b_lo))))

  if (!all(is.finite(c(v_hi, v_lo, u$hi, u$lo)))) return(NULL)
  list(v = list(hi = v_hi, lo = v_lo), u = u)
}


# ---- the certificate --------------------------------------------------------
#
# `sigma` is the priced matrix -- the realigned cov2cor(Sigma-hat) -- and `d`
# the derivative set from axes_se_derivs(). Returns a list of two finite,
# non-negative numbers: `se`, the estimated relative error of the corrected
# component SE vector (worst component), and `cval`, the estimated relative
# error of the scaling factor. See this file's header for what they estimate,
# what they are not, and what the sentinel value 1 means.
axes_certificate_safety_factor <- 10

axes_accuracy_certificate <- function(sigma, d) {
  sentinel <- list(se = 1, cval = 1)
  if (!axes_dd_selftest()) return(sentinel)
  if (!all(is.finite(sigma))) return(sentinel)

  v_hat <- axes_v_pricing(sigma, d)
  u_hat <- axes_u_pricing(sigma, d)
  if (is.character(v_hat) || is.character(u_hat)) return(sentinel)
  ref <- axes_dd_pricing(sigma, d)
  if (is.null(ref)) return(sentinel)

  v_hat <- v_hat$corrected
  if (!all(is.finite(v_hat)) || !is.finite(u_hat)) return(sentinel)
  # Nonpositive quadratic forms and a vanished cval numerator are the two
  # denominators that can fail here: the first is the cancellation the
  # "indefinite" backstop in axes_se_pricing() exists for, the second the one
  # the scaling surface's cval <= 0 backstop exists for. Neither is a matrix
  # about which a relative error can be stated at all.
  if (any(ref$v$hi <= 0) || ref$u$hi == 0) return(sentinel)

  delta_v <- max(abs(dd_to_double(
    dd_div(dd_sub(dd_of(v_hat), ref$v), ref$v)
  )))
  delta_u <- abs(dd_to_double(
    dd_div(dd_sub(dd_of(u_hat), ref$u), ref$u)
  ))

  # delta_v / 2 converts a variance's relative error to its square root's, to
  # first order; the O(delta^2) that conversion drops is what the safety factor
  # covers, and covers with two decades to spare at every geometry measured
  # (RR21 section 3).
  eps <- .Machine$double.eps
  fac <- axes_certificate_safety_factor
  out <- list(se = fac * max(delta_v / 2, 2 * eps),
              cval = fac * max(delta_u, 2 * eps))
  if (!is.finite(out$se) || !is.finite(out$cval)) return(sentinel)
  out
}
