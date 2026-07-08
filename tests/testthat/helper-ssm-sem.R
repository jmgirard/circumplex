# Shared analytic-truth machinery for the SEM-based SSM (M5 T3).
# Auto-sourced by testthat; also source()d by devel/m5-coverage-oracle.R so
# the unit tests and the coverage oracle draw from ONE copy of the truth
# algebra (a divergence here would make them validate against silently
# different populations).

# Analytic-truth population from the spec sec. 3.1 scaled model itself (or,
# with a full `phi`, the strict-tier shape): chosen (a_i, c_i, Theta, Phi,
# sigma_m, v_m) imply a joint (scales + measures) covariance matrix in closed
# form, and the true latent profile rho0 = Cov(M, t_i) / (SD(M) SD(t_i)) is
# available without simulation.
sem_pop <- function(a, cc, theta, angles_deg, sigma_m, v_m,
                    phi_gx = 0, phi_gy = 0, phi = NULL,
                    scales = paste0("s", seq_along(angles_deg)),
                    measures = paste0("m", seq_len(ncol(sigma_m)))) {
  p <- length(angles_deg)
  m <- ncol(sigma_m)
  th <- angles_deg * pi / 180
  lambda <- cbind(a, cc * cos(th), cc * sin(th))
  if (is.null(phi)) {
    phi <- rbind(
      c(1, phi_gx, phi_gy),
      c(phi_gx, 1, 0),
      c(phi_gy, 0, 1)
    )
  }
  # Joint (factors, measures) covariance must be PSD for the population to
  # exist; guards against nonsense cells.
  joint_fm <- rbind(cbind(phi, sigma_m), cbind(t(sigma_m), diag(v_m, m)))
  stopifnot(min(eigen(joint_fm, symmetric = TRUE)$values) > -1e-12)

  sigma_ss <- lambda %*% phi %*% t(lambda) + diag(theta, p)
  sigma_sm <- lambda %*% sigma_m # p x m
  sigma <- rbind(
    cbind(sigma_ss, sigma_sm),
    cbind(t(sigma_sm), diag(v_m, m))
  )
  dimnames(sigma) <- list(c(scales, measures), c(scales, measures))

  var_t <- rowSums((lambda %*% phi) * lambda)
  rho0 <- t(sigma_sm) / sqrt(outer(v_m, var_t)) # m x p

  list(
    sigma = sigma, rho0 = rho0, var_t = var_t, scales = scales,
    measures = measures, angles = angles_deg
  )
}

# Fit a model tier to exact population moments (deterministic; the optimum
# reproduces the generating values because misfit is exactly zero).
sem_pop_fit <- function(pop, model = "scaled", n = 10000, ...) {
  syn <- ssm_sem_syntax(
    scales = pop$scales, angles = pop$angles,
    measures = pop$measures, model = model
  )
  lavaan::cfa(syn, sample.cov = pop$sigma, sample.nobs = n, ...)
}
