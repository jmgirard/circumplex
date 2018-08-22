
get_loadings <- function(.data, scales) {
  scales_en <- rlang::enquo(scales)
  fit <- .data %>% 
    dplyr::select(!!scales_en) %>% 
    as.matrix() %>% 
    psych::fa(nfactors = 2, rotate = "none", fm = "pa")
  fit$loadings
}

get_rotation <- function(lambda, angle) {
  rot <- matrix(nrow = 2, ncol = 2)
  theta <- angle * (pi / 180)
  diag(rot) <- cos(theta)
  rot[1, 2] <- sin(theta) * -1
  rot[2, 1] <- sin(theta)
  lambda %*% rot
}

#' Fisher Test of equal axes
#' 
#' Conduct the Fisher Test of equal axes. The test will have the most power if
#' the items have been ipsatized first. See the \code{ipsatize} function. 
#' Simulation studies suggest that test values less than 0.10 almost certainly
#' indicate equal axes, whereas test values less than 0.15 indicate that equal
#' axes are twice as likely as non-equal axes.
#' 
#' @param .data Required. A data frame containing at least circumplex scales.
#' @param scales Required. The variable names or column numbers for the
#'   variables in \code{.data} that contain circumplex scales to be analyzed.
#' @return A double
#' @family fit functions
#' @family analysis functions
#' @export
fit_fisher <- function(.data, scales) {
  scales_en <- rlang::enquo(scales)
  lambda <- get_loadings(.data, !!scales_en)
  radius <- (lambda[, 1] ^ 2) + (lambda[, 2] ^ 2)
  sd(radius) / mean(radius)
}

#' Gap Test of interstitiality
#' 
#' Conduct the Gap Test of interstitiality. The test will have the most power if
#' the items have been ipsatized first. See the \code{ipsatize} function.
#' Simulation studies suggest that values less than 0.03 almost certainly
#' indicate equal axes, whereas test values less than 0.05 indicate that equal
#' axes are twice as likely as non-equal axes.
#' 
#' @param .data Required. A data frame containing at least circumplex scales.
#' @param scales Required. The variable names or column numbers for the
#'   variables in \code{.data} that contain circumplex scales to be analyzed.
#' @return A double
#' @family fit functions
#' @family analysis functions
#' @export
fit_gap <- function(.data, scales) {
  scales_en <- rlang::enquo(scales)
  lambda <- get_loadings(.data, !!scales_en)
  commun <- (lambda[, 1] ^ 2) + (lambda[, 2] ^ 2)
  theta <- sign(lambda[, 2]) * acos(lambda[, 1] / sqrt(commun))
  gaps <- diff(theta)
  var(gaps)
}

#' Variance Test of interstitiality
#' 
#' Conduct the Variance Test of interstitiality (sometimes called VT2). The test
#' will have the most power if the items have been ipsatized first. See the 
#' \code{ipsatize} function. It is also best in the absence of a general factor.
#' Simulation studies suggest that test values less that 0.40 almost certainly
#' indicate interstitiality, test values less than 0.48 indicate that
#' interstitiality is three times as likely as non-interstitiality, and test
#' values less than 0.65 indicate that interstitiality is twice as likely as
#' non-interstitiality.
#' 
#' @param .data Required. A data frame containing at least circumplex scales.
#' @param scales Required. The variable names or column numbers for the
#'   variables in \code{.data} that contain circumplex scales to be analyzed.
#' @return A double
#' @family fit functions
#' @family analysis functions
#' @export
fit_vt <- function(.data, scales) {
  scales_en <- rlang::enquo(scales)
  lambda <- get_loadings(.data, !!scales_en)
  criterion <- rep(0, 10)
  for (i in 0:9) {
    rlambda <- get_rotation(lambda, i * 5)
    criterion[i] <- var(rlambda[, 1] / sum(rlambda ^ 2))
  }
  sd(criterion) / mean(criterion)
}

# Rotation Test of interstitiality
# less than .14 is almost certain interstitiality
# less than .31 is twice as likely

#' Rotation Test of interstitiality
#' 
#' Conduct the Rotation Test of interstitiality. The test will have the most
#' power if the items have been ipsatized first. See the \code{ipsatize}
#' function. Simulation studies suggest that test values less that 0.14 almost
#' certainly indicate interstitiality, whereas test values less than 0.31
#' indicate that interstitiality is twice  as likely as non-interstitiality.
#' 
#' @param .data Required. A data frame containing at least circumplex scales.
#' @param scales Required. The variable names or column numbers for the
#'   variables in \code{.data} that contain circumplex scales to be analyzed.
#' @return A double
#' @family fit functions
#' @family analysis functions
#' @export
fit_rt <- function(.data, scales) {
  scales_en <- rlang::enquo(scales)
  lambda <- get_loadings(.data, !!scales_en)
  criterion <- rep(0, 10)
  for (i in 0:9) {
    rlambda <- get_rotation(lambda, i * 5)
    criterion[i] <- sum(apply(rlambda ^ 2, 1, var))
  }
  sd(criterion) / mean(criterion)
}

#' Test circumplex fit using the RANDALL Concordance Index
#' 
#' Conduct the Randomization Test of Hypothesized Order Relations (RANDALL).
#' Estimate the RANDALL Concordance Index using a Monte-Carlo-based bootstrap
#' procedure and estimate its percentile confidence interval.
#' 
#' @param .data Required. A data frame containing at least circumplex scales.
#' @param scales Required. The variable names or column numbers for the
#'   variables in \code{.data} that contain circumplex scales to be analyzed.
#' @param boots Optional. A single positive integer indicating how many
#'   bootstrap resamples to use when estimating the confidence intervals
#'   (default = 2000).
#' @param interval Optional. A single positive number between 0 and 1
#'   (exclusive) that indicates what confidence level to use when estimating the
#'   confidence intervals (default = 0.95).
#' @param listwise Optional. A logical indicating whether missing values should
#'   be handled by listwise deletion (TRUE) or pairwise deletion (FALSE). Note
#'   that pairwise deletion may result in different missing data patterns in
#'   each bootstrap resample and is slower to compute (default = TRUE).
#' @param tolerance Optional. Tolerance (relative to largest variance) for
#'   numerical lack of positive-definiteness in the correlation matrix computed
#'   among \code{scales} (default = 0.1). See \code{MASS::mvrnorm}.
#' @return A three-element vector containing the mean concordance index, the 
#'   lower bound of the confidence interval, and the upper bound of the
#'   confidence interval. (Will soon be replaced by a list object.)
#' @family fit functions
#' @family analysis functions
#' @export
#' @examples 
#' data("jz2017")
#' fit_randall(.data = jz2017, scales = PA:NO) 
fit_randall <- function(.data, scales, boots = 2000, interval = 0.95,
  listwise = TRUE, tolerance = 0.1) {
  
  # Select and count circumplex scales
  scales_en <- rlang::enquo(scales)
  dat <- .data %>% dplyr::select(!!scales_en)
  nscales <- ncol(dat)
  
  # Perform listwise deletion if requested
  if (listwise == TRUE) {
    dat <- dat %>% tidyr::drop_na()
  }
  
  # Calculate observed correlation matrix
  rmat <- cor(dat, use = "pairwise.complete.obs")
  
  # Calculate hypothesized rankings
  hyp_ranking <- get_ranking(nscales)
  
  # Simulate from a multivariate normal distribution
  bs_input <- data.frame(
    MASS::mvrnorm(
      n = nrow(dat),
      mu = rep(0, nscales),
      Sigma = rmat,
      tol = tolerance,
      empirical = TRUE
    )
  )
  
  # Function to perform during bootstrapping
  bs_function <- function(bs_input, r) {
    
    # Calculate resample correlation matrix
    data_r <- bs_input[r, ]
    rmat_r <- cor(data_r, use = "pairwise.complete.obs")
    vals_r <- rmat_r[lower.tri(rmat_r)]
    
    # See how often correlations match hypothesized rankings
    ncorrect <- 0
    ntotal <- 0
    for (i in 1:length(vals_r)) {
      lower_ranks <- hyp_ranking > hyp_ranking[i]
      ncorrect <- ncorrect + sum(vals_r[i] > vals_r[lower_ranks])
      ntotal <- ntotal + length(vals_r[lower_ranks])
    }
    
    # Calculate and return the resample concordance interval
    (ncorrect / ntotal) - ((ntotal - ncorrect) / ntotal)
  }
  
  # Perform the bootstrap procedure and capture results
  bs_results <- boot::boot(bs_input, bs_function, boots)
  
  # Calculate confidence interval
  bs_interval <- boot::boot.ci(bs_results, conf = interval, type = "perc")$percent
  c(bs_results$t0, bs_interval[4], bs_interval[5])
}

get_ranking <- function(nv) {
  m <- matrix(NA, nrow = nv, ncol = nv)
  for (i in 1:nv) {
    for (j in 1:nv) {
      m[i, j] <- n_away(i, j, nv)
    }
  }
  m[lower.tri(m)]
}

n_away <- function(a, b, nv) {
  sequence <- rep(1:nv, 2)
  a_idx <- which(sequence == a)
  b_idx <- which(sequence == b)
  min(abs(outer(a_idx, b_idx, FUN = "-")))
}
