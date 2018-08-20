
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

# Fisher Test of equal axes
# Best if the data were ipsatized
# less than .10 is almost certain equal axes
# less than .15 is twice as likely
fit_fisher <- function(.data, scales) {
  scales_en <- rlang::enquo(scales)
  lambda <- get_loadings(.data, !!scales_en)
  radius <- (lambda[, 1] ^ 2) + (lambda[, 2] ^ 2)
  sd(radius) / mean(radius)
}

# Gap Test of interstitiality
# less than .03 is almost certian interstitiality
# less than .05 is twice as likely
# affected by the number of variables
# from Upton & Fingleton 1989
fit_gap <- function(.data, scales) {
  scales_en <- rlang::enquo(scales)
  lambda <- get_loadings(.data, !!scales_en)
  commun <- (lambda[, 1] ^ 2) + (lambda[, 2] ^ 2)
  theta <- sign(lambda[, 2]) * acos(lambda[, 1] / sqrt(commun))
  gaps <- diff(theta)
  var(gaps)
}

# Variance Test of interstitiality
# best with no general factor
# less than .40 is almost certain interstitiality
# less than .48 is three times as likely
# less than .65 is twice as likely
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
