#
get_loadings <- function(.data, scales, nf, rot, fm) {
  scales_en <- rlang::enquo(scales)
  fit <- .data %>% 
    dplyr::select(!!scales_en) %>% 
    as.matrix() %>% 
    psych::fa(nfactors = nf, rotate = rot, fm = fm)
  fit$loadings
}

# Fisher Test of equal axes
# Best if the data were ipsatized
# Reject unequal axes if less than .11 
fit_fisher <- function(.data, scales) {
  scales_en <- rlang::enquo(scales)
  lambda <- get_loadings(.data, !!scales_en, nf = 2, rot = "none", fm = "pa")
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
  lambda <- get_loadings(.data, !!scales_en, nf = 2, rot = "none", fm = "pa")
  commun <- (lambda[, 1] ^ 2) + (lambda[, 2] ^ 2)
  theta <- sign(lambda[, 2]) * acos(lambda[, 1] / sqrt(commun))
  gaps <- diff(theta)
  var(gaps)
}

# Variance Test 2 of interstitiality
# best with no general factor
# less than .40 is almost certain interstitiality
# less than .48 is three times as likely
# less than .65 is twice as likely

# Rotation Test of interstitiality
# less than .14 is almost certain interstitiality
# less than .31 is twice as likely

