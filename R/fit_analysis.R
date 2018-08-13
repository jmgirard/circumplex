

nvar <- function(df, vars) {
  vars_en <- rlang::enquo(vars)
  df_sub <- dplyr::select(df, !!vars_en)
  ncol(df_sub)
}

efa2 <- function(df, vars) {
  vars_en <- rlang::enquo(vars)
  df %>% 
    dplyr::select(!!vars_en) %>% 
    ipsatize(scales = !!vars_en) %>% 
    stats::factanal(factors = 2, rotation = "none")
}

# Fisher Test of equal axes
# less than .10 is almost certain equal axes
# less than .15 is twice as likely
# from Fisher 1997
fit_fisher <- function(df, vars) {
  lambda <- efa2(df, !!rlang::enquo(vars))$loadings
  x <- lambda[, 1] ^ 2 + lambda[, 2] ^ 2
  sd(x) / mean(x)
}

# Gap Test of interstitiality
# less than .03 is almost certian interstitiality
# less than .05 is twice as likely
# affected by the number of variables
# from Upton & Fingleton 1989
fit_gap <- function(df, vars) {
  lambda <- efa2(df, !!rlang::enquo(vars))$loadings
  theta <- atan2(y = lambda[, 2], x = lambda[, 1])
}

# Variance Test 2 of interstitiality
# best with no general factor
# less than .40 is almost certain interstitiality
# less than .48 is three times as likely
# less than .65 is twice as likely

# Rotation Test of interstitiality
# less than .14 is almost certain interstitiality
# less than .31 is twice as likely


# Minkowski Test of interstitiality
# best with large samples (n>600)
