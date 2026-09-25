# Growth-recipe output: the certified trajectory table (M152) -----------------
# The step after the fit. Fixed effects and their covariance are drawn once
# by mvn_draws() (the Monte Carlo engine's draw root), or a draws matrix is
# taken as given (the brms path); each time point's (e, x, y) draws are one
# contrast of the coefficient draws, and ssm_draws(type = "parameters")
# summarizes them with the circular machinery and the certification rule.
# The package fits nothing and calls no engine (D-060, D-064).

#' Trajectory of SSM parameters from a fitted growth model
#'
#' Turn the fixed effects of the joint growth model that
#' [ssm_growth_formula()] builds into a table of Structural Summary Method
#' parameters at each time point, with intervals and the displacement
#' certification. Two input shapes are accepted. The frequentist shape is
#' `coef` and `vcov`, the fixed effects and their covariance from a glmmTMB
#' or nlme fit. The function draws `n_draws` coefficient vectors from the
#' multivariate normal distribution they define, the same large-sample step
#' the package's Monte Carlo method takes. The Bayesian shape is `draws`, a
#' matrix of posterior coefficient draws such as `as.matrix(fit)` from brms,
#' used as given. Under either shape, each draw's `e`, `x` and `y` at time
#' `t` are its intercept plus `t` times its slope, and the draws at each
#' time go through [ssm_draws()] with `type = "parameters"`.
#'
#' The default contrast reads six coefficient names: `dve`, `dvx`, `dvy` and
#' `dve:<time>`, `dvx:<time>`, `dvy:<time>`, where `<time>` is the `time`
#' argument. Those are the names the model from [ssm_growth_formula()] gives
#' its fixed effects on the long table [ssm_growth_data()] builds. A `b_`
#' prefix on the column names of `draws` is dropped, and other columns, such
#' as brms's `sd_`, `cor_` and `lp__` columns, are ignored. A missing name
#' is an error. A `contrast` function replaces the default for a model with
#' other terms, such as a quadratic time term; it must return, for one time
#' value, the 3 by `p` matrix that maps the `p` coefficients to `e`, `x` and
#' `y` at that time, in coefficient order.
#'
#' The displacement interval at a time point is wrong when `x(t)` and `y(t)`
#' are treated as independent, which is what fitting the coordinates in
#' separate models does. So the `coef` and `vcov` shape refuses a `vcov`
#' whose implied covariance between `x(t)` and `y(t)` is exactly zero at
#' every time in `times`, unless `vcov` is zero everywhere. The check reads
#' only the given `times`, so with a single time it sees only the covariance
#' terms that time reaches. A `draws` matrix is not checked for a joint fit.
#' The intervals from a REML fit's `coef` and `vcov` condition on its
#' estimated variance components and are too narrow at small samples; the
#' "Growth Models on SSM Parameters" vignette, Section 7, states what the
#' package's coverage oracle measured and that no shipped correction exists.
#' Intervals from `draws` summarize those draws as given.
#'
#' @param coef The fixed effects: a named numeric vector. Required with
#'   `vcov`, and absent with `draws`.
#' @param vcov The covariance matrix of `coef`: a symmetric numeric matrix
#'   with one row and column per element of `coef`, its dimnames equal to
#'   `names(coef)` when present. Required with `coef`, and absent with
#'   `draws`.
#' @param times Required. A numeric vector of the time values at which to
#'   evaluate the trajectory, one output row each.
#' @param draws Optional. A numeric matrix of coefficient draws, one row per
#'   draw and one named column per coefficient, in place of `coef` and
#'   `vcov`. When given, `n_draws` is ignored.
#' @param time Optional. The name of the time column in the output, and the
#'   suffix of the slope coefficients (default `"wave"`), the same `time`
#'   given to [ssm_growth_data()] and [ssm_growth_formula()].
#' @param interval Optional. A single number between 0 and 1 giving the
#'   level of the intervals (default = 0.95).
#' @param n_draws Optional. The number of coefficient vectors to draw from
#'   `coef` and `vcov` (default = 4000).
#' @param contrast Optional. `NULL` (default) for the default contrast, or a
#'   function of one time value returning a 3 by `p` numeric matrix whose
#'   rows are `e`, `x` and `y` and whose columns follow the order of `coef`
#'   or of the columns of `draws`.
#' @return A data frame of class `"circumplex_ssm_trajectory"` with
#'   attribute `time` naming its time column and attribute `input` recording
#'   the input shape, `"coef_vcov"` or `"draws"`, one row per value of
#'   `times`.
#'   Its columns are `<time>`; `e_est`, `e_lci`, `e_uci`, and the same three
#'   for `x`, `y`, `a` and `d`; and `certified`. The estimates and bounds
#'   are those [ssm_draws()] reports: medians and equal-tailed interval
#'   bounds for `e`, `x`, `y` and `a`, and the circular mean with circular
#'   quantile bounds in degrees for `d`. A `d` interval that straddles 0/360
#'   degrees has `d_lci > d_uci`. `certified` is the displacement
#'   certification at that time, and at an uncertified time the `d` interval
#'   is not interpretable. Printing shows the table rounded, marks each
#'   uncertified row, and ends with the caution for the input shape: the
#'   small-sample caution under `coef` and `vcov`, and under `draws` that the
#'   intervals summarize the draws as given. A subset that drops the
#'   attribute, or an `rbind()` of tables of different shapes, prints a
#'   caution that says the shape is not recorded.
#'   [ssm_plot_trajectory()] plots the object with no `time` argument.
#' @family growth functions
#' @export
#' @examples
#' # Fixed effects and covariance in the shape a joint fit returns
#' coef <- c(
#'   dve = 0.52, dvx = 0.61, dvy = -0.10,
#'   "dve:wave" = 0.00, "dvx:wave" = 0.00, "dvy:wave" = 0.065
#' )
#' vcov <- diag(c(7e-4, 3e-4, 3e-4, 5e-5, 2e-5, 2e-5))
#' vcov[2, 3] <- vcov[3, 2] <- 4e-5
#' dimnames(vcov) <- list(names(coef), names(coef))
#' set.seed(1)
#' trajectory <- ssm_trajectory(coef, vcov, times = 0:4)
#' trajectory
#' ssm_plot_trajectory(trajectory)
#'
#' # The same from a matrix of coefficient draws, here drawn by hand; a brms
#' # fit gives one as `as.matrix(fit)`
#' set.seed(1)
#' draws <- matrix(rnorm(500 * 6), nrow = 500) %*% chol(vcov)
#' draws <- sweep(draws, 2, coef, "+")
#' colnames(draws) <- names(coef)
#' ssm_trajectory(times = 0:4, draws = draws)
ssm_trajectory <- function(coef, vcov, times, draws = NULL, time = "wave",
                           interval = 0.95, n_draws = 4000,
                           contrast = NULL) {

  has_coef <- !missing(coef)
  has_vcov <- !missing(vcov)
  has_draws <- !is.null(draws)
  if (!((has_coef && has_vcov && !has_draws) ||
        (has_draws && !has_coef && !has_vcov))) {
    given <- c("coef", "vcov", "draws")[c(has_coef, has_vcov, has_draws)]
    stop(
      "Supply `coef` and `vcov` together, or `draws` alone; received ",
      if (length(given) == 0) "none of them" else
        paste0("`", given, "`", collapse = " and "),
      ".",
      call. = FALSE
    )
  }
  if (missing(times) || !is.numeric(times) || length(times) == 0 ||
      anyNA(times) || any(!is.finite(times))) {
    stop("`times` must be a numeric vector of at least one finite value.",
         call. = FALSE)
  }
  times <- as.numeric(times)
  if (!is_char(time, n = 1) || is.na(time) || !nzchar(time)) {
    stop("`time` must be a single column name.", call. = FALSE)
  }
  if (time %in% trajectory_value_cols()) {
    stop("`time` cannot be \"", time, "\", a column the trajectory table ",
         "reserves.", call. = FALSE)
  }
  if (!is_num(interval, n = 1) || is.na(interval) ||
      interval <= 0 || interval >= 1) {
    stop("`interval` must be a single number strictly between 0 and 1.",
         call. = FALSE)
  }
  if (!is.null(contrast) && !is.function(contrast)) {
    stop("`contrast` must be NULL or a function of one time value.",
         call. = FALSE)
  }

  if (has_draws) {
    B <- trajectory_check_draws(draws)
  } else {
    trajectory_check_coef_vcov(coef, vcov)
    if (!is_num(n_draws, n = 1) || is.na(n_draws) || n_draws < 2 ||
        n_draws != round(n_draws)) {
      stop("`n_draws` must be a single whole number of at least 2.",
           call. = FALSE)
    }
    B <- mvn_draws(as.integer(n_draws), as.numeric(coef), vcov)
    colnames(B) <- names(coef)
  }
  p <- ncol(B)
  cn <- colnames(B)

  # The contrast: one 3 x p matrix per time, rows (e, x, y). The default
  # reads the six names of the joint model and ignores every other column.
  if (is.null(contrast)) {
    needed <- c(paste0("dv", c("e", "x", "y")),
                paste0("dv", c("e", "x", "y"), ":", time))
    missing_names <- setdiff(needed, cn)
    if (length(missing_names) > 0) {
      stop(
        "Coefficient name", if (length(missing_names) > 1) "s" else "", " ",
        paste0("`", missing_names, "`", collapse = ", "),
        " not found in ", if (has_draws) "`colnames(draws)`" else
          "`names(coef)`",
        "; the default contrast reads the six names the model from ",
        "ssm_growth_formula() gives, `dve`, `dvx`, `dvy` and `dve:", time,
        "`, `dvx:", time, "`, `dvy:", time, "`. Pass `contrast` for a ",
        "model with other terms.",
        call. = FALSE
      )
    }
    B <- B[, needed, drop = FALSE]
    p <- 6L
    cn <- needed
    contrast_fn <- function(t) {
      L <- matrix(0, nrow = 3, ncol = 6, dimnames = list(c("e", "x", "y"),
                                                         needed))
      L[cbind(1:3, 1:3)] <- 1
      L[cbind(1:3, 4:6)] <- t
      L
    }
  } else {
    contrast_fn <- contrast
  }
  L_list <- lapply(times, function(t) {
    L <- contrast_fn(t)
    if (!is.matrix(L) || !is.numeric(L) || !identical(dim(L), c(3L, p)) ||
        anyNA(L)) {
      stop(
        "`contrast` must return a numeric 3 by ", p, " matrix (rows e, x, y; ",
        "one column per coefficient) at every time; at time ", t,
        " it returned ",
        if (is.matrix(L)) paste0("a ", nrow(L), " by ", ncol(L), " matrix")
        else paste0("an object of class ", class(L)[1]),
        ".",
        call. = FALSE
      )
    }
    rn <- rownames(L)
    if (!is.null(rn) && !identical(rn, c("e", "x", "y"))) {
      stop(
        "`contrast` must return its rows in the order e, x, y; the row names ",
        "at time ", t, " are ", paste0("\"", rn, "\"", collapse = ", "), ".",
        call. = FALSE
      )
    }
    unname(L)
  })

  # The joint-fit refusal (coef and vcov shape only): the covariance of x(t)
  # and y(t) implied by vcov through the contrast, exactly zero at every
  # time, is the signature of coordinates fit in separate models. An
  # all-zero vcov, read over the coefficients the contrast uses, is a
  # degenerate input with nothing to refuse.
  V <- if (has_draws) NULL else if (is.null(contrast))
    vcov[needed, needed, drop = FALSE] else vcov
  if (!has_draws && !all(V == 0)) {
    cross <- vapply(L_list, function(L) {
      as.numeric(L[2, , drop = FALSE] %*% V %*% L[3, ])
    }, numeric(1))
    if (all(cross == 0)) {
      stop(
        "`vcov` implies zero covariance between x(t) and y(t) at every time ",
        "in `times`. The displacement interval needs the joint fit of the ",
        "three coordinates that ssm_growth_formula() builds; a covariance ",
        "matrix assembled from separate fits per coordinate gives wrong ",
        "intervals.",
        call. = FALSE
      )
    }
  }

  per_time <- lapply(L_list, function(L) {
    draws_t <- B %*% t(L)
    colnames(draws_t) <- c("e", "x", "y")
    ssm_draws(draws_t, interval = interval, type = "parameters")
  })

  out <- data.frame(times)
  names(out) <- time
  for (par in c("e", "x", "y", "a", "d")) {
    for (s in c("_est", "_lci", "_uci")) {
      col <- paste0(par, s)
      out[[col]] <- vapply(per_time, function(r) as.numeric(r$results[[col]]),
                           numeric(1))
    }
  }
  out$certified <- vapply(per_time, function(r) r$details$certified,
                          logical(1))
  structure(out, time = time,
            input = if (has_draws) "draws" else "coef_vcov",
            class = c("circumplex_ssm_trajectory", "data.frame"))
}

trajectory_value_cols <- function() {
  c(paste0(rep(c("e", "x", "y", "a", "d"), each = 3),
           c("_est", "_lci", "_uci")), "certified")
}

trajectory_check_coef_vcov <- function(coef, vcov) {
  if (!is.numeric(coef) || is.null(names(coef)) || anyNA(coef) ||
      any(!is.finite(coef)) || any(!nzchar(names(coef)))) {
    stop("`coef` must be a named numeric vector with no missing values.",
         call. = FALSE)
  }
  if (anyDuplicated(names(coef))) {
    stop("`coef` has duplicated names.", call. = FALSE)
  }
  if (!is.matrix(vcov) || !is.numeric(vcov)) {
    stop("`vcov` must be a symmetric numeric matrix with no missing values.",
         call. = FALSE)
  }
  if (anyNA(vcov) || any(!is.finite(vcov))) {
    stop("`vcov` must be a symmetric numeric matrix with no missing values.",
         call. = FALSE)
  }
  p <- length(coef)
  if (!identical(dim(vcov), c(p, p))) {
    stop("`vcov` must be a square matrix with one row and column per `coef` ",
         "(", p, "); received ", nrow(vcov), " by ", ncol(vcov), ".",
         call. = FALSE)
  }
  if (!isSymmetric(unname(vcov))) {
    stop("`vcov` must be a symmetric matrix.", call. = FALSE)
  }
  dn <- dimnames(vcov)
  if (!is.null(dn) && (!identical(dn[[1]], names(coef)) ||
                       !identical(dn[[2]], names(coef)))) {
    stop("The dimnames of `vcov` must equal `names(coef)`, in the same order.",
         call. = FALSE)
  }
  # A covariance matrix is positive semidefinite. mvn_root() clamps a
  # negative eigenvalue to zero, so an invalid matrix (a covariance larger
  # than the product of its standard deviations) would draw without a
  # word; floating-point residue from a fitted vcov is under the tolerance.
  ev <- eigen(unname(vcov), symmetric = TRUE, only.values = TRUE)$values
  if (min(ev) < -1e-8 * max(abs(ev), .Machine$double.eps)) {
    stop("`vcov` is not a valid covariance matrix: its smallest eigenvalue ",
         "is ", signif(min(ev), 3), ", so at least one covariance exceeds ",
         "what its variances allow.", call. = FALSE)
  }
  invisible(TRUE)
}

# A draws matrix as given: numeric, at least two rows, every column named. A
# `b_` prefix (brms's fixed-effect prefix) is dropped from the names.
trajectory_check_draws <- function(draws) {
  if (is.data.frame(draws)) draws <- as.matrix(draws)
  if (!is.matrix(draws) || !is.numeric(draws) || nrow(draws) < 2 ||
      anyNA(draws) || any(!is.finite(draws))) {
    stop("`draws` must be a numeric matrix of at least two rows with no ",
         "missing or infinite values, one row per draw and one column per ",
         "coefficient.", call. = FALSE)
  }
  cn <- colnames(draws)
  if (is.null(cn) || any(is.na(cn)) || any(!nzchar(cn))) {
    stop("`draws` must have a name on every column.", call. = FALSE)
  }
  cn <- sub("^b_", "", cn)
  if (anyDuplicated(cn)) {
    stop("`draws` has duplicated column names once any `b_` prefix is ",
         "dropped.", call. = FALSE)
  }
  colnames(draws) <- cn
  draws
}

#' @rdname ssm_trajectory
#' @param x An object of class `"circumplex_ssm_trajectory"`.
#' @param digits The number of decimal places to print (default = 2).
#' @param ... Ignored (S3 consistency).
#' @method print circumplex_ssm_trajectory
#' @export
print.circumplex_ssm_trajectory <- function(x, digits = 2, ...) {
  stopifnot(is_num(digits, n = 1))
  time <- attr(x, "time")
  tab <- as.data.frame(x)
  # A column subset of the object keeps its class and may have dropped
  # `certified`; then no row is marked. An `NA` verdict is undecided, and
  # an undecided row is marked as uncertified rather than passed.
  cert <- if (is.logical(tab$certified)) tab$certified else
    rep(TRUE, nrow(tab))
  cert[is.na(cert)] <- FALSE
  tab$certified <- NULL
  num <- vapply(tab, is.numeric, logical(1))
  num[names(tab) == time] <- FALSE
  tab[num] <- lapply(tab[num], round, digits = digits)
  # A matrix, not a data frame: a data frame refuses the repeated row name
  # that marks every uncertified row.
  m <- as.matrix(format(tab))
  rownames(m) <- ifelse(cert, "", "*")

  cat("\n# SSM Trajectory:\n\n")
  print(m, quote = FALSE, right = TRUE, print.gap = 1L)
  if (any(!cert)) {
    cat_prose(
      paste0(
        "* Uncertified: the amplitude interval's lower bound is under 0.35 ",
        "interval-widths above zero, so the displacement interval at that ",
        "time is not interpretable."
      ),
      prefix = "  "
    )
  }
  # The caution follows the input shape the object records. A subset that
  # rebuilds the frame (a column subset, `subset()`, `transform()`) drops the
  # attribute, and then neither shape's caution is known to hold.
  input <- attr(x, "input")
  caution <- if (identical(input, "coef_vcov")) {
    paste0(
      "Caution: intervals from a fitted model's fixed-effect covariance ",
      "condition on its estimated variance components, and are too narrow ",
      "at small samples. See vignette(\"growth-ssm-analysis\"), Section 7."
    )
  } else if (identical(input, "draws")) {
    paste0(
      "Caution: these intervals summarize the supplied draws as given. ",
      "Their coverage depends on how the draws were produced, and the ",
      "joint fit was not checked. See vignette(\"growth-ssm-analysis\"), ",
      "Sections 7 and 10."
    )
  } else {
    paste0(
      "Caution: the input shape is not recorded on this object, so the ",
      "caution for its intervals is not known. See ",
      "vignette(\"growth-ssm-analysis\"), Sections 7 and 10."
    )
  }
  cat_prose(caution, prefix = "  ")
  cat("\n")
  invisible(x)
}

#' @rdname ssm_trajectory
#' @param ... For `rbind()`, trajectory tables to stack.
#' @method rbind circumplex_ssm_trajectory
#' @export
rbind.circumplex_ssm_trajectory <- function(...) {
  # Stacking keeps a shape only when every table has the same one; a mixed
  # stack carries no `input`, so it prints the shape-neutral caution rather
  # than the first table's.
  parts <- list(...)
  out <- do.call(rbind, lapply(parts, as.data.frame))
  shapes <- unique(vapply(parts, function(p) {
    s <- attr(p, "input")
    if (is.null(s)) NA_character_ else s
  }, character(1)))
  # rbind.data.frame copies the first part's attributes, so the mixed case
  # removes `input` rather than leaving it.
  attr(out, "input") <- if (length(shapes) == 1L && !is.na(shapes)) shapes
  attr(out, "time") <- attr(parts[[1L]], "time")
  class(out) <- class(parts[[1L]])
  out
}

#' @rdname ssm_plot_trajectory
#' @export
ssm_plot_trajectory.circumplex_ssm_trajectory <- function(x,
                                                          time = attr(x, "time"),
                                                          drop_xy = FALSE,
                                                          base_size = 11,
                                                          na.rm = TRUE,
                                                          ...) {
  if (is.null(time)) {
    stop(
      "This trajectory table has no `time` attribute; pass `time = ` naming ",
      "its time column.",
      call. = FALSE
    )
  }
  ssm_plot_trajectory.data.frame(
    as.data.frame(x), time = time, drop_xy = drop_xy,
    base_size = base_size, na.rm = na.rm, ...
  )
}
