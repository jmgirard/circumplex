# Bayesian draws adapter -------------------------------------------------------

#' Summarize posterior draws as SSM parameters
#'
#' Transform posterior draws from a user-fitted Bayesian model (e.g., a brms
#' cosine regression) into Structural Summary Method parameter draws and
#' summarize them with the package's circular-statistics machinery. Two draw
#' shapes are accepted, distinguished explicitly (never guessed):
#'
#' * **Parameter draws** (`angles = NULL`, `type = "parameters"`): a numeric
#'   matrix or data frame with exactly three columns interpreted **in column
#'   order** as (e, x, y) -- elevation (intercept), the cosine coefficient
#'   (x), and the sine coefficient (y). Each row is mapped to amplitude
#'   `a = sqrt(x^2 + y^2)` and displacement `d = atan2(y, x)` wrapped to
#'   \[0, 360\] (the 0/360 pole is reported as 360, the package's LM = 360
#'   convention). Column names are not used to reorder: when names are present
#'   but do not look (intercept, cos, sin)-like, a message states the
#'   assumed mapping. A row with exactly zero amplitude has undefined
#'   displacement (`NA`); model fit is undefined for parameter draws
#'   (`fit = NA`) because no profile is available to measure it against.
#' * **Profile draws** (`angles` supplied): a numeric matrix with one column
#'   per circumplex scale (`ncol(draws) == length(angles)`); each row goes
#'   through the closed-form SSM transform exactly as a bootstrap replicate
#'   would, inheriting the standing degenerate-profile NA semantics.
#'
#' With `angles = NULL` and a column count other than 3 the input matches
#' neither shape and an error explains both. With `angles = NULL` and
#' exactly 3 columns the shape is ambiguous (a p = 3 instrument's profile
#' draws look like parameter draws), so `type = "parameters"` is required.
#'
#' Point estimates are posterior medians for e, x, y, a, and fit (amplitude
#' is right-skewed, so a mean would be biased upward), and the circular mean
#' for displacement. Marginal summaries are not jointly coherent: the
#' reported a is not `sqrt(x^2 + y^2)` of the reported (x, y), and the
#' reported d is not their direction -- each is the honest marginal summary
#' of its own posterior. Intervals are equal-tailed credible intervals
#' (percentile quantiles of the draws), with displacement handled by the
#' package's circular quantile machinery (centered on the circular mean, so
#' intervals straddling 0/360 wrap correctly). Draws with undefined
#' displacement are excluded from the displacement summaries only, which are
#' therefore conditional on estimability (measure-zero for continuous
#' parameter-draw posteriors; can bind for profile draws). A diffuse
#' posterior with zero circular resultant has an undefined circular mean,
#' reported as `NA` rather than invented.
#'
#' Note that independent priors on (x, y) induce a non-uniform prior on
#' (a, d) -- roughly Rayleigh-shaped on amplitude, with mass pushed away
#' from a = 0 -- so the prior on the SSM scale should be inspected (e.g.,
#' by prior-predictive simulation) rather than assumed flat; see the
#' package's Bayesian SSM vignette.
#'
#' @param draws Required. A numeric matrix or data frame of posterior draws:
#'   one row per draw, columns per the shape rules above.
#' @param angles Optional. A numeric vector of angular displacements (in
#'   degrees) for profile draws, one per column of `draws`; `NULL` (default)
#'   for parameter draws.
#' @param interval Optional. A single number between 0 and 1 giving the
#'   credible level for the equal-tailed intervals (default = 0.95).
#' @param type Optional. `"parameters"` or `"profiles"`, required only where
#'   the shape is ambiguous (`angles = NULL` with exactly 3 columns); when
#'   given elsewhere it must not contradict `angles`.
#' @return An object of class `"circumplex_ssm_draws"` holding `draws` (the
#'   SSM parameter draws, one row per posterior draw, columns e, x, y, a, d,
#'   fit, displacement in degrees \[0, 360\], pole reported as
#'   360), `results` (the point summaries
#'   and credible bounds), and `details`, whose `certified` field records the
#'   package's displacement-interpretability certification applied to the
#'   amplitude credible interval (`a_lci / (a_uci - a_lci) >= 0.35`): when it
#'   fails, the displacement interval is not interpretable and printing adds
#'   a note saying so. Printing shows the summary table;
#'   `summary()` adds the analysis details.
#' @family ssm functions
#' @family analysis functions
#' @export
#' @examples
#' # Parameter draws (e.g., brms fixed-effect draws b_Intercept, b_cos, b_sin)
#' set.seed(1)
#' draws <- cbind(rnorm(500, 0.4, 0.1), rnorm(500, 0.9, 0.1),
#'                rnorm(500, -0.3, 0.1))
#' ssm_draws(draws, type = "parameters")
#'
ssm_draws <- function(draws, angles = NULL, interval = 0.95, type = NULL) {

  stopifnot(is.matrix(draws) || is.data.frame(draws))
  draws_mat <- as.matrix(draws)
  stopifnot(is.numeric(draws_mat))
  stopifnot(nrow(draws_mat) >= 1)
  stopifnot(is_null_or_num(angles))
  stopifnot(is_num(interval, n = 1))
  stopifnot(interval > 0, interval < 1)
  if (!is.null(type)) {
    type <- match.arg(type, c("parameters", "profiles"))
  }

  # Shape dispatch (spec sec. 5.1): `is.null(angles)` alone is unsound at
  # ncol = 3 -- a p = 3 instrument's profile draws passed without angles
  # would be silently transformed as (e, x, y), and vice versa -- so the
  # ambiguous cell requires an explicit type and contradictions error.
  if (!is.null(angles)) {
    if (identical(type, "parameters")) {
      stop(
        "parameter draws (shape A) take `angles = NULL`; ",
        "`angles` describes profile draws (shape B).",
        call. = FALSE
      )
    }
    if (ncol(draws_mat) != length(angles)) {
      stop(
        "profile draws require one column per scale: ncol(draws) = ",
        ncol(draws_mat), " but length(angles) = ", length(angles), ".",
        call. = FALSE
      )
    }
    shape <- "profiles"
  } else if (identical(type, "profiles")) {
    stop(
      "profile draws (shape B) require `angles` (one angular displacement ",
      "per scale column).",
      call. = FALSE
    )
  } else if (ncol(draws_mat) == 3) {
    if (!identical(type, "parameters")) {
      stop(
        "3-column draws are ambiguous: they could be (e, x, y) parameter ",
        "draws or a 3-scale instrument's profile draws. Pass ",
        "type = \"parameters\" for parameter draws, or supply `angles` ",
        "for profile draws.",
        call. = FALSE
      )
    }
    shape <- "parameters"
  } else if (identical(type, "parameters")) {
    stop(
      "parameter draws (shape A) have exactly 3 columns (e, x, y); ",
      "received ", ncol(draws_mat), ".",
      call. = FALSE
    )
  } else {
    stop(
      "cannot interpret `draws`: parameter draws (shape A) have exactly 3 ",
      "columns (e, x, y) with `angles = NULL`; profile draws (shape B) ",
      "have one column per scale and require `angles`. Received ",
      ncol(draws_mat), " columns without `angles`.",
      call. = FALSE
    )
  }

  # Transform each draw to the SSM parameters (columns in ssm_param_names()
  # order; displacement in radians for the interval machinery below)
  pnames <- ssm_param_names()
  if (shape == "profiles") {
    angles_rad <- as_radian(as_degree(angles))
    raw <- group_parameters(draws_mat, angles_rad)
    t_mat <- matrix(raw, ncol = length(pnames), byrow = TRUE)
    t_mat[is.nan(t_mat)] <- NA_real_
  } else {
    # Column order is the contract (e, x, y); names never reorder. When
    # names are present but not recognizably (intercept, cos, sin)-like,
    # say what is being assumed (spec sec. 5.1).
    cn <- colnames(draws_mat)
    if (!is.null(cn)) {
      recognized <-
        grepl("intercept|elev|^e$", cn[1], ignore.case = TRUE) &&
        grepl("cos|^x", cn[2], ignore.case = TRUE) &&
        grepl("sin|^y", cn[3], ignore.case = TRUE)
      if (!recognized) {
        message(
          "Assuming draws columns are, in column order, (e, x, y): \"",
          cn[1], "\" as e (elevation/intercept), \"", cn[2],
          "\" as x (cosine coefficient), \"", cn[3],
          "\" as y (sine coefficient)."
        )
      }
    }
    e <- draws_mat[, 1]
    x <- draws_mat[, 2]
    y <- draws_mat[, 3]
    a <- sqrt(x^2 + y^2)
    # Exactly zero amplitude has undefined displacement (atan2(0, 0) is an
    # arbitrary 0, not a direction) -- measure-zero for continuous
    # posteriors, kept NA for honesty. Fit is synthesized as NA in the full
    # 6-column ssm_param_names() layout (a 5-column matrix would misalign
    # every parameter downstream). The wrap adds 2*pi to negative angles
    # rather than using %%: over atan2's (-pi, pi] range this is exactly the
    # kernel's modu() (floor is -1 or 0), whereas R's %% applies a second
    # reduction that sends a tiny-negative atan2 to 0 instead of modu's 2*pi
    # -- which would break bit-equality with kernel-computed replicates at
    # the pole (D-003).
    d_raw <- atan2(y, x)
    d <- ifelse(!is.na(a) & a > 0, d_raw + 2 * pi * (d_raw < 0), NA_real_)
    t_mat <- cbind(e, x, y, a, d, NA_real_)
  }
  colnames(t_mat) <- pnames

  # Point summaries: medians for the linear parameters (amplitude is
  # right-skewed, so a mean would be biased upward), circular mean for
  # displacement (NA displacements stripped; angle_mean() has no na.rm).
  # An exact-pole circular mean reports 360, matching the estimate path and
  # the quantile method's pole convention (D-003/M20; window rationale in
  # quantile.circumplex_radian).
  med <- function(v) stats::median(v, na.rm = TRUE)
  d_ok <- t_mat[, "d"][!is.na(t_mat[, "d"])]
  if (length(d_ok) == 0) {
    d0 <- NA_real_
  } else {
    d0 <- as.numeric(angle_mean(d_ok))
    if (!is.na(d0)) {
      # Single-correction wrap (see the shape A comment above), then the
      # quantile method's pole window so both float representations of the
      # pole report 2*pi = 360
      if (d0 < 0) d0 <- d0 + 2 * pi
      pole <- d0 < (16 * .Machine$double.eps) |
        (2 * pi - d0) < (16 * .Machine$double.eps)
      if (pole) d0 <- 2 * pi
    }
  }
  t0 <- c(
    med(t_mat[, "e"]), med(t_mat[, "x"]), med(t_mat[, "y"]),
    med(t_mat[, "a"]), d0, med(t_mat[, "fit"])
  )

  # Summaries ride the existing interval machinery verbatim: percentile
  # quantiles of posterior draws are the equal-tailed credible interval, and
  # classing d as circumplex_radian applies the circular quantile/pole/
  # branch machinery unchanged. t0 here is the adapter's own point
  # summaries -- there is no observed estimate for posterior draws.
  results <- ssm_replicate_intervals(
    t0 = t0,
    t = t_mat,
    interval = interval,
    contrast = FALSE,
    replicate_label = "posterior draws",
    t0_warning = paste0(
      "The displacement point summary is undefined (no draws with defined ",
      "displacement, or zero circular resultant); reported as NA."
    ),
    interval_label = "credible interval",
    structural_na = if (shape == "parameters") "fit" else character(0)
  )

  # User-facing draws report displacement in degrees [0, 360)
  draws_out <- t_mat
  draws_out[, "d"] <- as.numeric(as_degree(as_radian(t_mat[, "d"])))

  new_s3_scalar(
    draws = draws_out,
    results = results,
    details = list(
      n_draws = nrow(draws_out),
      interval = interval,
      shape = shape,
      angles = if (shape == "profiles") as_degree(angles) else NULL,
      # D-007 displacement-interpretability certification applied to the
      # credible interval: a pure function of the amplitude interval pair, so
      # it gates posterior summaries exactly as it gates estimated profiles
      # (ssm_certified() in R/ssm_oop.R is THE single definition; the print
      # method recomputes from the same results columns). Stored for
      # programmatic access -- e.g., flagging uncertified timepoints in a
      # growth-trajectory table (see the growth vignette).
      certified = unname(ssm_certified(results$a_lci, results$a_uci))
    ),
    call = match.call(),
    class = "circumplex_ssm_draws"
  )
}

# Print method for objects of ssm_draws class
#' @method print circumplex_ssm_draws
#' @export
print.circumplex_ssm_draws <- function(x, digits = 3, ...) {
  dat <- x$results
  v <- c(
    dat$e_est, dat$x_est, dat$y_est, dat$a_est, dat$d_est, dat$fit_est,
    dat$e_lci, dat$x_lci, dat$y_lci, dat$a_lci, dat$d_lci, NA,
    dat$e_uci, dat$x_uci, dat$y_uci, dat$a_uci, dat$d_uci, NA
  )
  m <- round(matrix(v, nrow = 6, ncol = 3), digits)
  rownames(m) <- c(
    "Elevation", "X-Value", "Y-Value",
    "Amplitude", "Displacement", "Model Fit"
  )
  colnames(m) <- c("Estimate", "Lower CrI", "Upper CrI")
  cat("\n# Posterior Summary:\n\n")
  print.default(m, print.gap = 3L, na.print = "")

  # Interpretation guardrail: the D-007 certification rule applied to the
  # credible interval (recomputed here from the results columns, exactly as
  # print.circumplex_ssm does, so the note can never disagree with the rule)
  if (!ssm_certified(dat$a_lci, dat$a_uci)) {
    cat(
      "  Note: the amplitude CrI lower bound is under 0.35 CrI-widths ",
      "above zero; the displacement is not interpretable.\n",
      sep = ""
    )
  }
  cat("\n")
  invisible(x)
}

# Summary method for objects of ssm_draws class
#' @method summary circumplex_ssm_draws
#' @export
summary.circumplex_ssm_draws <- function(object, digits = 3, ...) {
  cat(
    "\nStatistical Basis:\tPosterior Draws",
    "\nPosterior Draws:\t", object$details$n_draws,
    "\nCredible Level:\t\t", object$details$interval,
    "\nDraw Shape:\t\t",
    if (object$details$shape == "profiles") "Profiles" else "Parameters",
    if (!is.null(object$details$angles)) {
      c("\nScale Displacements:\t", as.numeric(object$details$angles))
    },
    "\n"
  )
  print(object)
  invisible(object)
}
