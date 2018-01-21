#' Profile (Mean-based) Structural Summary Method
#'
#' Calculate SSM parameters with bootstrapped confidence intervals for the mean
#' of any number of groups.
#'
#' @param .data A matrix or data frame containing at least circumplex scales.
#' @param scales A list of the variables in \code{.data} that contain circumplex
#'   scales (in tidyverse-style NSE specification, see examples).
#' @param angles A numerical vector containing the angular displacement of each
#'   circumplex scale included in \code{scales} (in degrees).
#' @param boots The number of bootstrap resamples to use in calculating the
#'   confidence intervals (default = 2000).
#' @param interval The confidence intervals' percentage level (default = 0.95).
#' @param grouping Optional argument: the variable in \code{.data} that contains
#'   the group membership of each observation. To assess the mean profile of all
#'   observations, do not supply this argument (see examples).
#' @param contrast  (default = "none").
#' @param plot A logical determining whether a plot should be created (default =
#'   TRUE).
#' @param ... Additional parameters to be passed to \code{circle_plot()}.
#'   Examples include \code{amax} and \code{font.size}.
#' @return A tibble containing SSM parameters (point and interval estimates) for
#'   each group's mean profile (or the entire mean profile without grouping).
#' @seealso \code{\link{ssm_measures}}, which calculates SSM parameters for
#'   measures using a correlation-based approach.
#' @export

ssm_profiles <- function(.data, scales, angles, boots = 2000, interval = 0.95,
  grouping, contrast = "none", plot = TRUE, ...) {
  
  # Enable column specification using tidyverse-style NSE -------------------
  scales_en <- rlang::enquo(scales)
  
  # Check that inputs are valid ---------------------------------------------
  assert_that(is.numeric(angles), is.count(boots), is.flag(plot))
  assert_that(contrast %in% c("none", "model", "test"))
  assert_that(is.scalar(interval), interval > 0, interval < 1)
  
  # Select circumplex scales and grouping variable (if applicable) ----------
  if (base::missing(grouping)) {
    bs_input <- .data %>% 
      dplyr::select(!!scales_en) %>% 
      dplyr::mutate(Group = 1)
  } else {
    #TODO: Check that there are only two groups if contrast != none
    grouping_en <- rlang::enquo(grouping)
    #grouping_qn <- rlang::quo_name(grouping_en)
    bs_input <- .data %>%
      dplyr::select(!!grouping_en, !!scales_en) %>% 
      dplyr::mutate(Group = factor(!!grouping_en)) %>% 
      dplyr::select(-!!grouping_en)
  }
  
  bs_function <- function(.data, index, angles, contrast) {
    resample <- .data[index, ]
    mat <- as.matrix(resample[, 1:(ncol(resample) - 1)])
    grp <- resample$Group
    scores_r <- by(mat, grp, colMeans)
    ssm_by_group(scores_r, angles, contrast)
  }
  
  bs_output <- ssm_bootstrap(
    .data = bs_input,
    statistic = bs_function,
    angles = angles,
    boots = boots,
    interval = interval,
    contrast = contrast,
    strata = bs_input$Group
  )

  row_labels <- levels(bs_input$Group)
  if (contrast != "none") {
    row_labels <- c(row_labels, paste0("Diff ", contrast))
  }
  results <- bs_output %>% 
    dplyr::mutate(label = row_labels)
  
  # ht <- results_table(bs_output, contrast = pairwise,
  #   group = !base::missing(grouping)) %>% 
  #   htmlTable::htmlTable(
  #     caption = "Structural Summary Method Parameters with
  #     Bootstrap Confidence Intervals",
  #     align = "llllll",
  #     align.header = "llllll",
  #     rnames = FALSE,
  #     css.cell = "padding-right: 1em; min-width: 3em; white-space: nowrap;"
  #   )
  # print(ht)
  
  invisible(results)
}

#' Calculate structural summary parameters
#'
#' @param scores A numeric vector of scores on multiple circumplex scales: can
#'   be either mean scores or correlations.
#' @param angles A numeric vector containing an angular displacement for each
#'   circumplex scale provided in \code{scores} (in degrees).
#' @return A numerical vector containing structural summary method parameters
#'   that describe \code{scores} given \code{angles}. The vector will contain
#'   the following values: elevation, x-axis value, y-axis value, amplitude,
#'   angular displacement (in degrees), and model fit (R-squared).

ssm_parameters <- function(scores, angles) {
  
  # Calculate SSM parameters ------------------------------------------------
  nScales <- length(scores) #3e-8
  elev <- sum(scores) / nScales #4e-7
  xval <- as.numeric((2 / nScales) * (scores %*% cos(d2r(angles)))) #3e-6
  yval <- as.numeric((2 / nScales) * (scores %*% sin(d2r(angles)))) #3e-6
  ampl <- sqrt(xval * xval + yval * yval) #8e-7
  disp <- r2d(atan2(yval, xval)) %% 360 #1e-6
  gfit <- 1 - ((sum((elev + ampl * cos(d2r(angles - disp)) - scores) ^ 2)) /
      (stats::var(scores) * (nScales - 1))) #5e-5
  c(elev, xval, yval, ampl, disp, gfit) #4e-7
  
}
