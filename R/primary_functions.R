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
#' @param contrast The type of contrast to run for the first two \code{measures}
#'   specified. Options are "none" to run no contrasts, "model" to calculate SSM
#'   parameters for the difference between each scale score, or "test" to
#'   calculate the difference between each SSM parameter (default = "none").
#' @param plot A logical determining whether a plot should be created for the
#'   results and contrasts, if applicable (default = TRUE).
#' @param table A logical determining whether an HTML table should be created
#'   for the results and contrasts, if applicable (default = TRUE).
#' @param ... Additional parameters to be passed to \code{circle_plot()}.
#'   Examples include \code{amax} and \code{font.size}.
#' @return A tibble containing SSM parameters (point and interval estimates) for
#'   each group's mean profile (or the entire mean profile without grouping).
#' @seealso \code{\link{ssm_measures}}, which calculates SSM parameters for
#'   measures using a correlation-based approach.
#' @export

ssm_profiles <- function(.data, scales, angles, boots = 2000, interval = 0.95,
  grouping, contrast = "none", plot = TRUE, table = TRUE, ...) {
  
  cl <- match.call()
  
  # Enable column specification using tidyverse-style NSE ----------------------
  scales_en <- rlang::enquo(scales)
  
  # Check that inputs are valid ------------------------------------------------
  assert_that(is.numeric(angles), is.count(boots), is.flag(plot))
  assert_that(contrast %in% c("none", "model", "test"))
  assert_that(is.scalar(interval), interval > 0, interval < 1)
  #TODO: Add a warning if the scales do not appear to be standardized
  
  # Convert angles from degrees to radians -------------------------------------
  angles <- angles %>% as_degree() %>% as_radian()
  
  # Select circumplex scales and grouping variable (if applicable) -------------
  if (base::missing(grouping)) {
    bs_input <- .data %>% 
      dplyr::select(!!scales_en) %>% 
      dplyr::mutate(Group = factor(1))
  } else {
    #TODO: Check that there are only two groups if contrast != none
    grouping_en <- rlang::enquo(grouping)
    bs_input <- .data %>%
      dplyr::select(!!grouping_en, !!scales_en) %>% 
      dplyr::mutate(Group = factor(!!grouping_en)) %>% 
      dplyr::select(-!!grouping_en)
  }
  # Create function that will perform bootstrapping ----------------------------
  bs_function <- function(.data, index, angles, contrast) {
    resample <- .data[index, ]
    mat <- as.matrix(resample[, 1:(ncol(resample) - 1)])
    grp <- as.integer(resample$Group)
    scores_r <- group_scores(mat, grp)
    ssm_by_group(scores_r, angles, contrast)
  }
  # Perform bootstrapping ------------------------------------------------------
  bs_output <- ssm_bootstrap(
    bs_input = bs_input,
    bs_function = bs_function,
    angles = angles,
    boots = boots,
    interval = interval,
    contrast = contrast,
    strata = bs_input$Group
  )
  # Separate and label profile results and contrast results --------------------
  row_labels <- levels(bs_input$Group)
  if (contrast != "none") {
    row_labels <- c(row_labels, sprintf("%s: %s - %s",
      stringr::str_to_title(contrast), row_labels[2], row_labels[1]))
  }
  results <- bs_output %>% 
    dplyr::mutate(label = row_labels) 
  
  if (contrast != "none") {
    contrasts <- results[nrow(results), ]
    results <- results[1:(nrow(results) - 1), ]
  } else {
    contrasts <- NA
  }
  
  #TODO: Change the overall N to N per group
  details <- list(
    n = nrow(.data), 
    boots = boots, 
    interval = interval, 
    angles = as_degree(angles)
  )
  
  out <- new_ssm(
    results = results,
    contrasts = contrasts,
    call = cl,
    details = details,
    type = "Profile"
  )
  
  if (table == TRUE) {
    t1 <- ssm_table(out, "results",
      caption = sprintf("Mean-based Structural Summary Statistics with
        %s Confidence Intervals", str_percent(interval)))
    print(t1)
    if (contrast != "none") {
      t2 <- ssm_table(out, "contrasts", caption = sprintf("Mean-based 
        Structural Summary Statistics with %s Confidence Intervals for 
        Profile Contrasts", str_percent(interval)))
      print(t2)
    }
  }
  
  if (plot == TRUE) {
    #TODO: Add output plot code
  }
  
  out
}

#' Measure (Correlation-based) Structural Summary Method
#'
#' Calculate SSM parameters with bootstrapped confidence intervals for any
#' number of measures, based on their correlations with circumplex scales.
#'
#' @param .data A matrix or data frame containing at least circumplex scales.
#' @param scales A list of the variables in \code{.data} that contain circumplex
#'   scales (in tidyverse-style NSE specification, see examples).
#' @param angles A numerical vector containing the angular displacement of each
#'   circumplex scale included in \code{scales} (in degrees).
#' @param measures A list of variables in \code{.data} to be analyzed (in
#'   tidyverse-style NSE specification, see examples).
#' @param boots The number of bootstrap resamples to use in calculating the
#'   confidence intervals (default = 2000).
#' @param interval The confidence intervals' percentage level (default = 0.95).
#' @param contrast The type of contrast to run for the first two \code{measures}
#'   specified. Options are "none" to run no contrasts, "model" to calculate SSM
#'   parameters for the difference between each scale score, or "test" to
#'   calculate the difference between each SSM parameter (default = "none").
#' @param plot A logical determining whether a plot should be created for the
#'   results and contrasts, if applicable (default = TRUE).
#' @param table A logical determining whether an HTML table should be created
#'   for the results and contrasts, if applicable (default = TRUE).
#' @param ... Additional parameters to be passed to \code{circle_plot()}.
#'   Examples include \code{amax} and \code{font.size}.
#' @return A tibble containing SSM parameters (point and interval estimates) for
#'   each measure.
#' @seealso \code{\link{ssm_profiles}}, which calculates SSM parameters for
#'   profiles using a mean-based approach.
#' @export

ssm_measures <- function(.data, scales, angles, measures, boots = 2000,
  interval = 0.95, contrast = "none", plot = TRUE, table = TRUE, ...) {
  
  cl <- match.call()
  
  # Enable column specification using tidyverse-style NSE ----------------------
  scales_en <- rlang::enquo(scales)
  measures_en <- rlang::enquo(measures)
  
  # Check that inputs are valid ------------------------------------------------
  assert_that(is.numeric(angles), is.count(boots), is.flag(plot))
  assert_that(contrast %in% c("none", "model", "test"))
  assert_that(is.scalar(interval), interval > 0, interval < 1)
  
  # Convert angles from degrees to radians -------------------------------------
  angles <- angles %>% as_degree() %>% as_radian()
  
  # Select circumplex scales and measure variables -----------------------------
  bs_input <- .data %>% 
    dplyr::select(!!scales_en, !!measures_en)
  
  bs_function <- function(.data, index, angles, contrast) {
    resample <- .data[index, ]
    cs <- as.matrix(resample[, 1:length(angles)])
    mv <- as.matrix(resample[, (length(angles) + 1):ncol(resample)])
    scores_r <- measure_scores(cs, mv)
    ssm_by_group(scores_r, angles, contrast)
  }
  
  bs_output <- ssm_bootstrap(
    bs_input = bs_input,
    bs_function = bs_function,
    angles = angles,
    boots = boots,
    interval = interval,
    contrast = contrast
  )
  
  row_labels <- names(dplyr::select(.data, !!measures_en))
  if (contrast != "none") {
    row_labels <- c(row_labels, sprintf("%s: %s - %s",
      stringr::str_to_title(contrast), row_labels[2], row_labels[1]))
  }
  results <- bs_output %>% 
    dplyr::mutate(label = row_labels)
  
  if (contrast != "none") {
    contrasts <- results[nrow(results), ]
    results <- results[1:(nrow(results) - 1), ]
  } else {
    contrasts <- NA
  }
  
  details <- list(
    n = nrow(.data), 
    boots = boots, 
    interval = interval, 
    angles = as_degree(angles)
  )
  
  out <- new_ssm(
    results = results,
    contrasts = contrasts,
    call = cl,
    details = details,
    type = "Measure"
  )
  
  if (table == TRUE) {
    t1 <- ssm_table(out, "results",
      caption = sprintf("Correlation-based Structural Summary Statistics with
        %s Confidence Intervals", str_percent(interval)))
    print(t1)
    if (contrast != "none") {
      t2 <- ssm_table(out, "contrasts", caption = sprintf("Correlation-based 
        Structural Summary Statistics with %s Confidence Intervals for 
        Measure Contrasts", str_percent(interval)))
      print(t2)
    }
  }
  
  if (plot == TRUE) {
    #TODO: Add output plot code
  }
  
  out
}


#' Standardize scales using existing norms
#' @export
ssm_standardize <- function(.data, scales, angles, norms) {
  
  # Check that inputs are valid ------------------------------------------------
  assert_that(is.numeric(angles))
  # Enable column specification using tidyverse-style NSE ----------------------
  scales_en <- rlang::enquo(scales)
  # Move scale columns to the front of the tibble ------------------------------
  sdata <- .data %>% 
    dplyr::select(!!scales_en, dplyr::everything())
  # Match scales with norm variables and standardize ---------------------------
  for (i in 1:length(angles)) {
    index <- norms$Angle == angles[i]
    m <- norms$M[index]
    s <- norms$SD[index]
    sdata <- sdata %>% 
      dplyr::mutate_at(dplyr::funs((. - m) / s), .vars = i)
  }
  sdata
}