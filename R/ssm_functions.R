#' Mean-based Structural Summary Method for Profiles
#'
#' Calculate SSM parameters with bootstrapped confidence intervals for the mean
#' of any number of groups.
#'
#' @param .data A matrix or data frame containing at least circumplex scales,
#'   which should be standardized.
#' @param scales A list of the variables in \code{.data} that contain circumplex
#'   scales (in tidyverse-style NSE specification, see examples).
#' @param angles A numerical vector containing the angular displacement of each
#'   circumplex scale included in \code{scales} (in degrees).
#' @param grouping Optional argument: the variable in \code{.data} that contains
#'   the group membership of each observation. To assess the mean profile of all
#'   observations, do not supply this argument (see examples).
#' @param contrast The type of contrast to run for the first two \code{measures}
#'   specified. Options are "none" to run no contrasts, "model" to calculate SSM
#'   parameters for the difference between each scale score, or "test" to
#'   calculate the difference between each SSM parameter (default = "none").
#' @param table A logical determining whether an HTML table should be output to
#'   display the results of the SSM analysis (default = TRUE).
#' @param xyout A logical determining whether the X-Value and Y-Value parameters
#'   should be included in table and figure output (default = TRUE).
#' @param boots The number of bootstrap resamples to use in calculating the
#'   confidence intervals (default = 2000).
#' @param interval The confidence intervals' percentage level (default = 0.95).
#' @return A tibble containing SSM parameters (point and interval estimates) for
#'   each group's mean profile (or the entire mean profile without grouping).
#' @seealso \code{\link{ssm_measures}}, which calculates SSM parameters for
#'   measures using a correlation-based approach.
#' @export

ssm_profiles <- function(.data, scales, angles, grouping, contrast = "none",
  table = TRUE, xyout = TRUE, boots = 2000, interval = 0.95) {

  # Enable column specification using tidyverse-style NSE ----------------------
  scales_en <- rlang::enquo(scales)

  # Check that inputs are valid ------------------------------------------------
  assert_that(is_provided(.data))
  assert_that(is.numeric(angles), is.count(boots), is.flag(table))
  assert_that(contrast %in% c("none", "model", "test"))
  assert_that(is.scalar(interval), interval > 0, interval < 1)

  # Convert angles from degrees to radians -------------------------------------
  angles <- angles %>% as_degree() %>% as_radian()

  # Select circumplex scales and grouping variable (if applicable) -------------
  if (is_provided(grouping) == FALSE) {
    bs_input <- .data %>%
      dplyr::select(!!scales_en) %>%
      dplyr::mutate(Group = factor("Whole Sample")) %>%
      tidyr::drop_na()
    # Check if contrasts are requested without groups
    if (contrast != "none") {
      stop(c("Contrasts are only possible if a grouping variable is also ",
        "provided.\n\n  Hint: Add a grouping variable or set ",
        "contrast = 'none'."))
    }
  } else {
    grouping_en <- rlang::enquo(grouping)
    bs_input <- .data %>%
      dplyr::select(!!scales_en, Group = !!grouping_en) %>%
      dplyr::mutate(Group = factor(Group)) %>%
      tidyr::drop_na()
    # Check if more than one contrast is possible
    if (nlevels(bs_input$Group) > 2 && contrast != "none") {
      message(c("WARNING: Currently, only one contrast is possible at a time. ",
        "With more than two levels of the grouping variable, only the first ",
        "two levels will be compared."))
    }
  }

  # Check that scales are standardized -----------------------------------------
  extrema <- .data %>%
    dplyr::select(!!scales_en) %>%
    abs() %>%
    max()
  if (extrema >= 5) {
    message(c("WARNING: Your circumplex scales do not appear to be ",
      "standardized.\n\n  Hint: You can standardize these variables with the ",
      "standardize() function."))
  }
  
  # Calculate mean observed scores ---------------------------------------------
  mat <- as.matrix(bs_input[, which(names(bs_input) != "Group")])
  grp <- as.integer(bs_input$Group)
  scores <- group_scores(mat, grp)
  colnames(scores) <- names(dplyr::select(bs_input, !!scales_en))
  rownames(scores) <- levels(bs_input$Group)
  scores <- tibble::as_tibble(scores, rownames = "label")
  
  # Create function that will perform bootstrapping ----------------------------
  bs_function <- function(.data, index, angles, contrast) {
    resample <- .data[index, ]
    mat <- as.matrix(resample[, which(names(resample) != "Group")])
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

  # Label results --------------------------------------------------------------
  row_labels <- levels(bs_input$Group)
  if (contrast != "none") {
    row_labels <- c(row_labels, sprintf(
      "%s - %s", row_labels[2], row_labels[1]
    ))
  }
  results <- bs_output %>%
    dplyr::mutate(label = row_labels)

  # Separate results and contrasts ---------------------------------------------
  if (contrast != "none") {
    contrasts <- results[nrow(results), ]
    results <- results[1:(nrow(results) - 1), ]
  } else {
    contrasts <- NULL
  }

  # Collect analysis details ---------------------------------------------------
  details <- list(
    n = group_counts(bs_input),
    boots = boots,
    interval = interval,
    angles = as_degree(angles)
  )

  # Create output ssm object ---------------------------------------------------
  out <- new_ssm(
    results = results,
    contrasts = contrasts,
    scores = scores,
    call = match.call(),
    details = details,
    type = "Profile"
  )

  # Output HTML results table (if requested) -----------------------------------
  if (table == TRUE) {
    t1 <- ssm_table(out, "results",
      caption = sprintf("Mean-based Structural Summary Statistics with
        %s Confidence Intervals", str_percent(interval)), xyout)
    print(t1)
    if (contrast != "none") {
      t2 <- ssm_table(out, "contrasts", caption = sprintf("Mean-based 
        Structural Summary Statistics with %s Confidence Intervals for 
        Profile Contrasts", str_percent(interval)), xyout)
      print(t2)
    }
  }

  print(out)
  out
}

#' Correlation-based Structural Summary Method for Measures
#'
#' Calculate SSM parameters with bootstrapped confidence intervals for any
#' number of measures, based on their correlations with circumplex scales.
#'
#' @param .data A matrix or data frame containing at least circumplex scales and
#'   one additional measure to correlate with them.
#' @param scales A list of the variables or column numbers in \code{.data} that
#'   contain circumplex scales (in tidyverse-style NSE specification).
#' @param angles A numerical vector containing the angular displacement of each
#'   circumplex scale included in \code{scales} (in degrees).
#' @param measures A list of variables or column numbers in \code{.data} to be
#'   analyzed (in tidyverse-style NSE specification).
#' @param contrast The type of contrast to run for the first two \code{measures}
#'   specified. Options are "none" to run no contrasts, "model" to calculate SSM
#'   parameters for the difference between each scale score, or "test" to
#'   calculate the difference between each SSM parameter (default = "none").
#' @param table A logical determining whether an HTML table should be output to
#'   display the results of the SSM analysis (default = TRUE).
#' @param xyout A logical determining whether the X-Value and Y-Value parameters
#'   should be included in table and figure output (default = TRUE).
#' @param boots The number of bootstrap resamples to use in calculating the
#'   confidence intervals (default = 2000).
#' @param interval The confidence intervals' percentage level (default = 0.95).
#' @return A tibble containing SSM parameters (point and interval estimates) for
#'   each measure.
#' @seealso \code{\link{ssm_profiles}}, which calculates SSM parameters for
#'   profiles using a mean-based approach.
#' @export

ssm_measures <- function(.data, scales, angles, measures, contrast = "none",
  table = TRUE, xyout = TRUE, boots = 2000, interval = 0.95) {

  # Check that inputs are valid ------------------------------------------------
  assert_that(is.numeric(angles), is.count(boots), is.flag(table))
  assert_that(contrast %in% c("none", "model", "test"))
  assert_that(is.scalar(interval), interval > 0, interval < 1)
  assert_that(is_provided(.data), is_provided(scales), is_provided(measures))

  # Enable column specification using tidyverse-style NSE ----------------------
  scales_en <- rlang::enquo(scales)
  measures_en <- rlang::enquo(measures)
  
  # Convert angles from degrees to radians -------------------------------------
  angles <- angles %>% as_degree() %>% as_radian()

  # Select circumplex scales and measure variables -----------------------------
  bs_input <- .data %>%
    dplyr::select(!!scales_en, !!measures_en) %>%
    tidyr::drop_na()

  # Calculate observed scores --------------------------------------------------
  cs <- as.matrix(bs_input[, 1:length(angles)])
  mv <- as.matrix(bs_input[, (length(angles) + 1):ncol(bs_input)])
  scores <- measure_scores(cs, mv)
  colnames(scores) <- names(dplyr::select(bs_input, !!scales_en))
  rownames(scores) <- names(dplyr::select(bs_input, !!measures_en))
  scores <- tibble::as_tibble(scores, rownames = "label")
  
  # Create function that will perform bootstrapping ----------------------------
  bs_function <- function(.data, index, angles, contrast) {
    resample <- .data[index, ]
    cs <- as.matrix(resample[, 1:length(angles)])
    mv <- as.matrix(resample[, (length(angles) + 1):ncol(resample)])
    scores_r <- measure_scores(cs, mv)
    ssm_by_group(scores_r, angles, contrast)
  }

  # Perform bootstrapping ------------------------------------------------------
  bs_output <- ssm_bootstrap(
    bs_input = bs_input,
    bs_function = bs_function,
    angles = angles,
    boots = boots,
    interval = interval,
    contrast = contrast
  )

  # Label results --------------------------------------------------------------
  row_labels <- names(dplyr::select(.data, !!measures_en))
  if (contrast != "none") {
    row_labels <- c(row_labels, sprintf(
      "%s - %s", row_labels[2], row_labels[1]
    ))
  }
  results <- bs_output %>%
    dplyr::mutate(label = row_labels)

  # Separate main results and contrast results ---------------------------------
  if (contrast != "none") {
    contrasts <- results[nrow(results), ]
    results <- results[1:(nrow(results) - 1), ]
  } else {
    contrasts <- NULL
  }

  # Collect analysis details ---------------------------------------------------
  details <- list(
    n = rep(nrow(bs_input), nrow(scores)), # TODO: Add pairwise deletion
    boots = boots,
    interval = interval,
    angles = as_degree(angles)
  )

  # Create output ssm object ---------------------------------------------------
  out <- new_ssm(
    results = results,
    contrasts = contrasts,
    scores = scores,
    call = match.call(),
    details = details,
    type = "Measure"
  )

  # Create HTML results table (if requested) -----------------------------------
  if (table == TRUE) {
    t1 <- ssm_table(out, "results",
      caption = sprintf("Correlation-based Structural Summary Statistics with
        %s Confidence Intervals", str_percent(interval)), xyout)
    print(t1)
    if (contrast != "none") {
      t2 <- ssm_table(out, "contrasts", caption = sprintf("Correlation-based 
        Structural Summary Statistics with %s Confidence Intervals for 
        Measure Contrasts", str_percent(interval)), xyout)
      print(t2)
    }
  }

  print(out)
  out
}
