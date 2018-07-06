#' Perform analyses using the Structural Summary Method
#'
#' Calculate SSM parameters with bootstrapped confidence intervals for a variety
#' of different analysis types. Depending on what arguments are supplied, either
#' mean-based or correlation-based analyses will be performed, one or more
#' groups will be used to stratify the data, and contrasts between groups or
#' measures will be calculated.
#'
#' @param .data Required. A data frame containing at least circumplex scales.
#' @param scales Required. The variable names or column numbers for the
#'   variables in \code{.data} that contain circumplex scales to be analyzed.
#' @param angles Required. A numeric vector containing the angular displacement
#'   of each circumplex scale included in \code{scales} (in degrees).
#' @param measures Optional. The variable names or column numbers for one or
#'   more variables in \code{.data} to be correlated with the circumplex scales
#'   and analyzed using correlation-based SSM analyses. To analyze the
#'   circumplex scales using mean-based analyses, simply omit this argument or
#'   set it to NULL (default = NULL).
#' @param grouping Optional. The variable name or column number for the variable
#'   in \code{.data} that indicates the group membership of each observation. To
#'   analyze all observations in a single group, simply omit this argument or
#'   set it to NULL (default = NULL).
#' @param contrasts Optional. A string indicating what type of contrast to run.
#'   Current options are "none" for no contrasts, "model" to find SSM parameters
#'   for the difference scores, or "test" to find the difference between the SSM
#'   parameters (default = "none").
#' @param boots Optional. A single positive integer indicating how many
#'   bootstrap resamples to use when estimating the confidence intervals
#'   (default = 2000).
#' @param interval Optional. A single positive number between 0 and 1 that
#'   indicates what confidence level to use when estimating the confidence
#'   intevals (default = 0.95).
#' @return A list containing the results and description of the analysis.
#'   \item{results}{A tibble with the SSM parameter estimates} \item{details}{A
#'   list with the sample size for each results row (n), the number of bootstrap
#'   resamples (boots), the confidence interval percentage level (interval), and
#'   the angular displacement of scales (angles)} \item{call}{A language object
#'   containing the function call that created this object} \item{scores}{A
#'   tibble containing the mean scale scores} \item{type}{A string indicating
#'   what type of SSM analysis was done}
#' @family ssm functions
#' @family analysis functions
#' @export

ssm_analyze <- function(.data, scales, angles, measures = NULL, grouping = NULL,
  contrasts = "none", boots = 2000, interval = 0.95) {

  # Check for valid input arguments
  assert_that(is_provided(.data), is_provided(scales), is_provided(angles))
  assert_that(is.numeric(angles), contrasts %in% c("none", "test", "model"))
  assert_that(is.count(boots), is.number(interval), interval > 0, interval < 1)
  
  call = match.call()
  scales_en <- rlang::enquo(scales)
  measures_en <- rlang::enquo(measures)
  grouping_en <- rlang::enquo(grouping)
  
  # Forward to the appropriate subfunction
  if (is_provided(measures)) {
    if (is_provided(grouping)) {
      # Multiple group correlations
      ssm_analyze_corrs(.data,
        scales = !!scales_en, 
        angles = angles, 
        measures = !!measures_en, 
        grouping = !!grouping_en,
        contrasts = contrasts, 
        boots = boots,
        interval = interval,
        call = call)
    } else {
      # Single group correlations
      ssm_analyze_corrs(.data,
        scales = !!scales_en,
        angles = angles, 
        measures = !!measures_en,
        contrasts = contrasts,
        boots = boots,
        interval = interval,
        call = call)
    }
  } else {
    if (is_provided(grouping)) {
      # Multiple group means
      grouping_en <- rlang::enquo(grouping)
      ssm_analyze_means(.data,
        scales = !!scales_en,
        angles = angles,
        grouping = !!grouping_en,
        contrasts = contrasts,
        boots = boots,
        interval = interval,
        call = call)
    } else {
      # Single group means
      if (contrasts != "none") {
        message(c("Error: Without specifying measures or grouping, no ", 
          "contrasts are possible.\n\n  Hint: Set contrasts = 'none' or add ", 
          "the measures or grouping arguments."))
        return()
      }
      ssm_analyze_means(.data,
        scales = !!scales_en,
        angles = angles,
        boots = boots,
        interval = interval,
        call = call)
    }
  }
}


# Perform analyses using the mean-based Structural Summary Method --------------

ssm_analyze_means <- function(.data, scales, angles, grouping,
  contrasts = "none", boots = 2000, interval = 0.95, call) {

  scales_en <- rlang::enquo(scales)

  # Convert angles from degrees to radians
  angles <- angles %>% as_degree() %>% as_radian()

  # Select circumplex scales and grouping variable (if applicable)
  if (is_provided(grouping) == FALSE) {
    bs_input <- .data %>%
      dplyr::select(!!scales_en) %>%
      dplyr::mutate(Group = factor("Whole Sample")) %>%
      tidyr::drop_na()
  } else {
    grouping_en <- rlang::enquo(grouping)
    bs_input <- .data %>%
      dplyr::select(!!scales_en, Group = !!grouping_en) %>%
      dplyr::mutate(Group = factor(Group)) %>%
      tidyr::drop_na()
    # Check if more than one contrast is possible
    if (nlevels(bs_input$Group) > 2 && contrasts != "none") {
      message(c("WARNING: Currently, only one contrast is possible at a time. ",
        "With more than two levels of the grouping variable, only the first ",
        "two levels will be compared."))
    }
  }

  # Check that scales are standardized
  extrema <- bs_input %>%
    dplyr::select(!!scales_en) %>%
    abs() %>%
    max()
  if (extrema >= 5) {
    message(c("WARNING: Your circumplex scales do not appear to be ",
      "standardized.\n\n  Hint: You can standardize these variables with the ",
      "standardize() function."))
  }
  
  # Calculate mean observed scores
  mat <- as.matrix(bs_input[, which(names(bs_input) != "Group")])
  grp <- as.integer(bs_input$Group)
  scores <- group_scores(mat, grp)
  colnames(scores) <- names(dplyr::select(bs_input, !!scales_en))
  rownames(scores) <- levels(bs_input$Group)
  scores <- tibble::as_tibble(scores, rownames = "label")
  
  # Create function that will perform bootstrapping
  bs_function <- function(.data, index, angles, contrasts) {
    resample <- .data[index, ]
    mat <- as.matrix(resample[, which(names(resample) != "Group")])
    grp <- as.integer(resample$Group)
    scores_r <- group_scores(mat, grp)
    ssm_by_group(scores_r, angles, contrasts)
  }

  # Perform bootstrapping
  bs_output <- ssm_bootstrap(
    bs_input = bs_input,
    bs_function = bs_function,
    angles = angles,
    boots = boots,
    interval = interval,
    contrasts = contrasts,
    strata = bs_input$Group
  )

  # Select and label results
  group_levels <- levels(bs_input$Group)
  if (contrasts == "none") {
    row_data <- bs_output
    row_labels <- group_levels
    analysis_n <- group_counts(bs_input)
  } else {
    row_data <- bs_output[nrow(bs_output), ]
    row_labels <- sprintf("%s - %s", group_levels[[2]], group_levels[[1]])
    analysis_n <- sum(group_counts(bs_input))
  }
  results <- row_data %>%
    dplyr::mutate(label = row_labels)

  # Collect analysis details
  details <- list(
    n = analysis_n,
    boots = boots,
    interval = interval,
    angles = as_degree(angles),
    score_type = "Mean",
    results_type = dplyr::if_else(contrasts == "none", "Profile", "Contrast")
  )

  # Create output ssm object
  out <- new_ssm(
    results = results,
    scores = scores,
    call = call,
    details = details
  )

  out
}

# Perform analyses using the correlation-based SSM -----------------------------

ssm_analyze_corrs <- function(.data, scales, angles, measures, grouping, 
  contrasts = "none", boots = 2000, interval = 0.95, call) {

  if (is_provided(grouping)) {
    message("Error: Grouping is not yet implemented for correlation-based", 
      " analyses. This is a top priority that will be added soon. If these",
      " analyses are urgently needed, the original ssm package can do this.")
    return()
  }
  
  scales_en <- rlang::enquo(scales)
  measures_en <- rlang::enquo(measures)
  
  # Convert angles from degrees to radians
  angles <- angles %>% as_degree() %>% as_radian()

  # Select circumplex scales and measure variables
  bs_input <- .data %>%
    dplyr::select(!!scales_en, !!measures_en)

  # Calculate observed scores
  cs <- as.matrix(bs_input[, 1:length(angles)])
  mv <- as.matrix(bs_input[, (length(angles) + 1):ncol(bs_input)])
  scores <- measure_scores(cs, mv)
  colnames(scores) <- names(dplyr::select(bs_input, !!scales_en))
  rownames(scores) <- names(dplyr::select(bs_input, !!measures_en))
  scores <- tibble::as_tibble(scores, rownames = "label")
  
  # Create function that will perform bootstrapping
  bs_function <- function(.data, index, angles, contrasts) {
    resample <- .data[index, ]
    cs <- as.matrix(resample[, 1:length(angles)])
    mv <- as.matrix(resample[, (length(angles) + 1):ncol(resample)])
    scores_r <- measure_scores(cs, mv)
    ssm_by_group(scores_r, angles, contrasts)
  }

  # Perform bootstrapping
  bs_output <- ssm_bootstrap(
    bs_input = bs_input,
    bs_function = bs_function,
    angles = angles,
    boots = boots,
    interval = interval,
    contrasts = contrasts
  )
  
  # Select and label results
  measure_names <- names(dplyr::select(.data, !!measures_en))
  if (contrasts == "none") {
    row_data <- bs_output
    row_labels <- measure_names
  } else {
    row_data <- bs_output[nrow(bs_output), ]
    row_labels <- sprintf("%s - %s", measure_names[[2]], measure_names[[1]])
  }
  results <- row_data %>%
    dplyr::mutate(label = row_labels)

  if (contrasts == "none") {
    analysis_n <- rep(nrow(bs_input), nrow(scores))
  } else {
    analysis_n <- nrow(bs_input) # TODO: Replace with pairwise deletion
  }
  
  # Collect analysis details
  details <- list(
    n = analysis_n,
    boots = boots,
    interval = interval,
    angles = as_degree(angles),
    score_type = "Correlation",
    results_type = dplyr::if_else(contrasts == "none", "Profile", "Contrast")
  )

  # Create output ssm object
  out <- new_ssm(
    results = results,
    scores = scores,
    call = call,
    details = details
  )

  out
}
