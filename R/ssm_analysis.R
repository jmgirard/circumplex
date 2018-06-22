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
#'   circumplex scales using mean-based analyses, simply omit this argument.
#' @param grouping Optional. The variable name or column number for the variable
#'   in \code{.data} that indicates the group membership of each observation. To
#'   analyze all observations in a single group, simply omit this argument.
#' @param contrast Optional. A string indicating what type of contrast to run.
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
#'   \item{results}{A tibble with the SSM parameter estimates for each group}
#'   \item{contrasts}{A tibble with the SSM parameter contrasts}
#'   \item{details}{A list with the sample size for each results row (n), the
#'   number of bootstrap resamples (boots), the confidence interval percentage
#'   level (interval), and the angular displacement of scales (angles)}
#'   \item{call}{A language object containing the function call that created
#'   this object} \item{scores}{A tibble containing the mean scale scores}
#'   \item{type}{A string indicating what type of SSM analysis was done}
#' @family ssm functions
#' @family analysis functions
#' @export

ssm_analyze <- function(.data, scales, angles, measures, grouping,
  contrast = "none", boots = 2000, interval = 0.95) {

  # Check for valid input arguments
  assert_that(is_provided(.data), is_provided(scales), is_provided(angles))
  assert_that(is.numeric(angles), contrast %in% c("none", "test", "model"))
  assert_that(is.count(boots), is.number(interval), interval > 0, interval < 1)
  
  call = match.call()
  
  # Forward to the appropriate subfunction
  scales_en <- rlang::enquo(scales)
  if (is_provided(measures)) {
    measures_en <- rlang::enquo(measures)
    if (is_provided(grouping)) {
      #Type 1: Multiple group correlations
      grouping_en <- rlang::enquo(grouping)
      ssm_analyze_corrs(.data,
        scales = !!scales_en, 
        angles = angles, 
        measures = !!measures_en, 
        grouping = !!grouping_en,
        contrast = contrast, 
        boots = boots,
        interval = interval,
        call = call)
    } else {
      # Type 2: Single group correlations
      ssm_analyze_corrs(.data,
        scales = !!scales_en,
        angles = angles, 
        measures = !!measures_en,
        contrast = contrast,
        boots = boots,
        interval = interval,
        call = call)
    }
  } else {
    if (is_provided(grouping)) {
      # Type 3: Multiple group means
      grouping_en <- rlang::enquo(grouping)
      ssm_analyze_means(.data,
        scales = !!scales_en,
        angles = angles,
        grouping = !!grouping_en,
        contrast = contrast,
        boots = boots,
        interval = interval,
        call = call)
    } else {
      # Type 4: Single group means
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
  contrast = "none", boots = 2000, interval = 0.95, call) {

  print(is_provided(grouping))
  scales_en <- rlang::enquo(scales)

  # Convert angles from degrees to radians
  angles <- angles %>% as_degree() %>% as_radian()

  # Select circumplex scales and grouping variable (if applicable)
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

  # Check that scales are standardized
  extrema <- .data %>%
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
  bs_function <- function(.data, index, angles, contrast) {
    resample <- .data[index, ]
    mat <- as.matrix(resample[, which(names(resample) != "Group")])
    grp <- as.integer(resample$Group)
    scores_r <- group_scores(mat, grp)
    ssm_by_group(scores_r, angles, contrast)
  }

  # Perform bootstrapping
  bs_output <- ssm_bootstrap(
    bs_input = bs_input,
    bs_function = bs_function,
    angles = angles,
    boots = boots,
    interval = interval,
    contrast = contrast,
    strata = bs_input$Group
  )

  # Label results
  row_labels <- levels(bs_input$Group)
  if (contrast != "none") {
    row_labels <- c(row_labels, sprintf(
      "%s - %s", row_labels[2], row_labels[1]
    ))
  }
  results <- bs_output %>%
    dplyr::mutate(label = row_labels)

  # Separate results and contrasts
  if (contrast != "none") {
    contrasts <- results[nrow(results), ]
    results <- results[1:(nrow(results) - 1), ]
  } else {
    contrasts <- NULL
  }

  # Collect analysis details
  details <- list(
    n = group_counts(bs_input),
    boots = boots,
    interval = interval,
    angles = as_degree(angles)
  )

  # Create output ssm object
  out <- new_ssm(
    results = results,
    contrasts = contrasts,
    scores = scores,
    call = call,
    details = details,
    type = "means"
  )

  out
}

# Perform analyses using the correlation-based SSM -----------------------------

ssm_analyze_corrs <- function(.data, scales, angles, measures, grouping, 
  contrast = "none", boots = 2000, interval = 0.95, call) {

  scales_en <- rlang::enquo(scales)
  measures_en <- rlang::enquo(measures)
  
  # Convert angles from degrees to radians
  angles <- angles %>% as_degree() %>% as_radian()

  # Select circumplex scales and measure variables
  bs_input <- .data %>%
    dplyr::select(!!scales_en, !!measures_en) %>%
    tidyr::drop_na() # TODO: Replace w/pairwise deletion

  # Calculate observed scores
  cs <- as.matrix(bs_input[, 1:length(angles)])
  mv <- as.matrix(bs_input[, (length(angles) + 1):ncol(bs_input)])
  scores <- measure_scores(cs, mv)
  colnames(scores) <- names(dplyr::select(bs_input, !!scales_en))
  rownames(scores) <- names(dplyr::select(bs_input, !!measures_en))
  scores <- tibble::as_tibble(scores, rownames = "label")
  
  # Create function that will perform bootstrapping
  bs_function <- function(.data, index, angles, contrast) {
    resample <- .data[index, ]
    cs <- as.matrix(resample[, 1:length(angles)])
    mv <- as.matrix(resample[, (length(angles) + 1):ncol(resample)])
    scores_r <- measure_scores(cs, mv)
    ssm_by_group(scores_r, angles, contrast)
  }

  # Perform bootstrapping
  bs_output <- ssm_bootstrap(
    bs_input = bs_input,
    bs_function = bs_function,
    angles = angles,
    boots = boots,
    interval = interval,
    contrast = contrast
  )

  # Label results
  row_labels <- names(dplyr::select(.data, !!measures_en))
  if (contrast != "none") {
    row_labels <- c(row_labels, sprintf(
      "%s - %s", row_labels[2], row_labels[1]
    ))
  }
  results <- bs_output %>%
    dplyr::mutate(label = row_labels)

  # Separate main results and contrast results
  if (contrast != "none") {
    contrasts <- results[nrow(results), ]
    results <- results[1:(nrow(results) - 1), ]
  } else {
    contrasts <- NULL
  }

  # Collect analysis details
  details <- list(
    n = rep(nrow(bs_input), nrow(scores)), # TODO: Replace w/pairwise deletion
    boots = boots,
    interval = interval,
    angles = as_degree(angles)
  )

  # Create output ssm object
  out <- new_ssm(
    results = results,
    contrasts = contrasts,
    scores = scores,
    call = call,
    details = details,
    type = "corrs"
  )

  out
}
