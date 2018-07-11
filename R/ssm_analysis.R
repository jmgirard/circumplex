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
#' @param contrast Optional. A string indicating what type of contrast to run.
#'   Current options are "none" for no contrast, "model" to find SSM parameters
#'   for the difference scores, or "test" to find the difference between the SSM
#'   parameters. Note that only two groups or measures can be contrasted at a
#'   time (default = "none").
#' @param boots Optional. A single positive integer indicating how many
#'   bootstrap resamples to use when estimating the confidence intervals
#'   (default = 2000).
#' @param interval Optional. A single positive number between 0 and 1
#'   (exclusive) that indicates what confidence level to use when estimating the
#'   confidence intevals (default = 0.95).
#' @param listwise Optional. A logical indicating whether missing values should
#'   be handled by listwise deletion (TRUE) or pairwise deletion (FALSE). Note
#'   that pairwise deletion may result in different missing data patterns in
#'   each bootstrap resample and is slower to compute (default = TRUE).
#' @return A list containing the results and description of the analysis.
#'   \item{results}{A tibble with the SSM parameter estimates} \item{details}{A
#'   list with the number of bootstrap resamples (boots), the confidence
#'   interval percentage level (interval), and the angular displacement of
#'   scales (angles)} \item{call}{A language object containing the function call
#'   that created this object} \item{scores}{A tibble containing the mean scale
#'   scores} \item{type}{A string indicating what type of SSM analysis was done}
#' @family ssm functions
#' @family analysis functions
#' @export
#' @examples
#' # Load example data
#' data("jz2017")
#' 
#' # Single-group mean-based SSM
#' ssm_analyze(jz2017, scales = PA:NO, angles = octants())
#' 
#' # Multiple-group mean-based SSM
#' ssm_analyze(jz2017, scales = PA:NO, angles = octants(), grouping = Gender)
#' 
#' # Multiple-group mean-based SSM with contrast
#' ssm_analyze(jz2017, scales = PA:NO, angles = octants(), grouping = Gender,
#'   contrast = "model")
#'   
#' # Single-group correlation-based SSM
#' ssm_analyze(jz2017, scales = PA:NO, angles = octants(), 
#'   measures = c(NARPD, ASPD))
#'   
#' # Single-group correlation-based SSM with contrast
#' ssm_analyze(jz2017, scales = PA:NO, angles = octants(),
#'   measures = c(NARPD, ASPD), contrast = "test")
#' \donttest{
#' # Multiple-group correlation-based SSM
#' ssm_analyze(jz2017, scales = PA:NO, angles = octants(), measures = NARPD,
#'   grouping = Gender)
#'   
#' # Multiple-group correlation-based SSM with contrast
#' ssm_analyze(jz2017, scales = PA:NO, angles = octants(), measures = NARPD,
#'   grouping = Gender, contrast = "test")
#' }

ssm_analyze <- function(.data, scales, angles, measures = NULL, grouping = NULL,
  contrast = "none", boots = 2000, interval = 0.95, listwise = TRUE) {

  call = match.call()
  
  # Enable tidy evaluation
  scales_en <- rlang::enquo(scales)
  measures_en <- rlang::enquo(measures)
  grouping_en <- rlang::enquo(grouping)
  
  # Check for valid input arguments
  assert_that(is_provided(.data), is_provided(angles))
  assert_that(is_enquo(!!scales_en))
  assert_that(is.numeric(angles), contrast %in% c("none", "test", "model"))
  assert_that(is.count(boots), is.number(interval), interval > 0, interval < 1)
  assert_that(is.flag(listwise))
  # TODO: Check that scales and angles have same length
  # TODO: Check that grouping is missing, null, or single variable
  # TODO: Add a flag to flip contrast ordering
  
  # Convert angles from degrees to radians
  angles <- angles %>% as_degree() %>% as_radian()
  
  # Forward to the appropriate subfunction
  if (is_enquo(!!measures_en)) {
    if (is_enquo(!!grouping_en)) {
      # Multiple group correlations
      ssm_analyze_corrs(.data,
        scales = !!scales_en, 
        angles = angles, 
        measures = !!measures_en, 
        grouping = !!grouping_en,
        contrast = contrast, 
        boots = boots,
        interval = interval,
        listwise = listwise,
        call = call)
    } else {
      # Single group correlations
      ssm_analyze_corrs(.data,
        scales = !!scales_en,
        angles = angles, 
        measures = !!measures_en,
        contrast = contrast,
        boots = boots,
        interval = interval,
        listwise = listwise,
        call = call)
    }
  } else {
    if (is_enquo(!!grouping_en)) {
      # Multiple group means
      ssm_analyze_means(.data,
        scales = !!scales_en,
        angles = angles,
        grouping = !!grouping_en,
        contrast = contrast,
        boots = boots,
        interval = interval,
        listwise = listwise,
        call = call)
    } else {
      # Single group means
      if (contrast != "none") {
        stop(c("Error: Without specifying measures or grouping, no ", 
          "contrasts are possible.\n\n  Hint: Set contrast = 'none' or add ", 
          "the measures or grouping arguments."))
      }
      ssm_analyze_means(.data,
        scales = !!scales_en,
        angles = angles,
        boots = boots,
        contrast = contrast,
        interval = interval,
        listwise = listwise,
        call = call)
    }
  }
}

# Perform analyses using the mean-based Structural Summary Method --------------

ssm_analyze_means <- function(.data, scales, angles, grouping, contrast,
  boots, interval, listwise, call) {

  # Enable tidy evaluation
  scales_en <- rlang::enquo(scales)
  grouping_en <- rlang::enquo(grouping)

  # Select circumplex scales and grouping variable (if applicable)
  if (is_enquo(!!grouping_en)) {
    bs_input <- .data %>%
      dplyr::select(!!scales_en, Group = !!grouping_en) %>%
      dplyr::mutate(Group = factor(Group))
    # Check if more than one contrast is possible
    if (contrast != "none" && nlevels(bs_input$Group) != 2) {
      stop(c("Only two groups can be contrasted at a time.\n\n  Hint: Set ",
        "contrast = 'none' or use a dichotomous grouping variable."))
    }
  } else {
    bs_input <- .data %>%
      dplyr::select(!!scales_en) %>%
      dplyr::mutate(Group = factor("All"))
  }

  # Perform listwise deletion if requested
  if (listwise == TRUE) {
    bs_input <- bs_input %>% tidyr::drop_na()
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
  scores <- mean_scores(mat, grp, listwise)
  colnames(scores) <- names(dplyr::select(bs_input, !!scales_en))
  rownames(scores) <- levels(bs_input$Group)
  scores <- tibble::as_tibble(scores, rownames = "label")
  
  # Create function that will perform bootstrapping
  bs_function <- function(.data, index, angles, contrast, listwise) {
    resample <- .data[index, ]
    mat <- as.matrix(resample[, which(names(resample) != "Group")])
    grp <- as.integer(resample$Group)
    scores_r <- mean_scores(mat, grp, listwise)
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
    listwise = listwise,
    strata = bs_input$Group
  )

  # Select and label results
  group_levels <- levels(bs_input$Group)
  if (contrast == "none") {
    row_data <- bs_output
    row_labels <- group_levels
  } else {
    row_data <- bs_output[nrow(bs_output), ]
    row_labels <- sprintf("%s - %s", group_levels[[2]], group_levels[[1]])
  }
  results <- row_data %>%
    dplyr::mutate(label = row_labels)

  # Collect analysis details
  details <- list(
    boots = boots,
    interval = interval,
    listwise = listwise,
    angles = as_degree(angles),
    score_type = "Mean",
    results_type = dplyr::if_else(contrast == "none", "Profile", "Contrast")
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
  contrast, boots, interval, listwise, call) {

  # Enable tidy evaluation
  scales_en <- rlang::enquo(scales)
  measures_en <- rlang::enquo(measures)
  grouping_en <- rlang::enquo(grouping)

  # Select circumplex scales, measure variables, and grouping variable
  if (is_enquo(!!grouping_en)) {
    bs_input <- .data %>%
      dplyr::select(!!scales_en, !!measures_en, Group = !!grouping_en) %>% 
      dplyr::mutate(Group = factor(Group))
  } else {
    bs_input <- .data %>% 
      dplyr::select(!!scales_en, !!measures_en) %>% 
      dplyr::mutate(Group = factor("All"))
  }

  # Check that this combination of arguments is executable
  if (contrast != "none") {
    n_measures <- ncol(dplyr::select(.data, !!measures_en)) 
    n_groups <- nlevels(bs_input$Group)
    contrast_measures <- (n_measures == 2 && n_groups == 1)
    contrast_groups <- (n_measures == 1 && n_groups == 2)
    valid_contrast <- contrast_measures || contrast_groups
    if (valid_contrast == FALSE) {
      stop(c("No valid contrasts were possible. To contrast measures, ensure ",
        "there are 2 measures and no grouping variable. To contrast groups, ",
        "ensure there is 1 measure and a dichotomous grouping variable."))
    }
  }
  
  # Perform listwise deletion if requested
  if (listwise == TRUE) {
    bs_input <- bs_input %>% tidyr::drop_na()
  }

  # Calculate observed scores (i.e., correlations)
  cs <- as.matrix(bs_input[, 1:length(angles)])
  mv <- as.matrix(bs_input[, (length(angles) + 1):(ncol(bs_input) - 1)])
  grp <- as.integer(bs_input$Group)
  scores <- corr_scores(cs, mv, grp, listwise)
  colnames(scores) <- names(dplyr::select(bs_input, !!scales_en))
  scores <- tibble::as_tibble(scores) %>%
    dplyr::mutate(
      Group = rep(unique(bs_input$Group), each = ncol(mv)),
      Measure = rep(names(dplyr::select(bs_input, !!measures_en)),
        times = nlevels(bs_input$Group))
      ) %>% 
    dplyr::select(Group, Measure, dplyr::everything())
  if (is_enquo(!!grouping_en)) {
    scores <- scores %>% 
      dplyr::mutate(label = paste0(Group, "_", Measure))
  } else {
    scores <- scores %>% 
      dplyr::mutate(label = Measure)
  }
  
  # Create function that will perform bootstrapping
  bs_function <- function(.data, index, angles, contrast, listwise) {
    resample <- .data[index, ]
    grp <- as.integer(resample$Group)
    cs <- as.matrix(resample[, 1:length(angles)])
    mv <- as.matrix(resample[, (length(angles) + 1):(ncol(resample) - 1)])
    scores_r <- corr_scores(cs, mv, grp, listwise)
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
    listwise = listwise,
    strata = bs_input$Group
  )
  
  # Select and label results
  measure_names <- names(dplyr::select(.data, !!measures_en))
  group_names <- levels(bs_input$Group)
  if (contrast == "none") {
    row_data <- bs_output
    grp_labels <- rep(group_names, each = ncol(mv))
    msr_labels <- rep(measure_names, times = nlevels(bs_input$Group))
    if (is_enquo(!!grouping_en)) {
      lbl_labels <-  paste0(grp_labels, "_", msr_labels)
    } else {
      lbl_labels <- msr_labels
    }
    results <- row_data %>%
      dplyr::mutate(
        label = lbl_labels,
        Group = grp_labels,
        Measure = msr_labels
      ) %>% 
      dplyr::select(Group, Measure, dplyr::everything(), label)
  } else {
    row_data <- bs_output[nrow(bs_output), ]
    if (contrast_measures) {
      row_labels <- sprintf("%s - %s", measure_names[[2]], measure_names[[1]])
    } else if (contrast_groups) {
      row_labels <- sprintf("%s: %s - %s", measure_names[[1]], group_names[[2]],
         group_names[[1]])
    }
    results <- row_data %>%
      dplyr::mutate(label = row_labels) %>% 
      dplyr::select(label, dplyr::everything())
  }

  # Collect analysis details
  details <- list(
    boots = boots,
    interval = interval,
    listwise = listwise,
    angles = as_degree(angles),
    score_type = "Correlation",
    results_type = dplyr::if_else(contrast == "none", "Profile", "Contrast")
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
