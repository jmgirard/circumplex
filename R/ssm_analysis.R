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
#' @param angles Optional. A numeric vector containing the angular displacement
#'   of each circumplex scale included in \code{scales} (in degrees). (default =
#'   \code{octants()}).
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
#'   confidence intervals (default = 0.95).
#' @param listwise Optional. A logical indicating whether missing values should
#'   be handled by listwise deletion (TRUE) or pairwise deletion (FALSE). Note
#'   that pairwise deletion may result in different missing data patterns in
#'   each bootstrap resample and is slower to compute (default = TRUE).
#' @param measures_labels Optional. A character vector providing a label for
#'   each measure provided in \code{measures} (in the same order) to appear in
#'   the results as well as tables and plots derived from the results. If
#'   omitted or set to NULL will default to using the measures variable names
#'   (default = NULL).
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
#' # Single-group correlation-based SSM
#' ssm_analyze(jz2017,
#'   scales = PA:NO, angles = octants(),
#'   measures = c(NARPD, ASPD)
#' )
#' \donttest{
#' # Multiple-group mean-based SSM
#' ssm_analyze(jz2017, scales = PA:NO, angles = octants(), grouping = Gender)
#'
#' # Multiple-group mean-based SSM with contrast
#' ssm_analyze(jz2017,
#'   scales = PA:NO, angles = octants(), grouping = Gender,
#'   contrast = "model"
#' )
#'
#' # Single-group correlation-based SSM with contrast
#' ssm_analyze(jz2017,
#'   scales = PA:NO, angles = octants(),
#'   measures = c(NARPD, ASPD), contrast = "test"
#' )
#'
#' ssm_analyze(jz2017,
#'   scales = PA:NO, angles = octants(), measures = c(NARPD, ASPD), 
#'   measures_labels = c("Narcissistic", "Antisocial")
#' )
#'
#' # Multiple-group correlation-based SSM
#' ssm_analyze(jz2017,
#'   scales = PA:NO, angles = octants(), measures = NARPD,
#'   grouping = Gender
#' )
#'
#' # Multiple-group correlation-based SSM with contrast
#' ssm_analyze(jz2017,
#'   scales = PA:NO, angles = octants(), measures = NARPD,
#'   grouping = Gender, contrast = "test"
#' )
#' }
#' 
ssm_analyze <- function(.data, scales, angles = octants(), measures = NULL, 
                        grouping = NULL, contrast = c("none", "test", "model"), 
                        boots = 2000, interval = 0.95, listwise = TRUE,
                        measures_labels = NULL) {
  call <- match.call()
  contrast <- match.arg(contrast)

  # Check for valid input arguments
  assert_that(is_provided(.data), is_provided(angles))
  assert_that(is_provided(rlang::enquo(scales)))
  assert_that(is.numeric(angles), rlang::is_logical(listwise, n = 1))
  assert_that(is.count(boots), is.number(interval), interval > 0, interval < 1)
  assert_that(
    is.null(measures_labels) || 
      rlang::is_character(
        measures_labels, 
        n = count_measures(.data, {{measures}})
      )
    )
  # TODO: Check that scales and angles have same length
  # TODO: Check that grouping is missing, null, or single variable
  # TODO: Add a flag to flip contrast ordering

  # Convert angles from degrees to radians
  angles <- as_radian(as_degree(angles))

  # Forward to the appropriate subfunction
  if (is_provided(rlang::enquo(measures))) {
    if (is_provided(rlang::enquo(grouping))) {
      # Multiple group correlations
      ssm_analyze_corrs(.data,
        scales = {{scales}},
        angles = angles,
        measures = {{measures}},
        grouping = {{grouping}},
        contrast = contrast,
        boots = boots,
        interval = interval,
        listwise = listwise,
        measures_labels = measures_labels,
        call = call
      )
    } else {
      # Single group correlations
      ssm_analyze_corrs(.data,
        scales = {{scales}},
        angles = angles,
        measures = {{measures}},
        contrast = contrast,
        boots = boots,
        interval = interval,
        listwise = listwise,
        measures_labels = measures_labels,
        call = call
      )
    }
  } else {
    if (is_provided(rlang::enquo(grouping))) {
      # Multiple group means
      ssm_analyze_means(.data,
        scales = {{scales}},
        angles = angles,
        grouping = {{grouping}},
        contrast = contrast,
        boots = boots,
        interval = interval,
        listwise = listwise,
        call = call
      )
    } else {
      # Single group means
      if (contrast != "none") {
        stop(c(
          "Without specifying measures or grouping, no contrasts are ",
          "possible.\n\n  Hint: Set contrast = 'none' or add the measures or ",
          "grouping arguments."
        ))
      }
      ssm_analyze_means(.data,
        scales = {{scales}},
        angles = angles,
        boots = boots,
        contrast = contrast,
        interval = interval,
        listwise = listwise,
        call = call
      )
    }
  }
}

# Perform analyses using the mean-based Structural Summary Method --------------

ssm_analyze_means <- function(.data, scales, angles, 
                              grouping = NULL, contrast, 
                              boots, interval, listwise, call) {

  # Select circumplex scales and grouping variable (if applicable)
  if (is_provided(rlang::enquo(grouping))) {
    bs_input <- 
      .data %>%
      dplyr::select({{scales}}, Group = {{grouping}}) %>%
      dplyr::mutate(Group = factor(.$Group))
    # Check if more than one contrast is possible
    if (contrast != "none" && nlevels(bs_input$Group) != 2) {
      stop(c(
        "Only two groups can be contrasted at a time.\n\n  Hint: Set ",
        "contrast = 'none' or use a dichotomous grouping variable."
      ))
    }
  } else {
    bs_input <- 
      .data %>%
      dplyr::select({{scales}}) %>%
      dplyr::mutate(Group = factor("All"))
  }

  # Perform listwise deletion if requested
  if (listwise == TRUE) {
    bs_input <- 
      bs_input %>% 
      tidyr::drop_na()
  }

  # Calculate mean observed scores
  mat <- as.matrix(bs_input[, which(names(bs_input) != "Group")])
  grp <- as.integer(bs_input$Group)
  scores <- mean_scores(mat, grp, listwise)
  colnames(scores) <- names(dplyr::select(bs_input, {{scales}}))
  rownames(scores) <- levels(bs_input$Group)
  scores <- tibble::as_tibble(scores, rownames = "label")
  # TODO: If contrast == "model" then scores should be diff

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
  results <- row_data
  results$label <- row_labels

  # Collect analysis details
  details <- list(
    boots = boots,
    interval = interval,
    listwise = listwise,
    angles = as_degree(angles),
    contrast = contrast,
    score_type = "Mean",
    results_type = ifelse(contrast == "none", "Profile", "Contrast")
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

ssm_analyze_corrs <- function(.data, scales, angles, 
                              measures, grouping = NULL,
                              contrast, boots, interval, listwise, 
                              measures_labels, call) {

  # Select circumplex scales, measure variables, and grouping variable
  if (is_provided(rlang::enquo(grouping))) {
    bs_input <- 
      .data %>%
      dplyr::select({{scales}}, {{measures}}, Group = {{grouping}}) %>%
      dplyr::mutate(Group = factor(.$Group))
  } else {
    bs_input <- 
      .data %>%
      dplyr::select({{scales}}, {{measures}}) %>%
      dplyr::mutate(Group = factor("All"))
  }

  # Check that this combination of arguments is executable
  if (contrast != "none") {
    n_measures <- ncol(dplyr::select(.data, {{measures}}))
    n_groups <- nlevels(bs_input$Group)
    contrast_measures <- (n_measures == 2 && n_groups == 1)
    contrast_groups <- (n_measures == 1 && n_groups == 2)
    valid_contrast <- contrast_measures || contrast_groups
    if (valid_contrast == FALSE) {
      stop(c(
        "No valid contrasts were possible. To contrast measures, ensure ",
        "there are 2 measures and no grouping variable. To contrast groups, ",
        "ensure there is 1 measure and a dichotomous grouping variable."
      ))
    }
  }

  # Perform listwise deletion if requested
  if (listwise == TRUE) {
    bs_input <- tidyr::drop_na(bs_input)
  }

  # Select and label results
  if (is.null(measures_labels)) {
    measure_names <- names(dplyr::select(.data, {{measures}}))
  } else {
    measure_names <- measures_labels
  }
  
  # Calculate observed scores (i.e., correlations)
  cs <- as.matrix(bs_input[, 1:length(angles)])
  mv <- as.matrix(bs_input[, (length(angles) + 1):(ncol(bs_input) - 1)])
  grp <- as.integer(bs_input$Group)
  scores <- corr_scores(cs, mv, grp, listwise)
  colnames(scores) <- names(dplyr::select(bs_input, {{scales}}))
  scores <- 
    tibble::as_tibble(scores) %>%
    dplyr::mutate(
      Group = rep(unique(bs_input$Group), each = ncol(mv)),
      Measure = rep(
        measure_names,
        times = nlevels(bs_input$Group)
      )
    ) %>%
    dplyr::select(Group, Measure, dplyr::everything())
  if (is_provided(rlang::enquo(grouping))) {
    scores <- scores %>%
      dplyr::mutate(label = paste0(.$Group, "_", .$Measure))
  } else {
    scores <- scores %>%
      dplyr::mutate(label = .$Measure)
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

  group_names <- levels(bs_input$Group)
  if (contrast == "none") {
    row_data <- bs_output
    grp_labels <- rep(group_names, each = ncol(mv))
    msr_labels <- rep(measure_names, times = nlevels(bs_input$Group))
    if (is_provided(rlang::enquo(grouping))) {
      lbl_labels <- paste0(grp_labels, "_", msr_labels)
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
      row_labels <- sprintf(
        "%s: %s - %s", measure_names[[1]], group_names[[2]],
        group_names[[1]]
      )
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
    contrast = contrast,
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

#' Calculate Structural Summary Method parameters for a set of scores
#'
#' Calculate SSM parameters (without confidence intervals) for a set of scores
#' and generate a tibble with customizable labels for each parameter value. This
#' function requires the input to be a numeric vector (or coercable to one) and
#' returns only the parameters. See \code{\link{ssm_score}()} for a similar
#' function that calculates SSM parameters for each row of a data frame.
#'
#' @param scores Required. A numeric vector (or single row data frame)
#'   containing one score for each of a set of circumplex scales.
#' @param angles Required. A numeric vector containing the angular displacement
#'   of each circumplex scale included in \code{scores} (in degrees).
#' @param prefix Optional. A string to append to the beginning of all of the SSM
#'   parameters' variable names (default = "").
#' @param suffix Optional. A string to append to the end of all of the SSM
#'   parameters' variable names (default = "").
#' @param e_label Optional. A string representing the variable name of the SSM
#'   elevation parameter (default = "Elev").
#' @param x_label Optional. A string representing the variable name of the SSM
#'   x-value parameter (default = "Xval").
#' @param y_label Optional. A string representing the variable name of the SSM
#'   y-value parameter (default = "Yval").
#' @param a_label Optional. A string representing the variable name of the SSM
#'   amplitude parameter (default = "Ampl").
#' @param d_label Optional. A string representing the variable name of the SSM
#'   displacement parameter (default = "Disp").
#' @param f_label Optional. A string representing the variable name of the SSM
#'   fit or R-squared value (default = "Fit").
#' @return A tibble containing the SSM parameters calculated from \code{scores}.
#' @family ssm functions
#' @family analysis functions
#' @export
#' @examples
#' # Manually enter octant scores
#' scores <- c(0.55, 0.58, 0.62, 0.76, 1.21, 1.21, 1.48, 0.90)
#' ssm_parameters(scores, angles = octants())
#'
#' # Customize several of the labels
#' ssm_parameters(scores, angles = octants(), x_label = "LOV", y_label = "DOM")
#'
#' # Add a prefix to all labels
#' ssm_parameters(scores, angles = octants(), prefix = "IIP_")
#' 
ssm_parameters <- function(scores, angles, prefix = "", suffix = "", 
                           e_label = "Elev", x_label = "Xval", y_label = "Yval",
                           a_label = "Ampl", d_label = "Disp", f_label = "Fit") {

  assert_that(is_numvec(scores), is_numvec(angles))
  scores <- scores %>% as.numeric()
  assert_that(
    length(scores) == length(angles), 
    msg = "The 'scores' and 'angles' arguments must have the same length."
  )
  assert_that(
    rlang::is_character(prefix), rlang::is_character(suffix),
    msg = "The 'prefix' and 'suffix' arguments must be a string."
  )
  assert_that(
    rlang::is_character(e_label), rlang::is_character(x_label), 
    rlang::is_character(y_label), rlang::is_character(a_label), 
    rlang::is_character(d_label), rlang::is_character(f_label),
    msg = "The 'label' arguments must be strings."
  )
  
  
  angles <- angles %>% as_degree() %>% as_radian()
  params <- ssm_parameters_cpp(scores, angles)
  params[[5]] <- params[[5]] %>% as_radian() %>% as_degree()
  
  tibble::tibble(
    !!rlang::sym(paste0(prefix, e_label, suffix)) := params[[1]],
    !!rlang::sym(paste0(prefix, x_label, suffix)) := params[[2]],
    !!rlang::sym(paste0(prefix, y_label, suffix)) := params[[3]],
    !!rlang::sym(paste0(prefix, a_label, suffix)) := params[[4]],
    !!rlang::sym(paste0(prefix, d_label, suffix)) := params[[5]],
    !!rlang::sym(paste0(prefix, f_label, suffix)) := params[[6]]
  )
}

#' Calculate SSM parameters by row and add results as new columns
#'
#' Calculate the SSM parameters for each row of a data frame and add the results
#' as additional columns. This can be useful when the SSM is being used for the
#' description or visualization of individual data points rather than for
#' statistical inference on groups of data points.
#' 
#' @param .data Required. A data frame containing at least circumplex scales.
#' @param scales Required. The variable names or column numbers for the
#'   variables in \code{.data} that contain circumplex scales to be analyzed.
#' @param angles Required. A numeric vector containing the angular displacement
#'   of each circumplex scale included in \code{scales} (in degrees).
#' @param ... Optional. Additional parameters to pass to
#'   \code{\link{ssm_parameters}()}, such as \code{prefix} and \code{suffix}.
#' @return A data frame containing \code{.data} plus six additional columns
#'   containing the SSM parameters (calculated rowwise).
#' @family ssm functions
#' @family analysis functions
#' @export
#' @examples
#' data("aw2009")
#' ssm_score(aw2009, scales = PA:NO, angles = octants())
#' 
ssm_score <- function(.data, scales, angles, ...) {

  assert_that(is_provided(.data), is_provided(angles))
  assert_that(is_provided(rlang::enquo(scales)))
  
  scales_mat <- .data %>%
    dplyr::select({{scales}})
  scale_names <- names(scales_mat)
  df_params <- scales_mat %>% 
    dplyr::mutate(Group = 1:nrow(.)) %>% 
    tidyr::gather(key = "Scale", value = "Score", -Group) %>% 
    dplyr::mutate(Scale = factor(.$Scale, levels = scale_names)) %>% 
    tidyr::spread(key = Group, value = Score) %>% 
    dplyr::select(-Scale) %>% 
    purrr::map_dfr(ssm_parameters, angles = angles, ...)
  dplyr::bind_cols(.data, df_params)
}

