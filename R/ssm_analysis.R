#' Perform analyses using the Structural Summary Method
#'
#' Calculate SSM parameters with bootstrapped confidence intervals for a variety
#' of different analysis types. Depending on what arguments are supplied, either
#' mean-based or correlation-based analyses will be performed, one or more
#' groups will be used to stratify the data, and contrasts between groups or
#' measures will be calculated.
#'
#' @param data Required. A data frame containing at least circumplex scales.
#' @param scales Required. A character vector of column names, or a numeric
#'   vector of column indexes, from `data` that contains the circumplex scale
#'   scores to be analyzed.
#' @param angles Optional. A numeric vector containing the angular displacement
#'   of each circumplex scale included in `scales` (in degrees). (default =
#'   `octants()`).
#' @param measures Optional. Either `NULL` or a character vector of column names
#'   from `data` that contains one or more variables to be correlated with the
#'   circumplex scales and analyzed using correlation-based SSM analyses.
#' @param grouping Optional. Either `NULL` or a string that contains the column
#'   name from `data` of the variable that indicates the group membership of
#'   each observation.
#' @param contrast Optional. A logical indicating whether to output the
#'   difference between two measures' or two groups' SSM parameters. Can only be
#'   set to TRUE when there are exactly two measures and one group, one measure
#'   and two groups, or no measures and two groups (default = FALSE).
#' @param boots Optional. A single positive whole number indicating how many
#'   bootstrap resamples to use when estimating the confidence intervals
#'   (default = 2000).
#' @param interval Optional. A single positive number between 0 and 1
#'   (exclusive) that indicates what confidence level to use when estimating the
#'   confidence intervals (default = 0.95).
#' @param listwise Optional. A logical indicating whether missing values should
#'   be handled by listwise deletion (TRUE) or pairwise deletion (FALSE). Note
#'   that pairwise deletion may result in different missing data patterns in
#'   each bootstrap resample and is slower to compute (default = TRUE).
#' @param measures_labels Optional. Either `NULL` or a character vector
#'   providing a label for each measure provided in `measures` (in the same
#'   order) to appear in the results as well as tables and plots derived from
#'   the results.
#' @return A list containing the results and description of the analysis.
#'   \item{results}{A data frame with the SSM parameter estimates}
#'   \item{details}{A list with the number of bootstrap resamples (boots),
#'   the confidence interval percentage level (interval), and the angular
#'   displacement of scales (angles)}
#'   \item{call}{A language object containing the function call that created
#'   this object}
#'   \item{scores}{A data frame containing the mean scale scores} \item{type}{A
#'   string indicating what type of SSM analysis was done}
#' @family ssm functions
#' @family analysis functions
#' @export
#' @examples
#' # Load example data
#' data("jz2017")
#'
#' # Single-group mean-based SSM
#' ssm_analyze(
#'   jz2017,
#'   scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
#' )
#'
#' # Single-group correlation-based SSM
#' ssm_analyze(
#'   jz2017,
#'   scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
#'   measures = c("NARPD", "ASPD")
#' )
#' \donttest{
#' # Multiple-group mean-based SSM
#' ssm_analyze(
#'   jz2017,
#'   scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
#'   grouping = "Gender"
#' )
#'
#' # Multiple-group mean-based SSM with contrast
#' ssm_analyze(
#'   jz2017,
#'   scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
#'   grouping = "Gender",
#'   contrast = TRUE
#' )
#'
#' # Single-group correlation-based SSM with contrast
#' ssm_analyze(
#'   jz2017,
#'   scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
#'   measures = c("NARPD", "ASPD"),
#'   contrast = TRUE
#' )
#'
#' # Multiple-group correlation-based SSM
#' ssm_analyze(
#'   jz2017,
#'   scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
#'   measures = "NARPD",
#'   grouping = "Gender"
#' )
#'
#' # Multiple-group correlation-based SSM with contrast
#' ssm_analyze(
#'   jz2017,
#'   scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
#'   measures = "NARPD",
#'   grouping = "Gender",
#'   contrast = TRUE
#' )
#' }
#' 
ssm_analyze <- function(data, scales, angles = octants(), 
                        measures = NULL, grouping = NULL, contrast = FALSE, 
                        boots = 2000, interval = 0.95, listwise = TRUE,
                        measures_labels = NULL) {
  
  # Save function call
  call <- match.call()
  
  # Validate arguments
  stopifnot(is.data.frame(data) || is.matrix(data))
  stopifnot(is_var(scales))
  stopifnot(is.numeric(angles))
  stopifnot(length(scales) == length(angles))
  stopifnot(is_null_or_var(measures))
  stopifnot(is_null_or_var(grouping, n = 1))
  stopifnot(is_flag(contrast))
  stopifnot(is.numeric(boots) && boots > 0 && ceiling(boots) == floor(boots))
  stopifnot(is.numeric(interval) && interval > 0 && interval < 1)
  stopifnot(is_flag(listwise))
  stopifnot(is_null_or_char(measures_labels, n = length(measures)))

  if (contrast) {
    n_measures <- length(measures)
    n_groups <- ifelse(is.null(grouping), 1, nlevels(factor(data[[grouping]])))
    group_mean_contrast <- n_measures == 0 && n_groups == 2
    group_corr_contrast <- n_measures == 1 && n_groups == 2
    measure_corr_contrast <- n_measures == 2 && n_groups == 1
    if (!any(group_mean_contrast, group_corr_contrast, measure_corr_contrast)) {
      stop("Contrast can only be TRUE when comparing 2 groups or 2 measures.")
    }
  }
  
  # Convert angles from degrees to radians
  angles <- as_radian(as_degree(angles))

  # Forward to the appropriate subfunction
  if (is.null(measures)) {
    # No Measures = Mean Analysis
    ssm_analyze_means(
      data = data,
      scales = scales,
      angles = angles,
      grouping = grouping,
      contrast = contrast,
      boots = boots,
      interval = interval,
      listwise = listwise,
      call = call
    )
  } else {
    # Measures = Correlation Analysis
    ssm_analyze_corrs(
      data = data,
      scales = scales,
      angles = angles,
      measures = measures,
      grouping = grouping,
      contrast = contrast,
      boots = boots,
      interval = interval,
      listwise = listwise,
      measures_labels = measures_labels,
      call = call
    )
  }
}

# Perform analyses using the mean-based Structural Summary Method --------------

ssm_analyze_means <- function(data, scales, angles, grouping, contrast, 
                              boots, interval, listwise, call) {
  
  # Select circumplex scales and grouping variable (if applicable)
  bs_input <- data[scales]
  scales_names <- colnames(bs_input)
  if (is.null(grouping)) {
    bs_input <- cbind(bs_input, Group = rep("All", times = nrow(data)))
  } else {
    Group <- data[grouping]
    colnames(Group) <- "Group"
    bs_input <- cbind(bs_input, Group)
  }
  
  # Perform listwise deletion if requested
  if (listwise) {
    bs_input <- stats::na.omit(bs_input)
  }
  
  # Set group to factor
  bs_input[[ncol(bs_input)]] <- factor(bs_input[[ncol(bs_input)]])
  
  # Get counts
  n_scales <- length(scales)
  n_groups <- nlevels(bs_input[[ncol(bs_input)]])
  group_levels <- levels(bs_input[[ncol(bs_input)]])
  
  # Calculate mean observed scores
  mat <- as.matrix(bs_input[scales_names])
  grp <- as.integer(bs_input[[ncol(bs_input)]])
  scores <- mean_scores(mat, grp, listwise)
  colnames(scores) <- scales_names
  if (contrast) {
    scores <- rbind(scores, scores[2, ] - scores[1, ])
    scores <- cbind(
      label = c(group_levels, paste0(group_levels[[2]], " - ", group_levels[[1]])),
      as.data.frame(scores)
    )
  } else {
    scores <- cbind(label = group_levels, as.data.frame(scores))
  }

  # Create function that will perform bootstrapping
  bs_function <- function(.data, index, scales, angles, contrast, listwise, ...) {
    resample <- .data[index, ]
    mat <- as.matrix(resample[scales])
    grp <- as.integer(resample[[ncol(resample)]])
    scores_r <- mean_scores(mat, grp, listwise)
    ssm_by_group(scores_r, angles, contrast)
  }
  
  # Perform bootstrapping
  bs_output <- ssm_bootstrap(
    bs_input = bs_input,
    bs_function = bs_function,
    scales = scales_names,
    angles = angles,
    boots = boots,
    interval = interval,
    contrast = contrast,
    listwise = listwise,
    strata = bs_input[[ncol(bs_input)]]
  )
  
  # Select and label results
  row_labels <- group_levels
  if (contrast) {
    row_labels <- c(
      row_labels, 
      paste0(group_levels[[2]], " - ", group_levels[[1]])
    )
  }
  results <- cbind(label = row_labels, bs_output)
  
  # Collect analysis details
  details <- list(
    boots = boots,
    interval = interval,
    listwise = listwise,
    angles = as_degree(angles),
    contrast = contrast,
    score_type = "Mean"
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

ssm_analyze_corrs <- function(data, scales, angles, measures, grouping,
                              contrast, boots, interval, listwise, 
                              measures_labels, call) {
  
  # Select only the scales, measures, and grouping variables
  scales_data <- data[scales]
  scales_names <- colnames(scales_data)
  measures_data <- data[measures]
  measures_names <- colnames(measures_data)
  bs_input <- cbind(scales_data, measures_data)
  if (is.null(grouping)) {
    newcol <- data.frame(Group = rep("All", nrow(data)))
    bs_input <- cbind(bs_input, newcol)
  } else {
    newcol <- data[grouping]
    colnames(newcol) <- "Group"
    bs_input <- cbind(bs_input, newcol)
  }
  
  # Perform listwise deletion if requested
  if (listwise == TRUE) {
    bs_input <- stats::na.omit(bs_input)
  }
  
  # Set group as factor
  bs_input[[ncol(bs_input)]] <- factor(bs_input[[ncol(bs_input)]])
  
  # Get counts
  n_scales <- length(scales)
  n_measures <- length(measures)
  n_groups <- nlevels(bs_input$Group)
  
  # Get names of measures (using labels if provided)
  if (is.null(measures_labels)) {
    measures_labels <- measures_names
  }
  
  # Calculate observed correlation scores
  cs <- as.matrix(bs_input[scales_names])
  mv <- as.matrix(bs_input[measures_names])
  grp <- as.integer(bs_input[[ncol(bs_input)]])
  scores <- corr_scores(cs, mv, grp, listwise)
  colnames(scores) <- scales_names
  group_levels <- levels(bs_input[[ncol(bs_input)]])
  if (contrast) {
    scores <- rbind(scores, scores[2, ] - scores[1, ])
  }
  scores <- as.data.frame(scores)
  Group <- rep(group_levels, each = n_measures)
  Measure <- rep(measures_labels, times = n_groups)
  if (contrast && is.null(grouping)) {
    Group <- c(Group, Group[[1]])
    Measure <- c(Measure, paste0(Measure[[2]], " - ", Measure[[1]]))
  } else if (contrast && !is.null(grouping)) {
    Group <- c(Group, paste0(Group[[2]], " - ", Group[[1]]))
    Measure <- c(Measure, Measure[[1]])
  }
  scores <- cbind(Group, Measure, scores)
  if (is.null(grouping)) {
    scores$label <- Measure
  } else {
    scores$label <- paste0(Measure, ": ", Group)
  }

  # Create function that will perform bootstrapping
  bs_function <- function(.data, index, scales, measures, angles, contrast, 
                          listwise, ...) {
    resample <- .data[index, ]
    cs <- as.matrix(resample[scales])
    mv <- as.matrix(resample[measures])
    grp <- as.integer(resample[[ncol(resample)]])
    scores_r <- corr_scores(cs, mv, grp, listwise)
    ssm_by_group(scores_r, angles, contrast)
  }
  
  # Perform bootstrapping
  bs_output <- ssm_bootstrap(
    bs_input = bs_input,
    bs_function = bs_function,
    scales = scales_names,
    measures = measures_names,
    angles = angles,
    boots = boots,
    interval = interval,
    contrast = contrast,
    listwise = listwise,
    strata = bs_input$Group
  )
  
  Group <- rep(group_levels, each = n_measures)
  Measure <- rep(measures_labels, times = n_groups)
  if (contrast && is.null(grouping)) {
    Group <- c(Group, Group[[1]])
    Measure <- c(Measure, paste0(Measure[[2]], " - ", Measure[[1]]))
  } else if (contrast && !is.null(grouping)) {
    Group <- c(Group, paste0(Group[[2]], " - ", Group[[1]]))
    Measure <- c(Measure, Measure[[1]])
  }
  results <- cbind(Group, Measure, bs_output)
  if (is.null(grouping)) {
    results$label <- Measure
  } else {
    results$label <- paste0(Measure, ": ", Group)
  }

  # Collect analysis details
  details <- list(
    boots = boots,
    interval = interval,
    listwise = listwise,
    angles = as_degree(angles),
    contrast = contrast,
    score_type = "Correlation"
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
#' and generate a data frame with customizable labels for each parameter value.
#' This function requires the input to be a numeric vector (or coercable to one)
#' and returns only the parameters. See \code{\link{ssm_score}()} for a similar
#' function that calculates SSM parameters for each row of a data frame.
#'
#' @param scores Required. A numeric vector (or single row data frame)
#'   containing one score for each of a set of circumplex scales.
#' @param angles Required. A numeric vector containing the angular displacement
#'   of each circumplex scale included in `scores` (in degrees).
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
#' @return A data frame containing the SSM parameters calculated from `scores`.
#' @family ssm functions
#' @family analysis functions
#' @export
#' @examples
#' # Manually enter octant scores
#' scores <- c(0.55, 0.58, 0.62, 0.76, 1.21, 1.21, 1.48, 0.90)
#' ssm_parameters(scores)
#'
#' # Customize several of the labels
#' ssm_parameters(scores, x_label = "LOV", y_label = "DOM")
#'
#' # Add a prefix to all labels
#' ssm_parameters(scores, prefix = "IIP_")
#' 
ssm_parameters <- function(scores, angles = octants(), prefix = "", suffix = "", 
                           e_label = "Elev", x_label = "Xval", y_label = "Yval",
                           a_label = "Ampl", d_label = "Disp", f_label = "Fit") {

  stopifnot(is.numeric(scores))
  stopifnot(is.numeric(angles))
  stopifnot(length(scores) == length(angles))
  stopifnot(is_char(prefix, n = 1))
  stopifnot(is_char(suffix, n = 1))
  stopifnot(is_char(e_label, n = 1))
  stopifnot(is_char(x_label, n = 1))
  stopifnot(is_char(y_label, n = 1))
  stopifnot(is_char(a_label, n = 1))
  stopifnot(is_char(d_label, n = 1))
  stopifnot(is_char(f_label, n = 1))

  angles <- as_radian(as_degree(angles))
  params <- ssm_parameters_cpp(scores, angles)
  params[[5]] <- as_degree(as_radian(params[[5]]))
  
  rownames(params) <- paste0(
    prefix, 
    c(e_label, x_label, y_label, a_label, d_label, f_label), 
    suffix
  )
  
  as.data.frame(t(params))
}

#' Calculate SSM parameters by row and add results as new columns
#'
#' Calculate the SSM parameters for each row of a data frame and add the results
#' as additional columns. This can be useful when the SSM is being used for the
#' description or visualization of individual data points rather than for
#' statistical inference on groups of data points.
#'
#' @param data Required. A data frame containing at least circumplex scales.
#' @param scales Required. The variable names or column numbers for the
#'   variables in \code{.data} that contain circumplex scales to be analyzed.
#' @param angles Required. A numeric vector containing the angular displacement
#'   of each circumplex scale included in \code{scales} (in degrees).
#' @param append Optional. A logical indicating whether to append the output to
#'   `data` or simply return the output (default = "TRUE").
#' @param ... Optional. Additional parameters to pass to
#'   \code{\link{ssm_parameters}()}, such as \code{prefix} and \code{suffix}.
#' @return A data frame containing \code{.data} plus six additional columns
#'   containing the SSM parameters (calculated rowwise).
#' @family ssm functions
#' @family analysis functions
#' @export
#' @examples
#' data("aw2009")
#' ssm_score(
#'   aw2009,
#'   scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
#' )
#' 
ssm_score <- function(data, scales, angles = octants(), append = TRUE, ...) {

  stopifnot(is.data.frame(data) || is.matrix(data))
  stopifnot(is.character(scales))
  stopifnot(is.numeric(angles))
  stopifnot(length(scales) == length(angles))

  scales_mat <- as.matrix(data[scales])
  
  out <- do.call(
    rbind, 
    apply(
      scales_mat,
      MARGIN = 1, 
      FUN = ssm_parameters, 
      ...
    )
  )
  
  if (append) {
    out <- cbind(data, out)
  }
  
  out
}
