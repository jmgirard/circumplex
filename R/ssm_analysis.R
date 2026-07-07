#' Perform analyses using the Structural Summary Method
#'
#' Calculate SSM parameters with confidence intervals (bootstrapped by
#' default, or Monte Carlo via `method`) for a variety of different analysis
#' types. Depending on what arguments are supplied, either mean-based or
#' correlation-based analyses will be performed, one or more groups will be
#' used to stratify the data, and contrasts between groups or measures will be
#' calculated.
#'
#' @param data Required. A data frame or matrix containing at least
#'   circumplex scales.
#' @param scales Required. A character vector of column names, or a numeric
#'   vector of column indexes, from `data` that contains the circumplex scale
#'   scores to be analyzed.
#' @param angles Optional. A numeric vector containing the angular displacement
#'   of each circumplex scale included in `scales` (in degrees). (default =
#'   `octants()`). The closed-form SSM estimator used here equals the
#'   ordinary-least-squares cosine fit only when `angles` are equally spaced
#'   around the circle (e.g., octants at 45-degree intervals); for unequally
#'   spaced angles it is the conventional Gurtman estimator, not a
#'   least-squares fit, and the reported model fit is then no longer a bounded
#'   R-squared in `[0, 1]` (it can fall below 0).
#' @param measures Optional. Either `NULL` or a character vector of column names
#'   from `data` that contains one or more variables to be correlated with the
#'   circumplex scales and analyzed using correlation-based SSM analyses.
#' @param grouping Optional. Either `NULL` or a string that contains the column
#'   name from `data` of the variable that indicates the group membership of
#'   each observation.
#' @param contrast Optional. A logical indicating whether to output the
#'   difference between two measures' or two groups' SSM parameters. Can only be
#'   set to TRUE when there are exactly two measures and one group, one measure
#'   and two groups, or no measures and two groups (default = FALSE). The
#'   contrast is always the second level minus the first. For two groups, this
#'   is the second level of `grouping` alphabetically, unless `grouping` is
#'   already a factor with an explicit level order, in which case that order is
#'   used. For two measures, this is simply the second entry of `measures` as
#'   given (no reordering). The direction is shown in the result's Label (e.g.,
#'   "Male - Female").
#' @param boots Optional. A single positive whole number indicating how many
#'   bootstrap resamples (or, when `method = "montecarlo"`, Monte Carlo draws)
#'   to use when estimating the confidence intervals (default = 2000).
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
#' @param parallel Optional. A string indicating whether to distribute the
#'   bootstrap computation across multiple CPU cores: "no" (default),
#'   "multicore" (process forking; available on macOS and Linux, ignored on
#'   Windows), or "snow" (a local PSOCK cluster; available on all platforms).
#'   Passed to \code{\link[boot]{boot}}. Because the bootstrap resample
#'   indices are drawn in the main R process before any work is distributed,
#'   results for a given `set.seed()` are identical regardless of the
#'   `parallel` and `ncpus` settings.
#' @param ncpus Optional. A single positive whole number indicating how many
#'   CPU cores to use when `parallel` is not "no" (default = 1).
#' @param method Optional. A string indicating how to estimate the confidence
#'   intervals: "bootstrap" (default) resamples the data, whereas "montecarlo"
#'   draws parameter replicates from the asymptotic sampling distribution of
#'   the group mean vector (mean-based analyses) or the measure-scale
#'   correlation vector (correlation-based analyses) -- a multivariate normal
#'   with empirically estimated covariance -- and propagates them through the
#'   SSM parameter transformation. The Monte Carlo method is much faster for
#'   large samples but relies on the asymptotic normality of the means or
#'   correlations, so prefer the bootstrap for small samples; it also requires
#'   listwise-complete data. Correlations are drawn jointly across measures
#'   within each group on the Fisher z scale and back-transformed. The
#'   `parallel` and `ncpus` arguments apply only to the bootstrap.
#' @return A list containing the results and description of the analysis.
#'   \item{results}{A data frame with the SSM parameter estimates}
#'   \item{details}{A list with the number of bootstrap resamples or Monte
#'   Carlo draws (boots), the confidence interval percentage level (interval),
#'   the angular displacement of scales (angles), and the interval estimation
#'   method (method)}
#'   \item{call}{A language object containing the function call that created
#'   this object}
#'   \item{scores}{A data frame containing the mean scale scores} \item{type}{A
#'   string indicating what type of SSM analysis was done}
#'
#'   The profile displacement parameter is reported in the half-open interval
#'   `[0, 360)` degrees. A profile that peaks exactly at the 0/360 degree
#'   boundary is reported as approximately 360 (equivalently 0, the same
#'   direction); which of the two appears is a floating-point detail and both
#'   denote the same pole. Contrast displacements are instead reported as a
#'   signed difference in `(-180, 180]` degrees (see the "Contrast" block in
#'   the printed output).
#'
#'   Degenerate profiles (flat or zero-amplitude) have undefined displacement
#'   (and fit, if flat), which is reported as `NA` with a warning. Bootstrap
#'   resamples that produce degenerate profiles (e.g., a resampled measure
#'   with zero variance) are excluded from the confidence intervals with a
#'   warning reporting how many were dropped; the intervals are then
#'   conditional on estimability.
#' @section Reproducibility:
#'   This function consumes R's random number stream (so do
#'   `cpm_fit(ci_method = "bootstrap")`, `cpm_simulate()`, and
#'   `ssm_ci_accuracy()`; `ssm_score()`/`ssm_parameters()` and the tidying
#'   functions are deterministic). Call `set.seed()` immediately before
#'   `ssm_analyze()` for reproducible confidence intervals:
#'   \itemize{
#'     \item \strong{Bootstrap} (`method = "bootstrap"`, the default): the
#'       same seed gives byte-identical `results`, *regardless of* the
#'       `parallel`/`ncpus` settings (see their descriptions below), because
#'       `boot::boot()` draws all resample indices from the seed before any
#'       work is parallelized.
#'     \item \strong{Monte Carlo} (`method = "montecarlo"`): the same seed
#'       gives byte-identical `results`. Adding a group or measure, or
#'       reordering `scales`/`measures`, changes the random draw sequence, so
#'       results are reproducible for a fixed call but will not match after
#'       such structural edits even with the same seed.
#'     \item The two methods are \strong{not} expected to agree numerically
#'       for the same seed -- they consume the random stream in unrelated
#'       ways. Their statistical agreement (validated on real data; see
#'       `vignette("introduction-to-ssm-analysis")`) is a separate property
#'       from RNG reproducibility.
#'     \item Increasing `boots` changes the CI by design (more resamples/draws
#'       should tighten Monte Carlo error), so results are not expected to be
#'       stable across different `boots` values, only within a fixed call.
#'   }
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
#'
#' # Monte Carlo confidence intervals (faster for large samples)
#' ssm_analyze(
#'   jz2017,
#'   scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO"),
#'   method = "montecarlo"
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
                        measures_labels = NULL, parallel = "no", ncpus = 1,
                        method = "bootstrap") {

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
  parallel <- match.arg(parallel, c("no", "multicore", "snow"))
  stopifnot(is.numeric(ncpus) && ncpus >= 1 && ceiling(ncpus) == floor(ncpus))
  method <- match.arg(method, c("bootstrap", "montecarlo"))

  # Coerce matrix input to a data frame so column indexing behaves uniformly
  if (is.matrix(data)) data <- as.data.frame(data)

  # Drop observations with missing grouping values (unusable in any group).
  # Done here, on the user's actual grouping column, so both analysis paths
  # inherit clean data and never pass NA group codes into the C++ estimators.
  if (!is.null(grouping)) {
    na_group <- is.na(data[[grouping]])
    if (any(na_group)) {
      message(
        sum(na_group),
        " observation(s) removed due to missing values in the grouping variable."
      )
      data <- data[!na_group, , drop = FALSE]
      if (nrow(data) == 0) {
        stop("No observations remain after removing missing grouping values.")
      }
    }
  }

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
      parallel = parallel,
      ncpus = ncpus,
      method = method,
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
      parallel = parallel,
      ncpus = ncpus,
      method = method,
      call = call
    )
  }
}

# Perform analyses using the mean-based Structural Summary Method --------------

ssm_analyze_means <- function(data, scales, angles, grouping, contrast,
                              boots, interval, listwise, parallel, ncpus,
                              method, call) {
  
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
  obs_scores <- mean_scores(mat, grp, listwise)
  scores <- obs_scores
  colnames(scores) <- scales_names
  group_levels <- levels(bs_input[[ncol(bs_input)]])
  if (contrast) {
    scores <- rbind(scores, scores[2, ] - scores[1, ])
  }
  scores <- as.data.frame(scores)
  Group <- group_levels
  Measure <- rep(NA_character_, times = n_groups)
  if (contrast && !is.null(grouping)) {
    Group <- c(Group, paste0(Group[[2]], " - ", Group[[1]]))
    Measure <- c(Measure, Measure[[1]])
  }
  Label <- Group
  scores <- cbind(Label, Group, Measure, scores)

  # Create function that will perform bootstrapping
  bs_function <- function(.data, index, scales, angles, contrast, listwise, ...) {
    resample <- .data[index, ]
    mat <- as.matrix(resample[scales])
    grp <- as.integer(resample[[ncol(resample)]])
    scores_r <- mean_scores(mat, grp, listwise)
    ssm_by_group(scores_r, angles, contrast)
  }
  
  # Estimate confidence intervals with the requested engine
  bs_output <- ssm_estimate_intervals(
    method = method,
    bs_input = bs_input,
    bs_function = bs_function,
    scales = scales_names,
    measures = NULL,
    angles = angles,
    boots = boots,
    interval = interval,
    contrast = contrast,
    listwise = listwise,
    parallel = parallel,
    ncpus = ncpus,
    strata = bs_input[[ncol(bs_input)]],
    obs_scores = obs_scores
  )

  params <- bs_output
  Group <- group_levels
  Measure <- rep(NA_character_, times = n_groups)
  if (contrast && !is.null(grouping)) {
    Group <- c(Group, paste0(Group[[2]], " - ", Group[[1]]))
    Measure <- c(Measure, Measure[[1]])
  }
  Label <- Group
  results <- cbind(Label, Group, Measure, params)
  
  # Collect analysis details (suff_stats is a pure list addition for the
  # CI-accuracy diagnostic; see ssm_compute_suff_stats() and spec sec. 8.3)
  details <- list(
    boots = boots,
    interval = interval,
    listwise = listwise,
    angles = as_degree(angles),
    contrast = contrast,
    score_type = "Mean",
    method = method,
    suff_stats = ssm_compute_suff_stats(
      data = data, scales = scales, measures = NULL,
      grouping = grouping, listwise = listwise
    )$stats
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
                              measures_labels, parallel, ncpus, method,
                              call) {
  
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
  obs_scores <- corr_scores(cs, mv, grp, listwise)
  scores <- obs_scores
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
  if (is.null(grouping)) {
    Label <- Measure
  } else {
    Label <- paste0(Measure, ": ", Group)
  }
  scores <- cbind(Label, Group, Measure, scores)


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
  
  # Estimate confidence intervals with the requested engine
  bs_output <- ssm_estimate_intervals(
    method = method,
    bs_input = bs_input,
    bs_function = bs_function,
    scales = scales_names,
    measures = measures_names,
    angles = angles,
    boots = boots,
    interval = interval,
    contrast = contrast,
    listwise = listwise,
    parallel = parallel,
    ncpus = ncpus,
    strata = bs_input$Group,
    obs_scores = obs_scores
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
  if (is.null(grouping)) {
    Label <- Measure
  } else {
    Label <- paste0(Measure, ": ", Group)
  }
  results <- cbind(Label, Group, Measure, bs_output)


  # Collect analysis details (suff_stats is a pure list addition for the
  # CI-accuracy diagnostic; see ssm_compute_suff_stats() and spec sec. 8.3)
  details <- list(
    boots = boots,
    interval = interval,
    listwise = listwise,
    angles = as_degree(angles),
    contrast = contrast,
    score_type = "Correlation",
    method = method,
    suff_stats = ssm_compute_suff_stats(
      data = data, scales = scales, measures = measures,
      grouping = grouping, listwise = listwise
    )$stats
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

# Sufficient statistics for the CI-accuracy diagnostic -------------------------

# Compute, from the raw analysis inputs, the per-group sufficient statistics the
# CI-accuracy diagnostic needs (spec devel/m4-ci-accuracy-spec.md sec. 8.3):
# per-group sample size, per-scale SDs (mean-based path only), and the
# within-group correlation matrix (scale-only for the mean-based path; joint
# scales + measures for the correlation-based path). Also returns the profile
# vectors, computed with the same C++ estimators ssm_analyze() uses, so a
# `data =` fallback can verify it was handed the object's own dataset.
#
# Groups are ordered by sorted factor level, matching ssm_analyze()'s row order.
# The n/SD/correlation statistics use complete cases within each group: the
# diagnostic assesses the complete-data procedure (spec sec. 9), so they are
# exact under the default listwise deletion and assessed-as-listwise otherwise.
# The profile vectors honor the object's `listwise` setting via the C++
# estimators, so the fallback's consistency check is exact under either method.
ssm_compute_suff_stats <- function(data, scales, measures = NULL,
                                   grouping = NULL, listwise = TRUE,
                                   compute_profiles = FALSE) {
  if (is.matrix(data)) data <- as.data.frame(data)

  # Mirror ssm_analyze(): drop rows with a missing grouping value
  if (!is.null(grouping)) {
    data <- data[!is.na(data[[grouping]]), , drop = FALSE]
  }

  scales_data <- data[scales]
  scales_names <- colnames(scales_data)
  corr_based <- !is.null(measures)
  if (corr_based) {
    measures_data <- data[measures]
  }

  # Group vector (sorted factor levels = ssm_analyze() row/report order)
  if (is.null(grouping)) {
    group <- factor(rep("All", times = nrow(data)))
  } else {
    group <- factor(data[[grouping]])
  }
  group_levels <- levels(group)

  # Profile vectors, only when a data = fallback needs them for its consistency
  # check (the live ssm_analyze() path already holds the profiles as obs_scores
  # and keeps only $stats). Computed with the same C++ estimators ssm_analyze()
  # uses, under the same deletion method: when listwise, the full analysis
  # matrix is na.omit-ed before the estimator (matching ssm_analyze_means()/
  # ssm_analyze_corrs(), which omit over scales(+measures)+group up front), so
  # the recomputed profiles are bit-exact with the object's stored scores.
  profiles <- NULL
  if (compute_profiles) {
    cs <- as.matrix(scales_data)
    grp_fac <- group
    if (corr_based) mv <- as.matrix(measures_data)
    if (listwise) {
      keep <- if (corr_based) {
        stats::complete.cases(cs, mv)
      } else {
        stats::complete.cases(cs)
      }
      cs <- cs[keep, , drop = FALSE]
      if (corr_based) mv <- mv[keep, , drop = FALSE]
      grp_fac <- droplevels(grp_fac[keep])
    }
    grp <- as.integer(grp_fac)
    if (corr_based) {
      profiles <- corr_scores(cs, mv, grp, listwise)
    } else {
      profiles <- mean_scores(cs, grp, listwise)
    }
    colnames(profiles) <- scales_names
  }

  # Per-group sufficient statistics on complete cases within the group
  n <- stats::setNames(integer(length(group_levels)), group_levels)
  cormats <- stats::setNames(vector("list", length(group_levels)), group_levels)
  sds <- if (corr_based) {
    NULL
  } else {
    stats::setNames(vector("list", length(group_levels)), group_levels)
  }

  for (g in seq_along(group_levels)) {
    idx <- which(group == group_levels[[g]])
    if (corr_based) {
      block <- cbind(
        scales_data[idx, , drop = FALSE],
        measures_data[idx, , drop = FALSE]
      )
    } else {
      block <- scales_data[idx, , drop = FALSE]
    }
    block <- as.matrix(block)
    block <- block[stats::complete.cases(block), , drop = FALSE]
    n[[g]] <- nrow(block)
    cormats[[g]] <- stats::cor(block)
    if (!corr_based) {
      sds[[g]] <- apply(block, 2, stats::sd)
    }
  }

  list(
    stats = list(n = n, sds = sds, cormats = cormats),
    profiles = profiles
  )
}

# Retrieve the CI-accuracy sufficient statistics (spec sec. 8.3) from an ssm
# object, falling back to recomputation from re-supplied data for objects
# created before ssm_analyze() stored them. The fallback recovers the analysis
# arguments from the recorded call, recomputes the statistics from `data`, and
# verifies the recomputed profile vectors match the stored `scores` within 1e-8
# -- the guard against handing the diagnostic the wrong dataset.
ssm_suff_stats <- function(object, data = NULL) {
  stopifnot(inherits(object, "circumplex_ssm"))

  if (!is.null(object$details$suff_stats)) {
    return(object$details$suff_stats)
  }

  if (is.null(data)) {
    stop(
      "This ssm object predates sufficient-statistics storage; supply the ",
      "original data via `data = ` so they can be recomputed.",
      call. = FALSE
    )
  }
  stopifnot(is.data.frame(data) || is.matrix(data))

  # Recover the analysis arguments from the recorded call
  cl <- object$call
  scales <- eval(cl$scales, envir = parent.frame())
  measures <- if (is.null(cl$measures)) NULL else eval(cl$measures, parent.frame())
  grouping <- if (is.null(cl$grouping)) NULL else eval(cl$grouping, parent.frame())
  listwise <- if (is.null(cl$listwise)) TRUE else isTRUE(eval(cl$listwise, parent.frame()))

  recomputed <- ssm_compute_suff_stats(
    data, scales, measures, grouping, listwise, compute_profiles = TRUE
  )

  # Consistency check: recomputed profiles must match the stored profile vectors
  # (the non-contrast rows of `scores`) within 1e-8, NA pattern included.
  np <- nrow(recomputed$profiles)
  stored <- as.matrix(
    object$scores[seq_len(np), colnames(recomputed$profiles), drop = FALSE]
  )
  same_na <- all(is.na(stored) == is.na(recomputed$profiles))
  maxdiff <- suppressWarnings(max(abs(stored - recomputed$profiles), na.rm = TRUE))
  if (!is.finite(maxdiff)) maxdiff <- 0
  if (!same_na || maxdiff > 1e-8) {
    stop(
      "The supplied `data` is inconsistent with this ssm object (recomputed ",
      "profile vectors differ from the stored scores); supply the original ",
      "dataset.",
      call. = FALSE
    )
  }

  recomputed$stats
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
#'   of each circumplex scale included in `scores` (in degrees). The closed-form
#'   SSM estimator used here equals the ordinary-least-squares cosine fit only
#'   when `angles` are equally spaced around the circle (e.g., octants at
#'   45-degree intervals); for unequally spaced angles it is the conventional
#'   Gurtman estimator, not a least-squares fit, and the reported fit is then no
#'   longer a bounded R-squared in `[0, 1]` (it can fall below 0).
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
#'   fit or R-squared value (default = "Fit"). This value is a bounded R-squared
#'   in `[0, 1]` only for equally spaced `angles` (see `angles`).
#' @return A data frame containing the SSM parameters calculated from `scores`.
#'   For degenerate profiles the undefined parameters are returned as `NA`
#'   with a warning: a flat profile (zero variance) has undefined displacement
#'   and fit, and a profile with real variance but zero amplitude (i.e., no
#'   first-harmonic component) has undefined displacement and a fit of 0.
#'   Note that this applies only to amplitudes that are zero up to machine
#'   precision; small real amplitudes are always estimated, and their
#'   uncertainty is expressed through confidence intervals (see
#'   \code{\link{ssm_analyze}()}).
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
  if (is.na(params[[5]])) {
    warning(
      "Displacement is undefined for this profile (flat scores, zero ",
      "amplitude, or missing values); NA returned.",
      call. = FALSE
    )
  }
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
#' @param data Required. A data frame or matrix containing at least
#'   circumplex scales.
#' @param scales Required. The variable names or column numbers for the
#'   variables in \code{.data} that contain circumplex scales to be analyzed.
#' @param angles Required. A numeric vector containing the angular displacement
#'   of each circumplex scale included in \code{scales} (in degrees). The
#'   closed-form SSM estimator used here equals the ordinary-least-squares
#'   cosine fit only when \code{angles} are equally spaced around the circle
#'   (e.g., octants at 45-degree intervals); for unequally spaced angles it is
#'   the conventional Gurtman estimator, not a least-squares fit, and the
#'   reported fit is then no longer a bounded R-squared in \code{[0, 1]} (it can
#'   fall below 0).
#' @param append Optional. A logical indicating whether to append the output to
#'   `data` or simply return the output (default = "TRUE").
#' @param ... Optional. Additional \strong{named} arguments passed to
#'   \code{\link{ssm_parameters}()}, such as \code{prefix} and \code{suffix};
#'   each must be a single string. Unnamed or non-scalar arguments raise an
#'   error.
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
  stopifnot(is_var(scales))
  stopifnot(is.numeric(angles))
  stopifnot(length(scales) == length(angles))

  if (is.matrix(data)) data <- as.data.frame(data)
  scales_mat <- as.matrix(data[scales])
  angles_rad <- as_radian(as_degree(angles))

  # Label/prefix/suffix arguments are forwarded as they were when this called
  # ssm_parameters() per row; ssm_parameters()'s formals supply the defaults
  # so the two stay in sync.
  label_names <- c(
    "prefix", "suffix", "e_label", "x_label", "y_label",
    "a_label", "d_label", "f_label"
  )
  label_defaults <- lapply(formals(ssm_parameters)[label_names], eval)
  extra_args <- list(...)
  # Unnamed extras: v1.2.0's apply(FUN = ssm_parameters, ...) matched a bare
  # positional argument to ssm_parameters()'s `prefix` formal. modifyList()
  # below silently drops unnamed elements, so require names explicitly rather
  # than resurrect that error-prone positional matching.
  arg_names <- names(extra_args)
  if (length(extra_args) > 0 && (is.null(arg_names) || any(!nzchar(arg_names)))) {
    stop("arguments passed via `...` must be named ",
         "(e.g. prefix = \"IIP_\")", call. = FALSE)
  }
  unknown <- setdiff(arg_names, label_names)
  if (length(unknown) > 0) {
    stop("unused argument", if (length(unknown) > 1) "s" else "", " (",
         paste(unknown, collapse = ", "), ")", call. = FALSE)
  }
  # Each label/prefix/suffix must be a single string, as ssm_parameters()
  # enforced per row via is_char(x, n = 1); without this a vector recycles
  # through paste0() into interleaved column names.
  for (nm in arg_names) stopifnot(is_char(extra_args[[nm]], n = 1))
  label_args <- utils::modifyList(label_defaults, extra_args)

  # Elevation/x/y/amplitude/displacement/fit for every row in a single
  # compiled pass (group_parameters(), already used by ssm_bootstrap() for the
  # same estimator), replacing the previous row-wise apply() + rbind() of
  # per-row data frames.
  raw <- group_parameters(scales_mat, angles_rad)
  pnames <- ssm_param_names()
  out <- matrix(raw, ncol = length(pnames), byrow = TRUE)

  d_col <- which(pnames == "d")
  n_bad <- sum(is.na(out[, d_col]))
  if (n_bad > 0) {
    warning(
      n_bad, " of ", nrow(out), " profile(s) have undefined displacement ",
      "(flat scores, zero amplitude, or missing values); NA returned.",
      call. = FALSE
    )
  }
  out[, d_col] <- as.numeric(as_degree(as_radian(out[, d_col])))

  colnames(out) <- paste0(
    label_args$prefix,
    c(
      label_args$e_label, label_args$x_label, label_args$y_label,
      label_args$a_label, label_args$d_label, label_args$f_label
    ),
    label_args$suffix
  )
  out <- as.data.frame(out)

  if (append) {
    out <- cbind(data, out)
  }

  out
}
