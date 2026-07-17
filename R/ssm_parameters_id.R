# Per-person (intraindividual) SSM scoring -------------------------------------

#' Calculate SSM parameters for each person
#'
#' Score each person's own circumplex profile through the closed-form SSM
#' transform and return a per-person parameter table. When `id` is `NULL`,
#' every row of `data` is treated as one person's profile (like
#' [ssm_score()], but returning a fresh table rather than appending columns).
#' When `id` names a column, rows sharing an id (e.g., occasions of intensive
#' longitudinal data) are first averaged within person -- each scale's mean
#' uses that person's available (non-missing) rows -- and the within-person
#' mean profile is scored.
#'
#' Degenerate profiles keep their row and are reported as `NA`, never
#' silently dropped: a flat (zero-variance) profile has undefined
#' displacement and fit, a profile with real variance but zero
#' first-harmonic amplitude has undefined displacement and a fit of 0, and a
#' person with a completely missing scale has an undefined profile (all
#' parameters `NA`). The `na_rate` column exposes each person's share of
#' missing scale cells so missingness is visible alongside its consequences.
#'
#' @param data Required. A data frame or matrix containing at least
#'   circumplex scales, with one row per person or (with `id`) per
#'   person-occasion.
#' @param scales Required. The variable names or column numbers for the
#'   variables in `data` that contain circumplex scales to be analyzed.
#' @param angles Optional. A numeric vector containing the angular
#'   displacement of each circumplex scale included in `scales`, in degrees
#'   (default = `octants()`). The closed-form SSM estimator used here equals
#'   the ordinary-least-squares cosine fit for equally spaced `angles` --
#'   more generally, for any angle set satisfying first- and second-harmonic
#'   balance; see [ssm_parameters()].
#' @param id Optional. A single variable name or column number identifying
#'   persons. If `NULL` (default), each row is scored as its own person;
#'   otherwise rows sharing an id are averaged within person before scoring.
#'   Missing id values are an error (a person cannot be silently dropped).
#' @return A data frame of class `"circumplex_ssm_id"` with one row per
#'   person, in order of first appearance: the id column (named after `id`,
#'   or `id` when `NULL`), `n_obs` (rows contributing to that person),
#'   `na_rate` (proportion of missing scale cells among those rows), and the
#'   SSM parameters `Elev`, `Xval`, `Yval`, `Ampl`, `Disp` (degrees in
#'   \[0, 360)), and `Fit`. Use [summary.circumplex_ssm_id()] for
#'   group-level summaries with circular statistics for displacement.
#' @family ssm functions
#' @family analysis functions
#' @export
#' @examples
#' data("aw2009")
#' ssm_parameters_id(
#'   aw2009,
#'   scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
#' )
#'
ssm_parameters_id <- function(data, scales, angles = octants(), id = NULL) {

  stopifnot(is.data.frame(data) || is.matrix(data))
  stopifnot(is_var(scales))
  stopifnot(is.numeric(angles))
  stopifnot(length(scales) == length(angles))
  stopifnot(is_null_or_var(id, n = 1))

  if (is.matrix(data)) data <- as.data.frame(data)
  scales_mat <- as.matrix(data[scales])
  # as.matrix() on a zero-row data frame yields logical mode; coerce so the
  # 0-row edge returns an empty table instead of tripping the numeric check
  if (nrow(scales_mat) == 0) storage.mode(scales_mat) <- "double"
  stopifnot(is.numeric(scales_mat))
  angles_rad <- as_radian(as_degree(angles))

  # Person index: id column when supplied, else one person per row. Persons
  # are kept in order of first appearance (never resorted), and a missing id
  # is an error rather than a silent drop (split() would discard NA levels).
  if (is.null(id)) {
    ids <- seq_len(nrow(data))
    id_name <- "id"
  } else {
    ids <- data[[id]]
    id_name <- colnames(data[id])
    if (anyNA(ids)) {
      stop("missing values in `id`; persons cannot be silently dropped",
           call. = FALSE)
    }
  }
  fid <- factor(ids, levels = unique(ids))
  idx <- split(seq_len(nrow(scales_mat)), fid)
  ids_out <- ids[!duplicated(ids)]

  # Within-person mean profile per scale over that person's available rows
  # (a scale with no observed rows has no mean: NA, and the kernel's
  # flat-or-NaN branch propagates NA parameters for that person).
  p <- ncol(scales_mat)
  profiles <- t(vapply(
    idx,
    function(rows) colMeans(scales_mat[rows, , drop = FALSE], na.rm = TRUE),
    numeric(p)
  ))
  profiles[is.nan(profiles)] <- NA_real_
  n_obs <- lengths(idx, use.names = FALSE)
  na_rate <- vapply(
    idx,
    function(rows) mean(is.na(scales_mat[rows, , drop = FALSE])),
    numeric(1),
    USE.NAMES = FALSE
  )

  # Elevation/x/y/amplitude/displacement/fit for every person in a single
  # compiled pass (group_parameters(), the same kernel ssm_score() and the
  # bootstrap use, so per-person and group paths cannot drift).
  raw <- group_parameters(profiles, angles_rad)
  pnames <- ssm_param_names()
  out <- matrix(raw, ncol = length(pnames), byrow = TRUE)
  out[is.nan(out)] <- NA_real_

  d_col <- which(pnames == "d")
  n_bad <- sum(is.na(out[, d_col]))
  if (n_bad > 0) {
    warning(
      n_bad, " of ", nrow(out), " person(s) have undefined displacement ",
      "(flat scores, zero amplitude, or missing values); NA returned.",
      call. = FALSE
    )
  }
  out[, d_col] <- as.numeric(as_degree(as_radian(out[, d_col])))

  out_df <- data.frame(
    ids_out, as.integer(n_obs), na_rate, out,
    stringsAsFactors = FALSE, row.names = NULL
  )
  colnames(out_df) <- c(
    id_name, "n_obs", "na_rate",
    "Elev", "Xval", "Yval", "Ampl", "Disp", "Fit"
  )

  structure(out_df, class = c("circumplex_ssm_id", "data.frame"))
}

#' Summarize per-person SSM parameters at the group level
#'
#' Aggregate a per-person SSM parameter table (from [ssm_parameters_id()])
#' into group-level summaries, using circular statistics for displacement:
#' arithmetic means are meaningless for angles, so displacement is summarized
#' by its circular mean (the direction of the summed unit vectors) and the
#' mean resultant length (a 0 to 1 measure of directional concentration).
#'
#' Persons with undefined (`NA`) displacement are stripped before the
#' circular aggregation -- `n_na_d` reports how many -- while the arithmetic
#' means of the other parameters use all persons with defined values. Two
#' aggregation caveats apply. (1) The circular mean of per-person
#' displacements weights every person's direction equally; it is a
#' *different quantity* from the displacement of the group mean profile
#' (e.g., from [ssm_analyze()]), which weights persons by amplitude -- on
#' heterogeneous samples the two can differ substantially. (2) By the
#' triangle inequality, the amplitude of the group mean profile is at most
#' the mean per-person amplitude (`a_mean`), strictly smaller when
#' directions disperse; relatedly, the mean resultant length `d_res` falls
#' below 1 as directions disperse.
#'
#' @param object Required. An object of class `"circumplex_ssm_id"` created
#'   by [ssm_parameters_id()].
#' @param ... Ignored (S3 consistency).
#' @return A one-row data frame with columns `n` (persons), `n_na_d`
#'   (persons with undefined displacement, excluded from the circular
#'   summaries), `e_mean`, `x_mean`, `y_mean`, `a_mean` (arithmetic means),
#'   `d_mean` (circular mean of displacement, degrees in \[0, 360)), and
#'   `d_res` (mean resultant length in \[0, 1\]; `NA` when no displacement
#'   is defined, and undefined direction at zero resultant reports
#'   `d_mean = NA`).
#' @family ssm functions
#' @family analysis functions
#' @method summary circumplex_ssm_id
#' @export
#' @examples
#' data("aw2009")
#' res <- ssm_parameters_id(
#'   aw2009,
#'   scales = c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
#' )
#' summary(res)
#'
summary.circumplex_ssm_id <- function(object, ...) {

  n <- nrow(object)
  safe_mean <- function(x) {
    out <- mean(x, na.rm = TRUE)
    if (is.nan(out)) NA_real_ else out
  }

  # Circular aggregation of displacement: strip undefined d_i first (count
  # reported as n_na_d; angle_mean() in src/circular.cpp has no na.rm), then
  # take the circular mean and the mean resultant length. The wrap adds
  # 2*pi to a negative circular mean (exactly the estimate path's modu()
  # over atan2's range, where R's %% would second-reduce a tiny-negative
  # mean to 0), and the quantile method's pole window maps both float
  # representations of the 0/360 pole to 360 (D-003/M20 convention).
  d_rad <- object$Disp * (pi / 180)
  n_na_d <- sum(is.na(d_rad))
  d_ok <- d_rad[!is.na(d_rad)]
  if (length(d_ok) == 0) {
    d_mean <- NA_real_
    d_res <- NA_real_
  } else {
    d_mean <- as.numeric(angle_mean(d_ok))
    if (!is.na(d_mean)) {
      if (d_mean < 0) d_mean <- d_mean + 2 * pi
      pole <- d_mean < (16 * .Machine$double.eps) |
        (2 * pi - d_mean) < (16 * .Machine$double.eps)
      if (pole) d_mean <- 2 * pi
    }
    d_mean <- d_mean * (180 / pi)
    d_res <- sqrt(mean(cos(d_ok))^2 + mean(sin(d_ok))^2)
  }

  data.frame(
    n = n,
    n_na_d = as.integer(n_na_d),
    e_mean = safe_mean(object$Elev),
    x_mean = safe_mean(object$Xval),
    y_mean = safe_mean(object$Yval),
    a_mean = safe_mean(object$Ampl),
    d_mean = d_mean,
    d_res = d_res
  )
}
