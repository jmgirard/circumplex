# Growth-recipe input: the stacked long table (M151) ---------------------------
# The person-by-time-by-coordinate table that the joint growth model fits.
# Scoring goes through ssm_parameters_id() with one profile per row, so the
# coordinates cannot drift from the package's SSM transform; the stacking is
# base rep() indexing in a fixed row-then-dv order, which the tests pin.

#' Build the long table for a growth model on SSM coordinates
#'
#' Score every row of `data` as its own circumplex profile and stack the
#' three SSM coordinates, elevation `e`, `x` and `y`, into the long table that
#' the package's growth recipe fits. One input row gives three output rows,
#' one per coordinate, and the output holds each row's id and time value
#' beside the coordinate's name and value. [ssm_growth_formula()] gives the
#' fit call that takes this table.
#'
#' Growth in displacement is modeled through `x` and `y`, never through the
#' angle itself, so the table carries no amplitude or displacement. A flat
#' profile has `x` and `y` at zero to floating-point precision and a
#' defined `e`, and a profile with a scale entirely missing has `NA` on all
#' three coordinates. Both keep their rows. Each engine's fit call then drops
#' a row whose `value` is `NA`: glmmTMB and brms do so by default, and the
#' nlme call that [ssm_growth_formula()] prints sets `na.action = na.omit`
#' for the same effect.
#'
#' @param data Required. A data frame or matrix with one row per person per
#'   time point, containing the circumplex scales, an id column and a time
#'   column.
#' @param scales Required. The variable names or column numbers for the
#'   variables in `data` that contain circumplex scales.
#' @param angles Optional. A numeric vector containing the angular
#'   displacement of each circumplex scale included in `scales`, in degrees
#'   (default = `octants()`).
#' @param id Required. The name of the column in `data` identifying persons.
#'   A column name, not a number. Missing values are an error.
#' @param time Required. The name of the numeric column in `data` holding
#'   each row's time point. A column name, not a number. A column that is
#'   not numeric (a `Date`, factor or character column, among others) is an
#'   error, as are missing values: the growth model fits time as a number,
#'   so the caller chooses its origin and unit. `id` and `time` must differ
#'   from each other and from `dv` and `value`, the two column names the
#'   output reserves.
#' @return A data frame with `3 * nrow(data)` rows and four columns, named
#'   `<id>` (a factor), `<time>` (numeric), `dv` (a factor with levels `e`,
#'   `x` and `y`) and `value`. Rows are ordered by input row, then by `dv`.
#'   Each `value` is the `Elev`, `Xval` or `Yval` that
#'   [ssm_parameters_id()] with `id = NULL` gives that input row.
#' @family growth functions
#' @export
#' @examples
#' data("simulated_growth")
#' long <- ssm_growth_data(
#'   simulated_growth,
#'   scales = PANO(),
#'   id = "person",
#'   time = "wave"
#' )
#' head(long, 6)
ssm_growth_data <- function(data, scales, angles = octants(), id, time) {

  stopifnot(is.data.frame(data) || is.matrix(data))
  if (is.matrix(data)) data <- as.data.frame(data)
  stopifnot(is_var(scales))
  stopifnot(is.numeric(angles))
  if (length(scales) != length(angles)) {
    stop("`scales` and `angles` must have the same length.", call. = FALSE)
  }

  check_growth_name(id, "id")
  check_growth_name(time, "time")
  if (identical(time, id)) {
    stop("`id` and `time` must name different columns.", call. = FALSE)
  }
  if (!id %in% names(data)) {
    stop("`id` names a column not in `data`: \"", id, "\".", call. = FALSE)
  }
  if (!time %in% names(data)) {
    stop("`time` names a column not in `data`: \"", time, "\".",
         call. = FALSE)
  }
  id_col <- data[[id]]
  time_col <- data[[time]]
  if (!is.numeric(time_col)) {
    stop("`time` must name a numeric column (class ",
         paste(class(time_col), collapse = "/"),
         "); convert it to a number first.", call. = FALSE)
  }
  if (anyNA(id_col)) {
    stop("`id` column has missing values; persons cannot be silently dropped.",
         call. = FALSE)
  }
  if (anyNA(time_col)) {
    stop("`time` column has missing values; rows cannot be silently dropped.",
         call. = FALSE)
  }

  # One profile per row. ssm_parameters_id() warns when a row's displacement
  # is undefined (flat, zero amplitude or missing); the long table carries
  # e, x and y only, all of which are defined for a flat row, so that one
  # warning concerns nothing this table holds and is muffled. Any other
  # warning the scorer raises passes through.
  coord <- withCallingHandlers(
    ssm_parameters_id(data, scales = scales, angles = angles, id = NULL),
    warning = function(w) {
      if (grepl("undefined displacement", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  n <- nrow(data)
  each3 <- rep(seq_len(n), each = 3)
  out <- data.frame(
    factor(id_col)[each3],
    as.numeric(time_col)[each3],
    factor(rep(c("e", "x", "y"), times = n), levels = c("e", "x", "y")),
    as.numeric(t(as.matrix(coord[c("Elev", "Xval", "Yval")]))),
    stringsAsFactors = FALSE, row.names = NULL
  )
  colnames(out) <- c(id, time, "dv", "value")
  out
}
