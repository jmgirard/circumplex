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
#'   column. Two rows with the same id and the same time are an error. Time
#'   values are compared exactly, with no rounding. Ids
#'   are compared after conversion to a factor, which matches them by
#'   `as.character()` (15 significant digits), so two numeric ids that
#'   differ only beyond that precision count as one person. A column name
#'   that occurs more than once is an error when `id`, `time` or a name in
#'   `scales` refers to it.
#' @param scales Required. The variable names or column numbers for the
#'   variables in `data` that contain circumplex scales. They may not select
#'   the `id` or `time` column.
#' @param angles Optional. A numeric vector containing the angular
#'   displacement of each circumplex scale included in `scales`, in degrees
#'   (default = `octants()`).
#' @param id Required. The name of the column in `data` identifying persons.
#'   A column name, not a number. Missing values are an error.
#' @param time Required. The name of the numeric column in `data` holding
#'   each row's time point. A column name, not a number. A column that is
#'   not numeric (a `Date`, factor or character column, among others) is an
#'   error, as are missing and infinite values: the growth model fits time
#'   as a number, so the caller chooses its origin and unit. `id` and `time`
#'   must differ from each other and from `dv` and `value`, the two column
#'   names the output reserves.
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
  # The names after conversion are the ones `[[` resolves against: duplicates
  # survive as.data.frame(), and a blank matrix name becomes "V<n>", which can
  # itself duplicate another column's name.
  data_names <- names(data)
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

  # A name that occurs twice resolves silently to its first column, so each
  # name an argument selects by must be unique. Numeric scales select by
  # position and are judged below by the column they reach.
  dup_names <- unique(data_names[duplicated(data_names)])
  if (id %in% dup_names) {
    stop("`id` names a column that occurs more than once in `data`: \"",
         id, "\".", call. = FALSE)
  }
  if (time %in% dup_names) {
    stop("`time` names a column that occurs more than once in `data`: \"",
         time, "\".", call. = FALSE)
  }
  # An NA scale name is left to the scorer, which refuses it as an undefined
  # column.
  dup_scales <- if (is.character(scales)) {
    scales[!is.na(scales) & scales %in% dup_names]
  } else {
    character(0)
  }
  if (length(dup_scales) > 0) {
    stop("`scales` names a column that occurs more than once in `data`: \"",
         dup_scales[[1]], "\".", call. = FALSE)
  }

  # The column each scale reaches, by name or by position; indexing
  # seq_along(data) applies R's own rules, truncating a non-integer number.
  scale_cols <- if (is.character(scales)) {
    scales
  } else {
    names(data)[seq_along(data)[scales]]
  }
  # One column selected twice would enter the profile at two angles. Names
  # are judged as given and numbers by the position they reach; an NA name
  # or a number that reaches no column is left to the scorer's error.
  scale_keys <- if (is.character(scales)) {
    scales[!is.na(scales)]
  } else {
    pos <- seq_along(data)[scales]
    pos[!is.na(pos)]
  }
  repeated_scale <- anyDuplicated(scale_keys)
  if (repeated_scale > 0) {
    col <- scale_keys[[repeated_scale]]
    if (is.numeric(col)) col <- data_names[[col]]
    stop("`scales` selects the column \"", col, "\" more than once.",
         call. = FALSE)
  }
  if (id %in% scale_cols) {
    stop("`scales` selects the `id` column: \"", id, "\".", call. = FALSE)
  }
  if (time %in% scale_cols) {
    stop("`scales` selects the `time` column: \"", time, "\".", call. = FALSE)
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
  if (any(is.infinite(time_col))) {
    stop("`time` column has infinite values; the growth model needs a ",
         "finite time for every row.", call. = FALSE)
  }

  # The output's id is factor(id_col), which labels values by as.character()
  # (15 significant digits for doubles), so two distinct values can share a
  # level and two persons would become one. Refuse that before judging rows.
  id_out <- factor(id_col)
  id_values <- unique(id_col)
  if (nlevels(id_out) < length(id_values)) {
    first_rows <- match(id_values, id_col)
    labels <- as.character(id_out)[first_rows]
    clash <- which(duplicated(labels))[[1]]
    rows <- sort(first_rows[labels == labels[[clash]]])[1:2]
    stop("`id` values in rows ", rows[[1]], " and ", rows[[2]],
         " differ but share the label \"", labels[[clash]],
         "\" in the output, so two persons would become one; give each ",
         "person an id that stays distinct as text.", call. = FALSE)
  }

  # One row per person per time point, judged on the id and time as the
  # output holds them.
  time_out <- as.numeric(time_col)
  repeated <- duplicated(data.frame(id_out, time_out))
  if (any(repeated)) {
    first <- which(repeated)[[1]]
    stop("`data` has more than one row for `id` \"", id_out[[first]],
         "\" at `time` ", format(time_out[[first]], digits = 15),
         "; it must hold one row per person per time point.", call. = FALSE)
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
