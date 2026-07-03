# Scale helpers for circumplex plots ------------------------------------------

# Resolve a set of scale angles and text labels from the three ways a caller
# can specify them, shared by ggcircumplex() (the circular canvas) and
# scale_x_circumplex() (the linear angle axis) so the same inputs always
# produce the same labels in both contexts. An instrument supplies both the
# angles and (unless labels are given explicitly) the scale abbreviations.
resolve_circumplex_labels <- function(angles, labels, instrument) {
  if (!is.null(instrument)) {
    stopifnot(is_instrument(instrument))
    angles <- instrument$Scales$Angle
    if (is.null(labels)) labels <- instrument$Scales$Abbrev
  }
  stopifnot(is_num(angles))
  stopifnot(is_null_or_char(labels, n = length(angles)))
  list(angles = angles, labels = labels)
}

# Default degree labels for a set of scale angles, shared by the circular canvas
# (ggcircumplex()/circle_base()) and the linear axis (scale_x_circumplex()) so
# both render fractional angles identically (e.g. 22.5 -> "22.5 deg") instead
# of rounding to whole degrees. Integer angles print without a decimal.
circumplex_degree_labels <- function(angles) {
  paste0(angles, "\u00B0")
}

#' Angle-labeled x-axis scale for circumplex plots
#'
#' A \pkg{ggplot2} continuous position scale for the angle axis of a linear
#' circumplex plot, such as the score-by-angle curve drawn by
#' `ssm_plot_curve()`. It places axis breaks at the circumplex scale angles and
#' labels them, by default, with their angular position in degrees. Custom text
#' labels or a `circumplex_instrument` can be supplied instead, using the same
#' conventions as [ggcircumplex()], so the linear axis and the circular canvas
#' label their scales consistently.
#'
#' @param angles Optional. A numeric vector of the angular position (in
#'   degrees) of each circumplex scale (default = `octants()`). Ignored if
#'   `instrument` is supplied.
#' @param labels Optional. Either `NULL` or a character vector of axis labels,
#'   one per angle and in the same order (default = `NULL`, which labels each
#'   break with its angle in degrees, or with the instrument's scale
#'   abbreviations when `instrument` is supplied).
#' @param instrument Optional. Either `NULL` or a `circumplex_instrument`
#'   object (see `instrument()`) from which to take the scale angles and (unless
#'   `labels` is given) abbreviations (default = `NULL`).
#' @param ... Additional arguments passed to
#'   [ggplot2::scale_x_continuous()], such as `name` or `limits`.
#' @return A \pkg{ggplot2} scale object that can be added to a plot with `+`.
#' @seealso [ggcircumplex()]
#' @export
#' @examples
#' # Degree-labeled angle axis
#' scale_x_circumplex(octants())
#'
#' # Label the axis with an instrument's scale abbreviations
#' scale_x_circumplex(instrument = csip)
scale_x_circumplex <- function(angles = octants(), labels = NULL,
                               instrument = NULL, ...) {
  resolved <- resolve_circumplex_labels(angles, labels, instrument)
  scale_labels <- resolved$labels
  if (is.null(scale_labels)) {
    scale_labels <- circumplex_degree_labels
  }
  ggplot2::scale_x_continuous(
    breaks = resolved$angles,
    labels = scale_labels,
    ...
  )
}
