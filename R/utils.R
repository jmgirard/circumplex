#' Pipe operator
#'
#' See \code{magrittr} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Save a ggplot with sensible defaults
#'
#' See \code{ggplot2} for details.
#'
#' @name ggsave
#' @rdname ggsave
#' @keywords internal
#' @export
#' @importFrom ggplot2 ggsave
#' @usage ggsave(filename, plot = last_plot(), device = NULL, path = NULL, scale
#'   = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300,
#'   limitsize = TRUE, ...)
NULL

#' Angular displacements for octant circumplex scales
#'
#' Return a vector of angular displacements, in degrees, for eight equally
#' spaced circumplex scales corresponding to the circumplex octants. Can be
#' passed to the \code{angles} parameter of other functions in this package.
#'
#' @return A numeric vector with eight elements, each corresponding to the
#'   angular displacement (in degrees) of a subscale, in the following order:
#'   PA, BC, DE, FG, HI, JK, LM, NO.
#' @export
#' @usage octants()
octants <- function() {
  c(90, 135, 180, 225, 270, 315, 360, 45)
}

#' Angular displacements for pole circumplex scales
#'
#' Return a vector of angular displacements, in degrees, for four equally spaced
#' circumplex scales corresponding to the circumplex poles. Can be passed to the
#' \code{angles} parameter of other functions in this package.
#'
#' @return A numeric vector with four elements, each corresponding to the
#'   angular displacement (in degrees) of a subscale, in the following order:
#'   PA, DE, HI, LM.
#' @export
#' @usage poles()
poles <- function() {
  c(90, 180, 270, 360)
}

#' Angular displacements for quadrant circumplex scales
#'
#' Return a vector of angular displacements, in degrees, for four equally spaced
#' circumplex scales corresponding to the circumplex quadrants. Can be passed to
#' the \code{angles} parameter of other functions in this package.
#'
#' @return A numeric vector with eight elements, each corresponding to the
#'   angular displacement (in degrees) of a subscale, in the following order:
#'   BC, FG, JK, NO.
#' @export
#' @usage quadrants()
quadrants <- function() {
  c(135, 225, 315, 45)
}

#' Standardize specified variables in a data frame
#'
#' Takes in a data frame and the location of one or more variables to
#' standardize. Returns that same data frame where the specified variables have
#' been standardized (i.e., turned into z-scores with mean 0 and variance 1).
#'
#' @param .data A matrix or data frame containing at least one variable.
#' @param .vars A list of the variables or column numbers in \code{.data}
#'   that contain circumplex scales (in tidyverse-style NSE specification).
#' @return A data frame containing standardized versions of the variables
#'   specified in \code{.vars}, as well as any additional variables that
#'   were included in \code{.data}. Variable ordering is preserved.
#' @export
standardize <- function(.data, .vars) {
  
  # Enable tidy evaluation
  vars_en <- rlang::enquo(.vars)
  
  # Check for valid arguments
  assert_that(is_provided(.data), is_enquo(!!vars_en))
  
  # Convert the specified variables to z-scores
  .data %>% 
    dplyr::mutate_at(
      .funs = dplyr::funs(as.numeric(scale(., center = TRUE, scale = TRUE))),
      .vars = dplyr::vars(!!vars_en)
    )
  
}

# Compute differences between two sets of SSM parameters -----------------------
param_diff <- function(p1, p2) {
  assert_that(is.numeric(p1), is.numeric(p2))
  pd <- p1 - p2
  pd[[5]] <- angle_dist(as_radian(p1[[5]]), as_radian(p2[[5]]))
  pd
}

# Calculate angular distance ---------------------------------------------------
angle_dist <- function(x, y) {
  ((x - y + pi) %% (2 * pi)) - pi
}

# Convert degrees to ggplot's radian format ------------------------------------
ggrad <- function(v) {
  v <- as.numeric(v)
  (v - 90) * (-pi / 180)
}

# Convert percent number to a formatted string ---------------------------------
str_percent <- function(x, digits = 2) {
  paste0(round(x, digits + 2) * 100, "%")
}

# Determine good max amplitude value for circle plot ---------------------------
pretty_max <- function(v) {
  amax <- max(v, na.rm = TRUE)
  options <- c(
    0.05, 0.10, 0.15, 0.20, 0.25,
    0.50, 0.75, 1.00, 1.25, 1.50,
    2.00, 2.50, 3.00, 4.00, 5.00
  )
  match <- options > amax
  if (sum(match) >= 1) {
    out <- options[match][[1]]
  } else {
    out <- ceiling(amax * 1.50)
  }
  out
}