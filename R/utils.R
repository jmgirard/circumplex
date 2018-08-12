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
#' @examples
#' octants()
octants <- function() {
  as_degree(c(90, 135, 180, 225, 270, 315, 360, 45))
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
#' @examples
#' poles()
poles <- function() {
  as_degree(c(90, 180, 270, 360))
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
#' @examples
#' quadrants()
quadrants <- function() {
  as_degree(c(135, 225, 315, 45))
}

#' Standardize circumplex scales using normative data
#' 
#' Take in a data frame containing circumplex scales, angle definitions for each
#' scale, and normative data (from the package or custom) and return that same 
#' data frame with each specified circumplex scale transformed into standard
#' scores (i.e., z-scores) based on comparison to the normative data.
#' 
#' @param .data Required. A data frame containing at least circumplex scales.
#' @param scales Required. The variable names or column numbers for the
#'   variables in \code{.data} that contain circumplex scales to be analyzed.
#' @param angles Required. A numeric vector containing the angular displacement
#'   of each circumplex scale included in \code{scales} (in degrees).
#' @param norms Required. A data frame containing normative data for each
#'   circumplex scale included in \code{scales}. The following variables are
#'   required: \code{Angle}, \code{M}, and \code{SD}. See \code{?iipsc}.
#' @return A data frame that matches \code{.data} except that the variables
#'   included in \code{scales} have been transformed into standard scores.
#' @export
#' @examples 
#' data("jz2017")
#' data("iipsc")
#' z <- standardize(jz2017, scales = PA:NO, angles = octants(), norms = iipsc)
standardize <- function(.data, scales, angles, norms) {
  scales_en <- rlang::enquo(scales)
  scale_names <- names(dplyr::select(.data, !!scales_en))
  assert_that(is.numeric(angles))
  assert_that(length(scale_names) == length(angles))
  assert_that(length(scale_names) <= nrow(norms))
  for (i in 1:length(angles)) {
    scale_i <- scale_names[[i]]
    index_i <- norms$Angle == angles[[i]]
    m_i <- norms$M[index_i]
    s_i <- norms$SD[index_i]
    .data <- .data %>% 
      dplyr::mutate_at(dplyr::funs((. - m_i) / s_i), .vars = scale_i)
  }
  .data
}

#' Ipsatize circumplex scales using deviation scoring across variables
#' 
#' Description.
#' 
#' @param .data Required. A data frame containing at least circumplex scales.
#' @param scales Required. The variable names or column numbers for the
#'   variables in \code{.data} that contain circumplex scales to be ipsatized.
#' @return A data frame that matches \code{.data} except that the variables
#'   included in \code{scales} have been transformed into ipsatized scores.
#' @export
#' @examples 
#' data("jz2017")
#' ipsatize(jz2017, PA:NO)
ipsatize <- function(.data, scales) {
  scales_en <- rlang::enquo(scales)
  .data <- .data %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(.sm = mean(!!scales_en)) %>% 
    dplyr::mutate_at(
      .vars = dplyr::vars(!!scales_en),
      .funs = dplyr::funs(. - .sm)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-.sm)
  .data
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
  paste0(floor(x * 10 ^ (digits + 2)) / (10 ^ digits), "%")
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