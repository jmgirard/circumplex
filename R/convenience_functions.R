#' Two-letter abbreviations for octant circumplex scales
#'
#' Return a vector of abbreviations for octant circumplex scales, from PA to NO.
#'
#' @param case An optional string the determines whether the abbreviations should be
#'   in uppercase or lowercase. (default = "upper")
#' @return A character vector with eight elements, each corresponding to the
#'   abbreviation of an octant subscale: PA, BC, DE, FG, HI, JK, LM, NO.
#' @export
#' @examples
#' PANO()
#' PANO(case = "lower")
#'
PANO <- function(case = "upper") {
  case <- match.arg(case, choices = c("upper", "lower"))
  out <- c("PA", "BC", "DE", "FG", "HI", "JK", "LM", "NO")
  if (case == "lower") out <- tolower(out)
  out
}

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
#' @examples
#' poles()
poles <- function() {
  as_degree(c(90, 180, 270, 360))
}

#' Unwrap a sequence of angles onto a continuous branch
#'
#' Unwrap a temporally ordered sequence of angular displacements (e.g., one
#' displacement per measurement wave) onto a continuous numeric branch, so
#' that a trajectory drifting across the 0/360 boundary becomes a smooth
#' sequence suitable for linear growth modeling. Each input is first wrapped
#' to \[0, 360) (any real numbers are accepted); the output then starts at the
#' first wave's wrapped value and accumulates the shortest signed rotation
#' between successive waves, so successive values never differ by more than
#' 180 degrees. For example, `c(350, 10, 30)` unwraps to `c(350, 370, 390)`.
#'
#' Two conventions are pinned. An exact 180-degree step is directionally
#' ambiguous; it is resolved as +180 (ascending), matching the package's
#' contrast convention of reporting an exact half-turn as +180. A missing
#' wave makes every subsequent step branch-ambiguous, so `NA` propagates from
#' the missing wave onward rather than silently bridging the gap.
#'
#' Unwrapping assumes the sequence really does move by less than a half-turn
#' between successive waves; when the truth moves faster than the sampling
#' (or persons occupy heterogeneous locations with no common branch), the
#' unwrapped branch is wrong without warning. See the package's growth
#' modeling vignette for these failure modes and the bivariate (x, y)
#' alternative that avoids them.
#'
#' @param x A numeric vector of angles in degrees, in temporal order. Any
#'   real values are accepted and are wrapped to \[0, 360) first.
#' @return A plain numeric vector of the same length: the unwrapped angles in
#'   degrees on a continuous branch anchored at the first wave's wrapped
#'   value. Values may legitimately fall outside \[0, 360); the LM = 360
#'   reporting convention applies to displacements, not to the unwrapped
#'   branch (an input of 360 anchors at 0).
#' @export
#' @examples
#' angle_unwrap(c(350, 10, 30))
#' angle_unwrap(c(10, 350, 330))
#' angle_unwrap(c(350, NA, 30))
angle_unwrap <- function(x) {
  stopifnot(is.numeric(x))
  n <- length(x)
  if (n == 0) return(numeric(0))
  w <- as.numeric(x) %% 360
  if (n == 1) return(w)
  # Shortest signed rotation between successive waves, computed directly in
  # degrees (same formula and half-turn remap as angle_dist(), which works in
  # radians; degree arithmetic keeps integer-degree fixtures bit-exact). An
  # exact -180 only arises from an exact half-turn step, remapped to +180 per
  # the contrast convention. cumsum() propagates NA from a missing wave
  # onward, which is the documented policy (every later value is
  # branch-ambiguous).
  d <- ((w[-1] - w[-n] + 180) %% 360) - 180
  d[!is.na(d) & d == -180] <- 180
  cumsum(c(w[1], d))
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
#' @examples
#' quadrants()
quadrants <- function() {
  as_degree(c(135, 225, 315, 45))
}
