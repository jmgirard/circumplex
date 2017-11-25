#' Calculate difference between angular displacements around circle
#' 
#' @param w1,w2 Angular displacements (numeric).
#' @return Difference between \code{w1} and \code{w2} in circular space.

wd <- function(w1, w2) {
  w <- ((w2 - w1 + 180) %% 360) - 180
  return(w)
}
