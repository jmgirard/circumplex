#' Calculate difference between angular displacements around circle
#' 
#' @param w1,w2 Angular displacements (numeric).
#' @return Difference between \code{w1} and \code{w2} in circular space.

wd <- function(w1, w2) {
  w <- ((w2 - w1 + 180) %% 360) - 180
  return(w)
}

#' Calculate correct quantiles for circular and non-circular data
#'
#' @param data A vector or data frame containing numeric data.
#' @param probs A vector of probabilities to get quantiles for.
#' @return A vector of quantiles corresponding to the probabilities in
#'   \code{probs}; these quantiles will be circular if the data are circular and
#'   they wiull be normal quantiles otherwise.

smart_quantile <- function(data, probs){
  if (is.circular(data)) {
    q <- circular::quantile.circular(data, probs = probs) %% 360
  } else {
    q <- quantile(data, probs = probs)
  }
  return(q)
}
