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
  if (circular::is.circular(data)) {
    q <- circular::quantile.circular(data, probs = probs) %% 360
  } else {
    q <- quantile(data, probs = probs)
  }
  return(q)
}

#' Compute difference between two sets of SSM parameters
#'
#' @param p1,p2 Outputs from \code{ssm_parameters()}.
#' @return A numerical vector or tibble (data frame) with \code{p2 - p1} for
#'   each parameter, while accounting for the fact that differences in
#'   displacement need to be handled specially.

param_diff <- function(p1, p2) {
  pd <- p2 - p1
  pd[5] <- wd(p1[5], p2[5])
  return(pd)
}

#' Coerce a variable to the circular data type
#'
#' @param x A vector of numbers.
#' @return The same vector as \code{x} but with the circular data type (in
#'   degree units and counter-clockwise rotation).

make_circular <- function(x) {
  y <- circular::circular(x, units = "degrees", rotation = "counter")
  return(y)
}
