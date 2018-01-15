#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Calculate difference between two angular displacements
#'
#' @param d1,d2 Angular displacements (in degrees).
#' @return The difference (in degrees) between \code{d1} and \code{d2}. Positive
#'   and negative differences indicate that to move from \code{d1} to \code{d2},
#'   you would need to move clockwise and counter-clockwise around the
#'   circumference, respectively. Note that currently differences of 180 and
#'   -180 are both represented as -180, which may cause problems when
#'   calculating the bootstrap confidence interval.
#' @examples
#' disp_diff(270, 225) #>  45
#' disp_diff( 90, 180) #> -90
#' disp_diff( 45, 315) #>  90
disp_diff <- function(d1, d2) {
  assert_that(is.scalar(d1), is.scalar(d2))
  ((d1 - d2 + 180) %% 360) - 180
  #TODO: Figure out what to do when diff == -180 (can't ignore direction)
  #Look into circular.quantile to see if that may reveal a solution
}

rwd <- function(wd) {
  ((wd + 180) %% 360) - 180
}

#' Compute difference between two sets of SSM parameters
#'
#' @param p1,p2 Outputs from \code{ssm_parameters()}.
#' @return A numeric vector of differences between the parameters in \code{p1}
#'   and \code{p2}, respecting the circular nature of displacement parameters.
param_diff <- function(p1, p2) {
  assert_that(is.numeric(p1), is.numeric(p2))
  pd <- p1 - p2
  pd[[5]] <- disp_diff(p1[[5]], p2[[5]])
  pd
}

#' Calculate correct quantiles for circular and non-circular data
#'
#' @param .data A vector or data frame containing numeric data.
#' @param probs A vector of probabilities to get quantiles for.
#' @return A vector of quantiles corresponding to the probabilities in
#'   \code{probs}; these quantiles will be circular if the data are circular and
#'   they wiull be normal quantiles otherwise.

smart_quantile <- function(.data, probs){
  #TODO: Replace this function with quantile by using S3:circular
  
  if (circular::is.circular(.data)) {
    circular::quantile.circular(.data, probs = probs) %% 360
  } else {
    stats::quantile(.data, probs = probs)
  }
}

#' Coerce a variable to the circular data type
#'
#' @param x A vector of numbers.
#' @return The same vector as \code{x} but with the circular data type (in
#'   degree units and counter-clockwise rotation) for the circular package.

make_circular <- function(x) {
  circular::circular(x, units = "degrees", rotation = "counter")
}

#' Calculate bootstrap confidence intervals given data and function
#'
#' @param .data A matrix or data frame containing circumplex scales and possibly
#'   a measure (if called from ssm_measures).
#' @param bs_function A function that calculates the variables to be resampled.
#' @param ssm The best-guess SSM parameter estimates prior to bootstrapping.
#' @param angles A numerical vector specifying the angular displacement of each
#'   circumplex scale (in degrees).
#' @param boots A positive integer specifying the number of bootstrap resamples.
#' @param interval The confidence intervals' percentage level (e.g., 0.95).
#' @param ... Additional parameters to be passed to the \code{boot()} function.
#' @return A tibble containing SSM parameters (point and interval estimates).

ssm_bootstrap <- function(.data, statistic, angles, boots, interval, ...) {

  # Perform bootstrapping ---------------------------------------------------
  bs_results <- boot::boot(
    data = .data,
    statistic = statistic, 
    R = boots,
    angles = angles,
    ...
  )

  # Calculate point and interval estimates from bootstrap results -----------
  bs_est <- bs_results$t0 %>%
    reshape_params() %>% 
    `colnames<-`(c("e_est", "x_est", "y_est", "a_est", "d_est", "fit"))
  
  bs_t <- bs_results$t %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate_at(.funs = make_circular, .vars = (1:(ncol(.) / 6) * 6 - 1))
  
  bs_lci <- bs_t %>%
    purrr::map_dbl(smart_quantile, probs = ((1 - interval) / 2)) %>% 
    reshape_params() %>% 
    `colnames<-`(c("e_lci", "x_lci", "y_lci", "a_lci", "d_lci", "f_lci")) %>% 
    dplyr::select(-f_lci)
  
  bs_uci <- bs_t %>% 
    purrr::map_dbl(smart_quantile, probs = (1 - (1 - interval) / 2)) %>% 
    reshape_params() %>% 
    `colnames<-`(c("e_uci", "x_uci", "y_uci", "a_uci", "d_uci", "f_uci")) %>% 
    dplyr::select(-f_uci)
  
  results <- dplyr::bind_cols(bs_est, bs_lci, bs_uci)
  #TODO: Add a column describing each row (e.g., group name)
  
  results
}

#' Enumerate all unique pairwise combinations of a factor's levels
#'
#' @param f A vector of type factor.
#' @return An n-by-2 character matrix containing the n unique pairwise
#'   combinations of the levels in \code{f}.

unique_pairs <- function(f) {
  tibble::as_tibble(
    gtools::combinations(n = nlevels(f), r = 2, v = levels(f), repeats = FALSE)
  )
}

ggrad <- function(v) {
  (v - 90) * (-pi / 180)
}

pretty_max <- function(v) {
    amax <- max(v, na.rm = TRUE)
  options <- c(
    0.05, 0.10, 0.15, 0.20, 0.25,
    0.50, 0.75, 1.00, 1.25, 1.50, 
    2.00, 2.50, 3.00, 4.00, 5.00)
  match <- options > amax
  if (sum(match) > 1) {
    options[match][[2]]
  } else if (sum(match) == 1) {
    options[match][[1]]
  } else {
    ceiling(amax * 1.50)
  }
}

reshape_params <- function(df) {
  df %>%
    matrix(nrow = 6) %>% 
    t() %>% 
    tibble::as_tibble()
}

ssm_by_group <- function(scores, angles, contrast) {
  
  # Transpose scores so that each group is a column -------------------------
  scores <- scores %>% 
    gather(key = Scale, value = Score, -Group, factor_key = TRUE) %>% 
    spread(key = Group, value = Score) %>% 
    select(-Scale)
  
  # To model contrast, subtract scores then SSM -----------------------------
  if (contrast == "model") {
    scores <- scores %>%
      mutate(Contrast = .[[2]] - .[[1]])
  }
  
  # Calculate SSM parameters for each column --------------------------------
  results <- scores %>%
    map(ssm_parameters, angles) %>%
    flatten_dbl()
  
  # To test contrast, SSM then subtract parameters --------------------------
  if (contrast == "test") {
    results[13:18] <- param_diff(results[7:12], results[1:6])
  }
  
  results
}
