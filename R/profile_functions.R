#' Mean Profile Structural Summary Method
#'
#' Calculate SSM parameters with bootstrapped confidence intervals for the mean
#' of any number of groups.
#'
#' @param .data A matrix or data frame containing at least circumplex scales.
#' @param scales A list of the variables in \code{.data} that contain circumplex
#'   scales (in tidyverse-style NSE specification, see examples).
#' @param angles A numerical vector containing the angular displacement of each
#'   circumplex scale included in \code{scales} (in degrees).
#' @param boots The number of bootstrap resamples to use in calculating the
#'   confidence intervals (default = 2000).
#' @param interval The confidence intervals' percentage level (default = 0.95).
#' @param grouping Optional argument: the variable in \code{.data} that contains
#'   the group membership of each observation. To assess the mean profile of all
#'   observations, do not supply this argument (see examples).
#' @param pairwise A logical determining whether the groups defined by the
#'   \code{grouping} variable should be compared. If TRUE, the difference
#'   between each unique pairwise combination of groups will be output; if
#'   FALSE, the groups' mean profiles will be output (default = FALSE).
#' @param plot A logical determining whether a plot should be created (default =
#'   TRUE).
#' @param ... Additional parameters to be passed to \code{ssm_plot()}.
#' @return A tibble containing SSM parameters (point and interval estimates) for
#'   each group's mean profile (or the entire mean profile without grouping).

ssm_profiles <- function(.data, scales, angles,
  boots = 2000, interval = 0.95, grouping, pairwise = FALSE, plot = TRUE, ...) {
  
  # Enable column specification using tidyverse-style NSE -------------------
  scales_en <- rlang::enquo(scales)
  
  # Check that inputs are valid ---------------------------------------------
  assert_that(is.numeric(angles), is.count(boots), is.flag(pairwise))
  assert_that(is.numeric(interval), interval > 0, interval < 1)
  
  # Handle the presence or absence of a grouping variable -------------------
  if (base::missing(grouping)) {
    results <- .data %>%
      dplyr::select(!!scales_en) %>%
      ssm_profiles_one(angles, boots, interval) %>%
      dplyr::mutate(Group = factor(Inf)) %>%
      dplyr::select(Group, dplyr::everything())
    if (plot == TRUE) {
      p <- ssm_plot(results, angles, "Profile")
      print(p)
    }
  } else {
    grouping_en <- rlang::enquo(grouping)
    grouping_qn <- rlang::quo_name(grouping_en)
    data_groups <- .data %>%
      dplyr::select(!!grouping_en, !!scales_en) %>% 
      dplyr::mutate(Group = factor(!!grouping_en)) %>% 
      dplyr::select(-!!grouping_en)
    results <- data_groups %>% 
      dplyr::group_by(Group) %>%
      purrrlyr::by_slice(ssm_profiles_one, angles, boots, interval,
        .collate = "rows")
    if (plot == TRUE) {
      p <- ssm_plot(results, angles, "Profile")
      print(p)
    }
    if (pairwise == TRUE) {
      g_pairs <- unique_pairs(data_groups$Group)
      cmp_function <- function(pair, .data) {
        data_compare <- .data %>%
          dplyr::filter(Group %in% as.character(pair))
        ssm_profiles_two(data_compare, angles, boots, interval)
      }
      c_results <- g_pairs %>%
        purrrlyr::by_row(cmp_function, data_groups, .collate = "rows") %>%
        dplyr::mutate(Contrast = sprintf("%s-%s", V2, V1)) %>%
        dplyr::select(-c(V1, V2, .row))
      # TODO: Create a forest plot for the contrast effects here
      results <- dplyr::bind_rows(results, c_results) %>%
        dplyr::select(Group, Contrast, dplyr::everything())
    }
  }
  # TODO: Create htmlTable from results (after tidying it up)
  results
}

#' Mean Profile Structural Summary Method
#'
#' Worker function for ssm_profiles, calculates point and interval estimates for
#'   SSM parameters in a single group's mean profile.
#'
#' @param .data A matrix or data frame containing only circumplex scales.
#' @param angles A numerical vector containing the angular displacement of each
#'   circumplex scale included in \code{scales} (in degrees).
#' @param boots The number of bootstrap resamples to use in calculating the
#'   confidence intervals (default = 2000).
#' @param interval The confidence intervals' percentage level (default = 0.95).
#' @return A tibble containing SSM parameters (point and interval estimates) for
#'   the group's mean profile.

ssm_profiles_one <- function(.data, angles, boots, interval) {
  
  # Check that inputs are valid ---------------------------------------------
  assert_that(are_equal(length(.data), length(angles)))
    
  # Get SSM parameter estimates for mean profile ----------------------------
  scores <- .data %>% colMeans()
  ssm <- ssm_parameters(scores, angles, tibble = FALSE)
  
  # Perform bootstrap on SSM parameters -------------------------------------
  bs_function <- function(.data, index, angles) {
    resample <- .data[index, ]
    scores_r <- colMeans(resample)
    ssm_parameters(scores_r, angles, tibble = FALSE)
  }
  ssm_bootstrap(.data, bs_function, ssm, angles, boots, interval)
  
}

#' Mean Profile Comparison Structural Summary Method
#'
#' Worker function for ssm_profiles, calculates point and interval estimates for
#' the difference between two groups' mean profile SSM parameters.
#'
#' @param .data A matrix or data frame containing circumplex scales and a
#'   grouping variable with only two levels.
#' @param angles A numerical vector containing the angular displacement of each
#'   circumplex scale included in \code{.data} (in degrees).
#' @param boots The number of bootstrap resamples to use in calculating the
#'   confidence intervals.
#' @param interval The confidence intervals' percentage level.
#' @return A tibble containing SSM parameters (point and interval estimates) for
#'   the difference between two groups' mean profiles.

ssm_profiles_two <- function(.data, angles, boots, interval) {
  
  # TODO: Can increase speed of pairwise comparisons by doing a single
  # stratified bootstrapping with all pairwise comparisons built in, rather than
  # separate bootstraps for each pairwise combination.
  
  scores <- .data %>%
    dplyr::group_by(Group) %>%
    dplyr::summarize_all(mean)
  ssm_g1 <- ssm_parameters(as.double(scores[1, 2:ncol(scores)]), angles, FALSE)
  ssm_g2 <- ssm_parameters(as.double(scores[2, 2:ncol(scores)]), angles, FALSE)
  ssm_gd <- param_diff(ssm_g1, ssm_g2)
  
  bs_function <- function(.data, index, angles) {
    resample <- .data[index, ]
    scores_r <- resample %>%
      dplyr::group_by(Group) %>%
      dplyr::summarize_all(mean)
    ssm_r1 <- ssm_parameters(as.double(scores_r[1, 2:ncol(scores_r)]), angles, FALSE)
    ssm_r2 <- ssm_parameters(as.double(scores_r[2, 2:ncol(scores_r)]), angles, FALSE)
    param_diff(ssm_r1, ssm_r2)
  }
  ssm_bootstrap(.data, bs_function, ssm_gd, angles, boots, interval,
    strata = .data$Group)
}
