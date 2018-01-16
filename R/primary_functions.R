#' Profile (Mean-based) Structural Summary Method
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
#' @param contrast  (default = "none").
#' @param plot A logical determining whether a plot should be created (default =
#'   TRUE).
#' @param ... Additional parameters to be passed to \code{circle_plot()}.
#'   Examples include \code{amax} and \code{font.size}.
#' @return A tibble containing SSM parameters (point and interval estimates) for
#'   each group's mean profile (or the entire mean profile without grouping).
#' @seealso \code{\link{ssm_measures}}, which calculates SSM parameters for
#'   measures using a correlation-based approach.
#' @export

ssm_profiles <- function(.data, scales, angles, boots = 2000, interval = 0.95,
  grouping, contrast = "none", plot = TRUE, ...) {
  
  # Enable column specification using tidyverse-style NSE -------------------
  scales_en <- rlang::enquo(scales)
  
  # Check that inputs are valid ---------------------------------------------
  assert_that(is.numeric(angles), is.count(boots), is.flag(plot))
  assert_that(contrast %in% c("none", "model", "test"))
  assert_that(is.scalar(interval), interval > 0, interval < 1)
  
  # Select circumplex scales and grouping variable (if applicable) ----------
  if (base::missing(grouping)) {
    bs_input <- .data %>% 
      dplyr::select(!!scales_en) %>% 
      dplyr::mutate(Group = 1)
  } else {
    #TODO: Check that there are only two groups if contrast != none
    grouping_en <- rlang::enquo(grouping)
    #grouping_qn <- rlang::quo_name(grouping_en)
    bs_input <- .data %>%
      dplyr::select(!!grouping_en, !!scales_en) %>% 
      dplyr::mutate(Group = factor(!!grouping_en)) %>% 
      dplyr::select(-!!grouping_en)
  }
  
  bs_function <- function(.data, index, angles, contrast) {
    resample <- .data[index, ]
    scores_r <- resample %>% 
      dplyr::group_by(Group) %>% 
      dplyr::summarize_all(mean)
    ssm_by_group(scores_r, angles, contrast)
  }
  
  bs_output <- ssm_bootstrap(
    .data = bs_input,
    statistic = bs_function,
    angles = angles,
    boots = boots,
    interval = interval,
    contrast = contrast,
    strata = bs_input$Group
  )
  
  row_labels <- levels(bs_input$Group)
  if (contrast != "none") {
    row_labels <- c(row_labels, paste0("Diff ", contrast))
  }
  results <- bs_output %>% 
    dplyr::mutate(label = row_labels)
  
  # ht <- results_table(bs_output, contrast = pairwise,
  #   group = !base::missing(grouping)) %>% 
  #   htmlTable::htmlTable(
  #     caption = "Structural Summary Method Parameters with
  #     Bootstrap Confidence Intervals",
  #     align = "llllll",
  #     align.header = "llllll",
  #     rnames = FALSE,
  #     css.cell = "padding-right: 1em; min-width: 3em; white-space: nowrap;"
  #   )
  # print(ht)
  
  invisible(results)
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
  
  # Reshape parameters from wide to long format -----------------------------
  reshape_params <- function(df) {
    df %>%
      matrix(nrow = 6) %>% 
      t() %>% 
      tibble::as_tibble()
  }
  
  # Calculate point and interval estimates from bootstrap results -----------
  bs_est <- bs_results$t0 %>%
    reshape_params() %>% 
    `colnames<-`(c("e_est", "x_est", "y_est", "a_est", "d_est", "fit"))
  
  bs_t <- bs_results$t %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate_at(.funs = as_circular, .vars = (1:(ncol(.) / 6) * 6 - 1))
  
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
  
  dplyr::bind_cols(bs_est, bs_lci, bs_uci)
  
}
