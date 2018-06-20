# Create an Empty Circular Plot
circle_base <- function(angles, labels = sprintf("%d\u00B0", angles),
  amax = 0.5, font.size = 12) {

  ggplot2::ggplot() +
    # Require plot to be square and remove default styling ---------------------
    ggplot2::coord_fixed() +
    ggplot2::theme_void(base_size = font.size) +
    # Expand the axes multiplicatively to fit labels ---------------------------
    ggplot2::scale_x_continuous(expand = c(0.25, 0)) +
    ggplot2::scale_y_continuous(expand = c(0.10, 0)) +
    # Draw segments corresponding to displacement scale -----------------------
    ggplot2::geom_segment(
      aes(
        x = 0,
        y = 0,
        xend = 5 * cos(angles * pi / 180),
        yend = 5 * sin(angles * pi / 180)
      ),
      color = "gray60",
      size = 0.5
    ) +
    # Draw circles corresponding to amplitude scale ----------------------------
    ggforce::geom_circle(
      aes(x0 = 0, y0 = 0, r = 1:4),
      color = "gray60",
      size = 0.5
    ) +
    ggforce::geom_circle(
      aes(x0 = 0, y0 = 0, r = 5),
      color = "gray50",
      size = 1.5
    ) +
    # Draw labels for amplitude scale ------------------------------------------
    ggplot2::geom_label(
      aes(
        x = c(2, 4),
        y = 0,
        label = sprintf("%.2f",
          seq(from = 0, to = amax, length.out = 6)[c(3, 5)])
      ),
      color = "gray20",
      label.size = NA,
      size = font.size / 2.8346438836889
    ) +
    # Draw labels for displacement scale ---------------------------------------
    ggplot2::geom_label(
      aes(
        x = 5.1 * cos(angles * pi / 180),
        y = 5.1 * sin(angles * pi / 180),
        label = labels
      ),
      color = "gray20",
      label.size = NA,
      hjust = "outward",
      vjust = "outward",
      size = font.size / 2.8346438836889
    )

}

#' Create a Circular Plot of SSM Results
#'
#' Take in the results of a Structural Summary Method analysis and plot the
#' point and interval estimate for each row (e.g., group or measure) in a
#' circular space quantified by displacement and amplitude.
#'
#' @param .ssm_object The output of \code{ssm_profiles()} or
#'   \code{ssm_measures()}.
#' @param palette A string corresponding to the Color Brewer palette (default =
#'   "Set1"). See http://www.cookbook-r.com/Graphs/Colors_(ggplot2).
#' @param amax A positive real number corresponding to the radius of the circle.
#'   It is used to scale the amplitude values and will determine which amplitude
#'   labels are drawn.
#' @param font.size A positive real number corresponding to the size (in pt) of
#'   the text labels (default = 12).
#' @return A ggplot variable containing a completed circular plot.
#' @family ssm functions
#' @family visualization functions
#' @export

ssm_plot_circle <- function(.ssm_object, palette = "Set1",
                        amax = pretty_max(.ssm_object$results$a_uci), 
                        font.size = 12) {
  
  df <- .ssm_object$results
  angles <- as.numeric(.ssm_object$details$angles)

  # Convert results to numbers usable by ggplot and ggforce -----------------
  df_plot <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      d_uci = ifelse(d_uci < d_lci, ggrad(d_uci + 360), ggrad(d_uci)),
      d_lci = ggrad(d_lci),
      a_lci = a_lci * 10 / (2 * amax),
      a_uci = a_uci * 10 / (2 * amax),
      x_est = x_est * 10 / (2 * amax),
      y_est = y_est * 10 / (2 * amax)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(label = factor(label, levels = unique(as.character(label))))

  # Initialize and configure the circle plot --------------------------------
  p <- circle_base(angles = angles, amax = amax, font.size = font.size) +
    ggplot2::scale_color_brewer(palette = palette) +
    ggplot2::scale_fill_brewer(palette = palette)
  # TODO: Allow scale customization

  p <- p +
    ggforce::geom_arc_bar(
      data = df_plot,
      aes(
        x0 = 0, y0 = 0, r0 = a_lci, r = a_uci, start = d_lci, end = d_uci,
        fill = label, color = label
      ),
      alpha = 0.5,
      size = 1
    ) +
    ggplot2::geom_point(
      data = df_plot,
      aes(x = x_est, y = y_est, color = label),
      size = 2
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(.ssm_object$type),
      fill = ggplot2::guide_legend(.ssm_object$type)
    ) + 
    ggplot2::theme(
      legend.text = ggplot2::element_text(size = font.size)
    )
  # TODO: Account for the possibility of more than 8 plots

  p
}

#' Create a Difference Plot of SSM Contrast Results
#'
#' Take in the results of a Structural Summary Method analysis with pairwise
#' contrasts and plot the point and interval estimates for each parameter's
#' contrast (e.g., between groups or measures).
#'
#' @param .ssm_object The output of \code{ssm_profiles()} or
#'   \code{ssm_measures()} that included \code{contrast = "test"}.
#' @param axislabel Optional. A string to label the y-axis (default =
#'   "Difference").
#' @param xy A logical determining whether the X-Value and Y-Value parameters
#'   should be included in the plot (default = TRUE).
#' @param color Optional. A string corresponding to the color of the point range
#'   (default = "red").
#' @param linesize Optional. A positive number corresponding to the size of the
#'   point range elements in mm (default = 1.5).
#' @param fontsize Optional. A positive number corresponding to the size of the
#'   axis labels, numbers, and facet headings in pt (default = 12).
#' @return A ggplot variable containing difference point-ranges faceted by SSM
#'   parameter. An interval that does not contain the value of zero has p<.05.
#' @family ssm functions
#' @family visualization functions
#' @export

ssm_plot_contrast <- function(.ssm_object, axislabel = "Difference", 
  xy = TRUE, color = "red", linesize = 1.5, fontsize = 12) {
  
  param_names <- c(
    e = "Elevation",
    x = "X-Value",
    y = "Y-Value",
    a = "Amplitude",
    d = "Displacement"
  )
  
  res <- .ssm_object$contrasts
  
  if (xy == FALSE) {
    res <- dplyr::select(res,
      -c(tidyselect::starts_with("x"), tidyselect::starts_with("y")))
    param_names <- param_names[-c(2, 3)]
  }

  # TODO: Check that these ifelse() statements are correct
  
  res <- dplyr::mutate(res,
      d_uci = ifelse(d_uci < d_lci && d_uci < 180, circ_dist(d_uci), d_uci),
      d_lci = ifelse(d_lci > d_uci && d_lci > 180, circ_dist(d_lci), d_lci)
    ) %>%
    tidyr::gather(key, value, -label, -fit) %>%
    tidyr::extract(key, c("Parameter", "Type"), "(.)_(...)") %>%
    tidyr::spread(Type, value) %>%
    dplyr::rename(Difference = est, Contrast = label) %>% 
    dplyr::mutate(
      Parameter = factor(Parameter, levels = c("e", "x", "y", "a", "d"))
    )
  p <- ggplot2::ggplot(res) +
    ggplot2::theme_bw(base_size = fontsize) +
    ggplot2::theme(
      legend.position = "top",
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank()
    ) +
    ggplot2::geom_hline(yintercept = 0, size = linesize, color = "darkgray") +
    ggplot2::geom_pointrange(
      aes(
        x = Contrast, y = Difference, ymin = lci, ymax = uci
      ),
      size = linesize, color = color
    ) +
    ggplot2::labs(y = axislabel) +
    ggplot2::facet_wrap(~ Parameter,
      nrow = 1, scales = "free",
      labeller = ggplot2::as_labeller(param_names)
    )

  p
}

# TODO: Finish function
ssm_plot_curve <- function(.ssm_object) {
  scores <- .ssm_object$scores
  results <- .ssm_object$results
  df <- 
  ggplot2::ggplot(scores, aes(x = Scale, y = Score, color = ))
}

#' Create HTML table from SSM results or contrasts
#'
#' Take in the results of an SSM analysis and return an HTML table with the
#' desired formatting.
#'
#' @param .ssm_object The output of \code{ssm_profiles()} or
#'   \code{ssm_measures()}
#' @param filename A string determining the filename to which the table should
#'   be saved. If set to NULL, the table will be displayed but not saved
#'   (default = NULL).
#' @param type A string determining whether the table should contain the normal
#'   SSM "results" or SSM "contrast" results (default = "results").
#' @param caption A string to be displayed above the table (default = "").
#' @param xy A logical indicating whether the x-value and y-value parameters
#'   should be included in the table as columns (default = TRUE).
#' @return An HTML table containing SSM results or contrasts.
#' @family ssm functions
#' @family table functions
#' @export

ssm_table <- function(.ssm_object, filename = NULL, type = "results",
                      caption = dcaption(.ssm_object, type), xy = TRUE) {
  
  if (type == "results") {
    df <- .ssm_object$results
    N = .ssm_object$details$n
  } else if (type == "contrasts") {
    df <- .ssm_object$contrasts
    N = .ssm_object$details$n[[1]] # TODO: Replace w/pairwise deletion
  } else {
    return(NA)
  }

  type_sym <- rlang::sym(.ssm_object$type)
  
  df <- dplyr::transmute(df,
    (!!type_sym) := label,
    N = N,
    Elevation = sprintf("%.2f [%.2f, %.2f]", e_est, e_lci, e_uci),
    `X-Value` = sprintf("%.2f [%.2f, %.2f]", x_est, x_lci, x_uci),
    `Y-Value` = sprintf("%.2f [%.2f, %.2f]", y_est, y_lci, y_uci),
    Amplitude = sprintf("%.2f [%.2f, %.2f]", a_est, a_lci, a_uci),
    Displacement = sprintf("%.1f [%.1f, %.1f]", d_est, d_lci, d_uci),
    Fit = sprintf("%.3f", fit)
  )
  
  if (xy == TRUE) {
    align <- "llllll"
  } else {
    df <- dplyr::select(df, -c(`X-Value`, `Y-Value`))
    align <- "llll"
  }
  
  t <- htmlTable::htmlTable(df,
    caption = caption,
    align = align,
    align.header = align,
    rnames = FALSE,
    css.cell = "padding-right: 1em; min-width: 3em; white-space: nowrap;"
  )
  
  if (is.null(filename) == TRUE) {
    print(t, type = "html", useViewer = TRUE)
  } else {
    sink(filename)
    print(t, type = "html", useViewer = FALSE)
    sink()
  }
  
  t
}

# Build the default caption for the ssm_table function
dcaption <- function(.ssm_object, type) {
  if (.ssm_object$type == "Profile") {
    basetype = "Mean"
  } else if (.ssm_object$type == "Measure") {
    basetype = "Correlation"
  }
  if (type == "results") {
    sprintf(
      "%s-based Structural Summary Statistics with %s Confidence Intervals",
      basetype, str_percent(.ssm_object$details$interval)
    )
  } else if (type == "contrasts") {
    sprintf(
      "%s-based Structural Summary Contrasts with %s Confidence Intervals", 
      basetype, str_percent(.ssm_object$details$interval)
    )
  }
}