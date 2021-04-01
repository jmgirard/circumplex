#' Create a figure from SSM results
#'
#' Take in the results of an SSM analysis function and create figure from it.
#'
#' @param .ssm_object Required. The results output of \code{\link{ssm_analyze}}.
#' @param fontsize Optional. A single positive number indicating the font size
#'   of text in the figure, in points (default = 12).
#' @param ... Additional arguments to pass on to the plotting function.
#' @return A ggplot2 object representing the figure
#' @seealso ggsave Function for saving plots to image files.
#' @family ssm functions
#' @family visualization functions
#' @export
#' @examples
#' \donttest{
#' # Load example data
#' data("jz2017")
#' 
#' # Plot profile results
#' res <- ssm_analyze(jz2017,
#'   scales = PA:NO, angles = octants(),
#'   measures = c(NARPD, ASPD)
#' )
#' p <- ssm_plot(res)
#' 
#' # Plot contrast results
#' res <- ssm_analyze(jz2017,
#'   scales = PA:NO, angles = octants(),
#'   measures = c(NARPD, ASPD), contrast = "test"
#' )
#' p <- ssm_plot(res)
#' }
#' 
ssm_plot <- function(.ssm_object, fontsize = 12, ...) {

  # Check for valid input arguments
  assert_that(is_provided(.ssm_object))
  assert_that(is.number(fontsize), fontsize > 0)

  # Forward to the appropriate subfunction
  if (.ssm_object$details$contrast == "test") {
    ssm_plot_contrast(.ssm_object, fontsize = fontsize, ...)
  } else {
    ssm_plot_circle(.ssm_object, fontsize = fontsize, ...)
  }

  # TODO: Add more explanation of the possible arguments in documentation.
}

#' Create a Circular Plot of SSM Results
#'
#' Take in the results of a Structural Summary Method analysis and plot the
#' point and interval estimate for each row (e.g., group or measure) in a
#' circular space quantified by displacement and amplitude.
#'
#' @param .ssm_object The output of \code{ssm_profiles()} or
#'   \code{ssm_measures()}.
#' @param amax A positive real number corresponding to the radius of the circle.
#'   It is used to scale the amplitude values and will determine which amplitude
#'   labels are drawn.
#' @param fontsize A positive real number corresponding to the size (in pt) of
#'   the text labels (default = 12).
#' @param lowfit A logical determining whether profiles with low model fit
#'   (<.70) should be plotted, with dashed borders (default = TRUE).
#' @param repel An experimental argument for plotting text labels instead of
#'   colors.
#' @param angle_labels A character vector specifying text labels to plot around
#'   the circle for each scale. Can also specify NULL to default to numerical
#'   angle labels or a vector of empty strings ("") to hide the labels. If not
#'   NULL, must have the same length and ordering as the \code{angles} argument
#'   to \code{ssm_analyze()}. (default = NULL)
#' @return A ggplot variable containing a completed circular plot.

ssm_plot_circle <- function(.ssm_object, amax = NULL, fontsize = 12,
                            lowfit = TRUE, repel = FALSE,
                            angle_labels = NULL) {
  df <- .ssm_object$results
  
  assert_that(
    is.null(angle_labels) || 
      rlang::is_character(angle_labels, n = length(.ssm_object$details$angles))
  )
  
  angles <- as.integer(round(.ssm_object$details$angles))
  

  assert_that(is.null(amax) || is.number(amax))

  if (is.null(amax)) {
    amax <- pretty_max(.ssm_object$results$a_uci)
  }

  # Convert results to numbers usable by ggplot and ggforce
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

  # Remove profiles with low model fit (unless overrided)
  n <- nrow(df_plot)
  if (lowfit == FALSE) {
    df_plot <- df_plot %>%
      dplyr::filter(fit_est >= .70)
    n2 <- nrow(df_plot)
    if (n2 < 1) {
      stop("After removing profiles, there were none left to plot.")
    }
  }
  df_plot <- df_plot %>%
    dplyr::mutate(lnty = dplyr::if_else(fit_est >= .70, "solid", "dashed"))

  p <- 
    circle_base(
      angles = angles, 
      amax = amax, 
      fontsize = fontsize, 
      labels = angle_labels
    ) +
    ggplot2::scale_color_hue() +
    ggplot2::scale_fill_hue()

  p <- p +
    ggforce::geom_arc_bar(
      data = df_plot,
      ggplot2::aes(
        x0 = 0, y0 = 0, r0 = a_lci, r = a_uci, start = d_lci, end = d_uci,
        fill = label, color = label, linetype = lnty
      ),
      alpha = 0.4,
      size = 1
    ) +
    ggplot2::geom_point(
      data = df_plot,
      ggplot2::aes(x = x_est, y = y_est, color = label),
      shape = 16,
      size = 3
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(.ssm_object$details$results_type),
      fill = ggplot2::guide_legend(.ssm_object$details$results_type)
    ) +
    ggplot2::theme(
      legend.text = ggplot2::element_text(size = fontsize)
    ) +
    ggplot2::scale_linetype_identity()

  if (repel == TRUE) {
    p <- p + 
      ggrepel::geom_label_repel(
        data = df_plot,
        ggplot2::aes(x = x_est, y = y_est, label = label),
        nudge_x = -25 - df_plot$x_est,
        direction = "y",
        hjust = 1,
        size = fontsize / 2.8346438836889
      ) + 
      ggplot2::theme(legend.position = "none")
  }
  
  p
}

#' Create a Difference Plot of SSM Contrast Results
#'
#' Take in the results of a Structural Summary Method analysis with pairwise
#' contrasts and plot the point and interval estimates for each parameter's
#' contrast (e.g., between groups or measures).
#'
#' @param .ssm_object Required. The results output of \code{ssm_analyze}.
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

ssm_plot_contrast <- function(.ssm_object, axislabel = "Difference",
                              xy = TRUE, color = "red", linesize = 1.25, fontsize = 12) {
  plabs <- c(
    e = expression(paste(Delta, " Elevation")),
    x = expression(paste(Delta, " X-Value")),
    y = expression(paste(Delta, " Y-Value")),
    a = expression(paste(Delta, " Amplitude")),
    d = expression(paste(Delta, " Displacement"))
  )
  
  pvals <- c("e", "x", "y", "a", "d")

  res <- .ssm_object$results

  if (xy == FALSE) {
    res <- dplyr::select(
      res,
      -c(x_est, x_lci, x_uci, y_est, y_lci, y_uci)
    )
    plabs <- plabs[-c(2, 3)]
    pvals <- pvals[-c(2, 3)]
  }

  # TODO: Check that these ifelse() statements are correct

  res <- 
    res %>% 
    dplyr::mutate(
      d_est = unclass(d_est),
      d_uci = unclass(ifelse(d_uci < d_lci && d_uci < 180, circ_dist(d_uci), d_uci)),
      d_lci = unclass(ifelse(d_lci > d_uci && d_lci > 180, circ_dist(d_lci), d_lci))
    ) %>%
    dplyr::select(-fit_est) %>% 
    tidyr::pivot_longer(cols = e_est:d_uci, names_to = "key", values_to = "value") %>% 
    tidyr::extract(col = key, into = c("Parameter", "Type"), "(.)_(...)") %>% 
    tidyr::pivot_wider(names_from = Type, values_from = value) %>% 
    dplyr::rename(Difference = est, Contrast = label) %>%
    dplyr::mutate(Parameter = factor(Parameter, levels = pvals, labels = plabs))
  
  p <- 
    res %>% 
    ggplot2::ggplot() +
    ggplot2::theme_bw(base_size = fontsize) +
    ggplot2::theme(
      legend.position = "top",
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank()
    ) +
    ggplot2::geom_hline(yintercept = 0, size = linesize, color = "darkgray") +
    ggplot2::geom_point(
      ggplot2::aes(x = Contrast, y = Difference),
      size = linesize * 3, color = color
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(x = Contrast, ymin = lci, ymax = uci),
      size = linesize, color = color, width = 0.1
    ) +
    ggplot2::labs(y = axislabel) +
    ggplot2::facet_wrap(~Parameter,
      nrow = 1, scales = "free",
      labeller = ggplot2::label_parsed
    )

  p
}

# Create an Empty Circular Plot
circle_base <- function(angles, labels = NULL,
                        amax = 0.5, fontsize = 12) {
  
  if (is.null(labels)) labels <- sprintf("%d\u00B0", angles)
  
  ggplot2::ggplot() +
    # Require plot to be square and remove default styling
    ggplot2::coord_fixed() +
    ggplot2::theme_void(base_size = fontsize) +
    # Expand the axes multiplicatively to fit labels
    ggplot2::scale_x_continuous(expand = c(0.25, 0)) +
    ggplot2::scale_y_continuous(expand = c(0.10, 0)) +
    # Draw segments corresponding to displacement scale
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        y = 0,
        xend = 5 * cos(angles * pi / 180),
        yend = 5 * sin(angles * pi / 180)
      ),
      color = "gray60",
      size = 0.5
    ) +
    # Draw circles corresponding to amplitude scale
    ggforce::geom_circle(
      ggplot2::aes(x0 = 0, y0 = 0, r = 1:4),
      color = "gray60",
      size = 0.5
    ) +
    ggforce::geom_circle(
      ggplot2::aes(x0 = 0, y0 = 0, r = 5),
      color = "gray50",
      size = 1.5
    ) +
    # Draw labels for amplitude scale
    ggplot2::geom_label(
      ggplot2::aes(
        x = c(2, 4),
        y = 0,
        label = sprintf(
          "%.2f",
          seq(from = 0, to = amax, length.out = 6)[c(3, 5)]
        )
      ),
      color = "gray20",
      label.size = NA,
      size = fontsize / 2.8346438836889
    ) +
    # Draw labels for displacement scale
    ggplot2::geom_label(
      ggplot2::aes(
        x = 5.1 * cos(angles * pi / 180),
        y = 5.1 * sin(angles * pi / 180),
        label = labels
      ),
      color = "gray20",
      label.size = NA,
      hjust = "outward",
      vjust = "outward",
      size = fontsize / 2.8346438836889
    )
}

#' Create HTML table from SSM results or contrasts
#'
#' Take in the results of an SSM analysis and return an HTML table with the
#' desired formatting.
#'
#' @param .ssm_object The output of \code{ssm_profiles()} or
#'   \code{ssm_measures()}
#' @param caption A string to be displayed above the table (default = NULL).
#' @param xy A logical indicating whether the x-value and y-value parameters
#'   should be included in the table as columns (default = TRUE).
#' @param render A logical indicating whether the table should be displayed in
#'   the RStudio viewer or web browser (default = TRUE).
#' @return A tibble containing the information for the HTML table. As a
#'  side-effect, may also output the HTML table to the web viewer.
#' @family ssm functions
#' @family table functions
#' @export
#' @examples
#' \donttest{
#' # Load example data
#' data("jz2017")
#' 
#' # Create table of profile results
#' res <- ssm_analyze(jz2017,
#'   scales = PA:NO, angles = octants(),
#'   measures = c(NARPD, ASPD)
#' )
#' ssm_table(res)
#' 
#' # Create table of contrast results
#' res <- ssm_analyze(jz2017,
#'   scales = PA:NO, angles = octants(),
#'   measures = c(NARPD, ASPD), contrast = "test"
#' )
#' ssm_table(res)
#' }
#' 
ssm_table <- function(.ssm_object, caption = NULL, xy = TRUE, render = TRUE) {
  assert_that(is_provided(.ssm_object))
  assert_that(is.null(caption) || is.string(caption))
  assert_that(is.flag(xy), is.flag(render))

  df <- .ssm_object$results

  # Create default caption
  if (is.null(caption)) {
    caption <- dcaption(.ssm_object)
  }

  # Format output data
  df <- dplyr::transmute(df,
    Label = label,
    Elevation = sprintf("%.2f (%.2f, %.2f)", e_est, e_lci, e_uci),
    `X-Value` = sprintf("%.2f (%.2f, %.2f)", x_est, x_lci, x_uci),
    `Y-Value` = sprintf("%.2f (%.2f, %.2f)", y_est, y_lci, y_uci),
    Amplitude = sprintf("%.2f (%.2f, %.2f)", a_est, a_lci, a_uci),
    Displacement = sprintf("%.1f (%.1f, %.1f)", d_est, d_lci, d_uci),
    Fit = sprintf("%.3f", fit_est)
  )

  # Rename first column
  colnames(df)[[1]] <- .ssm_object$details$results_type

  # Add delta symbol to column names if results are contrasts
  if (.ssm_object$details$contrast == "test") {
    colnames(df)[[2]] <- "&Delta; Elevation"
    colnames(df)[[3]] <- "&Delta; X-Value"
    colnames(df)[[4]] <- "&Delta; Y-Value"
    colnames(df)[[5]] <- "&Delta; Amplitude"
    colnames(df)[[6]] <- "&Delta; Displacement"
    colnames(df)[[7]] <- "&Delta; Fit"
  }

  # Drop the x and y columns if requested
  if (xy == FALSE) df <- df[, -c(3, 4)]

  # Format and render HTML table if requested
  if (render == TRUE) {
    html_render(df, caption)
  }

  df
}

# Build the default caption for the ssm_table function
dcaption <- function(.ssm_object) {
  if (.ssm_object$details$results_type == "Profile") {
    sprintf(
      "%s-based Structural Summary Statistics with %s CIs",
      .ssm_object$details$score_type,
      str_percent(.ssm_object$details$interval)
    )
  } else if (.ssm_object$details$results_type == "Contrast") {
    sprintf(
      "%s-based Structural Summary Statistic Contrasts with %s CIs",
      .ssm_object$details$score_type,
      str_percent(.ssm_object$details$interval)
    )
  }
}

#' Combine SSM tables
#'
#' Combine SSM tables by appending them as rows.
#'
#' @param .ssm_table A data frame from the \code{ssm_table()} function to be the
#'   first row(s) of the combined table.
#' @param ... One or more additional data frames from the \code{ssm_table()}
#'   function to be appended to \code{.ssm_table} in the order of input.
#' @param caption A string to be displayed above the table if rendered.
#' @param render A logical indicating whether the table should be displayed in
#'   the RStudio viewer or web browser (default = TRUE).
#' @return A tibble containing the information for the HTML table. As a
#'  side-effect, may also output the HTML table to the web viewer.
#' @family ssm functions
#' @family table functions
#' @export
#' @examples
#' data("jz2017")
#' res1 <- ssm_analyze(jz2017, PA:NO, octants())
#' res2 <- ssm_analyze(jz2017, PA:NO, octants(), grouping = Gender)
#' tab1 <- ssm_table(res1, render = FALSE)
#' tab2 <- ssm_table(res2, render = FALSE)
#' ssm_append(tab1, tab2)
ssm_append <- function(.ssm_table, ..., caption = NULL, render = TRUE) {

  # TODO: Add more assertions
  assert_that(is.flag(render))

  # Bind the tibbles together by row
  df <- dplyr::bind_rows(.ssm_table, ...)

  # Format and render HTML table if requested
  if (render == TRUE) {
    html_render(df, caption)
  }

  df
}

#' Format and render data frame as HTML table
#'
#' Format a data frame as an HTML table and render it to the web viewer.
#'
#' @param df A data frame to be rendered as an HTML table.
#' @param caption A string to be displayed above the table.
#' @param align A string indicating the alignment of the cells (default = "l").
#' @param ... Other arguments to pass to \code{htmlTable}.
#' @return HTML syntax for the \code{df} table.
#' @family table functions
#' @export
html_render <- function(df, caption = NULL, align = "l", ...) {

  # TODO: Add assertions

  t <- htmlTable::htmlTable(df,
    caption = caption,
    align = align,
    align.header = align,
    rnames = FALSE,
    css.cell = "padding-right: 1em; min-width: 3em; white-space: nowrap;",
    ...
  )
  print(t, type = "html")
}
