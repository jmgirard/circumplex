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
#' res <- ssm_analyze(
#'   jz2017,
#'   scales = 2:9, 
#'   measures = c("NARPD", "ASPD")
#' )
#' p <- ssm_plot(res)
#' 
#' # Plot contrast results
#' res <- ssm_analyze(
#'   jz2017,
#'   scales = 2:9, 
#'   measures = c("NARPD", "ASPD"), 
#'   contrast = TRUE
#' )
#' p <- ssm_plot(res)
#' }
#' 
ssm_plot <- function(.ssm_object, fontsize = 12, ...) {

  # Check for valid input arguments
  stopifnot(class(.ssm_object) == "circumplex_ssm")
  stopifnot(is.numeric(fontsize) && length(fontsize) == 1 && fontsize > 0)

  # Forward to the appropriate subfunction
  if (.ssm_object$details$contrast) {
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
#' @param legend_font_size A positive real number corresponding to the size (in
#'   pt) of the text labels in the legend (default = 12).
#' @param scale_font_size A positive real number corresponding to the size (in
#'   pt) of the text labels for the amplitude and displacement scales (default =
#'   12).
#' @param lowfit A logical determining whether profiles with low model fit
#'   (<.70) should be plotted, with dashed borders (default = TRUE).
#' @param repel An experimental argument for plotting text labels instead of
#'   colors.
#' @param angle_labels A character vector specifying text labels to plot around
#'   the circle for each scale. Can also specify NULL to default to numerical
#'   angle labels or a vector of empty strings ("") to hide the labels. If not
#'   NULL, must have the same length and ordering as the \code{angles} argument
#'   to \code{ssm_analyze()}. (default = NULL)
#' @param legend.box.spacing A double corresponding to the distance (in inches)
#'   to add between the data plot and the legend (default = 0).
#' @param palette A string corresponding to the palette to be used from
#'   ColorBrewer for the color and fill aesthetics. If set to NULL, all points
#'   will appear blue and no legend will be there (useful for showing the
#'   coverage of a high number of variables).
#' @param ... Currently ignored.
#' @return A ggplot variable containing a completed circular plot.

ssm_plot_circle <- function(.ssm_object, amax = NULL, 
                            legend_font_size = 12,
                            scale_font_size = 12,
                            lowfit = TRUE, repel = FALSE,
                            angle_labels = NULL,
                            legend.box.spacing = 0,
                            palette = "Set2",
                            ...) {
  
  df <- .ssm_object$results
  
  stopifnot(
    is.null(angle_labels) || 
      (is.character(angle_labels) && 
         length(angle_labels) == length(.ssm_object$details$angles))
  )

  
  angles <- as.integer(round(.ssm_object$details$angles))
  

  stopifnot(is.null(amax) || is.number(amax))

  if (is.null(amax)) {
    amax <- pretty_max(.ssm_object$results$a_uci)
  }

  # Convert results to numbers usable by ggplot and ggforce
  df_plot <- df
  df_plot[["d_uci"]] <- ifelse(
    test = df_plot[["d_uci"]] < df_plot[["d_lci"]], 
    yes = ggrad(df_plot[["d_uci"]] + 360),
    no = ggrad(df_plot[["d_uci"]])
  )
  df_plot[["d_lci"]] <- ggrad(df_plot[["d_lci"]])
  df_plot[c("a_lci", "a_uci", "x_est", "y_est")] <- sapply(
    df_plot[c("a_lci", "a_uci", "x_est", "y_est")],
    function(x) x * 10 / (2 * amax)
  )
  df_plot$label <- factor(
    df_plot$label, 
    levels = unique(as.character(df_plot$label))
  )
  
  # Remove profiles with low model fit (unless over-rided)
  n <- nrow(df_plot)
  if (lowfit == FALSE) {
    df_plot <- df_plot[df_plot$fit_est >= .70, ]
    if (nrow(df_plot) < 1) {
      stop("After removing profiles with low fit, there were none left to plot.")
    }
  }
  
  df_plot[["lnty"]] <- ifelse(df_plot$fit_est >= .70, "solid", "dashed")

  p <- 
    circle_base(
      angles = angles, 
      amax = amax, 
      fontsize = scale_font_size, 
      labels = angle_labels
    ) +
    ggplot2::scale_color_brewer(palette = palette) +
    ggplot2::scale_fill_brewer(palette = palette)

  if (is.null(palette)) {
    p <- p +
      ggforce::geom_arc_bar(
        data = df_plot,
        ggplot2::aes(
          x0 = 0, y0 = 0, 
          r0 = a_lci, r = a_uci, start = d_lci, end = d_uci,
          linetype = lnty
        ),
        fill = "cornflowerblue", 
        color = "cornflowerblue", 
        alpha = 0.4,
        linewidth = 1
      ) +
      ggplot2::geom_point(
        data = df_plot,
        ggplot2::aes(x = x_est, y = y_est),
        shape = 21,
        size = 3,
        color = "black",
        fill = "cornflowerblue"
      ) +
      ggplot2::scale_linetype_identity() +
      ggplot2::theme(legend.position = "none")
  } else {
    p <- p +
      ggforce::geom_arc_bar(
        data = df_plot,
        ggplot2::aes(
          x0 = 0, y0 = 0, 
          r0 = a_lci, r = a_uci, start = d_lci, end = d_uci,
          fill = label, color = label, linetype = lnty
        ),
        alpha = 0.4,
        linewidth = 1
      ) +
      ggplot2::geom_point(
        data = df_plot,
        ggplot2::aes(x = x_est, y = y_est, color = label, fill = label),
        shape = 21,
        size = 3,
        color = "black"
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend("Profile"),
        fill = ggplot2::guide_legend("Profile")
      ) +
      ggplot2::theme(
        legend.text = ggplot2::element_text(size = legend_font_size),
        legend.box.spacing = ggplot2::unit(legend.box.spacing, "in")
      ) +
      ggplot2::scale_linetype_identity()
  }
  
  if (repel == TRUE) {
    require("ggrepel")
    p <- p + 
      ggrepel::geom_label_repel(
        data = df_plot,
        ggplot2::aes(x = x_est, y = y_est, label = label),
        nudge_x = -8 - df_plot$x_est,
        direction = "y",
        hjust = 1,
        size = legend_font_size / 2.8346438836889
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
#' @param drop_xy A logical determining whether the X-Value and Y-Value
#'   parameters should be removed from the plot (default = FALSE).
#' @param sig_color Optional. A string corresponding to the color to use to
#'   denote significant contrasts (default = "#fc8d62").
#' @param ns_color Optional. A string corresponding to the color to use to
#'   denote non-significant contrasts (default = "white").
#' @param linesize Optional. A positive number corresponding to the size of the
#'   point range elements in mm (default = 1.5).
#' @param fontsize Optional. A positive number corresponding to the size of the
#'   axis labels, numbers, and facet headings in pt (default = 12).
#' @param ... Additional arguments will be ignored.
#' @return A ggplot variable containing difference point-ranges faceted by SSM
#'   parameter. An interval that does not contain the value of zero has p<.05.

ssm_plot_contrast <- function(.ssm_object, drop_xy = FALSE, 
                              sig_color = "#fc8d62", ns_color = "white",
                              linesize = 1.25, fontsize = 12, ...) {
  
  stopifnot(.ssm_object$details$contrast)
  
  # Prepare all estimates
  plabs <- c(
    e = expression(paste(Delta, " Elevation")),
    x = expression(paste(Delta, " X Value")),
    y = expression(paste(Delta, " Y Value")),
    a = expression(paste(Delta, " Amplitude")),
    d = expression(paste(Delta, " Displacement"))
  )
  pvals <- c("e", "x", "y", "a", "d")
  res <- .ssm_object$results[nrow(.ssm_object$results), ]
  
  plot_df <- 
    data.frame(
      Parameter = factor(pvals, levels = pvals, labels = plabs),
      Difference = c(res$e_est, res$x_est, res$y_est, res$a_est, res$d_est),
      lci = c(res$e_lci, res$x_lci, res$y_lci, res$a_lci, res$d_lci),
      uci = c(res$e_uci, res$x_uci, res$y_uci, res$a_uci, res$d_uci)
    )
  
  plot_df$sig <- sign(plot_df$lci) == sign(plot_df$uci)
  
  # Drop x and y estimates if requested
  if (drop_xy) {
    plot_df <- plot_df[-c(2, 3), ]
  }

  p <- 
    ggplot2::ggplot(plot_df) +
    ggplot2::theme_bw(base_size = fontsize) +
    ggplot2::theme(
      legend.position = "top",
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_line(linetype = "dashed"),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::geom_hline(
      yintercept = 0, 
      linewidth = linesize, 
      color = "darkgray"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(x = "1", ymin = lci, ymax = uci),
      linewidth = linesize, width = 0.15
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = "1", y = Difference, fill = sig),
      size = linesize * 3,
      stroke = linesize,
      shape = 21
    ) +
    ggplot2::scale_fill_manual(
      "Significant",
      values = c("TRUE" = sig_color, "FALSE" = ns_color)
    ) +
    ggplot2::labs(y = paste0("Contrast (", res$label, ")")) +
    ggplot2::facet_wrap(~Parameter,
      nrow = 1, scales = "free",
      labeller = ggplot2::label_parsed
    )

  p
}

# Create an Empty Circular Plot
circle_base <- function(angles, labels = NULL, amin = 0,
                        amax = 0.5, fontsize = 12) {
  
  if (is.null(labels)) labels <- paste0(angles, "\u00B0")
  
  ggplot2::ggplot() +
    # Require plot to be square and remove default styling
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::theme_void(base_size = fontsize) +
    # Expand the axes multiplicatively to fit labels
    ggplot2::scale_x_continuous(expand = c(0.25, 0)) +
    ggplot2::scale_y_continuous(expand = c(0.10, 0)) +
    # Draw lowest circle
    ggforce::geom_circle(
      ggplot2::aes(x0 = 0, y0 = 0, r = 5),
      color = "gray50",
      fill = "white",
      linewidth = 1.5
    ) +
    # Draw segments corresponding to displacement scale
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        y = 0,
        xend = 5 * cos(angles * pi / 180),
        yend = 5 * sin(angles * pi / 180)
      ),
      color = "gray60",
      linewidth = 0.5
    ) +
    # Draw circles corresponding to amplitude scale
    ggforce::geom_circle(
      ggplot2::aes(x0 = 0, y0 = 0, r = 1:4),
      color = "gray60",
      linewidth = 0.5
    ) +
    # Draw labels for amplitude scale
    ggplot2::geom_label(
      ggplot2::aes(
        x = c(2, 4),
        y = 0,
        label = sprintf(
          "%.2f",
          seq(from = amin, to = amax, length.out = 6)[c(3, 5)]
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
      fill = "transparent",
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
#' res <- ssm_analyze(
#'   jz2017,
#'   scales = 2:9,
#'   measures = c("NARPD", "ASPD")
#' )
#' ssm_table(res)
#' 
#' # Create table of contrast results
#' res <- ssm_analyze(
#'   jz2017,
#'   scales = 2:9,
#'   measures = c("NARPD", "ASPD"), 
#'   contrast = TRUE
#' )
#' ssm_table(res)
#' }
#' 
ssm_table <- function(.ssm_object, caption = NULL, 
                      drop_xy = FALSE, render = TRUE) {
  
  stopifnot(class(.ssm_object) == "circumplex_ssm")
  stopifnot(is.null(caption) || (is.character(caption) && length(caption) == 1))
  stopifnot(is.logical(drop_xy) && length(drop_xy) == 1)
  stopifnot(is.logical(render) && length(render) == 1)

  df <- .ssm_object$results

  # Create default caption
  if (is.null(caption)) {
    caption <- dcaption(.ssm_object)
  }

  # Format output data
  table_df <- 
    data.frame(
      Profile = df$label,
      Elevation = sprintf("%.2f (%.2f, %.2f)", df$e_est, df$e_lci, df$e_uci),
      `X Value` = sprintf("%.2f (%.2f, %.2f)", df$x_est, df$x_lci, df$x_uci),
      `Y Value` = sprintf("%.2f (%.2f, %.2f)", df$y_est, df$y_lci, df$y_uci),
      Amplitude = sprintf("%.2f (%.2f, %.2f)", df$a_est, df$a_lci, df$a_uci),
      Displacement = sprintf("%.1f (%.1f, %.1f)", df$d_est, df$d_lci, df$d_uci),
      Fit = sprintf("%.3f", df$fit_est)
    )

  # Rename first column
  colnames(table_df)[[1]] <- ifelse(
    test = .ssm_object$details$contrast, 
    yes = "Contrast", 
    no = "Profile"
  )

  # Drop the x and y columns if requested
  if (drop_xy) {
    table_df <- table_df[, -c(3, 4)]
  }
  
  # Format and render HTML table if requested
  if (render == TRUE) {
    html_render(table_df, caption)
  }

  table_df
}

# Build the default caption for the ssm_table function
dcaption <- function(.ssm_object) {
  if (.ssm_object$details$contrast) {
    sprintf(
      "%s-based Structural Summary Statistic Contrasts with %s CIs",
      .ssm_object$details$score_type,
      str_percent(.ssm_object$details$interval)
    )
  } else {
    sprintf(
      "%s-based Structural Summary Statistics with %s CIs",
      .ssm_object$details$score_type,
      str_percent(.ssm_object$details$interval)
    )
  }
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

  stopifnot(is_null_or_char(caption, n = 1))
  stopifnot(align %in% c("l", "c", "r"))

  t <- htmlTable::htmlTable(
    df,
    caption = caption,
    align = align,
    align.header = align,
    rnames = FALSE,
    css.cell = "padding-right: 1em; min-width: 3em; white-space: nowrap;",
    ...
  )
  print(t, type = "html")
}

# S3 Generic

#' Create a spider/radar plot of circumplex scores
#'
#' Create a spider/radar plot of circumplex scores, either from a data frame
#' containing scale scores or the result of \code{ssm_analyze()}.
#'
#' @param x A dataframe or ssm result object.
#' @param amin An optional number to set as the minimum amplitude (center of
#'   circle). If set to `NULL`, will try to detect a reasonable value.
#' @param amax An optional number to set as the maximum amplitude (outer ring of
#'   circle). If set set to `NULL`, will try to detect a reasonable value.
#' @param angle_labels An optional character vector to display outside the
#'   circle at each angle. Must be the same length as the number of angles.
#' @param linewidth An optional width for the lines of the profile polygons.
#' @param pointsize An optional size for the points at the scale scores.
#' @param ... Additional arguments for the S3 methods
#' @return A spider/radar plot object
#' @export
ssm_plot_scores <- function(x, 
                            amin = NULL, amax = NULL, angle_labels = NULL, 
                            linewidth = 1, pointsize = 3, ...) {
  UseMethod("ssm_plot_scores")
}

#' @method ssm_plot_scores circumplex_ssm
#' @export
ssm_plot_scores.circumplex_ssm <- function(x,
                                           amin = NULL, 
                                           amax = NULL,
                                           angle_labels = NULL,
                                           linewidth = 1,
                                           pointsize = 3,
                                           ...) {
  
  # Get scores from SSM object
  scores <- x$scores
  # Reshape scores for plotting
  scores_long <- tidyr::pivot_longer(
    scores, 
    cols = dplyr::where(is.numeric),
    names_to = "Scale",
    values_to = "Score"
  )
  # Get angles from SSM object
  angles <- x$details$angles
  if (is.null(amin)) amin <- pretty_min(scores_long$Score)
  if (is.null(amax)) amax <- pretty_max(scores_long$Score)
  scores_long$Angle <- rep(angles, times = nrow(scores_long) / length(angles))
  scores_long$Radian <- as_radian(as_degree(scores_long$Angle))
  scores_long$pr <- rescale(
    scores_long$Score, 
    to = c(0, 5), 
    from = c(amin, amax)
  )
  scores_long$px <- scores_long$pr * cos(scores_long$Radian)
  scores_long$py <- scores_long$pr * sin(scores_long$Radian)
  
  p <- circle_base(
    angles = angles, 
    amin = amin,
    amax = amax,
    labels = angle_labels
  )
  
  p +
    ggplot2::geom_polygon(
      data = scores_long,
      mapping = ggplot2::aes(x = px, y = py, color = label, linetype = label),
      fill = NA,
      linewidth = linewidth
    ) +
    ggplot2::geom_point(
      data = scores_long,
      mapping = ggplot2::aes(x = px, y = py, color = label),
      size = pointsize
    )
  
}

#' @method ssm_plot_scores data.frame
#' @export
ssm_plot_scores.data.frame <- function(x, 
                                       amin = NULL, 
                                       amax = NULL,
                                       angle_labels = NULL,
                                       linewidth = 1,
                                       pointsize = 3,
                                       scales, 
                                       angles = octants(),
                                       group = NULL,
                                       ...) {
  
  if (!is_provided(group)) {
    x$Group <- as.character(1:nrow(x))
    group <- rlang::quo(Group)
  }
  # Get scores from SSM object
  scores <- dplyr::select(x, {{group}}, {{scales}})
  # Reshape scores for plotting
  scores_long <- tidyr::pivot_longer(
    scores, 
    cols = {{scales}},
    names_to = "Scale",
    values_to = "Score"
  )
  if (is.null(amin)) amin <- pretty_min(scores_long$Score)
  if (is.null(amax)) amax <- pretty_max(scores_long$Score)
  scores_long$Angle <- rep(angles, times = nrow(scores_long) / length(angles))
  scores_long$Radian <- as_radian(as_degree(scores_long$Angle))
  scores_long$pr <- rescale(
    scores_long$Score, 
    to = c(0, 5), 
    from = c(amin, amax)
  )
  scores_long$px <- scores_long$pr * cos(scores_long$Radian)
  scores_long$py <- scores_long$pr * sin(scores_long$Radian)
  
  p <- circle_base(
    angles = angles, 
    amin = amin, 
    amax = amax,
    labels = angle_labels
  )
  
  p +
    ggplot2::geom_polygon(
      data = scores_long,
      mapping = ggplot2::aes(x = px, y = py, color = {{group}}, linetype = {{group}}),
      fill = NA,
      linewidth = linewidth
    ) +
    ggplot2::geom_point(
      data = scores_long,
      mapping = ggplot2::aes(x = px, y = py, color = {{group}}),
      size = pointsize
    )
  
}
