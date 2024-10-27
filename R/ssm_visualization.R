
#' Create a Circular Plot of SSM Results
#'
#' Take in the results of a Structural Summary Method analysis and plot the
#' point and interval estimate for each row (e.g., group or measure) in a
#' circular space quantified by displacement and amplitude.
#'
#' @param ssm_object Required. The output of `ssm_analyze()`.
#' @param amax A positive real number corresponding to the radius of the circle.
#'   It is used to scale the amplitude values and will determine which amplitude
#'   labels are drawn.
#' @param legend_font_size A positive real number corresponding to the size (in
#'   pt) of the text labels in the legend (default = 12).
#' @param scale_font_size A positive real number corresponding to the size (in
#'   pt) of the text labels for the amplitude and displacement scales (default =
#'   12).
#' @param drop_lowfit A logical determining whether profiles with low model fit
#'   (<.70) should be omitted or plotted with dashed borders (default = FALSE).
#' @param repel An experimental argument for plotting text labels instead of
#'   colors.
#' @param angle_labels A character vector specifying text labels to plot around
#'   the circle for each scale. Can also specify NULL to default to numerical
#'   angle labels or a vector of empty strings ("") to hide the labels. If not
#'   NULL, must have the same length and ordering as the `angles` argument to
#'   `ssm_analyze()`. (default = NULL)
#' @param legend.box.spacing A double corresponding to the distance (in inches)
#'   to add between the data plot and the legend (default = 0).
#' @param palette A string corresponding to the palette to be used from
#'   ColorBrewer for the color and fill aesthetics. If set to NULL, all points
#'   will appear blue and no legend will be there (useful for showing the
#'   coverage of a high number of variables).
#' @param ... Currently ignored.
#' @return A ggplot variable containing a completed circular plot.
#' @export
#' @examples
#' \donttest{
#' data("jz2017")
#' res <- ssm_analyze(
#'   jz2017,
#'   scales = 2:9,
#'   measures = c("NARPD", "ASPD")
#' )
#' ssm_plot_circle(res)
#' }
ssm_plot_circle <- function(ssm_object, 
                            amax = NULL, 
                            legend_font_size = 12,
                            scale_font_size = 12,
                            drop_lowfit = FALSE, 
                            repel = FALSE,
                            angle_labels = NULL,
                            legend.box.spacing = 0,
                            palette = "Set2",
                            ...) {
  
  df <- ssm_object$results
  angles <- as.integer(round(ssm_object$details$angles))

  stopifnot(is_null_or_num(amax, n = 1))
  stopifnot(is_null_or_char(angle_labels, n = length(angles)))

  if (is.null(amax)) {
    amax <- pretty_max(ssm_object$results$a_uci)
  }
  
  if (ssm_object$details$contrast) {
    df <- df[1:2, ]
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
  df_plot$Label <- factor(
    df_plot$Label, 
    levels = unique(as.character(df_plot$Label))
  )
  
  # Remove profiles with low model fit (unless overrided)
  n <- nrow(df_plot)
  if (drop_lowfit) {
    df_plot <- df_plot[df_plot$fit_est >= .70, ]
    if (nrow(df_plot) < 1) {
      stop("After removing profiles with low fit, there were none left to plot.")
    }
  }
  
  df_plot[["lnty"]] <- ifelse(df_plot$fit_est >= .70, "solid", "dotted")

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
          fill = Label, color = Label, linetype = lnty
        ),
        alpha = 0.4,
        linewidth = 1
      ) +
      ggplot2::geom_point(
        data = df_plot,
        ggplot2::aes(x = x_est, y = y_est, color = Label, fill = Label),
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
        ggplot2::aes(x = x_est, y = y_est, label = Label),
        nudge_x = -8 - df_plot$x_est,
        direction = "y",
        hjust = 1,
        size = legend_font_size / 2.8346438836889
      ) + 
      ggplot2::theme(legend.position = "none")
  }
  
  p
}

#' Create a Curve Plot of SSM Results
#'
#' Take in the results of a Structural Summary Method analysis and plot the
#' scores by angle and the estimated SSM curve.
#'
#' @param ssm_object Required. The results output of `ssm_analyze()`.
#' @param angle_labels Optional. Either NULL or a character vector that
#'   determines the x-axis labels. If NULL, the labels will be the angle
#'   numbers. If a character vector, must be the same length and in the same
#'   order as the `angles` argument to `ssm_analyze()` (default = NULL).
#' @param base_size Optional. A positive number corresponding to the base font
#'   size in pts (default = 11).
#' @param drop_lowfit Optional. A logical indicating whether to omit profiles with
#'   low fit (<.70) or include them with dashed lines (default = FALSE).
#' @param ... Additional arguments will be ignored.
#' @return A ggplot object depicting the SSM curve(s) of each profile.
#' @export
#' @examples
#' \donttest{
#' data("jz2017")
#' res <- ssm_analyze(
#'   jz2017,
#'   scales = 2:9,
#'   measures = 10:13
#' )
#' ssm_plot_curve(res)
#' ssm_plot_curve(res, angle_lables = PANO())
#' }
ssm_plot_curve <- function(ssm_object, 
                           angle_labels = NULL,
                           base_size = 11,
                           drop_lowfit = FALSE,
                           ...) {
  
  stopifnot(class(ssm_object) == "circumplex_ssm")
  
  results <- ssm_object$results
  scores <- ssm_object$scores
  angles <- ssm_object$details$angles
  
  stopifnot(is_num(base_size, n = 1) && base_size > 0)
  stopifnot(is_null_or_char(angle_labels, n = length(angles)))
  stopifnot(is_flag(drop_lowfit))
  
  if (is.null(angle_labels)) {
    angle_labels <- function(x) sprintf("%.0f\U00B0", x)
    xlabel <- "Angle"
  } else {
    xlabel <- "Scale"
  }
  
  # Drop the contrast row if contrast
  if (ssm_object$details$contrast) {
    results <- results[1:2, ]
    scores <- scores[1:2, ]
  }
  
  # Drop profiles with low fit if requested
  if (drop_lowfit) {
    idx <- results$fit_est >= .70
    results <- results[idx, ]
    scores <- scores[idx, ]
  }
  
  # Drop the info columns
  scores_only <- scores[, -c(1:3)]
  
  # Reshape scores to long format
  score_df <- data.frame(
    Label = rep(scores$Label, times = length(angles)),
    Scale = rep(colnames(scores_only), each = nrow(scores)),
    Angle = rep(angles, each = nrow(scores)),
    Score = as.vector(unlist(scores_only))
  )
  curve_fit <- function(p, x) {
    p$e_est + p$a_est * cos((x - p$d_est) * pi / 180)
  }
  all_angles <- seq(from = min(angles), to = max(angles), length.out = 100)
  param_list <- split(results, results$Label)
  pred_mat <- sapply(param_list, FUN = curve_fit, x = all_angles)
  
  pred_df <- data.frame(
    Label = rep(colnames(pred_mat), each = nrow(pred_mat)),
    Angle = rep(all_angles, times = ncol(pred_mat)),
    Score = as.vector(pred_mat)
  )
  pred_df <- merge(pred_df, results[c("Label", "fit_est")])
  pred_df$lnty <- ifelse(pred_df$fit_est >= .70, "solid", "dashed")
  
  # Create ggplot
  ggplot2::ggplot() + 
    ggplot2::facet_wrap(~Label) +
    # Curve
    ggplot2::geom_line(
      data = pred_df,
      mapping = ggplot2::aes(
        x = Angle, 
        y = Score, 
        linetype = lnty, 
        color = Label
      ),
      linewidth = 1.25
    ) +
    # Connectors
    ggplot2::geom_line(
      data = score_df,
      mapping = ggplot2::aes(x = Angle, y = Score, group = Label),
      color = "black"
    ) +
    # Points
    ggplot2::geom_point(
      data = score_df,
      mapping = ggplot2::aes(x = Angle, y = Score, group = Label),
      color = "black"
    ) +
    ggplot2::scale_x_continuous(
      breaks = angles,
      labels = angle_labels
    ) +
    ggplot2::scale_linetype_identity() +
    ggplot2::labs(x = xlabel) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    )
}

#' Create a Difference Plot of SSM Contrast Results
#'
#' Take in the results of a Structural Summary Method analysis with pairwise
#' contrasts and plot the point and interval estimates for each parameter's
#' contrast (e.g., between groups or measures).
#'
#' @param ssm_object Required. The results output of \code{ssm_analyze()}.
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
#' @export
#' @examples
#' \donttest{
#' data("jz2017")
#' res <- ssm_analyze(
#'   jz2017,
#'   scales = 2:9, 
#'   measures = c("NARPD", "ASPD"),
#'   contrast = TRUE
#' )
#' ssm_plot_contrast(res)
#' }
ssm_plot_contrast <- function(ssm_object, drop_xy = FALSE, 
                              sig_color = "#fc8d62", ns_color = "white",
                              linesize = 1.25, fontsize = 12, ...) {
  
  stopifnot(ssm_object$details$contrast)
  
  # Prepare all estimates
  plabs <- c(
    e = expression(paste(Delta, " Elevation")),
    x = expression(paste(Delta, " X Value")),
    y = expression(paste(Delta, " Y Value")),
    a = expression(paste(Delta, " Amplitude")),
    d = expression(paste(Delta, " Displacement"))
  )
  pvals <- c("e", "x", "y", "a", "d")
  res <- ssm_object$results[nrow(ssm_object$results), ]
  
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
    ggplot2::labs(y = paste0("Contrast (", res$Label, ")")) +
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
#' @param ssm_object Required. The results output of `ssm_analyze()`.
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
ssm_table <- function(ssm_object, caption = NULL, 
                      drop_xy = FALSE, render = TRUE) {
  
  stopifnot(class(ssm_object) == "circumplex_ssm")
  stopifnot(is_null_or_char(caption, n = 1))
  stopifnot(is_flag(drop_xy))
  stopifnot(is_flag(render))

  df <- ssm_object$results

  # Create default caption
  if (is.null(caption)) {
    caption <- dcaption(ssm_object)
  }

  # Format output data
  table_df <- 
    data.frame(
      Profile = df$Label,
      Elevation = sprintf("%.2f (%.2f, %.2f)", df$e_est, df$e_lci, df$e_uci),
      `X Value` = sprintf("%.2f (%.2f, %.2f)", df$x_est, df$x_lci, df$x_uci),
      `Y Value` = sprintf("%.2f (%.2f, %.2f)", df$y_est, df$y_lci, df$y_uci),
      Amplitude = sprintf("%.2f (%.2f, %.2f)", df$a_est, df$a_lci, df$a_uci),
      Displacement = sprintf("%.1f (%.1f, %.1f)", df$d_est, df$d_lci, df$d_uci),
      Fit = sprintf("%.3f", df$fit_est)
    )

  # Rename first column
  colnames(table_df)[[1]] <- ifelse(
    test = ssm_object$details$contrast, 
    yes = "Contrast", 
    no = "Profile"
  )

  # Drop the x and y columns if requested
  if (drop_xy) {
    table_df <- table_df[, -c(3, 4)]
  }
  
  # Format and render HTML table if requested
  if (render) {
    html_render(table_df, caption)
  }

  table_df
}

# Build the default caption for the ssm_table function
dcaption <- function(ssm_object) {
  if (ssm_object$details$contrast) {
    sprintf(
      "%s-based Structural Summary Statistic Contrasts with %s CIs",
      ssm_object$details$score_type,
      str_percent(ssm_object$details$interval)
    )
  } else {
    sprintf(
      "%s-based Structural Summary Statistics with %s CIs",
      ssm_object$details$score_type,
      str_percent(ssm_object$details$interval)
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


#' @export
ssm_plot_scores <- function(x,
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
      mapping = ggplot2::aes(x = px, y = py, color = Label, linetype = Label),
      fill = NA,
      linewidth = linewidth
    ) +
    ggplot2::geom_point(
      data = scores_long,
      mapping = ggplot2::aes(x = px, y = py, color = Label),
      size = pointsize
    )
  
}
