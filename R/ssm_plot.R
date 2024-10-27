
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
  df_plot[["Label"]] <- factor(
    df_plot[["Label"]], 
    levels = unique(as.character(df_plot[["Label"]]))
  )
  
  # Remove profiles with low model fit (unless overrided)
  n <- nrow(df_plot)
  if (drop_lowfit) {
    df_plot <- df_plot[df_plot$fit_est >= .70, ]
    if (nrow(df_plot) < 1) {
      stop("After removing profiles with low fit, there were none left to plot.")
    }
  }
  
  df_plot[["lnty"]] <- ifelse(df_plot[["fit_est"]] >= .70, "solid", "dotted")

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
        mapping = ggplot2::aes(
          x0 = 0,
          y0 = 0,
          r0 = .data$a_lci, 
          r = .data$a_uci, 
          start = .data$d_lci, 
          end = .data$d_uci
        ),
        fill = "cornflowerblue", 
        color = "cornflowerblue", 
        alpha = 0.4,
        linewidth = 1
      ) +
      ggplot2::geom_point(
        data = df_plot,
        mapping = ggplot2::aes(
          x = .data$x_est, 
          y = .data$y_est
        ),
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
        mapping = ggplot2::aes(
          x0 = 0,
          y0 = 0,
          r0 = .data$a_lci,
          r = .data$a_uci,
          start = .data$d_lci,
          end = .data$d_uci,
          fill = .data$Label,
          color = .data$Label,
          linetype = .data$lnty
        ),
        alpha = 0.4,
        linewidth = 1
      ) +
      ggplot2::geom_point(
        data = df_plot,
        mapping = ggplot2::aes(
          x = .data$x_est,
          y = .data$y_est,
          color = .data$Label,
          fill = .data$Label
        ),
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
    requireNamespace("ggrepel")
    p <- p + 
      ggrepel::geom_label_repel(
        data = df_plot,
        mapping = ggplot2::aes(
          x = .data$x_est,
          y = .data$y_est,
          label = .data$Label
        ),
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
        x = .data$Angle,
        y = .data$Score,
        linetype = .data$lnty,
        color = .data$Label
      ),
      linewidth = 1.25
    ) +
    # Connectors
    ggplot2::geom_line(
      data = score_df,
      mapping = ggplot2::aes(
        x = .data$Angle,
        y = .data$Score,
        group = .data$Label
      ),
      color = "black"
    ) +
    # Points
    ggplot2::geom_point(
      data = score_df,
      mapping = ggplot2::aes(
        x = .data$Angle,
        y = .data$Score,
        group = .data$Label
      ),
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
    ggplot2::facet_wrap(
      ~Parameter, 
      nrow = 1, 
      scales = "free",
      labeller = ggplot2::label_parsed
    ) +
    ggplot2::geom_hline(
      yintercept = 0, 
      linewidth = linesize, 
      color = "darkgray"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        x = "1", 
        ymin = .data$lci, 
        ymax = .data$uci
      ),
      linewidth = linesize, 
      width = 0.15
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = "1", 
        y = .data$Difference, 
        fill = .data$sig
      ),
      size = linesize * 3,
      stroke = linesize,
      shape = 21
    ) +
    ggplot2::scale_fill_manual(
      name = "Significant",
      values = c("TRUE" = sig_color, "FALSE" = ns_color)
    ) +
    ggplot2::labs(y = paste0("Contrast (", res$Label, ")")) +
    ggplot2::theme_bw(base_size = fontsize) +
    ggplot2::theme(
      legend.position = "top",
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_line(linetype = "dashed"),
      axis.ticks.x = ggplot2::element_blank()
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
      mapping = ggplot2::aes(x0 = 0, y0 = 0, r = 5),
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
