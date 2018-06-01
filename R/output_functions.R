#' Create an Empty Circular Plot
#'
#' Initialize an empty circular plot to be filled in by \code{circle_plot()}.
#'
#' @param angles A numerical vector containing the angular displacement of each
#'   circumplex scale (in degrees). A line segment from the circle's origin to
#'   perimeter and a label outside the perimeter will be drawn for each angle.
#' @param amax A positive real number corresponding to the radius of the circle.
#'   It is used to scale the amplitude values and will determine which amplitude
#'   labels are drawn.
#' @param font.size A positive real number corresponding to the size (in mm) of
#'   the amplitude and displacement labels (default = 3).
#' @return A ggplot variable containing an empty circular plot.

circle_base <- function(angles = octants, amax = 0.5, font.size = 3) {

  # Require plot to be square and remove default styling --------------------
  b <- ggplot() + coord_fixed() + theme_void()
  
  # Expand the x-axis to fit long horizontal labels -------------------------
  b <- b + scale_x_continuous(expand = c(.1, .1))  

  # Draw circles corresponding to amplitude scale ---------------------------
  b <- b +
    ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1:4),
      color = "gray", size = 0.5) + 
    ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 5),
      color = "darkgray", size = 1)
  
  # Draw segments corresponding to displacement scale -----------------------
  b <- b +
    geom_segment(
      aes(
        x = 0,
        y = 0,
        xend = 5 * cos(angles * pi / 180),
        yend = 5 * sin(angles * pi / 180)
      ), 
      color = "darkgray",
      size = 0.5
    )
  
  # Draw labels for amplitude scale -----------------------------------------
  b <- b + 
    geom_label(
      aes(
        x = 1:4,
        y = 0,
        label = sprintf("%.2f", seq(from = 0, to = amax, length.out = 6)[2:5])
      ),
      color = "gray",
      label.size = NA,
      size = font.size
    )

  # Draw labels for displacement scale --------------------------------------
  b <- b + 
    geom_label(
      aes(
        x = 5.1 * cos(angles * pi / 180), y = 5.1 * sin(angles * pi / 180),
        label = sprintf("%d\u00B0", angles)
      ),
      size = font.size,
      color = "darkgray",
      label.size = NA,
      hjust = "outward",
      vjust = "outward"
    )
  
  b
}

#' Create a Circular Plot of SSM Results
#'
#' Take in the results of a Structural Summary Method analysis and plot the
#' point and interval estimate for each row (e.g., group or measure) in a
#' circular space quantified by displacement and amplitude.
#'
#' @param .results The output of \code{ssm_profiles()} or \code{ssm_measures()}.
#' @param angles A numerical vector containing the angular displacement of each
#'   circumplex scale (in degrees). A line segment from the circle's origin to
#'   perimeter and a label outside the perimeter will be drawn for each angle.
#' @param type A string corresponding to \code{"Profile"} or \code{"Measure"}.
#' @param palette A string corresponding to the Color Brewer palette (default =
#'   "Set1"). See http://www.cookbook-r.com/Graphs/Colors_(ggplot2).
#' @param amax A positive real number corresponding to the radius of the circle.
#'   It is used to scale the amplitude values and will determine which amplitude
#'   labels are drawn.
#' @param font.size A positive real number corresponding to the size (in mm) of
#'   the amplitude and displacement labels (default = 3).
#' @return A ggplot variable containing a completed circular plot.

circle_plot <- function(.results, angles, type, palette = "Set1",
  amax = pretty_max(.results$a_uci), font.size = 3) {
  
  # Convert results to numbers usable by ggplot and ggforce -----------------
  df_plot <- .results %>%
    dplyr::rowwise() %>% 
    dplyr::mutate(
      d_uci = ifelse(d_uci < d_lci && d_uci < 180, rwd(d_uci), d_uci),
      d_lci = ifelse(d_lci > d_uci && d_lci > 180, rwd(d_lci), d_lci),
      d_uci = (d_uci - 90) * (-pi / 180),
      d_lci = (d_lci - 90) * (-pi / 180),
      a_lci = a_lci * 10 / (2 * amax),
      a_uci = a_uci * 10 / (2 * amax),
      x_est = x_est * 10 / (2 * amax),
      y_est = y_est * 10 / (2 * amax)
    )

  # Initialize and configure the circle plot --------------------------------
  p <- circle_base(angles, amax, font.size) +
    scale_color_brewer(palette = palette) +
    scale_fill_brewer(palette = palette)
  
  if (type == "Measure") {
    # Draw point and interval estimates for each measure ----------------------
    p <- p + 
      ggforce::geom_arc_bar(
        data = df_plot,
        aes(x0 = 0, y0 = 0, r0 = a_lci, r = a_uci, start = d_lci, end = d_uci,
          fill = Measure, color = Measure),
        alpha = 0.5,
        size = 1
      ) +
      geom_point(
        data = df_plot,
        aes(x = x_est, y = y_est, color = Measure),
        size = 2
      )
  } else if (type == "Profile") {
    # Draw point and interval estimates for each group ------------------------
    p <- p +
      ggforce::geom_arc_bar(
        data = df_plot,
        aes(x0 = 0, y0 = 0, r0 = a_lci, r = a_uci, start = d_lci, end = d_uci,
          fill = Group, color = Group),
        alpha = 0.5,
        size = 1
      ) +
      geom_point(
        data = df_plot,
        aes(x = x_est, y = y_est, color = Group),
        size = 2
      )

    # Remove the legend if there is only one group ----------------------------
    if (nlevels(.results$Group) == 1) {
      p <- p + theme(legend.position = "none")
    }
  }
  
  p
}

#' Create a Difference Plot of SSM Results
#'
#' Take in the results of a Structural Summary Method analysis with pairwise
#' contrasts and plot the point and interval estimates for each parameter's
#' contrast (e.g., between groups or measures).
#'
#' @param .results The output of \code{ssm_profiles()} or \code{ssm_measures()}
#'   that included contrasts (i.e., \code{pairwise = TRUE}).
#' @return A ggplot variable containing difference point-ranges faceted by SSM
#'   parameter. An interval that does not contain the value of zero has p<.05.

diff_plot <- function(.results) {
  
  res <- .results %>%
    dplyr::mutate(
      d_uci = ifelse(d_uci < d_lci && d_uci < 180, rwd(d_uci), d_uci),
      d_lci = ifelse(d_lci > d_uci && d_lci > 180, rwd(d_lci), d_lci)
    ) %>%
    tidyr::gather(key, value, -Contrast, -fit) %>%
    tidyr::extract(key, c("Parameter", "Type"), "(.)_(...)") %>%
    tidyr::spread(Type, value) %>%
    dplyr::rename(Estimate = est)
  p <- ggplot(res) + theme_bw() + theme(legend.position = "top") +
    geom_pointrange(
      aes(
        x = Contrast, y = Estimate, ymin = lci, ymax = uci,
        color = Contrast
      ),
      size = 1
    ) +
    geom_hline(yintercept = 0) + 
    facet_wrap(~Parameter, nrow = 1, scales = "free")
    
  p
}

results_table <- function(results, contrast = FALSE, group = FALSE,
  measure = FALSE) {
  
  df <- results %>%
    dplyr::transmute(
      Elevation = sprintf("%.2f [%.2f, %.2f]", e_est, e_lci, e_uci),
      `X-Value` = sprintf("%.2f [%.2f, %.2f]", x_est, x_lci, x_uci),
      `Y-Value` = sprintf("%.2f [%.2f, %.2f]", y_est, y_lci, y_uci),
      Amplitude = sprintf("%.2f [%.2f, %.2f]", a_est, a_lci, a_uci),
      Displacement = sprintf("%.1f [%.1f, %.1f]", d_est, d_lci, d_uci),
      Fit = sprintf("%.3f", fit)
    )
  if (group == TRUE) {
    df <- dplyr::bind_cols(Group = results$label, df)
    if (contrast == TRUE) {
      df <- dplyr::bind_cols(Contrast = results$Contrast, df) %>%
        dplyr::mutate(
          New = ifelse(is.na(Contrast), as.character(Group), Contrast)
        ) %>%
        dplyr::select(New, dplyr::everything(), -Group, -Contrast) %>%
        dplyr::rename(Group = New)
    }
  }
  if (measure == TRUE) {
    df <- dplyr::bind_cols(Measure = results$label, df)
    if (contrast == TRUE) {
      df <- dplyr::bind_cols(Contrast = results$Contrast, df) %>%
        dplyr::mutate(
          New = ifelse(is.na(Contrast), as.character(Measure), Contrast)
        ) %>%
        dplyr::select(New, dplyr::everything(), -Measure, -Contrast) %>%
        dplyr::rename(Measure = New)
    }
  }
  df
}
