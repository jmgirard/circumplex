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