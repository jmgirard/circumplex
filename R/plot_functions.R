
base_plot <- function(data = NULL, angles = octants, alim = c(0, 1), 
  abreaks = round(seq(alim[1], alim[2], length.out = 5), 2)) {
  p <- ggplot(data) + xlab(NULL) + ylab(NULL) + theme_bw() +
    scale_x_continuous(breaks = octants, limits = c(0, 360)) + 
    scale_y_continuous(breaks = abreaks, limits = alim) +
    coord_polar(theta = "x", start = 1.5 * pi, direction = -1) +
    theme(panel.grid.major = element_line(size = 1),
      panel.grid.minor = element_blank())
  
  p
}

ssm_plot <- function(.results, angles, type, labels = FALSE, palette = "Set1") {
  p <- base_plot(.results, angles, alim = c(0, max(.results$a_uci) * 1.10)) +
    scale_color_brewer(palette = palette) + scale_fill_brewer(palette = palette)

  if (type == "Measure") {
    p <- p + geom_rect(aes(xmin = d_lci, xmax = d_uci, ymin = a_lci, ymax = a_uci,
      color = Measure, fill = Measure), alpha = 1 / 2, size = 1) + 
      geom_point(aes(x = d_est, y = a_est, color = Measure), size = 2) 
    if (labels == TRUE) {
      p <- p + geom_label_repel(aes(x = d_est, y = a_est,
        label = Measure, color = Measure)) + theme(legend.position = "none")
    }
  } else if (type == "Profile") {
    p <- p + geom_rect(aes(xmin = d_lci, xmax = d_uci, ymin = a_lci, ymax = a_uci,
      color = Group, fill = Group), alpha = 1 / 2, size = 1) + 
      geom_point(aes(x = d_est, y = a_est, color = Group), size = 2) 
    if (nlevels(.results$Group) == 1) {
      p <- p + theme(legend.position = "none")
    }
    if (labels == TRUE) {
      p <- p + geom_label_repel(aes(x = d_est, y = a_est,
        label = Group, color = Group)) + theme(legend.position = "none")
    }
  }

  p
}

diff_plot <- function(.results, interval) {
  res <- .results %>%
    dplyr::mutate(d_est = rwd(d_est), d_lci = rwd(d_lci), d_uci = rwd(d_uci)) %>%
    tidyr::gather(key, value, -Contrast, -fit) %>%
    tidyr::extract(key, c("Parameter", "Type"), "(.)_(...)") %>%
    tidyr::spread(Type, value) %>%
    dplyr::rename(Estimate = est)
  p <- ggplot(res) +
    geom_pointrange(aes(x = Contrast, y = Estimate, ymin = lci, ymax = uci, color = Contrast)) +
    geom_hline(yintercept = 0) + 
    facet_wrap(~Parameter, nrow = 1, scales = "free") +
    theme_bw() + theme(legend.position = "top")
  p
}
