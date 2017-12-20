
circle_base <- function(angles = octants, amax = 0.5, font.size = 3) {
  
  ggplot() + coord_fixed() + theme_void() +
    geom_circle(aes(x0 = 0, y0 = 0, r = 1:4), color = "gray", size = 0.5) + 
    geom_circle(aes(x0 = 0, y0 = 0, r = 5), color = "darkgray", size = 1) +
    geom_segment(
      aes(x = 0, y = 0,
        xend = 5 * cos(angles * pi / 180),
        yend = 5 * sin(angles * pi / 180)
      ),
      color = "darkgray", size = 0.5
    ) +
    geom_label(
      aes(x = 1:4, y = 0,
        label = sprintf("%.2f", seq(from = 0, to = amax, length.out = 6)[2:5])),
      color = "gray", label.size = NA, size = font.size
    ) +
    geom_label(
      aes(x = 5.1 * cos(angles * pi / 180), y = 5.1 * sin(angles * pi / 180),
        label = sprintf("%d\u00B0", angles)), size = font.size,
      color = "darkgray", label.size = NA, hjust = "outward", vjust = "outward"
    ) +
    scale_x_continuous(expand = c(.1, .1))
  
}

circle_plot <- function(.results, angles, type, palette = "Set1",
  amax = pretty_max(.results$a_uci), font.size = 3) {
  
  df_plot <- .results %>% dplyr::mutate(
    d_uci = ifelse(d_uci < d_lci && d_uci < 180, rwd(d_uci), d_uci),
    d_lci = ifelse(d_lci > d_uci && d_lci > 180, rwd(d_lci), d_lci)
  ) %>% 
  mutate(
    d_lci = ggrad(d_lci),
    d_uci = ggrad(d_uci),
    a_lci = a_lci * 10 / (2 * amax),
    a_uci = a_uci * 10 / (2 * amax),
    x_est = x_est * 10 / (2 * amax),
    y_est = y_est * 10 / (2 * amax)
  )
  p <- circle_base(angles, amax, font.size) +
    scale_color_brewer(palette = palette) +
    scale_fill_brewer(palette = palette)
  if (type == "Measure") {
    p <- p + geom_arc_bar(data = df_plot,
      aes(x0 = 0, y0 = 0, r0 = a_lci, r = a_uci, start = d_lci, end = d_uci,
        fill = Measure, color = Measure),
      alpha = 0.5, size = 1) +
      geom_point(data = df_plot, aes(x = x_est, y = y_est, color = Measure),
        size = 2)
  } else if (type == "Profile") {
    p <- p + geom_arc_bar(data = df_plot,
      aes(x0 = 0, y0 = 0, r0 = a_lci, r = a_uci, start = d_lci, end = d_uci,
        fill = Group, color = Group),
      alpha = 0.5, size = 1) +
      geom_point(data = df_plot, aes(x = x_est, y = y_est, color = Group),
        size = 2)
    if (nlevels(.results$Group) == 1) {
      p <- p + theme(legend.position = "none")
    }
  }
  
  p
}

diff_plot <- function(.results, interval) {
  
  res <- .results %>%
    dplyr::mutate(
      d_uci = ifelse(d_uci < d_lci && d_uci < 180, rwd(d_uci), d_uci),
      d_lci = ifelse(d_lci > d_uci && d_lci > 180, rwd(d_lci), d_lci)
    ) %>%
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
