# ptest <- data.frame(d = c(90,315), a = c(2,2))
# cibar <- data.frame(d_min = c(85,300), d_max = c(100,340), a_min = c(1,1.5), a_max = c(2.1,2.6))
# base <- ggplot() + xlab(NULL) + ylab(NULL) + theme_bw() + 
#   scale_x_continuous(breaks = seq(0, 360, 45), limits = c(0, 360)) + 
#   scale_y_continuous(breaks = seq(0, 3, 1), limits = c(0,3)) +
#   coord_polar(theta = "x", start = 1.5 * pi, direction = -1) +
#   geom_rect(data = cibar,
#     mapping = aes(xmin = d_min, xmax = d_max, ymin = a_min, ymax = a_max), alpha = 1/2, color = "red", fill = "red") +
#   geom_point(data = ptest, mapping = aes(x = d, y = a))
# base
