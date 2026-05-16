# Used in: ../03-convex.tex
#
# Create two schematic convexity plots: one for a strictly convex function and
# one for a convex but not strictly convex function.

set.seed(1L)

library(data.table)
library(ggplot2)

dir.create("../figure", recursive = TRUE, showWarnings = FALSE)

strictly_convex_function = function(x) {
  x^2 - 4 * x + 4
}

convex_piecewise_function = function(x) {
  ifelse(x > 1, 0, 1 - x)
}

convexity_plot_1 = ggplot(data.table(x = c(0.5, 4.5)), aes(x = x)) +
  stat_function(fun = strictly_convex_function, linewidth = 1) +
  geom_point(data = data.table(x = c(1, 4), y = c(1, 4)), aes(x = x, y = y), size = 3) +
  geom_point(data = data.table(x = 2.5, y = 2.5), aes(x = x, y = y), colour = "red", size = 4) +
  geom_point(data = data.table(x = 2.5, y = 0.25), aes(x = x, y = y), colour = "blue", size = 4) +
  geom_segment(
    data = data.table(x = 1, y = 1, xend = 4, yend = 4),
    aes(x = x, y = y, xend = xend, yend = yend),
    linetype = 2
  ) +
  annotate("text", x = 1, y = 0.7, label = "x", size = 6) +
  annotate("text", x = 4, y = 3.7, label = "y", size = 6) +
  annotate("text", x = 2.5, y = 3, label = "f(x) + t[f(y) - f(x)]", colour = "red", angle = 22, size = 5) +
  annotate("text", x = 3.2, y = 0.25, label = "f(x + t[y - x])", colour = "blue", size = 5) +
  coord_cartesian(xlim = c(0.5, 4.5), ylim = c(-0.2, 5)) +
  theme_bw(base_size = 14) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

convexity_plot_2 = ggplot(data.table(x = c(-1, 3)), aes(x = x)) +
  stat_function(fun = convex_piecewise_function, linewidth = 1) +
  geom_point(data = data.table(x = c(0, 2), y = c(1, 0)), aes(x = x, y = y), size = 3) +
  geom_point(data = data.table(x = 1, y = 0.5), aes(x = x, y = y), colour = "red", size = 4) +
  geom_point(data = data.table(x = 1, y = 0), aes(x = x, y = y), colour = "blue", size = 4) +
  geom_segment(
    data = data.table(x = 0, y = 1, xend = 2, yend = 0),
    aes(x = x, y = y, xend = xend, yend = yend),
    linetype = 2
  ) +
  annotate("text", x = 0, y = 0.8, label = "x", size = 6) +
  annotate("text", x = 2, y = -0.2, label = "y", size = 6) +
  annotate("text", x = 1.15, y = 0.7, label = "f(x) + t[f(y) - f(x)]", colour = "red", angle = -38, size = 5) +
  annotate("text", x = 1, y = -0.2, label = "f(x + t[y - x])", colour = "blue", size = 5) +
  coord_cartesian(xlim = c(-1, 3), ylim = c(-0.35, 1.2)) +
  theme_bw(base_size = 14) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggsave("../figure/convexity_1.pdf", convexity_plot_1, width = 6, height = 4)
ggsave("../figure/convexity_2.pdf", convexity_plot_2, width = 6, height = 4)
