# Used in: ../03-convex.tex
#
# Visualize the first-order characterization of convexity by comparing a convex
# function with one of its tangent lines.

set.seed(1L)

library(data.table)
library(ggplot2)

dir.create("../figure", recursive = TRUE, showWarnings = FALSE)

x0 = 0.1
x_limits = c(-0.3, 0.7)
y_limits = c(-0.02, 0.2)

convex_function = function(y) {
  y^2
}

tangent_function = function(y) {
  x0^2 + 2 * x0 * (y - x0)
}

x_grid = seq(x_limits[1], x_limits[2], length.out = 400L)
plot_data = data.table(
  x = x_grid,
  function_value = convex_function(x_grid),
  tangent_value = tangent_function(x_grid)
)

first_order_plot = ggplot(plot_data, aes(x = x)) +
  geom_line(aes(y = function_value), linewidth = 1) +
  geom_line(aes(y = tangent_value), linewidth = 1, linetype = 2) +
  geom_point(
    data = data.table(x = x0, y = convex_function(x0)),
    aes(x = x, y = y),
    inherit.aes = FALSE,
    size = 2.8
  ) +
  annotate("text", x = -0.25, y = convex_function(-0.25), label = "f(y)", hjust = 0, size = 5.5) +
  annotate("text", x = x0 + 0.02, y = convex_function(x0) - 0.02, label = "(x, f(x))", size = 5.5) +
  annotate("text", x = 0.48, y = tangent_function(0.48) + 0.02, label = "f(x) + f'(x) (y - x)", size = 5.5) +
  coord_cartesian(xlim = x_limits, ylim = y_limits, expand = FALSE) +
  theme_void(base_size = 14)

ggsave("../figure/conv-first-order-cond.png", first_order_plot, width = 4, height = 3)
