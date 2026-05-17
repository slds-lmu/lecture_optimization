# Used in: ../02-taylor.tex
#
# Plot sin(x) together with its Taylor polynomials of orders 0 through 4 around
# the expansion point a = 2.

set.seed(1L)

library(data.table)
library(ggplot2)

dir.create("../figure", recursive = TRUE, showWarnings = FALSE)

x_grid = seq(-15, 15, length.out = 1000L)
expansion_point = 2

y0 = rep(sin(expansion_point), length(x_grid))
y1 = y0 + cos(expansion_point) * (x_grid - expansion_point)
y2 = y1 - sin(expansion_point) * (x_grid - expansion_point)^2 / factorial(2)
y3 = y2 - cos(expansion_point) * (x_grid - expansion_point)^3 / factorial(3)
y4 = y3 + sin(expansion_point) * (x_grid - expansion_point)^4 / factorial(4)

plot_data = rbindlist(list(
  data.table(x = x_grid, y = sin(x_grid), order = "sin(x)"),
  data.table(x = x_grid, y = y0, order = "m = 0"),
  data.table(x = x_grid, y = y1, order = "m = 1"),
  data.table(x = x_grid, y = y2, order = "m = 2"),
  data.table(x = x_grid, y = y3, order = "m = 3"),
  data.table(x = x_grid, y = y4, order = "m = 4")
))
plot_data[, order := factor(order, levels = c("sin(x)", "m = 0", "m = 1", "m = 2", "m = 3", "m = 4"))]

taylor_plot = ggplot(plot_data, aes(x = x, y = y, colour = order)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = c("black", "red", "blue", "orange", "green", "cyan")) +
  labs(
    title = "Taylor polynomials of sin(x) around a = 2",
    x = "x",
    y = "y",
    colour = "Order"
  ) +
  coord_cartesian(xlim = c(-6, 10), ylim = c(-8, 8)) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "right",
    legend.title = element_blank()
  )

ggsave("../figure/taylor_univariate.png", taylor_plot, width = 10, height = 8)
