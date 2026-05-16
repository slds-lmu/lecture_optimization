# Used in: ../03-convex.tex
#
# Compare one convex, one concave, and one neither-convex-nor-concave function
# together with illustrative secant lines.

set.seed(1L)

library(data.table)
library(ggplot2)
library(grid)

dir.create("../figure", recursive = TRUE, showWarnings = FALSE)

x_grid = seq(-15, 15, length.out = 1000L)
x_grid_log = x_grid[x_grid > 0]

curve_data = rbindlist(list(
  data.table(x = x_grid, y = abs(x_grid), function_id = "abs"),
  data.table(x = x_grid_log, y = log(x_grid_log), function_id = "log"),
  data.table(x = x_grid, y = exp(-(x_grid^2)), function_id = "exp")
))

point_data = rbindlist(list(
  data.table(function_id = "abs", x = c(-1, 1.5), y = abs(c(-1, 1.5))),
  data.table(function_id = "log", x = c(0.5, 2), y = log(c(0.5, 2))),
  data.table(function_id = "exp", x = c(-2.2, 0.5), y = exp(-(c(-2.2, 0.5)^2)))
))

segment_data = data.table(
  function_id = c("abs", "log", "exp"),
  x = c(-1, 0.5, -2.2),
  y = c(1, log(0.5), exp(-(-2.2)^2)),
  xend = c(1.5, 2, 0.5),
  yend = c(1.5, log(2), exp(-(0.5^2)))
)

function_colors = c(abs = "#0072B2", log = "#009E73", exp = "#D55E00")

conv_conc_plot = ggplot() +
  geom_line(
    data = curve_data,
    aes(x = x, y = y, colour = function_id),
    linewidth = 1
  ) +
  geom_point(
    data = point_data,
    aes(x = x, y = y, colour = function_id),
    size = 2
  ) +
  geom_segment(
    data = segment_data,
    aes(x = x, y = y, xend = xend, yend = yend, colour = function_id),
    linewidth = 0.9,
    linetype = 2
  ) +
  scale_colour_manual(
    values = function_colors,
    labels = c(abs = "|x|", log = "log(x)", exp = "exp(-x^2)")
  ) +
  labs(x = "x", y = "y", colour = NULL) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-1, 2)) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = c(0.15, 0.12),
    legend.key.size = unit(1.1, "cm"),
    panel.grid.minor = element_blank()
  )

ggsave("../figure/conv_conc_functions.png", conv_conc_plot, width = 10, height = 8)
