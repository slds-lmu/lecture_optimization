# Used in: ../05-qfun.tex
#
# Show contour lines of a quadratic form together with its eigenvector
# directions.

set.seed(1L)

library(data.table)
library(ggplot2)
library(grid)

dir.create("../figure", recursive = TRUE, showWarnings = FALSE)

v1 = c(1, -1) / sqrt(2)
v2 = c(1, 1) / sqrt(2)
lambda_1 = 3
lambda_2 = 1

quadratic_form = function(x1, x2) {
  2 * x1^2 - 2 * x1 * x2 + 2 * x2^2
}

axis_limits = c(-8, 8)
x1_grid = seq(axis_limits[1], axis_limits[2], by = 0.05)
x2_grid = seq(axis_limits[1], axis_limits[2], by = 0.05)
contour_data = CJ(x1 = x1_grid, x2 = x2_grid)
contour_data[, z := quadratic_form(x1, x2)]

vector_data = data.table(
  x = 0,
  y = 0,
  xend = c(lambda_1 * v1[1], lambda_2 * v2[1]),
  yend = c(lambda_1 * v1[2], lambda_2 * v2[2]),
  colour = c("magenta", "orange")
)

quadratic_plot = ggplot(contour_data, aes(x = x1, y = x2, z = z)) +
  geom_contour(bins = 6, colour = "blue", linewidth = 0.8) +
  geom_abline(intercept = 0, slope = -1, colour = "magenta", linetype = 3, linewidth = 0.8) +
  geom_abline(intercept = 0, slope = 1, colour = "orange", linetype = 3, linewidth = 0.8) +
  geom_segment(
    data = vector_data,
    aes(x = x, y = y, xend = xend, yend = yend, colour = colour),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(0.12, "inches")),
    linewidth = 1
  ) +
  geom_point(
    data = data.table(x1 = 0, x2 = 0),
    aes(x = x1, y = x2),
    inherit.aes = FALSE,
    colour = "blue",
    size = 2.5
  ) +
  scale_colour_identity() +
  coord_fixed(xlim = axis_limits, ylim = axis_limits) +
  theme_void(base_size = 14)

ggsave("../figure/quadr-eigenv.png", quadratic_plot, width = 4, height = 4)
