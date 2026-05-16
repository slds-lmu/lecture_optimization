# Used in: ../01-diff.tex
#
# Visualize the gradient and the two partial derivatives of a quadratic
# function on a contour plot.

set.seed(1L)

library(data.table)
library(ggplot2)
library(grid)

dir.create("../figure", recursive = TRUE, showWarnings = FALSE)

objective = function(x1, x2) {
  0.5 * x1^2 + x1 * x2 + x2^2
}

gradient_function = function(x1, x2) {
  c(x1 + x2, x1 + 2 * x2)
}

x_limits = c(-2.5, 2.5)
x1_grid = seq(x_limits[1], x_limits[2], length.out = 100L)
x2_grid = seq(x_limits[1], x_limits[2], length.out = 100L)
contour_data = CJ(x1 = x1_grid, x2 = x2_grid)
contour_data[, z := objective(x1, x2)]

x1_point = 0.5
x2_point = 0.5
gradient_value = gradient_function(x1_point, x2_point)

arrow_data = data.table(
  label = c("gradient", "partial_x1", "partial_x2"),
  x = x1_point,
  y = x2_point,
  xend = c(x1_point + gradient_value[1], x1_point + gradient_value[1], x1_point),
  yend = c(x2_point + gradient_value[2], x2_point, x2_point + gradient_value[2]),
  alpha = c(1, 0.5, 0.5)
)

gradient_plot = ggplot(contour_data, aes(x = x1, y = x2, z = z)) +
  geom_contour_filled(bins = 8) +
  geom_segment(
    data = arrow_data,
    aes(x = x, y = y, xend = xend, yend = yend, alpha = alpha),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(0.12, "inches")),
    colour = "#D55E00",
    linewidth = 0.8,
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = 1.05,
    y = 0.95,
    label = "nabla * f(x[1], x[2])",
    parse = TRUE,
    colour = "#D55E00",
    size = 5.5
  ) +
  annotate(
    "text",
    x = 1.2,
    y = 0.1,
    label = "frac(partialdiff * f(x[1], x[2]), partialdiff * x[1])",
    parse = TRUE,
    colour = "#D55E00",
    size = 4.8
  ) +
  annotate(
    "text",
    x = -0.35,
    y = 1.75,
    label = "frac(partialdiff * f(x[1], x[2]), partialdiff * x[2])",
    parse = TRUE,
    colour = "#D55E00",
    size = 4.8
  ) +
  labs(x = expression(x[1]), y = expression(x[2])) +
  coord_fixed() +
  theme_minimal(base_size = 16) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("../figure/gradient.png", gradient_plot, width = 6, height = 4.5)
