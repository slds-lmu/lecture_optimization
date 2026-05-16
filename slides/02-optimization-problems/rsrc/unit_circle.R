# Used in: ../slides-problems-2-constrained.tex
#
# Visualize the contour lines of f(x) = x1 + x2 together with the feasible set
# defined by the unit circle and the optimal boundary point.

set.seed(1L)

library(data.table)
library(ggplot2)

objective = function(x1, x2) {
  x1 + x2
}

# Create the contour grid and the unit circle boundary.
axis_limits = c(-1.5, 1.5)
x1_grid = seq(axis_limits[1], axis_limits[2], length.out = 100L)
x2_grid = seq(axis_limits[1], axis_limits[2], length.out = 100L)

contour_data = CJ(x1 = x1_grid, x2 = x2_grid)
contour_data[, z := objective(x1, x2)]

angles = seq(0, 2 * pi, length.out = 300L)
circle_data = data.table(x1 = cos(angles), x2 = sin(angles))
optimal_point = data.table(x1 = -sqrt(2) / 2, x2 = -sqrt(2) / 2)

# Draw the objective contours, feasible set, and optimizer.
unit_circle_plot = ggplot() +
  geom_contour_filled(data = contour_data, aes(x = x1, y = x2, z = z)) +
  geom_path(data = circle_data, aes(x = x1, y = x2), colour = "black", linewidth = 1) +
  geom_point(data = optimal_point, aes(x = x1, y = x2), colour = "red", size = 5) +
  labs(
    x = expression(x[1]),
    y = expression(x[2]),
    fill = expression(f(x[1], x[2]))
  ) +
  coord_fixed(xlim = axis_limits, ylim = axis_limits) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "right")

ggsave("../figure/unit_circle.png", unit_circle_plot, width = 8, height = 6)
