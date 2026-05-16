# Used in: slides-multivar-first-order-1-GD.tex
#
# Visualizes a descent direction, the gradient, and the orthogonal contour direction
# on a two-dimensional quadratic objective.

set.seed(1L)

library(ggplot2)
library(grid)

quadratic_fun = function(x1, x2) {
  2 * x1^2 + x2^2
}

compute_grid_data = function(x_range, y_range, objective_fun) {
  grid_data = expand.grid(x1 = x_range, x2 = y_range)
  grid_data$z = mapply(objective_fun, grid_data$x1, grid_data$x2)
  grid_data
}

build_contour_plot = function(grid_data) {
  ggplot(grid_data, aes(x = x1, y = x2, z = z)) +
    geom_contour_filled() +
    geom_point(
      data = data.frame(x1 = 0, x2 = 0),
      aes(x = x1, y = x2),
      colour = "green4",
      size = 3,
      inherit.aes = FALSE
    ) +
    labs(x = expression(x[1]), y = expression(x[2])) +
    guides(fill = "none") +
    theme_minimal(base_size = 12)
}

grid_data = compute_grid_data(
  x_range = seq(-2, 2, by = 0.05),
  y_range = seq(-2, 2, by = 0.05),
  objective_fun = quadratic_fun
)

point = c(1, 1)
gradient_direction = c(4 * point[1], 2 * point[2])
gradient_direction = gradient_direction / sqrt(sum(gradient_direction^2))

orthogonal_direction = c(1, -gradient_direction[1] / gradient_direction[2])
orthogonal_direction = orthogonal_direction / sqrt(sum(orthogonal_direction^2))

descent_direction = c(0, -1)
arrow_style = arrow(length = unit(0.1, "inches"))

descent_plot = build_contour_plot(grid_data) +
  geom_segment(
    aes(
      x = point[1],
      y = point[2],
      xend = point[1] + descent_direction[1],
      yend = point[2] + descent_direction[2]
    ),
    arrow = arrow_style,
    colour = "orange"
  ) +
  annotate(
    "text",
    x = point[1] - 0.3,
    y = point[2] - 0.4,
    label = "d",
    parse = TRUE,
    colour = "orange",
    size = 3
  ) +
  geom_segment(
    aes(
      x = point[1],
      y = point[2],
      xend = point[1] + gradient_direction[1],
      yend = point[2] + gradient_direction[2]
    ),
    arrow = arrow_style,
    colour = "red3"
  ) +
  annotate(
    "text",
    x = point[1] + 0.3,
    y = point[2] + 0.4,
    label = "nabla * f(x)",
    parse = TRUE,
    colour = "red3",
    size = 3
  ) +
  geom_segment(
    aes(
      x = point[1],
      y = point[2],
      xend = point[1] - orthogonal_direction[1],
      yend = point[2] - orthogonal_direction[2]
    ),
    colour = "red3",
    linetype = "dashed"
  ) +
  geom_segment(
    aes(
      x = point[1],
      y = point[2],
      xend = point[1] + orthogonal_direction[1],
      yend = point[2] + orthogonal_direction[2]
    ),
    colour = "red3",
    linetype = "dashed"
  ) +
  annotate("text", x = point[1] + 0.6, y = point[2] - 0.8, label = "90°", colour = "red3", size = 2) +
  annotate("text", x = point[1] - 0.2, y = point[2] + 0.8, label = "270°", colour = "red3", size = 2)

if (interactive()) {
  print(descent_plot)
}

ggsave(
  filename = "../figure/descent_direction.pdf",
  plot = descent_plot,
  width = 3,
  height = 3
)
