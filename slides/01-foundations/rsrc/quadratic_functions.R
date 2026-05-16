# Used in: ../05-qfun.tex
#
# Create the univariate and multivariate quadratic-function illustrations used
# throughout the foundations chapter.

set.seed(1L)

library(data.table)
library(ggplot2)

dir.create("../figure", recursive = TRUE, showWarnings = FALSE)

build_surface_colors = function(z_matrix, n_colors) {
  n_rows = nrow(z_matrix)
  n_cols = ncol(z_matrix)
  z_facet = z_matrix[-1, -1] + z_matrix[-1, -n_cols] +
    z_matrix[-n_rows, -1] + z_matrix[-n_rows, -n_cols]
  palette = colorRampPalette(c("blue", "green", "yellow"))(n_colors)

  palette[cut(z_facet, n_colors)]
}

build_contour_grid = function(x1_grid, x2_grid, objective) {
  contour_data = CJ(x1 = x1_grid, x2 = x2_grid)
  contour_data[, z := objective(x1, x2)]
  contour_data
}

objective_example_1 = function(x1, x2) {
  2 * x1^2 - 2 * x1 * x2 + 2 * x2^2
}

x_grid_1d = seq(-2, 2, length.out = 400L)

quadratic_1d_data = rbindlist(list(
  data.table(x = x_grid_1d, y = x_grid_1d^2, panel = "q1(x) = x^2"),
  data.table(x = x_grid_1d, y = -(x_grid_1d^2), panel = "q2(x) = -x^2")
))

quadratic_1d_plot = ggplot(quadratic_1d_data, aes(x = x, y = y)) +
  geom_line(colour = "#D55E00", linewidth = 1) +
  facet_wrap(~ panel, nrow = 1) +
  coord_cartesian(ylim = c(-4, 4)) +
  labs(x = "x", y = "q(x)") +
  theme_bw(base_size = 14)

curvature_data = rbindlist(list(
  data.table(x = x_grid_1d, y = x_grid_1d^2, function_id = "q(x) = x^2", panel = "a > 0"),
  data.table(x = x_grid_1d, y = 2 * x_grid_1d^2, function_id = "q(x) = 2x^2", panel = "a > 0"),
  data.table(x = x_grid_1d, y = -(x_grid_1d^2), function_id = "q(x) = -x^2", panel = "a < 0"),
  data.table(x = x_grid_1d, y = -3 * x_grid_1d^2, function_id = "q(x) = -3x^2", panel = "a < 0")
))

curvature_plot = ggplot(curvature_data, aes(x = x, y = y, colour = function_id)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ panel, nrow = 1) +
  coord_cartesian(ylim = c(-12, 8)) +
  labs(x = "x", y = "q(x)", colour = NULL) +
  theme_bw(base_size = 14)

tangent_points = data.table(x0 = c(-2, -1, 0, 2))
tangent_points[, `:=`(
  y0 = x0^2,
  slope = 2 * x0,
  intercept = -(x0^2),
  panel = sprintf("x = %s", x0)
)]

x_grid_derivative = seq(-3, 3, length.out = 400L)
derivative_data = rbindlist(lapply(seq_len(nrow(tangent_points)), function(i) {
  point = tangent_points[i]

  data.table(
    panel = point[["panel"]],
    x = x_grid_derivative,
    curve = x_grid_derivative^2,
    tangent = point[["intercept"]] + point[["slope"]] * x_grid_derivative
  )
}))

derivative_plot = ggplot(derivative_data, aes(x = x)) +
  geom_line(aes(y = curve), colour = "#D55E00", linewidth = 1) +
  geom_line(aes(y = tangent), linetype = 2, linewidth = 0.8) +
  geom_point(
    data = tangent_points,
    aes(x = x0, y = y0),
    inherit.aes = FALSE,
    size = 2
  ) +
  facet_wrap(~ panel, nrow = 1) +
  coord_cartesian(ylim = c(0, 9)) +
  labs(x = "x", y = "q(x)") +
  theme_bw(base_size = 14)

x1_grid_surface = seq(-3, 3, by = 0.05)
x2_grid_surface = seq(-3, 3, by = 0.05)
z_surface = outer(x1_grid_surface, x2_grid_surface, objective_example_1)
surface_colors = build_surface_colors(z_surface, n_colors = 100L)

x1_grid_contour = seq(-3, 3, by = 0.05)
x2_grid_contour = seq(-3, 3, by = 0.05)
contour_data = build_contour_grid(x1_grid_contour, x2_grid_contour, objective_example_1)

quadratic_2d_contour_plot = ggplot(contour_data, aes(x = x1, y = x2, z = z)) +
  geom_contour(colour = "#1F4E79", bins = 12) +
  geom_point(
    data = data.table(x1 = 0, x2 = 0),
    aes(x = x1, y = x2),
    inherit.aes = FALSE,
    colour = "#D55E00",
    size = 3
  ) +
  labs(x = expression(x[1]), y = expression(x[2])) +
  coord_fixed() +
  theme_minimal(base_size = 16)

ggsave("../figure/quadratic_functions_1D.png", quadratic_1d_plot, width = 6, height = 3)
ggsave("../figure/quadratic_functions_1D_curvature.png", curvature_plot, width = 6, height = 3)
ggsave("../figure/quadratic_functions_1D_derivative.png", derivative_plot, width = 12, height = 3)

png("../figure/quadratic_functions_2D_example_1_1.png", width = 800, height = 600)
persp(
  x1_grid_surface,
  x2_grid_surface,
  z_surface,
  col = surface_colors,
  phi = 30,
  theta = -20,
  border = NA,
  xlab = expression(x[1]),
  ylab = expression(x[2]),
  zlab = expression(q(x))
)
dev.off()

ggsave("../figure/quadratic_functions_2D_example_1_2.png", quadratic_2d_contour_plot, width = 3, height = 3)
