# Used in: ../01-diff.tex
#
# Create a 3D surface plot and a contour plot for a bivariate function whose
# Hessian highlights positive, negative, and mixed curvature.

set.seed(1L)

library(data.table)
library(ggplot2)
library(grid)

dir.create("../figure", recursive = TRUE, showWarnings = FALSE)

objective = function(x1, x2) {
  sin(x1) * cos(2 * x2)
}

hessian_matrix = function(x1, x2) {
  matrix(
    c(
      -sin(x1) * cos(2 * x2), -2 * cos(x1) * sin(2 * x2),
      -2 * cos(x1) * sin(2 * x2), -4 * sin(x1) * cos(2 * x2)
    ),
    nrow = 2L,
    byrow = TRUE
  )
}

build_surface_colors = function(z_matrix, n_colors) {
  n_rows = nrow(z_matrix)
  n_cols = ncol(z_matrix)
  z_facet = z_matrix[-1, -1] + z_matrix[-1, -n_cols] +
    z_matrix[-n_rows, -1] + z_matrix[-n_rows, -n_cols]
  palette = colorRampPalette(c("navy", "skyblue", "gold", "darkorange", "firebrick"))(n_colors)

  palette[cut(z_facet, n_colors)]
}

build_eigenvector_segments = function(label, x1, x2, scale = 0.7) {
  eigen_decomposition = eigen(hessian_matrix(x1, x2))

  rbindlist(list(
    data.table(
      label = label,
      x = x1,
      y = x2,
      xend = x1 + scale * eigen_decomposition$vectors[1, 1],
      yend = x2 + scale * eigen_decomposition$vectors[2, 1]
    ),
    data.table(
      label = label,
      x = x1,
      y = x2,
      xend = x1 + scale * eigen_decomposition$vectors[1, 2],
      yend = x2 + scale * eigen_decomposition$vectors[2, 2]
    )
  ))
}

point_data = data.table(
  label = c("a", "b", "c"),
  x1 = c(-pi / 2, 0, pi / 2),
  x2 = c(0, pi / 4, 0),
  colour = c("limegreen", "purple", "orange")
)

arrow_data = rbindlist(lapply(
  seq_len(nrow(point_data)),
  function(i) build_eigenvector_segments(point_data[["label"]][i], point_data[["x1"]][i], point_data[["x2"]][i])
))

x1_grid = seq(-pi, pi, length.out = 200L)
x2_grid = seq(-pi / 2, pi / 2, length.out = 200L)
contour_data = CJ(x1 = x1_grid, x2 = x2_grid)
contour_data[, z := objective(x1, x2)]

hessian_contour_plot = ggplot(contour_data, aes(x = x1, y = x2, z = z)) +
  geom_contour_filled(bins = 10) +
  geom_segment(
    data = arrow_data,
    aes(x = x, y = y, xend = xend, yend = yend),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(0.1, "inches")),
    linewidth = 0.7,
    colour = "black"
  ) +
  geom_text(
    data = point_data,
    aes(x = x1, y = x2, label = label, colour = colour),
    inherit.aes = FALSE,
    size = 6,
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  labs(x = expression(x[1]), y = expression(x[2]), fill = "f(x)") +
  coord_fixed() +
  theme_minimal(base_size = 16)

z_surface = outer(x1_grid, x2_grid, objective)
surface_colors = build_surface_colors(z_surface, n_colors = 100L)
point_data[, z := objective(x1, x2)]

ggsave("../figure/hessian_contour.png", hessian_contour_plot, width = 6.5, height = 5)

png("../figure/hessian_3d.png", width = 900, height = 650)
perspective = persp(
  x1_grid,
  x2_grid,
  z_surface,
  col = surface_colors,
  theta = 35,
  phi = 25,
  expand = 0.6,
  border = NA,
  xlab = expression(x[1]),
  ylab = expression(x[2]),
  zlab = expression(f(x))
)

projected_points = trans3d(point_data[["x1"]], point_data[["x2"]], point_data[["z"]], pmat = perspective)
points(projected_points, pch = 16, col = c("limegreen", "purple", "orange"))
text(projected_points, labels = point_data[["label"]], pos = 3)

dev.off()
