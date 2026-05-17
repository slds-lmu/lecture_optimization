# Used in: ../04-optcond.tex
#
# Create a contour plot and a 3D surface plot of the Branin function together
# with its three local minima.

set.seed(1L)

library(data.table)
library(ggplot2)

dir.create("../figure", recursive = TRUE, showWarnings = FALSE)

branin_surface = function(x1, x2) {
  a = 1
  b = 5.1 / (4 * pi^2)
  c = 5 / pi
  r = 6
  s = 10
  t = 1 / (8 * pi)

  a * (x2 - b * x1^2 + c * x1 - r)^2 + s * (1 - t) * cos(x1) + s
}

build_surface_colors = function(z_matrix, n_colors) {
  n_rows = nrow(z_matrix)
  n_cols = ncol(z_matrix)
  z_facet = z_matrix[-1, -1] + z_matrix[-1, -n_cols] +
    z_matrix[-n_rows, -1] + z_matrix[-n_rows, -n_cols]
  palette = colorRampPalette(c("blue", "green", "yellow", "orange", "red"))(n_colors)

  palette[cut(z_facet, n_colors)]
}

minima_data = data.table(
  x1 = c(-pi, pi, 3 * pi),
  x2 = c(12.275, 2.275, 2.475)
)
minima_data[, z := branin_surface(x1, x2)]

x1_grid_2d = seq(-5, 10, by = 0.1)
x2_grid_2d = seq(0, 15, by = 0.1)
contour_data = CJ(x1 = x1_grid_2d, x2 = x2_grid_2d)
contour_data[, z := branin_surface(x1, x2)]

branin_contour_plot = ggplot(contour_data, aes(x = x1, y = x2, z = z)) +
  geom_contour_filled(bins = 12) +
  geom_point(
    data = minima_data,
    aes(x = x1, y = x2),
    inherit.aes = FALSE,
    colour = "#D55E00",
    size = 2.5
  ) +
  labs(
    x = expression(x[1]),
    y = expression(x[2]),
    fill = "f(x)"
  ) +
  theme_bw(base_size = 14)

x1_grid_3d = seq(-5, 10, by = 0.5)
x2_grid_3d = seq(0, 15, by = 0.5)
z_surface = outer(x1_grid_3d, x2_grid_3d, branin_surface)
surface_colors = build_surface_colors(z_surface, n_colors = 100L)

ggsave("../figure/branin3d_2d.pdf", branin_contour_plot, width = 6, height = 4)

pdf("../figure/branin3d_3d.pdf", width = 5, height = 7.5, colormodel = "cmyk")
par(xaxs = "i", yaxs = "i")

perspective = persp(
  x1_grid_3d,
  x2_grid_3d,
  z_surface,
  col = surface_colors,
  theta = 15,
  phi = 20,
  xlab = expression(x[1]),
  ylab = expression(x[2]),
  zlab = expression(f(x))
)

projected_minima = trans3d(
  minima_data[["x1"]],
  minima_data[["x2"]],
  minima_data[["z"]],
  pmat = perspective
)
points(projected_minima, pch = 16, col = 2)

dev.off()
