# Used in: slides/11-multicrit/slides-multicrit-3-bo.tex
#
# Visualizes the augmented Chebyshev scalarization used by ParEGO for different
# weight vectors and overlays the Pareto front of the toy objectives.

library(data.table)
library(ggplot2)
library(patchwork)

set.seed(1L)

output_dir = "../figure"
resolution = 160L
rho = 0.05

theme_set(theme_bw(base_size = 11))

f1 = function(x) {
  (x - 1)^2
}

f2 = function(x) {
  3 * (x - 2)^2
}

augmented_chebyshev = function(y1, y2, w1, w2, rho = 0.05) {
  pmax(w1 * y1, w2 * y2) + rho * (w1 * y1 + w2 * y2)
}

plot_scalarization = function(w1, w2, grid, pareto_front) {
  plot_grid = copy(grid)
  plot_grid[, scalarized_value := augmented_chebyshev(f1, f2, w1 = w1, w2 = w2, rho = rho)]
  front_optimum = copy(pareto_front)
  front_optimum[, scalarized_value := augmented_chebyshev(f1, f2, w1 = w1, w2 = w2, rho = rho)]
  front_optimum = front_optimum[which.min(scalarized_value)]

  ggplot(plot_grid, aes(x = f1, y = f2, fill = scalarized_value, z = scalarized_value)) +
    geom_raster() +
    geom_contour(
      data = plot_grid,
      aes(x = f1, y = f2, z = scalarized_value),
      inherit.aes = FALSE,
      color = "grey20",
      bins = 35,
      linewidth = 0.15
    ) +
    geom_path(data = pareto_front, aes(x = f1, y = f2), inherit.aes = FALSE, color = "#00A087", linewidth = 1) +
    geom_point(
      data = front_optimum,
      aes(x = f1, y = f2),
      inherit.aes = FALSE,
      shape = 21,
      color = "black",
      fill = "white",
      stroke = 0.7,
      size = 2.4
    ) +
    scale_fill_gradientn(colors = c("#FDE725", "#F8961E", "#D62828")) +
    labs(
      x = expression(f[1]),
      y = expression(f[2]),
      fill = "Scalarized\nvalue",
      title = bquote(w[1] == .(w1) * "," ~ w[2] == .(w2))
    ) +
    theme(legend.position = "none")
}

objective_grid = CJ(
  f1 = seq(0, 4, length.out = resolution),
  f2 = seq(0, 12, length.out = resolution)
)

front_x = seq(1, 2, length.out = resolution)
pareto_front = data.table(f1 = f1(front_x), f2 = f2(front_x))
setorder(pareto_front, f1)

parego_plot = plot_scalarization(0.90, 0.10, objective_grid, pareto_front) +
  plot_scalarization(0.49, 0.51, objective_grid, pareto_front) +
  plot_scalarization(0.10, 0.90, objective_grid, pareto_front) +
  plot_layout(nrow = 1L)

if (interactive()) {
  print(parego_plot)
}

ggsave(file.path(output_dir, "parego_viz.png"), parego_plot, width = 9, height = 3, dpi = 300)
