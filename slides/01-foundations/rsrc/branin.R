# Used in: ../01-diff.tex
#
# Visualize the modified Branin function on the unit square and mark several
# negative-gradient directions.

set.seed(1L)

library(data.table)
library(DiceKriging)
library(ggplot2)
library(grid)
library(numDeriv)

dir.create("../figure", recursive = TRUE, showWarnings = FALSE)

x1_grid = seq(0, 1, length.out = 200L)
x2_grid = seq(0, 1, length.out = 200L)
grid_data = CJ(x1 = x1_grid, x2 = x2_grid)
grid_matrix = as.matrix(grid_data[, .(x1, x2)])
grid_data[, z := apply(grid_matrix, 1L, branin)]

points_data = data.table(
  x1 = c(0.35, 0.10, 0.50, 0.05, 0.80, 0.55, 0.20),
  x2 = c(0.40, 0.40, 0.70, 0.05, 0.80, 0.18, 0.80)
)

gradient_values = t(vapply(
  seq_len(nrow(points_data)),
  function(i) grad(branin, as.numeric(points_data[i, .(x1, x2)])),
  numeric(2L)
))
negative_gradient = -gradient_values / 2000

points_data[, `:=`(
  xend = x1 + negative_gradient[, 1],
  yend = x2 + negative_gradient[, 2]
)]

branin_plot = ggplot(grid_data, aes(x = x1, y = x2, z = z)) +
  geom_contour(bins = 40, colour = "grey30") +
  geom_point(
    data = points_data,
    aes(x = x1, y = x2),
    inherit.aes = FALSE,
    colour = "#D55E00",
    size = 2.5
  ) +
  geom_segment(
    data = points_data,
    aes(x = x1, y = x2, xend = xend, yend = yend),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(0.12, "inches")),
    linewidth = 0.7
  ) +
  labs(
    title = "Modified Branin function with negative gradients",
    x = expression(x[1]),
    y = expression(x[2])
  ) +
  coord_fixed() +
  theme_bw(base_size = 14)

ggsave("../figure/branin.png", branin_plot, width = 7, height = 5)
