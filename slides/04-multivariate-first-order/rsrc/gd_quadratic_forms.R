# Used in: slides-multivar-first-order-7-gd-quadratic-forms.tex
#
# Generates contour plots of q(x) = x^T A x - b^T x with A = [[2,1],[1,2]],
# b = (6, 6), minimum at x* = (1, 1), in original x-space (tilted ellipses)
# and eigenspace w (axis-aligned ellipses, min at 0).
# Produces:
#   figure/gd_quadratic_forms_xspace.pdf
#   figure/gd_quadratic_forms_wspace.pdf

library(ggplot2)

set.seed(123L)

A = matrix(c(2, 1, 1, 2), nrow = 2L)
b = c(6, 6)  # minimum at x* = 0.5 * A^{-1} b = (1, 1)
lambdas = sort(eigen(A)$values)  # c(1, 3), ascending: lambda_1 <= ... <= lambda_n

n = 300L
lim = 4

grid_x = expand.grid(
  x1 = seq(-lim, lim, length.out = n),
  x2 = seq(-lim, lim, length.out = n)
)
grid_x$z = A[1L, 1L] * grid_x$x1^2 + 2 * A[1L, 2L] * grid_x$x1 * grid_x$x2 +
  A[2L, 2L] * grid_x$x2^2 - b[1L] * grid_x$x1 - b[2L] * grid_x$x2

grid_w = expand.grid(
  w1 = seq(-lim, lim, length.out = n),
  w2 = seq(-lim, lim, length.out = n)
)
grid_w$z = lambdas[1L] * grid_w$w1^2 + lambdas[2L] * grid_w$w2^2

make_contour_plot = function(grid, xvar, yvar, xlab, ylab) {
  ggplot(grid, aes(x = .data[[xvar]], y = .data[[yvar]], z = z)) +
    geom_contour(aes(color = after_stat(level)), bins = 20L) +
    scale_color_viridis_c() +
    guides(color = "none") +
    labs(x = xlab, y = ylab) +
    theme_bw()
}

p_x = make_contour_plot(grid_x, "x1", "x2", expression(x[1]), expression(x[2]))
p_w = make_contour_plot(grid_w, "w1", "w2", expression(w[1]), expression(w[2]))

ggsave("../figure/gd_quadratic_forms_xspace.pdf", plot = p_x, width = 4, height = 3)
ggsave("../figure/gd_quadratic_forms_wspace.pdf", plot = p_w, width = 4, height = 3)
