# Used in: slides/05-multivariate-second-order/slides-multivar-second-order-1b-limitations.tex
#
# Saddle-point problem of Newton-Raphson (Aggarwal Ch. 5.6.2).
#
# NR_saddle_2d.png: the 2d saddle g(x1, x2) = x1^2 - x2^2 with H = diag(2, -2).
#   Newton-Raphson solves grad g = 0 and therefore jumps onto the saddle at the
#   origin in a single step from any starting point (g is quadratic, so the
#   quadratic model is exact). Gradient descent is repelled along the negative
#   curvature direction x2 and escapes.
# NR_saddle_1d.png: the 1d inflection point of f(x) = x^3. The second-order
#   Taylor model is an upright bowl at x = 1 and an inverted bowl at x = -1, so
#   the Newton step points towards a minimum in one case and towards a maximum
#   in the other. At x = 0 both f' and f'' vanish and the update becomes 0/0.

library(ggplot2)

method_colors <- c("Newton-Raphson" = "firebrick", "Gradient Descent" = "steelblue")

# ---------------------------------------------------------------------------
# 2d saddle
# ---------------------------------------------------------------------------

g <- function(x1, x2) x1^2 - x2^2

grid <- expand.grid(
  x1 = seq(-2, 2, length.out = 300),
  x2 = seq(-2, 2, length.out = 300)
)
grid$z <- g(grid$x1, grid$x2)

x_start <- c(1.5, 0.25)

# Gradient descent: x1 shrinks, x2 grows in absolute value -> escapes saddle
lr <- 0.12
gd_path <- matrix(NA_real_, nrow = 13L, ncol = 2L)
gd_path[1L, ] <- x_start
for (t in seq_len(nrow(gd_path) - 1L)) {
  x <- gd_path[t, ]
  gd_path[t + 1L, ] <- x - lr * c(2 * x[1L], -2 * x[2L])
}
gd_path <- as.data.frame(gd_path)
names(gd_path) <- c("x1", "x2")
gd_path$method <- "Gradient Descent"
gd_path <- gd_path[abs(gd_path$x1) <= 2 & abs(gd_path$x2) <= 2, ]

# Newton-Raphson: d = -H^{-1} grad g = -(x1, x2) -> lands on the saddle
nr_path <- data.frame(
  x1 = c(x_start[1L], 0),
  x2 = c(x_start[2L], 0),
  method = "Newton-Raphson"
)

paths <- rbind(nr_path, gd_path)

saddle_plot <- ggplot(grid, aes(x = x1, y = x2)) +
  geom_raster(aes(fill = z), interpolate = TRUE) +
  geom_contour(aes(z = z), colour = "white", alpha = 0.35, bins = 18) +
  geom_path(
    data = paths,
    aes(colour = method),
    linewidth = 0.9,
    arrow = arrow(length = unit(0.18, "cm"), type = "closed")
  ) +
  geom_point(data = paths, aes(colour = method), size = 1.9) +
  annotate("point", x = 0, y = 0, shape = 4, size = 4, stroke = 1.4, colour = "black") +
  annotate("text", x = -0.05, y = -0.3, label = "saddle", hjust = 1, size = 4.6) +
  scale_fill_viridis_c(option = "viridis", guide = "none") +
  scale_colour_manual(values = method_colors, name = NULL) +
  coord_fixed(xlim = c(-2, 2), ylim = c(-2, 2), expand = FALSE) +
  labs(
    subtitle = expression(g(x[1], x[2]) == x[1]^2 - x[2]^2),
    x = expression(x[1]),
    y = expression(x[2])
  ) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "right", plot.margin = margin(2, 2, 2, 2))

ggsave(
  filename = "../figure/NR_saddle_2d.png",
  plot = saddle_plot,
  width = 7.4,
  height = 4.2,
  dpi = 300
)

# ---------------------------------------------------------------------------
# 1d inflection point of f(x) = x^3
# ---------------------------------------------------------------------------

f <- function(x) x^3
# 2nd order Taylor models at x = 1 (upright bowl) and x = -1 (inverted bowl)
model_at_pos <- function(x) 3 * x^2 - 3 * x + 1
model_at_neg <- function(x) -3 * x^2 - 3 * x - 1

xs <- seq(-1.7, 1.7, length.out = 400)
curves <- rbind(
  data.frame(x = xs, y = f(xs), which = "f(x) = x^3"),
  data.frame(x = xs, y = model_at_pos(xs), which = "model at x = 1: convex, step -> min"),
  data.frame(x = xs, y = model_at_neg(xs), which = "model at x = -1: concave, step -> max")
)
curves$which <- factor(
  curves$which,
  levels = c(
    "f(x) = x^3",
    "model at x = 1: convex, step -> min",
    "model at x = -1: concave, step -> max"
  )
)

expansion_points <- data.frame(x = c(1, -1), y = f(c(1, -1)))

taylor_plot <- ggplot(curves, aes(x = x, y = y, colour = which, linetype = which)) +
  geom_hline(yintercept = 0, colour = "grey80") +
  geom_vline(xintercept = 0, colour = "grey80") +
  geom_line(linewidth = 0.9) +
  geom_point(data = expansion_points, aes(x = x, y = y), inherit.aes = FALSE, size = 2.4) +
  annotate("point", x = 0, y = 0, shape = 4, size = 4, stroke = 1.4, colour = "black") +
  annotate("text", x = 0.12, y = -1.1, label = "degenerate: f' = f'' = 0", hjust = 0, size = 4.4) +
  scale_colour_manual(values = c("black", "firebrick", "darkorange"), name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed", "dashed"), name = NULL) +
  coord_cartesian(ylim = c(-4, 4)) +
  labs(x = "x", y = NULL) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0),
    plot.margin = margin(2, 6, 2, 2)
  )

ggsave(
  filename = "../figure/NR_saddle_1d.png",
  plot = taylor_plot,
  width = 7.4,
  height = 4.2,
  dpi = 300
)
