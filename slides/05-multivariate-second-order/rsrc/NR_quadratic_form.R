# Used in: slides/05-multivariate-second-order/slides-multivar-second-order-1-newton-raphson.tex
#
# Analytical example of the deck, visualized:
#
#   f(x1, x2) = x1^2 + x2^2 / 2,   x0 = (2, 2),   x* = (0, 0)
#
# The Newton direction -H^-1 grad f points straight at the center of the
# elliptical contours, so a single step lands exactly on x*, while the
# steepest-descent direction (drawn with the same length) misses it: the
# inverse Hessian rescales the gradient per coordinate.

library(ggplot2)

f <- function(x1, x2) x1^2 + x2^2 / 2
f_grad <- function(x) c(2 * x[1], x[2])
f_hess <- matrix(c(2, 0, 0, 1), nrow = 2)

x0 <- c(2, 2)
newton_step <- -solve(f_hess, f_grad(x0))
x1_new <- x0 + newton_step

# negative gradient at its true length: both steps move by -2 in x2 (scaled by
# 1 by H^-1), while the Newton step moves half as far in x1 (scaled by 1/2),
# so both arrows end on the line x2 = 0
sd_end <- x0 - f_grad(x0)

grid <- expand.grid(
  x1 = seq(-3, 3, length.out = 241),
  x2 = seq(-3.2, 3.2, length.out = 241)
)
grid$z <- f(grid$x1, grid$x2)

# trace colors of the chapter (see NR.R)
col_newton <- "#ff6262"
col_sd <- "#ffcc00"
arrow_style <- arrow(length = unit(0.09, "inches"))
label_size <- 4.2

p <- ggplot(grid, aes(x = x1, y = x2, z = z)) +
  geom_raster(aes(fill = z), interpolate = TRUE) +
  geom_contour(color = "grey20", alpha = 0.4, linewidth = 0.3, bins = 14) +
  scale_fill_viridis_c() +
  guides(fill = "none") +
  annotate(
    "segment",
    x = x0[1],
    y = x0[2],
    xend = sd_end[1],
    yend = sd_end[2],
    arrow = arrow_style,
    linewidth = 0.8,
    color = col_sd
  ) +
  annotate(
    "segment",
    x = x0[1],
    y = x0[2],
    xend = x1_new[1] + 0.07 * (x0[1] - x1_new[1]),
    yend = x1_new[2] + 0.07 * (x0[2] - x1_new[2]),
    arrow = arrow_style,
    linewidth = 0.8,
    color = col_newton
  ) +
  annotate("point", x = x0[1], y = x0[2], size = 2, color = "white") +
  annotate("point", x = 0, y = 0, shape = 8, size = 2.4, stroke = 0.9, color = "white") +
  annotate(
    "text",
    x = x0[1] - 0.24,
    y = x0[2] + 0.26,
    label = "x^{'[0]'}",
    parse = TRUE,
    size = label_size,
    color = "white"
  ) +
  annotate(
    "text",
    x = 0.28,
    y = -0.3,
    label = "x^'*'",
    parse = TRUE,
    size = label_size,
    color = "white"
  ) +
  annotate(
    "text",
    x = 1.22,
    y = 0.72,
    label = "-H^{-1} * nabla * f",
    parse = TRUE,
    size = label_size,
    hjust = 0,
    color = col_newton
  ) +
  annotate(
    "text",
    x = -0.15,
    y = 1.42,
    label = "-nabla * f",
    parse = TRUE,
    size = label_size,
    hjust = 0.5,
    color = col_sd
  ) +
  coord_fixed(xlim = c(-3, 3), ylim = c(-3.2, 3.2), expand = FALSE) +
  labs(x = expression(x[1]), y = expression(x[2])) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave(
  filename = "../figure/NR_quadratic_form.png",
  plot = p,
  width = 3.6,
  height = 3.6,
  dpi = 300
)
