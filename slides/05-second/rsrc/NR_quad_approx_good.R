# Used in: slides/05-second/01-nr.tex
#
# Benign counterpart to Figure 5.8 in Aggarwal (2020), where a far too flat
# quadratic model makes the Newton step overshoot and worsen the objective.
#
#   f(x) = x^2 / 2 + x^4 / 80,   x0 = 2,   x* = 0
#
# Here the model is too curved instead (f'' drops from 1.6 at x0 to 1 at x*),
# so it lies above the true function and the step stops short of the optimum
# -- but it still yields a large decrease (2.2 -> 0.13, better than the
# predicted 0.40), so no line search is needed.

library(ggplot2)

f <- function(x) x^2 / 2 + x^4 / 80
f_grad <- function(x) x + x^3 / 20
f_hess <- function(x) 1 + 3 * x^2 / 20

x0 <- 2
x_star <- 0

# second-order Taylor model around x0 and the resulting Newton step
quad <- function(x) f(x0) + f_grad(x0) * (x - x0) + 0.5 * f_hess(x0) * (x - x0)^2
x1 <- x0 - f_grad(x0) / f_hess(x0)

x_lim <- c(-1.1, 2.3)
y_lim <- c(-0.95, 3.4)

grid <- data.frame(x = seq(x_lim[1], x_lim[2], length.out = 601))
curves <- rbind(
  data.frame(x = grid$x, y = f(grid$x), fun = "true"),
  data.frame(x = grid$x, y = quad(grid$x), fun = "quad")
)

pts <- data.frame(
  x = c(x0, x1, x1),
  y = c(f(x0), f(x1), quad(x1))
)

label_size <- 3.3
arrow_style <- arrow(length = unit(0.02, "npc"))

p <- ggplot(curves, aes(x = x, y = y, linetype = fun, color = fun)) +
  geom_line(linewidth = 0.9) +
  scale_linetype_manual(values = c(true = "solid", quad = "dashed")) +
  scale_color_manual(values = c(true = "black", quad = "firebrick")) +
  # vertical drop from the bottom of the quadratic model to the true function
  annotate(
    "segment",
    x = x1,
    xend = x1,
    y = quad(x1),
    yend = f(x1),
    linetype = "dotted",
    linewidth = 0.5
  ) +
  # true minimizer for reference
  annotate(
    "segment",
    x = x_star,
    xend = x_star,
    y = f(x_star),
    yend = -0.62,
    linetype = "dotted",
    linewidth = 0.4,
    color = "grey40"
  ) +
  annotate(
    "text",
    x = x_star,
    y = -0.78,
    label = "true optimum",
    size = label_size - 0.4,
    color = "grey40"
  ) +
  geom_point(
    data = pts,
    aes(x = x, y = y),
    inherit.aes = FALSE,
    shape = 8,
    size = 2.1,
    stroke = 0.7
  ) +
  # annotations
  annotate(
    "text",
    x = 2.25,
    y = 3.15,
    label = "starting point",
    size = label_size,
    hjust = 1,
    fontface = "bold"
  ) +
  annotate(
    "segment",
    x = 2.15,
    xend = 2.03,
    y = 2.95,
    yend = 2.35,
    arrow = arrow_style,
    linewidth = 0.4
  ) +
  annotate(
    "text",
    x = -1.05,
    y = 3.15,
    label = "local quadratic approximation",
    size = label_size,
    hjust = 0,
    color = "firebrick"
  ) +
  annotate(
    "segment",
    x = -0.55,
    xend = -0.75,
    y = 2.95,
    yend = 1.8,
    arrow = arrow_style,
    linewidth = 0.4,
    color = "firebrick"
  ) +
  annotate(
    "text",
    x = 1.85,
    y = 2.5,
    label = "bottom of quadratic approximation",
    size = label_size - 0.15,
    hjust = 1,
    lineheight = 0.95,
    color = "firebrick"
  ) +
  annotate(
    "segment",
    x = 1.0,
    xend = x1 + 0.06,
    y = 2.3,
    yend = quad(x1) + 0.1,
    arrow = arrow_style,
    linewidth = 0.4,
    color = "firebrick"
  ) +
  annotate(
    "text",
    x = 2.25,
    y = -0.35,
    label = "Newton step reaches here",
    size = label_size,
    hjust = 1,
    fontface = "bold",
    lineheight = 0.95
  ) +
  annotate(
    "segment",
    x = 0.82,
    xend = x1 + 0.05,
    y = -0.33,
    yend = f(x1) - 0.08,
    arrow = arrow_style,
    linewidth = 0.4
  ) +
  annotate(
    "text",
    x = -1.05,
    y = -0.35,
    label = "true function",
    size = label_size,
    hjust = 0
  ) +
  annotate(
    "segment",
    x = -0.6,
    xend = -0.55,
    y = -0.22,
    yend = f(-0.55) - 0.09,
    arrow = arrow_style,
    linewidth = 0.4
  ) +
  coord_cartesian(xlim = x_lim, ylim = y_lim, expand = FALSE) +
  labs(x = "optimization variable", y = "objective function") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(linewidth = 0.4),
    axis.title = element_text(size = 12)
  )

ggsave(
  filename = "../figure/NR_quad_approx_good.png",
  plot = p,
  width = 4.5,
  height = 2.5,
  dpi = 300
)
