# Used in: slides/03-univariate-optimization/slides-univariate-3-further.tex
#
# Two iterations of the secant method applied to f' in a single plot.

library(ggplot2)

dir.create("../figure", recursive = TRUE, showWarnings = FALSE)

palette = list(
  curve = "black",
  secant1 = "#C84B6C",
  secant2 = "#7C3AED",
  points = "#2563EB",
  zeroline = "#D1D5DB"
)

f_prime = function(x) 0.9 * x^2 - 2 * x - 1

secant_step = function(xa, xb) {
  fa = f_prime(xa)
  fb = f_prime(xb)
  xb - fb * (xb - xa) / (fb - fa)
}

x1 = 1.0
x2 = 4.0
x3 = secant_step(x1, x2) # ≈ 1.667
x4 = secant_step(x2, x3) # ≈ 2.091

make_secant_fn = function(xa, xb) {
  fa = f_prime(xa)
  fb = f_prime(xb)
  slope = (fb - fa) / (xb - xa)
  intercept = fa - slope * xa
  function(x) slope * x + intercept
}

secant1 = make_secant_fn(x1, x2)
secant2 = make_secant_fn(x2, x3)

x_range = c(0.0, 5.0)
y_lim = c(-3.5, 9.5)

p = ggplot() +
  geom_hline(yintercept = 0, colour = palette$zeroline, linewidth = 0.5) +
  # f' curve
  stat_function(fun = f_prime, xlim = x_range, linewidth = 0.9) +
  # Step 1 secant: through (x1, f'(x1)) and (x2, f'(x2))
  stat_function(fun = secant1, xlim = c(0.7, 4.3), colour = palette$secant1, linetype = "dashed", linewidth = 0.9) +
  # Step 2 secant: through (x2, f'(x2)) and (x3, f'(x3))
  stat_function(fun = secant2, xlim = c(1.4, 4.3), colour = palette$secant2, linetype = "dashed", linewidth = 0.9) +
  # Drop lines for evaluation points
  geom_segment(
    aes(x = x1, xend = x1, y = f_prime(x1), yend = 0),
    colour = palette$points,
    linetype = "dotted",
    linewidth = 0.6
  ) +
  geom_segment(
    aes(x = x2, xend = x2, y = f_prime(x2), yend = 0),
    colour = palette$points,
    linetype = "dotted",
    linewidth = 0.6
  ) +
  geom_segment(
    aes(x = x3, xend = x3, y = f_prime(x3), yend = 0),
    colour = palette$points,
    linetype = "dotted",
    linewidth = 0.6
  ) +
  # Points on f' curve
  geom_point(aes(x = x1, y = f_prime(x1)), colour = palette$points, size = 2.5) +
  geom_point(aes(x = x2, y = f_prime(x2)), colour = palette$points, size = 2.5) +
  geom_point(aes(x = x3, y = f_prime(x3)), colour = palette$points, size = 2.5) +
  # New proposals on x-axis
  geom_point(aes(x = x3, y = 0), colour = palette$secant1, size = 3) +
  geom_point(aes(x = x4, y = 0), colour = palette$secant2, size = 3) +
  # Labels on the f' curve
  annotate(
    "text",
    x = x1 + 0.3,
    y = f_prime(x1) - 0.5,
    label = "x[1]",
    colour = palette$points,
    size = 3.5,
    parse = TRUE,
    hjust = 1
  ) +
  annotate(
    "text",
    x = x2 + 0.1,
    y = f_prime(x2) - 0.1,
    label = "x[2]",
    colour = palette$points,
    size = 3.5,
    parse = TRUE,
    hjust = 0
  ) +
  # Labels for new proposals below x-axis
  annotate(
    "text",
    x = x3 - 0.15,
    y = 0,
    label = "x[3]",
    colour = palette$secant1,
    size = 3.5,
    parse = TRUE,
    hjust = 1
  ) +
  annotate(
    "text",
    x = x4 - 0.25,
    y = 0,
    label = "x[4]",
    colour = palette$secant2,
    size = 3.5,
    parse = TRUE
  ) +
  coord_cartesian(xlim = x_range, ylim = y_lim) +
  labs(x = "x", y = "f'(x)") +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave("../figure/secant.pdf", plot = p, width = 4, height = 3)
