# Used in: slides-multivar-first-order-2-stepsize.tex
#
# Plots the Armijo sufficient-decrease condition along a one-dimensional line search.

set.seed(1L)

library(ggplot2)

objective_fun = function(alpha) {
  -alpha^3 + 6 * alpha^2 - 9 * alpha + 2
}

tangent_fun = function(alpha) {
  -9 * alpha + 2
}

armijo_fun = function(alpha, gamma1 = 0.05) {
  gamma1 * (-9) * alpha + 2
}

armijo_limit = uniroot(
  function(alpha) objective_fun(alpha) - armijo_fun(alpha),
  interval = c(1, 3)
)$root

curve_data = data.frame(
  alpha = seq(0, armijo_limit, by = 0.01),
  value = objective_fun(seq(0, armijo_limit, by = 0.01))
)

armijo_plot = ggplot(data.frame(alpha = 0), aes(x = alpha)) +
  geom_point(aes(x = 0, y = 2)) +
  geom_text(aes(x = 0, y = 2.1, label = "f(x)"), parse = TRUE) +
  stat_function(fun = objective_fun) +
  geom_text(aes(x = 3, y = 2.2, label = "f(x + alpha~d)"), parse = TRUE) +
  stat_function(fun = tangent_fun, colour = "grey40") +
  geom_text(aes(x = 0.1, y = -1.3, label = "Tangent"), colour = "grey40") +
  geom_text(
    aes(x = 0.1, y = -1.7, label = "alpha~nabla~f(x)^T~d"),
    parse = TRUE,
    colour = "grey40"
  ) +
  stat_function(fun = armijo_fun, colour = "#0b61a4", linetype = "dashed") +
  geom_text(aes(x = 1.5, y = 2, label = "Armijo bound"), colour = "#0b61a4") +
  geom_text(
    aes(x = 1.5, y = 1.7, label = "gamma[1]~alpha~nabla~f(x)^T~d"),
    parse = TRUE,
    colour = "#0b61a4"
  ) +
  geom_line(data = curve_data, aes(x = alpha, y = value), colour = "#c0392b") +
  geom_text(aes(x = 1, y = -1, label = "Armijo rule holds"), colour = "#c0392b") +
  labs(x = expression(alpha), y = "y") +
  coord_cartesian(xlim = c(0, 4), ylim = c(-2, 3)) +
  theme_bw(base_size = 12)

if (interactive()) {
  print(armijo_plot)
}

ggsave(
  filename = "../figure/armijo.pdf",
  plot = armijo_plot,
  width = 6,
  height = 4
)
