# Used in: slides/05-multivariate-second-order/slides-multivar-second-order-3-gauss-newton.tex
#
# Fits an exponential curve by nonlinear least squares and shows the observed
# values, fitted values, and residual segments used to motivate least-squares
# objectives before introducing Gauss-Newton.

set.seed(1L)

suppressPackageStartupMessages(library(ggplot2))

observations = data.frame(
  x = c(1, 2, 4, 5, 8),
  y = c(3, 5, 6, 13, 20)
)

fit = nls(
  y ~ a * exp(b * x),
  data = observations,
  start = list(a = 1, b = 0.2)
)

plot_data = transform(observations, fitted = as.numeric(predict(fit)))
curve_data = data.frame(x = seq(min(observations$x), max(observations$x), length.out = 200L))
curve_data$fitted = as.numeric(predict(fit, newdata = curve_data))

squares_plot = ggplot(plot_data, aes(x = x, y = y)) +
  geom_segment(aes(xend = x, yend = fitted), color = "#D55E00", linewidth = 0.7) +
  geom_line(data = curve_data, aes(y = fitted), color = "black", linewidth = 0.7) +
  geom_point(size = 2.2) +
  geom_point(aes(y = fitted), color = "#0072B2", size = 2.2) +
  labs(x = "x", y = "y") +
  theme_bw(base_size = 12)

if (interactive()) {
  print(squares_plot)
}

ggsave(
  filename = "../figure/squares.png",
  plot = squares_plot,
  width = 4,
  height = 3,
  dpi = 300
)
