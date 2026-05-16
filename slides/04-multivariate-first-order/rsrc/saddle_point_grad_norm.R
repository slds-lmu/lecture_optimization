# Used in: slides-multivar-first-order-5-weaknesses-saddle.tex
#
# Plots the gradient norm along a gradient-descent path near a saddle point.

set.seed(1L)

library(ggplot2)

source("functions.R")

objective_fun = function(x, y) {
  x^2 - y^2
}

gradient_norm = function(x, y) {
  sqrt((2 * x)^2 + (2 * y)^2)
}

x = y = seq(-1.5, 1.5, length = 50L)
z = outer(x, y, objective_fun)
p = c(list(list(1, 0)), optim0(1, 0, FUN = objective_fun, maximum = FALSE, alpha = 0.2))

gradient_norm_data = data.frame(
  grad_norm = vapply(p, function(point) gradient_norm(point[[1]], point[[2]]), numeric(1L))
)
gradient_norm_data$iter = seq_len(nrow(gradient_norm_data))

gradient_norm_plot = ggplot(gradient_norm_data, aes(x = iter, y = grad_norm)) +
  geom_line() +
  theme_bw(base_size = 12)

if (interactive()) {
  print(gradient_norm_plot)
}

ggsave(
  filename = "../figure/saddle_point_grad_norm.pdf",
  plot = gradient_norm_plot,
  width = 9,
  height = 2.5
)
