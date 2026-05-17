# Used in: slides-multivar-first-order-2-stepsize.tex
#
# Compares fixed and adaptive step sizes for gradient descent on a one-dimensional
# objective with a flat tail around the optimum.

set.seed(1L)

library(ggplot2)
library(patchwork)
library(rootSolve)

objective_fun = function(x, delta = 0.05) {
  ifelse(abs(x) <= delta, 0.5 * x^2, delta * (abs(x) - 0.5 * delta))
}

run_gradient_descent = function(x0, step_size_fun, n_steps = 20L, method_label) {
  path = vector("list", length = n_steps + 1L)
  path[[1L]] = data.frame(iteration = 0L, x = x0, objective = objective_fun(x0))

  current_x = x0
  for (iter in seq_len(n_steps)) {
    current_gradient = gradient(objective_fun, current_x)
    current_x = current_x - step_size_fun(iter) * current_gradient
    path[[iter + 1L]] = data.frame(
      iteration = iter,
      x = current_x,
      objective = objective_fun(current_x)
    )
  }

  path = do.call(rbind, path)
  path$method = method_label
  path
}

objective_curve = ggplot(data.frame(x = c(-1, 1)), aes(x = x)) +
  stat_function(fun = objective_fun) +
  labs(x = "x", y = "f(x)") +
  theme_bw(base_size = 12)

comparison_data = do.call(
  rbind,
  list(
    run_gradient_descent(x0 = -0.8, step_size_fun = function(iter) 0.5, method_label = "α = 0.5"),
    run_gradient_descent(x0 = 0.8, step_size_fun = function(iter) 10, method_label = "α = 10"),
    run_gradient_descent(x0 = 0.8, step_size_fun = function(iter) 10 / iter, method_label = "α[t] = 10 / t")
  )
)

trajectory_plot = objective_curve +
  geom_point(data = comparison_data, aes(x = x, y = objective, colour = method)) +
  geom_line(data = comparison_data, aes(x = x, y = objective, colour = method))

loss_plot = ggplot(comparison_data, aes(x = iteration, y = objective, colour = method)) +
  geom_line() +
  labs(x = "Iteration", y = "f(x)") +
  theme_bw(base_size = 12)

combined_plot = (trajectory_plot + loss_plot) +
  plot_layout(nrow = 1, guides = "collect") &
  theme(legend.position = "right")

if (interactive()) {
  print(combined_plot)
}

ggsave(
  filename = "../figure/fixed_vs_adaptive.pdf",
  plot = combined_plot,
  width = 9,
  height = 2.5
)
