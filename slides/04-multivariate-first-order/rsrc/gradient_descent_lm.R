# Used in: slides-multivar-first-order-1-GD.tex
#
# Simulates gradient descent for a simple linear model and visualizes
# the RSS trajectory together with the fitted lines across iterations.

set.seed(1L)

library(ggplot2)
library(patchwork)

x = runif(25L, 0, 2)
design_matrix = cbind(1, x)
response = 1 + 2 * x + rnorm(length(x), sd = 1)

rss = function(theta) {
  sum((response - design_matrix %*% theta)^2)
}

rss_gradient = function(theta) {
  drop(-2 * crossprod(design_matrix, response - design_matrix %*% theta))
}

run_gradient_descent = function(theta0, step_size, n_steps) {
  trajectory = vector("list", length = n_steps + 1L)
  theta = theta0

  trajectory[[1L]] = data.frame(
    iteration = 0L,
    theta0 = theta[1],
    theta1 = theta[2],
    rss = rss(theta)
  )

  for (iter in seq_len(n_steps)) {
    gradient = rss_gradient(theta)
    theta = theta - step_size * gradient / sqrt(sum(gradient^2))

    trajectory[[iter + 1L]] = data.frame(
      iteration = iter,
      theta0 = theta[1],
      theta1 = theta[2],
      rss = rss(theta)
    )
  }

  do.call(rbind, trajectory)
}

trajectory = run_gradient_descent(theta0 = c(0, 0), step_size = 0.1, n_steps = 30L)

data_plot = ggplot(data.frame(x = x, y = response), aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw(base_size = 12)

rss_plot = ggplot(trajectory, aes(x = iteration, y = rss)) +
  geom_line() +
  labs(x = "Iteration", y = "RSS") +
  theme_bw(base_size = 12)

fit_plot = data_plot +
  geom_abline(
    data = trajectory,
    aes(intercept = theta0, slope = theta1, colour = iteration),
    alpha = 0.5
  )

combined_plot = (rss_plot + fit_plot) +
  plot_layout(nrow = 1, guides = "collect") &
  theme(legend.position = "right")

if (interactive()) {
  print(combined_plot)
}

ggsave(
  filename = "../figure/gradient_descent_lm.pdf",
  plot = combined_plot,
  width = 7,
  height = 2.5
)
