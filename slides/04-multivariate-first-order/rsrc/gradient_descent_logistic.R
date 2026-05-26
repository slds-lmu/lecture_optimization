# Used in: slides-multivar-first-order-1-GD.tex
#
# Simulates gradient descent for logistic regression and visualizes
# the cross-entropy trajectory together with evolving decision boundaries.

set.seed(1L)

library(ggplot2)
library(patchwork)
library(ggnewscale)

n = 80L
x1 = c(rnorm(n / 2, mean = -1), rnorm(n / 2, mean = 1))
x2 = c(rnorm(n / 2, mean = -1), rnorm(n / 2, mean = 1))
y = c(rep(0L, n / 2), rep(1L, n / 2))

design_matrix = cbind(1, x1, x2)

sigmoid = function(z) 1 / (1 + exp(-z))

cross_entropy = function(theta) {
  pi = sigmoid(design_matrix %*% theta)
  pi = pmax(pmin(pi, 1 - 1e-15), 1e-15)
  -sum(y * log(pi) + (1 - y) * log(1 - pi))
}

ce_gradient = function(theta) {
  pi = sigmoid(design_matrix %*% theta)
  drop(crossprod(design_matrix, pi - y))
}

run_gradient_descent = function(theta0, step_size, n_steps) {
  trajectory = vector("list", length = n_steps + 1L)
  theta = theta0

  trajectory[[1L]] = data.frame(
    iteration = 0L,
    theta0 = theta[1],
    theta1 = theta[2],
    theta2 = theta[3],
    loss = cross_entropy(theta)
  )

  for (iter in seq_len(n_steps)) {
    gradient = ce_gradient(theta)
    theta = theta - step_size * gradient

    trajectory[[iter + 1L]] = data.frame(
      iteration = iter,
      theta0 = theta[1],
      theta1 = theta[2],
      theta2 = theta[3],
      loss = cross_entropy(theta)
    )
  }

  do.call(rbind, trajectory)
}

trajectory = run_gradient_descent(theta0 = c(0, 0, 0), step_size = 0.5, n_steps = 30L)

loss_plot = ggplot(trajectory, aes(x = iteration, y = loss)) +
  geom_line() +
  labs(x = "Iteration", y = expression(R[emp])) +
  theme_bw(base_size = 12)

trajectory$boundary_intercept = -trajectory$theta0 / trajectory$theta2
trajectory$boundary_slope = -trajectory$theta1 / trajectory$theta2

data_df = data.frame(x1 = x1, x2 = x2, y = factor(y))

boundary_plot = ggplot(data_df, aes(x = x1, y = x2)) +
  geom_point(aes(colour = y, shape = y), alpha = 0.7) +
  scale_colour_discrete(name = "Class", guide = guide_legend(position = "inside")) +
  scale_shape_discrete(name = "Class", guide = guide_legend(position = "inside")) +
  new_scale_colour() +
  geom_abline(
    data = trajectory,
    aes(intercept = boundary_intercept, slope = boundary_slope, colour = iteration),
    alpha = 0.5
  ) +
  scale_colour_continuous(name = "iteration") +
  coord_cartesian(xlim = c(-3.5, 4.5), ylim = c(-3.5, 4.5)) +
  labs(x = expression(x[1]), y = expression(x[2])) +
  theme_bw(base_size = 12) +
  theme(legend.position.inside = c(0.87, 0.74))

combined_plot = (loss_plot + boundary_plot) +
  plot_layout(nrow = 1)

if (interactive()) {
  print(combined_plot)
}

ggsave(
  filename = "../figure/gradient_descent_logistic.pdf",
  plot = combined_plot,
  width = 7,
  height = 2.5
)
