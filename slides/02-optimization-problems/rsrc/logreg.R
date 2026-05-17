# Used in: ../slides-problems-1-unconstrained.tex
#
# Simulate one logistic regression problem and save empirical-risk contour plots
# for different elastic-net regularization settings.

set.seed(314L)

library(data.table)
library(ggplot2)
library(vistool)

# Simulate one classification task that is reused across all figures.
n_obs = 1000L
sim_data = data.table(
  x1 = rnorm(n_obs, mean = -5, sd = 5),
  x2 = rnorm(n_obs, mean = -10, sd = 10)
)
linear_predictor = 2 * sim_data[["x1"]] + 3 * sim_data[["x2"]]
class_probabilities = 1 / (1 + exp(-linear_predictor))
sim_data[, y := rbinom(n_obs, size = 1, prob = class_probabilities)]

x_mat = as.matrix(sim_data[, .(x1, x2)])
response = sim_data[["y"]]

build_logreg_objective = function(x_mat, y, alpha, lambda, limits) {
  objective = objective_logistic(
    x = x_mat,
    y = y,
    lambda = lambda,
    alpha = alpha,
    include_intercept = FALSE,
    penalize_intercept = FALSE,
    id = sprintf("logreg_alpha_%s_lambda_%s", alpha, lambda),
    label = "Regularized empirical risk",
    transform = objective_transform_log()
  )

  objective$lower = c(limits$x1[1], limits$x2[1])
  objective$upper = c(limits$x1[2], limits$x2[2])
  objective
}

save_logreg_plot = function(
    x_mat,
    y,
    alpha,
    lambda,
    limits,
    file_path,
    title,
    legend = TRUE,
    width = 3.5,
    height = 3.5) {
  objective = build_logreg_objective(x_mat, y, alpha, lambda, limits)
  visualizer = as_visualizer(objective)
  visualizer$add_contours(alpha = 0.5)

  theta_hat = optim(
    c(0, 0),
    fn = function(theta) objective$eval(theta),
    gr = function(theta) objective$grad(theta)
  )$par
  visualizer$add_points(matrix(theta_hat, ncol = 2), color = "#f6e05e")

  x_offset = (limits$x1[2] - limits$x1[1]) * 0.04
  visualizer$add_annotation(
    text = "$\\hat{\\theta}$",
    color = "#f6e05e",
    x = theta_hat[1] + x_offset,
    y = theta_hat[2],
    latex = TRUE
  )

  plot_object = visualizer$plot(
    plot_title = title,
    x_lab = "θ₁",
    y_lab = "θ₂",
    x_limits = limits$x1,
    y_limits = limits$x2,
    legend_title = "log(R_emp)",
    show_legend = legend
  )

  ggsave(file_path, plot_object, width = width, height = height)
}

# Save the unregularized objective and the regularized variants.
wide_limits = list(x1 = c(-1, 3), x2 = c(-1, 4))
save_logreg_plot(
  x_mat = x_mat,
  y = response,
  alpha = 1,
  lambda = 0,
  limits = wide_limits,
  file_path = "../figure/logreg-0.png",
  title = "Unregularized R_emp",
  legend = TRUE,
  width = 4,
  height = 3
)

regularized_limits = list(x1 = c(-0.25, 0.5), x2 = c(-0.25, 0.5))
for (alpha in c(0, 0.5, 1)) {
  for (lambda in c(0.1, 1)) {
    save_logreg_plot(
      x_mat = x_mat,
      y = response,
      alpha = alpha,
      lambda = lambda,
      limits = regularized_limits,
      file_path = sprintf("../figure/logreg-%s-%s.png", alpha, lambda),
      title = sprintf("Reg. risk with λ = %s, α = %s", lambda, alpha),
      legend = FALSE
    )
  }
}
