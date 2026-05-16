# Used in: ../slides-problems-1-unconstrained.tex
#
# Simulate a linear regression problem, visualize the empirical risk surface,
# and compare the unregularized, lasso, and ridge objectives.

set.seed(314L)

library(data.table)
library(ggplot2)
library(patchwork)
library(vistool)

# Simulate one linear regression task that is reused across all panels.
n_obs = 1000L
sim_data = data.table(
  x1 = rnorm(n_obs, mean = -5, sd = 5),
  x2 = rnorm(n_obs, mean = -10, sd = 10)
)
sim_data[, y := 2 * x1 + 3 * x2 + rnorm(n_obs, sd = 0.5)]

x_mat = as.matrix(sim_data[, .(x1, x2)])
response = sim_data[["y"]]
theta_limits = list(x1 = c(-1, 3), x2 = c(-1, 3.5))

build_linreg_objective = function(x_mat, y, alpha, lambda, limits) {
  objective = objective_linear(
    x = x_mat,
    y = y,
    lambda = lambda,
    alpha = alpha,
    include_intercept = FALSE,
    penalize_intercept = FALSE,
    id = sprintf("linreg_alpha_%s_lambda_%s", alpha, lambda),
    label = "Regularized empirical risk"
  )

  objective$lower = c(limits$x1[1], limits$x2[1])
  objective$upper = c(limits$x1[2], limits$x2[2])
  objective
}

plot_linreg_objective = function(x_mat, y, alpha, lambda, limits, title, legend = TRUE) {
  objective = build_linreg_objective(x_mat, y, alpha, lambda, limits)
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

  visualizer$plot(
    plot_title = title,
    x_lab = "θ₁",
    y_lab = "θ₂",
    x_limits = limits$x1,
    y_limits = limits$x2,
    legend_title = "R_emp",
    show_legend = legend
  )
}

# Render the three objective surfaces used in the slides.
plot_unregularized = plot_linreg_objective(
  x_mat = x_mat,
  y = response,
  alpha = 1,
  lambda = 0,
  limits = theta_limits,
  title = "Unregularized R_emp",
  legend = TRUE
)

plot_lasso = plot_linreg_objective(
  x_mat = x_mat,
  y = response,
  alpha = 1,
  lambda = 200,
  limits = theta_limits,
  title = "Lasso",
  legend = FALSE
)

plot_ridge = plot_linreg_objective(
  x_mat = x_mat,
  y = response,
  alpha = 0,
  lambda = 200,
  limits = theta_limits,
  title = "Ridge",
  legend = FALSE
)

combined_plot = plot_unregularized + plot_lasso + plot_ridge + plot_layout(ncol = 3)

ggsave("../figure/linreg.png", combined_plot, width = 8, height = 3)
