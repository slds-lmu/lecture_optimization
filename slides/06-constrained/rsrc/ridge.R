# Ridge regression objective visualizations (unregularized, penalized, constrained)

set.seed(1L)

library(vistool)
library(ggplot2)

cars_df = cars
design_matrix = matrix(cars_df$speed, ncol = 1)
response = cars_df$dist

theta_limits = list(x1 = c(-18, 18), x2 = c(-18, 18))
constraint_limits = list(x1 = c(-6, 6), x2 = c(-6, 6))

build_objective = function(lambda_value) {
  objective_linear(
    x = design_matrix,
    y = response,
    include_intercept = TRUE,
    penalize_intercept = FALSE,
    lambda = lambda_value,
    alpha = 0,
    label = if (lambda_value == 0) {
      "Squared error risk"
    } else {
      sprintf("Ridge risk (lambda = %s)", lambda_value)
    }
  )
}

estimate_minimizer = function(objective, start = c(0, 0)) {
  stats::optim(
    par = start,
    fn = objective$eval,
    gr = objective$grad,
    method = "BFGS"
  )$par
}

unreg_objective = build_objective(0)
theta_unreg = estimate_minimizer(unreg_objective, start = c(-10, 5))

vis = as_visualizer(
  unreg_objective,
  type = "2d",
  x1_limits = c(-20, 20),
  x2_limits = c(-20, 20)
)
vis$add_contours(
  alpha = 0.6
  )$add_points(
    matrix(theta_unreg, ncol = 2, byrow = TRUE),
    color = "#f6e05e",
    annotations = "$\\hat{\\theta}$")

vis$plot(
  x_lab = "$\\theta_1$",
  y_lab = "$\\theta_2$",
  show_legend = FALSE,
  show_title = FALSE,
  latex = TRUE
)
vis$save("../figure/ridge-unregularized.png")

ridge_lambda = 20
ridge_objective = build_objective(ridge_lambda)
theta_ridge = estimate_minimizer(ridge_objective, start = theta_unreg)

vis_ridge = as_visualizer(
  ridge_objective,
  type = "2d",
  x1_limits = c(-20, 20),
  x2_limits = c(-20, 20)
)
vis_ridge$add_contours(
  alpha = 0.6
  )$add_points(
    matrix(theta_ridge, ncol = 2, byrow = TRUE),
    color = "#f6e05e",
    annotations = "$\\hat{\\theta}$")

p = vis_ridge$plot(
  x_lab = "$\\theta_1$",
  y_lab = "$\\theta_2$",
  show_legend = FALSE,
  show_title = FALSE,
  latex = TRUE
)
ggsave(plot = p, "../figure/ridge-penalized.png")

ball_radius = sqrt(sum(theta_ridge^2))
angles = seq(0, 2 * pi, length.out = 361)
constraint_path = data.frame(
  theta1 = ball_radius * cos(angles),
  theta2 = ball_radius * sin(angles)
)
constraint_layer = ggplot2::geom_path(
  data = constraint_path,
  mapping = ggplot2::aes(theta1, theta2),
  inherit.aes = FALSE,
  linewidth = 0.5,
  color = "#f56565"
)
vis_constrained = p + constraint_layer
vis_constrained
