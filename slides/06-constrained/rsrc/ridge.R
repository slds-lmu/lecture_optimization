# Ridge regression objective visualizations (unregularized, penalized, constrained)

set.seed(1L)

library(vistool)
library(ggplot2)

cars_df = cars

# for nicer contours
speed_standardized = as.numeric(scale(cars_df$speed, center = TRUE, scale = TRUE))
dist_standardized = as.numeric(scale(cars_df$dist, center = TRUE, scale = TRUE))
design_matrix = matrix(speed_standardized, ncol = 1)
response = dist_standardized

theta_limits = list(x1 = c(-1.5, 1.5), x2 = c(-1.5, 1.5))
constraint_limits = list(x1 = c(-1, 1), x2 = c(-1, 1))

build_objective = function(lambda_value) {
  objective_linear(
    x = design_matrix,
    y = response,
    include_intercept = TRUE,
    penalize_intercept = TRUE,
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
theta_unreg = estimate_minimizer(unreg_objective, start = c(0, 0))

vis = as_visualizer(
  unreg_objective,
  type = "2d",
  x1_limits = theta_limits$x1,
  x2_limits = theta_limits$x2
)
vis$add_contours(
  alpha = 0.6
  )

# for later
vis_constrained = vis$clone(deep = TRUE)

vis$add_points(
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

ridge_lambda = 5
ridge_objective = build_objective(ridge_lambda)
theta_ridge = estimate_minimizer(ridge_objective, start = theta_unreg)

vis_ridge = as_visualizer(
  ridge_objective,
  type = "2d",
  x1_limits = theta_limits$x1,
  x2_limits = theta_limits$x2
)
vis_ridge$add_contours(
  alpha = 0.6
  )$add_points(
    matrix(theta_ridge, ncol = 2, byrow = TRUE),
    color = "#f6e05e",
    annotations = "$\\hat{\\theta}$")

vis_ridge$plot(
  x_lab = "$\\theta_1$",
  y_lab = "$\\theta_2$",
  show_legend = FALSE,
  show_title = FALSE,
  latex = TRUE
)
vis_ridge$save("../figure/ridge-penalized.png")

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

vis_constrained$add_points(
    matrix(theta_ridge, ncol = 2, byrow = TRUE),
    color = "#f6e05e",
    annotations = "$\\hat{\\theta}$")

p = vis_constrained$plot(
  x_lab = "$\\theta_1$",
  y_lab = "$\\theta_2$",
  show_legend = FALSE,
  show_title = FALSE,
  latex = TRUE
)

constraint_plot = p + constraint_layer
constraint_plot
ggsave(plot = constraint_plot, "../figure/ridge-constrained.png")
