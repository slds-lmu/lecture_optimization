# Used in: slides/06-constrained/slides-optim-rest-6-regularity-conditions.tex
#
# Builds three contour plots for the ridge regression regularity-condition slides:
# the unregularized squared-error objective, the ridge-penalized objective, and
# the equivalent constrained view using the same optimum.

set.seed(1L)

library(data.table)
library(ggplot2)
library(vistool)

figure_dir = "../figure"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

theta_limits = list(x1 = c(-1.5, 1.5), x2 = c(-1.5, 1.5))
point_color = "#f6e05e"
point_label_color = "#252525"
constraint_color = "#e34a33"

cars_data = as.data.table(cars)
design_matrix = matrix(as.numeric(scale(cars_data$speed, center = TRUE, scale = TRUE)), ncol = 1L)
response = as.numeric(scale(cars_data$dist, center = TRUE, scale = TRUE))

build_objective = function(lambda_value) {
  objective_label = if (lambda_value == 0) {
    "Squared error risk"
  } else {
    sprintf("Ridge risk (lambda = %s)", lambda_value)
  }

  objective_linear(
    x = design_matrix,
    y = response,
    include_intercept = TRUE,
    penalize_intercept = TRUE,
    lambda = lambda_value,
    alpha = 0,
    label = objective_label
  )
}

estimate_minimizer = function(objective, start_value = c(0, 0)) {
  fit = optim(
    par = start_value,
    fn = objective$eval,
    gr = objective$grad,
    method = "BFGS"
  )

  fit$par
}

theta_point = function(theta) {
  matrix(theta, ncol = 2L, byrow = TRUE)
}

build_visualizer = function(objective) {
  visualizer = as_visualizer(
    objective,
    type = "2d",
    x1_limits = theta_limits$x1,
    x2_limits = theta_limits$x2
  )

  visualizer$add_contours(alpha = 0.6)
  visualizer
}

plot_objective = function(objective, theta, constraint_path = NULL) {
  visualizer = build_visualizer(objective)
  visualizer$add_points(
    theta_point(theta),
    color = point_color
  )

  plot = visualizer$plot(
    show_legend = FALSE,
    show_title = FALSE
  ) +
    labs(x = expression(theta[1]), y = expression(theta[2])) +
    annotate(
      "text",
      x = theta[1L] + 0.12,
      y = theta[2L] + 0.16,
      label = "hat(theta)",
      parse = TRUE,
      color = point_label_color,
      size = 5
    )

  if (is.null(constraint_path)) {
    plot
  } else {
    plot +
      geom_path(
        data = constraint_path,
        mapping = aes(x = theta_1, y = theta_2),
        inherit.aes = FALSE,
        linewidth = 0.7,
        color = constraint_color
      )
  }
}

save_ridge_plot = function(plot, filename) {
  ggsave(
    filename = file.path(figure_dir, filename),
    plot = plot,
    width = 5,
    height = 4.5,
    dpi = 300,
    bg = "white"
  )
}

unregularized_objective = build_objective(lambda_value = 0)
theta_unregularized = estimate_minimizer(unregularized_objective)
unregularized_plot = plot_objective(unregularized_objective, theta_unregularized)

if (interactive()) {
  print(unregularized_plot)
}

save_ridge_plot(unregularized_plot, "ridge-unregularized.png")

ridge_lambda = 5
ridge_objective = build_objective(lambda_value = ridge_lambda)
theta_ridge = estimate_minimizer(ridge_objective, start_value = theta_unregularized)
ridge_plot = plot_objective(ridge_objective, theta_ridge)

if (interactive()) {
  print(ridge_plot)
}

save_ridge_plot(ridge_plot, "ridge-penalized.png")

ball_radius = sqrt(sum(theta_ridge^2))
angles = seq(0, 2 * pi, length.out = 361L)
constraint_path = data.table(
  theta_1 = ball_radius * cos(angles),
  theta_2 = ball_radius * sin(angles)
)
constrained_plot = plot_objective(unregularized_objective, theta_ridge, constraint_path)

if (interactive()) {
  print(constrained_plot)
}

save_ridge_plot(constrained_plot, "ridge-constrained.png")
