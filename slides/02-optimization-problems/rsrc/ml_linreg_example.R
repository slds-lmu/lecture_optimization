# Used in: ../slides-problems-1-unconstrained.tex
#
# Simulate a small linear regression example and save figures for the data view
# and the corresponding empirical-risk contour plot.

set.seed(314L)

library(data.table)
library(mlr3)
library(vistool)

# Simulate one regression dataset and expose it as an mlr3 task.
n_obs = 20L
sim_data = data.table(x = runif(n_obs, min = 0, max = 5))
sim_data[, y := x + rnorm(n_obs)]

task_linreg = TaskRegr$new(
  id = "ml_linreg_example",
  backend = sim_data,
  target = "y"
)

# Figure 1: observed data with the true linear hypothesis.
true_hypothesis = hypothesis(
  fun = function(x) x,
  type = "regr",
  predictors = "x"
)

vis_line = as_visualizer(
  task_linreg,
  hypothesis = true_hypothesis,
  x1_limits = c(0, 5)
)
vis_line$add_training_data(color = "#1f77b4")
if (interactive()) {
  vis_line$plot(show_title = FALSE, show_legend = FALSE)
}
vis_line$save("../figure/ml_linreg_example_1.png", width = 3, height = 2.5)

# Figure 2: contour plot of the empirical squared-error loss.
x_mat = cbind(1, sim_data[["x"]])
intercept_limits = c(-20, 20)
slope_limits = c(-20, 20)

linreg_objective = objective_linear(
  x = x_mat,
  y = sim_data[["y"]],
  lambda = 0,
  alpha = 1,
  include_intercept = FALSE,
  penalize_intercept = FALSE,
  id = "ml_linreg_objective",
  label = "Squared error loss"
)

linreg_objective$lower = c(intercept_limits[1], slope_limits[1])
linreg_objective$upper = c(intercept_limits[2], slope_limits[2])

theta_true = c(0, 1)
loss_at_true = linreg_objective$eval(theta_true)

vis_contour = as_visualizer(
  linreg_objective,
  x1_limits = intercept_limits,
  x2_limits = slope_limits
)
vis_contour$add_contours(alpha = 0.5)
vis_contour$add_points(matrix(theta_true, nrow = 1), color = "#f6e05e", size = 3)
if (interactive()) {
  vis_contour$plot(
    show_title = FALSE,
    show_legend = FALSE,
    x_lab = "θ₀",
    y_lab = "θ₁"
  )
}
vis_contour$save("../figure/ml_linreg_example_2.png", width = 3, height = 2)

# Figure 3: optional interactive surface of the same objective.
vis_surface = as_visualizer(
  linreg_objective,
  type = "surface",
  x1_limits = intercept_limits,
  x2_limits = slope_limits
)
vis_surface$add_points(matrix(c(theta_true, loss_at_true), nrow = 1), color = "#f6e05e", size = 5)
vis_surface$set_scene(x = 2, y = 0.4, z = 0.9)
if (interactive()) {
  vis_surface$plot(
    show_title = FALSE,
    show_legend = FALSE,
    x_lab = "θ₀",
    y_lab = "θ₁",
    z_lab = "L(θ)"
  )
}
