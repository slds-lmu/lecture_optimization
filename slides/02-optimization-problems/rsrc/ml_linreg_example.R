set.seed(314L)
library(vistool)
library(mlr3)

n_obs = 20L
df = data.frame(
  x = runif(n_obs, min = 0, max = 5)
)
df$y = df$x + rnorm(n_obs)

task_linreg = TaskRegr$new(
  id = "ml_linreg_example",
  backend = df,
  target = "y"
)

# Figure 1: data points with the true linear hypothesis
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
vis_line$plot(
  show_title = FALSE,
  show_legend = FALSE
)
vis_line$save("../figure/ml_linreg_example_1.png", width = 3, height = 2.5)

# Shared linear objective for contour and surface views
xmat = cbind(1, df$x)
theta_limits = list(theta0 = c(-20, 20), theta1 = c(-20, 20))

lin_objective = objective_linear(
  x = xmat,
  y = df$y,
  lambda = 0,
  alpha = 1,
  include_intercept = FALSE,
  penalize_intercept = FALSE,
  id = "ml_linreg_objective",
  label = "Squared Error Loss"
)

lin_objective$lower = c(theta_limits$theta0[1], theta_limits$theta1[1])
lin_objective$upper = c(theta_limits$theta0[2], theta_limits$theta1[2])

theta_true = c(0, 1)
loss_at_true = lin_objective$eval(theta_true)

# Figure 2: contour visualization of the squared error surface.
vis_contour = as_visualizer(
  lin_objective,
  x1_limits = theta_limits$theta0,
  x2_limits = theta_limits$theta1
)
vis_contour$add_contours(alpha = 0.5)
vis_contour$add_points(matrix(theta_true, nrow = 1), color = "#f6e05e", size = 3)
vis_contour$plot(
  show_title = FALSE,
  show_legend = FALSE,
  x_lab = "$\\theta_0$",
  y_lab = "$\\theta_1$",
  latex = TRUE
)
vis_contour$save("../figure/ml_linreg_example_2.png", width = 3, height = 2)

# Figure 3: interactive surface of the same objective
vis_surface = as_visualizer(
  lin_objective,
  type = "surface",
  x1_limits = theta_limits$theta0,
  x2_limits = theta_limits$theta1
)
vis_surface$add_points(matrix(c(theta_true, loss_at_true), nrow = 1), color = "#f6e05e", size = 5)
vis_surface$set_scene(x = 2, y = 0.4, z = 0.9)
vis_surface$plot(
  show_title = FALSE,
  show_legend = FALSE,
  x_lab = "theta_0",
  y_lab = "theta_1",
  z_lab = "L(theta)"
)
vis_surface$save("../figure/ml_linreg_example_3.png", width = 1000, height = 800)
