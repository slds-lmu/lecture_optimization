set.seed(314L)
library(vistool)

# Simulate a simple logistic regression task once for all plots.
n = 1000L
df = data.frame(
  x1 = rnorm(n, mean = -5, sd = 5),
  x2 = rnorm(n, mean = -10, sd = 10)
)
z = 2 * df$x1 + 3 * df$x2
prob = 1 / (1 + exp(-z))
df$y = rbinom(n, size = 1, prob = prob)

xmat = as.matrix(df[, c("x1", "x2")])
response = df$y

logreg_objective = function(xmat, y, alpha, lambda, limits) {
  objective = objective_logistic(
    x = xmat,
    y = y,
    lambda = lambda,
    alpha = alpha,
    include_intercept = FALSE,
    penalize_intercept = FALSE,
    id = sprintf("logreg_alpha_%s_lambda_%s", alpha, lambda),
    label = "Regularized Logistic Risk",
    transform = objective_transform_log()
  )

  objective$lower = c(limits$x1[1], limits$x2[1])
  objective$upper = c(limits$x1[2], limits$x2[2])

  objective
}

render_logreg = function(xmat, y, alpha, lambda, limits, filename, title,
                          legend = TRUE, width = 3.5, height = 3.5) {
  objective = logreg_objective(xmat, y, alpha, lambda, limits)
  vis = as_visualizer(objective)
  vis$add_contours(alpha = 0.5)

  theta_hat = stats::optim(
    c(0, 0),
    fn = function(theta) objective$eval(theta),
    gr = function(theta) objective$grad(theta)
  )$par
  vis$add_points(
    matrix(theta_hat, ncol = 2),
    color = "#f6e05e"
  )
  offset_x = (limits$x1[2] - limits$x1[1]) * 0.04
  vis$add_annotation(
    text = "$\\hat{\\theta}$",
    color = "#f6e05e",
    x = theta_hat[1] + offset_x,
    y = theta_hat[2],
    latex = TRUE
  )

  plot_obj = vis$plot(
    plot_title = title,
    x_lab = "$\\theta_1$",
    y_lab = "$\\theta_2$",
    legend_title = "log($R_{emp})$",
    show_legend = legend
  )

  ggplot2::ggsave(filename, plot_obj, width = width, height = height)
}

wide_limits = list(x1 = c(-1, 3), x2 = c(-1, 4))
render_logreg(
  xmat = xmat,
  y = response,
  alpha = 1,
  lambda = 0,
  limits = wide_limits,
  filename = "../figure/logreg-0.png",
  title = "Unregularized $R_{emp}$",
  legend = TRUE,
  width = 4,
  height = 3
)

regularized_limits = list(x1 = c(-0.25, 0.5), x2 = c(-0.25, 0.5))
for (alpha in c(0, 0.5, 1)) {
  for (lambda in c(0.1, 1, 5)) {
    render_logreg(
      xmat = xmat,
      y = response,
      alpha = alpha,
      lambda = lambda,
      limits = regularized_limits,
      filename = sprintf("../figure/logreg-%s-%s.png", alpha, lambda),
      title = sprintf("Reg. risk with \\lambda = %s,  \\alpha = %s", lambda, alpha),
      legend = FALSE
    )
  }
}
