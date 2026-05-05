set.seed(314L)
library(vistool)
library(ggpubr)

# Simulate a linear regression task once for all plots.
n = 1000L
df = data.frame(
  x1 = rnorm(n, mean = -5, sd = 5),
  x2 = rnorm(n, mean = -10, sd = 10)
)
df$y = 2 * df$x1 + 3 * df$x2 + rnorm(n, sd = 0.5)

xmat = as.matrix(df[, c("x1", "x2")])
response = df$y

linreg_objective = function(xmat, y, alpha, lambda, limits) {
  objective = vistool::objective_linear(
    x = xmat,
    y = y,
    lambda = lambda,
    alpha = alpha,
    include_intercept = FALSE,
    penalize_intercept = FALSE,
    id = sprintf("linreg_alpha_%s_lambda_%s", alpha, lambda),
    label = "Regularized Squared Error Risk"
  )

  objective$lower = c(limits$x1[1], limits$x2[1])
  objective$upper = c(limits$x1[2], limits$x2[2])

  objective
}

render_linreg = function(xmat, y, alpha, lambda, limits, title, legend = TRUE) {
  objective = linreg_objective(xmat, y, alpha, lambda, limits)
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

  vis$plot(
    plot_title = title,
    x_lab = "$\\theta_1$",
    y_lab = "$\\theta_2$",
    x_limits = c(-1, 3),
    y_limits = c(-1, 3.5),
    legend_title = "$R_{emp}$",
    show_legend = legend
  )
}

limits = list(x1 = c(-1, 3), x2 = c(-1, 3.5))
plot_unreg = render_linreg(
  xmat = xmat,
  y = response,
  alpha = 1,
  lambda = 0,
  limits = limits,
  title = "Unregularized $R_{emp}$",
  legend = TRUE
)

plot_lasso = render_linreg(
  xmat = xmat,
  y = response,
  alpha = 1,
  lambda = 200,
  limits = limits,
  title = "Lasso",
  legend = FALSE
)

plot_ridge = render_linreg(
  xmat = xmat,
  y = response,
  alpha = 0,
  lambda = 200,
  limits = limits,
  title = "Ridge",
  legend = FALSE
)

# Combine the vistool plots into a single layout.
combined = ggpubr::ggarrange(
  plot_unreg,
  plot_lasso,
  plot_ridge,
  ncol = 3,
  nrow = 1,
  common.legend = TRUE,
  legend = "right"
)

ggplot2::ggsave("../figure/linreg.png", combined, width = 8, height = 3)
