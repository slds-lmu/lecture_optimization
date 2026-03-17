# ------------------------------------------------------------------------------
# multivariate first order

# FIG: plot gradeint descent for linear model
# ------------------------------------------------------------------------------

set.seed(1L)

library(ggplot2)
library(patchwork)

theme_set(theme_bw())

# ------------------------------------------------------------------------------

x = runif(25, 0, 2)
X = as.matrix(cbind(1, x))
Y = as.matrix(1 + 2 * x + rnorm(length(x), sd = 1))

p = ggplot(data = data.frame(x = x, y = Y), aes(x = x, y = y)) + geom_point() + geom_smooth(method='lm')
if (interactive()) print(p)

th0 = c(0, 0)
alpha = 0.1

progress = c(t = 0, theta0 = th0[1], theta1 = th0[2], R = sum((Y - X %*% th0)^2))

th = th0
for (t in 1:30) {

	grad = 2 * colSums(diag(as.vector(Y - X %*% th)) %*% X) 
	grad = grad / sqrt(sum(grad^2))
	th = th + alpha * grad
	R = sum((Y - X %*% th)^2)

	progress = rbind(progress, c(t = t, theta0 = th[1], theta1 = th[2], R = sum((Y - X %*% th)^2)))
	print(progress)
}


p1 = ggplot(data = as.data.frame(progress), aes(x = t, y = R)) + geom_line() + xlim(c(1, nrow(progress)))
if (interactive()) print(p1)

p2 = p + geom_abline(data = as.data.frame(progress), aes(intercept = theta0, slope = theta1, colour = t), alpha = 0.5)
if (interactive()) print(p2)

p = p1 + p2 +
  plot_layout(nrow = 1, guides = "collect") &
  theme(legend.position = "right")
if (interactive()) print(p)
ggsave(filename = "../figure/gradient_descent_lm.pdf", p, width = 7, height = 2.5)
