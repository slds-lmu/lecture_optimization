library(ggplot2)
library(ggpubr)

theme_set(theme_bw())

x = runif(25, 0, 2)
X = as.matrix(cbind(1, x))
Y = as.matrix(1 + 2 * x + rnorm(length(x), sd = 1))

p = ggplot(data = data.frame(x = x, y = Y), aes(x = x, y = y)) + geom_point() + geom_smooth(method='lm')
p

th0 = c(0, 0)
alpha = 0.1

progress = c(t = 0, theta0 = th0[1], theta1 = th0[2], L = sum((Y - X %*% th)^2))

th = th0
for (t in 1:30) {

	grad = 2 * colSums(diag(as.vector(Y - X %*% th)) %*% X) 
	grad = grad / sqrt(sum(grad^2))
	th = th + alpha * grad
	L = sum((Y - X %*% th)^2)

	progress = rbind(progress, c(t = t, theta0 = th[1], theta1 = th[2], L = sum((Y - X %*% th)^2)))
}


p1 = ggplot(data = as.data.frame(progress), aes(x = t, y = L)) + geom_line() + xlim(c(1, nrow(progress)))
p1

p2 = p + geom_abline(data = as.data.frame(progress), aes(intercept = theta0, slope = theta1, colour = t), alpha = 0.5)
p2

p = ggarrange(p1, p2, nrow = 1, common.legend = TRUE, legend = "right")

ggsave(filename = "figure_man/gradient_descent_lm.pdf", p, width = 7, height = 2.5)


