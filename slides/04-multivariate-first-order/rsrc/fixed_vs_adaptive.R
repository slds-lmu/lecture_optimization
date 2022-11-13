library(ggplot2)
library(ggpubr)
library(rootSolve) #gradient()

theme_set(theme_bw())

f = function(x, delta = 0.05) {
	ifelse (abs(x) <= delta, 1 / 2 * x^2, delta * (abs(x) - 1 / 2 * delta))
}


p = ggplot(data.frame(x = c(-1, 1)), aes(x)) + stat_function(fun = f)
p

# Perform GD with small step size
x0 = - 0.8
alpha = 0.5

progress = c(t = 0, x = x0, y = f(x0))

x = x0
for (t in 1:20) {

	grad = gradient(f, x)
	x = x - alpha * grad
	y = f(x)

	progress = rbind(progress, c(t = t, x = x, y = y))
}

progress = as.data.frame(progress)
progress$stepsize = "0.5"

# Perform GD with big step size
x0 = 0.8
alpha = 10

progress2 = c(t = 0, x = x0, y = f(x0))

x = x0
for (t in 1:20) {

	grad = gradient(f, x)
	x = x - alpha * grad
	y = f(x)

	progress2 = rbind(progress2, c(t = t, x = x, y = y))
}

progress2 = as.data.frame(progress2)
progress2$stepsize = "10"
progress = rbind(progress, progress2)

# Perform GD with adaptive size
x0 = 0.8
alpha0 = 10

progress2 = c(t = 0, x = x0, y = f(x0))

x = x0
for (t in 1:20) {
	
	alpha = alpha0 / t

	grad = gradient(f, x)
	x = x - alpha * grad 
	y = f(x)

	progress2 = rbind(progress2, c(t = t, x = x, y = y))
}

progress2 = as.data.frame(progress2)
progress2$stepsize = "10/t"
progress = rbind(progress, progress2)

p1 = p + geom_point(data = progress, aes(x = x, y = y, color = stepsize))
p1 = p1 + geom_line(data = progress, aes(x = x, y = y, color = stepsize))
p1

p2 = ggplot(data = progress, aes(x = t, y = y, color = stepsize)) + geom_line()
p2

p = ggarrange(p1, p2, nrow = 1, common.legend = TRUE, legend = "right")

ggsave(filename = "figure_man/fixed_vs_adaptive.pdf", p, width = 9, height = 2.5)

