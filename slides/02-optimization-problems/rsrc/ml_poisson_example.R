library(ggplot2)

theme_set(theme_bw())

load("rsrc/e100.RData")
e99 = e100[-which.max(e100)] # start without a tricky outlier

p = ggplot() + geom_histogram(data = data.frame(x = factor(e99, levels = c(0, 1, 2), ordered = TRUE)), aes(x = x), stat = "count") + theme_bw()

ggsave("figure_man/ml_poisson_example_1.pdf", p, width = 3, height = 2.5)


likelihood = function(lambda) {
  sum(dpois(e99, lambda = lambda, log = TRUE))
}

lambda = seq(0, 1, by = 0.01)
f = sapply(lambda, likelihood)
df = data.frame(lambda = lambda, f = f)

optimum = optimize(likelihood, interval = c(0, 1), maximum = TRUE)

p = ggplot() + geom_line(data = df, aes(x = lambda, y = f)) + geom_point(data = data.frame(x = optimum$maximum, y = optimum$objective), aes(x = x, y = y), colour = "blue") + xlab("x")

ggsave("figure_man/ml_poisson_example_2.pdf", p, width = 3, height = 2)


p = ggplot() + geom_line(data = df, aes(x = lambda, y = - f)) + geom_point(data = data.frame(x = optimum$maximum, y = - optimum$objective), aes(x = x, y = y), colour = "blue") + xlab("x")

ggsave("figure_man/ml_poisson_example_3.pdf", p, width = 3, height = 2)



