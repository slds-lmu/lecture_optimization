library(Rmpfr)
library(ggplot2)
library(gridExtra)

set.seed(25)
x = rpois(25, lambda = 0.75)

neg_log_lik = function(theta) {
  - sum(log(dpois(x, theta)))
}

theta_grid = seq(0, 1, length.out = 1000)
y = sapply(theta_grid, neg_log_lik)
s = sapply(theta_grid, score_fkt)
df = data.frame(theta = theta_grid, y = y, s = s)

p1 = ggplot(data = df, aes(x = theta_grid, y = y)) + geom_line() + xlab(TeX('$\\lambda$')) + ylab("Negative log-likelihood") + theme_bw()

p1 = p1 + geom_point(data = data.frame(x = 0.6, y = neg_log_lik(0.6)), aes(x = x, y = y), colour = "blue", size = 2)

ggsave("figure_man/poisson.pdf", p1, width = 4, height = 4)

# Here we could extract convergence rates 

optimizeR(neg_log_lik, lower = 0, upper = 1, trace = TRUE)

optimizeR(neg_log_lik, lower = 0, upper = 1, trace = TRUE, method = "GoldenRatio")

## TODO: Compare Brent and Golden Ratio, and show convergence rates as plots