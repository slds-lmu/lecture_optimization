library(Rmpfr)
library(ggplot2)
library(gridExtra)
library(latex2exp)
library(ggpubr)

set.seed(1234)
x = rpois(25, lambda = 0.75)

neg_log_lik = function(theta) {
  y = - sum(log(dpois(x, theta)))
  thetav <<- append(thetav, as.numeric(theta))
  out <<- append(out, as.numeric(y))
  return(y)
}

theta_grid = seq(0.25, 0.75, length.out = 1000)
y = sapply(theta_grid, neg_log_lik)
df = data.frame(theta = theta_grid, y = y)

p1 = ggplot(data = df, aes(x = theta_grid, y = y)) + geom_line() + xlab(TeX('$\\lambda$')) + ylab("Negative log-likelihood") + theme_bw()

# p1 = p1 + geom_point(data = data.frame(x = 0.6, y = neg_log_lik(0.6)), aes(x = x, y = y), colour = "blue", size = 2, shape = 8)


thetav = list()
out = list()

optimizeR(neg_log_lik, lower = 0, upper = 1, trace = TRUE, method = "GoldenRatio")

archive = data.frame(iter = 1:length(thetav), x = unlist(thetav), y = unlist(out), M = "GoldenRatio")

thetav = list()
out = list()

optimizeR(neg_log_lik, lower = 0, upper = 1, trace = TRUE)

archive = rbind(archive, data.frame(iter = 1:length(thetav), x = unlist(thetav), y = unlist(out), M = "Brent"))

p1 = p1 + geom_point(data = archive, aes(x = x, y = y, colour = M), alpha = 0.5)
# p1 = p1 + ylim(c(20, 40))

p2 = ggplot() + geom_line(data = archive, aes(x = log(iter), y = y, colour = M)) + theme_bw()

p = ggarrange(p1, p2, common.legend = TRUE)


ggsave("figure_man/poisson.pdf", p, width = 6, height = 3)
