library(Rmpfr)
library(ggplot2)
library(gridExtra)
library(latex2exp)
library(ggpubr)

set.seed(1234)
x = rpois(250, lambda = 1.)

thetav = list()
out = list()

neg_log_lik = function(theta) {
  y = - sum(log(dpois(x, theta)))
  thetav <<- append(thetav, as.numeric(theta))
  out <<- append(out, as.numeric(y))
  return(y)
}

theta_grid = seq(0.75, 1.25, length.out = 1000)
y = sapply(theta_grid, neg_log_lik)
df = data.frame(theta = theta_grid, y = y)

p = ggplot(data = df, aes(x = theta_grid, y = y)) + geom_line() + xlab(TeX('$\\lambda$')) + ylab("Negative log-likelihood") + theme_bw()

thetav = list()
out = list()

optimizeR(neg_log_lik, lower = 0.5, upper = 1.5, trace = TRUE, method = "GoldenRatio", tol = 1e-10)

archive = data.frame(iter = 1:length(thetav), x = unlist(thetav), y = unlist(out), Method = "GoldenRatio")

thetav = list()
out = list()

optimizeR(neg_log_lik, lower = 0.5, upper = 1.5, trace = TRUE, tol = 1e-10)

archive = rbind(archive, data.frame(iter = 1:length(thetav), x = unlist(thetav), y = unlist(out), Method = "Brent"))

p = p + geom_point(data = archive, aes(x = x, y = y, colour = Method), alpha = 0.5) + theme(axis.text.y=element_blank())


ggsave("figure_man/poisson.pdf", p, width = 4, height = 2)
