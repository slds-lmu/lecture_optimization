# ------------------------------------------------------------------------------
# univariate optimization

# FIG: find the optimal Poisson parameter using two optimization methods:
#   (1)	Golden Ratio Search
#   (2)	Brent’s Method
# ------------------------------------------------------------------------------

library(ggplot2)
library(latex2exp)

set.seed(1234)

# ------------------------------------------------------------------------------

x = rpois(250, lambda = 1.)

thetav = list()
out = list()

neg_log_lik = function(theta) {
  y = - sum(log(dpois(x, theta)))
  thetav <<- append(thetav, as.numeric(theta))
  out <<- append(out, as.numeric(y))
  return(y)
}

golden_ratio_trace = function(fn, lower, upper, tol = 1e-10, max_iter = 1000L) {
  phi = (sqrt(5) - 1) / 2
  left = lower
  right = upper
  c_point = right - phi * (right - left)
  d_point = left + phi * (right - left)
  f_c = fn(c_point)
  f_d = fn(d_point)

  iter = 0L
  while ((right - left) > tol && iter < max_iter) {
    iter = iter + 1L
    if (f_c < f_d) {
      right = d_point
      d_point = c_point
      f_d = f_c
      c_point = right - phi * (right - left)
      f_c = fn(c_point)
    } else {
      left = c_point
      c_point = d_point
      f_c = f_d
      d_point = left + phi * (right - left)
      f_d = fn(d_point)
    }
  }

  invisible((left + right) / 2)
}

theta_grid = seq(0.75, 1.25, length.out = 1000)
y = sapply(theta_grid, neg_log_lik)
df = data.frame(theta = theta_grid, y = y)

p = ggplot(data = df, aes(x = theta_grid, y = y)) + geom_line() + xlab(TeX('$\\lambda$')) + ylab("Negative log-likelihood") + theme_bw()

thetav = list()
out = list()

golden_ratio_trace(neg_log_lik, lower = 0.5, upper = 1.5, tol = 1e-10)

archive = data.frame(iter = 1:length(thetav), x = unlist(thetav), y = unlist(out), Method = "GoldenRatio")

thetav = list()
out = list()

stats::optimize(neg_log_lik, lower = 0.5, upper = 1.5, tol = 1e-10)

archive = rbind(archive, data.frame(iter = 1:length(thetav), x = unlist(thetav), y = unlist(out), Method = "Brent"))

p = p + geom_point(data = archive, aes(x = x, y = y, colour = Method), alpha = 0.5) + theme(axis.text.y=element_blank())

if (interactive()) print(p)
ggsave("../figure/poisson.pdf", p, width = 4, height = 2)
