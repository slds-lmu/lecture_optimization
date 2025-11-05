# ------------------------------------------------------------------------------
# multivariate second order

# FIG: fit an exponential model to a dataset using nonlinear least squares (nls)
# ------------------------------------------------------------------------------

library(knitr)
library(microbenchmark)
library(snow)
library(colorspace)
library(ggplot2)
library(zoo)
library(gridExtra)
source("functions.R")

# ------------------------------------------------------------------------------

x = c(1, 2, 4, 5, 8)
y = c(3, 5, 6, 13, 20)
mod = nls(y ~ a*exp(b*x), start = list(a = 1, b = 0.2))

d = data.frame(x = x, y = y, pred = predict(mod))

plot = ggplot(data = d, aes(x = x, y = y)) + geom_point()
plot = plot + geom_smooth(method = "nls", formula = y ~ a*exp(b*x),method.args = list(start = c(a = 1, b = 0.2)), se = FALSE, color = "black" )
plot = plot + geom_point(aes(x = x, y = pred))
plot = plot + geom_segment(aes(x = x, y = y, xend = x, yend = pred), color = "red")
plot = plot + theme_bw()
plot

ggsave("../figure_man/squares.png", width = 4, height = 3)