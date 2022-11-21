library(knitr)
library(microbenchmark)
library(snow)
library(colorspace)
library(ggplot2)
library(zoo)
library(gridExtra)
source("rsrc/functions.R")


f = function(x) - x^3 + 6 * x^2 - 9 * x + 2
tangent = function(x) - 9 * x + 2
tangent_red = function(x) 0.05 * (- 9) * x + 2

sp = uniroot(function(x) f(x) - tangent_red(x), interval = c(1, 3))$root
x = seq(0, sp, by = 0.01)
d = data.frame(x = x, y = f(x))

plot = ggplot(data = data.frame(x = 0), aes(x = x))
plot = plot + geom_point(aes(x = 0, y = 2)) + geom_text(aes(x = 0, y = 2.1, label = "f(x)"), parse = T)
plot = plot + stat_function(fun = f)
plot = plot + geom_text(aes(x = 3, y = 2.2, label = "f(x+alpha~d)"), parse = T)

plot = plot + stat_function(fun = tangent, color = "grey")
plot = plot + geom_text(aes(x = 0.1, y = -1.3, label = "Tangent"), color = "grey")
plot = plot + geom_text(aes(x = 0.1, y = -1.7, label = "alpha~nabla~f(x)^T~d"), parse = T, color = "grey")

plot = plot + stat_function(fun = tangent_red, color = "blue", lty = 2)
plot = plot + geom_text(aes(x = 1.5, y = 2, label = "tapered tangent"), color = "blue")
plot = plot + geom_text(aes(x = 1.5, y = 1.7, label = "gamma~alpha~nabla~f(x)^T~d"), parse = T, color = "blue")

plot = plot + geom_line(data = d, aes(x = x, y = y), color = "red")
plot = plot + geom_text(aes(x = 1, y = -1, label = "Armijo rule holds"), color = "red")

plot = plot + xlab(expression(alpha)) + ylab("y")
plot = plot + xlim(c(0, 4)) + ylim(c(-2, 3)) + theme_bw()

ggsave(filename = paste0("figure_man/armijo.pdf"), plot, width = 6, height = 4)
