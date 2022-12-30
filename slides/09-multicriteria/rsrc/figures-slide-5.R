

library(knitr)
library(ggplot2)
library(gridExtra)
library(ecr)
library(smoof)


x = seq(-1, 4, length.out = 1000)
lin = 3 * 0.4 - 2 * x
fun1 = function(x) (x - 1)^2
fun2 = function(x) 3 * (x - 2)^2

p2 = ggplot() + geom_point(data = data.frame(f1 = fun1(x), f2 = fun2(x)), aes(x = f1, y = f2), size = 0.7)
p2 = p2 + geom_line(aes(x = x, y = lin)) + ylim(c(-3, 25))
p2 = p2 + geom_point(aes(x = 0.36, y = 0.48), colour = "green", size = 3)
p2 = p2 + theme_bw()
p2

ggsave("../14-optim-multicrit/figure_man/emo.png", p2)



f1 = function(x) 0.01 * sum(x^2) - 2
f2 = function(x) 0.01 * sum(c(0.1, 0.3) * (x - c(-10, 20))^2)

x1 = x2 = seq(-10, 20, length.out = 100)
grid = expand.grid(x1 = x1, x2 = x2)
grid$y1 = apply(grid[, 1:2], 1, f1)
grid$y2 = apply(grid[, 1:2], 1, f2)

melt = reshape2::melt(grid, id.vars = c("x1", "x2"))

p = ggplot(data = melt) + geom_raster(aes(x = x1, y = x2, fill = value))
p = p + geom_contour(aes(x = x1, y = x2, z = value, colour = variable), bins = 15)
p = p + ylim(c(-20, 40)) + xlim(c(-20, 40)) + theme_bw()
p

ggsave("../14-optim-multicrit/figure_man/1.png", p)
