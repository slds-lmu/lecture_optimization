### This short script generates exemplary plots ofthe objectives plotted against each other 
### as well as against the decision varianble x

library(ggplot2)

fun = function(x) (x - 1)^2
p = ggplot(data.frame(x = c(0, 3)), aes(x)) + stat_function(fun = fun)
p = p + geom_point(x = 1, y = 0, color = "green", size = 3)
p = p + theme_bw() + ylab("f") + xlab(expression(x))
ggsave(p, filename = "../figure_man/graph1.png", height = 2, width = 2)

fun1 = function(x) (x - 1)^2
fun2 = function(x) 3 * (x - 2)^2
p = ggplot(data.frame(x = c(0, 3)), aes(x)) + stat_function(fun = fun1) + stat_function(fun = fun2, color = "blue")
p = p + theme_bw() +  ylab("f") + xlab(expression(lambda))

ggsave(p, filename = "../figure_man/graph2.png", height = 2, width = 2)

x = seq(0, 3, length.out = 1000)
xpareto = seq(1, 2, length.out = 1000)

p2 = ggplot() + geom_point(data = data.frame(f1 = fun1(x), f2 = fun2(x)), aes(x = f1, y = f2), size = 0.05) + geom_point(data = data.frame(f1 = fun1(xpareto), f2 = fun2(xpareto)), aes(x = f1, y = f2), color = "green", size = 0.05) + theme_bw()

ggsave(p2, filename = "../figure_man/graph3.png", height = 2, width = 2)
