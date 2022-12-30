
library(knitr)
library(ggplot2)
library(gridExtra)
library(ecr)
library(smoof)

df = readRDS("../14-optim-multicrit/rsrc/expedia_example.rds")

p = ggplot(data = df, aes(x = mean_price, y = - mean_rating)) + geom_point(size = 3.5)
p = p + theme_bw()
p = p + ylim(c(- 5.5, -2))
p = p + xlab("Price per night") + ylab("Rating")


ggsave("../14-optim-multicrit/figure_man/price-per-night.png", p)


p1 = ggplot(data = df, aes(x = mean_price, y = - mean_rating)) + geom_point(size = 3.5)
p1 = p1 + geom_point(data = df[16:17, ], aes(x = mean_price, y = - mean_rating), size = 4, colour = c("green", "red"))
p1 = p1 + theme_bw()
p1 = p1 + ylim(c(- 5.5, -2))
p1 = p1 + xlab("Price per night") + ylab("Rating")

ggsave("../14-optim-multicrit/figure_man/Example1.png", p1)


p2 = ggplot(data = df, aes(x = mean_price, y = - mean_rating)) + geom_point(size = 3.5)
p2 = p2 + geom_point(data = df[c(10, 16), ], aes(x = mean_price, y = - mean_rating), size = 4, colour = "orange")
p2 = p2 + theme_bw()
p2 = p2 + ylim(c(-5.5, -2))
p2 = p2 + xlab("Price per night") + ylab("Rating")


ggsave("../14-optim-multicrit/figure_man/Example2.png", p2)




df$mean_rating = - df$mean_rating
P = df[order(df$mean_rating, df$mean_price,decreasing=FALSE),]
P = P[which(!duplicated(cummin(P$mean_price))),]

p2 = ggplot(data = df, aes(x = mean_price, y = mean_rating)) + geom_point(size = 2)
p2 = p2 + geom_point(data = P, aes(x = mean_price, y = mean_rating), size = 2, colour = "orange")
p2 = p2 + geom_line(data = P, aes(x = mean_price, y = mean_rating), colour = "orange")
p2 = p2 + theme_bw()
p2 = p2 + ylim(c(-5, -2))
p2 = p2 + xlab("Price per night") + ylab("Rating")

p2

ggsave("../14-optim-multicrit/figure_man/pareto.png", p2)





fun = function(x) (x - 1)^2
p = ggplot(data.frame(x = c(0, 3)), aes(x)) + stat_function(fun = fun)
p = p + geom_point(x = 1, y = 0, color = "green", size = 3)
p = p + theme_bw()
p

ggsave("../14-optim-multicrit/figure_man/one-obj-func.png", p)



fun1 = function(x) (x - 1)^2
fun2 = function(x) 3 * (x - 2)^2
p = ggplot(data.frame(x = c(0, 3)), aes(x)) + stat_function(fun = fun1) + stat_function(fun = fun2, color = "blue")
p = p + theme_bw()
p

ggsave("../14-optim-multicrit/figure_man/two-target-func.png", p)
       
       



x = seq(0, 3, length.out = 1000)
xpareto = seq(1, 2, length.out = 1000)

p2 = ggplot() + geom_point(data = data.frame(f1 = fun1(x), f2 = fun2(x)), aes(x = f1, y = f2), size = 0.05) + geom_point(data = data.frame(f1 = fun1(xpareto), f2 = fun2(xpareto)), aes(x = f1, y = f2), color = "green", size = 0.05) + theme_bw()
p2

ggsave("../14-optim-multicrit/figure_man/pareto-front.png", p2)     
       
