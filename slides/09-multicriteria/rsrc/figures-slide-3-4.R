

library(knitr)
library(ggplot2)
library(gridExtra)
library(ecr)
library(smoof)



df$apriori = df$mean_price + 50 * df$mean_rating

p1 = ggplot()
p1 = p1 + geom_point(data = df, aes(x = apriori, y = 0), size = 2.75)
p1 = p1 + geom_point(data = df[which.min(df$apriori), ], aes(x = apriori, y = 0), colour = "green", size = 2.75)
p1 = p1 + theme_bw()
p1 = p1 + xlab("Weighted sum")
p1 = p1 + theme(axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank())

ggsave("../14-optim-multicrit/figure_man/priori1.png", p1)



p2 = ggplot(data = df, aes(x = mean_price, y = mean_rating)) + geom_point(size = 2.75)
p2 = p2 + geom_point(data = df[which.min(df$apriori), ], aes(x = mean_price, y = mean_rating), size = 2.75, colour = "green")
p2 = p2 + theme_bw()
p2 = p2 + ylim(c(-5, -2))
p2 = p2 + xlab("Price per night") + ylab("Rating")

p=grid.arrange(p1, p2, ncol = 2)

ggsave("../14-optim-multicrit/figure_man/priori2.png", p2)




p1 = ggplot(data = df, aes(x = mean_price, y = mean_rating)) + geom_point(size = 2.75)
p1 = p1 + geom_point(data = df[df$mean_rating == -5, ], aes(x = mean_price, y = mean_rating), size = 2.75, colour = "orange")
p1 = p1 + theme_bw()
p1 = p1 + ylim(c(-5, -2))
p1 = p1 + ggtitle("1) max. rating")
p1 = p1 + xlab("Price per night") + ylab("Rating")

ggsave("../14-optim-multicrit/figure_man/priori3.png", p1)



p2 = p1 + geom_point(data = df[(df$mean_rating == - 5.0 & df$mean_price < 150), ], colour = "green", size = 2.75)
p2 = p2 + ggtitle("2) min. price")

grid.arrange(p1, p2, ncol = 2)

ggsave("../14-optim-multicrit/figure_man/priori4.png", p2)




P = df[order(df$mean_rating, df$mean_price,decreasing=FALSE),]
P = P[which(!duplicated(cummin(P$mean_price))),]

p1 = ggplot(data = df, aes(x = mean_price, y = mean_rating)) + geom_point(size = 2.75)
p1 = p1 + geom_point(data = P, aes(x = mean_price, y = mean_rating), size = 2.75, colour = "orange")
p1 = p1 + geom_line(data = P, aes(x = mean_price, y = mean_rating), colour = "orange")
p1 = p1 + theme_bw()
p1 = p1 + ylim(c(-5, -2))
p1 = p1 + xlab("Price per night") + ylab("Rating")

ggsave("../14-optim-multicrit/figure_man/posteriori1.png", p1)



p2 = p1 + geom_point(data = P[P$mean_rating == -4.0, ], aes(x = mean_price, y = mean_rating), colour = "green", size = 2.75)

grid.arrange(p1, p2, ncol = 2)

ggsave("../14-optim-multicrit/figure_man/posteriori2.png", p2)



