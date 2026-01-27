# ------------------------------------------------------------------------------
# univariate optimization

# FIG: plot empirical cumulative distribution function
# ------------------------------------------------------------------------------

library(ggplot2)

# ------------------------------------------------------------------------------


datap4 = data.frame(x = rnorm(5000))
p4 = ggplot(data = datap4, aes(x))
p4 = p4 + stat_ecdf(geom = "step")
p4 = p4 + geom_segment(aes(x = qnorm(0.75), y = 0.75, xend = -4, yend = 0.75), color = "black",linetype = "dashed")
p4 = p4 + geom_segment(aes(x = qnorm(0.75), y = 0.75, xend = qnorm(0.75), yend = 0), color = "black",linetype = "dashed")
p4 = p4 + ylab("F(x)")
p4 = p4 + theme_bw()
p4
ggsave("../figure_man/cal-quantiles.png", p4, width = 4, height = 3)

datap5 = data.frame(x025 = ((1:length(datap4$x)-1)/(length(datap4$x)-1))-(0.25),
                    x050 = ((1:length(datap4$x)-1)/(length(datap4$x)-1))-(0.5),
                    x075 = ((1:length(datap4$x)-1)/(length(datap4$x)-1))-(0.75),
                    y =  sort(datap4$x))
p5 = ggplot(data = datap5, aes(x = y))
p5 = p5 + geom_line(aes(y = x025, colour = "x025"))
p5 = p5 + geom_line(aes(y = x050, colour = "x050"))
p5 = p5 + geom_line(aes(y = x075, colour = "x075"))
p5 = p5 + geom_segment(aes(x = qnorm(0.75), y = 0, xend = qnorm(0.75), yend = -1), color = "black",linetype = "dashed")
p5 = p5 + geom_segment(aes(x = qnorm(0.75), y = 0, xend = -4, yend = 0), color = "black",linetype = "dashed")
p5 = p5 + theme(plot.title = element_text(size = 15))
p5 = p5 + scale_color_discrete(name = "",labels = c(expression(paste(alpha," = 0.25")),
                                                    expression(paste(alpha," = 0.50")),
                                                    expression(paste(alpha," = 0.75"))))
p5 = p5 + xlab("x")
p5 = p5 + ylab(expression(paste("g(x) = F(x) - ",alpha)))
p5 = p5 + theme_bw()
p5
ggsave("../figure_man/cal-quantiles2.png", p5, width = 4, height = 3)
