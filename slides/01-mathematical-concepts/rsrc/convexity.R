# ------------------------------------------------------------------------------
# mathematical concepts

# FIG: plot convexity characteristics
# ------------------------------------------------------------------------------

set.seed(1L)

library(ggplot2)

theme_set(theme_bw())

# DATA -------------------------------------------------------------------------

f1 = function(x) x^2 - 4 * x + 4
f2 = function(x) ifelse(x > 1, 0, 1 - x)

p = ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + stat_function(fun = f1) + xlim(c(0.5, 4.5))
p = p + geom_point(aes(x = 1, y = 1), size = 4) + geom_text(aes(x = 1, y = 0.7), label = "x", size = 6)
p = p + geom_point(aes(x = 4, y = 4), size = 4) + geom_text(aes(x = 4, y = 3.7), label = "y", size = 6)
p = p + geom_segment(aes(x = 1, y = 1, xend = 4, yend = 4), lty = 2) + geom_text(x = 2.5, y = 3, label = "f(x) + t[f(y) - f(x)]", color = "red", angle = 22, size = 6) + geom_point(aes(x = 2.5, y = 2.5), size = 5, color = "red")
p = p + geom_point(aes(x = 2.5, y = 0.25), color = "blue", size = 4) + geom_text(x = 3.2, y = 0.25, label = "f(x + t[y - x])", color = "blue", size = 6) + theme_bw()
p

ggsave("../figure/convexity_1.pdf", p, width = 6, height = 4)



p = ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + stat_function(fun = f2) + xlim(c(-1, 3))
p = p + geom_point(aes(x = 0, y = 1), size = 4) + geom_text(aes(x = 0, y = 0.8), label = "x", size = 6)
p = p + geom_point(aes(x = 2, y = 0), size = 4) + geom_text(aes(x = 2, y = - 0.2), label = "y", size = 6)
p = p + geom_segment(aes(x = 0, y = 1, xend = 2, yend = 0), lty = 2) + geom_text(x = 1, y = 0.7, label = "f(x) + t[f(y) - f(x)]", color = "red", angle = - 38, size = 6) + geom_point(aes(x = 1, y = 0.5), size = 5, color = "red")
p = p + geom_point(aes(x = 1, y = 0), color = "blue", size = 4) + geom_text(x = 1, y = -0.2, label = "f(x + t[y - x])", color = "blue", size = 6) + theme_bw()
p

ggsave("../figure/convexity_2.pdf", p, width = 6, height = 4)
