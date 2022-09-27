library(ggplot2)

theme_set(theme_bw())

f1 = function(x) x^2
f2 = function(x) {
	ifelse(x > 1, 0, 1 - x)
}
f3 = function(x) {
	ifelse(x > 0, 0, 1)
}

df = data.frame(x = seq(-2, 2, by = 0.01))
df$L2 = f1(df$x)
df$Hinge = f2(df$x)
df$ZeroOne = f3(df$x)

df = reshape::melt(df, id.vars = "x")
names(df) = c("x", "Loss", "y")
p = ggplot(data = df, aes(x = x, y = y, colour = Loss)) + geom_line()

ggsave("figure_man/hinge_vs_l2.pdf", p, width = 3, height = 2)


p = ggplot(data = df[df$Loss == "Hinge", ], aes(x = x, y = y, colour = Loss)) + geom_line()
p = p + xlab("yf(x)") + ylab("L(y, f(x))")

ggsave("figure_man/hinge.pdf", p, width = 3, height = 2)

