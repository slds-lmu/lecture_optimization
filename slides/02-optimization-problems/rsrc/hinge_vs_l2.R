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
df$f1 = f1(df$x)
df$f2 = f2(df$x)
df$f3 = f3(df$x)

df = reshape::melt(df, id.vars = "x")
names(df) = c("x", "fun", "y")
p = ggplot(data = df, aes(x = x, y = y, colour = fun)) + geom_line()

ggsave("figure_man/hinge_vs_l2.pdf", p, width = 3, height = 2)
