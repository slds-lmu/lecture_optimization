# ------------------------------------------------------------------------------
# mathematical concepts

# FIG: plot hinge loss vs l2 loss, 0-1 loss
# ------------------------------------------------------------------------------

library(ggplot2)
library(reshape)
theme_set(theme_bw())

# DATA -------------------------------------------------------------------------

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

df = melt(df, id.vars = "x")
names(df) = c("x", "Loss", "y")

# PLOT -------------------------------------------------------------------------

p = ggplot(data = df, aes(x = x, y = y, colour = Loss)) + geom_line()
p
ggsave("../figure_man/hinge_vs_l2.pdf", p, width = 3, height = 2)

