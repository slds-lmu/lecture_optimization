library(ggplot2)

df = data.frame(x1 = c(1, 3), x2 = c(5, 1))
df$label = c("original", "original")

df = rbind(df, c(3, 5, "uniform"))
df$x1 = as.numeric(df$x1)
df$x2 = as.numeric(df$x2)

x = df[1, 1:2]
xt = df[2, 1:2]
intermediate = 0.5 * (x + xt)

beta = 0.4
sbx1 = intermediate + 0.5 * beta * (xt - x)
sbx2 = intermediate - 0.5 * beta * (xt - x)

df = rbind(df, c(intermediate[, 1], intermediate[, 2], "intermediate"))
df = rbind(df, c(sbx1[, 1], sbx1[, 2], "sbx"))
df = rbind(df, c(sbx2[, 1], sbx2[, 2], "sbx"))
df$x1 = as.numeric(df$x1)
df$x2 = as.numeric(df$x2)


p = ggplot(data = df, aes(x = x1, y = x2, colour = label, label = label)) + geom_point()
p = p + geom_segment(aes(x=df[1, 1], xend=df[2, 1], y=df[1, 2], yend=df[2, 2]), linetype=2, color = "black")
p = p + geom_segment(aes(x=df[1, 1], xend=df[3, 1], y=df[1, 2], yend=df[3, 2]), linetype=2, color = "black")
p = p + geom_segment(aes(x=df[2, 1], xend=df[3, 1], y=df[2, 2], yend=df[3, 2]), linetype=2, color = "black")
p = p + geom_label(aes(fill = label), colour = "white", fontface = "bold")
p = p + theme_bw() + theme(legend.position = "none")
p = p + xlim(c(0.5, 3.5)) + ylim(c(0.5, 5.5))

ggsave("figure_man/ea_recombination_numeric.pdf", p, width = 3, height = 3)