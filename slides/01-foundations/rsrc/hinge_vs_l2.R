# Used in: ../01-diff.tex
#
# Compare a smooth quadratic loss, the hinge loss, and the discontinuous 0-1
# loss on the same axis.

set.seed(1L)

library(data.table)
library(ggplot2)

dir.create("../figure", recursive = TRUE, showWarnings = FALSE)

loss_data = data.table(x = seq(-2, 2, by = 0.01))
loss_data[, l2 := x^2]
loss_data[, hinge := ifelse(x > 1, 0, 1 - x)]
loss_data[, zero_one := ifelse(x > 0, 0, 1)]

plot_data = melt(
  loss_data,
  id.vars = "x",
  variable.name = "loss",
  value.name = "value"
)
plot_data[, loss := factor(loss, levels = c("l2", "hinge", "zero_one"))]

loss_plot = ggplot(plot_data, aes(x = x, y = value, colour = loss)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(
    values = c(l2 = "#0072B2", hinge = "#D55E00", zero_one = "#009E73"),
    labels = c(l2 = "L2", hinge = "Hinge", zero_one = "0-1")
  ) +
  labs(x = "x", y = "Loss", colour = NULL) +
  theme_bw(base_size = 14)

ggsave("../figure/hinge_vs_l2.pdf", loss_plot, width = 3.4, height = 2.6)
