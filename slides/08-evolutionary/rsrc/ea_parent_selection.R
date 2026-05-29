# Used in: slides-evolutionary-algorithms-1-ea-intro.tex
#
# Creates a roulette-wheel parent-selection diagram. Ten individuals receive
# random positive fitness values, which are converted to proportional sampling
# probabilities and shown as slices of a polar bar chart.

set.seed(1L)

library(data.table)
library(ggplot2)

fitness_dt = data.table(
  individual = factor(seq_len(10L)),
  fitness = round(runif(10L, min = 1, max = 10), 1)
)
fitness_dt[, selection_probability := fitness / sum(fitness)]
setorder(fitness_dt, -fitness)

parent_selection_plot = ggplot(fitness_dt, aes(x = 1, y = selection_probability, fill = fitness)) +
  geom_col(width = 1, color = "white", linewidth = 0.4) +
  geom_text(
    aes(label = fitness),
    color = "white",
    fontface = "bold",
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  coord_polar(theta = "y", start = 0) +
  scale_fill_gradient(low = "#74a9cf", high = "#045a8d") +
  theme_void(base_size = 12) +
  theme(legend.position = "none")

dir.create("../figure", showWarnings = FALSE)

ggsave("../figure/ea_parent_selection.pdf", parent_selection_plot, width = 5, height = 5)
