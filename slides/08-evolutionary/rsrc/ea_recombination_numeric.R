# Used in: slides-evolutionary-algorithms-2-ea-numeric.tex
#
# Compares three numeric recombination examples for two parent vectors:
# uniform crossover, intermediate recombination, and simulated binary crossover
# (SBX). The geometry is deterministic so the diagram stays stable.

set.seed(1L)

library(data.table)
library(ggplot2)

parent_a = c(x1 = 1, x2 = 5)
parent_b = c(x1 = 3, x2 = 1)
uniform_child = c(x1 = parent_b[["x1"]], x2 = parent_a[["x2"]])
intermediate_child = 0.5 * (parent_a + parent_b)

beta = 0.4
sbx_offset = 0.5 * beta * (parent_b - parent_a)
sbx_children = rbind(
  intermediate_child + sbx_offset,
  intermediate_child - sbx_offset
)

point_dt = rbindlist(list(
  data.table(x1 = parent_a[["x1"]], x2 = parent_a[["x2"]], label = "parent 1", type = "Parent"),
  data.table(x1 = parent_b[["x1"]], x2 = parent_b[["x2"]], label = "parent 2", type = "Parent"),
  data.table(x1 = uniform_child[["x1"]], x2 = uniform_child[["x2"]], label = "uniform", type = "Uniform"),
  data.table(
    x1 = intermediate_child[["x1"]],
    x2 = intermediate_child[["x2"]],
    label = "intermediate",
    type = "Intermediate"
  ),
  data.table(x1 = sbx_children[, "x1"], x2 = sbx_children[, "x2"], label = "SBX", type = "SBX")
))

segment_dt = data.table(
  x1 = c(parent_a[["x1"]], parent_a[["x1"]], parent_b[["x1"]]),
  x2 = c(parent_a[["x2"]], parent_a[["x2"]], parent_b[["x2"]]),
  xend = c(parent_b[["x1"]], uniform_child[["x1"]], uniform_child[["x1"]]),
  yend = c(parent_b[["x2"]], uniform_child[["x2"]], uniform_child[["x2"]])
)

recombination_plot = ggplot(point_dt, aes(x = x1, y = x2)) +
  geom_segment(
    data = segment_dt,
    aes(x = x1, y = x2, xend = xend, yend = yend),
    color = "grey35",
    linetype = "dashed",
    inherit.aes = FALSE
  ) +
  geom_point(aes(color = type), size = 3) +
  geom_label(aes(label = label, fill = type), color = "white", fontface = "bold", size = 3, linewidth = 0) +
  scale_color_manual(values = c(Parent = "#252525", Uniform = "#3182bd", Intermediate = "#31a354", SBX = "#de2d26")) +
  scale_fill_manual(values = c(Parent = "#252525", Uniform = "#3182bd", Intermediate = "#31a354", SBX = "#de2d26")) +
  coord_cartesian(xlim = c(0.5, 3.5), ylim = c(0.5, 5.5), expand = FALSE) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none") +
  labs(x = expression(x[1]), y = expression(x[2]))

dir.create("../figure", showWarnings = FALSE)

ggsave("../figure/ea_recombination_numeric.pdf", recombination_plot, width = 3, height = 3)
