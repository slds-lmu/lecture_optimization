# Used in: ../slides-problems-1-unconstrained.tex, ../slides-problems-2-constrained.tex
#
# Visualize the hinge loss with vistool and save the figure for the
# optimization slides.

set.seed(1L)

library(ggplot2)
library(vistool)

options(vistool.theme = vistool_theme(
  palette = "plasma"
))

# Build and save the hinge-loss visualization.
vis_hinge = as_visualizer(lss("hinge"))
plot_hinge = vis_hinge$plot()

ggsave("../figure/hinge.png", plot_hinge, width = 3, height = 2)
