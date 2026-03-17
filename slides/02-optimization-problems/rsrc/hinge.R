set.seed(1L)
library(vistool)
options(vistool.theme = vistool_theme(
  palette = "plasma"
))

vis_hinge = as_visualizer(lss("hinge"))
plot_hinge = vis_hinge$plot()

ggplot2::ggsave("../figure/hinge.png", plot_hinge, width = 3, height = 2)
