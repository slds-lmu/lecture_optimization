set.seed(1L)
library(vistool)
options(vistool.theme = vistool_theme(
  palette = "plasma"
))

loss_l2_classif = LossFunction$new(
	id = "l2_margin",
	label = "L2 Loss",
	task_type = "classif",
	fun = function(r) r^2
)
loss_hinge = lss("hinge")
loss_zero_one = lss("zero-one")

vis_losses = as_visualizer(
	list(loss_hinge, loss_l2_classif, loss_zero_one),
	input_type = "score",
	n_points = 1500L
)

plot_losses = vis_losses$plot(
	x_limits = c(-2, 2),
	y_limits = c(0, 4)
)

ggplot2::ggsave("../figure/hinge_vs_l2.png", plot_losses, width = 3, height = 2)

vis_hinge = as_visualizer(loss_hinge)
plot_hinge = vis_hinge$plot()

ggplot2::ggsave("../figure/hinge.png", plot_hinge, width = 3, height = 2)
