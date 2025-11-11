# Create saddle point visualization with vistool.
# Produces figure/saddlepoint_*.png

set.seed(1L)
library(vistool)


saddle_obj = Objective$new(
	id = "saddle_point_quad",
	label = "Saddle point quadratic",
	fun = function(x, Q) {
		drop(t(x) %*% Q %*% x)
	},
	xdim = 2L,
	lower = c(-3, -3),
	upper = c(3, 3),
	minimize = TRUE,
	Q = matrix(c(1, 0, 0, -1), nrow = 2L, byrow = TRUE)
)

vis = as_visualizer(
	saddle_obj,
	type = "surface"
)
vis$add_contours()

my_theme = vistool_theme(
  alpha = 1
)
vis$set_theme(my_theme)

p = vis$plot(
	flatten = FALSE,
	show_title = FALSE,
	show_legend = FALSE
)
p = plotly::layout(
  p,
  scene = list(
    aspectratio = list(x = 1, y = 1, z = 1.2),
    eye = list(x = 1, y = 1.5, z = 1.3)
  ),
  height = 500
)
plotly::save_image(p, "../figure/saddlepoint.png", width = 700, height = 500)
