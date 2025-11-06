# Create ill-conditioned quadratic visualizations with vistool.
# Produces figure/ill-cond.png for the curvature slide deck.

set.seed(1L)
library(vistool)

ill_cond_obj = Objective$new(
	id = "ill_cond_quad",
	label = "Ill-conditioned quadratic",
	fun = function(x, Q) {
		drop(t(x) %*% Q %*% x)
	},
	xdim = 2L,
	lower = c(-10, -10),
	upper = c(10, 10),
	minimize = TRUE,
	Q = diag(c(1, 36))
)

lims = list(x1 = c(-10, 10), x2 = c(-10, 10))

vis = as_visualizer(
	ill_cond_obj,
	type = "surface"
)
vis$plot(
	flatten = FALSE,
	show_title = FALSE,
	show_legend = FALSE
)
vis$save("../figure/ill-cond_1.png")

vis$plot(
	flatten = TRUE,
	show_title = FALSE
)
vis$save("../figure/ill-cond_2.png")
