# Used in: slides-multivar-first-order-5-weaknesses-saddle.tex
#
# Sets up the saddle-point surface used for the manual screenshot in
# figure_man/saddlepoint.png.

set.seed(1L)

library(plotly)
library(vistool)

saddle_objective = Objective$new(
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

saddle_visualizer = as_visualizer(saddle_objective, type = "surface")
saddle_visualizer$add_contours()
saddle_visualizer$set_theme(vistool_theme(alpha = 1))

saddle_widget = layout(
  saddle_visualizer$plot(flatten = FALSE, show_title = FALSE, show_legend = FALSE),
  scene = list(
    aspectratio = list(x = 1, y = 1, z = 1.2),
    eye = list(x = 1, y = 1.5, z = 1.3)
  ),
  height = 500
)

if (interactive()) {
  print(saddle_widget)
}
# ../figure_man/saddlepoint.png is a manual screenshot.
