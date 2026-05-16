# Used in: slides-multivar-first-order-1-GD.tex
#
# Sets up the two-dimensional gradient-descent example used for the manual screenshots
# in figure_man/gradient-descent-example_1.png and figure_man/gradient-descent-example_2.png.

set.seed(1L)

library(vistool)

objective_fun = function(x) {
  -sin(x[1]) * dnorm(x[2], mean = pi / 2, sd = 0.8)
}

objective = Objective$new(
  id = "gradient_descent_example",
  label = "f(x)",
  fun = objective_fun,
  xdim = 2L,
  lower = c(0, 0),
  upper = c(pi, pi),
  minimize = TRUE
)

optimizer = OptimizerGD$new(
  objective = objective,
  x_start = c(1, 0.1),
  lr = 0.4,
  print_trace = FALSE
)

optimizer$optimize(
  steps = 6L,
  step_size_control = step_size_control_line_search(lower = 0, upper = 4)
)

trace_visualizer = as_visualizer(objective, type = "surface")
trace_visualizer$add_optimization_trace(
  optimizer,
  add_marker_at = seq_len(nrow(optimizer$archive))
)

if (interactive()) {
  trace_visualizer$set_scene(x = 1, y = 1, z = 1.5)$plot(
    show_title = FALSE,
    show_legend = FALSE
  )
}
# ../figure_man/gradient-descent-example_1.png is a manual screenshot.

flattened_visualizer = as_visualizer(objective, type = "surface")

if (interactive()) {
  flattened_visualizer$plot(
    flatten = TRUE,
    show_title = FALSE,
    show_legend = FALSE
  )
}
# ../figure_man/gradient-descent-example_2.png is a manual screenshot.
