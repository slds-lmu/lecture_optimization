# Used in: lecture_optimization/slides/10-bayesian-optimization/slides-bayesian-optimization-3-bayesian-loop_1.tex
# Used in: lecture_optimization/slides/10-bayesian-optimization/slides-bayesian-optimization-3-bayesian-loop_2.tex
#
# Creates Expected Improvement figures showing exploration, exploitation,
# surrogate uncertainty, improvement relative to the incumbent, and BO updates.

library(bbotk)
library(data.table)
library(ggplot2)
library(mlr3learners)
library(mlr3mbo)
library(patchwork)

source("bo-helpers.R")
set.seed(123L)

objective = make_1d_objective()
instance = make_singlecrit_instance(objective)
instance$eval_batch(data.table(x = c(0.100, 0.300, 0.650, 1.000, 0.348, 0.400, 0.349)))

grid = make_grid(instance)

plot = plot_surrogate_1d(
  grid = grid,
  archive = instance$archive$data,
  y_limits = c(-2, 2.2),
  show_prediction = FALSE,
  show_uncertainty = FALSE
) +
  annotate("rect", xmin = 0.25, xmax = 0.45, ymin = -Inf, ymax = Inf, fill = "lightblue", alpha = 0.2) +
  annotate("rect", xmin = 0.70, xmax = 0.90, ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.2) +
  annotate("text", x = 0.35, y = -1.8, label = "well explored", colour = "lightblue4") +
  annotate("text", x = 0.80, y = -1.8, label = "insufficiently explored", colour = "indianred4")
save_figure(plot, "bayesian_loop_ee.png")

surrogate = make_km_surrogate(instance)
acq_function = acqf("ei", surrogate = surrogate)

update_ei_state = function() {
  update_surrogate_prediction(acq_function, grid, "x")
  add_acquisition_column(acq_function, grid, "x", "ei")
  best_grid_row(grid, "ei", minimize = FALSE)
}

ei_argmax = update_ei_state()

plot = plot_surrogate_1d(
  grid = grid,
  archive = instance$archive$data,
  y_limits = c(-2, 2.2)
)
save_figure(plot, "bayesian_loop_sm.png")

plot = plot_surrogate_1d(
  grid = grid,
  archive = instance$archive$data,
  y_limits = c(-2, 2.2),
  best_point = instance$archive$best()
)
save_figure(plot, "bayesian_loop_sm_fmin.png")

draw_normal_example = function(best_point = NULL, show_best_line = FALSE) {
  density_curve = normal_curve(
    y_limits = c(-2, 2.2),
    mean = ei_argmax$y_hat,
    se = ei_argmax$y_max - ei_argmax$y_hat
  )

  plot_surrogate_1d(
    grid = grid,
    archive = instance$archive$data,
    y_limits = c(-2, 2.2),
    best_point = best_point,
    show_best_line = show_best_line,
    candidate = ei_argmax,
    candidate_color = "darkgrey",
    density_curve = density_curve
  ) +
    geom_segment(
      aes(x = 0, xend = 0.1, y = ei_argmax$y_hat, yend = ei_argmax$y_hat),
      colour = "darkgrey"
    )
}

save_figure(draw_normal_example(), "bayesian_loop_sm_normal.png")
save_figure(
  draw_normal_example(best_point = instance$archive$best(), show_best_line = TRUE),
  "bayesian_loop_sm_normal_fmin.png"
)

instance$archive$clear()
instance$eval_batch(data.table(x = c(0.100, 0.300, 0.370, 0.650, 1.000)))

save_ei_iteration = function(filename, previous_point = NULL) {
  surrogate_plot = plot_surrogate_1d(
    grid = grid,
    archive = instance$archive$data,
    y_limits = c(-2, 2.2),
    previous_point = previous_point
  )
  ei_plot = plot_acquisition_1d(grid, "ei", ei_argmax, "EI")
  save_figure(surrogate_plot / ei_plot, filename)
}

ei_argmax = update_ei_state()
save_ei_iteration("bayesian_loop_1.png")

previous_argmax = ei_argmax
instance$eval_batch(ei_argmax[, .(x)])

for (iteration in 2L:6L) {
  ei_argmax = update_ei_state()
  save_ei_iteration(sprintf("bayesian_loop_%d.png", iteration), previous_argmax)

  previous_argmax = ei_argmax
  instance$eval_batch(ei_argmax[, .(x)])
}
