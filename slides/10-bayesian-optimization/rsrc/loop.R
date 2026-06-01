# Used in: lecture_optimization/slides/10-bayesian-optimization/slides-bayesian-optimization-2-loop.tex
#
# Builds the step-by-step loop figures for model-based optimization with the
# surrogate posterior mean as acquisition function.

library(bbotk)
library(data.table)
library(ggplot2)
library(mlr3learners)
library(mlr3mbo)

source("bo-helpers.R")
set.seed(123L)

objective = make_1d_objective()
instance = make_singlecrit_instance(objective)
instance$eval_batch(data.table(x = c(0.1, 0.3, 0.65, 1)))

surrogate = make_km_surrogate(instance)
acq_function = acqf("mean", surrogate = surrogate)
grid = make_grid(instance)

refit_mean_surrogate = function() {
  update_surrogate_prediction(acq_function, grid, "x")
  add_acquisition_column(acq_function, grid, "x", "mean")
  best_grid_row(grid, "mean")
}

save_loop_plot = function(filename, show_truth = FALSE, show_prediction = TRUE, candidate = NULL) {
  plot = plot_surrogate_1d(
    grid = grid,
    archive = instance$archive$data,
    y_limits = c(-2, 2.2),
    show_truth = show_truth,
    show_prediction = show_prediction,
    show_uncertainty = FALSE,
    candidate = candidate
  )
  save_figure(plot, filename)
}

mean_argmin = refit_mean_surrogate()

plot = plot_surrogate_1d(
  grid = grid,
  archive = NULL,
  y_limits = c(-2, 2.2),
  show_truth = TRUE,
  show_prediction = FALSE,
  show_uncertainty = FALSE
)
save_figure(plot, "loop_0.png")

plot = plot_surrogate_1d(
  grid = grid,
  archive = instance$archive$data,
  y_limits = c(-2, 2.2),
  show_truth = FALSE,
  show_prediction = FALSE,
  show_uncertainty = FALSE
)
save_figure(plot, "loop_1.png")

save_loop_plot("loop_2.png")
save_loop_plot("loop_3.png", candidate = mean_argmin)

instance$eval_batch(mean_argmin[, .(x)])
save_loop_plot("loop_4.png")

mean_argmin = refit_mean_surrogate()
save_loop_plot("loop_5.png")
save_loop_plot("loop_6.png", candidate = mean_argmin)

instance$eval_batch(mean_argmin[, .(x)])
save_loop_plot("loop_7.png")

mean_argmin = refit_mean_surrogate()
save_loop_plot("loop_8.png")
save_loop_plot("loop_9.png", candidate = mean_argmin)

instance$eval_batch(mean_argmin[, .(x)])
save_loop_plot("loop_10.png")
save_loop_plot("loop_11.png", show_truth = TRUE)
