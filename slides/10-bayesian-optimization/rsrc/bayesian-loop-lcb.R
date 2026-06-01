# Used in: lecture_optimization/slides/10-bayesian-optimization/slides-bayesian-optimization-3-bayesian-loop_1.tex
#
# Creates Lower Confidence Bound figures for increasing uncertainty weights.

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
instance$eval_batch(data.table(x = c(0.100, 0.300, 0.370, 0.650, 1.000)))

grid = make_grid(instance)
surrogate = make_km_surrogate(instance)
acq_function = acqf("cb", surrogate = surrogate)

update_lcb_state = function(tau) {
  acq_function$constants$values$lambda = tau
  update_surrogate_prediction(acq_function, grid, "x", se_multiplier = tau)
  add_acquisition_column(acq_function, grid, "x", "cb")
  best_grid_row(grid, "cb")
}

save_lcb_plot = function(index, tau) {
  cb_argmin = update_lcb_state(tau)
  surrogate_plot = plot_surrogate_1d(
    grid = grid,
    archive = instance$archive$data,
    y_limits = c(-2, 3.8),
    acquisition_line = "y_min"
  )
  cb_plot = plot_acquisition_1d(
    grid = grid,
    acq_col = "cb",
    candidate = cb_argmin,
    y_label = bquote("LCB, " ~ tau == .(tau)),
    y_limits = c(-2, 3.8)
  )
  save_figure(surrogate_plot / cb_plot, sprintf("bayesian_loop_lcb_%d.png", index))
}

save_lcb_plot(0L, 1)
save_lcb_plot(1L, 5)
save_lcb_plot(2L, 10)
