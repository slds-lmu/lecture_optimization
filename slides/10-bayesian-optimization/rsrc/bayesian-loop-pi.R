# Used in: lecture_optimization/slides/10-bayesian-optimization/slides-bayesian-optimization-3-bayesian-loop_2.tex
#
# Creates Probability of Improvement figures for the predictive distribution at
# one point and for successive BO iterations.

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
surrogate = make_km_surrogate(instance, nugget = 1e-6)
acq_function = acqf("pi", surrogate = surrogate)

update_pi_state = function() {
  update_surrogate_prediction(acq_function, grid, "x")
  add_acquisition_column(acq_function, grid, "x", "pi")
  best_grid_row(grid, "pi", minimize = FALSE)
}

pi_argmax = update_pi_state()

density = data.table(y = seq(-2, 2.2, length.out = 1001L))
density[, value := dnorm(y, mean = grid$y_hat[1L], sd = grid$y_max[1L] - grid$y_hat[1L])]

plot = ggplot(density, aes(x = y, y = value)) +
  geom_area(data = density[y <= instance$archive$best()$y]) +
  geom_line(colour = "darkgrey") +
  geom_vline(xintercept = instance$archive$best()$y, colour = "#00A64F", linetype = 2) +
  labs(x = "Y(x)", y = "Density") +
  bo_theme()
save_figure(plot, "bayesian_loop_pi_0.png")

instance$archive$clear()
instance$eval_batch(data.table(x = c(0.100, 0.300, 0.370, 0.650, 1.000)))

save_pi_iteration = function(filename, previous_point = NULL) {
  surrogate_plot = plot_surrogate_1d(
    grid = grid,
    archive = instance$archive$data,
    y_limits = c(-2, 2.2),
    previous_point = previous_point
  )
  pi_plot = plot_acquisition_1d(grid, "pi", pi_argmax, "PI")
  save_figure(surrogate_plot / pi_plot, filename)
}

pi_argmax = update_pi_state()
save_pi_iteration("bayesian_loop_pi_1.png")

previous_argmax = pi_argmax
instance$eval_batch(pi_argmax[, .(x)])

for (iteration in 2L:9L) {
  pi_argmax = update_pi_state()
  save_pi_iteration(sprintf("bayesian_loop_pi_%d.png", iteration), previous_argmax)

  previous_argmax = pi_argmax
  instance$eval_batch(pi_argmax[, .(x)])
}
