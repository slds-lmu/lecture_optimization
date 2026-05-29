# Used in: lecture_optimization/slides/10-bayesian-optimization/slides-bayesian-optimization-5-noisy.tex
#
# Creates figures for Bayesian optimization with noisy observations, including
# noisy GP prediction, augmented EI, and reinterpolation.

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
instance$eval_batch(data.table(x = c(0.1, 0.3, 0.65, 1)))
instance$archive$data[, y := y + rnorm(4L, sd = 0.5)]

grid = make_grid(instance)

plot = plot_surrogate_1d(
  grid = grid,
  archive = instance$archive$data,
  y_limits = c(-2, 2.2),
  show_prediction = FALSE,
  show_uncertainty = FALSE
)
save_figure(plot, "noisy_0.png")

surrogate = make_km_surrogate(instance, nugget.stability = 1e-8)
surrogate$update()
add_prediction_columns(grid, surrogate, "x")

surrogate_noisy = make_km_surrogate(instance, nugget = 0.01, jitter = 1e-12)
surrogate_noisy$update()
add_prediction_columns(grid, surrogate_noisy, "x", prefix = "y_noisy")

plot = plot_surrogate_1d(
  grid = grid,
  archive = instance$archive$data,
  y_limits = c(-2, 2.4),
  prediction_prefix = "y_noisy"
)
save_figure(plot, "noisy_2.png")

acq_function = acqf("aei", surrogate = surrogate_noisy)
update_surrogate_prediction(acq_function, grid, "x", prefix = "y_noisy")
add_acquisition_column(acq_function, grid, "x", "aei")
aei_argmax = best_grid_row(grid, "aei", minimize = FALSE)

surrogate_plot = plot_surrogate_1d(
  grid = grid,
  archive = instance$archive$data,
  y_limits = c(-2, 2.4),
  prediction_prefix = "y_noisy"
)
aei_plot = plot_acquisition_1d(grid, "aei", aei_argmax, "AEI")
save_figure(surrogate_plot / aei_plot, "noisy_3.png")

reinterpolated_instance = instance$clone(deep = TRUE)
reinterpolated_prediction = surrogate_noisy$predict(instance$archive$data[, instance$archive$cols_x, with = FALSE])
reinterpolated_instance$archive$data[, y := reinterpolated_prediction$mean]

acq_function = acqf("ei", surrogate = surrogate)
acq_function$surrogate$archive = reinterpolated_instance$archive
update_surrogate_prediction(acq_function, grid, "x")
add_acquisition_column(acq_function, grid, "x", "ei")
ei_argmax = best_grid_row(grid, "ei", minimize = FALSE)

surrogate_plot = ggplot(grid, aes(x = x)) +
  geom_line(aes(y = y), colour = "black") +
  geom_line(aes(y = y_noisy_hat), colour = "grey55", linetype = 2) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "grey55", data = reinterpolated_instance$archive$data) +
  geom_ribbon(aes(ymin = y_min, ymax = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  geom_line(aes(y = y_hat), colour = "steelblue", linetype = 2) +
  coord_cartesian(xlim = c(0, 1), ylim = c(-2, 2.4)) +
  bo_theme()
ei_plot = plot_acquisition_1d(grid, "ei", ei_argmax, "EI")
save_figure(surrogate_plot / ei_plot, "noisy_4.png")

final_instance = instance$clone(deep = TRUE)
final_instance$archive$clear()
final_instance$eval_batch(data.table(x = c(0.1, 0.3, 0.65, 0.8, 1)))
final_instance$archive$data[, y_noisy := y + c(-0.1, -0.35, 0.3, 0.7, -0.05)]

plot = ggplot(grid, aes(x = x)) +
  geom_line(aes(y = y), colour = "black") +
  geom_segment(
    aes(x = x, xend = x, y = y, yend = y_noisy),
    colour = "orange",
    data = final_instance$archive$data
  ) +
  geom_point(aes(x = x, y = y_noisy), size = 3L, colour = "black", data = final_instance$archive$data) +
  annotate("text", x = 0.15, y = -1, label = "Overrated", colour = "orange") +
  annotate("text", x = 0.75, y = 1.5, label = "Underrated", colour = "orange") +
  coord_cartesian(xlim = c(0, 1), ylim = c(-2, 2.2)) +
  bo_theme()
save_figure(plot, "noisy_5.png")
