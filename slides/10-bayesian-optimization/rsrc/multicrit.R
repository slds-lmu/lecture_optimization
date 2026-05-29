# Used in: lecture_optimization/slides/10-bayesian-optimization/slides-bayesian-optimization-6-multicrit.tex
#
# Creates multi-objective BO figures: dominated vs. Pareto points, dominated
# hypervolume, the Pareto front, and ParEGO scalarization with EI.

library(bbotk)
library(data.table)
library(ggplot2)
library(mlr3learners)
library(mlr3mbo)
library(patchwork)

source("bo-helpers.R")
set.seed(123L)

objective = ObjectiveRFunDt$new(
  fun = function(xdt) data.table(y1 = xdt$x^2, y2 = (xdt$x - 2)^2),
  domain = ps(x = p_dbl(lower = -1, upper = 3)),
  codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
)
instance = OptimInstanceBatchMultiCrit$new(
  objective = objective,
  terminator = trm("none")
)

label_pareto_points = function(data) {
  data[, type := "dominated"]
  data[!is_dominated(t(data[, .(y1, y2)])), type := "Pareto"]
  data
}

plot_pareto_points = function(data) {
  ggplot(data, aes(x = y1, y = y2)) +
    geom_point(aes(colour = type), size = 3L, data = data[type == "dominated"]) +
    geom_point(aes(colour = type), size = 3L, data = data[type == "Pareto"]) +
    geom_step(direction = "hv", data = data[type == "Pareto"]) +
    labs(x = expression(y[1]), y = expression(y[2]), colour = "") +
    coord_cartesian(xlim = c(0, 9), ylim = c(0, 9)) +
    bo_theme() +
    theme(legend.position = "bottom")
}

instance$eval_batch(generate_design_random(instance$search_space, n = 100L)$data)
label_pareto_points(instance$archive$data)
save_figure(plot_pareto_points(instance$archive$data), "multicrit_0.png")

pareto_data = instance$archive$data[type == "Pareto"][, .(y1, y2)]
setorderv(pareto_data, cols = "y1")
pareto_data = pareto_data[c(10L, 15L, 25L, 35L), ]

polygon_data = copy(pareto_data)
polygon_data = rbindlist(lapply(seq_len(nrow(polygon_data)), function(row_id) {
  if (row_id == nrow(polygon_data)) {
    polygon_data[row_id, ]
  } else {
    rbind(polygon_data[row_id, ], data.table(y1 = polygon_data[row_id + 1L, y1], y2 = polygon_data[row_id, y2]))
  }
}))
polygon_data = rbind(
  data.table(y1 = c(9, polygon_data$y1[1L]), y2 = c(9, 9)),
  polygon_data,
  data.table(y1 = 9, y2 = polygon_data$y2[nrow(polygon_data)])
)

plot = ggplot(pareto_data, aes(x = y1, y = y2)) +
  geom_polygon(aes(x = y1, y = y2), data = polygon_data, alpha = 0.2) +
  geom_point(size = 3L) +
  geom_point(aes(x = 9, y = 9), colour = "darkred", size = 3L) +
  geom_step(direction = "hv") +
  labs(x = expression(y[1]), y = expression(y[2]), colour = "") +
  coord_cartesian(xlim = c(0, 9), ylim = c(0, 9)) +
  bo_theme() +
  theme(legend.position = "bottom")
save_figure(plot, "multicrit_1.png")

instance$archive$clear()
instance$eval_batch(data.table(x = c(-0.9, 0.1, 1.1, 2.3)))

grid = make_grid(instance)
grid[, type := "dominated"]
grid[!is_dominated(t(grid[, .(y1, y2)])), type := "pareto"]

plot_pareto_front = function(previous_point = NULL) {
  plot = ggplot() +
    geom_line(aes(x = y1, y = y2), data = grid[type == "pareto"]) +
    geom_point(aes(x = y1, y = y2), size = 3L, colour = "black", data = instance$archive$data) +
    labs(x = expression(y[1]), y = expression(y[2])) +
    bo_theme()

  if (!is.null(previous_point)) {
    plot = plot + geom_point(aes(x = y1, y = y2), size = 3L, colour = "grey55", data = previous_point)
  }

  plot
}

save_figure(plot_pareto_front(), "multicrit_2.png")

scale_unit = function(y) {
  span = diff(range(y, na.rm = TRUE))
  if (span == 0) {
    rep(0, length(y))
  } else {
    (y - min(y, na.rm = TRUE)) / span
  }
}

scalarize_archive = function() {
  archive = instance$archive$data
  scaled_objectives = archive[, lapply(.SD, scale_unit), .SDcols = c("y1", "y2")]
  lambda = lambdas[sample.int(nrow(lambdas), 1L), , drop = TRUE]
  weighted_objectives = Map("*", scaled_objectives, lambda)
  weighted_sum = Reduce("+", weighted_objectives)
  archive[, y_scal := do.call(pmax, weighted_objectives) + 0.05 * weighted_sum]
}

update_parego_state = function() {
  scalarize_archive()
  acq_function$surrogate$update()
  add_prediction_columns(grid, acq_function$surrogate, "x")
  add_acquisition_column(acq_function, grid, "x", "ei")
  best_grid_row(grid, "ei", minimize = FALSE)
}

plot_scalarized_surrogate = function() {
  ggplot() +
    geom_point(aes(x = x, y = y_scal), size = 3L, colour = "black", data = instance$archive$data) +
    geom_ribbon(aes(x = x, ymin = y_min, ymax = y_max), fill = "steelblue", colour = NA, alpha = 0.1, data = grid) +
    geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2, data = grid) +
    coord_cartesian(xlim = c(-1, 3)) +
    labs(x = NULL, y = expression(y[scalarized])) +
    bo_theme()
}

plot_ei = function(candidate) {
  ggplot(grid, aes(x = x, y = ei)) +
    geom_line(colour = "darkred") +
    geom_point(aes(x = x, y = ei), size = 3L, colour = "darkred", data = candidate) +
    coord_cartesian(xlim = c(-1, 3)) +
    labs(x = NULL, y = "EI") +
    bo_theme()
}

lambdas = calculate_parego_weights(s = 100L, k = 2L)
surrogate = make_km_surrogate(instance, nugget.stability = 1e-8)
surrogate$cols_y = "y_scal"
acq_function = acqf("ei", surrogate = surrogate)

ei_argmax = update_parego_state()
save_figure(plot_pareto_front() + (plot_scalarized_surrogate() / plot_ei(ei_argmax)), "multicrit_3.png",
  width = 10, height = 4
)

previous_argmax = ei_argmax
instance$eval_batch(ei_argmax[, .(x)])

ei_argmax = update_parego_state()
save_figure(
  plot_pareto_front(previous_argmax) + (plot_scalarized_surrogate() / plot_ei(ei_argmax)),
  "multicrit_4.png",
  width = 10,
  height = 4
)
