# Used in: lecture_optimization/slides/10-bayesian-optimization/rsrc/*.R
#
# Shared utilities for Bayesian optimization slide figures. These helpers keep
# objective definitions, prediction columns, plot styling, and figure saving
# consistent across the chapter scripts.

library(bbotk)
library(data.table)
library(ggplot2)
library(lgr)
library(mlr3mbo)
library(mlr3learners)

set.seed(1L)

get_logger("bbotk")$set_threshold("warn")
get_logger("mlr3")$set_threshold("warn")

calculate_parego_weights = getFromNamespace("calculate_parego_weights", "mlr3mbo")

figure_path = function(filename) {
  file.path("..", "figure", filename)
}

save_figure = function(plot, filename, width = 5, height = 4) {
  path = figure_path(filename)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggsave(path, plot = plot, width = width, height = height, dpi = 300)
}

bo_theme = function(base_size = 13L) {
  theme_minimal(base_size = base_size)
}

target_1d = function(x) {
  2 * x * sin(14 * x)
}

make_1d_objective = function() {
  ObjectiveRFunDt$new(
    fun = function(xdt) data.table(y = target_1d(xdt$x)),
    domain = ps(x = p_dbl(lower = 0, upper = 1)),
    codomain = ps(y = p_dbl(tags = "minimize"))
  )
}

make_ackley_objective = function() {
  ObjectiveRFunDt$new(
    fun = function(xdt) {
      y = -20 * exp(-0.2 * sqrt(0.5 * (xdt$x1^2 + xdt$x2^2))) -
        exp(0.5 * (cos(2 * pi * xdt$x1) + cos(2 * pi * xdt$x2))) + exp(1) + 20
      data.table(y = y)
    },
    domain = ps(x1 = p_dbl(lower = -5, upper = 5), x2 = p_dbl(lower = -5, upper = 5)),
    codomain = ps(y = p_dbl(tags = "minimize"))
  )
}

make_singlecrit_instance = function(objective, terminator = trm("none")) {
  OptimInstanceBatchSingleCrit$new(
    objective = objective,
    terminator = terminator
  )
}

make_grid = function(instance, resolution = 1001L) {
  grid = generate_design_grid(instance$search_space, resolution = resolution)$data
  values = instance$objective$eval_dt(grid[, instance$archive$cols_x, with = FALSE])
  for (col in names(values)) {
    set(grid, j = col, value = values[[col]])
  }
  grid
}

make_km_surrogate = function(instance = NULL, covtype = "matern5_2", optim_method = "BFGS",
                             control = list(trace = FALSE), ...) {
  learner = lrn("regr.km", covtype = covtype, optim.method = optim_method, control = control, ...)
  if (is.null(instance)) {
    srlrn(learner)
  } else {
    srlrn(learner, archive = instance$archive)
  }
}

add_prediction_columns = function(grid, surrogate, x_cols, prefix = "y", se_multiplier = 1) {
  prediction = surrogate$predict(grid[, x_cols, with = FALSE])
  set(grid, j = paste0(prefix, "_hat"), value = prediction$mean)
  set(grid, j = paste0(prefix, "_min"), value = prediction$mean - se_multiplier * prediction$se)
  set(grid, j = paste0(prefix, "_max"), value = prediction$mean + se_multiplier * prediction$se)
  prediction
}

update_surrogate_prediction = function(acq_function, grid, x_cols, prefix = "y", se_multiplier = 1) {
  acq_function$surrogate$update()
  add_prediction_columns(grid, acq_function$surrogate, x_cols, prefix, se_multiplier)
}

add_acquisition_column = function(acq_function, grid, x_cols, acq_id) {
  acq_function$update()
  values = acq_function$eval_dt(grid[, x_cols, with = FALSE])
  set(grid, j = acq_id, value = values[[paste0("acq_", acq_id)]])
  grid
}

best_grid_row = function(grid, column, minimize = TRUE) {
  index = if (minimize) which.min(grid[[column]]) else which.max(grid[[column]])
  grid[index, ]
}

normal_curve = function(y_limits, mean, se, width = 0.1, by = 0.01) {
  curve = data.table(y = seq(y_limits[1L], y_limits[2L], by = by))
  density = dnorm(curve$y, mean = mean, sd = se)
  set(curve, j = "x", value = width * density / max(density))
  curve
}

plot_design_2d = function(data, title, x_col = "x1", y_col = "x2") {
  ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_point(size = 3L) +
    geom_rug() +
    labs(title = title, x = expression(x[1]), y = expression(x[2])) +
    bo_theme()
}

plot_surrogate_1d = function(grid, archive, y_limits = c(-2, 2.2), show_truth = TRUE,
                             show_prediction = TRUE, show_uncertainty = TRUE,
                             prediction_prefix = "y", acquisition_line = NULL,
                             previous_point = NULL, best_point = NULL,
                             show_best_line = FALSE, candidate = NULL,
                             candidate_y_col = paste0(prediction_prefix, "_hat"),
                             candidate_color = "darkred", density_curve = NULL) {
  y_hat_col = paste0(prediction_prefix, "_hat")
  y_min_col = paste0(prediction_prefix, "_min")
  y_max_col = paste0(prediction_prefix, "_max")

  plot = ggplot(grid, aes(x = x))

  if (show_truth) {
    plot = plot + geom_line(aes(y = y), colour = "black")
  }
  if (show_uncertainty) {
    plot = plot +
      geom_ribbon(aes(ymin = .data[[y_min_col]], ymax = .data[[y_max_col]]),
        fill = "steelblue", colour = NA, alpha = 0.1
      )
  }
  if (show_prediction) {
    plot = plot + geom_line(aes(y = .data[[y_hat_col]]), colour = "steelblue", linetype = 2)
  }
  if (!is.null(acquisition_line)) {
    plot = plot + geom_line(aes(y = .data[[acquisition_line]]), colour = "darkred")
  }
  if (!is.null(archive)) {
    plot = plot + geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = archive)
  }
  if (!is.null(previous_point)) {
    plot = plot + geom_point(aes(x = x, y = y), size = 3L, colour = "grey55", data = previous_point)
  }
  if (!is.null(best_point)) {
    plot = plot + geom_point(aes(x = x, y = y), size = 3L, colour = "#00A64F", data = best_point)
  }
  if (show_best_line && !is.null(best_point)) {
    plot = plot + geom_hline(yintercept = best_point$y, colour = "#00A64F", linetype = 2)
  }
  if (!is.null(candidate)) {
    plot = plot +
      geom_point(aes(x = x, y = .data[[candidate_y_col]]),
        size = 3L, colour = candidate_color, data = candidate
      )
  }
  if (!is.null(density_curve)) {
    plot = plot + geom_path(aes(x = x, y = y), colour = "darkgrey", data = density_curve)
  }

  plot +
    coord_cartesian(xlim = c(0, 1), ylim = y_limits) +
    bo_theme()
}

plot_acquisition_1d = function(grid, acq_col, candidate, y_label, y_limits = NULL) {
  ggplot(grid, aes(x = x, y = .data[[acq_col]])) +
    geom_line(colour = "darkred") +
    geom_point(aes(x = x, y = .data[[acq_col]]), size = 3L, colour = "darkred", data = candidate) +
    coord_cartesian(xlim = c(0, 1), ylim = y_limits) +
    labs(x = NULL, y = y_label) +
    bo_theme()
}

collect_trace = function(instance, method, repl) {
  trace = copy(instance$archive$data)
  trace[, best := cummin(y)]
  trace[, method := method]
  trace[, iter := seq_len(.N)]
  trace[, repl := repl]
  trace
}

summarize_traces = function(results) {
  results[, .(mean_best = mean(best), se_best = sd(best) / sqrt(.N)), by = .(iter, method)]
}

plot_trace_summary = function(summary) {
  ggplot(summary, aes(x = iter, y = mean_best, colour = method, fill = method)) +
    geom_ribbon(
      aes(ymin = mean_best - se_best, ymax = mean_best + se_best),
      colour = NA,
      alpha = 0.25
    ) +
    geom_step() +
    labs(
      x = "Nr. Function Evaluations",
      y = "Best Objective Value",
      colour = "Method",
      fill = "Method"
    ) +
    bo_theme() +
    theme(legend.position = "bottom")
}
