# comparison of NR and GD, show that NR converges much faster (good starting point)

library(ggplot2)
library(vistool)
library(rlang)
library(data.table)

rosenbrock <- function(x) {
  100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2
}

rosenbrock_grad <- function(x) {
  c(
    -400 * x[1] * (x[2] - x[1]^2) + 2 * (x[1] - 1),
    200 * (x[2] - x[1]^2)
  )
}

rosenbrock_hess <- function(x) {
  matrix(
    c(
      1200 * x[1]^2 - 400 * x[2] + 2,
      -400 * x[1],
      -400 * x[1],
      200
    ),
    nrow = 2,
    byrow = TRUE
  )
}

make_rosenbrock_objective <- function() {
  obj = Objective$new(
    id = "rosen",
    label = "Rosenbrock",
    fun = rosenbrock,
    xdim = 2L,
    lower = c(-2, -1),
    upper = c(2, 3),
    minimize = TRUE
  )
  obj$.__enclos_env__$private$p_gradient = rosenbrock_grad
  obj$.__enclos_env__$private$p_hessian = rosenbrock_hess
  obj
}

prepend_initial_iterations = function(trace_data) {
  stopifnot(data.table::is.data.table(trace_data))
  start_rows = trace_data[, .SD[1], by = "optim_id"]
  start_rows[["iteration"]] = 0L
  start_rows[["fval_out"]] = start_rows[["fval_in"]]
  start_rows[["x_out"]] = start_rows[["x_in"]]
  start_rows[["update"]] = lapply(start_rows[["x_in"]], function(x) rep(0, length(x)))
  start_rows[["step_size"]] = NA_real_
  start_rows[["lr"]] = NA_real_
  combined = data.table::rbindlist(list(start_rows, trace_data), use.names = TRUE, fill = TRUE)
  data.table::setorderv(combined, c("optim_id", "iteration"))
  combined
}

plot_value_trace = function(trace_data) {
  trace_df = as.data.frame(trace_data[order(trace_data$optim_id, trace_data$iteration)])
  ggplot(trace_df, aes(
    x = .data$iteration,
    y = .data$fval_out,
    color = .data$optim_id,
    linetype = .data$optim_id
  )) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.2) +
    scale_color_manual(
      values = c("Gradient Descent" = "#ffcc00", "Newton-Raphson" = "#ff6262"),
      name = "Method"
    ) +
    scale_linetype_manual(
      values = c("Gradient Descent" = "solid", "Newton-Raphson" = "dashed"),
      name = "Method"
    ) +
    labs(
      title = NULL,
      x = "Steps",
      y = "Objective value"
    ) +
    theme_minimal(base_size = 12)
}

build_contour_visualizer = function(opt_nr, opt_gd) {
  vis = VisualizerObj$new(
    objective = make_rosenbrock_objective(),
    x1_limits = c(-2, 2),
    x2_limits = c(-1, 3),
    n_points = 150L,
    type = "2d"
  )
  vis$add_contours(bins = 20)
  vis$add_optimization_trace(opt_nr, name = "Newton-Raphson", line_type = "dashed", line_color = "#ff6262", line_width = 2.3)
  vis$add_optimization_trace(opt_gd, name = "Gradient Descent", line_type = "solid", line_color = "#ffcc00", line_width = 2.3)
  vis
}

build_surface_visualizer = function(opt_nr, opt_gd) {
  vis = VisualizerSurfaceObj$new(
    objective = make_rosenbrock_objective(),
    x1_limits = c(-2, 2),
    x2_limits = c(-1, 3),
    n_points = 80L
  )
  vis$init_layer_surface()
  vis$add_optimization_trace(opt_nr, name = "Newton-Raphson", line_type = "dashed", line_color = "#b22222")
  vis$add_optimization_trace(opt_gd, name = "Gradient Descent", line_type = "solid", line_color = "#1f77b4")
  vis
}

run_comparison = function(steps = 25L, start = c(0.25, 2.5)) {
  obj_nr = make_rosenbrock_objective()
  optim_nr = OptimizerNR$new(
    objective = obj_nr,
    x_start = start,
    step_size = 0.9,
    gamma = 0.3,
    tau = 0.9,
    print_trace = FALSE
  )
  optim_nr$optimize(steps = steps)

  obj_gd = make_rosenbrock_objective()
  optim_gd = OptimizerGD$new(
    objective = obj_gd,
    x_start = start,
    lr = 1e-4,
    print_trace = FALSE
  )
  optim_gd$optimize(steps = steps)

  list(newton = optim_nr, gd = optim_gd)
}

comparators = run_comparison()
optim_nr = comparators$newton
optim_gd = comparators$gd

trace_data = merge_optim_archives(optim_nr, optim_gd)
trace_data = prepend_initial_iterations(trace_data)
trace_plot = plot_value_trace(trace_data)
trace_plot

ggplot2::ggsave(
  "../figure/NR_1.png",
  plot = trace_plot,
  width = 4,
  height = 3,
  dpi = 300
)

contour_vis = build_contour_visualizer(optim_nr, optim_gd)
contour_vis$plot(
  show_title = FALSE,
  theme = list(alpha = 1)
)
contour_vis$save("../figure/NR_2.png")

surface_vis = build_surface_visualizer(optim_nr, optim_gd)
surface_vis$plot(
  show_title = FALSE,
  legend_title = "Method"
)

