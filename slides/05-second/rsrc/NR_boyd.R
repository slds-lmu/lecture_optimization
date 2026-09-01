# Used in: slides/05-second/01-nr.tex
#
# Newton-Raphson on the nonquadratic test problem of Boyd & Vandenberghe (2004),
# eq. (9.20), used there in 9.5.3:
#
#   f(x1, x2) = exp(x1 + 3 x2 - 0.1) + exp(x1 - 3 x2 - 0.1) + exp(-x1 - 0.1)
#
# Convex, smooth, with a narrow curved valley; x* = (-log(2)/2, 0), f* = 2.559.
# Undamped Newton (full step, no line search) reaches machine precision in 5
# steps: the error is squared at every step (quadratic convergence).
#
# Written as flip-book frames after 1, 2 and 5 steps (as in
# 06-advfirst/rsrc/gradient_descent_NN.R), with axis limits
# shared across frames so the panels can be overlaid with \only<..>:
#
#   ../figure/NR_boyd_<k>_contour.png
#   ../figure/NR_boyd_<k>_error.png

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  suppressWarnings(library(vistool))
})

boyd = function(x) {
  exp(x[1] + 3 * x[2] - 0.1) + exp(x[1] - 3 * x[2] - 0.1) + exp(-x[1] - 0.1)
}

boyd_grad = function(x) {
  e1 = exp(x[1] + 3 * x[2] - 0.1)
  e2 = exp(x[1] - 3 * x[2] - 0.1)
  e3 = exp(-x[1] - 0.1)
  c(e1 + e2 - e3, 3 * e1 - 3 * e2)
}

boyd_hess = function(x) {
  e1 = exp(x[1] + 3 * x[2] - 0.1)
  e2 = exp(x[1] - 3 * x[2] - 0.1)
  e3 = exp(-x[1] - 0.1)
  matrix(
    c(
      e1 + e2 + e3, 3 * e1 - 3 * e2,
      3 * e1 - 3 * e2, 9 * e1 + 9 * e2
    ),
    nrow = 2,
    byrow = TRUE
  )
}

# analytic optimum: grad_2 = 0 => x2 = 0, grad_1 = 0 => exp(2 x1) = 1/2
x_opt = c(-log(2) / 2, 0)
f_opt = boyd(x_opt)

x_start = c(-1.1, 0.9)
snapshots = c(1L, 2L, 5L)
steps_nr = max(snapshots)

x1_limits = c(-1.7, 0.5)
x2_limits = c(-0.55, 1.05)

make_objective = function(transform = objective_transform_identity()) {
  objective = Objective$new(
    id = "boyd",
    label = "Boyd (9.20)",
    fun = boyd,
    xdim = 2L,
    lower = c(x1_limits[1], x2_limits[1]),
    upper = c(x1_limits[2], x2_limits[2]),
    minimize = TRUE,
    transform = transform
  )
  objective$.__enclos_env__$private$p_gradient = boyd_grad
  objective$.__enclos_env__$private$p_hessian = boyd_hess
  objective
}

col_nr = "#ff6262"

# undamped Newton: the constant step size control switches off the Armijo
# backtracking that OptimizerNR applies by default. The Hessian is positive
# definite everywhere here (det = 9 (4 e1 e2 + e3 (e1 + e2)) > 0), so the
# gradient fallback of OptimizerNR never triggers either.
run_nr = function(steps) {
  optimizer = OptimizerNR$new(
    objective = make_objective(),
    x_start = x_start,
    step_size = 1,
    id = "Newton-Raphson",
    print_trace = FALSE
  )
  optimizer$optimize(steps = steps, step_size_control = function(x, u, obj, opt) 1)
  optimizer
}

optim_full = run_nr(steps_nr)
trace_data = as.data.table(optim_full$archive)

progress = rbind(
  data.table(iteration = 0L, fval = trace_data$fval_in[1L]),
  trace_data[, .(iteration = step, fval = fval_out)]
)
setorder(progress, iteration)
progress[, error := fval - f_opt]

# the iterates themselves, so the steps can be counted in the contour plot
iterates = rbind(
  matrix(x_start, nrow = 1L),
  do.call(rbind, trace_data$x_out)
)
iterates = data.table(iteration = 0:steps_nr, x1 = iterates[, 1L], x2 = iterates[, 2L])

cat(sprintf("f* = %.6f at (%.4f, %.4f)\n", f_opt, x_opt[1], x_opt[2]))
print(progress[, .(iteration, error)])

# --- frames -----------------------------------------------------------------
# axis limits are fixed across frames, otherwise the overlaid panels would jump
error_ylim = c(0, max(progress$error) * 1.04)

save_error_frame = function(steps, filename) {
  shown = progress[iteration <= steps]
  p = ggplot(shown, aes(x = iteration, y = error)) +
    geom_line(linewidth = 0.9, color = col_nr) +
    geom_point(size = 1.6, color = col_nr) +
    scale_x_continuous(breaks = 0:steps_nr) +
    coord_cartesian(xlim = c(0, steps_nr), ylim = error_ylim) +
    labs(x = "Steps", y = expression(f(x^"[t]") - f(x^"*"))) +
    theme_minimal(base_size = 12)
  ggsave(filename = filename, plot = p, width = 4, height = 3, dpi = 300)
}

# the objective grows exponentially towards the corners, which would squeeze all
# level sets near the optimum into a single band: plot log f instead, which has
# the same level curves but usable spacing
save_contour_frame = function(steps, filename) {
  contour_vis = as_visualizer(
    make_objective(transform = objective_transform_log()),
    x1_limits = x1_limits,
    x2_limits = x2_limits,
    n_points = 150L
  )
  contour_vis$add_contours(bins = 20)

  # the trace is drawn by hand rather than with add_optimization_trace(): the
  # latter draws one arrow per step, which degenerates for a single step and
  # would put a full-size arrowhead on the (invisibly short) late steps
  shown = iterates[iteration <= steps]
  p = contour_vis$plot(show_title = FALSE) +
    geom_path(
      data = shown,
      mapping = aes(x = x1, y = x2),
      inherit.aes = FALSE,
      linewidth = 1.1,
      color = col_nr
    ) +
    geom_point(
      data = shown,
      mapping = aes(x = x1, y = x2),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.1,
      stroke = 0.8,
      fill = "white",
      color = col_nr
    ) +
    annotate("point", x = x_opt[1], y = x_opt[2], shape = 8, size = 2.2, stroke = 0.8, color = "white") +
    # the colorbar would be labelled with values of log f, not f: drop it and
    # let the level curves carry the landscape
    guides(fill = "none")
  ggsave(filename = filename, plot = p, width = 6, height = 4, dpi = 300)
}

for (k in snapshots) {
  save_error_frame(k, sprintf("../figure/NR_boyd_%d_error.png", k))
  save_contour_frame(k, sprintf("../figure/NR_boyd_%d_contour.png", k))
}
