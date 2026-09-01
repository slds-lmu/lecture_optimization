# Used in: slides/05-multivariate-second-order/01-nr.tex
#
# Newton-Raphson overshooting, and the same run repaired by a line search:
#
#   f(x1, x2) = sqrt(1 + x1^2) + x2^2 / 2,   x* = (0, 0)
#
# x2 is quadratic and solved exactly in one step. Along x1 the curvature decays,
# (1 + x1^2)^(-3/2) -> 0, so the quadratic model is far too flat and its minimum
# lies well beyond x*: the update reduces to x1 <- -x1^3, and every |x1| > 1
# oscillates about x* with growing amplitude. The Hessian stays positive
# definite, so only the step length is wrong, which is what lets a line search
# repair it (unlike NR_divergence.R, where f'' < 0 spoils the direction itself).
#
# Growth is triply exponential, so the start sits just outside |x1| = 1: further
# out throws the late iterates off the window, closer makes three steps too few
# to exceed the initial error.
#
# Flip-book frames with axis limits shared across all frames and both runs:
#
#   ../figure/NR_overshoot_<k>_contour.png, _error.png     undamped, k = 1, 2, 3
#   ../figure/NR_overshoot_damped_contour.png, _error.png  final comparison

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  suppressWarnings(library(vistool))
})

overshoot = function(x) sqrt(1 + x[1]^2) + x[2]^2 / 2

overshoot_grad = function(x) c(x[1] / sqrt(1 + x[1]^2), x[2])

overshoot_hess = function(x) {
  matrix(c((1 + x[1]^2)^(-1.5), 0, 0, 1), nrow = 2L)
}

x_opt = c(0, 0)
f_opt = overshoot(x_opt)

x_start = c(1.05, 0.6)
snapshots = c(1L, 2L, 3L)
steps_nr = max(snapshots)

# Armijo: sufficient-decrease constant and backtracking factor. The vistool
# default gamma = 0.99 demands 99% of the decrease predicted by the linear
# model, which backtracks on nearly every step; 1e-4 is the textbook value.
armijo_gamma = 1e-4
armijo_tau = 0.5

# plot window, chosen for framing only
x1_limits = c(-4.4, 2.2)
x2_limits = c(-0.45, 1.05)

# f is defined on all of R^2, so the box of the objective is kept wide and
# separate from the plot window: framing the picture must not change the problem
objective_box = 1e3

col_undamped = "#ff6262" # undamped Newton-Raphson (full step, alpha = 1)
col_damped = "#4d9de0" # damped Newton-Raphson (Armijo backtracking line search)

make_objective = function() {
  objective = Objective$new(
    id = "overshoot",
    label = "pseudo-Huber + quadratic",
    fun = overshoot,
    xdim = 2L,
    lower = rep(-objective_box, 2L),
    upper = rep(objective_box, 2L),
    minimize = TRUE
  )
  objective$.__enclos_env__$private$p_gradient = overshoot_grad
  objective$.__enclos_env__$private$p_hessian = overshoot_hess
  objective
}

run_nr = function(steps, line_search) {
  optimizer = OptimizerNR$new(
    objective = make_objective(),
    x_start = x_start,
    step_size = 1,
    gamma = armijo_gamma,
    tau = armijo_tau,
    id = "Newton-Raphson",
    print_trace = FALSE
  )
  if (line_search) {
    # the built-in Armijo backtracking of OptimizerNR
    optimizer$optimize(steps = steps)
  } else {
    # constant control switches the backtracking off: always the full step
    optimizer$optimize(steps = steps, step_size_control = function(x, u, obj, opt) 1)
  }
  optimizer
}

collect = function(optimizer) {
  archive = as.data.table(optimizer$archive)
  iterates = rbind(matrix(x_start, nrow = 1L), do.call(rbind, archive$x_out))
  # both runs take every step at three steps; the damped one would stop early
  # (zero gradient) for larger steps_nr, leaving the two curves different lengths
  stopifnot(nrow(archive) == steps_nr)
  data.table(
    iteration = 0:steps_nr,
    x1 = iterates[, 1L],
    x2 = iterates[, 2L],
    fval = c(archive$fval_in[1L], archive$fval_out),
    alpha = c(NA_real_, archive$step_size)
  )
}

trace_undamped = collect(run_nr(steps_nr, line_search = FALSE))
trace_damped = collect(run_nr(steps_nr, line_search = TRUE))
trace_undamped[, error := fval - f_opt]
trace_damped[, error := fval - f_opt]

# --- frames -----------------------------------------------------------------
error_ylim = c(0, max(trace_undamped$error, trace_damped$error) * 1.05)

save_error_frame = function(steps, filename, damped) {
  shown = rbind(
    trace_undamped[iteration <= steps, .(iteration, error, run = "undamped")],
    if (damped) trace_damped[iteration <= steps, .(iteration, error, run = "damped (Armijo)")]
  )
  shown[, run := factor(run, levels = c("undamped", "damped (Armijo)"))]

  p = ggplot(shown, aes(x = iteration, y = error, color = run)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.6) +
    scale_color_manual(values = c("undamped" = col_undamped, "damped (Armijo)" = col_damped), name = NULL) +
    scale_x_continuous(breaks = 0:steps_nr) +
    coord_cartesian(xlim = c(0, steps_nr), ylim = error_ylim) +
    labs(x = "Steps", y = expression(f(x^"[t]") - f(x^"*"))) +
    theme_minimal(base_size = 12) +
    theme(legend.position = if (damped) "bottom" else "none")
  ggsave(filename = filename, plot = p, width = 4, height = 3, dpi = 300)
}

save_contour_frame = function(steps, filename, damped) {
  contour_vis = as_visualizer(
    make_objective(),
    x1_limits = x1_limits,
    x2_limits = x2_limits,
    n_points = 150L
  )
  contour_vis$add_contours(bins = 18)

  # traces drawn by hand: add_optimization_trace() puts one arrow per step,
  # which degenerates for a single step and stamps a full-size arrowhead on the
  # very short late steps
  add_trace = function(p, tr, color, linetype) {
    shown = tr[iteration <= steps]
    p +
      geom_path(
        data = shown, mapping = aes(x = x1, y = x2), inherit.aes = FALSE,
        color = color, linewidth = 1.1, linetype = linetype
      ) +
      geom_point(
        data = shown, mapping = aes(x = x1, y = x2), inherit.aes = FALSE,
        shape = 21, size = 2.1, stroke = 0.8, fill = "white", color = color
      )
  }

  p = contour_vis$plot(show_title = FALSE)
  p = add_trace(p, trace_undamped, col_undamped, "solid")
  if (damped) {
    p = add_trace(p, trace_damped, col_damped, "dashed")
  }
  p = p +
    annotate("point", x = x_opt[1], y = x_opt[2], shape = 8, size = 2.2, stroke = 0.8, color = "white") +
    guides(fill = "none")
  ggsave(filename = filename, plot = p, width = 6, height = 4, dpi = 300)
}

# the undamped run is animated over all three steps ...
for (k in snapshots) {
  save_error_frame(k, sprintf("../figure/NR_overshoot_%d_error.png", k), damped = FALSE)
  save_contour_frame(k, sprintf("../figure/NR_overshoot_%d_contour.png", k), damped = FALSE)
}

# ... while the damped comparison is a single figure of the final result
save_error_frame(steps_nr, "../figure/NR_overshoot_damped_error.png", damped = TRUE)
save_contour_frame(steps_nr, "../figure/NR_overshoot_damped_contour.png", damped = TRUE)
