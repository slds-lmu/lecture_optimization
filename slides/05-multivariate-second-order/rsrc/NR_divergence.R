# Used in: slides/05-multivariate-second-order/slides-multivar-second-order-1-newton-raphson.tex
#
# Classic example of undamped Newton-Raphson failing to converge: minimizing
# f(x) = x^4/4 - x^2 + 2x, starting at x0 = 0, where f''(0) < 0 (negative
# curvature) makes the first step an ascent direction. The iterates then
# cycle between 0 and 1 forever instead of converging to the true minimum
# near x = -1.77. Gradient descent from the same starting point is shown
# alongside it, since it only uses the gradient and is unaffected by the
# sign of the curvature.
#
# vistool::OptimizerNR guards against exactly this Newton-Raphson failure
# mode: it falls back to gradient descent on non-descent directions and
# applies Armijo backtracking by default. OptimizerNRRaw strips both
# safeguards to expose the raw, textbook-pathological update.

library(vistool)
library(data.table)
library(ggplot2)
library(patchwork)

f <- function(x) x^4 / 4 - x^2 + 2 * x
f_grad <- function(x) x^3 - 2 * x + 2
f_hess <- function(x) matrix(3 * x^2 - 2, nrow = 1)

objective <- Objective$new(id = "f", label = "f", fun = f, xdim = 1L, minimize = TRUE)
objective$.__enclos_env__$private$p_gradient <- f_grad
objective$.__enclos_env__$private$p_hessian <- f_hess

OptimizerNRRaw <- R6::R6Class(
  "OptimizerNRRaw",
  inherit = OptimizerNR,
  private = list(
    compute_direction = function(grad, hess, minimize) -as.numeric(solve(hess, grad))
  )
)

optim_nr <- OptimizerNRRaw$new(objective, x_start = 0, id = "Newton-Raphson", print_trace = FALSE)
optim_nr$optimize(steps = 8L, step_size_control = function(x, u, obj, opt) 1)

optim_gd <- OptimizerGD$new(objective, x_start = 0, lr = 0.15, id = "Gradient Descent", print_trace = FALSE)
optim_gd$optimize(steps = 8L)

method_colors <- c("Newton-Raphson" = "firebrick", "Gradient Descent" = "steelblue")

curve_visualizer <- as_visualizer(objective, x1_limits = c(-2.5, 2.5))
curve_visualizer$set_theme(vistool_theme(text_size = 14))
curve_visualizer$add_optimization_trace(
  optim_nr,
  name = "Newton-Raphson",
  line_color = method_colors[["Newton-Raphson"]]
)
curve_visualizer$add_optimization_trace(
  optim_gd,
  name = "Gradient Descent",
  line_color = method_colors[["Gradient Descent"]]
)
trace_data <- merge_optim_archives(optim_nr, optim_gd)

path_start <- trace_data[, .(iteration = 0L, x = x_in[[1L]][1L], fval = fval_in[1L]), by = "optim_id"]
path_rest <- trace_data[, .(optim_id, iteration, x = sapply(x_out, `[`, 1L), fval = fval_out)]
path_data <- rbind(path_start, path_rest)
setorder(path_data, optim_id, iteration)

curve_plot <- curve_visualizer$plot(show_title = FALSE, show_legend = FALSE) +
  geom_path(
    data = path_data,
    aes(x = x, y = fval, color = optim_id),
    linewidth = 0.8,
    linetype = "dashed",
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = method_colors) +
  labs(y = "f") +
  theme(legend.position = "none")

start_rows <- trace_data[, .(iteration = 0L, fval = fval_in[1L]), by = "optim_id"]
rest_rows <- trace_data[, .(optim_id, iteration, fval = fval_out)]
progress_data <- rbind(start_rows, rest_rows)
setorder(progress_data, optim_id, iteration)

progress_plot <- ggplot(progress_data, aes(x = iteration, y = fval, color = optim_id)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  scale_color_manual(values = method_colors, name = NULL) +
  scale_x_continuous(breaks = unique(progress_data$iteration)) +
  labs(x = "Iteration t", y = expression(f(x^"[t]"))) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

combined_plot <- curve_plot + progress_plot + plot_layout(nrow = 1)

ggsave(
  filename = "../figure/NR_divergence.png",
  plot = combined_plot,
  width = 10,
  height = 3.3,
  dpi = 300
)
