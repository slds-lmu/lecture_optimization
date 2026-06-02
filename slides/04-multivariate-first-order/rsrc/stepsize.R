# Used in: slides-multivar-first-order-2-stepsize.tex
#
# Generates GD trajectory plots illustrating the effect of step size on convergence
# for f(x1, x2) = 10*x1^2 + x2^2/2.
# Produces: figure/stepsize_small.png, figure/stepsize_large.png

library(vistool)
library(ggplot2)

objective = Objective$new(
  id = "stepsize_quadratic",
  fun = function(x) 10 * x[1L]^2 + x[2L]^2 / 2,
  xdim = 2L,
  lower = c(-20, -20),
  upper = c(20, 20),
  minimize = TRUE
)

save_trace_plot = function(optimizer, filename, steps, alpha, width = 4, height = 4) {
  optimizer$optimize(steps = steps)

  viz = as_visualizer(objective, x1_limits = c(-20, 20), x2_limits = c(-20, 20))
  viz$add_contours()
  viz$add_optimization_trace(
    optimizer,
    add_marker_at = seq_len(nrow(optimizer$archive)),
    line_color = "black",
    marker_color = "black"
  )

  p = viz$plot(show_title = FALSE, show_legend = FALSE) +
    annotate("point", x = 0, y = 0, shape = 8, size = 4, color = "black") +
    annotate("label", x = -19, y = 19, hjust = 0, vjust = 1, size = 4, label = paste0("alpha == ", alpha), parse = TRUE)
  ggsave(filename, plot = p, width = width, height = height, dpi = 150)
}

# Small step size: alpha << 1/L (L=20), x2 converges extremely slowly
gd_small = OptimizerGD$new(
  objective,
  x_start = c(2, 20),
  lr = 0.05,
  id = "Small step size",
  print_trace = FALSE
)
save_trace_plot(gd_small, "../figure/stepsize_small.png", steps = 13L, alpha = 0.05)

# Large step size: alpha > 1/L causes |1 - 20*alpha| > 1, x1 oscillates and diverges
gd_large = OptimizerGD$new(
  objective,
  x_start = c(2, 20),
  lr = 0.11,
  id = "Large step size",
  print_trace = FALSE
)
save_trace_plot(gd_large, "../figure/stepsize_large.png", steps = 13L, alpha = 0.11)
