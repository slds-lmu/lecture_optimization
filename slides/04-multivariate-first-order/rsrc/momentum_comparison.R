# Used in: slides-multivar-first-order-6-momentum.tex
#
# Generates GD-with-momentum trajectory plots on the ill-conditioned quadratic
# f(x1, x2) = x1^2/2 + 10*x2^2, starting from x0 = (10, 1) with lr = 0.1.
# Produces:
#   figure/momentum_comparison.pdf        (phi in {0, 0.1, 0.5})
#   figure/momentum_comparison_overshoot.pdf  (phi in {0, 0.1, 0.5, 0.7})

set.seed(1L)

library(vistool)
library(ggplot2)

objective = Objective$new(
  id = "momentum_quadratic",
  fun = function(x) x[1L]^2 / 2 + 10 * x[2L]^2,
  xdim = 2L,
  lower = c(-2, -1.5),
  upper = c(10, 1.5),
  minimize = TRUE
)

run_optimizer = function(phi, x_start = c(10, 1), lr = 0.1, n_steps = 12L) {
  opt = OptimizerMomentum$new(
    objective = objective,
    x_start = x_start,
    lr = lr,
    momentum = phi,
    print_trace = FALSE
  )
  opt$optimize(steps = n_steps)
  arc = opt$archive
  x_start_row = data.frame(x1 = arc$x_in[[1L]][1L], x2 = arc$x_in[[1L]][2L])
  x_rest = data.frame(
    x1 = sapply(arc$x_out, `[`, 1L),
    x2 = sapply(arc$x_out, `[`, 2L)
  )
  traj = rbind(x_start_row, x_rest)
  traj$phi = factor(phi)
  traj
}

make_plot = function(phi_vals, colors) {
  traj_data = do.call(rbind, lapply(phi_vals, run_optimizer))
  traj_data$phi = factor(traj_data$phi, levels = phi_vals)

  viz = as_visualizer(objective, x1_limits = c(-2, 10), x2_limits = c(-1.5, 1.5))
  viz$add_contours()
  p = viz$plot(show_title = FALSE, show_legend = FALSE)

  p +
    geom_path(data = traj_data, aes(x = x1, y = x2, color = phi)) +
    geom_point(data = traj_data, aes(x = x1, y = x2, color = phi), size = 1.5) +
    scale_color_manual(
      values = setNames(colors, as.character(phi_vals)),
      labels = parse(text = paste0("varphi == ", phi_vals)),
      name = NULL
    ) +
    guides(fill = "none") +
    theme(legend.position = "right")
}

p1 = make_plot(
  phi_vals = c(0, 0.1, 0.5),
  colors = c("black", "orange", "dodgerblue")
)
ggsave("../figure/momentum_comparison.pdf", plot = p1, width = 6, height = 3)

p2 = make_plot(
  phi_vals = c(0, 0.1, 0.5, 0.7),
  colors = c("black", "orange", "dodgerblue", "red")
)
ggsave("../figure/momentum_comparison_overshoot.pdf", plot = p2, width = 6, height = 3)
