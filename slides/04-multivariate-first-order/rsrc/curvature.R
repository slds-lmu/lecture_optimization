# Used in: slides-multivar-first-order-4-weaknesses-curvature.tex
#
# Produces figure/curvature_1.png for the gradient decomposition and
# figure/curvature_2.png for the large-vs-small-step-size trajectories.

set.seed(1L)

library(ggplot2)
library(grid)
library(vistool)

curvature_matrix = diag(c(8, 36))

ill_conditioned_objective = Objective$new(
  id = "ill_cond_quad",
  label = "Ill-conditioned quadratic",
  fun = function(x, Q) {
    drop(t(x) %*% Q %*% x)
  },
  xdim = 2L,
  lower = c(-10, -10),
  upper = c(10, 10),
  minimize = TRUE,
  Q = curvature_matrix
)

base_visualizer = as_visualizer(
  ill_conditioned_objective,
  x1_limits = c(-10, 10),
  x2_limits = c(-10, 10)
)

base_visualizer$add_contours()
base_plot = base_visualizer$plot(show_title = FALSE, show_legend = FALSE)

# Match the point used in the lecture slide.
anchor = c(7, -6)
gradient = as.numeric(2 * curvature_matrix %*% anchor)
descent_direction = -gradient
scale_factor = 0.012

gradient_segment = data.frame(
  x = anchor[1],
  y = anchor[2],
  xend = anchor[1] + descent_direction[1] * scale_factor,
  yend = anchor[2] + descent_direction[2] * scale_factor
)

component_segments = rbind(
  data.frame(
    x = anchor[1],
    y = anchor[2],
    xend = anchor[1] + descent_direction[1] * scale_factor,
    yend = anchor[2],
    colour = "darkblue",
    label_x = anchor[1] + descent_direction[1] * scale_factor * 0.5,
    label_y = anchor[2] - 1.0,
    label = "v[min]"
  ),
  data.frame(
    x = anchor[1],
    y = anchor[2],
    xend = anchor[1],
    yend = anchor[2] + descent_direction[2] * scale_factor,
    colour = "darkred",
    label_x = anchor[1] + 1.5,
    label_y = anchor[2] + descent_direction[2] * scale_factor * 0.5,
    label = "v[max]"
  )
)

arrow_head = arrow(length = unit(0.25, "cm"), type = "closed")

plot_with_arrows = base_plot +
  geom_point(
    data = data.frame(x = anchor[1], y = anchor[2]),
    aes(x = x, y = y),
    colour = "black",
    size = 2
  ) +
  geom_segment(
    data = gradient_segment,
    aes(x = x, y = y, xend = xend, yend = yend),
    colour = "black",
    linewidth = 0.8,
    arrow = arrow_head
  ) +
  geom_segment(
    data = component_segments,
    aes(x = x, y = y, xend = xend, yend = yend, colour = colour),
    linewidth = 0.7,
    linetype = "dashed",
    arrow = arrow_head,
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  annotate(
    "text",
    x = component_segments$label_x,
    y = component_segments$label_y,
    label = component_segments$label,
    colour = component_segments$colour,
    parse = TRUE,
    size = 5
  ) +
  annotate(
    "text",
    x = gradient_segment$xend,
    y = gradient_segment$yend + 0.8,
    label = "g",
    colour = "black",
    size = 5
  )

ggsave(
  filename = "../figure/curvature_1.png",
  plot = plot_with_arrows,
  width = 6,
  height = 4.5,
  dpi = 300
)

large_step_colour = "#e27c3e"
small_step_colour = "#0b3d91"
trace_steps = 24L

gd_large = OptimizerGD$new(
  ill_conditioned_objective,
  x_start = anchor,
  lr = 0.025,
  id = "Large step size",
  print_trace = FALSE
)

gd_small = OptimizerGD$new(
  ill_conditioned_objective,
  x_start = anchor,
  lr = 0.005,
  id = "Small step size",
  print_trace = FALSE
)

gd_large$optimize(steps = trace_steps)
gd_small$optimize(steps = trace_steps)

trace_visualizer = as_visualizer(
  ill_conditioned_objective,
  x1_limits = c(-10, 10),
  x2_limits = c(-10, 10)
)

trace_visualizer$add_contours()
trace_visualizer$add_optimization_trace(
  gd_large,
  line_color = large_step_colour,
  line_type = "solid",
  line_width = 1.4,
  marker_shape = 16,
  marker_size = 2.8,
  add_marker_at = seq_len(nrow(gd_large$archive)),
  name = "large step size"
)

trace_visualizer$add_optimization_trace(
  gd_small,
  line_color = small_step_colour,
  line_type = "dashed",
  line_width = 1.1,
  marker_shape = 16,
  marker_size = 2.8,
  add_marker_at = seq_len(nrow(gd_small$archive)),
  name = "small step size"
)

trace_plot = trace_visualizer$plot(show_title = FALSE, show_legend = TRUE) +
  geom_point(
    data = data.frame(x1 = anchor[1], x2 = anchor[2]),
    aes(x = x1, y = x2),
    colour = "black",
    size = 2.5,
    inherit.aes = FALSE
  ) +
  annotate(
    "text",
    x = anchor[1] + 0.6,
    y = anchor[2] - 0.6,
    label = "start",
    colour = "black",
    size = 4
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

ggsave(
  filename = "../figure/curvature_2.png",
  plot = trace_plot,
  width = 8,
  height = 4.5,
  dpi = 300
)
