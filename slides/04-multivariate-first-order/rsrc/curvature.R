# Produce figure/curvature_1.png visualizing gradient decomposition on an ill-conditioned quadratic.
# and effects of step size (curvature_2.png)

set.seed(1L)

library(vistool)
library(ggplot2)

curvature_Q = diag(c(8, 36))

ill_cond_obj = Objective$new(
  id = "ill_cond_quad",
  label = "Ill-conditioned quadratic",
  fun = function(x, Q) {
    drop(t(x) %*% Q %*% x)
  },
  xdim = 2L,
  lower = c(-10, -10),
  upper = c(10, 10),
  minimize = TRUE,
  Q = curvature_Q
)

vis = as_visualizer(
  ill_cond_obj,
  x1_limits = c(-10, 10),
  x2_limits = c(-10, 10)
)

vis$add_contours()

base_plot = vis$plot(
  show_title = FALSE,
  show_legend = FALSE
)

# Gradient decomposition anchored at (7, -6) to match the slide layout.
anchor = c(7, -6)
Q = curvature_Q
grad = as.numeric(2 * Q %*% anchor)
scale_factor = 0.012

# Visualize the negative gradient (descent direction) so arrows head towards the valley.
direction = -grad

grad_segment = data.frame(
  x = anchor[1],
  y = anchor[2],
  xend = anchor[1] + direction[1] * scale_factor,
  yend = anchor[2] + direction[2] * scale_factor
)

component_segments = rbind(
  data.frame(
    x = anchor[1],
    y = anchor[2],
    xend = anchor[1] + direction[1] * scale_factor,
    yend = anchor[2],
    colour = "darkblue",
    label_x = anchor[1] + direction[1] * scale_factor * 0.5,
    label_y = anchor[2] - 1.0,
    text = "v_min"
  ),
  data.frame(
    x = anchor[1],
    y = anchor[2],
    xend = anchor[1],
    yend = anchor[2] + direction[2] * scale_factor,
    colour = "darkred",
    label_x = anchor[1] + 1.5,
    label_y = anchor[2] + direction[2] * scale_factor * 0.5,
    text = "v_max"
  )
)

arrow_head = grid::arrow(length = grid::unit(0.25, "cm"), type = "closed")

plot_with_arrows = base_plot +
  geom_point(
    data = data.frame(x = anchor[1], y = anchor[2]),
    mapping = aes(x = x, y = y),
    colour = "black",
    size = 2
  ) +
  geom_segment(
    data = grad_segment,
    mapping = aes(x = x, y = y, xend = xend, yend = yend),
    colour = "black",
    linewidth = 0.8,
    arrow = arrow_head
  ) +
  geom_segment(
    data = component_segments,
    mapping = aes(x = x, y = y, xend = xend, yend = yend, colour = colour),
    linewidth = 0.7,
    linetype = "dashed",
    arrow = arrow_head,
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  annotate(
    geom = "text",
    x = component_segments$label_x,
    y = component_segments$label_y,
    label = component_segments$text,
    colour = component_segments$colour,
    size = 5
  ) +
  annotate(
    geom = "text",
    x = grad_segment$xend,
    y = grad_segment$yend + 0.8,
    label = "g",
    colour = "black",
    size = 5
  )

ggplot2::ggsave(
  filename = "../figure/curvature_1.png",
  plot = plot_with_arrows,
  width = 6,
  height = 4.5,
  dpi = 300
)

# Visualize gradient descent step sizes with vistool optimization traces for figure/curvature_traces.png.
large_step_colour = "#e27c3e"
small_step_colour = "#0b3d91"
trace_steps = 24L

gd_large = OptimizerGD$new(
  ill_cond_obj,
  x_start = anchor,
  lr = 0.025,
  id = "Large step size",
  print_trace = FALSE
)

gd_small = OptimizerGD$new(
  ill_cond_obj,
  x_start = anchor,
  lr = 0.005,
  id = "Small step size",
  print_trace = FALSE
)

gd_large$optimize(steps = trace_steps)
gd_small$optimize(steps = trace_steps)

vis_traces = as_visualizer(
  ill_cond_obj,
  x1_limits = c(-10, 10),
  x2_limits = c(-10, 10)
)

vis_traces$add_contours()

vis_traces$add_optimization_trace(
  gd_large,
  line_color = large_step_colour,
  line_type = "solid",
  line_width = 1.4,
  marker_shape = 16,
  marker_size = 2.8,
  add_marker_at = seq_len(nrow(gd_large$archive)),
  name = "large step size"
)

vis_traces$add_optimization_trace(
  gd_small,
  line_color = small_step_colour,
  line_type = "dashed",
  line_width = 1.1,
  marker_shape = 16,
  marker_size = 2.8,
  add_marker_at = seq_len(nrow(gd_small$archive)),
  name = "small step size"
)

trace_plot = vis_traces$plot(
  show_title = FALSE,
  show_legend = TRUE
)

trace_plot = trace_plot +
  ggplot2::geom_point(
    data = data.frame(x1 = anchor[1], x2 = anchor[2]),
    mapping = ggplot2::aes(x = x1, y = x2),
    colour = "black",
    size = 2.5,
    inherit.aes = FALSE
  ) +
  ggplot2::annotate(
    geom = "text",
    x = anchor[1] + 0.6,
    y = anchor[2] - 0.6,
    label = "start",
    colour = "black",
    size = 4
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "right",
    legend.title = ggplot2::element_text(size = 11),
    legend.text = ggplot2::element_text(size = 10)
  )

ggplot2::ggsave(
  filename = "../figure/curvature_2.png",
  plot = trace_plot,
  width = 8,
  height = 4.5,
  dpi = 300
)

