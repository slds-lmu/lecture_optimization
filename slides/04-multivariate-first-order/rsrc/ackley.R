# Used in: slides-multivar-first-order-5-weaknesses-saddle.tex
#
# Creates the Ackley optimization traces for figure/ackley_2.png.
# The 3D slide screenshots are still taken manually from the interactive widget.

set.seed(1L)

library(ggplot2)
library(plotly)
library(vistool)

ackley_fun = function(x, a = 20, b = 0.2, c = 2 * pi) {
  dimension = length(x)
  sum_sq = sum(x^2)
  sum_cos = sum(cos(c * x))

  -a * exp(-b * sqrt(sum_sq / dimension)) - exp(sum_cos / dimension) + a + exp(1)
}

ackley_objective = Objective$new(
  id = "ackley_custom",
  label = "Ackley function",
  fun = ackley_fun,
  xdim = 2L,
  lower = c(-1.5, -2.0),
  upper = c(1.5, 1.7),
  minimize = TRUE
)

start_points = matrix(
  c(
    0.61, 0.72,
    0.61, 0.72,
    0.61, 0.73,
    0.62, 0.75
  ),
  ncol = 2L,
  byrow = TRUE
)

learning_rates = c(0.05, 0.01, 0.01, 0.01)
trace_steps = rep(50L, length(learning_rates))
optimizer_labels = c(
  "α = 0.05, x₀ = (0.61, 0.72)",
  "α = 0.01, x₀ = (0.61, 0.72)",
  "α = 0.01, x₀ = (0.61, 0.73)",
  "α = 0.01, x₀ = (0.62, 0.75)"
)

line_colours = c(
  "α = 0.05, x₀ = (0.61, 0.72)" = "#af0909",
  "α = 0.01, x₀ = (0.61, 0.72)" = "#078300",
  "α = 0.01, x₀ = (0.61, 0.73)" = "#ff7f0e",
  "α = 0.01, x₀ = (0.62, 0.75)" = "#1f77b4"
)

line_types = c(
  "α = 0.05, x₀ = (0.61, 0.72)" = "solid",
  "α = 0.01, x₀ = (0.61, 0.72)" = "dashed",
  "α = 0.01, x₀ = (0.61, 0.73)" = "dotdash",
  "α = 0.01, x₀ = (0.62, 0.75)" = "dotted"
)

optimizers = lapply(seq_len(nrow(start_points)), function(i) {
  optimizer = OptimizerGD$new(
    objective = ackley_objective,
    x_start = start_points[i, ],
    lr = learning_rates[i],
    id = optimizer_labels[i],
    print_trace = FALSE
  )
  optimizer$optimize(steps = trace_steps[i])
  optimizer
})

ackley_visualizer = as_visualizer(
  ackley_objective,
  type = "surface",
  x1_limits = c(-1.5, 1.5),
  x2_limits = c(-2.0, 1.7)
)

if (interactive()) {
  ackley_visualizer$plot(show_title = FALSE)
}
# ../figure_man/ackley_0a.png is a manual screenshot.

if (interactive()) {
  ackley_visualizer$plot(show_title = FALSE, flatten = TRUE)
}
# ../figure_man/ackley_0b.png is a manual screenshot.

ackley_visualizer$add_contours(levels = 40L)

for (i in seq_along(optimizers)) {
  ackley_visualizer$add_optimization_trace(
    optimizers[[i]],
    line_color = line_colours[[optimizer_labels[i]]],
    line_type = line_types[[optimizer_labels[i]]],
    line_width = 1.2,
    marker_size = 2.2,
    add_marker_at = seq_len(nrow(optimizers[[i]]$archive)),
    name = optimizer_labels[i]
  )
}

ackley_contour = ackley_visualizer$plot(
  show_title = FALSE,
  show_legend = TRUE
)

ackley_widget = layout(
  ackley_contour,
  scene = list(
    aspectratio = list(x = 1, y = 1, z = 1.2),
    eye = list(x = 1, y = 1.5, z = 1.3)
  ),
  height = 500
)

if (interactive()) {
  print(ackley_widget)
}
# ../figure_man/ackley_1.png is a manual screenshot.

optimizer_archives = as.data.frame(do.call(merge_optim_archives, optimizers))
loss_by_optimizer = split(optimizer_archives, optimizer_archives$optim_id, drop = TRUE)

loss_trajectories = do.call(rbind, lapply(loss_by_optimizer, function(archive) {
  data.frame(
    optim_id = archive$optim_id[1L],
    iteration = c(0L, archive$iteration),
    objective_value = c(archive$fval_in[1L], archive$fval_out)
  )
}))

row.names(loss_trajectories) = NULL
loss_trajectories$optim_id = factor(loss_trajectories$optim_id, levels = optimizer_labels)

ackley_loss_plot = ggplot(
  loss_trajectories,
  aes(x = iteration, y = objective_value, colour = optim_id, linetype = optim_id)
) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = line_colours, breaks = optimizer_labels) +
  scale_linetype_manual(values = line_types, breaks = optimizer_labels) +
  labs(
    x = "Steps",
    y = "Objective value",
    colour = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

ggsave(
  filename = "../figure/ackley.png",
  plot = ackley_loss_plot,
  width = 6,
  height = 4,
  dpi = 300
)
