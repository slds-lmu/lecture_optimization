# Used in: slides-multivar-first-order-1-GD.tex
#
# Creates the Franke-function gradient-descent traces and the accompanying
# loss-versus-iteration comparison.

set.seed(1L)

library(ggplot2)
library(vistool)

franke_theme = vistool_theme(
  text_size = 25,
  line_width = 3,
  point_size = 5
)

franke_objective = obj("TF_franke")
start_points = matrix(
  c(
    0.21, 0.31,
    0.35, 0.00,
    0.70, 0.50
  ),
  ncol = 2L,
  byrow = TRUE
)

optimizer_labels = c(
  "x₀ = (0.21, 0.31)",
  "x₀ = (0.35, 0.0)",
  "x₀ = (0.7, 0.5)"
)

line_colours = c(
  "x₀ = (0.21, 0.31)" = "#003860",
  "x₀ = (0.35, 0.0)" = "#d62728",
  "x₀ = (0.7, 0.5)" = "#ff7f0e"
)

line_types = c(
  "x₀ = (0.21, 0.31)" = "solid",
  "x₀ = (0.35, 0.0)" = "dotted",
  "x₀ = (0.7, 0.5)" = "dashed"
)

steps = 100L

optimizers = lapply(seq_len(nrow(start_points)), function(i) {
  optimizer = OptimizerGD$new(
    objective = franke_objective,
    x_start = start_points[i, ],
    lr = 0.01,
    id = optimizer_labels[i],
    print_trace = FALSE
  )

  optimizer$optimize(steps = steps, minimize = TRUE)
  optimizer
})

franke_visualizer = as_visualizer(franke_objective)
franke_visualizer$add_contours()

for (i in seq_along(optimizers)) {
  franke_visualizer$add_optimization_trace(
    optimizers[[i]],
    line_color = line_colours[[optimizer_labels[i]]],
    line_type = line_types[[optimizer_labels[i]]]
  )
}

franke_visualizer$set_theme(franke_theme)

if (interactive()) {
  franke_visualizer$plot(show_title = FALSE, show_legend = TRUE)
}

franke_visualizer$save(filename = "../figure/gradient-descent-franke_1.png")

optimizer_traces = as.data.frame(do.call(merge_optim_archives, optimizers))
trace_by_optimizer = split(optimizer_traces, optimizer_traces$optim_id, drop = TRUE)

loss_trajectories = do.call(rbind, lapply(trace_by_optimizer, function(trace_data) {
  data.frame(
    optim_id = trace_data$optim_id[1L],
    iteration = c(0L, trace_data$iteration),
    objective_value = c(trace_data$fval_in[1L], trace_data$fval_out)
  )
}))

row.names(loss_trajectories) = NULL
loss_trajectories$optim_id = factor(loss_trajectories$optim_id, levels = optimizer_labels)

gradient_history_plot = ggplot(
  loss_trajectories,
  aes(x = iteration, y = objective_value, colour = optim_id, linetype = optim_id)
) +
  geom_line(linewidth = 1.5) +
  scale_colour_manual(values = line_colours, breaks = optimizer_labels) +
  scale_linetype_manual(values = line_types, breaks = optimizer_labels) +
  labs(x = "Steps", y = "Objective value", colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = c(0.75, 0.75),
    legend.justification = c("center", "center"),
    legend.background = element_rect(fill = rgb(1, 1, 1, 0.8), colour = NA)
  )

ggsave(
  filename = "../figure/gradient-descent-franke_2.png",
  plot = gradient_history_plot,
  width = 4,
  height = 4,
  dpi = 300
)
