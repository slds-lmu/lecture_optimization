# multivariate first order
# Gradient descent trajectories on the Franke objective (vistool)

set.seed(1L)
library(vistool)
library(ggplot2)

my_theme = vistool_theme(
  text_size = 25,
  line_width = 3,
  point_size = 5
)

objective_surface = obj("TF_franke")

start_points = matrix(
  c(
    0.21, 0.31,
    0.35, 0,
    0.7, 0.5
  ),
  ncol = 2L,
  byrow = TRUE
)

legend_labels = c(
  "x0 = (0.21, 0.31)",
  "x0 = (0.35, 0.0)",
  "x0 = (0.7, 0.5)"
)

optimizer_labels = legend_labels
line_colors = c(
  "x0 = (0.21, 0.31)" = "#003860",
  "x0 = (0.35, 0.0)" = "#d62728",
  "x0 = (0.7, 0.5)" = "#ff7f0e"
)
line_types = c(
  "x0 = (0.21, 0.31)" = "solid",
  "x0 = (0.35, 0.0)" = "dotted",
  "x0 = (0.7, 0.5)" = "dashed"
)
steps = 100L

optimizers = lapply(seq_len(nrow(start_points)), function(i) {
  optimizer = OptimizerGD$new(
    objective = obj("TF_franke"),
    x_start = start_points[i, ],
  lr = 0.01,
    id = optimizer_labels[i],
    print_trace = FALSE
  )
  optimizer$optimize(
    steps = steps,
    minimize = TRUE
  )
  optimizer
})

visualizer = as_visualizer(objective_surface)
visualizer$add_contours()
for (i in seq_along(optimizers)) {
  visualizer$add_optimization_trace(
    optimizers[[i]],
    line_color = line_colors[[optimizer_labels[i]]],
    line_type = line_types[[optimizer_labels[i]]]
  )
}

visualizer$set_theme(my_theme)
visualizer$plot(
  show_title = FALSE,
  show_legend = TRUE,

)

visualizer$save(
  filename = "../figure/gradient-descent-franke_1.png"
)

# Collect objective trajectories for a step-vs-loss plot.
optimizer_traces = do.call(merge_optim_archives, optimizers)
optimizer_traces = as.data.frame(optimizer_traces)

trace_by_optimizer = split(optimizer_traces, optimizer_traces$optim_id, drop = TRUE)
loss_trajectories = do.call(rbind, lapply(trace_by_optimizer, function(df) {
  data.frame(
    optim_id = df$optim_id[1L],
    iteration = c(0L, df$iteration),
    objective_value = c(df$fval_in[1L], df$fval_out)
  )
}))
row.names(loss_trajectories) = NULL

loss_trajectories$optim_id = factor(loss_trajectories$optim_id, levels = optimizer_labels)

gradient_history_plot <- ggplot(loss_trajectories, aes(x = iteration, y = objective_value, color = optim_id, linetype = optim_id)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(breaks = legend_labels, values = line_colors, labels = legend_labels) +
  scale_linetype_manual(breaks = legend_labels, values = line_types, labels = legend_labels) +
  labs(x = "Steps", y = "Objective value", color = NULL, linetype = NULL) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = c(0.75, 0.75),              # top-right quarter
    legend.justification = c("center", "center"), # anchor at its center
    legend.background = element_rect(fill = rgb(1, 1, 1, 0.8), colour = NA)
  )

ggsave(
  filename = "../figure/gradient-descent-franke_2.png",
  plot = gradient_history_plot,
  width = 4, height = 4, dpi = 300
)
