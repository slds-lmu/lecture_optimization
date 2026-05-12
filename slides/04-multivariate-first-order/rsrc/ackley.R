set.seed(1L)
library(vistool)
library(ggplot2)

# Gradient-descent behavior on the Ackley surface using vistool (port of non_conv_ackley.R).

ackley_fun = function(x, a = 20, b = 0.2, c = 2 * pi) {
	d = length(x)
	sum1 = sum(x^2)
	sum2 = sum(cos(c * x))

	-a * exp(-b * sqrt(sum1 / d)) - exp(sum2 / d) + a + exp(1)
}

ackley_obj = Objective$new(
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
	"alpha = 0.05, x0 = (0.61, 0.72)",
	"alpha = 0.01, x0 = (0.61, 0.72)",
	"alpha = 0.01, x0 = (0.61, 0.73)",
	"alpha = 0.01, x0 = (0.62, 0.75)"
)

line_colors = c(
	"alpha = 0.05, x0 = (0.61, 0.72)" = "#af0909",
	"alpha = 0.01, x0 = (0.61, 0.72)" = "#078300",
	"alpha = 0.01, x0 = (0.61, 0.73)" = "#ff7f0e",
	"alpha = 0.01, x0 = (0.62, 0.75)" = "#1f77b4"
)

line_types = c(
	"alpha = 0.05, x0 = (0.61, 0.72)" = "solid",
	"alpha = 0.01, x0 = (0.61, 0.72)" = "dashed",
	"alpha = 0.01, x0 = (0.61, 0.73)" = "dotdash",
	"alpha = 0.01, x0 = (0.62, 0.75)" = "dotted"
)

optimizers = vector("list", length = nrow(start_points))
for (i in seq_len(nrow(start_points))) {
	optimizers[[i]] = OptimizerGD$new(
		objective = ackley_obj,
		x_start = start_points[i, ],
		lr = learning_rates[i],
		id = optimizer_labels[i],
		print_trace = FALSE
	)
	optimizers[[i]]$optimize(steps = trace_steps[i])
}

ackley_vis = as_visualizer(
	ackley_obj,
	type = "surface",
	x1_limits = c(-1.5, 1.5),
	x2_limits = c(-2.0, 1.7)
)
ackley_vis$plot(show_title = FALSE)
ackley_vis$save("../figure/ackley_0a.png")
ackley_vis$plot(show_title = FALSE, flatten = TRUE)
ackley_vis$save("../figure/ackley_0b.png")

ackley_vis$add_contours(levels = 40L)

for (i in seq_along(optimizers)) {
	ackley_vis$add_optimization_trace(
		optimizers[[i]],
		line_color = line_colors[[optimizer_labels[i]]],
		line_type = line_types[[optimizer_labels[i]]],
		line_width = 1.2,
		marker_size = 2.2,
		add_marker_at = seq_len(nrow(optimizers[[i]]$archive)),
		name = optimizer_labels[i]
	)
}

ackley_contour = ackley_vis$plot(
	show_title = FALSE,
	show_legend = TRUE
)
ackley_contour
ackley = plotly::layout(
  ackley_contour,
  scene = list(
    aspectratio = list(x = 1, y = 1, z = 1.2),
    eye = list(x = 1, y = 1.5, z = 1.3)
  ),
  height = 500
)
plotly::save_image(p, "../figure/ackley_1.png", width = 700, height = 500)

optimizer_archives = do.call(merge_optim_archives, optimizers)
optimizer_archives = as.data.frame(optimizer_archives)

loss_by_optimizer = split(optimizer_archives, optimizer_archives$optim_id, drop = TRUE)
loss_trajectories = do.call(rbind, lapply(loss_by_optimizer, function(df) {
	data.frame(
		optim_id = df$optim_id[1L],
		iteration = c(0L, df$iteration),
		objective_value = c(df$fval_in[1L], df$fval_out)
	)
}))
row.names(loss_trajectories) = NULL

loss_trajectories$optim_id = factor(loss_trajectories$optim_id, levels = optimizer_labels)

ackley_loss_plot = ggplot(loss_trajectories, aes(
	x = iteration,
	y = objective_value,
	color = optim_id,
	linetype = optim_id
)) +
	geom_line(linewidth = 1.1) +
	scale_color_manual(values = line_colors, breaks = optimizer_labels, labels = optimizer_labels) +
	scale_linetype_manual(values = line_types, breaks = optimizer_labels, labels = optimizer_labels) +
	labs(
		x = "Steps",
		y = "Objective value",
		color = NULL,
		linetype = NULL
	) +
	theme_minimal(base_size = 12) +
	theme(legend.position = "right")

ggplot2::ggsave(
	filename = "../figure/ackley_2.png",
	plot = ackley_loss_plot,
	width = 6,
	height = 4,
	dpi = 300
)
