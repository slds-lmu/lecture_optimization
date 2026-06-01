# Used in: slides/11-multicrit/slides-multicrit-1a-intro.tex
#
# Builds the one-dimensional toy objective figures that introduce a Pareto front
# by plotting objectives over x and then in objective space.

library(data.table)
library(ggplot2)

set.seed(1L)

output_dir = "../figure"

theme_set(theme_bw(base_size = 11))

save_plot = function(plot, filename, width = 2, height = 2) {
  ggsave(file.path(output_dir, filename), plot, width = width, height = height, dpi = 300)
}

f1 = function(x) {
  (x - 1)^2
}

f2 = function(x) {
  3 * (x - 2)^2
}

x_grid = seq(0, 3, length.out = 1000L)
objective_curve = data.table(x = x_grid, f1 = f1(x_grid), f2 = f2(x_grid))
pareto_curve = objective_curve[x >= 1 & x <= 2]

single_objective_plot = ggplot(data.table(x = c(0, 3)), aes(x = x)) +
  stat_function(fun = f1, linewidth = 0.7) +
  annotate("point", x = 1, y = 0, color = "#009E73", size = 3) +
  labs(x = expression(x), y = expression(f(x)))

two_objective_plot = ggplot(data.table(x = c(0, 3)), aes(x = x)) +
  stat_function(aes(color = "f1"), fun = f1, linewidth = 0.7) +
  stat_function(aes(color = "f2"), fun = f2, linewidth = 0.7) +
  scale_color_manual(
    values = c(f1 = "black", f2 = "#0072B2"),
    labels = c(f1 = expression(f[1]), f2 = expression(f[2]))
  ) +
  labs(x = expression(x), y = expression(f(x)), color = NULL) +
  theme(legend.position = "bottom")

objective_space_plot = ggplot(objective_curve, aes(x = f1, y = f2)) +
  geom_path(linewidth = 0.45) +
  geom_path(data = pareto_curve, color = "#009E73", linewidth = 1) +
  labs(x = expression(f[1]), y = expression(f[2]))

if (interactive()) {
  print(single_objective_plot)
  print(two_objective_plot)
  print(objective_space_plot)
}

save_plot(single_objective_plot, "graph1.png")
save_plot(two_objective_plot, "graph2.png")
save_plot(objective_space_plot, "graph3.png")
