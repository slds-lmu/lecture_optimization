# Used in: slides/11-multicrit/slides-multicrit-2-evolutionary.tex
#
# Illustrates the exclusive hypervolume contribution of each point in a small
# two-objective Pareto set with respect to a fixed reference point.

library(data.table)
library(ggplot2)

set.seed(1L)

output_dir = "../figure"
reference_point = c(f1 = 9, f2 = 9)

theme_set(theme_bw(base_size = 11))

pareto_points = data.table(
  f1 = c(0, 2, 3, 8),
  f2 = c(7, 5.5, 4, 2)
)
setorder(pareto_points, f1)

pareto_points[, f1_max := shift(f1, type = "lead", fill = reference_point["f1"])]
pareto_points[, f2_max := shift(f2, type = "lag", fill = reference_point["f2"])]
pareto_points[, hvc := (f1_max - f1) * (f2_max - f2)]
pareto_points[, hvc_label := sprintf("HVC = %.1f", hvc)]
pareto_points[, label_x := fifelse(f1 > 7, f1 + 1.0, f1 + 0.25)]
pareto_points[, label_y := f2 + 0.55]
pareto_points[, label_hjust := fifelse(f1 > 7, 1, 0)]

least_contributing_point = pareto_points[which.min(hvc)]

hypervolume_plot = ggplot(pareto_points, aes(x = f1, y = f2)) +
  geom_rect(aes(xmin = f1, xmax = f1_max, ymin = f2, ymax = f2_max), fill = "#4C78A8", alpha = 0.18) +
  geom_point(size = 3) +
  geom_point(
    data = data.table(f1 = reference_point["f1"], f2 = reference_point["f2"]),
    shape = 4,
    size = 3,
    stroke = 1.1,
    color = "grey35"
  ) +
  geom_text(aes(x = label_x, y = label_y, label = hvc_label, hjust = label_hjust), size = 3) +
  annotate(
    "text",
    x = least_contributing_point$f1 - 0.45,
    y = least_contributing_point$f2 - 0.5,
    size = 5,
    parse = TRUE,
    label = as.character(expression(tilde(lambda)))
  ) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10), expand = FALSE) +
  labs(x = expression(f[1]), y = expression(f[2]))

if (interactive()) {
  print(hypervolume_plot)
}

ggsave(file.path(output_dir, "hv_contrib.png"), hypervolume_plot, width = 3, height = 3, dpi = 300)
