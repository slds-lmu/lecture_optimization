# Used in: slides/03-univariate-optimization/slides-univariate-1-golden-ratio.tex
#
# Placement of x_best and x_new according to the golden section rule,
# overlaid on the objective function from the simple nesting procedure.

set.seed(1L)

library(ggplot2)

phi = (sqrt(5) - 1) / 2

objective = function(x) {
  0.22 + 0.85 * (x - 0.48)^2 + 0.18 * (x - 0.48)^4 + 0.05 * (x - 0.48)
}

palette = list(
  paper = "#FFFFFF",
  ink = "#2F3A4A",
  curve = "#454E5E",
  boundary = "#7A7F87",
  best = "#0F8B8D",
  new = "#C84B6C",
  blue = "#2563EB"
)

x_left = 0.05
x_right = 1.45
a = x_right - x_left
x_best = x_left + (1 - phi) * a
x_new = x_left + phi * a

x_limits = c(-0.05, 1.60)
y_limits = c(-0.50, 1.32)
axis_y = 0
label_y = -0.17

curve_data = data.frame(x = seq(x_left, x_right, length.out = 600L))
curve_data$y = objective(curve_data$x)

key_xs = c(x_left, x_best, x_new, x_right)
key_colors = c(palette$boundary, palette$best, palette$new, palette$boundary)
key_labels = c("italic(x)[left]", "italic(x)[best]", "italic(x)[new]", "italic(x)[right]")
key_ys = objective(key_xs)

points_df = data.frame(
  x = key_xs,
  y = key_ys,
  fill = key_colors,
  label = key_labels,
  stem_y = pmax(key_ys - 0.045, axis_y + 0.045),
  stringsAsFactors = FALSE
)

y_b1 = -0.33
y_b2 = -0.45

p = ggplot(curve_data, aes(x = x, y = y)) +
  coord_cartesian(xlim = x_limits, ylim = y_limits, clip = "off") +
  theme_void(base_size = 22) +
  theme(
    plot.background = element_rect(fill = palette$paper, colour = NA),
    panel.background = element_rect(fill = palette$paper, colour = NA),
    plot.margin = margin(20, 24, 64, 24)
  ) +
  annotate(
    "segment",
    x = -0.01,
    xend = x_limits[2] - 0.03,
    y = axis_y,
    yend = axis_y,
    colour = palette$ink,
    linewidth = 0.8,
    arrow = arrow(length = unit(0.18, "cm"), type = "closed")
  ) +
  annotate(
    "segment",
    x = -0.01,
    xend = -0.01,
    y = axis_y,
    yend = y_limits[2] - 0.06,
    colour = palette$ink,
    linewidth = 0.8,
    arrow = arrow(length = unit(0.18, "cm"), type = "closed")
  ) +
  annotate("text", x = x_limits[2] - 0.05, y = -0.06, label = "x", colour = palette$ink, fontface = "bold", size = 6) +
  annotate("text", x = -0.08, y = 0.62, label = "y", colour = palette$ink, fontface = "bold", size = 6) +
  geom_line(colour = palette$curve, linewidth = 1.25, lineend = "round") +
  geom_segment(
    data = points_df,
    aes(x = x, xend = x, y = axis_y + 0.015, yend = stem_y, colour = fill),
    inherit.aes = FALSE,
    linewidth = 0.65,
    alpha = 0.55,
    show.legend = FALSE
  ) +
  geom_point(
    data = points_df,
    aes(x = x, y = y, fill = fill),
    inherit.aes = FALSE,
    shape = 21,
    size = 5.2,
    stroke = 1.1,
    colour = "white",
    show.legend = FALSE
  ) +
  geom_text(
    data = points_df,
    aes(x = x, y = label_y, label = label),
    inherit.aes = FALSE,
    parse = TRUE,
    size = 5.0,
    colour = palette$ink,
    family = "serif"
  ) +
  scale_fill_identity() +
  scale_colour_identity() +
  # b = phi * a: x_left to x_new
  annotate(
    "segment",
    x = x_left,
    xend = x_new,
    y = y_b1,
    yend = y_b1,
    colour = palette$blue,
    linewidth = 1.2,
    arrow = arrow(length = unit(0.15, "cm"), ends = "both", type = "closed")
  ) +
  annotate(
    "label",
    x = (x_left + x_new) / 2,
    y = y_b1,
    label = "phi * (italic(x)[right] - italic(x)[left])",
    parse = TRUE,
    fill = palette$blue,
    colour = "white",
    fontface = "bold",
    size = 5.0,
    label.padding = unit(0.20, "lines"),
    label.r = unit(0.15, "lines")
  ) +
  # b = phi * a: x_best to x_right
  annotate(
    "segment",
    x = x_best,
    xend = x_right,
    y = y_b2,
    yend = y_b2,
    colour = palette$blue,
    linewidth = 1.2,
    arrow = arrow(length = unit(0.15, "cm"), ends = "both", type = "closed")
  ) +
  annotate(
    "label",
    x = (x_best + x_right) / 2,
    y = y_b2,
    label = "phi * (italic(x)[right] - italic(x)[left])",
    parse = TRUE,
    fill = palette$blue,
    colour = "white",
    fontface = "bold",
    size = 5.0,
    label.padding = unit(0.20, "lines"),
    label.r = unit(0.15, "lines")
  )

dir.create("../figure", recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = "../figure/golden-section-placement.png",
  plot = p,
  width = 9.6,
  height = 5.8,
  dpi = 150,
  bg = palette$paper
)
