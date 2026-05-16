# Used in: slides/03-univariate-optimization/slides-univariate-1-golden-ratio.tex,
#          slides/03-univariate-optimization/slides-univariate-3-further.tex
#
# Create the schematic golden section figures used to explain the interval
# relationships a, b, and c in the lecture.

set.seed(1L)

library(data.table)
library(ggplot2)
library(grid)

dir.create("../figure", recursive = TRUE, showWarnings = FALSE)

palette = list(
  paper = "#FFFFFF",
  ink = "#23313E",
  blue = "#2563EB",
  amber = "#F59E0B",
  slate = "#4B5563"
)

coords = list(
  x_left = 0.12,
  x_best = 0.42,
  x_new = 0.56,
  x_right = 0.82,
  y_base = 0.28,
  y_xlabels = 0.19,
  y_left = 0.79,
  y_best = 0.55,
  y_new_a = 0.64,
  y_new_b = 0.45,
  y_right = 0.66
)

interval_bands = list(
  upper = 0.06,
  mid = -0.02,
  lower = -0.10
)

accent_bands = list(
  goldensec4_c = interval_bands$upper + 0.03,
  summary_b = interval_bands$mid + 0.03
)

guide_data = data.table(
  x = c(coords$x_left, coords$x_best, coords$x_right),
  y = coords$y_base,
  yend = c(0.88, 0.67, 0.83)
)

x_label_data = data.table(
  x = c(coords$x_left, coords$x_best, coords$x_new, coords$x_right),
  y = coords$y_xlabels,
  label = c(
    "italic(x)[left]",
    "italic(x)[best]",
    "italic(x)[new]",
    "italic(x)[right]"
  )
)

build_points = function(highlight = character()) {
  points = data.table(
    id = c("left", "best", "new_a", "new_b", "right"),
    x = c(
      coords$x_left,
      coords$x_best,
      coords$x_new,
      coords$x_new,
      coords$x_right
    ),
    y = c(
      coords$y_left,
      coords$y_best,
      coords$y_new_a,
      coords$y_new_b,
      coords$y_right
    ),
    label = c(
      "italic(f)[left]",
      "italic(f)[best]",
      "italic(f)[new*','*a]",
      "italic(f)[new*','*b]",
      "italic(f)[right]"
    ),
    label_x = c(
      coords$x_left - 0.06,
      coords$x_best - 0.05,
      coords$x_new + 0.05,
      coords$x_new + 0.05,
      coords$x_right + 0.05
    ),
    hjust = c(1, 1, 0, 0, 0)
  )

  points$fill = palette$paper
  points$colour = palette$ink
  points$stroke = 1.4

  solid_ids = c("left", "best", "right")
  points$fill[points$id %in% solid_ids] = palette$ink

  if (length(highlight) > 0L) {
    points$fill[points$id %in% highlight] = palette$blue
    points$colour[points$id %in% highlight] = palette$blue
    points$stroke[points$id %in% highlight] = 1.7
  }

  points
}

interval_layer = function(intervals) {
  list(
    geom_segment(
      data = intervals,
      aes(x = x, xend = xend, y = y, yend = y, colour = colour),
      inherit.aes = FALSE,
      linewidth = 1.35,
      lineend = "round",
      arrow = arrow(
        length = unit(0.18, "cm"),
        ends = "both",
        type = "closed"
      ),
      show.legend = FALSE
    ),
    geom_label(
      data = intervals[intervals$label != "", , drop = FALSE],
      aes(x = 0.5 * (x + xend), y = label_y, label = label, fill = colour),
      inherit.aes = FALSE,
      colour = "white",
      fontface = "bold",
      size = 6,
      linewidth = 0,
      label.padding = unit(0.18, "lines"),
      show.legend = FALSE
    )
  )
}

make_figure = function(highlight = character(), intervals = NULL, is_summary = FALSE) {
  points = build_points(highlight = highlight)
  filled_points = points[points$fill != palette$paper, , drop = FALSE]
  open_points = points[points$fill == palette$paper, , drop = FALSE]
  y_limits = if (is_summary) c(-0.20, 0.92) else c(-0.14, 0.92)

  p = ggplot() +
    coord_cartesian(xlim = c(0.06, 0.89), ylim = y_limits, clip = "off") +
    theme_void(base_size = 20) +
    theme(
      plot.background = element_rect(fill = palette$paper, colour = NA),
      panel.background = element_rect(fill = palette$paper, colour = NA),
      plot.margin = margin(18, 28, 54, 28)
    ) +
    annotate(
      "segment",
      x = coords$x_left,
      xend = coords$x_right,
      y = coords$y_base,
      yend = coords$y_base,
      linewidth = 1.4,
      colour = palette$ink,
      lineend = "round"
    ) +
    geom_segment(
      data = guide_data,
      aes(x = x, xend = x, y = y, yend = yend),
      inherit.aes = FALSE,
      linewidth = 1.35,
      colour = palette$ink,
      lineend = "round"
    ) +
    annotate(
      "segment",
      x = coords$x_new,
      xend = coords$x_new,
      y = coords$y_base,
      yend = 0.85,
      linewidth = 1.15,
      colour = palette$ink,
      linetype = "22",
      lineend = "round"
    ) +
    geom_point(
      data = filled_points,
      aes(x = x, y = y, fill = fill),
      inherit.aes = FALSE,
      shape = 21,
      size = 6.8,
      stroke = 1.2,
      colour = "white",
      show.legend = FALSE
    ) +
    geom_point(
      data = open_points,
      aes(x = x, y = y, colour = colour),
      inherit.aes = FALSE,
      shape = 21,
      size = 6.6,
      stroke = 1.5,
      fill = palette$paper,
      show.legend = FALSE
    ) +
    geom_text(
      data = points,
      aes(x = label_x, y = y, label = label, hjust = hjust),
      inherit.aes = FALSE,
      parse = TRUE,
      size = 7.2,
      colour = palette$ink,
      family = "serif",
      show.legend = FALSE
    ) +
    geom_text(
      data = x_label_data,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      parse = TRUE,
      size = 7.4,
      colour = palette$ink,
      family = "serif",
      show.legend = FALSE
    ) +
    scale_fill_identity() +
    scale_colour_identity()

  if (!is.null(intervals)) {
    p = p + interval_layer(intervals)
  }

  if (is_summary) {
    p = p + theme(plot.margin = margin(24, 30, 72, 30))
  }

  p
}

figures = list(
  "goldensec-0" = make_figure(),
  "goldensec-1" = make_figure(
    highlight = "new_a",
    intervals = data.table(
      x = coords$x_left,
      xend = coords$x_new,
      y = interval_bands$upper,
      label = "",
      label_y = NA_real_,
      colour = palette$blue
    )
  ),
  "goldensec-3" = make_figure(
    intervals = data.table(
      x = c(coords$x_left, coords$x_best),
      xend = c(coords$x_new, coords$x_right),
      y = c(interval_bands$upper, interval_bands$mid),
      label = c("b", "b"),
      label_y = c(interval_bands$upper + 0.05, interval_bands$mid + 0.05),
      colour = c(palette$blue, palette$blue)
    )
  ),
  "goldensec-4" = make_figure(
    intervals = data.table(
      x = c(coords$x_left, coords$x_best, coords$x_left, coords$x_new),
      xend = c(coords$x_new, coords$x_right, coords$x_best, coords$x_right),
      y = c(
        interval_bands$mid,
        interval_bands$lower,
        accent_bands$goldensec4_c,
        accent_bands$goldensec4_c
      ),
      label = c("b", "b", "c", "c"),
      label_y = c(
        interval_bands$mid + 0.05,
        interval_bands$lower + 0.05,
        accent_bands$goldensec4_c + 0.05,
        accent_bands$goldensec4_c + 0.05
      ),
      colour = c(
        palette$blue,
        palette$blue,
        palette$amber,
        palette$amber
      )
    )
  ),
  "goldensec" = make_figure(
    intervals = data.table(
      x = c(coords$x_left, coords$x_best, coords$x_left),
      xend = c(coords$x_best, coords$x_right, coords$x_right),
      y = c(interval_bands$upper, accent_bands$summary_b, interval_bands$lower),
      label = c("c", "b", "a"),
      label_y = c(
        interval_bands$upper + 0.05,
        accent_bands$summary_b + 0.05,
        interval_bands$lower + 0.05
      ),
      colour = c(
        palette$amber,
        palette$blue,
        palette$slate
      )
    ),
    is_summary = TRUE
  )
)

for (name in names(figures)) {
  ggsave(
    filename = sprintf("../figure/%s.png", name),
    plot = figures[[name]],
    width = if (name == "goldensec") 7.2 else 10.0,
    height = if (name == "goldensec") 7.2 else 5.625,
    dpi = 220,
    bg = palette$paper
  )
}
