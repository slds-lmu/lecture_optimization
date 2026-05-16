# Used in: slides/03-univariate-optimization/slides-univariate-1-golden-ratio.tex
#
# Create the simple nesting animation frames and the summary figure for the
# univariate optimization slides.

set.seed(1L)

library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)

dir.create("../figure", recursive = TRUE, showWarnings = FALSE)

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
  badge = "#F59E0B",
  interval = "#F4B942",
  interval_fill = "#FCE7B2"
)

x_limits = c(-0.05, 1.60)
y_limits = c(-0.30, 1.32)
axis_x = -0.01
axis_y = 0
label_y = -0.17
badge_x = 0.78
badge_y = 1.16

curve_data = data.table(x = seq(0, 1.5, length.out = 800L))
curve_data[, y := objective(x)]

point_styles = data.table(
  role = c("left", "right", "best", "new", "new_best", "old_best"),
  label = c(
    "italic(x)[left]",
    "italic(x)[right]",
    "italic(x)[best]",
    "italic(x)[new]",
    "italic(x)[best]",
    "italic(x)[new]"
  ),
  fill = c(
    palette$boundary,
    palette$boundary,
    palette$best,
    palette$new,
    palette$best,
    palette$new
  )
)

make_points = function(x_values) {
  points = data.table(
    role = names(x_values),
    x = as.numeric(x_values)
  )
  points$label = point_styles$label[match(points$role, point_styles$role)]
  points$fill = point_styles$fill[match(points$role, point_styles$role)]
  points$y = objective(points$x)
  setorder(points, x)
  points
}

make_frame = function(points = NULL, badge = NULL, interval = NULL, compare = NULL) {
  p = ggplot(curve_data, aes(x = x, y = y)) +
    coord_cartesian(xlim = x_limits, ylim = y_limits, clip = "off") +
    theme_void(base_size = 22) +
    theme(
      plot.background = element_rect(fill = palette$paper, colour = NA),
      panel.background = element_rect(fill = palette$paper, colour = NA),
      plot.margin = margin(20, 24, 36, 24)
    )

  if (!is.null(interval)) {
    p = p +
      annotate(
        "rect",
        xmin = interval[1],
        xmax = interval[2],
        ymin = axis_y,
        ymax = y_limits[2] - 0.18,
        fill = palette$interval_fill,
        alpha = 0.40
      ) +
      annotate(
        "segment",
        x = interval[1],
        xend = interval[2],
        y = axis_y + 0.025,
        yend = axis_y + 0.025,
        colour = palette$interval,
        linewidth = 6,
        alpha = 0.95,
        lineend = "round"
      )
  }

  p = p +
    annotate(
      "segment",
      x = axis_x,
      xend = x_limits[2] - 0.03,
      y = axis_y,
      yend = axis_y,
      colour = palette$ink,
      linewidth = 0.8,
      arrow = arrow(length = unit(0.18, "cm"), type = "closed")
    ) +
    annotate(
      "segment",
      x = axis_x,
      xend = axis_x,
      y = axis_y,
      yend = y_limits[2] - 0.06,
      colour = palette$ink,
      linewidth = 0.8,
      arrow = arrow(length = unit(0.18, "cm"), type = "closed")
    ) +
    annotate(
      "text",
      x = x_limits[2] - 0.05,
      y = axis_y - 0.055,
      label = "x",
      colour = palette$ink,
      fontface = "bold",
      size = 6
    ) +
    annotate(
      "text",
      x = axis_x - 0.07,
      y = 0.62,
      label = "y",
      colour = palette$ink,
      fontface = "bold",
      size = 6
    ) +
    geom_line(
      colour = palette$curve,
      linewidth = 1.25,
      lineend = "round"
    )

  if (!is.null(points) && nrow(points) > 0L) {
    stem_y = pmax(points$y - 0.045, axis_y + 0.045)

    p = p +
      geom_segment(
        data = transform(points, stem_y = stem_y),
        aes(x = x, xend = x, y = axis_y + 0.015, yend = stem_y, colour = fill),
        inherit.aes = FALSE,
        linewidth = 0.65,
        alpha = 0.55,
        show.legend = FALSE
      ) +
      geom_point(
        data = points,
        aes(fill = fill),
        shape = 21,
        size = 5.2,
        stroke = 1.1,
        colour = "white",
        show.legend = FALSE
      ) +
      geom_label(
        data = points,
        aes(x = x, y = label_y, label = label, fill = fill),
        inherit.aes = FALSE,
        angle = 90,
        colour = "white",
        parse = TRUE,
        fontface = "bold",
        size = 4.6,
        linewidth = 0,
        label.padding = unit(0.18, "lines"),
        label.r = unit(0.18, "lines"),
        show.legend = FALSE
      ) +
      scale_fill_identity() +
      scale_colour_identity()
  }

  if (!is.null(compare)) {
    compare_y = max(objective(compare)) + 0.11
    p = p +
      annotate(
        "segment",
        x = min(compare),
        xend = max(compare),
        y = compare_y,
        yend = compare_y,
        colour = palette$badge,
        linewidth = 1.1,
        arrow = arrow(
          length = unit(0.18, "cm"),
          type = "closed",
          ends = "both"
        )
      )
  }

  if (!is.null(badge)) {
    p = p +
      annotate(
        "label",
        x = badge_x,
        y = badge_y,
        label = badge,
        fill = palette$badge,
        colour = "white",
        fontface = "bold",
        size = 6.2,
        label.padding = unit(0.28, "lines"),
        label.r = unit(0.18, "lines")
      )
  }

  p
}

states = list(
  list(id = 0L),
  list(
    id = 1L,
    points = make_points(c(left = 0.05, best = 0.95, right = 1.45))
  ),
  list(
    id = 2L,
    points = make_points(c(left = 0.05, new = 0.58, best = 0.95, right = 1.45)),
    badge = "New proposal"
  ),
  list(
    id = 3L,
    points = make_points(c(left = 0.05, new = 0.58, best = 0.95, right = 1.45)),
    badge = "Compare objective values",
    compare = c(0.58, 0.95)
  ),
  list(
    id = 4L,
    points = make_points(c(left = 0.05, new_best = 0.58, old_best = 0.95, right = 1.45)),
    badge = "Accept proposal"
  ),
  list(
    id = 5L,
    points = make_points(c(left = 0.05, best = 0.58, right = 0.95)),
    badge = "Shrink search interval",
    interval = c(0.05, 0.95)
  ),
  list(
    id = 6L,
    points = make_points(c(left = 0.05, best = 0.58, new = 0.78, right = 0.95)),
    badge = "New proposal"
  ),
  list(
    id = 7L,
    points = make_points(c(left = 0.05, best = 0.58, new = 0.78, right = 0.95)),
    badge = "Compare objective values",
    compare = c(0.58, 0.78)
  ),
  list(
    id = 8L,
    points = make_points(c(left = 0.05, best = 0.58, new = 0.78, right = 0.95)),
    badge = "Keep current best"
  ),
  list(
    id = 9L,
    points = make_points(c(left = 0.05, best = 0.58, right = 0.78)),
    badge = "Shrink search interval",
    interval = c(0.05, 0.78)
  ),
  list(
    id = 10L,
    points = make_points(c(left = 0.05, best = 0.58, new = 0.42, right = 0.78)),
    badge = "New proposal"
  ),
  list(
    id = 11L,
    points = make_points(c(left = 0.05, best = 0.58, new = 0.42, right = 0.78)),
    badge = "Compare objective values",
    compare = c(0.42, 0.58)
  ),
  list(
    id = 12L,
    points = make_points(c(left = 0.05, new_best = 0.42, old_best = 0.58, right = 0.78)),
    badge = "Accept proposal"
  ),
  list(
    id = 13L,
    points = make_points(c(left = 0.05, best = 0.42, right = 0.58)),
    badge = "Shrink search interval",
    interval = c(0.05, 0.58)
  )
)

for (state in states) {
  plot = make_frame(
    points = state$points,
    badge = state$badge,
    interval = state$interval,
    compare = state$compare
  )

  ggsave(
    filename = sprintf("../figure/golden-ratio-%d.png", state$id),
    plot = plot,
    width = 9.6,
    height = 5.4,
    dpi = 100,
    bg = palette$paper
  )
}

summary_plot = arrangeGrob(
  grobs = list(
    make_frame(
      points = make_points(c(left = 0.05, new = 0.58, best = 0.95, right = 1.45)),
      badge = "Compare objective values",
      compare = c(0.58, 0.95)
    ),
    make_frame(
      points = make_points(c(left = 0.05, new_best = 0.58, old_best = 0.95, right = 1.45)),
      badge = "Accept proposal"
    ),
    make_frame(
      points = make_points(c(left = 0.05, best = 0.58, right = 0.95)),
      badge = "Shrink search interval",
      interval = c(0.05, 0.95)
    )
  ),
  ncol = 3
)

ggsave(
  filename = "../figure/golden-ratio-summary.png",
  plot = summary_plot,
  width = 14.08,
  height = 4.68,
  dpi = 100,
  bg = palette$paper
)
