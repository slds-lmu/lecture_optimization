# Used in: slides/06-constrained/slides-constrained-2-lp.tex
# Used in: slides/06-constrained/slides-constrained-2-lp-simplex.tex
#
# Builds the linear-programming figures used in the constrained optimization slides.
# The plots show a feasible polytope, objective level sets, and three LP solution cases.

set.seed(1L)

library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)

figure_dir = "../figure"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

constraint_color = "#238b45"
objective_color = "#252525"
highlight_color = "#de2d26"
fill_alpha = 0.15

save_plot = function(plot, filename, width, height) {
  ggsave(
    filename = file.path(figure_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

feasible_vertices = function(a_matrix, b_vector, tolerance = 1e-9) {
  index_pairs = combn(seq_len(nrow(a_matrix)), 2L, simplify = FALSE)

  vertices = lapply(index_pairs, function(active_index) {
    active_matrix = a_matrix[active_index, , drop = FALSE]

    if (abs(det(active_matrix)) <= tolerance) {
      NULL
    } else {
      point = as.numeric(solve(active_matrix, b_vector[active_index]))
      is_feasible = all(drop(a_matrix %*% point) <= b_vector + tolerance)

      if (is_feasible) {
        data.table(x1 = point[1L], x2 = point[2L])
      } else {
        NULL
      }
    }
  })

  vertices = Filter(Negate(is.null), vertices)

  if (length(vertices) == 0L) {
    data.table(x1 = numeric(), x2 = numeric())
  } else {
    unique(rbindlist(vertices))
  }
}

order_vertices = function(vertices) {
  if (nrow(vertices) <= 2L) {
    vertices
  } else {
    center_x1 = mean(vertices$x1)
    center_x2 = mean(vertices$x2)
    ordered_vertices = copy(vertices)
    ordered_vertices[, angle := atan2(x2 - center_x2, x1 - center_x1)]
    setorder(ordered_vertices, angle)
    ordered_vertices[, angle := NULL]
    ordered_vertices
  }
}

constraint_lines = function(a_matrix, b_vector, tolerance = 1e-9) {
  line_data = data.table(
    a1 = a_matrix[, 1L],
    a2 = a_matrix[, 2L],
    rhs = b_vector
  )

  list(
    abline = line_data[abs(a2) > tolerance][
      ,
      .(intercept = rhs / a2, slope = -a1 / a2)
    ],
    vline = line_data[abs(a2) <= tolerance & abs(a1) > tolerance][
      ,
      .(x_intercept = rhs / a1)
    ]
  )
}

base_lp_theme = function(base_size = 14L) {
  theme_bw(base_size = base_size) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )
}

plot_polytope = function(a_matrix, b_vector, x_limits = c(0, 1), y_limits = c(0, 1), base_size = 14L) {
  vertices = order_vertices(feasible_vertices(a_matrix, b_vector))
  lines = constraint_lines(a_matrix, b_vector)

  plot = ggplot() +
    geom_abline(
      data = lines$abline,
      mapping = aes(intercept = intercept, slope = slope),
      color = constraint_color,
      linewidth = 0.6
    ) +
    geom_vline(
      data = lines$vline,
      mapping = aes(xintercept = x_intercept),
      color = constraint_color,
      linewidth = 0.6
    )

  if (nrow(vertices) > 0L) {
    plot = plot +
      geom_polygon(
        data = vertices,
        mapping = aes(x = x1, y = x2),
        fill = constraint_color,
        alpha = fill_alpha
      ) +
      geom_point(
        data = vertices,
        mapping = aes(x = x1, y = x2),
        color = constraint_color,
        size = 2
      )
  }

  plot +
    coord_equal(xlim = x_limits, ylim = y_limits, expand = FALSE) +
    labs(x = expression(x[1]), y = expression(x[2])) +
    base_lp_theme(base_size)
}

plot_objective_direction = function(plot, start, end, label = "-c") {
  plot +
    geom_segment(
      aes(x = start[1L], y = start[2L], xend = end[1L], yend = end[2L]),
      arrow = arrow(length = unit(0.03, "npc")),
      linewidth = 0.5
    ) +
    geom_text(
      aes(x = start[1L], y = start[2L], label = label),
      hjust = -1.3,
      vjust = -0.4,
      size = 5
    )
}

constraint_matrix = matrix(
  c(
    1, 2,
    2, 1,
    -1, 0,
    0, -1
  ),
  ncol = 2L,
  byrow = TRUE
)
constraint_rhs = c(1, 1, 0, 0)

feasible_set_plot = plot_polytope(constraint_matrix, constraint_rhs)
save_plot(feasible_set_plot, "cons-linear-pro-example.png", width = 5, height = 5)

objective_levels = CJ(
  x1 = seq(-2, 2, by = 0.01),
  objective_value = seq(-2, 2, by = 1 / 6)
)
objective_levels[, x2 := -objective_value - x1]

opposite_direction_plot = plot_polytope(constraint_matrix, constraint_rhs) +
  geom_path(
    data = objective_levels,
    mapping = aes(x = x1, y = x2, color = objective_value, group = objective_value),
    linewidth = 0.35
  ) +
  scale_color_gradientn(colours = terrain.colors(5L)) +
  geom_abline(intercept = 2 / 3, slope = -1, color = objective_color, linewidth = 0.5) +
  labs(color = expression(c^T * x))

opposite_direction_plot = plot_objective_direction(
  opposite_direction_plot,
  start = c(1 / 3, 1 / 3),
  end = c(1 / 3 + 0.1, 1 / 3 + 0.1)
)

save_plot(opposite_direction_plot, "cons-opposite-direction.png", width = 5.5, height = 5)

case_plot = function(title, x_limits, y_limits) {
  ggplot() +
    coord_equal(xlim = x_limits, ylim = y_limits, expand = FALSE) +
    labs(title = title, x = expression(x[1]), y = expression(x[2])) +
    base_lp_theme(base_size = 12L)
}

case_1_plot = case_plot("Case 1", x_limits = c(0, 2), y_limits = c(0, 2)) +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = 1,
    ymin = -Inf,
    ymax = Inf,
    fill = constraint_color,
    alpha = fill_alpha
  ) +
  geom_polygon(
    data = data.table(x1 = c(0, 2, 2), x2 = c(2, 0, 2)),
    mapping = aes(x = x1, y = x2),
    fill = "#3182bd",
    alpha = fill_alpha
  ) +
  geom_polygon(
    data = data.table(x1 = c(0, 2, 2), x2 = c(0, 0, 1)),
    mapping = aes(x = x1, y = x2),
    fill = "#fdae6b",
    alpha = fill_alpha
  ) +
  geom_vline(xintercept = 1, color = constraint_color, linewidth = 0.6) +
  geom_abline(intercept = 2, slope = -1, color = "#3182bd", linewidth = 0.6) +
  geom_abline(intercept = 0, slope = 1 / 2, color = "#fdae6b", linewidth = 0.6)

case_2_plot = case_plot("Case 2", x_limits = c(0, 1), y_limits = c(0.5, 1.5)) +
  geom_polygon(
    data = data.table(
      x1 = c(0.5, 1, 1, 0.5, 0),
      x2 = c(0.5, 0.5, 1.5, 1.5, 1)
    ),
    mapping = aes(x = x1, y = x2),
    fill = constraint_color,
    alpha = fill_alpha
  ) +
  geom_abline(intercept = 1, slope = -1, color = constraint_color, linewidth = 0.6) +
  geom_abline(intercept = 1, slope = 1, color = constraint_color, linewidth = 0.6) +
  geom_abline(intercept = 1.5, slope = -1, color = objective_color, linewidth = 0.5)

case_2_plot = plot_objective_direction(
  case_2_plot,
  start = c(0.5, 1),
  end = c(0.75, 1.25)
)

case_3_plot = case_plot("Case 3", x_limits = c(0, 1), y_limits = c(0, 1)) +
  geom_polygon(
    data = data.table(
      x1 = c(0, 0.5, 29 / 34, 2 / 3, 1 / 3),
      x2 = c(0.5, 0, 0.29, 2 / 3, 5 / 6)
    ),
    mapping = aes(x = x1, y = x2),
    fill = constraint_color,
    alpha = fill_alpha
  ) +
  geom_abline(intercept = 2, slope = -2, color = constraint_color, linewidth = 0.6) +
  geom_abline(intercept = 1, slope = -0.5, color = constraint_color, linewidth = 0.6) +
  geom_abline(intercept = 0.5, slope = 1, color = constraint_color, linewidth = 0.6) +
  geom_abline(intercept = 0.5, slope = -1, color = constraint_color, linewidth = 0.6) +
  geom_abline(intercept = -5 / 12, slope = 5 / 6, color = constraint_color, linewidth = 0.6) +
  geom_abline(intercept = 4 / 3, slope = -1, color = objective_color, linewidth = 0.5) +
  geom_point(aes(x = 2 / 3, y = 2 / 3), size = 2, color = highlight_color)

case_3_plot = plot_objective_direction(
  case_3_plot,
  start = c(2 / 3, 2 / 3),
  end = c(5 / 6, 5 / 6)
)

solution_cases_plot = arrangeGrob(case_1_plot, case_2_plot, case_3_plot, ncol = 3L)

if (interactive()) {
  grid.draw(solution_cases_plot)
}

save_plot(solution_cases_plot, "cons-solutions-lp.png", width = 12, height = 4)
