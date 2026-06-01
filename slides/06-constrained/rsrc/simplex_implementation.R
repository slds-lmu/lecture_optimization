# Used in: slides/06-constrained/slides-constrained-3-lp-solvers.tex
#
# Runs a small two-dimensional simplex example and saves one plot per iteration.
# Each plot shows the feasible polytope, visited vertices, and objective level lines.

set.seed(1L)

library(data.table)
library(ggplot2)
library(grid)

figure_dir = "../figure"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

constraint_color = "#238b45"
trace_color = "#de2d26"
fill_alpha = 0.15

constraint_matrix = matrix(
  c(
    -1, 1,
    1, 2,
    2, 1,
    1, -1,
    -1, 0,
    0, -1
  ),
  ncol = 2L,
  byrow = TRUE
)
constraint_rhs = c(0.5, 2, 2, 0.5, 0, 0)
start_point = c(0, 0)
cost_vector = c(-1, -1)

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

plot_polytope = function(a_matrix, b_vector) {
  vertices = order_vertices(feasible_vertices(a_matrix, b_vector))
  lines = constraint_lines(a_matrix, b_vector)

  ggplot() +
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
    ) +
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
    ) +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    labs(x = expression(x[1]), y = expression(x[2])) +
    theme_bw(base_size = 14L) +
    theme(plot.title = element_text(hjust = 0.5))
}

select_active_basis = function(a_matrix, active_set, n_variables, tolerance = 1e-9) {
  if (length(active_set) < n_variables) {
    stop("The current point has too few active constraints to identify a simplex basis.")
  }

  basis_candidates = combn(active_set, n_variables, simplify = FALSE)
  valid_basis = vapply(
    basis_candidates,
    function(basis_rows) {
      basis_matrix = a_matrix[basis_rows, , drop = FALSE]
      abs(det(basis_matrix)) > tolerance
    },
    logical(1L)
  )

  selected_index = which(valid_basis)[1L]

  if (is.na(selected_index)) {
    stop("The active constraints do not contain a nonsingular simplex basis.")
  }

  basis_candidates[[selected_index]]
}

trace_row = function(point, previous_point, cost) {
  objective_value = as.numeric(cost %*% point)

  data.table(
    x1 = point[1L],
    x2 = point[2L],
    x1_old = previous_point[1L],
    x2_old = previous_point[2L],
    objective_value = round(objective_value, 2L),
    objective_label = sprintf("%.2f", round(objective_value, 2L)),
    intercept = objective_value / cost[2L],
    slope = -cost[1L] / cost[2L]
  )
}

simplex_trace = function(a_matrix, b_vector, cost, start, tolerance = 1e-8, max_iterations = 20L) {
  n_variables = ncol(a_matrix)
  point = start
  previous_point = c(NA_real_, NA_real_)
  trace = list(trace_row(point, previous_point, cost))

  for (iteration in seq_len(max_iterations)) {
    active_set = which(abs(drop(a_matrix %*% point - b_vector)) <= tolerance)
    basis_rows = select_active_basis(a_matrix, active_set, n_variables, tolerance)
    edge_matrix = -solve(a_matrix[basis_rows, , drop = FALSE])
    objective_improvement = drop(-cost %*% edge_matrix)

    if (all(objective_improvement <= tolerance)) {
      break
    }

    edge_index = which(objective_improvement > tolerance)[1L]
    edge = edge_matrix[, edge_index]
    blocking_constraints = which(drop(a_matrix %*% edge) > tolerance)

    if (length(blocking_constraints) == 0L) {
      stop("The linear program is unbounded in the selected simplex direction.")
    }

    step_lengths = (
      b_vector[blocking_constraints] - drop(a_matrix[blocking_constraints, , drop = FALSE] %*% point)
    ) / drop(a_matrix[blocking_constraints, , drop = FALSE] %*% edge)

    positive_steps = step_lengths[step_lengths > tolerance]

    if (length(positive_steps) == 0L) {
      stop("No positive simplex step length found.")
    }

    previous_point = point
    point = point + min(positive_steps) * edge
    trace[[length(trace) + 1L]] = trace_row(point, previous_point, cost)
  }

  rbindlist(trace)
}

simplex_path = simplex_trace(constraint_matrix, constraint_rhs, cost_vector, start_point)

for (iteration in seq_len(min(4L, nrow(simplex_path)))) {
  visible_path = simplex_path[seq_len(iteration)]
  visible_steps = visible_path[!is.na(x1_old)]
  label_positions = copy(visible_path)
  label_positions[, `:=`(
    label_x = x1,
    label_y = x2,
    hjust = -0.2,
    vjust = -1
  )]
  label_positions[objective_label == "-0.50", `:=`(
    label_x = x1,
    label_y = x2 + 0.1,
    hjust = 0.5,
    vjust = 0.5
  )]
  label_positions[objective_label == "-1.17", `:=`(
    label_x = x1 + 0.06,
    label_y = x2 + 0,
    hjust = 0.5,
    vjust = 0.5
  )]

  plot = plot_polytope(constraint_matrix, constraint_rhs) +
    labs(title = sprintf("Iteration %s", iteration)) +
    geom_abline(
      data = visible_path,
      mapping = aes(intercept = intercept, slope = slope),
      linetype = "dashed",
      linewidth = 0.5
    ) +
    geom_point(
      data = visible_path,
      mapping = aes(x = x1, y = x2),
      color = trace_color,
      size = 2
    ) +
    geom_text(
      data = label_positions,
      mapping = aes(x = label_x, y = label_y, label = objective_label, hjust = hjust, vjust = vjust),
      size = 4
    ) +
    geom_segment(
      data = visible_steps,
      mapping = aes(x = x1_old, y = x2_old, xend = x1, yend = x2),
      color = trace_color,
      arrow = arrow(length = unit(0.03, "npc")),
      linewidth = 0.6
    )

  ggsave(
    filename = file.path(figure_dir, sprintf("simplex_implementation-%s.png", iteration)),
    plot = plot,
    width = 5,
    height = 5,
    dpi = 300,
    bg = "white"
  )
}
