# Used in: slides/03-univariate-optimization/slides-univariate-2-brent.tex
#
# Show the quadratic interpolation proposal, acceptance decision, and updated
# bracket for Brent's method.

set.seed(1L)

library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)

dir.create("../figure", recursive = TRUE, showWarnings = FALSE)

palette = list(
  boundary = "#2563EB",
  best = "#0F8B8D",
  new = "#C84B6C",
  highlight = "#1D4ED8",
  interval = "#DBEAFE"
)

quadratic_objective = function(x) {
  0.25 * (x - 0.75)^3 - 2 * x
}

interpolate_quadratic = function(objective_function, x_left, x_right, x_best) {
  f_left = objective_function(x_left)
  f_right = objective_function(x_right)
  f_best = objective_function(x_best)

  denominator = (x_left - x_best) * (x_left - x_right) * (x_best - x_right)
  coefficient_a = (
    x_right * (f_best - f_left) +
      x_best * (f_left - f_right) +
      x_left * (f_right - f_best)
  ) / denominator
  coefficient_b = (
    x_right^2 * (f_left - f_best) +
      x_best^2 * (f_right - f_left) +
      x_left^2 * (f_best - f_right)
  ) / denominator
  coefficient_c = (
    x_best * x_right * (x_best - x_right) * f_left +
      x_right * x_left * (x_right - x_left) * f_best +
      x_left * x_best * (x_left - x_best) * f_right
  ) / denominator

  list(
    a = coefficient_a,
    b = coefficient_b,
    c = coefficient_c,
    x_new = -coefficient_b / (2 * coefficient_a)
  )
}

arrange_with_shared_legend = function(..., ncol = length(list(...)), nrow = 1L) {
  plots = list(...)
  legend_grob = ggplotGrob(plots[[1]] + theme(legend.position = "bottom"))$grobs
  legend = legend_grob[[which(vapply(legend_grob, `[[`, character(1L), "name") == "guide-box")]]
  plots_without_legend = lapply(plots, function(plot_object) plot_object + theme(legend.position = "none"))
  plots_without_legend = c(plots_without_legend, ncol = ncol, nrow = nrow)

  arrangeGrob(
    do.call(arrangeGrob, plots_without_legend),
    legend,
    ncol = 1L,
    heights = unit.c(unit(1, "npc") - sum(legend$height), sum(legend$height))
  )
}

make_state = function(x_left, x_best, x_right, x_new = NA_real_) {
  state = data.table(
    role = c("left", "best", "right"),
    x = c(x_left, x_best, x_right)
  )

  if (!is.na(x_new)) {
    state = rbind(
      state,
      data.table(
        role = "new",
        x = x_new
      ),
      fill = TRUE
    )
  }

  state[, y := quadratic_objective(x)]
  state
}

build_bracket_plot = function(state) {
  x_left = state[role == "left", x]
  x_right = state[role == "right", x]
  interval_data = data.table(xmin = x_left, xmax = x_right)
  guide_data = copy(state[role %in% c("best", "new")])
  guide_data[, role := factor(
    role,
    levels = c("best", "new"),
    labels = c("best", "new")
  )]
  point_data = copy(state)
  point_data[role %in% c("left", "right"), point_role := "boundary"]
  point_data[role %in% c("best", "new"), point_role := role]

  ggplot(state, aes(x = x, y = y)) +
    geom_rect(
      data = interval_data,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE,
      fill = palette$interval,
      alpha = 0.45
    ) +
    stat_function(fun = quadratic_objective, linewidth = 0.8) +
    geom_vline(
      data = guide_data,
      aes(xintercept = x, colour = role, linetype = role),
      linewidth = 0.8,
      show.legend = TRUE
    ) +
    geom_point(
      data = point_data,
      aes(colour = point_role),
      size = 2.3,
      show.legend = FALSE
    ) +
    coord_cartesian(xlim = c(x_left - 1, x_right + 1)) +
    scale_colour_manual(
      values = c(
        boundary = palette$boundary,
        best = palette$best,
        new = palette$new
      ),
      labels = c(
        best = expression(x[best]),
        new = expression(x[new])
      ),
      breaks = c("best", "new"),
      name = NULL
    ) +
    scale_linetype_manual(
      values = c(
        best = "solid",
        new = "dashed"
      ),
      breaks = c("best", "new"),
      guide = "none"
    ) +
    labs(x = NULL, y = "y") +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      legend.margin = margin(0, 0, 0, 0),
      panel.grid.minor = element_blank()
    )
}

quadratic_interpolation = function(x_left, x_right, x_best, max_iter = 5L) {
  state = make_state(x_left = x_left, x_best = x_best, x_right = x_right)

  for (iter in seq_len(max_iter)) {
    interpolation = interpolate_quadratic(
      objective_function = quadratic_objective,
      x_left = state[role == "left", x],
      x_right = state[role == "right", x],
      x_best = state[role == "best", x]
    )
    state = rbind(
      state[role != "new"],
      make_state(
        x_left = state[role == "left", x],
        x_best = state[role == "best", x],
        x_right = state[role == "right", x],
        x_new = interpolation$x_new
      )[role == "new"]
    )

    proposal_plot = build_bracket_plot(state) +
      stat_function(
        fun = function(x) {
          interpolation$a * x^2 + interpolation$b * x + interpolation$c
        },
        linetype = "dashed",
        linewidth = 0.8,
        colour = "#4B5563"
      ) +
      ggtitle(bquote("Define " * x[new] * " via the parabola"))

    if (state[role == "new", y] < state[role == "best", y]) {
      decision_state = copy(state)
      new_x = decision_state[role == "new", x]
      new_y = decision_state[role == "new", y]
      old_best_x = decision_state[role == "best", x]
      old_best_y = decision_state[role == "best", y]
      decision_state[role == "best", `:=`(x = new_x, y = new_y)]
      decision_state[role == "new", `:=`(x = old_best_x, y = old_best_y)]
      decision_plot = build_bracket_plot(decision_state) +
        ggtitle(bquote("Switch " * x[new] * " and " * x[best]))
      state = decision_state
    } else {
      decision_plot = build_bracket_plot(state) +
        ggtitle(bquote("Do not switch " * x[new] * " and " * x[best]))
    }

    if (state[role == "best", x] < state[role == "new", x]) {
      updated_state = make_state(
        x_left = state[role == "left", x],
        x_best = state[role == "best", x],
        x_right = state[role == "new", x]
      )
      update_plot = build_bracket_plot(updated_state) +
        geom_vline(
          xintercept = updated_state[role == "right", x],
          colour = palette$highlight,
          linewidth = 0.8
        ) +
        ggtitle(bquote(x[new] * " is the new right boundary"))
    } else {
      updated_state = make_state(
        x_left = state[role == "new", x],
        x_best = state[role == "best", x],
        x_right = state[role == "right", x]
      )
      update_plot = build_bracket_plot(updated_state) +
        geom_vline(
          xintercept = updated_state[role == "left", x],
          colour = palette$highlight,
          linewidth = 0.8
        ) +
        ggtitle(bquote(x[new] * " is the new left boundary"))
    }

    figure = arrange_with_shared_legend(
      proposal_plot,
      decision_plot,
      update_plot,
      ncol = 3L
    )

    ggsave(
      filename = sprintf("../figure/quadratic-%d.pdf", iter),
      plot = figure,
      width = 9,
      height = 4
    )

    state = updated_state
  }
}

quadratic_interpolation(
  x_left = 0.5,
  x_right = 5,
  x_best = 3,
  max_iter = 5L
)
