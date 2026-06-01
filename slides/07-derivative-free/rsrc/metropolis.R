# Used in: ../slides-derivative-free-3-simulated-annealing.tex, ../slides-derivative-free-4-multistart-optimization.tex
#
# Create the one-dimensional figures for local search, Metropolis acceptance,
# temperature-dependent acceptance probabilities, and a Nelder-Mead basin example.

set.seed(1L)

library(data.table)
library(ggplot2)

local_search_file = "../figure/metropolis-local-search.png"
metropolis_algorithm_file = "../figure/metropolis-algorithm.png"
acceptance_curve_file = "../figure/metropolis-algorithm2.png"
nelder_mead_file = "../figure/metropolis-example-nelder.png"

proposal_radius = 0.5
curve_domain = c(0, 4)
curve_points = 500L
neighborhood_points = 200L
accepted_colour = "#1a9850"
rejected_colour = "#d73027"

metropolis_palette = c("#d73027", "#f46d43", "#fee08b", "#1a9850")
accent_palette = c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0")

objective_value = function(x) {
  x^1.1 * sin(x - 2) + 1 - 2.5 * sin(3 * x - 1.5)
}

acceptance_probability = function(delta_f, temperature) {
  pmin(1, exp(-delta_f / temperature))
}

make_curve_data = function() {
  curve = data.table(x = seq(curve_domain[1], curve_domain[2], length.out = curve_points))
  curve[, value := objective_value(x)]
}

make_neighborhood = function(center, radius = proposal_radius, temperature = NA_real_) {
  neighborhood = data.table(
    center = center,
    x = seq(center - radius, center + radius, length.out = neighborhood_points)
  )
  neighborhood[, center_value := objective_value(center)]
  neighborhood[, value := objective_value(x)]

  if (is.na(temperature)) {
    neighborhood[, accepted := factor(value < center_value, levels = c(FALSE, TRUE), labels = c("reject", "accept"))]
  } else {
    neighborhood[, `:=`(
      temperature = temperature,
      acceptance_prob = acceptance_probability(value - center_value, temperature)
    )]
  }

  neighborhood
}

base_objective_plot = function(curve) {
  ggplot(curve, aes(x = x, y = value)) +
    geom_line(linewidth = 0.5, colour = "black") +
    labs(x = NULL, y = "f(x)") +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank()
    )
}

plot_local_search = function() {
  curve = make_curve_data()
  centers = c(2.5, 2.8)
  vertical_lines = data.table(center = c(centers, 3.05))
  vertical_lines[, value := objective_value(center)]
  neighborhoods = rbindlist(lapply(centers, make_neighborhood))
  labels = data.table(
    x = centers,
    y = objective_value(centers) + 0.5,
    label = c("V(x^\"[0]\")", "V(x^\"[1]\")")
  )

  base_objective_plot(curve) +
    geom_segment(
      data = vertical_lines,
      aes(x = center, xend = center, y = -3, yend = value),
      inherit.aes = FALSE,
      linetype = "dashed"
    ) +
    geom_point(
      data = neighborhoods,
      aes(x = x, y = center_value, colour = accepted),
      inherit.aes = FALSE,
      shape = 15,
      size = 0.7
    ) +
    geom_text(data = labels, aes(x = x, y = y, label = label), inherit.aes = FALSE, parse = TRUE, size = 3.3) +
    scale_colour_manual(values = c(reject = rejected_colour, accept = accepted_colour)) +
    scale_x_continuous(
      breaks = c(2.5, 2.8, 3.05),
      labels = parse(text = c("x^\"[0]\"", "x^\"[1]\"", "x^\"[2]\""))
    ) +
    coord_cartesian(ylim = c(-3, 4.2), expand = FALSE)
}

plot_metropolis_algorithm = function() {
  curve = make_curve_data()
  centers = c(2.5, 2.75, 2.3, 1.81)
  temperatures = c(3, 1.5, 0.15, 0.15)
  neighborhoods = rbindlist(Map(make_neighborhood, center = centers, temperature = temperatures))
  vertical_lines = data.table(center = centers)
  vertical_lines[, value := objective_value(center)]
  labels = data.table(
    x = centers,
    y = objective_value(centers) + 0.4,
    label = c("V(x^\"[0]\")", "V(x^\"[1]\")", "V(x^\"[2]\")", "V(x^\"[3]\")")
  )

  base_objective_plot(curve) +
    geom_segment(
      data = vertical_lines,
      aes(x = center, xend = center, y = -3, yend = value),
      inherit.aes = FALSE,
      linetype = "dashed"
    ) +
    geom_point(
      data = neighborhoods,
      aes(x = x, y = center_value, colour = acceptance_prob),
      inherit.aes = FALSE,
      shape = 15,
      size = 0.7
    ) +
    geom_text(data = labels, aes(x = x, y = y, label = label), inherit.aes = FALSE, parse = TRUE, size = 3.1) +
    scale_colour_gradientn(
      colours = metropolis_palette,
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2),
      name = "P(accept)"
    ) +
    scale_x_continuous(
      breaks = centers,
      labels = parse(text = c("x^\"[0]\"", "x^\"[1]\"", "x^\"[2]\"", "x^\"[3]\""))
    ) +
    coord_cartesian(ylim = c(-3, 4.2), expand = FALSE) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
}

plot_acceptance_curves = function() {
  delta_grid = seq(-5, 50, length.out = 500L)
  temperatures = c(250, 125, 50, 10, 2)
  acceptance_data = CJ(delta_f = delta_grid, temperature = temperatures)
  acceptance_data[, acceptance_prob := acceptance_probability(delta_f, temperature)]
  acceptance_data[, temperature_label := factor(
    paste("T =", temperature),
    levels = paste("T =", temperatures)
  )]

  ggplot(acceptance_data, aes(x = delta_f, y = acceptance_prob, colour = temperature_label)) +
    geom_hline(yintercept = c(0, 1), linetype = "dashed", linewidth = 0.3) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = accent_palette, name = NULL) +
    scale_x_continuous(name = expression(Delta[f])) +
    scale_y_continuous(name = "P(accept)", breaks = seq(0, 1, by = 0.2), limits = c(-0.1, 1.3)) +
    guides(colour = guide_legend(nrow = 1L)) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "top",
      legend.margin = margin(b = -6),
      panel.grid.minor = element_blank()
    )
}

multimodal_value = function(x) {
  x^4 + x^3 - 2 * x^2
}

optimize_from_start = function(start_value) {
  result = suppressWarnings(optim(start_value, fn = multimodal_value, method = "Nelder-Mead"))
  c(parameter = result$par, value = result$value)
}

plot_nelder_mead_basins = function() {
  start_grid = seq(-2.5, 2.5, by = 0.01)
  optimization_results = t(vapply(start_grid, optimize_from_start, numeric(2L)))
  stationary_points = sort(polyroot(c(-4, 3, 4)))
  local_minima = Re(stationary_points[abs(Im(stationary_points)) < 1e-10])
  local_minima = local_minima[local_minima != 0]

  basin_data = data.table(
    x = start_grid,
    value = multimodal_value(start_grid),
    optimum = optimization_results[, "parameter"],
    optimum_value = optimization_results[, "value"]
  )
  basin_data[, basin := ifelse(
    abs(optimum - local_minima[1]) <= abs(optimum - local_minima[2]),
    "left",
    "right"
  )]
  basin_data[, basin := factor(basin, levels = c("left", "right"))]

  ggplot(basin_data, aes(x = x, y = value)) +
    geom_line(aes(colour = basin), linewidth = 0.65) +
    geom_point(aes(y = optimum_value, shape = "optimum found by Nelder-Mead"), colour = "black", size = 0.35) +
    scale_x_continuous(
      breaks = c(-2, local_minima[1], 0, local_minima[2], 2),
      labels = c("-2", expression(x[1]), "0", expression(x[2]), "2")
    ) +
    scale_colour_manual(
      name = "basin of attraction",
      values = c(left = "#1f77b4", right = "#d62728"),
      labels = expression(x[1], x[2])
    ) +
    scale_shape_manual(name = NULL, values = c("optimum found by Nelder-Mead" = 16)) +
    guides(shape = guide_legend(override.aes = list(size = 2))) +
    coord_cartesian(xlim = c(-2.5, 2), ylim = c(-3, 3), expand = FALSE) +
    labs(x = NULL, y = "f(x)") +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
}

local_search_plot = plot_local_search()
metropolis_plot = plot_metropolis_algorithm()
acceptance_curve_plot = plot_acceptance_curves()
nelder_mead_plot = plot_nelder_mead_basins()

if (interactive()) {
  print(local_search_plot)
  print(metropolis_plot)
  print(acceptance_curve_plot)
  print(nelder_mead_plot)
}

ggsave(local_search_file, local_search_plot, width = 5.2, height = 3, dpi = 100)
ggsave(metropolis_algorithm_file, metropolis_plot, width = 5.2, height = 3, dpi = 100)
ggsave(acceptance_curve_file, acceptance_curve_plot, width = 4.8, height = 3, dpi = 100)
ggsave(nelder_mead_file, nelder_mead_plot, width = 7, height = 5.36, dpi = 341)
