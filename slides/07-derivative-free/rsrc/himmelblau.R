# Used in: ../slides-derivative-free-3-simulated-annealing.tex
#
# Simulate a short simulated annealing run on Himmelblau's function and save
# the acceptance-probability contour used on the title slide.

set.seed(1111L)

library(data.table)
library(ggplot2)
library(msm)

output_file = "../figure/himmelblau-iter3_probabilities.pdf"

domain = c(-5, 5)
proposal_bounds = c(-3.8, 3.8)
proposal_sd = 1.5
objective_grid_size = 400L
proposal_grid_size = 100L
proposal_contour_radius = 2
initial_temperature = 200
cooling_rate = 0.8
snapshot_iterations = c(2L, 3L, 5L, 10L, 20L, 30L, 50L, 60L)
snapshot_index = 3L

himmelblau_value = function(x1, x2) {
  (x1 + x2^2 - 11)^2 + (x1^2 + x2 - 7)^2
}

himmelblau = function(x) {
  himmelblau_value(x[1], x[2])
}

acceptance_probability = function(proposal_value, current_value, temperature) {
  pmin(1, exp(-(proposal_value - current_value) / temperature))
}

sample_proposal = function(current_point) {
  c(
    rtnorm(1L, mean = current_point[1], sd = proposal_sd, lower = proposal_bounds[1], upper = proposal_bounds[2]),
    rtnorm(1L, mean = current_point[2], sd = proposal_sd, lower = proposal_bounds[1], upper = proposal_bounds[2])
  )
}

make_objective_grid = function() {
  axis_values = seq(domain[1], domain[2], length.out = objective_grid_size)
  grid = CJ(x1 = axis_values, x2 = axis_values)
  grid[, objective_value := himmelblau_value(x1, x2)]
}

make_temperature_schedule = function() {
  c(
    rep(initial_temperature, 50L),
    initial_temperature * cooling_rate^seq_len(200L)
  )
}

run_simulated_annealing = function(temperature_schedule) {
  trace = data.table(
    iteration = seq_along(temperature_schedule),
    best_x1 = NA_real_,
    best_x2 = NA_real_,
    proposal_x1 = NA_real_,
    proposal_x2 = NA_real_,
    current_x1 = NA_real_,
    current_x2 = NA_real_,
    best_value = NA_real_,
    accepted = NA_character_
  )

  current_point = c(0, 0)
  best_point = current_point

  trace[1L, `:=`(
    best_x1 = best_point[1],
    best_x2 = best_point[2],
    proposal_x1 = current_point[1],
    proposal_x2 = current_point[2],
    current_x1 = current_point[1],
    current_x2 = current_point[2],
    best_value = himmelblau(best_point)
  )]

  for (iter in seq(2L, length(temperature_schedule))) {
    old_point = current_point
    proposal = sample_proposal(old_point)
    old_value = himmelblau(old_point)
    proposal_value = himmelblau(proposal)

    accept_candidate = if (proposal_value < old_value) {
      TRUE
    } else {
      runif(1L) < acceptance_probability(proposal_value, old_value, temperature_schedule[iter])
    }

    if (accept_candidate) {
      current_point = proposal
    }
    if (himmelblau(current_point) < himmelblau(best_point)) {
      best_point = current_point
    }

    trace[iter, `:=`(
      best_x1 = best_point[1],
      best_x2 = best_point[2],
      proposal_x1 = proposal[1],
      proposal_x2 = proposal[2],
      current_x1 = old_point[1],
      current_x2 = old_point[2],
      best_value = himmelblau(best_point),
      accepted = if (accept_candidate) "accept" else "reject"
    )]
  }

  trace
}

make_acceptance_grid = function(objective_grid, current_point, temperature) {
  current_value = himmelblau(current_point)
  acceptance_grid = copy(objective_grid)
  acceptance_grid[, acceptance_prob := acceptance_probability(objective_value, current_value, temperature)]
}

make_proposal_density_grid = function(current_point) {
  x1 = seq(
    current_point[1] - proposal_contour_radius,
    current_point[1] + proposal_contour_radius,
    length.out = proposal_grid_size
  )
  x2 = seq(
    current_point[2] - proposal_contour_radius,
    current_point[2] + proposal_contour_radius,
    length.out = proposal_grid_size
  )

  density_grid = CJ(x1 = x1, x2 = x2)
  density_grid[, proposal_density := dnorm(x1, mean = current_point[1], sd = proposal_sd) *
    dnorm(x2, mean = current_point[2], sd = proposal_sd)]
}

plot_acceptance_snapshot = function(trace, objective_grid, temperature_schedule, iteration_id) {
  current_row = trace[iteration_id]
  current_point = c(current_row$current_x1, current_row$current_x2)

  acceptance_grid = make_acceptance_grid(
    objective_grid = objective_grid,
    current_point = current_point,
    temperature = temperature_schedule[iteration_id]
  )
  density_grid = make_proposal_density_grid(current_point)
  history = trace[seq_len(iteration_id)]

  ggplot() +
    geom_contour(
      data = acceptance_grid,
      aes(x = x1, y = x2, z = acceptance_prob, colour = after_stat(level)),
      bins = 10L,
      linewidth = 0.45
    ) +
    geom_contour(
      data = density_grid,
      aes(x = x1, y = x2, z = proposal_density),
      alpha = 0.5,
      linewidth = 0.35
    ) +
    geom_point(data = history, aes(x = current_x1, y = current_x2), colour = "grey75", size = 1.2) +
    geom_point(data = current_row, aes(x = current_x1, y = current_x2), colour = "#1f5fbf", size = 2.2) +
    geom_point(data = current_row, aes(x = proposal_x1, y = proposal_x2), colour = "#f28e2b", size = 2.2) +
    scale_colour_gradient(low = "#d73027", high = "#1a9850", limits = c(0, 1), name = "P(accept)") +
    coord_cartesian(xlim = domain, ylim = domain, expand = FALSE) +
    labs(x = expression(x[1]), y = expression(x[2])) +
    theme_bw(base_size = 11) +
    theme(legend.position = "right")
}

objective_grid = make_objective_grid()
temperature_schedule = make_temperature_schedule()
annealing_trace = run_simulated_annealing(temperature_schedule)
plot_iteration = snapshot_iterations[snapshot_index]

acceptance_plot = plot_acceptance_snapshot(
  trace = annealing_trace,
  objective_grid = objective_grid,
  temperature_schedule = temperature_schedule,
  iteration_id = plot_iteration
)

if (interactive()) {
  print(acceptance_plot)
}

ggsave(output_file, acceptance_plot, width = 5, height = 4)
