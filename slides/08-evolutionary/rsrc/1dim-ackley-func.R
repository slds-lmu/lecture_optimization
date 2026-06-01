# Used in: slides-evolutionary-algorithms-2-ea-numeric.tex
#
# Builds the staged figures for a simple (mu + lambda) evolutionary strategy on
# the one-dimensional Ackley objective. The same run is used across the slide
# sequence, so initialization, parent selection, mutation, survival selection,
# and the final optimization trace remain consistent.

set.seed(1212L)

suppressWarnings(library(vistool))
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)

ackley_1d = function(x) {
  z = x[1L]
  -20 * exp(-0.2 * abs(z)) - exp(cos(2 * pi * z)) + 20 + exp(1)
}

evaluate_population = function(population) {
  vapply(population, function(individual) ackley_1d(individual), numeric(1L))
}

mutate_gaussian = function(population, sdev, lower, upper) {
  mutated = population + rnorm(length(population), sd = sdev)
  pmin(upper, pmax(lower, mutated))
}

select_mu_plus_lambda = function(population, offspring, fitness, offspring_fitness, mu) {
  combined_population = c(population, offspring)
  combined_fitness = c(fitness, offspring_fitness)
  keep = order(combined_fitness)[seq_len(mu)]

  list(
    population = combined_population[keep],
    fitness = combined_fitness[keep]
  )
}

make_ackley_plot = function(objective) {
  plot_file = tempfile(fileext = ".pdf")
  pdf(plot_file)
  on.exit({
    dev.off()
    unlink(plot_file)
  }, add = TRUE)

  ackley_plot = as_visualizer(objective, n_points = 1000L)$plot(
    show_title = FALSE,
    x_lab = "x",
    y_lab = "f(x)"
  )

  ackley_plot +
    theme_bw(base_size = 12)
}

plot_population = function(base_plot, population_dt, color, size = 3) {
  base_plot +
    geom_point(
      data = population_dt,
      mapping = aes(x = x, y = fitness),
      color = color,
      size = size,
      inherit.aes = FALSE
    )
}

arrange_plots = function(...) {
  plot_file = tempfile(fileext = ".pdf")
  pdf(plot_file)
  on.exit({
    dev.off()
    unlink(plot_file)
  }, add = TRUE)

  arrangeGrob(...)
}

lower = -30
upper = 30
mu = 10L
lambda = 5L
max_iter = 50L
mutation_sd = 2

objective = Objective$new(
  id = "ackley_1d",
  fun = ackley_1d,
  label = "Ackley",
  xdim = 1L,
  lower = lower,
  upper = upper,
  minimize = TRUE
)

population = runif(mu, min = lower, max = upper)
fitness = evaluate_population(population)
population_dt = data.table(x = population, fitness = fitness)

base_plot = make_ackley_plot(objective)
initial_population_plot = plot_population(base_plot, population_dt, color = "black")

parent_ids = sample(seq_len(mu), lambda)
parents_dt = population_dt[parent_ids]
neutral_selection_plot = initial_population_plot +
  geom_point(
    data = parents_dt,
    mapping = aes(x = x, y = fitness),
    color = "#d7301f",
    size = 3,
    inherit.aes = FALSE
  ) +
  ggtitle("Neutral selection")

offspring = mutate_gaussian(population[parent_ids], sdev = mutation_sd, lower = lower, upper = upper)
offspring_fitness = evaluate_population(offspring)
offspring_dt = data.table(
  x = offspring,
  fitness = offspring_fitness,
  parent_x = parents_dt$x,
  parent_fitness = parents_dt$fitness
)

gaussian_mutation_plot = neutral_selection_plot +
  geom_segment(
    data = offspring_dt,
    mapping = aes(x = parent_x, y = parent_fitness, xend = x, yend = fitness),
    color = "#d7301f",
    arrow = arrow(length = unit(0.01, "npc")),
    inherit.aes = FALSE
  ) +
  geom_point(
    data = offspring_dt,
    mapping = aes(x = x, y = fitness),
    color = "#d7301f",
    size = 3,
    inherit.aes = FALSE
  ) +
  ggtitle("Gaussian mutation")

selection = select_mu_plus_lambda(population, offspring, fitness, offspring_fitness, mu = mu)
population = selection$population
fitness = selection$fitness
selected_dt = data.table(x = population, fitness = fitness)

survival_selection_plot = gaussian_mutation_plot +
  geom_point(
    data = selected_dt,
    mapping = aes(x = x, y = fitness),
    color = "#1a9850",
    size = 3,
    inherit.aes = FALSE
  ) +
  geom_hline(yintercept = max(selected_dt$fitness), linetype = "dashed") +
  ggtitle(expression(group("(", mu + lambda, ")") * "-selection"))

population_history = vector("list", max_iter)
best_history = numeric(max_iter)

for (iteration in seq_len(max_iter)) {
  parent_ids = sample(seq_len(mu), lambda)
  offspring = mutate_gaussian(population[parent_ids], sdev = mutation_sd, lower = lower, upper = upper)
  offspring_fitness = evaluate_population(offspring)

  selection = select_mu_plus_lambda(population, offspring, fitness, offspring_fitness, mu = mu)
  population = selection$population
  fitness = selection$fitness

  population_history[[iteration]] = data.table(
    x = population,
    fitness = fitness,
    iteration = iteration
  )
  best_history[iteration] = min(fitness)
}

best_history_dt = data.table(iteration = seq_len(max_iter), fitness = best_history)
population_history_dt = rbindlist(population_history)

fitness_trace_plot = ggplot(best_history_dt, aes(x = iteration, y = fitness)) +
  geom_line(linewidth = 0.8, color = "#225ea8") +
  theme_bw(base_size = 12) +
  labs(x = "Iteration", y = "Fitness (best individual)")

final_population_plot = make_ackley_plot(objective) +
  geom_point(
    data = population_history_dt,
    mapping = aes(x = x, y = fitness, color = iteration),
    size = 1.7,
    alpha = 0.7,
    inherit.aes = FALSE
  ) +
  scale_color_viridis_c(name = "Iteration")

final_plot = arrange_plots(fitness_trace_plot, final_population_plot, nrow = 1L)

dir.create("../figure", showWarnings = FALSE)

ggsave("../figure/1dim-ackley-func.png", base_plot, width = 5, height = 4, dpi = 300)
ggsave("../figure/1dim-ackley-func-2.png", initial_population_plot, width = 5, height = 4, dpi = 300)
ggsave(
  "../figure/1dim-ackley-func-neutral-selec.png",
  neutral_selection_plot,
  width = 5,
  height = 4,
  dpi = 300
)
ggsave(
  "../figure/1dim-ackley-func-gaussian-mutation.png",
  gaussian_mutation_plot,
  width = 5,
  height = 4,
  dpi = 300
)
ggsave(
  "../figure/1dim-ackley-func-selection.png",
  survival_selection_plot,
  width = 5,
  height = 4,
  dpi = 300
)
ggsave("../figure/1dim-ackley-func-final.png", final_plot, height = 4, width = 8, dpi = 300)
