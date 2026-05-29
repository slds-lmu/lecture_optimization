# Used in: slides-evolutionary-algorithms-3-ea-bit.tex
#
# Runs a simple one-max evolutionary algorithm on bit strings. The figure shows
# the best individual after each iteration, with green points marking bits that
# are set to one and the right-hand labels showing the current fitness.

set.seed(1L)

library(data.table)
library(ggplot2)

evaluate_population = function(population) {
  vapply(population, sum, integer(1L))
}

generate_binary_population = function(n_individuals, n_bits) {
  replicate(n_individuals, sample(c(0L, 1L), n_bits, replace = TRUE), simplify = FALSE)
}

mutate_bitflip = function(population, p_flip) {
  lapply(population, function(individual) {
    mutation_mask = runif(length(individual)) < p_flip
    individual[mutation_mask] = 1L - individual[mutation_mask]
    individual
  })
}

select_mu_plus_lambda = function(population, offspring, fitness, offspring_fitness, mu) {
  combined_population = c(population, offspring)
  combined_fitness = c(fitness, offspring_fitness)
  keep = order(combined_fitness, decreasing = TRUE)[seq_len(mu)]

  list(
    population = combined_population[keep],
    fitness = combined_fitness[keep]
  )
}

record_best_individual = function(population, fitness, iteration) {
  best_id = which.max(fitness)
  data.table(
    iteration = iteration,
    bit = seq_along(population[[best_id]]),
    value = population[[best_id]],
    fitness = fitness[[best_id]]
  )
}

n_bits = 15L
mu = 15L
lambda = 5L
max_iter = 150L
mutation_probability = 0.05

population = generate_binary_population(mu, n_bits)
fitness = evaluate_population(population)
best_history = list(record_best_individual(population, fitness, iteration = 0L))

for (iteration in seq_len(max_iter)) {
  parent_ids = sample(seq_len(mu), lambda)
  offspring = mutate_bitflip(population[parent_ids], p_flip = mutation_probability)
  offspring_fitness = evaluate_population(offspring)

  selection = select_mu_plus_lambda(population, offspring, fitness, offspring_fitness, mu = mu)
  population = selection$population
  fitness = selection$fitness

  best_history[[iteration + 1L]] = record_best_individual(population, fitness, iteration = iteration)

  if (max(fitness) == n_bits) {
    break
  }
}

best_history_dt = rbindlist(best_history)
fitness_labels_dt = unique(best_history_dt[, .(iteration, fitness)], by = "fitness")
max_recorded_iter = max(best_history_dt$iteration)

one_max_plot = ggplot(best_history_dt[value == 1L], aes(x = bit, y = iteration)) +
  geom_point(size = 1.2, color = "#238b45") +
  geom_text(
    data = fitness_labels_dt,
    aes(x = n_bits + 1L, y = iteration, label = fitness),
    inherit.aes = FALSE,
    size = 3
  ) +
  coord_cartesian(xlim = c(0.5, n_bits + 1.5), ylim = c(0, max_recorded_iter + 2), expand = FALSE) +
  scale_x_continuous(breaks = seq_len(n_bits)) +
  theme_bw(base_size = 12) +
  labs(x = "Bits", y = "Iteration")

dir.create("../figure", showWarnings = FALSE)

ggsave("../figure/one_max_example.pdf", one_max_plot, width = 7, height = 4)
