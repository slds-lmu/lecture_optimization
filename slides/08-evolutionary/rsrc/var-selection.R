# Used in: slides-evolutionary-algorithms-3-ea-bit.tex
#
# Runs a toy evolutionary feature-selection example for linear regression. The
# binary chromosome indicates which variables enter the model, and BIC is used
# as the fitness value to minimize.

set.seed(123L)

library(data.table)
library(ggplot2)

evaluate_population = function(population, objective) {
  vapply(population, objective, numeric(1L))
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

uniform_crossover = function(parent_a, parent_b, p_parent_a) {
  inherit_from_parent_a = runif(length(parent_a)) < p_parent_a
  as.integer(ifelse(inherit_from_parent_a, parent_a, parent_b))
}

generate_offspring = function(population, fitness, lambda, p_mutation, p_crossover) {
  ranked_ids = order(fitness)[seq_len(min(length(population), lambda))]
  mating_pool = population[ranked_ids]

  replicate(lambda, {
    parent_ids = sample(seq_along(mating_pool), size = 2L, replace = TRUE)
    child = uniform_crossover(
      mating_pool[[parent_ids[1L]]],
      mating_pool[[parent_ids[2L]]],
      p_parent_a = p_crossover
    )
    mutate_bitflip(list(child), p_flip = p_mutation)[[1L]]
  }, simplify = FALSE)
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

make_bic_objective = function(design, response) {
  function(individual) {
    selected_variables = which(individual == 1L)

    model_data = if (length(selected_variables) == 0L) {
      data.frame(response = response)
    } else {
      data.frame(response = response, design[, selected_variables, drop = FALSE])
    }

    BIC(lm(response ~ ., data = model_data))
  }
}

record_best_individual = function(population, fitness, iteration, n_features) {
  best_id = which.min(fitness)
  data.table(
    iteration = iteration,
    variable = seq_len(n_features),
    included = population[[best_id]],
    fitness = fitness[[best_id]]
  )
}

n_observations = 1000L
n_features = 50L
true_variables = as.integer(seq(1L, 43L, length.out = 8L))
feature_sd = seq(1, 5, length.out = n_features)

design = matrix(
  rnorm(n_observations * n_features),
  nrow = n_observations,
  ncol = n_features
)
design = sweep(design, MARGIN = 2L, STATS = feature_sd, FUN = "*")

response = -1.2 + rowSums(design[, true_variables]) + rnorm(n_observations)
objective = make_bic_objective(design, response)

mu = 100L
lambda = 50L
max_iter = 25L
mutation_probability = 1 / n_features
crossover_probability = 0.5

population = generate_binary_population(mu, n_features)
fitness = evaluate_population(population, objective)

fitness_history = list(data.table(iteration = 0L, individual = seq_along(fitness), bic = fitness))
best_history = list(record_best_individual(population, fitness, iteration = 0L, n_features = n_features))

for (iteration in seq_len(max_iter)) {
  offspring = generate_offspring(
    population = population,
    fitness = fitness,
    lambda = lambda,
    p_mutation = mutation_probability,
    p_crossover = crossover_probability
  )
  offspring_fitness = evaluate_population(offspring, objective)

  selection = select_mu_plus_lambda(population, offspring, fitness, offspring_fitness, mu = mu)
  population = selection$population
  fitness = selection$fitness

  fitness_history[[iteration + 1L]] = data.table(
    iteration = iteration,
    individual = seq_along(fitness),
    bic = fitness
  )
  best_history[[iteration + 1L]] = record_best_individual(
    population,
    fitness,
    iteration = iteration,
    n_features = n_features
  )
}

fitness_history_dt = rbindlist(fitness_history)
best_fitness_dt = fitness_history_dt[, .(bic = min(bic)), by = iteration]

bic_plot = ggplot(fitness_history_dt, aes(x = factor(iteration), y = bic)) +
  geom_boxplot(fill = "grey90", color = "grey35", outlier.size = 0.5) +
  geom_line(
    data = best_fitness_dt,
    aes(x = factor(iteration), y = bic, group = 1L, color = "Best individual"),
    linewidth = 0.8,
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = c("Best individual" = "#d7301f"), name = NULL) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom") +
  labs(
    x = "Iteration",
    y = "BIC",
    title = "Fitness values (BIC) of the population per iteration"
  )

best_history_dt = rbindlist(best_history)

variable_plot = ggplot(best_history_dt, aes(x = variable, y = iteration)) +
  geom_point(color = "grey85", size = 5, alpha = 0.45) +
  geom_point(
    data = best_history_dt[included == 1L],
    aes(x = variable, y = iteration),
    color = "#238b45",
    size = 2.6,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(breaks = seq(1L, n_features, by = 5L)) +
  theme_bw(base_size = 12) +
  labs(
    x = "Variable",
    y = "Iteration",
    title = "Included variables (green) per iteration"
  )

dir.create("../figure", showWarnings = FALSE)

ggsave("../figure/var-selection1.png", bic_plot, width = 7, height = 4, dpi = 300)
ggsave("../figure/var-selection2.png", variable_plot, width = 7, height = 4, dpi = 300)
