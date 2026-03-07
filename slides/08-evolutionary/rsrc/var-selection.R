# ------------------------------------------------------------------------------
# evolutionary algorithms

# FIG: variable selection using BIC
# ------------------------------------------------------------------------------

library(knitr)
library(ecr)
library(ggplot2)
library(smoof)
library(latex2exp)
library(ggforce)
library(reshape2)
library(mlr)

set.seed(123)
# ------------------------------------------------------------------------------

evaluate_population = function(population, objective) {
  vapply(population, objective, numeric(1L))
}

generate_binary_population = function(n, dimension) {
  replicate(n, sample(c(0L, 1L), dimension, replace = TRUE), simplify = FALSE)
}

mutate_bitflip = function(population, p) {
  lapply(population, function(individual) {
    mutation_mask = stats::runif(length(individual)) < p
    individual[mutation_mask] = 1L - individual[mutation_mask]
    individual
  })
}

uniform_crossover = function(parent_a, parent_b, p) {
  inherit_from_a = stats::runif(length(parent_a)) < p
  ifelse(inherit_from_a, parent_a, parent_b)
}

generate_offspring = function(population, fitness, lambda, p_mut, p_cross) {
  ranked_ids = order(fitness)[seq_len(min(length(population), lambda))]
  mating_pool = population[ranked_ids]

  lapply(seq_len(lambda), function(i) {
    parent_ids = sample(seq_along(mating_pool), size = 2L, replace = TRUE)
    child = uniform_crossover(
      mating_pool[[parent_ids[1L]]],
      mating_pool[[parent_ids[2L]]],
      p = p_cross
    )
    mutate_bitflip(list(child), p = p_mut)[[1L]]
  })
}

select_mu_plus_lambda = function(population, offspring, fitness, fitness_offspring, mu) {
  combined_population = c(population, offspring)
  combined_fitness = c(fitness, fitness_offspring)
  keep = order(combined_fitness)[seq_len(mu)]

  list(
    population = combined_population[keep],
    fitness = combined_fitness[keep]
  )
}

X = matrix(rnorm(50000, sd = 1:5), ncol = 50, byrow = TRUE)

vars = seq(1, 43, length = 8)
vars

y = - 1.2 + rowSums(X[, vars]) + rnorm(nrow(X), 1)

fn = function(x) {
  mod = lm(y ~ X[, x == 1])
  bic = BIC(mod)
  return(bic)
}

MU = 100L # Size of the population
LAMBDA = 50L # Number of offspring per iteration

MAX.ITER = 100L # Number of iterations

# Step 1: Initialize & rate population
population = generate_binary_population(MU, 50)
fitness = evaluate_population(population, fn)

for (i in seq_len(MAX.ITER)) {
  # Step 2: variation
  offspring = generate_offspring(population, fitness, LAMBDA, p_mut = 0.3, p_cross = 0.5)
  fitness.o = evaluate_population(offspring, fn)
  
  # Step 3: survival selection
  sel = select_mu_plus_lambda(population, offspring, fitness, fitness.o, mu = MU)
  population = sel$population
  fitness = sel$fitness
}

MAX.ITER = 25L # Number of iterations

# Step 1: Initialize & rate population
population = generate_binary_population(MU, 50)
fitness = evaluate_population(population, fn)
bic = matrix(fitness, ncol = MU)
best = matrix(population[[which.min(fitness)]], ncol = 50)


for (i in seq_len(MAX.ITER)) {
  # Step 2: variation
  offspring = generate_offspring(population, fitness, LAMBDA, p_mut = 0.3, p_cross = 0.5)
  fitness.o = evaluate_population(offspring, fn)
  
  # Step 3: survival selection
  sel = select_mu_plus_lambda(population, offspring, fitness, fitness.o, mu = MU)
  population = sel$population
  fitness = sel$fitness
  best = rbind(best, population[[which.min(fitness)]])
  bic = rbind(bic, fitness)
  
  if (i %% 10 == 0) {
    print(paste("After", i, "iterations:"))
    print(which(population[[1]] == 1))
  }
  
  if (all((which(population[[1]] == 1)) %in% vars)) {
    print(paste("Included variables after", i, "iterations:"))
    print(which(population[[1]] == 1))
    break
  }
}

bic = as.data.frame(bic)
bic$iteration = 1:nrow(bic)

bic.melt = melt(bic, id.vars = "iteration")
bic.melt$iteration = as.factor(bic.melt$iteration)

bic$min = apply(bic[, 1:(ncol(bic) - 1)], 1, min)

p = ggplot(data = bic.melt, aes(x = iteration, y = value)) + geom_boxplot() + theme_bw()
p = p + xlab("Iteration") + ylab("BIC") + ggtitle("Fitness values (BIC) of the population per iteration")
p = p + geom_line(data = bic, aes(x = iteration, y = min, colour = "red")) + labs(colour = " ") + scale_colour_manual(values = "red", labels = "Best individual") + theme(legend.position = "bottom")
p

ggsave("../figure/var-selection1.png", p)


best = as.data.frame(best)
names(best) = as.character(1:50)
best$iteration = (1:nrow(best)) - 1

melted = melt(best, id.vars = "iteration")
melted$value = as.factor(melted$value)
p = ggplot() + geom_point(data = melted, aes(x = variable, y = iteration), size = 7, alpha = 0.1, colour = "grey") + theme_bw()
p = p + geom_point(data = melted[melted$value == 1, ], aes(x = variable, y = iteration), colour = "green", size = 3) + ylab("Iteration")
p = p + ggtitle("Included variables (green) per iteration (y-axis)")
p


ggsave("../figure/var-selection2.png", p)
