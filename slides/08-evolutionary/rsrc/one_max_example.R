# ------------------------------------------------------------------------------
# evolutionary algorithms

# FIG: plot example of one-max
# ------------------------------------------------------------------------------

set.seed(1L)

library(ecr)
library(ggplot2)
library(smoof)
library(reshape2)

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

select_mu_plus_lambda = function(population, offspring, fitness, fitness_offspring, mu) {
  combined_population = c(population, offspring)
  combined_fitness = c(fitness, fitness_offspring)
  keep = order(combined_fitness, decreasing = TRUE)[seq_len(mu)]

  list(
    population = combined_population[keep],
    fitness = combined_fitness[keep]
  )
}

d = 15
ps = makeParamSet(makeDiscreteVectorParam("x", len = d, values = c(0, 1)))
fn = makeSingleObjectiveFunction(
  name = "Onemax",
  fn = function(x) sum(x),
  par.set = ps,
  minimize = FALSE
)

# evolutionary algorithm by hand
MU = 10L; LAMBDA = 5L; MAX.ITER = 150L
lower = getLowerBoxConstraints(fn)
upper = getUpperBoxConstraints(fn)

population = generate_binary_population(MU, getNumberOfParameters(fn))
fitness = evaluate_population(population, fn)
best = matrix(population[[which.max(fitness)]], ncol=d)
best

for (i in seq_len(MAX.ITER)) {
  # sample lambda individuals at random
  idx = sample(1:MU, LAMBDA)
  # generate offspring by mutation and evaluate their fitness
  offspring = mutate_bitflip(population[idx], p = 0.05)
  fitness.o = evaluate_population(offspring, fn)
  # now select the best out of the union of population and offspring
  sel = select_mu_plus_lambda(population, offspring, fitness, fitness.o, mu = MU)
  population = sel$population
  fitness = sel$fitness
  
  # add data to data_to_plot
  best = rbind(best, population[[which.max(fitness)]])
  
  
  if (all(population[[which.max(fitness)]] == 1)) {
    print(paste("STOPPING CRITERIA REACHED AFTER ",i, " ITERATIONS"))
    print(paste("BEST SOLUTION: ",max(fitness)," AT ID ",which.max(fitness)))
    print(population[[which.max(fitness)]])
    break
  }
  if (i == MAX.ITER) {
    print(paste("MAX.ITER OF ",MAX.ITER," REACHED. BEST SOLUTION: ",max(fitness)))
    print(population[[which.max(fitness)]])
  }
}


print(max(fitness))
print(which.max(fitness))
print(population[[which.max(fitness)]])

best_df = as.data.frame(best)
best_df$sum = rowSums(best_df)
best_df$iteration = (1:nrow(best_df)) - 1

melted = melt(best_df, id.vars="iteration")
melted$value = as.factor(melted$value)
update_geom_defaults("text", list(size = 3))

unique_sum <- best_df[match(unique(best_df$sum), best_df$sum),]

p = ggplot() + geom_point(data=melted[melted$value == 1, ], 
                          aes(x=variable,
                              y=iteration),
                          size=1, color="green")
p = p + ylab("Iteration") + xlab("Bits")
p = p + expand_limits(x=c(0, 17))
p = p + geom_text(data=unique_sum, aes(x=16, y=iteration, label=sum), show.legend = FALSE)
p = p + theme_bw()

p
ggsave("../figure/one_max_example.pdf", p, width = 7, height = 4)
