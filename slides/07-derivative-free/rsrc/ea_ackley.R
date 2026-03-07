# ------------------------------------------------------------------------------
# derivative free

# FIG: applies an evolutionary algorithm to optimize the Ackley function
# ------------------------------------------------------------------------------

library(knitr)
library(ecr)
library(ggplot2)
library(smoof)
library(latex2exp)
library(ggforce)
library(reshape2)
library(mlr)

set.seed(1234)

# ------------------------------------------------------------------------------

evaluate_population = function(population, objective) {
  vapply(population, objective, numeric(1L))
}

mutate_gaussian = function(population, sdev, lower, upper) {
  lapply(population, function(individual) {
    mutated = individual + stats::rnorm(length(individual), sd = sdev)
    pmin(upper, pmax(lower, mutated))
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

fn = makeAckleyFunction(1L)

pl = autoplot(fn, show.optimum = F, length.out = 1000L)
pl = pl + theme_bw()

ggsave("../figure/ea_ackley-1dim-ackley-func.png", pl)


# initialize evolutionary algorithm
MU = 20L; LAMBDA = 5L; MAX.ITER = 200L
lower = - 30
upper = 30
population = replicate(
  MU,
  runif(getNumberOfParameters(fn), min = lower, max = upper),
  simplify = FALSE
)
fitness = evaluate_population(population, fn)
pl = autoplot(fn, show.optimum = F, length.out = 1000L)
df = data.frame(x = unlist(population), y = as.numeric(fitness))
pl = pl + geom_point(data = df, mapping = aes(x = x, y = y), size = 3) + theme_bw()
pl

ggsave("../figure/ea_ackley-1dim-ackley-func-2.png", pl)



# neutral Selection von lambda Eltern
set.seed(1234)
idx = sample(1:MU, LAMBDA)
pl = pl + geom_point(data = df[idx, ], mapping = aes(x = x, y = y), colour = "red", size = 3)
pl = pl + ggtitle("Neutral Selection")

ggsave("../figure/ea_ackley-neutral-selec.png", pl)




offspring = mutate_gaussian(population[idx], sdev = 2, lower = lower, upper = upper)
fitness.o = evaluate_population(offspring, fn)
df.o = data.frame(x = unlist(offspring), y = as.numeric(fitness.o))

pl = pl + geom_point(data = df.o, aes(x = x, y = y), color = "red", size = 3)
pl = pl + geom_point(data = df[idx,], aes(x = x, y = y), color = "red", size = 3)
pl2 = pl + geom_segment(data = data.frame(x = df[idx, ]$x, y = df[idx, ]$y, xend = df.o$x, yend = df.o$y), aes(x = x, y = y, xend = xend, yend = yend), colour = "red", linetype = 1, arrow = arrow(length = unit(0.01, "npc"))
)
pl2 = pl2 + ggtitle("Gaussian Mutation")


ggsave("../figure/ea_ackley-gaussian-mutation.png", pl2)


sel = select_mu_plus_lambda(population, offspring, fitness, fitness.o, mu = MU)
population = sel$population
fitness = sel$fitness
df = data.frame(x = unlist(population), y = as.numeric(fitness))

pl = pl + geom_point(data = df, aes(x = x, y = y), color = "green", fill = "green", size = 3)
pl = pl + geom_hline(yintercept = max(df$y), lty = 2)
pl = pl + ggtitle(expression(paste("(",mu, "+" ,lambda,")-selection")))

ggsave("../figure/ea_ackley-selection.png", pl)
