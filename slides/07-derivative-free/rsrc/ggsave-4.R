
library(knitr)
library(ecr)
library(ggplot2)
library(smoof)
library(latex2exp)
library(ggforce)
library(reshape2)
library(mlr)
library(ecr)
library(data.table)


fn = makeAckleyFunction(1L)
l = - 30
u = 30
MU = 30L # Population size
LAMBDA = 5L # Number of offspring per iteration

control = initECRControl(fn)
control = registerECROperator(control, "mutate", mutGauss,
                           sdev = 2, lower = l, upper = u)
control = registerECROperator(control, "selectForSurvival",
                          selGreedy)

population = genReal(MU, getNumberOfParameters(fn), l, u)
fitness = evaluateFitness(control, population)
df = data.frame(x = unlist(population), y = as.numeric(fitness), iteration = 0)

MAX.ITER = 5L # Number of iterations

for (i in seq_len(MAX.ITER)) {
  # Step 2: neutral selection of parents
  idx = sample(1:MU, LAMBDA)
  
  # Step 3: variation
  offspring = mutate(control, population[idx], p.mut = 1)
  fitness.o = evaluateFitness(control, offspring)
  
  # Step 4: survival selection
  sel = replaceMuPlusLambda(control, population, offspring,
                            fitness, fitness.o)
  population = sel$population
  fitness = sel$fitness
}

MAX.ITER = 5L # Number of iterations

for (i in seq_len(MAX.ITER)) {
  # sample lambda individuals at random
  idx = sample(1:MU, LAMBDA)
  # generate offspring by mutation and evaluate their fitness
  offspring = mutate(control, population[idx], p.mut = 1)
  fitness.o = evaluateFitness(control, offspring)
  # now select the best out of the union of population and offspring
  sel = replaceMuPlusLambda(control, population, offspring,
                            fitness, fitness.o)
  population = sel$population
  fitness = sel$fitness
  
  df = rbind(df, data.frame(x = unlist(population), y = as.numeric(fitness), iteration = i))
}

library(data.table)
df$iteration = as.factor(df$iteration)
cummin = setDT(df)[, y == min(y), by = iteration]
dfcummin = df[cummin$V1, ]

p = ggplot(data = df, aes(x = iteration, y = y)) + geom_boxplot() + theme_bw() + ylim(c(0 ,25))
p = p + xlab("Iteration") + ylab("f(y)") + ggtitle("Fitness values of the population per iteration")
p = p + geom_line(data = dfcummin, aes(x = as.numeric(iteration), y = y, colour = "red")) + labs(colour = " ") + scale_colour_manual(values = "red", labels = "Best individual") + theme(legend.position = "bottom")


ggsave("../13-optim-ableitunsfrei/figure_man/example.png", p)