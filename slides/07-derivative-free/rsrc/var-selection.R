# ------------------------------------------------------------------------------
# derivative free

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

control = initECRControl(fn, n.objectives = 1)
control = registerECROperator(control, "mutate", mutBitflip,
                              p = 0.3)
control = registerECROperator(control, "selectForMating",
                              selGreedy, n.select = LAMBDA)
control = registerECROperator(control, "recombine",
                              recUnifCrossover, p = 0.5)
control = registerECROperator(control, "selectForSurvival",
                              selGreedy)


MAX.ITER = 100L # Number of iterations

# Step 1: Initialize & rate population
population = genBin(MU, 50)
fitness = evaluateFitness(control, population)

for (i in seq_len(MAX.ITER)) {
  # Step 2: variation
  offspring = generateOffspring(control, population, fitness,
                                LAMBDA)
  fitness.o = evaluateFitness(control, offspring)
  
  # Step 3: survival selection
  sel = replaceMuPlusLambda(control, population, offspring,
                            fitness, fitness.o)
  population = sel$population
  fitness = sel$fitness
}

MAX.ITER = 25L # Number of iterations

# Step 1: Initialize & rate population
population = genBin(MU, 50)
fitness = evaluateFitness(control, population)
bic = matrix(fitness[1, ], ncol = MU)
best = matrix(population[[which.min(fitness)]], ncol = 50)


for (i in seq_len(MAX.ITER)) {
  # Step 2: variation
  offspring = generateOffspring(control, population, fitness,
                                LAMBDA)
  fitness.o = evaluateFitness(control, offspring)
  
  # Step 3: survival selection
  sel = replaceMuPlusLambda(control, population, offspring,
                            fitness, fitness.o)
  population = sel$population
  fitness = sel$fitness
  best = rbind(best, population[[which.min(fitness)]])
  bic = rbind(bic, fitness[1, ])
  
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

ggsave("../figure_man/var-selection1.png", p)


best = as.data.frame(best)
names(best) = as.character(1:50)
best$iteration = (1:nrow(best)) - 1

melted = melt(best, id.vars = "iteration")
melted$value = as.factor(melted$value)
p = ggplot() + geom_point(data = melted, aes(x = variable, y = iteration), size = 7, alpha = 0.1, colour = "grey") + theme_bw()
p = p + geom_point(data = melted[melted$value == 1, ], aes(x = variable, y = iteration), colour = "green", size = 3) + ylab("Iteration")
p = p + ggtitle("Included variables (green) per iteration (y-axis)")
p


ggsave("../figure_man/var-selection2.png", p)
