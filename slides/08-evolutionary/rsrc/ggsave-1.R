
library(knitr)
library(ecr)
library(ggplot2)
library(smoof)
library(latex2exp)
library(reshape2)
library(mlr)
library(gridExtra)



fn = makeAckleyFunction(1L)

pl = autoplot(fn, show.optimum = F, length.out = 1000L)
pl = pl + theme_bw()

ggsave("figure_man/1dim-ackley-func.png", pl, width = 5, height = 4)


# initialize evolutionary algorithm
MU = 10L; LAMBDA = 5L; MAX.ITER = 50L
lower = - 30
upper = 30
control = initECRControl(fn)
control = registerECROperator(control, "mutate", mutGauss, sdev = 2, lower = lower, upper = upper)
control = registerECROperator(control, "selectForSurvival", selGreedy)


set.seed(1212)
population = genReal(MU, getNumberOfParameters(fn), lower, upper)
fitness = evaluateFitness(control, population)
pl = autoplot(fn, show.optimum = F, length.out = 1000L)
df = data.frame(x = unlist(population), y = as.numeric(fitness))
pl = pl + geom_point(data = df, mapping = aes(x = x, y = y), size = 3) + theme_bw()
pl

ggsave("figure_man/1dim-ackley-func-2.png", pl, width = 5, height = 4)



# neutral Selection von lambda Eltern
set.seed(1234)
idx = sample(1:MU, LAMBDA)
pl = pl + geom_point(data = df[idx, ], mapping = aes(x = x, y = y), colour = "red", size = 3)
pl = pl + ggtitle("Neutral Selection")

ggsave("figure_man/1dim-ackley-func-neutral-selec.png", pl, width = 5, height = 4)




offspring = mutate(control, population[idx], p.mut = 1)
fitness.o = evaluateFitness(control, offspring)
df.o = data.frame(x = unlist(offspring), y = as.numeric(fitness.o))

pl = pl + geom_point(data = df.o, aes(x = x, y = y), color = "red", size = 3)
pl = pl + geom_point(data = df[idx,], aes(x = x, y = y), color = "red", size = 3)
pl2 = pl + geom_segment(data = data.frame(x = df[idx, ]$x, y = df[idx, ]$y, xend = df.o$x, yend = df.o$y), aes(x = x, y = y, xend = xend, yend = yend), colour = "red", linetype = 1, arrow = arrow(length = unit(0.01, "npc"))
)
pl2 = pl2 + ggtitle("Gaussian Mutation")


ggsave("figure_man/1dim-ackley-func-gaussian-mutation.png", pl2, width = 5, height = 4)



sel = replaceMuPlusLambda(control, population, offspring, fitness, fitness.o)
population = sel$population
fitness = sel$fitness
df = data.frame(x = unlist(population), y = as.numeric(fitness))

pl = pl + geom_point(data = df, aes(x = x, y = y), color = "green", fill = "green", size = 3)
pl = pl + geom_hline(yintercept = max(df$y), lty = 2)
pl = pl + ggtitle(expression(paste("(",mu, "+" ,lambda,")-selection")))

ggsave("figure_man/1dim-ackley-func-selection.png", pl, width = 5, height = 4)


best_individual = c()
pops = data.frame()

# After 200 iterations 
for (i in seq_len(MAX.ITER)) {
    # sample lambda individuals at random
    idx = sample(1:MU, LAMBDA)
    # generate offspring by mutation and evaluate their fitness
    offspring = mutate(control, population[idx], p.mut = 1)
    fitness.o = evaluateFitness(control, offspring)
    # now select the best out of the union of population and offspring
    sel = replaceMuPlusLambda(control, population, offspring, fitness, fitness.o)
    population = sel$population

    fitness = sel$fitness
    pops = rbind(pops, cbind(x = unlist(population), y = as.vector(fitness), iteration = i))
    best_individual = c(best_individual, min(fitness))
}

# Over time
df = data.frame(x = 1:MAX.ITER, y = best_individual)
p1 = ggplot(data = df, aes(x = x, y = y)) + geom_line() + theme_bw()
p1 = p1 + xlab("Iteration") + ylab("Fitness (best individual)")

pl = autoplot(fn, show.optimum = F, length.out = 1000L)
p2 = pl + theme_bw()
p2 = p2 + geom_point(data = pops, aes(x = x, y = y, colour = iteration ))

p = grid.arrange(p1, p2, nrow = 1)
ggsave("figure_man/1dim-ackley-func-final.png", p, height = 4, width = 8)