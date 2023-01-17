
library(knitr)
library(ecr)
library(ggplot2)
library(smoof)
library(latex2exp)
library(ggforce)
library(reshape2)
library(mlr)



fn = makeAckleyFunction(1L)

pl = autoplot(fn, show.optimum = F, length.out = 1000L)
pl = pl + theme_bw()

ggsave("../13-optim-ableitunsfrei/figure_man/1dim-ackley-func.png", pl)


# initialize evolutionary algorithm
MU = 10L; LAMBDA = 5L; MAX.ITER = 200L
lower = - 30
upper = 30
control = initECRControl(fn)
control = registerECROperator(control, "mutate", mutGauss, sdev = 2, lower = lower, upper = upper)
control = registerECROperator(control, "selectForSurvival", selGreedy)


set.seed(1234)
population = genReal(MU, getNumberOfParameters(fn), lower, upper)
fitness = evaluateFitness(control, population)
pl = autoplot(fn, show.optimum = F, length.out = 1000L)
df = data.frame(x = unlist(population), y = as.numeric(fitness))
pl = pl + geom_point(data = df, mapping = aes(x = x, y = y), size = 3) + theme_bw()
pl

ggsave("../13-optim-ableitunsfrei/figure_man/1dim-ackley-func-2.png", pl)



# neutral Selection von lambda Eltern
set.seed(1234)
idx = sample(1:MU, LAMBDA)
pl = pl + geom_point(data = df[idx, ], mapping = aes(x = x, y = y), colour = "red", size = 3)
pl = pl + ggtitle("Neutral Selection")

ggsave("../13-optim-ableitunsfrei/figure_man/neutral-selec.png", pl)




offspring = mutate(control, population[idx], p.mut = 1)
fitness.o = evaluateFitness(control, offspring)
df.o = data.frame(x = unlist(offspring), y = as.numeric(fitness.o))

pl = pl + geom_point(data = df.o, aes(x = x, y = y), color = "red", size = 3)
pl = pl + geom_point(data = df[idx,], aes(x = x, y = y), color = "red", size = 3)
pl2 = pl + geom_segment(data = data.frame(x = df[idx, ]$x, y = df[idx, ]$y, xend = df.o$x, yend = df.o$y), aes(x = x, y = y, xend = xend, yend = yend), colour = "red", linetype = 1, arrow = arrow(length = unit(0.01, "npc"))
)
pl2 = pl2 + ggtitle("Gaussian Mutation")


ggsave("../13-optim-ableitunsfrei/figure_man/gaussian-mutation.png", pl2)





sel = replaceMuPlusLambda(control, population, offspring, fitness, fitness.o)
population = sel$population
fitness = sel$fitness
df = data.frame(x = unlist(population), y = as.numeric(fitness))

pl = pl + geom_point(data = df, aes(x = x, y = y), color = "green", fill = "green", size = 3)
pl = pl + geom_hline(yintercept = max(df$y), lty = 2)
pl = pl + ggtitle(expression(paste("(",mu, "+" ,lambda,")-selection")))

ggsave("../13-optim-ableitunsfrei/figure_man/selection.png", pl)








