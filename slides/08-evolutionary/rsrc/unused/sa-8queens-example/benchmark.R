# setwd("~/work/cim/cim1/2017/11-Optimierung")
# source("sa-8queens-example/board.R")
# source("sa-8queens-example/objective.R")
# source("sa-8queens-example/sa.R")
# source("sa-8queens-example/sa_run.R")



library(plyr)


n = 8L
nrep = 3L
maxit = 10000L


methods = c("random", "greedy", "sa")
# methods = c("greedy", "sa")
# methods = c("sa")
ops = c("1step",  "geom")
reps = 1:nrep

t.starts =  c(random = 100000, greedy = 1e-16, sa = 0.001)
t.factors = c(random = 1,      greedy = 1,     sa = 0.999)

grid = expand.grid(method = methods, op = ops, rep = reps) 

for (i in seq_row(grid)) {
  print(i)
  g = grid[i, ]
  m = as.character(g$method)
  b = createRandomBoard(n = n)
  z = sa(b, maxit = maxit, op = g$op , t.start = t.starts[m],
    t.factor = t.factors[m])
  grid[i, "y"] = min(z$penalties)
  grid[i, "ert"] = length(z$penalties)
}

grid2 = ddply(grid, c("method", "op"), summarize, ert = median(ert))

