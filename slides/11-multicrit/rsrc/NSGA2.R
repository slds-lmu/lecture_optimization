# ------------------------------------------------------------------------------
# multicrit

# FIG: Vizualization of NSGAII
#    (1) Steps: evolution of the population at iterations 1, 3, and 10.
#    (2) NDS: Displays the population split into non-dominated fronts 
#             using different colors, with dashed lines connecting points 
#             within each front for visual separation.
#    (3) CS1: Points in Pareto front with the smallest and largest crowding dist.
#    (4) CS2: Demonstrates regions of the Pareto front with colored boxes 
#             highlighting specific crowding distances, showing how diversity 
#             in the solution space is maintained.
# ------------------------------------------------------------------------------

library(smoof)
library(ggplot2)
library(ecr)
library(mco)
library(gridExtra)
library(grid)
set.seed(666)

# ------------------------------------------------------------------------------

# viz in objective space 2 crit
plotObjectiveSpace2Crit = function(smoof.fun) {
  des = generateRandomDesign(n = 10000L, par.set = getParamSet(smoof.fun))
  des.eval = apply(des, 1, smoof.fun)
  des.eval.df = data.frame(t(des.eval))
  names(des.eval.df) = c("x1", "x2")
  
  p = ggplot() + geom_point(data = des.eval.df, aes(x = x1, y = x2), size = 0.7, color = "grey", alpha = 0.2)
  
  p = p + theme_bw()
  return(p)
}

# Example function
ps = makeNumericParamSet("x", lower = c(0.1, 0), upper = c(1, 5))

fn = makeMultiObjectiveFunction(name = "Min-Ex",
  fn = function(x) c(x[1], (1 + x[2]) / (x[1] * 60)),
  par.set = ps 
)

lower = getLower(ps)
upper = getUpper(ps)

POP_SIZE = 24L

run_nsga2_snapshot = function(generations) {
  set.seed(666)
  mco::nsga2(
    fn,
    idim = 2L,
    odim = 2L,
    lower.bounds = lower,
    upper.bounds = upper,
    popsize = POP_SIZE,
    generations = generations,
    cprob = 0.7,
    cdist = 15,
    mprob = 0.2,
    mdist = 25
  )
}

p = plotObjectiveSpace2Crit(fn)
snapshot_iters = c(1L, 3L, 5L, 10L)
populations = lapply(snapshot_iters, run_nsga2_snapshot)
names(populations) = as.character(snapshot_iters)

for (i in snapshot_iters) {
  popdf = data.frame(populations[[as.character(i)]]$value)
  pl = p + geom_point(data = popdf, aes(x = X1, y = X2), colour = "blue")
  pl = pl + ggtitle(paste("Iteration", i))
  assign(paste("p", i, sep = ""), value = pl)
}

g = arrangeGrob(p1, p3, p10, ncol = 3)
if (interactive()) grid::grid.draw(g)
ggsave(g, file = "../figure/NSGA2_steps.png", width = 8, height = 4)


# non-dominated sorting
pop = t(populations[["1"]]$value)
sorted = doNondominatedSorting(pop)
rank_max = max(sorted$ranks)
ranks = 1:rank_max

popdf = data.frame(t(pop))
popdf$Front = factor(sorted$ranks, ordered = TRUE, levels = ranks)


pl = p + geom_point(data = popdf[popdf$Front %in% ranks, ], aes(x = X1, y = X2, colour = Front)) 
pl = pl + geom_line(data = popdf[popdf$Front %in% ranks, ], aes(x = X1, y = X2, colour = Front), lty = 2)
if (interactive()) print(pl)
ggsave(pl, file = "../figure/NSGA2_NDS.png", width = 4, height = 3)


# Crowd Sort - Example 1
front_sizes = table(sorted$ranks)
front_candidates = as.integer(names(front_sizes[front_sizes >= 3]))
front_plot = if (3L %in% front_candidates) 3L else front_candidates[1L]
if (is.na(front_plot)) {
  front_plot = 1L
}
F3 = popdf[which(popdf$Front == front_plot), ]
row.names(F3) <- seq_len(nrow(F3))
F3 = F3[stats::complete.cases(F3[, c("X1", "X2")]), ]
cd = computeCrowdingDistance(t(as.matrix(F3[, c("X1", "X2")])))

pl = p + geom_point(data = F3, aes(x = X1, y = X2), alpha = 0.3)
pl = pl + geom_line(data = F3, aes(x = X1, y = X2), lty = 2, alpha = 0.3)

  
pl1 = pl + geom_point(data = F3[order(cd, decreasing = FALSE)[1:5], ], aes(x = X1, y = X2), size = 3, shape = 17) 
pl1 = pl1 + theme(legend.position = "none")
pl2 = pl + geom_point(data = F3[order(cd, decreasing = TRUE)[1:5], ], aes(x = X1, y = X2), shape = 17, size = 3) 
pl2 = pl2 + theme(legend.position = "none")
pl2 = pl2 

g = arrangeGrob(pl1, pl2, ncol = 2)
if (interactive()) grid::grid.draw(g)

ggsave(g, file = "../figure/NSGA2_CS1.png", width = 6, height = 3)


cdo = order(cd, decreasing = TRUE)[c(5, length(cd)-1)]
F3.oX1 = F3[order(F3$X1), c("X1", "X2")]

cuboids = F3[cdo, c("X1", "X2")]
idx = which(F3.oX1$X1 %in% cuboids$X1)
cuboids = cbind(cuboids, F3.oX1[idx + 1, ])
cuboids = cbind(cuboids, F3.oX1[idx - 1, ])
names(cuboids) = c("x", "y", "xmin", "ymin", "xmax", "ymax")
cuboids$point = c("i", "j")

F3 = F3[!is.na(F3$X1), ]
pl1 = p + geom_point(data = F3, aes(x = X1, y = X2)) 
pl1 = pl1 + theme(legend.position = "none")
pl1 = pl1 + geom_line(data = F3, aes(x = X1, y = X2), lty = 2)
pl1 = pl1 + geom_rect(data = cuboids, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, colour = point, fill = point), alpha = 0.2)
pl1 = pl1 + geom_point(data = cuboids, aes(x = x, y = y, colour = point, fill = point), size = 3)
if (interactive()) print(pl1)
ggsave(pl1, file = "../figure/NSGA2_CS2.png", width = 3, height = 3)
