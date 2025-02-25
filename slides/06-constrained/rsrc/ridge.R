# ------------------------------------------------------------------------------
# constrained

# FIG: plot ridge regression
# ------------------------------------------------------------------------------

library(rgl)
library(colorspace)

# ------------------------------------------------------------------------------

plotContour = function(x, y, z, constraint = F, opt) {
  col = terrain_hcl(nrow(z) * ncol(z))
  image(x, y, z, col = col, xlab = "theta1", ylab = "theta2")
  contour(x, y, z, add = TRUE, levels = seq(10000, 80000, by = 10000))
  points(x = opt$par[1], y = opt$par[2], pch = 18, cex = 1.5, col = "red")
  
  if (constraint == T) {
    n = length(h.x1)
    for (j in seq_along(h.x1)) {
      if (j > 1) {
        lines(c(h.x1[j], h.x1[j-1]), c(h.x2[j], h.x2[j-1]), col = "red")
      } else {
        lines(c(h.x1[j], h.x1[n]), c(h.x2[j], h.x2[n]), col = "red")
      }
    }
  }
}

data(cars)
n = dim(cars)[1]

# squared loss
loss = function(x1, x2) {
  
  res = 0
  
  for (i in 1:n) {
    res = res + ((x1 + x2 * cars[i, ]$speed) - cars[i, ]$dist)^2
  }
  res = res / n
  return(res)
}

# optimum
fun = function(x) loss(x[1], x[2])
opt1 = optim(par = c(-10, 5), fn = fun)

# set grid
x1 = seq(- 18, 18, by = 0.5)
x2 = seq(- 18, 18, by = 0.5)

# first version: instable version 
z1 = outer(x1, x2, loss)

png('../figure_man/ridge_original.png', width = 480, height = 300)
plotContour(x1, x2, z1, opt = opt1)
persp3d(x1, x2, z1, alpha = 0.8, color = "grey", xlab = expression(theta[1]), ylab = expression(theta[2]), zlab = "Loss")
points3d(opt1$par[1], opt1$par[2], opt1$value, col = "red", size = 5)
dev.off()

# Ridge Regression - Penalty term
loss.penalty = function(x1, x2, lambda = 20) {
  
  res = loss(x1, x2)
  
  res = res + lambda * (x1^2 + x2^2)
  
  return(res)
}

opt2 = optim(par = c(-10, 5), fn = function(x) loss.penalty(x[1], x[2]))

z2 = outer(x1, x2, loss.penalty)

png('../figure_man/ridge_formulation1.png', width = 480, height = 300)
persp3d(x1, x2, z2, col = "grey", alpha = 0.8, xlab = expression(theta[1]), ylab = expression(theta[2]), zlab = "Loss")
points3d(opt2$par[1], opt2$par[2], opt2$value, col = "red", size = 5)
plotContour(x1, x2, z2, opt = opt2)
dev.off()

# constrained ridge regression
t = sum(opt2$par^2)


feasible = function(x1, x2) {
  res = ifelse(x1^2 + x2^2 <= t, loss(x1, x2), NA)
  return(res)
}

h.x1 = seq(-1, 1, length.out = 100)
h.x2 = c(- sqrt(1 - h.x1[1:50]^2), - sqrt(1 - h.x1[51:100]^2), sqrt(1 - h.x1[100:51]^2), sqrt(1 - h.x1[50:1]^2))
h.x1 = c(h.x1[1:50], h.x1[51:100], h.x1[100:51], h.x1[50:1])

h.x1 = h.x1 * sqrt(t)
h.x2 = h.x2 * sqrt(t)


x1.feasible = seq(- 8, 8, by = 0.05)
x2.feasible = seq(- 8, 8,  by = 0.05)

z.feasible = outer(x1.feasible, x2.feasible, feasible)

png('../figure_man/ridge_formulation2.png', width = 480, height = 300)
persp3d(x1, x2, z1, col = "grey", alpha = 0.8, xlab = expression(theta[1]), ylab = expression(theta[2]), zlab = "Loss")
points3d(opt2$par[1], opt2$par[2], opt2$value, col = "red", size = 5)
persp3d(x1.feasible, x2.feasible, z.feasible, col = "green", add = T, alpha = 0.5)

plotContour(x1, x2, z1, constraint = T, opt = opt2)
dev.off()
