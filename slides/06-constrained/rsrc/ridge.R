# Ridge Regression plots 

loss = function(x1, x2) x1^2 + x2^2

# Ridge Regression - Penalty term
loss.penalty = function(x1, x2, lambda = 20) {
  
  res = loss(x1, x2) 
  
  res = res + lambda * (x1^2 + x2^2)

  return(res)
}

opt2 = optim(par = c(-10, 5), fn = function(x) loss.penalty(x[1], x[2]))

z2 = outer(x1, x2, loss.penalty)

pmat = persp(x1, x2, z2, col = col, phi = 10, theta = 60, xlab = "θ1", ylab = "θ2", zlab = "\n\nLoss", ticktype="detailed")

points(trans3d(opt2$par[1],opt2$par[2], opt2$value, pmat), col = "red", cex = 1.5, bg = "red", pch = 19)


# Ridge Regression - Constrained optimization problem

t = sum(opt2$par^2)

pmat = persp(x1, x2, z1, col = col, phi = 10, theta = 60, xlab = "θ1", ylab = "θ2", zlab = "\n\nLoss", ticktype="detailed")


feasible = function(x1, x2) {
  res = ifelse(x1^2 + x2^2 <= t, loss(x1, x2), NA)
  return(res)
}
z.feasible = outer(x1, x2, feasible)


persp3d(x1, x2, z1, col = col, alpha=0.8)


persp3d(x1, x2, z.feasible, col = "red", add = T)

points(trans3d(opt$par[1],opt$par[2], opt$value, pmat), col = "red", cex = 1.5, bg = "red", pch = 19)
