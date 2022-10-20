# Optimization WiSe 22/23
# Chapter 1.1 Differentiation
# Plot negative gradient

library(DiceKriging)

# plot: https://rdrr.io/cran/DiceKriging/man/branin.html
n.grid <- 20
x.grid <- y.grid <- seq(0,1,length=n.grid)
design.grid <- expand.grid(x.grid, y.grid)
response.grid <- apply(design.grid, 1, branin)
z.grid <- matrix(response.grid, n.grid, n.grid)

# Mod. branin function: https://www.sfu.ca/~ssurjano/Code/braninr.html
branin <- function(xx, a=1, b=5.1/(4*pi^2), c=5/pi, r=6, s=10, t=1/(8*pi)) {
  x1 <- xx[1]
  x2 <- xx[2]
  term1 <- a * (x2 - b*x1^2 + c*x1 - r)^2
  term2 <- s*(1-t)*cos(x1)
  y <- term1 + term2 + s
  return(y)
}

library(rootSolve)
p1 <- c(0.35, 0.4)
p2 <- c(0.1, 0.4)
p3 <- c(0.5, 0.7)
p4 <- c(0.05, 0.05)
p5 <- c(0.8, 0.8)
p1g <- -gradient(branin, p1) # 18.48023, 10.11757
p2g <- -gradient(branin, p2) # 18.00026, 10.88427
p3g <- -gradient(branin, p3) # 17.8716 , 9.073042
p4g <- -gradient(branin, p4) # 19.01539, 11.74149
p5g <- -gradient(branin, p5) # 17.99311, 8.018877

plot.new()
contour(x.grid,y.grid,z.grid,40)
points(rbind(t(p1), t(p2), t(p3), t(p4), t(p5)), pch=19, col="red")
arrows(x0=p1[1], y0=p1[2], x1=(p1[1]+p1g[1,1]/100), y1=(p1[2]+p1g[1,2]/100), angle=10, length=0.15)
arrows(x0=p2[1], y0=p2[2], x1=(p2[1]+p2g[1,1]/100), y1=(p2[2]+p2g[1,2]/100), angle=10, length=0.15)
arrows(x0=p3[1], y0=p3[2], x1=(p3[1]+p3g[1,1]/100), y1=(p3[2]+p3g[1,2]/100), angle=10, length=0.15)
arrows(x0=p4[1], y0=p4[2], x1=(p4[1]+p4g[1,1]/100), y1=(p4[2]+p4g[1,2]/100), angle=10, length=0.15)
arrows(x0=p5[1], y0=p5[2], x1=(p5[1]+p5g[1,1]/100), y1=(p5[2]+p5g[1,2]/100), angle=10, length=0.15)
# /100, da sonst Pfeil bis Punkt p1g reicht, nicht nur in dessen Richtung
title("Mod. Branin function with gradients")




