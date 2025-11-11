# ------------------------------------------------------------------------------
# mathematical concepts

# FIG: plot negative gradients on contour plot of branin function
# ------------------------------------------------------------------------------

library(DiceKriging)
library(rootSolve)

# DATA -------------------------------------------------------------------------

# plot: https://rdrr.io/cran/DiceKriging/man/branin.html
n.grid <- 20
x.grid <- y.grid <- seq(0,1,length=n.grid)
design.grid <- expand.grid(x.grid, y.grid)
response.grid <- apply(design.grid, 1, branin)
z.grid <- matrix(response.grid, n.grid, n.grid)

p1 <- c(0.35, 0.4)
p2 <- c(0.1, 0.4)
p3 <- c(0.5, 0.7)
p4 <- c(0.05, 0.05)
p5 <- c(0.8, 0.8)
p6 <- c(0.55, 0.18)
p7 <- c(0.2, 0.8)

p1g <- -gradient(branin, p1) / 2000
p2g <- -gradient(branin, p2) / 2000
p3g <- -gradient(branin, p3) / 2000
p4g <- -gradient(branin, p4) / 2000
p5g <- -gradient(branin, p5) / 2000
p6g <- -gradient(branin, p6) / 2000
p7g <- -gradient(branin, p7) / 2000

# PLOT -------------------------------------------------------------------------

png(filename = "../figure_man/branin.png", width = 800, height = 600)

plot.new()
contour(x.grid,y.grid,z.grid,40)
points(rbind(t(p1), t(p2), t(p3), t(p4), t(p5), t(p6), t(p7)), pch=19, col="red")
arrows(x0=p1[1], y0=p1[2], x1=(p1[1]+p1g[1,1]), y1=(p1[2]+p1g[1,2]), angle=10, length=0.15)
arrows(x0=p2[1], y0=p2[2], x1=(p2[1]+p2g[1,1]), y1=(p2[2]+p2g[1,2]), angle=10, length=0.15)
arrows(x0=p3[1], y0=p3[2], x1=(p3[1]+p3g[1,1]), y1=(p3[2]+p3g[1,2]), angle=10, length=0.15)
arrows(x0=p4[1], y0=p4[2], x1=(p4[1]+p4g[1,1]), y1=(p4[2]+p4g[1,2]), angle=10, length=0.15)
arrows(x0=p5[1], y0=p5[2], x1=(p5[1]+p5g[1,1]), y1=(p5[2]+p5g[1,2]), angle=10, length=0.15)
arrows(x0=p6[1], y0=p6[2], x1=(p6[1]+p6g[1,1]), y1=(p6[2]+p6g[1,2]), angle=10, length=0.15)
arrows(x0=p7[1], y0=p7[2], x1=(p7[1]+p7g[1,1]), y1=(p7[2]+p7g[1,2]), angle=10, length=0.15)
# /100, da sonst Pfeil bis Punkt p1g reicht, nicht nur in dessen Richtung
title("Mod. Branin function with gradients")

dev.off()

