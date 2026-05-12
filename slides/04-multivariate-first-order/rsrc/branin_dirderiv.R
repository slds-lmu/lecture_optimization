# ------------------------------------------------------------------------------
# multivariate first order

# FIG: plot branin directional derivatives
# ------------------------------------------------------------------------------

library(DiceKriging)
library(rootSolve) #gradient()

# ------------------------------------------------------------------------------

n.grid <- 20
x.grid <- y.grid <- seq(0,1,length=n.grid)
design.grid <- expand.grid(x.grid, y.grid)
response.grid <- apply(design.grid, 1, branin)
z.grid <- matrix(response.grid, n.grid, n.grid)

# Mod. branin function: https://www.sfu.ca/~ssurjano/Code/braninr.html

p1 <- c(0.6, 0.6)
p1_neggrad <- -gradient(branin, p1)
p1_grad <- gradient(branin, p1)

p1_hleft <- gradient(branin, p1) * c(-1, 0)
p1_hright <- gradient(branin, p1) * c(1, 0)
p1_vdown <- gradient(branin, p1) * c(0, -1)
p1_vup <- gradient(branin, p1) * c(0, 1)
p1_contourleft <- gradient(branin, p1) * c(-1, 135/231)
p1_contourright <- gradient(branin, p1) * c(1, -135/231)
p1_9 <- cbind(gradient(branin, p1)[2], -gradient(branin, p1)[1])
p1_10 <- cbind(-gradient(branin, p1)[2], gradient(branin, p1)[1])
#p1_11 <- -1, 1
#p1_12 <- 1, -1

dirdf <- data.frame(
  rbind(p1_hleft,
        p1_hright,
        p1_vdown,
        p1_vup,
        p1_contourleft,
        p1_contourright))

graddf <- data.frame(
  rbind(p1_neggrad, 
        p1_grad)
)



# plot point, pos. & neg. gradients and different directional derivatives
# plot: https://rdrr.io/cran/DiceKriging/man/branin.html
png('../figure_man/branin_dirderiv.png', width = 480, height = 320)
plot.new()
contour(x.grid,y.grid,z.grid,40)
points(t(p1), pch=19, col="black")
s <- 1000

for (row in 1:nrow(graddf)) {
  print(dirdf[row, 1:2])
  # gradient and neg. gradient
  arrows(x0=p1[1], y0=p1[2], x1=(p1[1]+graddf[row, 1]/s), y1=(p1[2]+graddf[row, 2]/s), 
           angle=15, length=0.15, col = "green")
}

for (row in 1:nrow(dirdf)) {
  #s <- sqrt(dirdf[row, 1]^2 + dirdf[row, 2]^2) # scale by norm
  print(dirdf[row, 1:2])
  if (sign(dirdf[row, 1]) == -1 | sign(dirdf[row, 2]) == -1) {
    # at least 1 neg. directional derivative
    arrows(x0=p1[1], y0=p1[2], x1=(p1[1]+dirdf[row, 1]/s), y1=(p1[2]+dirdf[row, 2]/s), 
           angle=15, length=0.15, col = "blue")
  } else {
    # only pos. directional derivatives
    arrows(x0=p1[1], y0=p1[2], x1=(p1[1]+dirdf[row, 1]/s), y1=(p1[2]+dirdf[row, 2]/s), 
           angle=15, length=0.15, col = "red")
  }
}


title("Mod. Branin function with gradients & dir. deriv.")
legend("bottomleft", legend=c("2 pos. directional derivatives", 
                            ">=1 neg. directional derivatives",
                            "Gradient & neg. Gradient"),
       col=c("red", "blue", "green"),adj = c(0, 0.6), lty=1, cex=0.75)
dev.off()
