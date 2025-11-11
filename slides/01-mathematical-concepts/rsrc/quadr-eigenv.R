# ------------------------------------------------------------------------------
# mathematical concepts

# FIG: plot eigen-vectors of quadratic form
# ------------------------------------------------------------------------------
v1 = c(1, -1) / sqrt(2)
v2 = c(1, 1) / sqrt(2)
l1 = 3
l2 = 1

V <- cbind(v1, v2)
L <- diag(c(l1, l2))
A <- V %*% L %*% t(V)

f <- function(x, y) {
  t(c(x, y)) %*% A %*% c(x, y)
}

lim_x = c(-8, 8)
lim_y = lim_x

x <- seq(lim_x[1], lim_x[2], 0.05)
y <- seq(lim_y[1], lim_y[2], 0.05)
z <- outer(x, y, FUN = function(x, y) {
  xy <- mapply(c, x, y)
  apply(xy, 2, function(v) {
    f(v[1], v[2])
  })
})

png("../figure_man/quadr-eigenv.png", width = 400, height = 400)

par(xaxt = "n", yaxt = "n", mar = c(0, 0, 0, 0))
contour(x, y, z, drawlabels = FALSE, nlevels = 6)

abline(a = 0, b = -1, col = "magenta", lty = 3, lwd = 2)
abline(a = 0, b = 1, col = "orange", lty = 3, lwd = 2)

grid(nx = 7, ny = 7, lty = 1, col = "gray", lwd = 2)

contour(x, y, z, col = "blue", lwd = 3, drawlabels = FALSE, nlevels = 6, add = TRUE)

par(xaxt = "n", yaxt = "n", mar = c(0, 0, 0, 0))

arrows(0, 0, l1 * v1[1], l1 * v1[2], length = 0.15, col = "magenta", lwd = 3)
arrows(0, 0, l2 * v2[1], l2 * v2[2], length = 0.15, col = "orange", lwd = 3)

points(0, 0, col = "blue", pch = 19, cex = 2)

dev.off()
