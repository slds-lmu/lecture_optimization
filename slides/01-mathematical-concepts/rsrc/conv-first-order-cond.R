# ------------------------------------------------------------------------------
# mathematical concepts

# FIG: plot first-order condition
# ------------------------------------------------------------------------------

x0 <- 0.1

f1 <- function(x) { x^2 }
f2 <- function(x) { x0^2 + 2*x0 * (x-x0) }

png("../figure_man/conv-first-order-cond.png", width=400, height=300)

a <- -0.3
b <- 0.7

par(xaxt='n', yaxt='n', mar=c(0,0,0,0))

curve(f1, from=a, to=b, lwd=3, xlim=c(a,0.75), ylim=c(f2(a), 0.2), xlab="", ylab="")
curve(f2, from=a, to=b, lwd=3, lty='dashed', add=TRUE)
points(x0, f1(x0), pch=19, cex=1.5)
text(a+0.02, f1(a+0.02), "f(y)", pos=4, cex=1.5)
text(x0+0.02, f1(0), "(x,f(x))", pos=1, cex=1.5)
text(0.6, f2(0.4), "f(x) + f'(x)(y-x)", pos=1, cex=1.5)

dev.off()
