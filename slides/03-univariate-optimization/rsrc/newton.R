# ------------------------------------------------------------------------------
# univariate optimization

# FIG: plot newton method process
# ------------------------------------------------------------------------------

newton = function(f = function(x) sin(x) * -1, a = 2, b = 5, q = 0.5, tol = .Machine$double.eps^0.5, maxiter = 1000, plot.i) {
 if (f(a) * f(b) > 0) 
   stop("no root within interval!")
  x = a
  z = seq(a - 0.2 * a, b + 0.2 * b, length = 100)

  eps = tol + 1
  i = 1

  while(eps >= tol & i < maxiter) {
    x0 = x
    if(i == plot.i) {
      plot(f(z) ~ z, type = "l", xlab = "x", ylab = "f(x)")
      abline(h = 0, lty = 2, lwd = 0.5)
      y = drop(attr(numericDeriv(quote(f(x)), "x"), "gradient")) * (z - x0) + f(x0)
      lines(y ~ z, col = "lightgray")
    }
    x = x - f(x) / drop(attr(numericDeriv(quote(f(x)), "x"), "gradient"))
    if(f(x) == 0) break
    eps = abs(f(x))

    if(i == plot.i) {
      lines(c(0, f(x)) ~ c(x, x), col = "lightgray")
      lines(f(z) ~ z, lwd = 2)
      points(x, 0, pch = 16, col = "lightgray")
      points(x0, f(x0), pch = 16)
      text(x0, f(x0), parse(text = paste("x^(", i, ")", sep = "")), pos = 2)
      mtext(paste("root =", format(x, digits = 6, nsmall = 6)), side = 3, line = 1)
    }

    i = i + 1
 }
#png(paste("figure_man/newton/pic-", i, ".png", sep = ""), units = "in", res = 120, width = 5, height = 4)
#par(mar = c(4, 4, 3, 0.1))




   #dev.off()


 rval = list("root" = x, "f.root" = f(x),
              "iter" = i, "estim.prec" = eps)
 # return(rval)
}

png('../figure_man/newton1.png', width = 480, height = 320)
newton(plot.i = 1)
dev.off()

png('../figure_man/newton2.png', width = 480, height = 320)
newton(plot.i = 2)
dev.off()

png('../figure_man/newton3.png', width = 480, height = 320)
newton(plot.i = 3)
dev.off()

png('../figure_man/newton4.png', width = 480, height = 320)
newton(plot.i = 4)
dev.off()

png('../figure_man/newton5.png', width = 480, height = 320)
newton(plot.i = 5)
dev.off()

