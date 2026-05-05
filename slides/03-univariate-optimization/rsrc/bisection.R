# ------------------------------------------------------------------------------
# univariate optimization

# FIG: plot bisection method process
# ------------------------------------------------------------------------------

bisection = function(a,b, fun, q = .5, max.iter = 10, tol = 0.001) {
  lower = numeric(max.iter)
  upper = numeric(max.iter)
  new = numeric(max.iter)
  root = NA
  
  lower[1] = a
  upper[1] = b
  
  for(i in 1:max.iter) {
    new[i] = q * (lower[i]+upper[i])
    if(fun(new[i]) == 0 | (upper[i]-lower[i])/2 < tol) {
      print("Root found!")
      root = new[i]
      break
    }
    if(sign(fun(new[i])) == sign(fun(lower[i]))) {
      lower[i+1] = new[i]
      upper[i+1] = upper[i]
    } else {
      lower[i+1] = lower[i]
      upper[i+1] = new[i]
    }
  }
  return(list(root = root, lower = lower, upper = upper, new = new, a = a, b = b))
}

sin.bi = bisection(0.5, 4.5, sin, max.iter = 6)

plot.bisection = function(i, results, real.root = NA, colors = c("firebrick1", "firebrick4", "blue")) {
  curve(sin, results$a, results$b, lwd = 2, ylab = "f(x)")
  abline(h = 0, lty = 2)
  if(!is.na(real.root)) abline(v = real.root, lty = 2)
  abline(v = results$lower[i], lwd = 2, col = colors[1])
  abline(v = results$upper[i], lwd = 2, col = colors[2])
  abline(v = results$new[i], lwd = 2,  col = colors[3])
  legend(3.6, 1, c("left", "right", "new"), col = colors, lwd = 2)
}

pi = base::pi


sin.bi = bisection(2, 4, sin, max.iter = 6)

png('../figure_man/bisection0.png', width = 480, height = 320)
plot.bisection(1, sin.bi, real.root = pi, colors = c("firebrick1", "firebrick4", NA))
dev.off()

png('../figure_man/bisection1.png', width = 480, height = 320)
plot.bisection(1, sin.bi, real.root = pi)
dev.off()

png('../figure_man/bisection2.png', width = 480, height = 320)
plot.bisection(2, sin.bi, real.root = pi)
dev.off()

png('../figure_man/bisection3.png', width = 480, height = 320)
plot.bisection(3, sin.bi, real.root = pi)
dev.off()

png('../figure_man/bisection4.png', width = 480, height = 320)
plot.bisection(4, sin.bi, real.root = pi)
dev.off()

png('../figure_man/bisection5.png', width = 480, height = 320)
plot.bisection(5, sin.bi, real.root = pi)
dev.off()

png('../figure_man/bisection6.png', width = 480, height = 320)
plot.bisection(6, sin.bi, real.root = pi)
dev.off()
