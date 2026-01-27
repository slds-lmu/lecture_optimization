

f = function(x) -10*exp(-0.01*x^2) + sin(2*x)
g = function(x) {
  y = f(x)
  curve(f, -2 , 15, lwd = 2)
  points(x, y, cex = 2, pch = 20)
  Sys.sleep(0.3)
  return(y)
}

par = 9
optim(par, g, method = "SA")

