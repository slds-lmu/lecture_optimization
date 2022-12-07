### Lecture: Optimizaiton, WiSe22/23
### Section 5.1 Newton-Raphson
### Plot Newton-Raphson vs GD

options(warn = 1)
library(ggplot2)

source("../../vistool/optimvis.R")

#https://www.sfu.ca/~ssurjano/Code/ackleyr.html
ackley <- function(xx, a=20, b=0.2, c=2*pi) {
  d <- length(xx)
  
  sum1 <- sum(xx^2)
  sum2 <- sum(cos(c*xx))
  
  term1 <- -a * exp(-b*sqrt(sum1/d))
  term2 <- -exp(sum2/d)
  
  y <- term1 + term2 + a + exp(1)
  return(y)
}

#https://www.sfu.ca/~ssurjano/Code/ackleyr.html
branin <- function(x, a=1, b=5.1/(4*pi^2), c=5/pi, r=6, s=10, t=1/(8*pi)) {  
  y <- a * (x[2] - b * x[1]^2 + c * x[1] - r)^2 + s * (1 - t) * cos(x[1]) + s
  return(y)
}

rosenbrock <- function(x) {  
  y <- 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2 
  return(y)
}


# Newton-Raphson
objNR = Objective$new(fun = rosenbrock)
optimNR = OptimizerNR$new(steps = 200L, step_size = 1, x0 = c(1.8, -0.8), gamma = 0.99, tau = 0.5)
optimNR$optimize(objNR)
print(objNR$archive)

# Gradient descent
objGD = Objective$new(fun = rosenbrock)
optimGD = OptimizerGD$new(steps = 200L, step_size = 0.05, x0 = c(1.8, -0.8))
optimGD$optimize(objGD)

# Visualization
visNR = Visualizer$new(obj = objNR, run_archs = list(objNR$archive, objGD$archive), x1lim = c(-2, 2), x2lim = c(-1, 3))

png(file="figure_man/NR_1.png",width=400, height=350)
visNR$plot_y_trace()
dev.off()

png(file="figure_man/NR_2.png",width=400, height=350)
visNR$plot_rbase_contour(20)
dev.off()

visNR$plot_rbase_3dsurf()
