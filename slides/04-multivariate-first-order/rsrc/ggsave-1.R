

library(knitr)
library(microbenchmark)
library(snow)
library(colorspace)
library(ggplot2)
library(zoo)
library(gridExtra)
source("rsrc/functions.R")




pi = base::pi

foo = function(x, y) {
  - sin(x) * dnorm(y, mean = pi / 2, sd = 0.8)
}

x = y = seq(0, base::pi, length = 50)
z = outer(x, y, foo)
p = c(list(list(1, 0.1)), optim0(1, 0.1, FUN = foo, maximum = F))

sd_plot()








data(cars)

n = dim(cars)[1]

X = cbind(rep(1, n), cars$speed)
y = cars$dist

f = function(theta)  1 / n * sum((y - X %*% t(theta))^2)
grad = function(theta) 2 / n * t(y - X %*% theta) %*% X

gradient.descent = function(X, y, n.iter = 3, theta.start = c(1, 1)) {
  
  plist = list()
  
  theta = theta.start
  
  counter = 0
  
  while (counter < n.iter) {
    
    p = ggplot(data = data.frame(cbind(x = X[, -1], y = y)), aes(x = x, y = y)) + geom_point()
    p = p + theme_bw() + geom_smooth(method = "lm", aes(colour='True relationship'), show.legend = NA) + labs(colour = "Iteration")
    
    p = p + geom_abline(aes(intercept = theta[1], slope = theta[2], colour='Gradient Descent'), show.legend=FALSE)
    p = p + ggtitle(paste("Iteration t =", counter)) + scale_colour_manual(name='legend', values=c('red', 'blue'))
    ggsave(plot)
    counter = counter + 1
    
    plist[[counter]] = p
    
    dir = grad(theta)
    dir = dir / sqrt(sum(dir^2))
    
    stepsize = optimize(f = function(s) f(theta + s * dir), interval = c(0, 10))$minimum
    
    theta = as.vector(theta + stepsize * dir)
  }
  return(plist)
}

plist = gradient.descent(X, y, n.iter = 4, theta.start = c(-7, -1))

plist[[1]]
plist[[2]]
plist[[3]]


