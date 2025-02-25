# ------------------------------------------------------------------------------
# multivariate first order

# FIG: plot bisection method process
# ------------------------------------------------------------------------------

library(knitr)
library(microbenchmark)
library(snow)
library(colorspace)
library(ggplot2)
library(zoo)
library(gridExtra)
source("functions.R")

# ------------------------------------------------------------------------------

pi = base::pi

foo = function(x, y) {
  - sin(x) * dnorm(y, mean = pi / 2, sd = 0.8)
}

x = y = seq(0, base::pi, length = 50)
z = outer(x, y, foo)
p = c(list(list(1, 0.1)), optim0(1, 0.1, FUN = foo, maximum = F))

png("../figure_man/iter-example-descent1.png", height=320, width=630)
plot <- sd_plot()
dev.off()

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
    
    p = ggplot(data = data.frame(cbind(x = X[, -1], y = y)), aes(x = x, y = y)) +
      geom_point() + 
      theme_bw() + 
      geom_smooth(method = "lm", color='blue') + 
      geom_abline(intercept = theta[1], slope = theta[2], colour='red') +
      ggtitle(paste("Iteration t =", counter)) +
      annotate("text", x = 25, y = 20, label = "True Relationship", 
               hjust = 1, size = 4, color = "blue") +
      annotate("text", x = 25, y = 10, label = "Gradient Descent", 
               hjust = 1, size = 4, color = "red")
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

ggsave("../figure_man/iter0.png", plist[[1]], width = 5, height = 3)
ggsave("../figure_man/iter1.png", plist[[2]], width = 5, height = 3)
ggsave("../figure_man/iter2.png", plist[[3]], width = 5, height = 3)

