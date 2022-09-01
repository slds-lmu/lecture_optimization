source("./functions.R")

f(x, y) = -sin(x)
  
    pi = base::pi
    
    foo = function(x, y) {
      - sin(x) * dnorm(y, mean = pi / 2, sd = 0.8)
    }
    
    x = y = seq(0, base::pi, length = 50)
    z = outer(x, y, foo)
    p = c(list(list(1, 0.1)), optim0(1, 0.1, FUN = foo, maximum = F))
    
    sd_plot()