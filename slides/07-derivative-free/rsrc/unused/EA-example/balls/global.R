library(shiny)
library(ggforce)
library(ecr)
library(latex2exp)

mutate = ecr::mutate


# some definitions
lower = c(0, 0)
upper = c(10, 10)


plot.grid = function(d) {
  p = ggplot() + geom_circle(data = d, aes(x0 = x, y0 = y, r = r), colour = "grey",fill = "grey", alpha = 0.8)  + theme_bw() + coord_equal() + xlim(c(0, 10)) + ylim(c(0, 10))
  p = p + geom_rect(aes(xmin = 0, xmax = 10, ymin = 0, ymax = 10), colour = "grey", alpha = 0.05)
  p = p + theme(axis.line = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    line = element_blank(),
    title = element_blank(),
    text = element_blank()
  )
  return(p)
}

setup.grid = function(n) {
  x = runif(1, min = 0, max = 10)
  y = runif(1, min = 0, max = 10)
  r = min(c(x, 10 - x, y, 10 - y, runif(1) + 0.7))
  default.grid = data.frame(x = x, y = y, r = r) 
  
  i = 0
  
  while (nrow(default.grid) < n) {
    x = runif(1, min = 0, max = 10)
    y = runif(1, min = 0, max = 10)
    r = min(c(x, 10 - x, y, 10 - y, runif(1) + 0.3))
    
    dists = t(apply(default.grid[, 1:2], 1, function(z) sqrt(sum((z - c(x, y))^2))))
    dists = dists - default.grid[, 3]
    
    if (any(dists < 0)) {
      default.grid = default.grid
    } else {
      r = min(dists, r)
      
      default.grid = rbind(default.grid, c(x, y, r))
    }
  }
    
  return(default.grid)
}

calculateFitnessFun = function(grid) {
  fun = function(cand) {
    distances = apply(grid[, 1:2], 1, function(x) sqrt(sum((x - cand)^2))) - grid[, 3]
    boundaries = c(cand[1L], 10 - cand[1L], cand[2L], 10 - cand[2L])
    res = min(c(distances, boundaries))
    
    res = ifelse(res < 0, 0, res)
    
    return(res)
  }
  return(fun)
}

