# ------------------------------------------------------------------------------
# derivative free

# FIG: circle grids
# ------------------------------------------------------------------------------

library(knitr)
library(ecr)
library(ggplot2)
library(smoof)
library(latex2exp)
library(ggforce)
library(reshape2)
library(mlr)

set.seed(124)

# ------------------------------------------------------------------------------

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
    r = min(c(x, 10 - x, y, 10 - y, runif(1) + 0.5))
    
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

# define new grid
grid = setup.grid(20)

Pl = plot.grid(grid)

ggsave("../figure_man/grid.png", Pl)

