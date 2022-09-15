



library(knitr)
library(linprog)
library(snow)
library(colorspace)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)


plotPoly = function(A, b) {
  
  p = ggplot(data = data.frame(x = 0), mapping = aes(x = x))
  p = p + geom_point(x = 0, y = 0, size = 2, color = "green")
  
  for (i in 1:dim(A)[1]) {
    p = p + geom_abline(intercept = b[i] / A[i, 2], slope = - A[i, 1] / A[i, 2], color = "green")
  }
  
  p = p + geom_segment(aes(x = 0, y = -Inf, xend = 0, yend = Inf), color = "green")
  p = p + geom_segment(aes(x = -Inf, y = 0, xend = Inf, yend = 0), color = "green")
  
  combs = combn(1:dim(A)[1], 2)
  
  V = matrix(0, 0, 2)
  
  for (i in 1:dim(combs)[2]) {
    
    ind = combs[, i]
    
    if (det(A[ind, ]) != 0) {
      sol = solve(A[ind, ], b[ind])
      
      if (all(A %*% sol <= b)) {
        V = rbind(V, solve(A[ind, ], b[ind]))
        p = p + geom_point(x = sol[1], y = sol[2], color = "green", size = 2)
      }
    }
  }
  
  V = as.data.frame(V)
  
  center = apply(V, 2, mean)
  Vdiff = V - center
  
  ordering = vector(length = 0)
  
  for (i in 1:dim(Vdiff)[1]) {
    ordering = cbind(ordering, atan2(Vdiff[i, 1], Vdiff[i, 2]))
  }
  
  V = V[order(ordering), ]
  
  p = p + geom_polygon(data = V, aes(x = V1, y = V2), fill = "green", alpha = 0.1)
  
  p = p + coord_equal() + xlab(expression(x[1])) + ylab(expression(x[2]))
  
  
  p = p + xlim(c(0, 1)) + ylim(c(0, 1)) + theme_bw()
  
  p
}


A = matrix(c(1, 2, -1, 0, 2, 1, 0, -1), ncol = 2)
b = c(1, 1, 0, 0)

plotPoly(A, b)


###############



f = function(x) b[1] / A[1, 2] - b[1] / A[1, 2] * x

p = ggplot(data = data.frame(x = 0))
p = p + geom_abline(slope = - b[1] / A[1, 2], intercept =  b[1] / A[1, 2], color = "green")
p = p + geom_polygon(data = data.frame(x = c(-5, 5, 5, -5), y = c(-5, -5, f(5), f(-5))), aes(x = x, y = y), fill = "green", alpha = 0.1)
p = p + geom_segment(aes(x = 0, y = f(0), xend = 0 - A[1, 1], yend = f(0) - A[1, 2]), arrow = arrow(length = unit(0.03, "npc")), color = "green")

p = p + xlab(expression(x[1])) + ylab(expression(x[2]))

p = p + theme_bw() + coord_equal()

p = p + xlim(c(-5, 5))
p = p + ylim(c(-5, 5))

p

plotPoly(A, b)

#################


f =  function(x, z) - z - x
dd = expand.grid(x = seq(-2, 2, by = 0.01), z = seq(-2, 2, by = 1 / 6))
dd %>% rowwise %>% mutate(y = f(x = x, z = z)) -> dd2

p = ggplot() + geom_path(data = dd2, aes(x, y, col = z, group = z))
p = p + scale_colour_gradientn(colours = terrain.colors(10))
p = p + theme_bw() + xlim(c(0, 1)) + ylim(c(0, 1))
p = p + xlab("x1") + ylab("x2") + labs(colour = "y")
p = p + geom_segment(aes(x = 0.5, y = 0.5, xend = 0.625, yend = 0.625), arrow = arrow(length = unit(0.03, "npc")))
p = p + geom_text(aes(x = 0.5, y = 0.5, label = "-c"), hjust = -1.5, vjust = -0.4, size = 5)

p = p + coord_equal()

p

#################


p = plotPoly(A, b)

p = p + geom_path(data = dd2, aes(x, y, col = z, group = z))

p = p + scale_colour_gradientn(colours = terrain.colors(5))
p = p + xlab("x1") + ylab("x2") + labs(colour = "y") + theme_bw()

p = p + geom_abline(intercept = 2 / 3, slope = - 1, color = "black")
p = p + geom_segment(aes(x = 1 / 3, y = 1 / 3, xend = 1 / 3 + 0.1, yend = 1 / 3 + 0.1), arrow = arrow(length = unit(0.03, "npc")))
p = p + geom_text(aes(x = 1 / 3, y = 1 / 3, label = "-c"), hjust = -1.5, vjust = -0.4, size = 5)

p = p + coord_equal()

p

####################

# Case 1: feasible region empty
p1 = ggplot(data = data.frame(x = 0), mapping = aes(x = x))
p1 = p1 + geom_rect(aes(xmin = -Inf, ymin = -Inf, xmax = 1, ymax = Inf), fill = "green", alpha = 0.1)
p1 = p1 + geom_vline(xintercept = 1,color = "green")
p1 = p1 + geom_abline(intercept = 2, slope = - 1, color = "blue")
p1 = p1 + geom_polygon(data = data.frame(x = c(0, 2, 2), y = c(2, 0, 2)), aes(x = x, y = y), fill = "blue", alpha = 0.1)
p1 = p1 + geom_abline(intercept = 0, slope = 1 / 2, color = "orange")
p1 = p1 + geom_polygon(data = data.frame(x = c(0, 2, 2), y = c(0, 0, 1)), aes(x = x, y = y), fill = "orange", alpha = 0.1)
p1 = p1 + coord_equal() + ggtitle("Case 1")
p1 = p1 + xlab(expression(paste(x[1]))) + xlab(expression(paste(x[2])))
p1 = p1 + xlim(c(0, 2)) + ylim(c(0, 2)) + theme_bw()


# Case 2: unrestricted solution
p2 = ggplot(data = data.frame(x = 0), mapping = aes(x = x))

p2 = p2 + geom_abline(intercept = 1, slope = - 1, color = "green")
p2 = p2 + geom_abline(intercept = 1, slope = 1, color = "green")
p2 = p2 + geom_polygon(data = data.frame(x = c(0.5, 1, 1, 0.5, 0), y = c(0.5, 0.5, 1.5, 1.5, 1)), aes(x = x, y = y), fill = "green", alpha = 0.1)
p2 = p2 + geom_abline(intercept = 1.5, slope = - 1)
p2 = p2 + geom_segment(aes(x = 0.5, y = 1, xend = 0.75, yend = 1.25), arrow = arrow(length = unit(0.03, "npc")))
p2 = p2 + geom_text(aes(x = 0.5, y = 1, label = "-c"), hjust = -1.5, vjust = -0.4, size = 5)
p2 = p2 + xlab(expression(paste(x[1]))) + xlab(expression(paste(x[2])))
p2 = p2 + coord_equal() + ggtitle("Case 2")
p2 = p2 + xlim(c(0, 1)) + ylim(c(0.5, 1.5)) + theme_bw()
p2 = p2 + xlab(expression(x[1])) + ylab(expression(x[2]))

# Case 3: feasible region restricted and optimum solution exists
p3 = ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  geom_abline(intercept = 2, slope = - 2, color = "green")
p3 = p3 + geom_abline(intercept = 1, slope = -0.5, color = "green")
p3 = p3 + geom_abline(intercept = 0.5, slope = 1, color = "green")
p3 = p3 + geom_abline(intercept = 0.5, slope = - 1, color = "green")
p3 = p3 + geom_abline(intercept = - 5 / 12, slope = 5 / 6, color = "green")
p3 = p3 + geom_polygon(data = data.frame(x = c(0, 0.5, 29/34, 2/3, 1/3), y = c(0.5, 0, 0.29, 2/3, 5/6)), aes(x = x, y = y), fill = "green", alpha = 0.1)
p3 = p3 + geom_point(aes(x = 2 / 3, y = 2 /3), size = 2, colour = "red")
p3 = p3 + geom_abline(intercept = 4/3, slope = - 1)
p3 = p3 + geom_segment(aes(x = 2 / 3, y = 2 / 3, xend = 5/6, yend = 5/6), arrow = arrow(length = unit(0.03, "npc")))
p3 = p3 + geom_text(aes(x = 2 / 3, y = 2 / 3, label = "-c"), hjust = -1.5, vjust = -0.4, size = 5)
p3 = p3 + coord_equal()
p3 = p3 + ggtitle("Case 3")
p3 = p3 + xlab(expression(paste(x[1]))) + xlab(expression(paste(x[2])))
p3 = p3 + xlim(c(0, 1)) + ylim(c(0, 1)) + theme_bw()

grid.arrange(p1, p2, p3, ncol = 3)






