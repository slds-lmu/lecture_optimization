# ------------------------------------------------------------------------------
# constrained

# FIG: simplex algorithm implementation
# ------------------------------------------------------------------------------

library(ggplot2)

# ------------------------------------------------------------------------------

A = matrix(c(-1, 1, 2, 1, -1, 0, 1, 2, 1, -1, 0, - 1), ncol = 2)
b = c(0.5, 2, 2, 0.5, 0, 0)

start = c(0, 0)
cost = c(-1, -1)


simplex = function(A, b, cost, start) {
  c = - cost

  V = data.frame(x1 = start[1], x2 = start[2], x1old = NA, x2old = NA, cost = cost %*% start, intercept = cost %*% start / cost[2], slope = - cost[1] / cost[2])
  
  m = dim(A)[1]
  n = dim(A)[2]
  
  combs = (combn(dim(A)[1], dim(A)[2]))

  end = F
  counter = 1
  

  while (end == F) {
    
    v = as.numeric(V[counter, c("x1", "x2")])
    
    basis = which(round(A %*% v - b, 2) == 0)
    
    N = setdiff(1:m, basis)
    
    edges = - solve(A[basis, ])
    
    if (all(c %*% edges <= 0)) {
      end = T
      
      # return(v)
      
    } else {
      p = sample(which(c %*% edges > 0), 1)
      R = which(A %*% edges[, p] > 0)
      
      if (length(R) == 0) {
        stop("unbounded problem")
      } else {
        inew = R[which.min((b[R] - A[R, ] %*% v) / (A[R, ] %*% edges[, p]))]
        lambda = min((b[R] - A[R, ] %*% v) / (A[R, ] %*% edges[, p]))
        basis = basis[- p]
        basis = c(basis, inew)
        
        vnew = v + lambda * edges[, p]
        costnew = round(cost %*% vnew, 2)
        
        counter = counter + 1
        
        V = rbind(V, c(vnew, v, costnew, costnew / cost[2], - cost[1] / cost[2]))
        
      }
    }
  }
  return(V)
}


V = simplex(A, b, cost, start)
V


# plot polyhedron
plotPoly = function(A, b) {
  
  p = ggplot(data = data.frame(x = 0), mapping = aes(x = x)) 
  p = p + geom_point(x = 0, y = 0, size = 2, color = "green")
  
  for (i in 1:dim(A)[1]) {
    p = p + geom_abline(intercept = b[i] / A[i, 2], slope = - A[i, 1] / A[i, 2], color = "green")
    # p = p + geom_point(x = 0, y = b[i] / A[i, 2], color = "green", size = 2)
    # p = p + geom_point(x = b[i] / A[i, 1], y = 0, color = "green", size = 2)
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
  
  p = p + coord_equal()
  
  
  p = p + xlim(c(0, 1)) + ylim(c(0, 1)) + theme_bw() 
  
  p = p + xlab("x1") + ylab("x2")
  
  p
}


for (i in 1:dim(V)[1]) {
  p1 = plotPoly(A, b) + ggtitle(paste("Iteration", i))
  p1 = p1 + geom_abline(data = V[1:i, ], aes(intercept = intercept, slope = slope), lty = 2)
  # p1 = p1 + geom_segment(data = V[1:i, ], aes(x = x1, y = x2, xend = x1 - 0.4 * cost[1], yend = x2 - 0.4 * cost[2]), arrow = arrow(length = unit(0.03, "npc")))
  p1 = p1 + geom_point(data = V[1:i, ], aes(x = x1, y = x2), color = "red", size = 2)
  p1 = p1 + geom_text(data = V[1:i, ], aes(x = x1, y = x2, label = cost), hjust = - 0.2, vjust = - 1)
  p1 = p1 + geom_segment(data = V[1:i, ], aes(x = x1old, y = x2old, xend = x1, yend = x2), color = "red", arrow = arrow(length = unit(0.03, "npc")))
  
  ggsave(filename = paste("../figure_man/simplex_implementation/iter", i, ".png", sep = ""), p1)
}
