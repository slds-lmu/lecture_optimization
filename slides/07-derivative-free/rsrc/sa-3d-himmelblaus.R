library(msm)
library(plotly)
library(ggplot2)

#1 Initialize optim function and its domain; generate function to calculate acceptance probs.
#1.1 Choose function to optimize; here 'Himmelblau's function' chosen.

f = function(x){
  (x[2]^2 + x[1] - 11)^2 + (x[2] + x[1]^2 - 7)^2
}
fvec = function(x1, x2){
  (x2^2 + x1 - 11)^2 + (x2 + x1^2 - 7)^2

}

#1.2 generate domain and respective values of function f.
x1 = seq(-5, 5, length.out = 400)
x2 = x1
z = outer(x1, x2, fvec) # needed for plotly
grid <- expand.grid(x1 = x1, x2 = x2) # needed for ggplot
grid$y = apply(grid, 1, f)

#1.3 Calculates the acceptance probabilities given a current iteration
#### Function input: f = optim function, xcurr = current point in space, z = overall matrix of z=f(x) values,
#### tempcurr = current temperature.
calcAcceptanceProbs = function(f, xcurr, grid, tempcurr){
    zcurr = f(xcurr)
    grid$prob = 1
    idx = which(grid$y >= zcurr)
    grid[idx, ]$prob =  exp(-(grid[idx, ]$y - zcurr) / tempcurr)
    return(grid)
}

#2 Visualize function in 3d space and with contour lines.
#2.1 3d plot
plot3d = plot_ly() %>% 
        add_surface(x = x1, y = x2, z = z) %>%
          layout(
            title = "Himmelblau's function",
            scene = list(
            xaxis = list(title = "x1"),
            yaxis = list(title = "x2"),
            zaxis = list(title = "z"),
            color = c("green")
            )
          ) %>% 
        add_annotations(text = "z", xref="paper", yref="paper", x=1.05, xanchor="left",
            y=1, yanchor="bottom", legendtitle=TRUE, showarrow=FALSE)


#2.1 2d contour plot

p = ggplot() + stat_contour_filled(data = grid, aes(x = x1, y = x2, z = y)) + xlab(expression(x[1])) + ylab(expression(x[2]))    
# p = p + geom_point(data = data.frame(x = 0, y = 1), aes(x = x, y = y), colour = "orange")
p = p + guides(fill = "none")

ggsave("figure_man/sa-himmelblauFun2D.pdf", p, height = 3, width=3)

#3 Simulated Annealing algorithm
#3.1 set start value x; set current best point 'xbest'; generate sequence of temperatures;
#### create 'tracking' matrix for saving parameters of each iteration.
#### 'tracking' columns: 'xbest_x1' and 'xbest_x2' = x and y coords of current best point,
#### 'xnew' = x and y coords of sampled point in respective iteration.
set.seed(1111)
x = c(0, 0) 
xbest = x
c = 0.8
T = 200
tempseq = c(rep(T, 50), c^{1:200} * T)
iters = c(2, 3, 4, 50, 51, 70) # iterations we want to consider 


# start point
tracking = data.frame(x[1], x[2], x[1], x[2], 0, 0, f(x), 1, NA)
colnames(tracking) = c("xbest_x1", "xbest_x2", "xnew_x1", "xnew_x2", "xold_x1", "xold_x2", "z", "iteration", "type")

#3.2 start algorithm simulated annealing.
for(k in 2:length(tempseq)){    
  xold = x                                    
  xnew = c(rtnorm(n = 1, mean = x[1], sd = 1.5, lower = -3.8, upper = 3.8),
           rtnorm(n = 1, mean = x[2], sd = 1.5, lower = -3.8, upper = 3.8))          
  type = "reject"
  if(f(xnew) < f(x)){
    x = xnew
    type = "accept"
  } else{
      prob = exp(-(f(xnew) - f(x))/tempseq[k])
      if(sample(c(0,1), size = 1, prob = c(1-prob, prob)) == 1){
        x = xnew
        type = "accept"
      }
    }
  if(f(x) < f(xbest)){
    xbest = x 
  }
  tracking[k,] = c(xbest[1], xbest[2], xnew[1], xnew[2], xold[1], xold[2], f(xbest), k, type)
}

#4 Generate probabilitites of acceptance

#4.1 Prob matrizes for iteration 1, 2, 3, 50, 100
PAacceptiter = lapply(iters, function(x) {
  calcAcceptanceProbs(f = f, xcurr = as.numeric(c(tracking[x,"xold_x1"], tracking[x,"xold_x2"])), grid = grid,
                                    tempcurr = tempseq[x])
})


#4.2 Arrange data for plot of acceptance probs
dfp = tracking[, c("xold_x1", "xold_x2", "xnew_x1", "xnew_x2", "iteration", "type")]
dfp = as.data.frame(dfp)

#4.3 Plot accceptance probs
for (k in 1:length(PAacceptiter)) {

  p1 = ggplot()
  p1 = p1 + geom_contour_filled(data = df, aes(x = x1, y = x2, z = y, colour = after_stat(level))) 
  p1 = p1 + xlab(expression(x[1])) + ylab(expression(x[2]))    
  p1 = p1 + geom_point(data = dfp[1:iters[k], ], aes(x = as.numeric(xold_x1), y = as.numeric(xold_x2)), colour = "lightgray")
  p1 = p1 + geom_point(data = dfp[iters[k], ], aes(x = as.numeric(xold_x1), y = as.numeric(xold_x2)), colour = "blue")
  p1 = p1 + geom_point(data = dfp[iters[k], ], aes(x = as.numeric(xnew_x1), y = as.numeric(xnew_x2)), colour = "orange")
  p1 = p1 + theme_bw() + theme(legend.position = "none")

  df = PAacceptiter[[k]]
  p2 = ggplot()
  p2 = p2 + geom_contour(data = df, aes(x = x1, y = x2, z = prob, colour = after_stat(level))) 
  p2 = p2 + xlab(expression(x[1])) + ylab(expression(x[2]))    
  p2 = p2 + scale_colour_gradient(low="red", high="green")
  p2 = p2 + theme_bw()
  p2 = p2 + geom_point(data = dfp[1:iters[k], ], aes(x = as.numeric(xold_x1), y = as.numeric(xold_x2)), colour = "lightgray")
  p2 = p2 + geom_point(data = dfp[iters[k], ], aes(x = as.numeric(xold_x1), y = as.numeric(xold_x2)), colour = "blue")
  p2 = p2 + geom_point(data = dfp[iters[k], ], aes(x = as.numeric(xnew_x1), y = as.numeric(xnew_x2)), colour = "orange")
  p2 = p2 + labs(colour='P(accept)') 

  # plot contour lines of normal distribution
  m = as.numeric(c(dfp[iters[k], ]$xold_x1, dfp[iters[k], ]$xold_x2))
  sigma = matrix(c(1.5, 0, 0, 1.5), nrow=2)
  grid2 = expand.grid(x1 = seq(m[1] - 1.5, m[1] + 1.5, length.out = 100), x2 = seq(m[2] - 1.5, m[2] + 1.5, length.out = 100))
  qsamp <- cbind(grid2, prob = mvtnorm::dmvnorm(grid2, mean = m, sigma = sigma))

  p2 = p2 + geom_contour(data = qsamp, aes(x = x1, y = x2, z = prob), alpha = 0.5)

  g = grid.arrange(p1, p2, nrow = 1)

  ggsave(paste0("figure_man/sa-iter", k, ".pdf"), g, height = 4, width = 10)
}



for(k in 1:length(PAacceptiter)){
  plot = plot_ly(x = ~x1, y = ~x2, z = PAacceptiter[[k]], 
    type = "contour", 
    autocontour = FALSE, 
    contours = list(end = 1, size = 0.1, start = 0), 
    colors = colorRamp(c("indianred3", "springgreen3"))) %>% 
    layout(
    title = paste0("P(acceptance) Iteration ", iters[k]),
    xaxis = list(title = "x1"),
    yaxis = list(title = "x2")
    ) %>%
    add_trace(type = "scatter", x = ~dfp$xold_x1[1:iters[k]], y = ~dfp$xold_x2[1:iters[k]], 
      mode = "markers", marker = list(color = "lightgray", size = 12), name = "old iter", dfp$iteration[1:iters[k]]) %>%
    add_trace(type = "scatter", x = ~dfp$xold_x1[iters[k]], y = ~dfp$xold_x2[iters[k]], 
      mode = "markers", marker = list(color = "blue", size = 12), dfp$iteration[iters[k]], name = "current iter") %>%
    add_trace(type = "scatter", x = ~dfp$xnew_x1[iters[k]], y = ~dfp$xnew_x2[iters[k]], 
      mode = "markers", marker = list(color = "orange", size = 12), name = "next iter", dfp$iteration[iters[k]]) %>%
    layout(annotations = list(text = dfp$type[iters[k]], x = ~dfp$xnew_x1[iters[k]], y = ~dfp$xnew_x2[iters[k]])) %>%
  config(
    toImageButtonOptions = list(
      format = "png",
      filename = paste0("sa-probs-iter", iters[k]),
      width = 700,
      height = 400
    ))
  print(plot)
}



#4.4 Arrange data for plot of simulated annealing algorithm
plot2Data = data.frame(iteration = iters)
colorticks = list(iter1 = c(1), iter2 = c(0, 1), iter3 = c(rep(0, times = iters[3] - 1), 1), iter4 = c(rep(0, times = iters[4] - 1), 1))

#4.5 Plot simulated annealing sampling points from iteration 0 to: 1, 2, 10, 500.
for(k in 1:length(PAacceptiter)){
plot2 = plot_ly(x = ~x1, y = ~x2, z = z) %>% 
          add_contour() %>%
          layout(title = paste0("Simulated Annealing Iteration ", iters[k]),
            xaxis = list(title = "x1"),
            yaxis = list(title = "x2")) %>%
    add_trace(type = "scatter", x = ~dfp$xold_x1[1:iters[k]], y = ~dfp$xold_x2[1:iters[k]], 
      mode = "markers", marker = list(color = "lightgray", size = 12), name = "old iter", dfp$iteration[1:iters[k]]) %>%
    add_trace(type = "scatter", x = ~dfp$xold_x1[iters[k]], y = ~dfp$xold_x2[iters[k]], 
      mode = "markers", marker = list(color = "blue", size = 12), dfp$iteration[iters[k]], name = "current iter") %>%
    add_trace(type = "scatter", x = ~dfp$xnew_x1[iters[k]], y = ~dfp$xnew_x2[iters[k]], 
      mode = "markers", marker = list(color = "orange", size = 12), name = "next iter", dfp$iteration[iters[k]]) %>%
  config(
    toImageButtonOptions = list(
      format = "png",
      filename = paste0("sa-iter", iters[k]),
      width = 700,
      height = 400
    ))
  print(plot2)
}
