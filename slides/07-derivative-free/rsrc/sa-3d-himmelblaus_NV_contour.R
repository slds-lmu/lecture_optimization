library(msm)
library(plotly)
library(mnormt)
#1 Initialize optim function and its domain; generate function to calculate acceptance probs.
#1.1 Choose function to optimize; here 'Himmelblau's function' chosen.
f = function(x1, x2){
  (x2^2 + x1 - 11)^2 + (x2 + x1^2 - 7)^2
}

#1.2 generate domain and respective values of function f.
x1 = seq(-5, 5, length.out = 400)
x2 = seq(-5, 5, length.out = 400)
z = outer(x1, x2, f)

#1.3 Calculates the acceptance probabilities given a current iteration
#### Function input: f = optim function, xcurr = current point in space, z = overall matrix of z=f(x) values,
#### tempcurr = current temperature.
calcAcceptanceProbs = function(f, xcurr, z, tempcurr){
    zcurr = f(xcurr[1], xcurr[2])
    prob = z
    prob[prob < zcurr] = 1
    prob[prob >= zcurr] = exp(-(prob[prob >= zcurr] - zcurr) / tempcurr)
    return(prob)
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
plot3d

#2.1 2d contour plot
plot2d = plot_ly(x = ~x1, y = ~x2, z = z) %>% 
  add_contour() %>%
    layout(
      title = "Himmelblau's function",
      scene = list(
      xaxis = list(title = "x2"),
      yaxis = list(title = "x1")
      )
    ) %>% 
    add_annotations(text = "z", xref="paper", yref="paper", x=1.05, xanchor="left",
      y=1, yanchor="bottom", legendtitle=TRUE, showarrow=FALSE)

plot2d

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
tracking = data.frame(x[1], x[2], x[1], x[2], 0, 0, f(x[1], x[2]), 1, NA)
colnames(tracking) = c("xbest_x1", "xbest_x2", "xnew_x1", "xnew_x2", "xold_x1", "xold_x2", "z", "iteration", "type")

#3.2 start algorithm simulated annealing.
for(k in 2:length(tempseq)){    
  xold = x                                    
  xnew = c(rtnorm(n = 1, mean = x[1], sd = 1.5, lower = -3.8, upper = 3.8),
           rtnorm(n = 1, mean = x[2], sd = 1.5, lower = -3.8, upper = 3.8))          
  type = "reject"
  if(f(xnew[1], xnew[2]) < f(x[1], x[2])){
    x = xnew
    type = "accept"
  } else{
      prob = exp(-(f(xnew[1],xnew[2]) - f(x[1],x[2]))/tempseq[k])
      if(sample(c(0,1), size = 1, prob = c(1-prob, prob)) == 1){
        x = xnew
        type = "accept"
      }
    }
  if(f(x[1], x[2]) < f(xbest[1], xbest[2])){
    xbest = x 
  }
  tracking[k,] = c(xbest[1], xbest[2], xnew[1], xnew[2], xold[1], xold[2], f(xbest[1], xbest[2]), k, type)
}

#4 Generate probabilitites of acceptance

#4.1 Prob matrizes for iteration 1, 2, 3, 50, 100
PAacceptiter = lapply(iters, function(x) calcAcceptanceProbs(f = f, xcurr = as.numeric(c(tracking[x,"xold_x1"], tracking[x,"xold_x2"])), z = z,
                                    tempcurr = tempseq[x]))


#4.2 Arrange data for plot of acceptance probs
dfp = tracking[, c("xold_x1", "xold_x2", "xnew_x1", "xnew_x2", "iteration", "type")]
dfp = as.data.frame(dfp)


#4.3 Plot accceptance probs
for(k in 1:length(PAacceptiter)){
  
  #create bivariate normal distribution for contour
  x_nd <- seq(as.numeric(dfp$xold_x1[iters[k]])-1, as.numeric(dfp$xold_x1[iters[k]]), as.numeric(dfp$xold_x1[iters[k]])+1) 
  y_nd <- seq(as.numeric(dfp$xold_x2[iters[k]])-1, as.numeric(dfp$xold_x2[iters[k]]), as.numeric(dfp$xold_x2[iters[k]])+1)
  mu_nd <- c(as.numeric(dfp$xold_x1[iters[k]]), as.numeric(dfp$xold_x2[iters[k]]))
  sigma_nd <- matrix(c(1.5, 0, 0, 1.5), nrow=2)
  f_nd <- function(x_nd, y_nd) dmnorm(cbind(x_nd, y_nd), mu_nd, sigma_nd)
  z_nd <- outer(x_nd, y_nd, f_nd)
  
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
    add_trace(type= "scatter", mode="markers",
              x = as.numeric(dfp$xold_x1[iters[k]]),
              y = as.numeric(dfp$xold_x2[iters[k]]),
              marker=list(color="blue", size=80, opacity=0.1)) %>%
    add_trace(type= "scatter", mode="markers",
              x = as.numeric(dfp$xold_x1[iters[k]]),
              y = as.numeric(dfp$xold_x2[iters[k]]),
              marker=list(color="blue", size=60, opacity=0.2)) %>%
    add_trace(type= "scatter", mode="markers",
              x = as.numeric(dfp$xold_x1[iters[k]]),
              y = as.numeric(dfp$xold_x2[iters[k]]),
              marker=list(color="blue", size=40, opacity=0.3)) %>%
    
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
    add_trace(type= "scatter", mode="markers",
              x = as.numeric(dfp$xold_x1[iters[k]]),
              y = as.numeric(dfp$xold_x2[iters[k]]),
              marker=list(color="blue", size=80, opacity=0.1)) %>%
    add_trace(type= "scatter", mode="markers",
              x = as.numeric(dfp$xold_x1[iters[k]]),
              y = as.numeric(dfp$xold_x2[iters[k]]),
              marker=list(color="blue", size=60, opacity=0.2)) %>%
    add_trace(type= "scatter", mode="markers",
              x = as.numeric(dfp$xold_x1[iters[k]]),
              y = as.numeric(dfp$xold_x2[iters[k]]),
              marker=list(color="blue", size=40, opacity=0.3)) %>%
  
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
