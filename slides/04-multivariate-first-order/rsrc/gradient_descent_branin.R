# Optimization WiSe 22/23
# Chapter 4.1 
# Plot gradient descent

library(DiceKriging) # Mod. branin function: https://www.sfu.ca/~ssurjano/Code/braninr.html


n.grid <- 20
x.grid <- y.grid <- seq(0,1,length=n.grid)
design.grid <- expand.grid(x.grid, y.grid)
response.grid <- apply(design.grid, 1, branin)
z.grid <- matrix(response.grid, n.grid, n.grid)


library(rootSolve) #gradient()

create_point <- function(x0) {
  neggrad <- -gradient(branin, x0)
  neggrad <- neggrad/norm(neggrad)
  points <- matrix(x0, nrow=1)
  return(points)
}

grad_desc <- function(x0, list_of_points, alpha=0.001, steps=50) {
  x_old <- x0
  for (step in 1:steps) {
    x_new <- x_old + alpha*(-gradient(branin, x_old))
    y_new <- branin(x_new)
    x_old <- x_new
    
    points <- rbind(points, x_old)
    step <- step + 1
  }
  return(points)
}

plot_points <- function(list_of_points, color="red") {
  for (point in 1:nrow(list_of_points)) {
    points(t(list_of_points[point, ]), pch=19, col=color, cex=0.8)
  }
}


#### plot point, pos. & neg. gradients and different directional derivatives
# plot: https://rdrr.io/cran/DiceKriging/man/branin.html

# prepare points & HPs
starting_points <- matrix(rbind(c(0.7, 0.95), c(0.001, 0.001), c(0.3, 0.9)), nrow=3)
colors = c("red", "green", "blue")
alpha <- c(0.0001, 0.0001, 0.0001)
steps <- c(500, 500, 500)
i <- 1

# create plot
plot.new()

pdf(file = "figure_man/gradient_descent_branin.pdf",   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 5) # The height of the plot in inches

contour(x.grid,y.grid,z.grid,40)

for (starting_point in 1:nrow(starting_points)) {
  # prepare data
  point <- starting_points[starting_point, ]
  points <- create_point(point)
  points <- grad_desc(x0=point, list_of_points=points, alpha=alpha[i], steps=steps[i])
  
  # plot paths
  plot_points(list_of_points=points, color=colors[i])
  lines(points[,1], points[,2], pch=16, col=colors[i])
  
  # check if arrived at minimum
  print(paste0("Point: ", point))
  print(paste0("Gradient: ",gradient(branin, points[nrow(points), ]))) 
  i <- i+1
  starting_point <- starting_point+1
}


title("Gradient descent for Branin")
# legend(0.63, 0.17, legend=c(paste0("alpha=", alpha[1], ", steps=",steps[1]),
#                            paste0("alpha=", alpha[2], ", steps=",steps[2]),
#                            paste0("alpha=", alpha[3], ", steps=",steps[3])),
#         col=c("red", "green", "blue"), lty=1, cex=0.8)


dev.off()