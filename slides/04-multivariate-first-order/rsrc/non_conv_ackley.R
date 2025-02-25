# ------------------------------------------------------------------------------
# multivariate first order

# FIG: plot (non-)converging gradient descent for ackley
# ------------------------------------------------------------------------------

library(rootSolve) #gradient()

# ------------------------------------------------------------------------------

#https://www.sfu.ca/~ssurjano/Code/ackleyr.html
ackley <- function(xx, a=20, b=0.2, c=2*pi) {
  d <- length(xx)

  sum1 <- sum(xx^2)
  sum2 <- sum(cos(c*xx))

  term1 <- -a * exp(-b*sqrt(sum1/d))
  term2 <- -exp(sum2/d)

  y <- term1 + term2 + a + exp(1)
  return(y)
}

n.grid <- 30
x.grid <- seq(-1.5,1.5,length=n.grid)
y.grid <- seq(-2,1.7,length=n.grid)
design.grid <- expand.grid(x.grid, y.grid)
response.grid <- apply(design.grid, 1, ackley)
z.grid <- matrix(response.grid, n.grid, n.grid)

create_point <- function(x0) {
  neggrad <- -gradient(ackley, x0)
  neggrad <- neggrad/norm(neggrad)
  points <- matrix(x0, nrow=1)
  return(points)
}

grad_desc <- function(x0, list_of_points, alpha=0.001, steps=50, i) {
  x_old <- x0
  for (step in 1:steps) {
    x_new <- x_old + alpha*(-gradient(ackley, x_old))
    y_new <- ackley(x_new)
    y_values[step+1, i] <<- y_new
    x_old <- x_new
    
    points <- rbind(points, x_old)
    step <- step + 1
  }
  return(points)
}

plot_points <- function(list_of_points, color="red") {
  for (point in 1:nrow(list_of_points)) {
    points(t(list_of_points[point, ]), pch=20, col=color, cex=0.8)
  }
}


#### plot point, pos. & neg. gradients and different directional derivatives
# plot: https://rdrr.io/cran/DiceKriging/man/ackley.html

# prepare points & HPs
starting_points <- matrix(rbind(c(0.61, 0.72), c(0.61, 0.72), c(0.61, 0.73), c(0.62, 0.75)), nrow=4)
colors = c("green", "red", "orange", "blue")
alpha <- c(0.05, 0.01, 0.01, 0.01)
steps <- c(50, 50, 50, 50)
i <- 1
y_start <- c(apply(starting_points, 1, ackley))
y_values <- data.frame(matrix(ncol=nrow(starting_points), nrow=max(steps)))
y_values <- rbind(y_start, y_values)

# create plot
plot.new()
png('../figure_man/nonconv_ackley_plot.png', width = 480, height = 320)
contour(x.grid,y.grid,z.grid,40)

for (starting_point in 1:nrow(starting_points)) {
  # prepare data
  point <- starting_points[starting_point, ]
  points <- create_point(point)
  points <- grad_desc(x0=point, list_of_points=points, alpha=alpha[i], steps=steps[i], i)
  
  # plot paths
  plot_points(list_of_points=points, color=colors[i])
  lines(points[,1], points[,2], pch=16, col=colors[i])
  
  # check if arrived at minimum
  print(paste0("Point: ", point))
  print(paste0("Gradient: ",gradient(ackley, points[nrow(points), ]))) 
  i <- i+1
  
  starting_point <- starting_point+1
}


legend(-1.5, -1.1, legend=c(paste0("starting point: ", starting_points[1,1],", ",starting_points[1,2]),
                            paste0("starting point: ", starting_points[2,1],", ",starting_points[2,2]),
                            paste0("starting point: ", starting_points[3,1],", ",starting_points[3,2]),
                            paste0("starting point: ", starting_points[4,1],", ",starting_points[4,2])),
         col=c("green", "red", "orange", "blue"), lty=1, cex=0.8)

dev.off()

######
png('../figure_man/nonconv_ackley_path.png', width = 480, height = 320)
i=1
plot(1, type="n", xlab="Steps", ylab="y", xlim=c(1,max(steps)), ylim=c(0,max(y_start)))
for (line in 1:ncol(y_values)) {
  lines(y_values[,i], pch=16, col=colors[i])
  i <- i+1
}
legend(33.7, 4.75, legend=c(paste0("alpha=", alpha[1], ", steps=",steps[1]),
                         paste0("alpha=", alpha[2], ", steps=",steps[2]),
                         paste0("alpha=", alpha[3], ", steps=",steps[3]),
                         paste0("alpha=", alpha[4], ", steps=",steps[4])),
       col=c("green", "red", "orange", "blue"), lty=1, cex=0.8)
dev.off()

