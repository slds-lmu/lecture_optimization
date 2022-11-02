# Optimization WiSe 22/23
# Chapter 2.1 Other optimization problems
# Plot Logistic Regression with and without Regularization

# TASK 
set.seed(314)

library(mlr3)
library(glmnet)
library(data.table)
library(ggplot2)
library(ggnewscale)

### Calculate Bernoulli / Log Loss
calc_log_loss <- function(theta, X, y, lambda=0, alpha = 1){
  #Log Loss
  #pi <- 1/(1 + exp(-as.matrix(X) %*% theta))
  #loss <- sum(-y*log(pi) - (1-y)*log(1-pi))
  
  #Bernoulli Loss
  f <- as.matrix(X) %*% theta
  bernoulli <- (1/(2*length(y)))*sum(-y*f + log(1+exp(f))) + alpha * lambda * sum(abs(theta)) + (1 - alpha) / 2 * lambda * sum(theta^2)
  return(bernoulli)
}

### Helper grid for Loss calculation
b1 <- seq(-1,2.5,length.out = 100)
b2 <- seq(-1,3.5,length.out = 100)
grid <- expand.grid(b1, b2)


n <- 1000

### Generate data
mydf <- data.frame(
  x1 = rnorm(n, -5, 5),
  x2 = rnorm(n, -10, 10)
)
z <- 2*mydf$x1 + 3*mydf$x2
pr <- 1/(1+exp(-z))
y <- rbinom(n,1,pr)
df <- mydf
df$y <- y

plot_elasticnet = function(data, grid, alpha, lambda) {

  # Fit LogReg with and without regularization
  theta_reg = optim(c(0, 0), function(x) calc_log_loss(x, df[, c(1, 2)], df$y, lambda, alpha))
  theta_reg = data.frame(x1 = theta_reg$par[1], x2 = theta_reg$par[2])

  grid$r_emp <- apply(grid, 1, function(theta) calc_log_loss(theta, df[, c(1, 2)], df$y, lambda, alpha))

  p = ggplot() +
    
    #Remp
    new_scale_color() +
    geom_raster(data = grid, aes(x=Var1, y=Var2, fill=log(r_emp), color=r_emp)) + 
    scale_color_gradient(low="green", high="blue") +
    geom_contour(data = grid, aes(x=Var1, y=Var2, z=log(r_emp)), color="white") +
    
    #Lambda
    new_scale_color() +
    geom_point(data = theta_reg, aes(x=x1, y=x2), color = "yellow", size = 2) +    
    annotate(geom="text", x=theta_reg[,"x1"]+0.15, y=theta_reg[,"x2"], label=expression(hat(theta)), color="yellow") +

    theme_bw() + xlab(expression(theta[1])) + ylab(expression(theta[2])) +
    theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE) 

  return(p)

}

p = plot_elasticnet(df, grid, 1, 0)
p = p + ggtitle(expression(paste("Unregularized ", R[emp]))) 

ggsave(paste0("figure_man/logreg-0.pdf"), p, width = 4, height = 3)

for (alpha in c(0, 0.5, 1)) {
  for (lambda in c(0.1, 1, 5)) {

    p = plot_elasticnet(df, grid, alpha, lambda)
    p = p + ggtitle(bquote("Reg. risk with  " ~ lambda ~ " = " ~ .(lambda) ~ ", " ~ alpha ~ " = " ~ .(alpha))) 
    p = p + theme(legend.position="none")

    ggsave(paste0("figure_man/logreg-", alpha, "-", lambda, ".pdf"), p, width = 4, height = 4)

  } 
}
