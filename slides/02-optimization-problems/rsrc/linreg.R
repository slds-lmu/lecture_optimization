# Optimization WiSe 22/23
# Chapter 2.1 Other optimization problems
# Plot Logistic Regression with and without Regularization

# TASK 
set.seed(314)

library(data.table)
library(ggplot2)
library(ggnewscale)
library(ggpubr)

### Calculate Bernoulli / Log Loss
calc_l2_loss <- function(theta, X, y, lambda=0, alpha = 1){
  #Log Loss
  #pi <- 1/(1 + exp(-as.matrix(X) %*% theta))
  #loss <- sum(-y*log(pi) - (1-y)*log(1-pi))
  
  #Bernoulli Loss
  f <- as.matrix(X) %*% theta
  l2 <- 1 / length(y) * sum((f - y)^2) + alpha * lambda * sum(abs(theta)) + (1 - alpha) / 2 * lambda * sum(theta^2)
  return(l2)
}

### Helper grid for Loss calculation
b1 <- seq(-1,2.5,length.out = 100)
b2 <- seq(-1,3.5,length.out = 100)
grid <- expand.grid(b1, b2)


n <- 100

### Generate data
mydf <- data.frame(
  x1 = rnorm(n, -5, 5),
  x2 = rnorm(n, -10, 10)
)
y <- 2*mydf$x1 + 3*mydf$x2 + rnorm(n, sd = 0.5)
df <- mydf
df$y <- y

plot_elasticnet = function(data, grid, alpha, lambda) {

  # Fit LogReg with and without regularization
  theta_reg = optim(c(0, 0), function(x) calc_l2_loss(x, df[, c(1, 2)], df$y, lambda, alpha))
  theta_reg = data.frame(x1 = theta_reg$par[1], x2 = theta_reg$par[2])

  grid$r_emp <- apply(grid, 1, function(theta) calc_l2_loss(theta, df[, c(1, 2)], df$y, lambda, alpha))
  grid$r_emp <- log(grid$r_emp)

  p = ggplot() +
    
    #Remp
    new_scale_color() +
    geom_raster(data = grid, aes(x=Var1, y=Var2, fill=r_emp, color=r_emp)) + 
    scale_color_gradient(low="green", high="blue") +
    geom_contour(data = grid, aes(x=Var1, y=Var2, z=r_emp), color="white") +
    
    #Lambda
    new_scale_color() +
    geom_point(data = theta_reg, aes(x=x1, y=x2), color = "yellow", size = 2) +    
    annotate(geom="text", x=theta_reg[,"x1"]+0.15, y=theta_reg[,"x2"], label=expression(hat(theta)), color="yellow") +

    theme_bw() + xlab(expression(theta[1])) + ylab(expression(theta[2])) +
    theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE) 

  return(p)

}

p1 = plot_elasticnet(df, grid, 0, 0)
p1 = p1 + ggtitle(expression(paste("Unregularized ", R[emp]))) 

p2 = plot_elasticnet(df, grid, lambda = 200, alpha = 1)
p2 = p2 + ggtitle(expression(paste("Lasso"))) 

p3 = plot_elasticnet(df, grid, lambda = 200, alpha = 0)
p3 = p3 + ggtitle(expression(paste("Ridge"))) 

p = ggarrange(p1, p2, p3, ncol=3, nrow=1, common.legend = TRUE, legend="right")

ggsave(paste0("figure_man/linreg.pdf"), p, width = 15, height = 4)
