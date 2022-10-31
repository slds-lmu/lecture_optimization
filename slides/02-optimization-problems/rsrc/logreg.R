# Optimization WiSe 22/23
# Chapter 2.3 Other optimization problems
# Plot Logistic Regression with and without Regularization

set.seed(314)

library(mlr3)
library(glmnet)
library(data.table)
library(ggplot2)
library(ggnewscale)
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

# Fit LogReg with and without regularization
mod_reg <- glmnet(mydf, df$y, family = "binomial", alpha = 1, nlambda=20)
mod <- glm(y ~ x1 + x2,  family = binomial(link = "logit"), data = mydf)

### get coefficients
theta_best <- transpose(as.data.frame(mod$coefficients))
theta_reg <- coef(mod_reg)
theta_reg <- data.frame(x1 = theta_reg["x1",], x2 = theta_reg["x2",])
theta_reg$lambda <- mod_reg$lambda
#append theta_best to theta_reg DF
theta_reg[nrow(theta_reg)+1,] = c(theta_best[2], theta_best[3], 0)

### Helper grid for Loss calculation
b1 <- seq(-1,2.5,length.out = 100)
b2 <- seq(-1,3.5,length.out = 100)
eval_grid <- expand.grid(b1, b2)

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

# eval_grid$r_emp <- apply(eval_grid, 1, function(beta) sum((as.matrix(mydf[, c("x1", "x2")]) %*% beta - mydf$y)^2))
eval_grid$r_emp <- apply(eval_grid, 1, function(theta) calc_log_loss(theta, mydf, df$y))
eval_grid


#complete plot: Remp landscape, different lambdas
p = ggplot() +
  
  #Remp
  new_scale_color() +
  geom_raster(data = eval_grid, aes(x=Var1, y=Var2, fill=log(r_emp), color=r_emp)) + 
  scale_color_gradient(low="green", high="blue") +
  geom_contour(data = eval_grid, aes(x=Var1, y=Var2, z=log(r_emp)), color="white") +
  
  #Lambda
  new_scale_color() +
  geom_point(data = theta_reg, aes(x=x1, y=x2, color = lambda), size = 2) +
  scale_color_gradient(low="yellow", high="red") +
  #geom_path(data = theta_reg, aes(x=x1, y=x2), color = "yellow", size=0.5) +
  
  annotate(geom="text", x=theta_reg[nrow(theta_reg),"x1"]+0.15, y=theta_reg[nrow(theta_reg),"x2"], label=expression(hat(theta)), color="yellow") +

  theme_bw() + xlab(expression(theta[1])) + ylab(expression(theta[2])) +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE) +
  ggtitle(expression(paste("Unregularized ", R[emp], ", ", alpha, " = 1"))) 

ggsave("figure_man/logreg-0.pdf", p, width = 5, height = 4)

#plots for various specific lambdas
lambdas=c(0.5, 1, 5)

for (lam in lambdas) {
  b1 <- seq(-1,2.5,length.out = 100)
  b2 <- seq(-1,3.5,length.out = 100)
  eval_grid <- expand.grid(b1, b2)
  mod_reg_lambda <- glmnet(mydf, df$y, family = "binomial", alpha = 1, lambda= lam / 10, thresh=1e-20)
  eval_grid$r_emp_lambda <- apply(eval_grid, 1, function(theta) calc_log_loss(theta, mydf, df$y, lambda=lam, alpha = 1))
  
  print(coef(mod_reg_lambda))
  print(lam)
  
  theta_reg_lam <- data.frame(x1 = coef(mod_reg_lambda)[2], x2 = coef(mod_reg_lambda)[3])

  p = ggplot() + new_scale_color() +
  geom_raster(data = eval_grid, aes(x=Var1, y=Var2, fill=log(r_emp_lambda))) + 
  # scale_fill_gradientn(colours=c("green","blue","red","black")) +
  geom_contour(data = eval_grid, aes(x=Var1, y=Var2, z=log(r_emp_lambda)), colour="white", show.legend = TRUE) +
  geom_point(data=theta_reg_lam, aes(x=x1, y=x2), size=2, color="yellow") + 
  xlab(expression(theta[1])) + ylab(expression(theta[2])) +
  ggtitle(bquote("Reg. risk with  " ~ lambda ~ " = " ~ .(lam) ~ ", " ~ alpha ~ " = 1")) + 
  theme_bw() + 
  theme(legend.position="none") 
  ggsave(paste0("figure_man/logreg-", lam, "-1.pdf"), p, width = 3, height = 3)
}

for (lam in lambdas) {
  b1 <- seq(-1,2.5,length.out = 100)
  b2 <- seq(-1,3.5,length.out = 100)
  eval_grid <- expand.grid(b1, b2)
  mod_reg_lambda <- glmnet(mydf, df$y, family = "binomial", alpha = 0, lambda= lam / 50, thresh=1e-20)
  eval_grid$r_emp_lambda <- apply(eval_grid, 1, function(theta) calc_log_loss(theta, mydf, df$y, lambda=lam, alpha = 0))
  
  print(coef(mod_reg_lambda))
  print(lam)
  
  theta_reg_lam <- data.frame(x1 = coef(mod_reg_lambda)[2], x2 = coef(mod_reg_lambda)[3])

  p = ggplot() + new_scale_color() +
  geom_raster(data = eval_grid, aes(x=Var1, y=Var2, fill=log(r_emp_lambda))) + 
  # scale_fill_gradientn(colours=c("green","blue","red","black")) +
  geom_contour(data = eval_grid, aes(x=Var1, y=Var2, z=log(r_emp_lambda)), colour="white", show.legend = TRUE) +
  geom_point(data=theta_reg_lam, aes(x=x1, y=x2), size=2, color="yellow") + 
  xlab(expression(theta[1])) + ylab(expression(theta[2])) +
  ggtitle(bquote("Reg. risk with  " ~ lambda ~ " = " ~ .(lam) ~ ", " ~ alpha ~ " = 0")) + 
  theme_bw() + 
  theme(legend.position="none") 
  ggsave(paste0("figure_man/logreg-", lam, "-0.pdf"), p, width = 3, height = 3)
}