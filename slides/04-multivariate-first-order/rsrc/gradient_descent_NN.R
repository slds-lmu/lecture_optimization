library(rgl)
library(ggplot2)
library(mlr3)
library(viridis)
library(plot3D)
library(data.table)

source("rsrc/NN_helpers.R")

set.seed(1234)

# ORIGINAL DATASET AND TASK 
f = function(x1, x2) x1^2 + x2^2 # + 2 * x1 * x2# (x1^2 + x2 -11)^2  + (x1 + x2^2 - 7)^2 

n <- 200
x1 <- runif(n, -5, 5)
x2 <- runif(n, -5, 5)
# Himmelblau's function
y <- f(x1, x2) + rnorm(n, 0, 1)
data <- data.frame(x1, x2, y)
data_table <- as.data.table(data)
data_task <- as_task_regr(data, target='y')


x1_scale = seq(-5, 5, 1)
x2_scale = seq(-5, 5, 1)
z = matrix(data=NA, nrow=length(x1_scale), ncol=length(x2_scale))

for(i in 1:length(x1_scale)) {
  for (j in 1:length(x2_scale)) {
    z[i,j] = f(x1_scale[i], x2_scale[j])
  }
}

# Plot original task 
pdf("figure_man/gradient_descent_NN_0.pdf", 5, 5, colormodel = "cmyk")
par(mfrow = c(1, 1))

persp3D(x = x1_scale, y = x2_scale, z = z, xlab = "x1", ylab = "x2", zlab = "y",
    expand = 0.5, d = 2, phi = 10, theta = -60, resfac = 2,
    image = FALSE, 
    colkey = list(side = 1, length = 0.5), shade = FALSE, alpha = 0.5)

scatter3D(x1, x2, y, col = "red", size = 30, add=TRUE, type = "h", bty = "b2", pch = 16)

dev.off()

## WITH VS WITHOUT MOMENTUM 
mlr3keras_set_seeds(1111)

for (mom in c(0, 0.5)) {
  print(mom)
  train_model(data_task, epochs = 10, momentum=mom)
  train_model(data_task, epochs = 100, momentum=mom)
  history = train_model(data_task, epochs = 300, momentum=mom)
  
  df = data.frame(loss = history$metrics$loss)
  df$epoch = 1:nrow(df)
  p = ggplot(data = df, aes(x = epoch, y = loss)) + geom_line()
  p = p + ylim(c(0, 130)) 
  p = p + annotate("text",label=paste0("Momentum: ",mom), x=50, y=10)
  print(p)
  ggsave(filename = paste0("figure_man/gradient_descent_NN_300_history_",mom,".pdf"), p, width = 4, height = 3)
}


## SGD VS WITHOUT SGD 
mlr3keras_set_seeds(1111)

epochs = 100

out = lapply(c(1, 1 / 200, 0.1, 0.5), function(bsf) {
  history = train_model(data_task, epochs = epochs, bs_fraction = bsf, lr = 0.001)
  cbind(data.frame(loss = history$metrics$loss), bs_fraction = bsf, epoch = 1:epochs)
})

df = do.call(rbind, out)
df$bs_fraction = as.factor(df$bs_fraction)

p = ggplot(data = df, aes(x = epoch, y = loss, colour = bs_fraction)) + geom_line()
# p = p + ylim(c(0, 130)) 
p = p + theme_bw() + ggtitle("SGD with different batch sizes")
p
ggsave(filename = "figure_man/gradient_descent_NN_SGD_vs_no_SGD.pdf", width = 5, height = 3)

## DIFFERENT OPTIMIZERS 
mlr3keras_set_seeds(1111)

epochs = 50
bsf = 1 / 200 # batch size 

out = lapply(c("optimizer_sgd", "optimizer_adam", "optimizer_rmsprop"), function(opts) {
  history = train_model(data_task, epochs = epochs, optimizer = opts, bs_fraction = bsf, lr = 0.005)
  cbind(data.frame(loss = history$metrics$loss), bs_fraction = bsf, epoch = 1:epochs, optimizer = opts)
})

df = do.call(rbind, out)
df$bs_fraction = as.factor(df$bs_fraction)

p = ggplot(data = df, aes(x = log(epoch), y = loss, colour = optimizer)) + geom_line()
# p = p + ylim(c(0, 130)) 
p = p + theme_bw() + ggtitle("Different optimizers")
p = p + xlim(c(0, 50)) 
p
ggsave(filename = "figure_man/gradient_descent_NN_ADAM_SGD_RMS_ADAGRAD.pdf", width = 5, height = 3)

