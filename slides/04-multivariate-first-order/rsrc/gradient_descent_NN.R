library(rgl)
library(ggplot2)
library(mlr3)
library(keras)
library(mlr3keras)
library(mlr3pipelines)
library(viridis)
library(plot3D)
library(data.table)

set.seed(1234)

f = function(x1, x2) x1^2 + x2^2 # + 2 * x1 * x2# (x1^2 + x2 -11)^2  + (x1 + x2^2 - 7)^2 

# ORIGINAL DATASET AND TASK 
n <- 200
x1 <- runif(n, -5, 5)
x2 <- runif(n, -5, 5)
# Himmelblau's function
y <- f(x1, x2) + rnorm(n, 0, 1)
data <- data.frame(x1, x2, y)
data_table <- as.data.table(data)
data_task <- as_task_regr(data, target='y')

# Plot original task 
pdf("figure_man/gradient_descent_NN_0.pdf", 5, 5, colormodel = "cmyk")
par(mfrow = c(1, 1))

x1_scale = seq(-5, 5, 1)
x2_scale = seq(-5, 5, 1)
z = matrix(data=NA, nrow=length(x1_scale), ncol=length(x2_scale))

for(i in 1:length(x1_scale)) {
  for (j in 1:length(x2_scale)) {
    z[i,j] = f(x1_scale[i], x2_scale[j])
  }
}

persp3D(x = x1_scale, y = x2_scale, z = z, xlab = "x1", ylab = "x2", zlab = "y",
    expand = 0.5, d = 2, phi = 10, theta = -60, resfac = 2,
    image = FALSE, 
    colkey = list(side = 1, length = 0.5), shade = FALSE, alpha = 0.5)

scatter3D(x1, x2, y, col = "red", size = 30, add=TRUE, type = "h", bty = "b2", pch = 16)

dev.off()

# Build model 

build_model = function(momentum) {     
  model = keras_model_sequential() %>%
      layer_dense(units = 2L, input_shape = 2L) %>%
      layer_dense(units = 2L, activation = "sigmoid") %>%
      layer_dense(units = 2L, activation = "linear") %>%
      layer_dense(units = 1L)
    
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_sgd(momentum = momentum),
    metrics = list("mean_absolute_error"))
}

train_model = function(task, task_predict = NULL, epochs, epochs_max = NULL, momentum) {
  
  if (is.null(epochs_max)) {
    epochs_max = epochs
  }

  po_scale = PipeOpScale$new()

  learner = lrn("regr.keras")  ### statt epochs for-loop -> Plot nach Index
  
  learner$param_set$values$epochs = epochs
  learner$param_set$values$model = build_model(momentum)
  learner$param_set$values$validation_split = 0
  learner$param_set$values$batch_size = task$nrow
  
  keras_500 = GraphLearner$new(po_scale %>>% learner)

  keras_500$train(task)
  
  history = keras_500$model$regr.keras$model$history

  z = matrix(data=NA, nrow=length(x1_scale), ncol=length(x2_scale))

  for(i in 1:length(x1_scale)) {
    for (j in 1:length(x2_scale)) {
      task_predict = data.table(x1 = x1_scale[i], x2 = x2_scale[j])
      z[i,j] = keras_500$predict_newdata(task_predict)$data$response
    }
  }


  pdf(paste0("figure_man/gradient_descent_NN_", epochs, "_surface_", momentum,".pdf"), 5, 5, colormodel = "cmyk")

  persp3D(x = x1_scale, y = x2_scale, z = z, xlab = "x1", ylab = "x2", zlab = "y",
      expand = 0.5, d = 2, phi = 10, theta = -60, resfac = 2,
      image = FALSE, 
      colkey = list(side = 1, length = 0.5), shade = FALSE, alpha = 0.5)

  # scatter3D(x1, x2, y, col = "red", size = 30, add=TRUE, type = "h", bty = "b2", pch = 16)

  dev.off()

  return(history)

}

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
