library(keras)
library(mlr3keras)
library(mlr3pipelines)

build_model = function(optimizer, momentum, lr) {     
  model = keras_model_sequential() %>%
      layer_dense(units = 2L, input_shape = 2L) %>%
      layer_dense(units = 2L, activation = "sigmoid") %>%
      layer_dense(units = 2L, activation = "linear") %>%
      layer_dense(units = 1L)
  
  if (optimizer == "optimizer_sgd") {
    model %>% compile(
    loss = "mse",
    optimizer = get(optimizer)(momentum = momentum, lr = lr),
    metrics = list("mean_absolute_error"))
  } else {
    model %>% compile(
    loss = "mse",
    optimizer = get(optimizer)(lr = lr),
    metrics = list("mean_absolute_error"))    
  }
}

train_model = function(task, task_predict = NULL, epochs, optimizer = "optimizer_sgd", momentum = 0, plot_surface = FALSE, bs_fraction = 1, lr = 0.01) {
  
  po_scale = PipeOpScale$new()

  learner = lrn("regr.keras")  ### statt epochs for-loop -> Plot nach Index
  
  learner$param_set$values$epochs = epochs
  learner$param_set$values$model = build_model(optimizer = optimizer, lr = lr, momentum = momentum)
  learner$param_set$values$validation_split = 0
  learner$param_set$values$batch_size = task$nrow * bs_fraction
  
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

  if (plot_surface) {
    pdf(paste0("figure_man/gradient_descent_NN_", epochs, "_surface_", momentum,".pdf"), 5, 5, colormodel = "cmyk")

    persp3D(x = x1_scale, y = x2_scale, z = z, xlab = "x1", ylab = "x2", zlab = "y",
        expand = 0.5, d = 2, phi = 10, theta = -60, resfac = 2,
        image = FALSE, 
        colkey = list(side = 1, length = 0.5), shade = FALSE, alpha = 0.5)

    # scatter3D(x1, x2, y, col = "red", size = 30, add=TRUE, type = "h", bty = "b2", pch = 16)

    dev.off()  
  }

  return(history)

}