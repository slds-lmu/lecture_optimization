# ------------------------------------------------------------------------------
# multivariate first order

# FUNC: 
#   builds and trains a neural network using keras for regression tasks, 
#   leveraging stochastic gradient descent (SGD) and other optimizers, 
#   also includes 3D surface visualization of predictions.
# ------------------------------------------------------------------------------

library(keras3)
library(tensorflow)

# ------------------------------------------------------------------------------

set_nn_seed = function(seed) {
  set.seed(as.integer(seed))
  tensorflow::set_random_seed(as.integer(seed))
}

build_optimizer = function(optimizer, momentum, lr) {
  if (optimizer == "optimizer_sgd") {
    return(optimizer_sgd(learning_rate = lr, momentum = momentum))
  }

  if (!exists(optimizer, mode = "function")) {
    stop(sprintf("Unknown optimizer: %s", optimizer))
  }

  get(optimizer)(learning_rate = lr)
}

build_model = function(optimizer, momentum, lr) {
  model = keras_model_sequential(
    input_shape = c(2L),
    layers = list(
      layer_dense(units = 2L),
      layer_dense(units = 2L, activation = "sigmoid"),
      layer_dense(units = 2L, activation = "linear"),
      layer_dense(units = 1L)
    )
  )

  model$compile(
    loss = "mse",
    optimizer = build_optimizer(optimizer = optimizer, momentum = momentum, lr = lr),
    metrics = list("mean_absolute_error")
  )

  model
}

history_loss = function(history) {
  unlist(history$history$loss)
}

train_model = function(task, task_predict = NULL, epochs, optimizer = "optimizer_sgd",
                       momentum = 0, plot_surface = FALSE, bs_fraction = 1,
                       lr = 0.01) {
  task_data = as.data.frame(task$data())
  x_train = as.matrix(task_data[, task$feature_names, drop = FALSE])
  y_train = as.numeric(task_data[[task$target_names]])

  x_center = colMeans(x_train)
  x_scale = apply(x_train, 2, stats::sd)
  x_scale[x_scale == 0] = 1

  x_train_scaled = op_array(
    unclass(scale(x_train, center = x_center, scale = x_scale)),
    dtype = "float32"
  )
  y_train = op_array(y_train, dtype = "float32")
  batch_size = as.integer(max(1L, round(task$nrow * bs_fraction)))

  model = build_model(optimizer = optimizer, lr = lr, momentum = momentum)
  history = model$fit(
    x = x_train_scaled,
    y = y_train,
    epochs = as.integer(epochs),
    batch_size = batch_size,
    validation_split = 0,
    verbose = 0
  )

  prediction_grid = expand.grid(x1 = x1_scale, x2 = x2_scale)
  prediction_grid_scaled = op_array(
    unclass(scale(
      as.matrix(prediction_grid),
      center = x_center,
      scale = x_scale
    )),
    dtype = "float32"
  )
  z = matrix(
    as.numeric(model$predict(prediction_grid_scaled, verbose = 0)),
    nrow = length(x1_scale),
    ncol = length(x2_scale)
  )

  if (plot_surface) {
    pdf(paste0("../figure/gradient_descent_NN_", epochs, "_surface_", momentum,".pdf"), 5, 5, colormodel = "cmyk")

    persp3D(x = x1_scale, y = x2_scale, z = z, xlab = "x1", ylab = "x2", zlab = "y",
        expand = 0.5, d = 2, phi = 10, theta = -60, resfac = 2,
        image = FALSE, 
        colkey = list(side = 1, length = 0.5), shade = FALSE, alpha = 0.5)

    # scatter3D(x1, x2, y, col = "red", size = 30, add=TRUE, type = "h", bty = "b2", pch = 16)

    dev.off()  
  }

  return(history)
}
