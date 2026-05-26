# Used in: rsrc/gradient_descent_NN.R
#
# Provides helper functions for the small neural-network regression examples.

library(keras3)
library(tensorflow)

set_nn_seed = function(seed) {
  set.seed(as.integer(seed))
  set_random_seed(as.integer(seed))
}

build_model = function(momentum = 0, learning_rate = 0.01) {
  model = keras_model_sequential(
    input_shape = c(2L),
    layers = list(
      layer_dense(units = 16L, activation = "relu"),
      layer_dense(units = 16L, activation = "relu"),
      layer_dense(units = 1L)
    )
  )

  model$compile(
    loss = "mse",
    optimizer = optimizer_sgd(learning_rate = learning_rate, momentum = momentum),
    metrics = list("mean_absolute_error")
  )

  model
}

history_loss = function(history) {
  unlist(history$history$loss)
}

train_model = function(
  task,
  epochs,
  x1_scale,
  x2_scale,
  momentum = 0,
  bs_fraction = 1,
  learning_rate = 0.01
) {
  task_data = as.data.frame(task$data())
  x_train = as.matrix(task_data[, task$feature_names, drop = FALSE])
  y_train = as.numeric(task_data[[task$target_names]])

  x_center = colMeans(x_train)
  x_scale_vec = apply(x_train, 2, sd)
  x_scale_vec[x_scale_vec == 0] = 1

  x_train_scaled = op_array(
    unclass(scale(x_train, center = x_center, scale = x_scale_vec)),
    dtype = "float32"
  )

  y_train_array = op_array(y_train, dtype = "float32")
  batch_size = as.integer(max(1L, round(task$nrow * bs_fraction)))

  model = build_model(
    learning_rate = learning_rate,
    momentum = momentum
  )

  history = model$fit(
    x = x_train_scaled,
    y = y_train_array,
    epochs = as.integer(epochs),
    batch_size = batch_size,
    validation_split = 0,
    verbose = 0
  )

  prediction_grid = expand.grid(x1 = x1_scale, x2 = x2_scale)
  prediction_grid_scaled = op_array(
    unclass(scale(as.matrix(prediction_grid), center = x_center, scale = x_scale_vec)),
    dtype = "float32"
  )

  surface_matrix = matrix(
    as.numeric(model$predict(prediction_grid_scaled, verbose = 0)),
    nrow = length(x1_scale),
    ncol = length(x2_scale)
  )

  list(history = history, surface_matrix = surface_matrix)
}
