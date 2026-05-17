# Used in: slides-multivar-first-order-12-comparison.tex
#
# Trains a small MNIST network under several SGD variants and saves the
# optimizer-comparison plots used in the benchmark slides.

set.seed(123L)

library(data.table)
library(ggplot2)
library(keras)
library(patchwork)
library(reticulate)
library(tensorflow)

out_dir = "../figure/simu-mnist"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

use_condaenv("r-reticulate", required = TRUE)

units1 = 128L
units2 = 64L
l2_penalty = 1e-4
train_samples = 5000L
validation_split = 0.2
seed = 123L

set.seed(seed)
mnist = dataset_mnist()
train_images_full = mnist$train$x
train_labels_full = mnist$train$y
test_images = mnist$test$x / 255
test_labels = to_categorical(mnist$test$y, 10)

subsample = sample(length(train_labels_full), size = train_samples)
train_images = train_images_full[subsample, , ] / 255
train_labels = to_categorical(train_labels_full[subsample], 10)

train_mnist_model = function(learning_rate, alpha, epochs, batch_size, momentum) {
  set.seed(seed)
  tf$random$set_seed(seed)

  inputs = layer_input(shape = c(28, 28))
  hidden = layer_flatten(inputs)
  hidden = layer_dense(
    hidden,
    units = units1,
    activation = "relu",
    kernel_regularizer = regularizer_l2(l2_penalty),
    bias_regularizer = regularizer_l2(l2_penalty)
  )
  hidden = layer_dense(
    hidden,
    units = units2,
    activation = "relu",
    kernel_regularizer = regularizer_l2(l2_penalty),
    bias_regularizer = regularizer_l2(l2_penalty)
  )
  outputs = layer_dense(
    hidden,
    units = 10L,
    activation = "softmax",
    kernel_regularizer = regularizer_l2(0.5 * l2_penalty),
    bias_regularizer = regularizer_l2(0.5 * l2_penalty)
  )

  model = keras_model(inputs = inputs, outputs = outputs)

  n_epochs = as.integer(epochs)
  batch_size = as.integer(batch_size)
  n_train = nrow(train_labels)

  lr_schedule = learning_rate_schedule_cosine_decay(
    initial_learning_rate = learning_rate,
    decay_steps = ((1 - validation_split) * n_train / batch_size) * n_epochs,
    alpha = alpha,
    name = "CosineDecay"
  )

  model$compile(
    optimizer = optimizer_sgd(learning_rate = lr_schedule, momentum = momentum),
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )

  start_time = Sys.time()
  history = model$fit(
    train_images,
    train_labels,
    epochs = n_epochs,
    batch_size = batch_size,
    verbose = 0,
    validation_split = validation_split,
    view_metrics = FALSE
  )
  total_time = as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  predictions = model$predict(test_images, verbose = 0)
  predicted_classes = max.col(predictions, "first")
  true_classes = max.col(test_labels, "first")
  test_accuracy = mean(predicted_classes == true_classes)

  data.table(
    train_loss = history$metrics$loss,
    val_loss = history$metrics$val_loss,
    train_acc = history$metrics$accuracy,
    val_acc = history$metrics$val_accuracy,
    elapsed_time = seq(total_time / n_epochs, total_time, length.out = n_epochs),
    epoch = seq_len(n_epochs),
    batch_size = batch_size,
    learning_rate = learning_rate,
    momentum = momentum,
    alpha = alpha,
    test_accuracy = test_accuracy
  )
}

build_plot_theme = function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 20, hjust = 0.5),
      axis.title = element_text(size = 17),
      axis.text = element_text(size = 16),
      legend.position = "bottom"
    )
}

comparison_runs = list(
  "SGD" = list(learning_rate = 0.01, alpha = 1, momentum = 0),
  "SGD+mom" = list(learning_rate = 0.01, alpha = 1, momentum = 0.8),
  "SGD+cos" = list(learning_rate = 0.01, alpha = 0.01, momentum = 0),
  "SGD+mom+cos" = list(learning_rate = 0.01, alpha = 0.01, momentum = 0.8),
  "SGD+mom+cos+large steps" = list(learning_rate = 0.1, alpha = 0.01, momentum = 0.8),
  "SGD+mom+large steps" = list(learning_rate = 0.1, alpha = 1, momentum = 0.8)
)

epochs = 100L
batch_size = 100L

comparison_results = lapply(names(comparison_runs), function(method_name) {
  cfg = comparison_runs[[method_name]]
  result = train_mnist_model(
    learning_rate = cfg$learning_rate,
    alpha = cfg$alpha,
    epochs = epochs,
    batch_size = batch_size,
    momentum = cfg$momentum
  )
  result[, method := method_name]
  result
})

comparison_data = rbindlist(comparison_results, use.names = TRUE)
plot_theme = build_plot_theme()

train_loss_plot = ggplot(comparison_data, aes(x = epoch, y = train_loss, colour = method)) +
  geom_line(linewidth = 1.2) +
  labs(x = "Epoch", y = "Training loss", title = "Training loss vs epoch") +
  plot_theme

validation_accuracy_plot = ggplot(comparison_data, aes(x = epoch, y = val_acc, colour = method)) +
  geom_line(linewidth = 1.2) +
  coord_cartesian(ylim = c(0.8, 0.96)) +
  labs(x = "Epoch", y = "Validation accuracy", title = "Validation accuracy vs epoch") +
  plot_theme

comparison_plot = (train_loss_plot + validation_accuracy_plot) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  )

if (interactive()) {
  print(comparison_plot)
}

ggsave(
  filename = file.path(out_dir, "SGD_compar.pdf"),
  plot = comparison_plot,
  width = 15,
  height = 6,
  device = cairo_pdf
)

epochs_gd = as.integer(round(train_samples / batch_size) * epochs)
gd_result = train_mnist_model(
  learning_rate = 0.01,
  alpha = 1,
  epochs = epochs_gd,
  batch_size = train_samples,
  momentum = 0
)
gd_result[, method := "GD (5000 iterations)"]

sgd_runtime_result = copy(comparison_results[[1L]])
sgd_runtime_result[, method := "Mini-batch SGD (5000 iterations)"]

runtime_data = rbindlist(list(sgd_runtime_result, gd_result), use.names = TRUE)
runtime_plot_theme = build_plot_theme()

runtime_loss_plot = ggplot(runtime_data, aes(x = elapsed_time, y = train_loss, colour = method)) +
  geom_line(linewidth = 1.2) +
  labs(x = "Seconds", y = "Training loss", title = "Training loss") +
  runtime_plot_theme

runtime_accuracy_plot = ggplot(runtime_data, aes(x = elapsed_time, y = val_acc, colour = method)) +
  geom_line(linewidth = 1.2) +
  coord_cartesian(ylim = c(0.4, 0.96)) +
  labs(x = "Seconds", y = "Validation accuracy", title = "Validation accuracy") +
  runtime_plot_theme

runtime_comparison_plot = (runtime_loss_plot + runtime_accuracy_plot) +
  plot_layout(ncol = 2, guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  )

if (interactive()) {
  print(runtime_comparison_plot)
}

ggsave(
  filename = file.path(out_dir, "SGD_GD_compar.pdf"),
  plot = runtime_comparison_plot,
  width = 15,
  height = 6,
  device = cairo_pdf
)
