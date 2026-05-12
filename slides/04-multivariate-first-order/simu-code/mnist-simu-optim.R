# This script compares different (S)GD optimizer variants using a small
# fully-connected DNN with ReLu activations on a subset of the MNIST classification
# task for hand-written digits. The script produces and saves two plots showing
# the training trajectories of SGD and momentum, cosine decay and large LRs
################################################################################################
## I. set up workspace and define parameters
################################################################################################

# clean workspace
rm(list=ls())
library(devtools)

## install.packages("pacman")
pacman::p_load(Matrix, dplyr, keras, reticulate, tensorflow, 
               Metrics, tidyr, rstudioapi, ggplot2, gridExtra, ggpubr) 

# define timestamp for file names
timestamp <- format(Sys.time(), "%Y-%m-%d-%H%M")

# directories
main.path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(main.path)

# Create 'figure_man' directory
if (!dir.exists("../figure_man/simu_mnist/")) {
  dir.create("../figure_man/simu_mnist")
}

# force conda environment
use_condaenv("r-reticulate", required = T)
print(reticulate::py_config())

# model params
units1=128L
units2=64L

# Default fitting params (will be overridden in function calls)
alpha = 0.1
la = 1e-4
train_samples = 5000L
val_split = 0.2
init = initializer_he_normal()
verb=0

# seed
seed = 123

################################################################################################
## II. load data
################################################################################################

set.seed(seed)

# Load the MNIST dataset
mnist <- dataset_mnist()
train_images_full <- mnist$train$x
train_labels_full <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y

# Reduce to 10k samples for faster computation
subsample <- sample(length(train_labels_full), size = train_samples)

train_images <- train_images_full[subsample,,]
train_labels <- train_labels_full[subsample]

# Pre-process the dataset
# Normalize the data
train_images <- train_images / 255
test_images <- test_images / 255

# Convert labels to categorical (one-hot encoding)
train_labels <- to_categorical(train_labels, 10)
test_labels <- to_categorical(test_labels, 10)

################################################################################################
## III. Define training function
################################################################################################

train_mnist_model <- function(lr, alpha, epochs, batch_size, mom) {
  # Reset random seeds for reproducibility
  set.seed(seed)
  tf$random$set_seed(seed)
  
  # Define model
  inputs <- layer_input(shape = c(28, 28))
  pred <- layer_flatten()(inputs)
  pred <- layer_dense(units = units1, activation = 'relu', 
                      kernel_regularizer = regularizer_l2(la), 
                      bias_regularizer = regularizer_l2(la))(pred)
  pred <- layer_dense(units = units2, activation = 'relu',
                      kernel_regularizer = regularizer_l2(la), 
                      bias_regularizer = regularizer_l2(la))(pred)
  out <-  layer_dense(units = 10L, activation = 'softmax',
                      kernel_regularizer = regularizer_l2(0.5*la), 
                      bias_regularizer = regularizer_l2(0.5*la))(pred)
  
  model <- keras_model(inputs = inputs, outputs = out)
  
  # Cosine decay learning rate
  lr_cosine <- learning_rate_schedule_cosine_decay( 
    initial_learning_rate = lr, 
    decay_steps = ((1-val_split)*(length(train_labels))/batch_size)*epochs,
    alpha = alpha,
    name = "CosineDecay"
  ) 
  
  # Compile model
  model %>% compile(
    optimizer = optimizer_sgd(learning_rate = lr_cosine, momentum = mom),
    loss = 'categorical_crossentropy',
    metrics = c('accuracy')
  )
  
  # Record start time
  start_time <- Sys.time()
  
  # Train model
  history <- model %>% fit(
    train_images, train_labels,
    epochs = epochs, 
    batch_size = batch_size,
    verbose = 0,
    validation_split = val_split,
    view_metrics = FALSE
  )
  
  # Record end time and calculate total duration
  end_time <- Sys.time()
  total_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Calculate time per epoch
  time_vector <- seq(from = total_time/epochs, 
                     to = total_time, 
                     length.out = epochs)
  
  # Get test accuracy
  preds.full <- predict(model, test_images)
  preds.classes <- max.col(preds.full, "first")
  true.classes <- max.col(test_labels, "first")
  test_acc <- sum(preds.classes == true.classes) / length(true.classes)
  
  # Create metrics dataframe
  metrics_df <- data.frame(
    train_loss = history$metrics$loss,
    val_loss = history$metrics$val_loss,
    train_acc = history$metrics$accuracy,
    val_acc = history$metrics$val_accuracy,
    elapsed_time = time_vector,
    epoch = 1:epochs,
    batch_size = batch_size,
    learning_rate = lr,
    momentum = mom,
    alpha = alpha,
    test_accuracy = test_acc
  )
  
  return(metrics_df)
}

################################################################################################
## IV. Run experiments
################################################################################################

small_lr <- 0.01
large_lr <- 0.1
epochs <- 100L
batch_size <- 100L
epochs_gd <- round(train_samples/batch_size)*epochs
mom <- 0.8

# Run different configurations
results_a <- train_mnist_model(lr = small_lr, alpha = 1, epochs = epochs, batch_size = 100L, mom = 0)
results_b <- train_mnist_model(lr = small_lr, alpha = 1, epochs = epochs, batch_size = 100L, mom = mom)
results_c <- train_mnist_model(lr = small_lr, alpha = 0.01, epochs = epochs, batch_size = 100L, mom = 0)
results_d <- train_mnist_model(lr = small_lr, alpha = 0.01, epochs = epochs, batch_size = 100L, mom = mom)
results_e <- train_mnist_model(lr = large_lr, alpha = 0.01, epochs = epochs, batch_size = 100L, mom = mom)
results_f <- train_mnist_model(lr = large_lr, alpha = 1, epochs = epochs, batch_size = 100L, mom = mom)
#results_f <- train_mnist_model(lr = small_lr, alpha = 1, epochs = epochs_gd, batch_size = 5000L, mom = 0)

################################################################################################
## V. Create visualizations
################################################################################################

# Add method labels
results_a$Method <- "SGD"
results_b$Method <- "SGD+mom"
results_c$Method <- "SGD+cos"
results_d$Method <- "SGD+mom+cos"
results_e$Method <- "SGD+mom+cos+large steps"
results_f$Method <- "SGD+mom+large steps"

# Combine first four results for plotting
plot_data <- rbind(results_a, results_b, results_c, results_d, results_e, results_f)

# Create common theme elements
plot_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 16),
    legend.position = "bottom"
  )

# Create plots
p1 <- ggplot(plot_data, aes(x = epoch, y = train_loss, color = Method)) +
  geom_line(linewidth = 1.2) +
  #scale_x_log10() +
  labs(x = "Epochs", y = "Training loss", title = "Training loss vs epochs") +
  plot_theme

p2 <- ggplot(plot_data, aes(x = epoch, y = val_acc, color = Method)) +
  geom_line(linewidth = 1.2) +
  coord_cartesian(ylim = c(0.8, 0.96)) +
  labs(x = "Epochs", y = "Validation accuracy", title = "Validation accuracy vs epochs") +
  plot_theme

# Extract legend
legend <- get_legend(
  p1 + 
    theme(legend.title = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.key.size = unit(1.2, "cm"))
)

# Combine plots with modified legend
combined_plot <- ggarrange(p1 + theme(legend.position = "none"),
                           p2 + theme(legend.position = "none"),
                           ncol = 2,
                           legend.grob = legend,
                           legend = "bottom")

# Display the plot
print(combined_plot)

# Save plot
ggsave(filename = "../figure_man/simu_mnist/SGD_compar.png", 
       plot = combined_plot, width = 15, height = 6, dpi = 300)
ggsave(filename = "../figure_man/simu_mnist/SGD_compar.pdf", 
       plot = combined_plot, width = 15, height = 6, device = cairo_pdf)

################################################################################################
## VI. Compare GD and SGD in runtime with same number of updates/iterations
################################################################################################

small_lr <- 0.01
large_lr <- 0.1
epochs <- 100L
batch_size <- 100L
epochs_gd <- round(train_samples/batch_size)*epochs
mom <- 0.8

# Run GD and mini-batch SGD
#results_a <- train_mnist_model(lr = small_lr, alpha = 1, epochs = epochs, batch_size = 100L, mom = 0)
results_gd <- train_mnist_model(lr = small_lr, alpha = 1, epochs = epochs_gd, batch_size = 5000L, mom = 0)

# Combine GD and SGD results
results_sgd <- results_a
results_sgd$Method <- "Mini-batch SGD (5000 iterations)"
results_gd$Method <- "GD (5000 iterations)"
plot_data_gd <- rbind(results_sgd, results_gd)

# Create common theme elements
plot_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 16),
    legend.position = "bottom"
  )

# Create plots
p3 <- ggplot(plot_data_gd, aes(x = elapsed_time, y = train_loss, color = Method)) +
  geom_line(linewidth = 1.2) +
  #scale_x_log10() +
  labs(x = "Seconds", y = "Training loss", title = "Training loss") +
  plot_theme

p4 <- ggplot(plot_data_gd, aes(x = elapsed_time, y = val_acc, color = Method)) +
  geom_line(linewidth = 1.2) +
  coord_cartesian(ylim = c(0.4, 0.96)) +
  labs(x = "Seconds", y = "Validation accuracy", title = "Validation accuracy") +
  plot_theme

# Extract legend
legend <- get_legend(
  p3 + 
    theme(legend.title = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.key.size = unit(1.2, "cm"))
)

# Combine plots with modified legend
combined_plot2 <- ggarrange(p3 + theme(legend.position = "none"),
                           p4 + theme(legend.position = "none"),
                           ncol = 2,
                           legend.grob = legend,
                           legend = "bottom")

# Display the plot
print(combined_plot2)

# Save plot
ggsave(filename = "../figure_man/simu_mnist/SGD_GD_compar.png", 
       plot = combined_plot2, width = 15, height = 6, dpi = 300)
ggsave(filename = "../figure_man/simu_mnist/SGD_GD_compar.pdf", 
       plot = combined_plot2, width = 15, height = 6, device = cairo_pdf)
