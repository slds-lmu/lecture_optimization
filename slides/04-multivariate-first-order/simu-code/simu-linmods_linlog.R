# This script contains functions for generating synthetic data for linear and logistic regression models,
# implementing optimization algorithms with various enhancements, and plotting the results.
# The focus is on comparing different optimization methods and observing their effects on model training
# under varying conditions, such as different condition numbers of the feature matrix.

# Load required libraries
library(MASS)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(grid)
#library(cowplot)
#library(rstudioapi)
library(reshape2)

set.seed(123)

# Set working directory to the location of the current file
#current_file <- rstudioapi::getActiveDocumentContext()$path
#if (current_file == "") {
#  stop("Please save the script before running.")
#}
#setwd(dirname(current_file))

# Create 'figure_man' directory
if (!dir.exists("../figure_man/simu_linmod/")) {
  dir.create("../figure_man/simu_linmod")
}


################################################################################
################################################################################

source(file.path("./simu-linmods-functions.R"))

################################################################################
################################################################################
# ### GD + small LR setting [ linear ]
# Data generation parameters
set.seed(123)
model_type <- "linear"
n <- 500
p <- 11
rho <- 0.0
sigma <- 1
n_test <- 5000

# Optimization parameters
learning_rate <- 2e-4
max_iter <- 10000  
momentum_value <- 0.5
decay <- 0.05
stochastic <- FALSE

# Run GD variants
results <- run_all_experiments(
  n = n, p = p, rho = rho, sigma = sigma, 
  model_type = model_type, n_test = n_test,
  learning_rate = learning_rate, 
  max_iter = max_iter,
  momentum_value = momentum_value, 
  decay = decay,
  seed = 123, 
  stochastic=stochastic
)

################################################################################
# Generate plot with iterations on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "iterations",
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/GD_reg_small_lr_iters.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_reg_small_lr_iters.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate plot with elaspsed time on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "time",
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/GD_reg_small_lr_time.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_reg_small_lr_time.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save the coefficient paths plot
ggsave(filename = "./figure_man/simu_linmod/GD_reg_coef_small.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_reg_coef_small.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)


################################################################################
################################################################################
# ### GD + small LR setting [ logistic ]
# Data generation parameters
set.seed(123)
model_type <- "logistic"
n <- 500
p <- 11
rho <- 0.0
sigma <- 1
n_test <- 5000

# Optimization parameters
learning_rate <- 2e-3
max_iter <- 10000  
momentum_value <- 0.5
decay <- 0.05
stochastic <- FALSE

# Run GD variants
results <- run_all_experiments(
  n = n, p = p, rho = rho, sigma = sigma, 
  model_type = model_type, n_test = n_test,
  learning_rate = learning_rate, 
  max_iter = max_iter,
  momentum_value = momentum_value, 
  decay = decay,
  seed = 123, 
  stochastic=stochastic
)

################################################################################
# Generate plot with iterations on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "iterations",
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/GD_log_small_lr_iters.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_log_small_lr_iters.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate plot with elaspsed time on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "time",
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/GD_log_small_lr_time.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_log_small_lr_time.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save the coefficient paths plot
ggsave(filename = "./figure_man/simu_linmod/GD_log_coef_small.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_log_coef_small.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)

################################################################################
################################################################################
# ### GD + med LR setting [ linear ]
# Data generation parameters
set.seed(123)
model_type <- "linear"
n <- 500
p <- 11
rho <- 0.0
sigma <- 1
n_test <- 5000

# Optimization parameters
learning_rate <- 2e-3
max_iter <- 10000  
momentum_value <- 0.5
decay <- 0.05
stochastic <- FALSE

# Run GD variants
results <- run_all_experiments(
  n = n, p = p, rho = rho, sigma = sigma, 
  model_type = model_type, n_test = n_test,
  learning_rate = learning_rate, 
  max_iter = max_iter,
  momentum_value = momentum_value, 
  decay = decay,
  seed = 123, 
  stochastic=stochastic
)
################################################################################
# Generate plot with iterations on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "iterations",
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/GD_reg_med_lr_iters.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_reg_med_lr_iters.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate plot with elaspsed time on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "time",
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/GD_reg_med_lr_time.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_reg_med_lr_time.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save the coefficient paths plot
ggsave(filename = "./figure_man/simu_linmod/GD_reg_coef_med.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_reg_coef_med.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)

################################################################################
################################################################################
# ### GD + med LR setting [ logistic ]
# Data generation parameters
set.seed(123)
model_type <- "logistic"
n <- 500
p <- 11
rho <- 0.0
sigma <- 1
n_test <- 5000

# Optimization parameters
learning_rate <- 5e-2
max_iter <- 10000  
momentum_value <- 0.5
decay <- 0.05
stochastic <- FALSE

# Run GD variants
results <- run_all_experiments(
  n = n, p = p, rho = rho, sigma = sigma, 
  model_type = model_type, n_test = n_test,
  learning_rate = learning_rate, 
  max_iter = max_iter,
  momentum_value = momentum_value, 
  decay = decay,
  seed = 123, 
  stochastic=stochastic
)
################################################################################
# Generate plot with iterations on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "iterations",
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/GD_log_med_lr_iters.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_log_med_lr_iters.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate plot with elaspsed time on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "time",
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/GD_log_med_lr_time.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_log_med_lr_time.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save the coefficient paths plot
ggsave(filename = "./figure_man/simu_linmod/GD_log_coef_med.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_log_coef_med.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)

################################################################################
################################################################################
### GD + large LR setting [ linear ]
# Data generation parameters
set.seed(123)
model_type <- "linear"
n <- 500
p <- 11
rho <- 0.0
sigma <- 1
n_test <- 5000

# Optimization parameters
learning_rate <- 1.4
max_iter <- 50  
momentum_value <- 0.5
decay <- 0.05
stochastic <- FALSE

# Run GD variants
results <- run_all_experiments(
  n = n, p = p, rho = rho, sigma = sigma, 
  model_type = model_type, n_test = n_test,
  learning_rate = learning_rate, 
  max_iter = max_iter,
  momentum_value = momentum_value, 
  decay = decay,
  seed = 123, 
  stochastic=stochastic
)
################################################################################
# Generate plot with iterations on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "iterations",
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/GD_reg_large_lr_iters.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_reg_large_lr_iters.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate plot with elaspsed time on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "time",
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/GD_reg_large_lr_time.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_reg_large_lr_time.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save the coefficient paths plot
ggsave(filename = "./figure_man/simu_linmod/GD_reg_coef_large.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_reg_coef_large.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)

################################################################################
################################################################################
### GD + large LR setting [ logistic ]
# Data generation parameters
set.seed(123)
model_type <- "logistic"
n <- 500
p <- 11
rho <- 0.0
sigma <- 1
n_test <- 5000

# Optimization parameters
learning_rate <- 10.0
max_iter <- 50  
momentum_value <- 0.5
decay <- 0.05
stochastic <- FALSE

# Run GD variants
results <- run_all_experiments(
  n = n, p = p, rho = rho, sigma = sigma, 
  model_type = model_type, n_test = n_test,
  learning_rate = learning_rate, 
  max_iter = max_iter,
  momentum_value = momentum_value, 
  decay = decay,
  seed = 123, 
  stochastic=stochastic
)
################################################################################
# Generate plot with iterations on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "iterations",
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/GD_log_large_lr_iters.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_log_large_lr_iters.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate plot with elaspsed time on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "time",
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/GD_log_large_lr_time.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_log_large_lr_time.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save the coefficient paths plot
ggsave(filename = "./figure_man/simu_linmod/GD_log_coef_large.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_log_coef_large.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)

################################################################################
################################################################################
# ### GD + med LR setting + correlated features [ linear ]
# Data generation parameters
set.seed(123)
model_type <- "linear"
n <- 500
p <- 11
rho <- 0.9
sigma <- 1
n_test <- 5000

# Optimization parameters
learning_rate <- 2e-3
max_iter <- 10000  
momentum_value <- 0.5
decay <- 0.05
stochastic <- FALSE

# Run GD variants
results <- run_all_experiments(
  n = n, p = p, rho = rho, sigma = sigma, 
  model_type = model_type, n_test = n_test,
  learning_rate = learning_rate, 
  max_iter = max_iter,
  momentum_value = momentum_value, 
  decay = decay,
  seed = 123, 
  stochastic=stochastic
)
################################################################################
# Generate plot with iterations on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "iterations",
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/GD_reg_med_lr_corr_iters.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_reg_med_lr_corr_iters.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate plot with elaspsed time on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "time",
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/GD_reg_med_lr_corr_time.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_reg_med_lr_corr_time.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save the coefficient paths plot
ggsave(filename = "./figure_man/simu_linmod/GD_reg_coef_med_corr.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_reg_coef_med_corr.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)

################################################################################
################################################################################
# ### GD + med LR setting + correlated features [logistic]
# Data generation parameters
set.seed(123)
model_type <- "logistic"
n <- 500
p <- 11
rho <- 0.9
sigma <- 1
n_test <- 5000

# Optimization parameters
learning_rate <- 5e-2
max_iter <- 10000  
momentum_value <- 0.5
decay <- 0.05
stochastic <- FALSE

# Run GD variants
results <- run_all_experiments(
  n = n, p = p, rho = rho, sigma = sigma, 
  model_type = model_type, n_test = n_test,
  learning_rate = learning_rate, 
  max_iter = max_iter,
  momentum_value = momentum_value, 
  decay = decay,
  seed = 123, 
  stochastic=stochastic
)
################################################################################
# Generate plot with iterations on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "iterations",
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/GD_log_med_lr_corr_iters.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_log_med_lr_corr_iters.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate plot with elaspsed time on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "time",
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/GD_log_med_lr_corr_time.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_log_med_lr_corr_time.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("GD", "GD+decay", "GD+mom", "GD+mom+decay")
)
# Save the coefficient paths plot
ggsave(filename = "./figure_man/simu_linmod/GD_log_coef_med_corr.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/GD_log_coef_med_corr.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)

################################################################################
################################################################################

#################################### SGD #######################################

################################################################################
################################################################################
# ### SGD + small LR setting [ linear ]
# Data generation parameters
set.seed(123)
model_type <- "linear"
n <- 500
p <- 11
rho <- 0.0
sigma <- 1
n_test <- 5000

# Optimization parameters
learning_rate <- 2e-4
max_iter <- 10000  
momentum_value <- 0.5
decay <- 0.05
stochastic <- TRUE

# Run GD variants
results <- run_all_experiments(
  n = n, p = p, rho = rho, sigma = sigma, 
  model_type = model_type, n_test = n_test,
  learning_rate = learning_rate, 
  max_iter = max_iter,
  momentum_value = momentum_value, 
  decay = decay,
  seed = 123, 
  stochastic=stochastic
)

################################################################################
# Generate plot with iterations on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "iterations",
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_small_lr_iters.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_small_lr_iters.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate plot with elaspsed time on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "time",
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_small_lr_time.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_small_lr_time.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save the coefficient paths plot
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_coef_small.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_coef_small.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)

################################################################################
################################################################################
# ### SGD + small LR setting [ logistic ]
# Data generation parameters
set.seed(123)
model_type <- "logistic"
n <- 500
p <- 11
rho <- 0.0
sigma <- 1
n_test <- 5000

# Optimization parameters
learning_rate <- 2e-3
max_iter <- 10000  
momentum_value <- 0.5
decay <- 0.05
stochastic <- TRUE

# Run GD variants
results <- run_all_experiments(
  n = n, p = p, rho = rho, sigma = sigma, 
  model_type = model_type, n_test = n_test,
  learning_rate = learning_rate, 
  max_iter = max_iter,
  momentum_value = momentum_value, 
  decay = decay,
  seed = 123, 
  stochastic=stochastic
)

################################################################################
# Generate plot with iterations on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "iterations",
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/SGD_log_small_lr_iters.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_log_small_lr_iters.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate plot with elaspsed time on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "time",
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/SGD_log_small_lr_time.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_log_small_lr_time.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save the coefficient paths plot
ggsave(filename = "./figure_man/simu_linmod/SGD_log_coef_small.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_log_coef_small.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)

################################################################################
################################################################################
# ### SGD + med LR setting [ linear ]
# Data generation parameters
set.seed(123)
model_type <- "linear"
n <- 500
p <- 11
rho <- 0.0
sigma <- 1
n_test <- 5000

# Optimization parameters
learning_rate <- 1e-3
max_iter <- 10000  
momentum_value <- 0.5
decay <- 0.05
stochastic <- TRUE

# Run GD variants
results <- run_all_experiments(
  n = n, p = p, rho = rho, sigma = sigma, 
  model_type = model_type, n_test = n_test,
  learning_rate = learning_rate, 
  max_iter = max_iter,
  momentum_value = momentum_value, 
  decay = decay,
  seed = 123, 
  stochastic=stochastic
)
################################################################################
# Generate plot with iterations on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "iterations",
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_med_lr_iters.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_med_lr_iters.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate plot with elaspsed time on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "time",
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_med_lr_time.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_med_lr_time.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save the coefficient paths plot
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_coef_med.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_coef_med.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)

################################################################################
################################################################################
# ### SGD + med LR setting [ logistic ]
# Data generation parameters
set.seed(123)
model_type <- "logistic"
n <- 500
p <- 11
rho <- 0.0
sigma <- 1
n_test <- 5000

# Optimization parameters
learning_rate <- 5e-2
max_iter <- 10000  
momentum_value <- 0.5
decay <- 0.05
stochastic <- TRUE

# Run GD variants
results <- run_all_experiments(
  n = n, p = p, rho = rho, sigma = sigma, 
  model_type = model_type, n_test = n_test,
  learning_rate = learning_rate, 
  max_iter = max_iter,
  momentum_value = momentum_value, 
  decay = decay,
  seed = 123, 
  stochastic=stochastic
)
################################################################################
# Generate plot with iterations on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "iterations",
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/SGD_log_med_lr_iters.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_log_med_lr_iters.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate plot with elaspsed time on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "time",
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/SGD_log_med_lr_time.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_log_med_lr_time.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save the coefficient paths plot
ggsave(filename = "./figure_man/simu_linmod/SGD_log_coef_med.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_log_coef_med.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)

################################################################################
################################################################################
### SGD + large LR setting [ linear ]
# Data generation parameters
set.seed(123)
model_type <- "linear"
n <- 500
p <- 11
rho <- 0.0
sigma <- 1
n_test <- 5000

# Optimization parameters
learning_rate <- 1e-2
max_iter <- 10000  
momentum_value <- 0.5
decay <- 0.05
stochastic <- TRUE

# Run GD variants
results <- run_all_experiments(
  n = n, p = p, rho = rho, sigma = sigma, 
  model_type = model_type, n_test = n_test,
  learning_rate = learning_rate, 
  max_iter = max_iter,
  momentum_value = momentum_value, 
  decay = decay,
  seed = 123, 
  stochastic=stochastic
)
################################################################################
# Generate plot with iterations on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "iterations",
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_large_lr_iters.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_large_lr_iters.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate plot with elaspsed time on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "time",
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_large_lr_time.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_large_lr_time.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save the coefficient paths plot
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_coef_large.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_coef_large.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)

################################################################################
################################################################################
### SGD + large LR setting [ logistic ]
# Data generation parameters
set.seed(123)
model_type <- "logistic"
n <- 500
p <- 11
rho <- 0.0
sigma <- 1
n_test <- 5000

# Optimization parameters
learning_rate <- 5e-1
max_iter <- 10000  
momentum_value <- 0.5
decay <- 0.05
stochastic <- TRUE

# Run GD variants
results <- run_all_experiments(
  n = n, p = p, rho = rho, sigma = sigma, 
  model_type = model_type, n_test = n_test,
  learning_rate = learning_rate, 
  max_iter = max_iter,
  momentum_value = momentum_value, 
  decay = decay,
  seed = 123, 
  stochastic=stochastic
)
################################################################################
# Generate plot with iterations on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "iterations",
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/SGD_log_large_lr_iters.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_log_large_lr_iters.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate plot with elaspsed time on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "time",
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/SGD_log_large_lr_time.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_log_large_lr_time.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save the coefficient paths plot
ggsave(filename = "./figure_man/simu_linmod/SGD_log_coef_large.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_log_coef_large.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)

################################################################################
################################################################################
# ### SGD + med LR setting + correlated features [ linear ]
# Data generation parameters
set.seed(123)
model_type <- "linear"
n <- 500
p <- 11
rho <- 0.97
sigma <- 1
n_test <- 5000

# Optimization parameters
learning_rate <- 1e-3
max_iter <- 10000  
momentum_value <- 0.5
decay <- 0.05
stochastic <- TRUE

# Run GD variants
results <- run_all_experiments(
  n = n, p = p, rho = rho, sigma = sigma, 
  model_type = model_type, n_test = n_test,
  learning_rate = learning_rate, 
  max_iter = max_iter,
  momentum_value = momentum_value, 
  decay = decay,
  seed = 123, 
  stochastic=stochastic
)
################################################################################
# Generate plot with iterations on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "iterations",
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_med_lr_corr_iters.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_med_lr_corr_iters.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate plot with elaspsed time on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "time",
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_med_lr_corr_time.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_med_lr_corr_time.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save the coefficient paths plot
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_coef_med_corr.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_reg_coef_med_corr.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)

################################################################################
################################################################################
# ### SGD + med LR setting + correlated features [ logistic ]
# Data generation parameters
set.seed(123)
model_type <- "logistic"
n <- 500
p <- 11
rho <- 0.97
sigma <- 1
n_test <- 5000

# Optimization parameters
learning_rate <- 5e-2
max_iter <- 10000  
momentum_value <- 0.5
decay <- 0.05
stochastic <- TRUE

# Run GD variants
results <- run_all_experiments(
  n = n, p = p, rho = rho, sigma = sigma, 
  model_type = model_type, n_test = n_test,
  learning_rate = learning_rate, 
  max_iter = max_iter,
  momentum_value = momentum_value, 
  decay = decay,
  seed = 123, 
  stochastic=stochastic
)
################################################################################
# Generate plot with iterations on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "iterations",
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/SGD_log_med_lr_corr_iters.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_log_med_lr_corr_iters.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate plot with elaspsed time on x-axis
pl <- plot_optimization_results(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  xaxis = "time",
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save as PNG/PDF
ggsave(filename = "./figure_man/simu_linmod/SGD_log_med_lr_corr_time.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_log_med_lr_corr_time.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
)
# Save the coefficient paths plot
ggsave(filename = "./figure_man/simu_linmod/SGD_log_coef_med_corr.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "./figure_man/simu_linmod/SGD_log_coef_med_corr.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)

################################################################################
################################################################################



