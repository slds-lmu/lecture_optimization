# ------------------------------------------------------------------------------
# multivariate second order

# FUNC: 
#   Functions for generating synthetic data for logistic regression models,
#   implementing Newton-Raphson and GD optimization and plotting the results.
# ------------------------------------------------------------------------------

# Load required libraries
library(MASS)
library(ggplot2)
library(gridExtra)
library(grid)
library(reshape2)

set.seed(123)

output_dir <- "../figure/simu-newton"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

save_plot <- function(plot, filename, width, height) {
  ggsave(
    filename = file.path(output_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

# ------------------------------------------------------------------------------

# newton-raphson step size
nr_step = 0.7

################################################################################
################################################################################

source(file.path("./simu-linmods-functions.R"))

################################################################################
################################################################################


################################ Logistic ######################################

################################################################################
################################################################################
# Newton-Raphson and GD comparison for 50 steps
# Data generation parameters
set.seed(123)
model_type <- "logistic"
n <- 500
p <- 11
rho <- 0.0
n_test <- 5000

# Optimization parameters
learning_rate_gd <- 1
step_size <- nr_step
max_iter <- 50
max_iter_gd <- 50
decay <- 1
stochastic <- FALSE

# Run NR+GD variants
results <- run_all_experiments_nr(
  n = n, p = p, rho = rho, 
  model_type = model_type, n_test = n_test,
  learning_rate_gd = learning_rate_gd,
  step_size = step_size,
  max_iter = max_iter,
  max_iter_gd = max_iter_gd,
  seed = 123
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
  methods_to_show = c("Newton-Raphson", "Newton-Raphson+step size", "GD+mom")
)
# Save the figure used on the slides
save_plot(plot = pl, filename = "NR_GD_log_indep_50iters", width = 15, height = 5)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("Newton-Raphson", "Newton-Raphson+step size", "GD+mom")
)
# Save the figure used on the slides
save_plot(plot = pl_coef, filename = "NR_GD_log_coef_50indep", width = 15, height = 4)

# Generate runtime comparison plot
pl_runtime <- plot_runtime_comparison(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  methods_to_show = c("Newton-Raphson", "Newton-Raphson+step size", "GD+mom")
)

# Save the figure used on the slides
save_plot(plot = pl_runtime, filename = "NR_GD_runtime_comparison", width = 15, height = 6)

################################################################################
################################################################################
# Newton-Raphson and GD comparison for 1000 steps
# Data generation parameters
set.seed(123)
model_type <- "logistic"
n <- 500
p <- 11
rho <- 0.0
n_test <- 5000

# Optimization parameters
learning_rate_gd <- 1
step_size <- nr_step
max_iter <- 1000
max_iter_gd <- 1000
decay <- 1
stochastic <- FALSE

# Run NR+GD variants
results <- run_all_experiments_nr(
  n = n, p = p, rho = rho, 
  model_type = model_type, n_test = n_test,
  learning_rate_gd = learning_rate_gd,
  step_size = step_size,
  max_iter = max_iter,
  max_iter_gd = max_iter_gd,
  seed = 123
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
  methods_to_show = c("Newton-Raphson", "Newton-Raphson+step size", "GD+mom")
)
# Save the figure used on the slides
save_plot(plot = pl, filename = "NR_GD_log_indep_1000iters", width = 15, height = 5)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("Newton-Raphson", "Newton-Raphson+step size", "GD+mom")
)
# Save the figure used on the slides
save_plot(plot = pl_coef, filename = "NR_GD_log_coef_1000indep", width = 15, height = 4)

################################################################################
################################################################################
# Newton-Raphson and GD comparison for 50 steps and correlated features
# Data generation parameters
set.seed(123)
model_type <- "logistic"
n <- 500
p <- 11
rho <- 0.99
n_test <- 5000

# Optimization parameters
learning_rate_gd <- 1
step_size <- nr_step
max_iter <- 50
max_iter_gd <- 50
decay <- 1
stochastic <- FALSE

# Run NR+GD variants
results <- run_all_experiments_nr(
  n = n, p = p, rho = rho, 
  model_type = model_type, n_test = n_test,
  learning_rate_gd = learning_rate_gd,
  step_size = step_size,
  max_iter = max_iter,
  max_iter_gd = max_iter_gd,
  seed = 123
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
  methods_to_show = c("Newton-Raphson", "Newton-Raphson+step size", "GD+mom")
)
# Save the figure used on the slides
save_plot(plot = pl, filename = "NR_GD_log_indep_50iters_corr", width = 15, height = 5)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("Newton-Raphson", "Newton-Raphson+step size", "GD+mom")
)
# Save the figure used on the slides
save_plot(plot = pl_coef, filename = "NR_GD_log_coef_50indep_corr", width = 15, height = 4)

# Generate runtime comparison plot
pl_runtime <- plot_runtime_comparison(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  methods_to_show = c("Newton-Raphson", "Newton-Raphson+step size", "GD+mom")
)

# Save the figure used on the slides
save_plot(plot = pl_runtime, filename = "NR_GD_runtime_comparison_corr", width = 15, height = 6)
