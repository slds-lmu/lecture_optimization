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
library(ggpubr)
library(grid)
#library(cowplot)
#library(rstudioapi)
library(reshape2)

set.seed(123)

# Set working directory to the location of the current file
current_file <- rstudioapi::getActiveDocumentContext()$path
if (current_file == "") {
  stop("Please save the script before running.")
}
setwd(dirname(current_file))

# Create 'figure_man' directory
if (!dir.exists("../../figure_man/simu-newton/")) {
  dir.create("../../figure_man/simu-newton/")
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
# Save as PNG/PDF
ggsave(filename = "../../figure_man/simu-newton/NR_GD_log_indep_50iters.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "../../figure_man/simu-newton/NR_GD_log_indep_50iters.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("Newton-Raphson", "Newton-Raphson+step size", "GD+mom")
)
# Save the coefficient paths plot
ggsave(filename = "../../figure_man/simu-newton/NR_GD_log_coef_50indep.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "../../figure_man/simu-newton/NR_GD_log_coef_50indep.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)

# Generate runtime comparison plot
pl_runtime <- plot_runtime_comparison(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  methods_to_show = c("Newton-Raphson", "Newton-Raphson+step size", "GD+mom")
)

# Save the runtime comparison plot
ggsave(filename = "../../figure_man/simu-newton/NR_GD_runtime_comparison.png", 
       plot = pl_runtime, width = 15, height = 6, dpi = 300)
ggsave(filename = "../../figure_man/simu-newton/NR_GD_runtime_comparison.pdf", 
       plot = pl_runtime, width = 15, height = 6, device = cairo_pdf)

print(pl_runtime)

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
# Save as PNG/PDF
ggsave(filename = "../../figure_man/simu-newton/NR_GD_log_indep_1000iters.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "../../figure_man/simu-newton/NR_GD_log_indep_1000iters.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("Newton-Raphson", "Newton-Raphson+step size", "GD+mom")
)
# Save the coefficient paths plot
ggsave(filename = "../../figure_man/simu-newton/NR_GD_log_coef_1000indep.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "../../figure_man/simu-newton/NR_GD_log_coef_1000indep.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)

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
# Save as PNG/PDF
ggsave(filename = "../../figure_man/simu-newton/NR_GD_log_indep_50iters_corr.png", plot = pl, width = 15, 
       height = 5, dpi = 300)
ggsave(filename = "../../figure_man/simu-newton/NR_GD_log_indep_50iters_corr.pdf", plot = pl, 
       width = 15, height = 5, device = cairo_pdf)
print(pl)

# Generate coefficient path plots
pl_coef <- plot_coef_paths(
  beta_histories = results$beta,
  method_names = results$method_names,
  beta_true = results$beta_true,
  methods_to_show = c("Newton-Raphson", "Newton-Raphson+step size", "GD+mom")
)
# Save the coefficient paths plot
ggsave(filename = "../../figure_man/simu-newton/NR_GD_log_coef_50indep_corr.png", 
       plot = pl_coef, width = 15, height = 4, dpi = 300)
ggsave(filename = "../../figure_man/simu-newton/NR_GD_log_coef_50indep_corr.pdf", 
       plot = pl_coef, width = 15, height = 4, device = cairo_pdf)

plot(pl_coef)

# Generate runtime comparison plot
pl_runtime <- plot_runtime_comparison(
  loss_histories = results$loss,
  test_loss_histories = results$test_loss,
  l2_diff_histories = results$l2_diff,
  time_histories = results$time,
  method_names = results$method_names,
  methods_to_show = c("Newton-Raphson", "Newton-Raphson+step size", "GD+mom")
)

# Save the runtime comparison plot
ggsave(filename = "../../figure_man/simu-newton/NR_GD_runtime_comparison_corr.png", 
       plot = pl_runtime, width = 15, height = 6, dpi = 300)
ggsave(filename = "../../figure_man/simu-newton/NR_GD_runtime_comparison_corr.pdf", 
       plot = pl_runtime, width = 15, height = 6, device = cairo_pdf)

print(pl_runtime)

