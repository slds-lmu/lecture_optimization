# This script contains functions for generating synthetic data for linear and logistic regression models,
# implementing optimization algorithms with various enhancements, and plotting the results.
# The focus is on comparing different optimization methods and observing their effects on model training
# under varying conditions, such as different condition numbers of the feature matrix.

# Load required libraries
library(MASS)
library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)
library(rstudioapi)

set.seed(123)

# Set working directory to the location of the current file
current_file <- rstudioapi::getActiveDocumentContext()$path
if (current_file == "") {
  stop("Please save the script before running.")
}
setwd(dirname(current_file))

# Create 'figures' directory
if (!dir.exists("figures")) {
  dir.create("figures")
}

## Set hyperparameters
# Data generation parameters
n <- 100    
p <- 20          
rho <- 0.0      
sigma <- 1       
model_type <- "linear"
sparse <- FALSE   
n_test <- 5000    

# Optimization parameters
learning_rate <- 5e-2
learning_rate_sgd <- 5e-3 
max_iter <- 10000          
momentum_value <- 0.1      
decay = 0.5

lwidth = 1.1
fsize=18
legendsize = 18

# Function to compute condition number
compute_condition_number <- function(X) {
  return(kappa(t(X) %*% X))
}

################################################################################
# Data generation function
generate_data <- function(n, p, rho, sigma, model, sparse, n_test) {
  # Generate covariance matrix
  if (rho == 0) {
    Sigma <- diag(p)
  } else {
    Sigma <- toeplitz(rho^(0:(p - 1)))
  }
  
  # Generate feature matrix X
  X <- mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
  X_test <- mvrnorm(n_test, mu = rep(0, p), Sigma = Sigma)
  
  # Generate true coefficients beta_true
  if (sparse) {
    beta_true <- rep(0, p)
    num_nonzero <- floor(0.1 * p)
    nonzero_indices <- sample(1:p, num_nonzero)
    beta_true[nonzero_indices] <- rnorm(num_nonzero)
  } else {
    beta_true <- rnorm(p)
  }
  
  # Linear predictor
  preds <- X %*% beta_true
  preds_test <- X_test %*% beta_true
  
  # Generate response variable y
  if (model == "linear") {
    noise <- rnorm(n, mean = 0, sd = sigma)
    y <- preds + noise
    noise_test <- rnorm(n_test, mean = 0, sd = sigma)
    y_test <- preds_test + noise_test
  } else if (model == "logistic") {
    probs <- 1 / (1 + exp(-preds))
    y <- rbinom(n, size = 1, prob = probs)
    probs_test <- 1 / (1 + exp(-preds_test))
    y_test <- rbinom(n_test, size = 1, prob = probs_test)
  } else {
    stop("Invalid model type. Choose 'linear' or 'logistic'.")
  }
  
  # Compute condition number
  cond_number <- compute_condition_number(X)
  
  # Print dataset properties
  cat("Dataset Properties:\n")
  cat("Model type:", model, "\n")
  cat("n (training set size):", n, "\n")
  cat("p (number of features):", p, "\n")
  cat("Sparsity:", ifelse(sparse, "Sparse", "Not Sparse"), "\n")
  cat("Condition number of X^T X:", cond_number, "\n\n")
  
  return(list(
    X = X, y = y, beta_true = beta_true, preds = preds,
    X_test = X_test, y_test = y_test, preds_test = preds_test,
    condition_number = cond_number
  ))
}

################################################################################
# Linear Regression Direct Solution
linreg_direct_solution <- function(X, y) {
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  return(beta_hat)
}

################################################################################
gradient_descent <- function(X, y, X_test, y_test, beta_true, model = model_type, learning_rate, max_iter, 
                             beta_init = NULL, stochastic = FALSE,
                             step_size_control = FALSE, decay_rate = 0,
                             momentum = NULL) {
  n <- nrow(X)
  p <- ncol(X)
  
  sigma_init <- sqrt(1 / p)
  if (is.null(beta_init)) {
    beta <- rnorm(p, mean = 0, sd = sigma_init)
  } else {
    beta <- beta_init
  }
  
  velocity <- rep(0, p)
  
  beta_history <- matrix(NA_real_, nrow = max_iter, ncol = p)
  loss_history <- rep(NA_real_, max_iter)
  l2_diff_history <- rep(NA_real_, max_iter)
  test_loss_history <- rep(NA_real_, max_iter)
  
  for (iter in 1:max_iter) {
    if (step_size_control) {
      learning_rate_iter <- learning_rate * (decay_rate^(iter / max_iter)) # standard sgd decay param
    } else {
      learning_rate_iter <- learning_rate
    }
    
    if (stochastic) {
      idx <- sample(1:n, 1)
      X_batch <- X[idx, , drop = FALSE]
      y_batch <- y[idx]
    } else {
      X_batch <- X
      y_batch <- y
    }
    
    if (model == "linear") {
      preds <- X_batch %*% beta
      error <- preds - y_batch
      gradient <- t(X_batch) %*% error / nrow(X_batch)
      
      train_preds <- X %*% beta
      test_preds <- X_test %*% beta
      train_loss <- mean((train_preds - y)^2) / 2
      test_loss <- mean((test_preds - y_test)^2) / 2
      
    } else if (model == "logistic") {
      if (stochastic) {
        preds_batch <- X_batch %*% beta
        probs_batch <- pmax(pmin(1 / (1 + exp(-preds_batch)), 1 - 1e-10), 1e-10)
        error <- probs_batch - y_batch
        gradient <- t(X_batch) %*% error
      } else {
        preds_batch <- X_batch %*% beta
        probs_batch <- pmax(pmin(1 / (1 + exp(-preds_batch)), 1 - 1e-10), 1e-10)
        error <- probs_batch - y_batch
        gradient <- t(X_batch) %*% error / nrow(X_batch)
      }
      
      train_preds <- X %*% beta
      test_preds <- X_test %*% beta
      train_probs <- pmax(pmin(1 / (1 + exp(-train_preds)), 1 - 1e-10), 1e-10)
      test_probs <- pmax(pmin(1 / (1 + exp(-test_preds)), 1 - 1e-10), 1e-10)
      
      train_loss <- -mean(y * log(train_probs) + (1 - y) * log(1 - train_probs))
      test_loss <- -mean(y_test * log(test_probs) + (1 - y_test) * log(1 - test_probs))
    }
    
    if (!is.finite(train_loss)) train_loss <- .Machine$double.xmax
    if (!is.finite(test_loss)) test_loss <- .Machine$double.xmax
    
    if (!is.null(momentum)) {
      velocity <- momentum * velocity - learning_rate_iter * gradient
      beta <- beta + velocity
    } else {
      beta <- beta - learning_rate_iter * gradient
    }
    
    beta_history[iter, ] <- as.numeric(beta)
    loss_history[iter] <- as.numeric(train_loss)
    test_loss_history[iter] <- as.numeric(test_loss)
    l2_diff_history[iter] <- as.numeric(sqrt(sum((beta - beta_true)^2)))
  }
  
  if (model == "linear") {
    train_preds <- X %*% beta
    test_preds <- X_test %*% beta
    train_rmse <- sqrt(mean((train_preds - y)^2))
    test_rmse <- sqrt(mean((test_preds - y_test)^2))
  } else if (model == "logistic") {
    train_preds_prob <- 1 / (1 + exp(-X %*% beta))
    test_preds_prob <- 1 / (1 + exp(-X_test %*% beta))
    train_rmse <- sqrt(mean((train_preds_prob - y)^2))
    test_rmse <- sqrt(mean((test_preds_prob - y_test)^2))
  }
  
  cat("Optimization Settings:\n")
  cat("Model type:", model, "\n")
  cat("Learning rate:", learning_rate, "\n")
  if (step_size_control) {
    cat("Exponential Learning Rate Decay: Enabled\n")
    cat("Decay rate:", decay_rate, "\n")
  }
  if (!is.null(momentum)) {
    cat("Momentum:", momentum, "\n")
  }
  if (stochastic) {
    cat("Stochastic Gradient Descent: TRUE\n")
  } else {
    cat("Stochastic Gradient Descent: FALSE\n")
  }
  cat("\nPerformance Metrics:\n")
  cat("Training RMSE:", train_rmse, "\n")
  cat("Test RMSE:", test_rmse, "\n")
  cat("Final Parameter Estimation Error:", l2_diff_history[length(l2_diff_history)], "\n\n")
  
  return(list(
    beta = beta,
    beta_history = beta_history,
    loss_history = loss_history,
    test_loss_history = test_loss_history,
    l2_diff_history = l2_diff_history
  ))
}

################################################################################
plot_combined_losses <- function(loss_histories, test_loss_histories, l2_diff_histories, 
                                 method_names, settings, convergence_threshold = 1e-6,
                                 ylim_min_train, ylim_max_train,
                                 ylim_min_test, ylim_max_test,
                                 ylim_min_param, ylim_max_param,
                                 ols_train_loss, ols_test_loss, ols_l2_diff,
                                 cond_number) {
  
  # Define consistent colors and order for methods
  method_colors <- c(
    "GD" = "#E41A1C",             # Red
    "GD+mom" = "#4DAF4A",         # Green
    "GD+mom+ExpDecay" = "#377EB8",  # Blue
    "SGD" = "#FF7F00",            # Orange
    "SGD+mom" = "#984EA3",        # Purple
    "SGD+mom+ExpDecay" = "#F781BF"  # Pink
  )
  
  # Order methods
  method_order <- c("GD", "GD+mom", "GD+mom+ExpDecay",
                    "SGD", "SGD+mom", "SGD+mom+ExpDecay")
  
  # Get number of iterations
  max_iter <- length(loss_histories[[1]])
  
  # Create data frames
  df_loss <- data.frame(
    Iteration = rep(1:max_iter, length(method_names)),
    Loss = unlist(loss_histories),
    Method = factor(rep(method_names, each = max_iter), 
                    levels = method_order)
  )
  
  df_test_loss <- data.frame(
    Iteration = rep(1:max_iter, length(method_names)),
    Loss = unlist(test_loss_histories),
    Method = factor(rep(method_names, each = max_iter), 
                    levels = method_order)
  )
  
  df_l2_diff <- data.frame(
    Iteration = rep(1:max_iter, length(method_names)),
    Error = unlist(l2_diff_histories),
    Method = factor(rep(method_names, each = max_iter), 
                    levels = method_order)
  )
  
  base_theme <- theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = fsize, face = "bold"),  
      axis.text = element_text(size = fsize * 0.8),           
      legend.position = "none",
      panel.grid.minor = element_blank()
    )
  
  # Plot Training Loss
  p1 <- ggplot(df_loss, aes(x = Iteration, y = Loss, color = Method)) +
    {if(!is.null(ols_train_loss)) 
      geom_hline(yintercept = ols_train_loss, linetype = "dashed", 
                 color = "black", linewidth = lwidth)} +
    geom_line(linewidth = lwidth, alpha = 0.8) +
    scale_y_continuous(limits = c(ylim_min_train, ylim_max_train)) +
    scale_color_manual(values = method_colors) +
    labs(title = "Training Loss", x = "Iteration", y = "Loss") +
    base_theme
  
  # Plot Test Loss
  p2 <- ggplot(df_test_loss, aes(x = Iteration, y = Loss, color = Method)) +
    {if(!is.null(ols_test_loss))
      geom_hline(yintercept = ols_test_loss, linetype = "dashed", 
                 color = "black", linewidth = lwidth)} +
    geom_line(linewidth = lwidth, alpha = 0.8) +
    scale_y_continuous(limits = c(ylim_min_test, ylim_max_test)) +
    scale_color_manual(values = method_colors) +
    labs(title = "Test Loss", x = "Iteration", y = "Loss") +
    base_theme
  
  # Plot L2 Difference
  p3 <- ggplot(df_l2_diff, aes(x = Iteration, y = Error, color = Method)) +
    {if(!is.null(ols_l2_diff))
      geom_hline(yintercept = ols_l2_diff, linetype = "dashed", 
                 color = "black", linewidth = lwidth)} +
    geom_line(linewidth = lwidth, alpha = 0.8) +
    scale_y_continuous(limits = c(ylim_min_param, ylim_max_param)) +
    scale_color_manual(values = method_colors) +
    labs(title = "Estimation Error", x = "Iteration", y = "Error") +
    base_theme
  
  # Extract legend
  legend <- cowplot::get_legend(
    p1 + 
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.box = "horizontal",
            legend.text = element_text(size = legendsize)) +
      guides(color = guide_legend(nrow = 1))
  )
  
  # Arrange plots in a single row
  plots <- gridExtra::arrangeGrob(
    p1, p2, p3,
    nrow = 1
  )
  
  # Arrange plots and legend with overall title
  combined_plot <- gridExtra::arrangeGrob(
    plots,
    legend,
    heights = c(10, 1),
    top = grid::textGrob(settings, gp = grid::gpar(fontsize = fsize, 
                                                   fontface = "bold", 
                                                   hjust = 0.5)),
    bottom = NULL
  )
  
  # Display the combined plot
  grid::grid.newpage()
  grid::grid.draw(combined_plot)
  
  # Save the plot
  ggsave(filename = "figures/combined_losses.png", 
         plot = combined_plot, 
         width = 16, 
         height = 6, 
         dpi = 300)
  ggsave(filename = "figures/combined_losses.pdf", 
         plot = combined_plot, 
         width = 16, 
         height = 6)
}

################################################################################
# Main execution code

# Generate data
data <- generate_data(n, p, rho, sigma, model_type, sparse, n_test)
X <- data$X
y <- data$y
X_test <- data$X_test
y_test <- data$y_test
beta_true <- data$beta_true
cond_number <- data$condition_number

# Create a single initialization for all methods
sigma_init <- sqrt(1 / p)
beta_init <- rnorm(p, mean = 0, sd = sigma_init)

# Only compute OLS for linear regression
if(model_type == "linear") {
  beta_ols <- linreg_direct_solution(X, y)
  ols_train_loss <- mean((X %*% beta_ols - y)^2) / 2
  ols_test_loss <- mean((X_test %*% beta_ols - y_test)^2) / 2
  ols_l2_diff <- sqrt(sum((beta_ols - beta_true)^2))
} else {
  ols_train_loss <- NULL
  ols_test_loss <- NULL
  ols_l2_diff <- NULL
}

# Prepare for experiments
loss_histories <- list()
test_loss_histories <- list()
l2_diff_histories <- list()
method_names <- c()

# GD without momentum
result_gd <- gradient_descent(
  X, y, X_test, y_test, beta_true,
  learning_rate = learning_rate,
  max_iter = max_iter,
  beta_init = beta_init,
  momentum = NULL,
  stochastic = FALSE,
  step_size_control = FALSE,
  decay_rate = decay
)
loss_histories[[1]] <- result_gd$loss_history
test_loss_histories[[1]] <- result_gd$test_loss_history
l2_diff_histories[[1]] <- result_gd$l2_diff_history
method_names[1] <- "GD"

# GD with momentum
result_gd_momentum <- gradient_descent(
  X, y, X_test, y_test, beta_true,
  learning_rate = learning_rate,
  max_iter = max_iter,
  beta_init = beta_init,
  momentum = momentum_value,
  stochastic = FALSE,
  step_size_control = FALSE,
  decay_rate = decay
)
loss_histories[[2]] <- result_gd_momentum$loss_history
test_loss_histories[[2]] <- result_gd_momentum$test_loss_history
l2_diff_histories[[2]] <- result_gd_momentum$l2_diff_history
method_names[2] <- "GD+mom"

# GD with momentum and exponential decay
result_gd_momentum_decay <- gradient_descent(
  X, y, X_test, y_test, beta_true,
  learning_rate = learning_rate,
  max_iter = max_iter,
  beta_init = beta_init,
  momentum = momentum_value,
  stochastic = FALSE,
  step_size_control = TRUE,
  decay_rate = decay
)
loss_histories[[3]] <- result_gd_momentum_decay$loss_history
test_loss_histories[[3]] <- result_gd_momentum_decay$test_loss_history
l2_diff_histories[[3]] <- result_gd_momentum_decay$l2_diff_history
method_names[3] <- "GD+mom+ExpDecay"

# SGD without momentum
result_sgd <- gradient_descent(
  X, y, X_test, y_test, beta_true,
  learning_rate = learning_rate_sgd,
  max_iter = max_iter,
  beta_init = beta_init,
  momentum = NULL,
  stochastic = TRUE,
  step_size_control = FALSE,
  decay_rate = decay
)
loss_histories[[4]] <- result_sgd$loss_history
test_loss_histories[[4]] <- result_sgd$test_loss_history
l2_diff_histories[[4]] <- result_sgd$l2_diff_history
method_names[4] <- "SGD"

# SGD with momentum
result_sgd_momentum <- gradient_descent(
  X, y, X_test, y_test, beta_true,
  learning_rate = learning_rate_sgd,
  max_iter = max_iter,
  beta_init = beta_init,
  momentum = momentum_value,
  stochastic = TRUE,
  step_size_control = FALSE,
  decay_rate = decay
)
loss_histories[[5]] <- result_sgd_momentum$loss_history
test_loss_histories[[5]] <- result_sgd_momentum$test_loss_history
l2_diff_histories[[5]] <- result_sgd_momentum$l2_diff_history
method_names[5] <- "SGD+mom"

# SGD with momentum and exponential decay
result_sgd_momentum_decay <- gradient_descent(
  X, y, X_test, y_test, beta_true,
  learning_rate = learning_rate_sgd,
  max_iter = max_iter,
  beta_init = beta_init,
  momentum = momentum_value,
  stochastic = TRUE,
  step_size_control = TRUE,
  decay_rate = decay
)
loss_histories[[6]] <- result_sgd_momentum_decay$loss_history
test_loss_histories[[6]] <- result_sgd_momentum_decay$test_loss_history
l2_diff_histories[[6]] <- result_sgd_momentum_decay$l2_diff_history
method_names[6] <- "SGD+mom+ExpDecay"


### Plot results
names(loss_histories) <- method_names
print(sapply(loss_histories, function(x) sum(is.na(x))))

lengths <- sapply(loss_histories, length)
print("Lengths of each vector:")
print(lengths)

cond_number <- round(cond_number, digits=1)
settings_title <- bquote("Model:" ~ .(model_type) ~ ", " ~ n == .(n) ~ ", " ~ p == .(p) ~ ", " ~ rho == .(rho) ~ ", " ~ sigma == .(sigma) ~ ", " ~ kappa == .(cond_number) ~ ", step size =" ~ .(learning_rate))

# Y-axis limits for plots
ylim_min_train <- min(sapply(loss_histories, min)) * 0.9
ylim_min_test <- min(sapply(test_loss_histories, min)) * 0.9
ylim_min_param <- min(sapply(l2_diff_histories, min)) * 0.9
ylim_max_train <- min(2,max(sapply(loss_histories, max)) * 1.1)  
ylim_max_test <- min(2,max(sapply(test_loss_histories, max)) * 1.1)
ylim_max_param <- min(2,max(sapply(l2_diff_histories, max)) * 1.1)


plot_combined_losses(
  loss_histories = loss_histories,
  test_loss_histories = test_loss_histories,
  l2_diff_histories = l2_diff_histories,
  method_names = method_names,
  settings = settings_title,
  ylim_min_train = ylim_min_train,
  ylim_max_train = ylim_max_train,
  ylim_min_test = ylim_min_test,
  ylim_max_test = ylim_max_test,
  ylim_min_param = ylim_min_param,
  ylim_max_param = ylim_max_param,
  ols_train_loss = ols_train_loss,
  ols_test_loss = ols_test_loss,
  ols_l2_diff = ols_l2_diff,
  cond_number = cond_number
)