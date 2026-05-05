# This script contains functions for generating synthetic data for linear and logistic regression models,
# implementing optimization algorithms with various enhancements, and plotting the results.
# The focus is on comparing different optimization methods and observing their effects on model training
# under varying conditions, such as different condition numbers of the feature matrix.

# Load libraries
library(MASS)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(grid)
#library(cowplot)
library(reshape2)

set.seed(123)

# Function to compute condition number
compute_condition_number <- function(X) {
  return(kappa(t(X) %*% X))
}

################################################################################
# Data generation function
generate_data <- function(n, p, rho, sigma, model, n_test) {
  set.seed(123)
  # Generate covariance matrix
  if (rho == 0) {
    Sigma <- diag(p)
  } else {
    Sigma <- toeplitz(rho^(0:(p - 1)))
  }
  
  # Generate feature matrix X
  X <- mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
  X_test <- mvrnorm(n_test, mu = rep(0, p), Sigma = Sigma)
  
  # Generate true coefficients beta_true with alternating signs
  if (p %% 2 == 0) {
    # For even p
    neg_values <- -(1:(p/2))  # Creates sequence -1, -2, ..., -p/2
    pos_values <- seq(p/2, 1)  # Creates sequence p/2, ..., 1
    beta_ground_truth <- c(neg_values, pos_values)
  } else {
    # For odd p
    neg_values <- -(1:floor(p/2))  # Creates sequence -1, -2, ..., -floor(p/2)
    pos_values <- seq(floor(p/2), 1)  # Creates sequence floor(p/2), ..., 1
    beta_ground_truth <- c(neg_values, 0, pos_values)
  }
  
  # Linear predictor
  preds <- X %*% beta_ground_truth
  preds_test <- X_test %*% beta_ground_truth
  
  # Generate response variable y
  if (model == "linear") {
    noise <- rnorm(n, mean = 0, sd = sigma)
    y <- preds + noise
    set.seed(123)
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
  
  # Calculate beta_true from training data
  if (model == "linear") {
    beta_true <- coef(lm(y ~ X - 1))
  } else if (model == "logistic") {
    beta_true <- coef(glm(y ~ X - 1, family = binomial()))
  }
  
  # Compute condition number
  cond_number <- compute_condition_number(X)
  
  # Print dataset properties
  cat("Dataset Properties:\n")
  cat("Model type:", model, "\n")
  cat("n (training set size):", n, "\n")
  cat("p (number of features):", p, "\n")
  cat("Condition number of X^T X:", cond_number, "\n\n")
  cat("lm/glm estimates:", beta_true, "\n\n")
  
  return(list(
    X = X, y = y, beta_true = beta_true, beta_ground_truth = beta_ground_truth, preds = preds,
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
  set.seed(123)
  n <- nrow(X)
  p <- ncol(X)
  
  beta_init = rep(0,p)
  beta <- beta_init
  
  
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  if (sum(y- X %*% beta_hat)<1e-7){
    beta_global_min <- beta_true
  } else {
    beta_global_min <- beta_hat
  }
  
  global_min_preds <- X %*% beta_global_min
  min_train_loss_regr <- mean((global_min_preds - y)^2) 
  
  velocity <- rep(0, p)
  
  beta_history <- matrix(NA_real_, nrow = max_iter, ncol = p)
  loss_history <- rep(NA_real_, max_iter)
  l2_diff_history <- rep(NA_real_, max_iter)
  test_loss_history <- rep(NA_real_, max_iter)
  time_history <- rep(NA_real_, max_iter)
  
  start_time <- Sys.time()
  
  for (iter in 1:max_iter) {
    if (step_size_control) {
      learning_rate_iter <- learning_rate * (decay_rate^(iter / max_iter))
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
      train_loss <- mean((train_preds - y)^2) #- min_train_loss_regr
      test_loss <- mean((test_preds - y_test)^2)
      
    } else if (model == "logistic") {
      preds_batch <- X_batch %*% beta
      probs_batch <- pmax(pmin(1 / (1 + exp(-preds_batch)), 1 - 1e-10), 1e-10)
      error <- probs_batch - y_batch
      gradient <- t(X_batch) %*% error / nrow(X_batch)
      
      # Calculate loss using all training data
      train_preds <- X %*% beta
      test_preds <- X_test %*% beta
      train_probs <- pmax(pmin(1 / (1 + exp(-train_preds)), 1 - 1e-10), 1e-10)
      test_probs <- pmax(pmin(1 / (1 + exp(-test_preds)), 1 - 1e-10), 1e-10)
      
      train_loss <- -mean(y * log(train_probs) + (1 - y) * log(1 - train_probs)) #- min_train_loss_log
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
    l2_diff_history[iter] <- as.numeric(log10(sqrt(sum((beta - beta_true)^2))))
    time_history[iter] <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  }
  
  # Final evaluation metrics
  if (model == "linear") {
    train_preds <- X %*% beta
    test_preds <- X_test %*% beta
    train_rmse <- sqrt(mean((train_preds - y)^2))
    test_rmse <- sqrt(mean((test_preds - y_test)^2))
    
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
    cat("Stochastic Gradient Descent:", ifelse(stochastic, "TRUE", "FALSE"), "\n")
    cat("\nPerformance Metrics:\n")
    cat("Training RMSE:", train_rmse, "\n")
    cat("Test RMSE:", test_rmse, "\n")
    cat("Approximate Parameter Optimization Error:", l2_diff_history[length(l2_diff_history)], "\n\n")
    
  } else if (model == "logistic") {
    # Calculate probabilities
    train_probs <- 1 / (1 + exp(-X %*% beta))
    test_probs <- 1 / (1 + exp(-X_test %*% beta))
    
    # Convert probabilities to class predictions (threshold = 0.5)
    train_preds_class <- ifelse(train_probs >= 0.5, 1, 0)
    test_preds_class <- ifelse(test_probs >= 0.5, 1, 0)
    
    # Calculate accuracy
    train_accuracy <- mean(train_preds_class == y)
    test_accuracy <- mean(test_preds_class == y_test)
    
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
    cat("Stochastic Gradient Descent:", ifelse(stochastic, "TRUE", "FALSE"), "\n")
    cat("\nPerformance Metrics:\n")
    cat("Training Accuracy:", sprintf("%.4f", train_accuracy), "\n")
    cat("Test Accuracy:", sprintf("%.4f", test_accuracy), "\n")
    cat("Approximate Parameter Optimization Error:", l2_diff_history[length(l2_diff_history)], "\n")
    cat("Final Cross-Entropy Loss (Train):", loss_history[length(loss_history)], "\n")
    cat("Final Cross-Entropy Loss (Test):", test_loss_history[length(test_loss_history)], "\n\n")
  }
  
  return(list(
    beta = beta,
    beta_history = beta_history,
    loss_history = loss_history,
    test_loss_history = test_loss_history,
    l2_diff_history = l2_diff_history,
    time_history = round(time_history, 5)
  ))
}

################################################################################

################################################################################
# Plotting configs
lwidth = 1
fsize = 14
legendsize = 20

########
plot_optimization_results <- function(loss_histories, test_loss_histories, l2_diff_histories, 
                                      time_histories, method_names, xaxis = "iterations",
                                      methods_to_show = method_names) {
  
  # Plotting configs
  lwidth <- 1
  fsize <- 14
  legendsize <- 20 
  
  # Create iteration indices
  iterations <- 1:length(loss_histories[[1]])
  
  # Prepare data for plotting
  plot_data <- data.frame(
    iteration = rep(iterations, length(method_names)),
    time = unlist(time_histories),
    train_loss = unlist(loss_histories),
    test_loss = unlist(test_loss_histories),
    l2_diff = unlist(l2_diff_histories),
    method = rep(method_names, each = length(iterations))
  )
  
  plot_data$train_loss[!is.finite(plot_data$train_loss)] <- -5
  
  # Filter methods to show
  plot_data <- plot_data[plot_data$method %in% methods_to_show, ]
  
  # Set x-axis based on user choice
  if (xaxis == "time") {
    x_var <- "time"
    x_lab <- "Time (seconds)"
    # Round time to nearest second for breaks
    max_time <- ceiling(max(plot_data$time))
    x_breaks <- seq(0, max_time, by = 0.5)
  } else {
    x_var <- "iteration"
    x_lab <- "Iterations"
    # Use sensible iteration breaks
    max_iter <- max(plot_data$iteration)
    x_breaks <- seq(0, max_iter, by = floor(max_iter / 5))
  }
  
  # Common theme for plots
  common_theme <- theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.spacing.x = unit(1.5, 'cm'),
      text = element_text(size = fsize + 5),
      axis.text = element_text(size = fsize + 1),
      legend.title = element_text(size = legendsize + 2),
      legend.text = element_text(size = legendsize)
    )
  
  # Create the three plots
  p1 <- ggplot(plot_data, aes_string(x = x_var, y = "train_loss", color = "method")) +
    geom_line(linewidth = lwidth) +
    scale_x_continuous(breaks = x_breaks) +
    coord_cartesian(ylim = c(0, min(15, max(plot_data$train_loss + 0.1)))) +
    labs(x = x_lab, y = "Train loss", color = "Method") +
    guides(color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      keywidth = unit(2.5, "cm"),
      keyheight = unit(0.5, "cm"),
      default.unit = "cm"
    )) +
    common_theme
  
  p2 <- ggplot(plot_data, aes_string(x = x_var, y = "test_loss", color = "method")) +
    geom_line(linewidth = lwidth) +
    scale_x_continuous(breaks = x_breaks) +
    coord_cartesian(ylim = c(min(plot_data$test_loss) - 0.1, min(max(plot_data$test_loss) + 0.1, 2))) +
    labs(x = x_lab, y = "Test Loss", color = "Method") +
    guides(color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      keywidth = unit(2.5, "cm"),
      keyheight = unit(0.5, "cm"),
      default.unit = "cm"
    )) +
    common_theme
  
  p3 <- ggplot(plot_data, aes_string(x = x_var, y = "l2_diff", color = "method")) +
    geom_line(linewidth = lwidth) +
    scale_x_continuous(breaks = x_breaks) +
    labs(x = x_lab, y = "Optimization error (log10)", color = "Method") +
    guides(color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      keywidth = unit(1.5, "cm"),
      keyheight = unit(0.5, "cm"),
      default.unit = "cm"
    )) +
    common_theme
  
  # Extract legend
  legend_grob <- ggpubr::get_legend(p1)
  
  # Remove legends from individual plots
  p1 <- p1 + theme(legend.position = "none")
  p2 <- p2 + theme(legend.position = "none")
  p3 <- p3 + theme(legend.position = "none")
  
  # Combine plots
  combined_plots <- ggpubr::ggarrange(
    p1, p2, p3,
    ncol = 3,
    nrow = 1,
    align = "hv"
  )
  
  # Add legend below the combined plots
  final_plot <- ggpubr::ggarrange(
    combined_plots,
    legend_grob,
    ncol = 1,
    heights = c(1, 0.15)  # Adjust the height ratio as needed
  )
  
  return(final_plot)
}


################################################################################

# Function to run a single optimizer variant
run_gd_experiment <- function(X, y, X_test, y_test, beta_true, model_type,
                              learning_rate, max_iter, momentum = NULL,
                              step_size_control = FALSE, stochastic = FALSE,
                              decay_rate = 0) {
  result <- gradient_descent(
    X = X, 
    y = y, 
    X_test = X_test, 
    y_test = y_test, 
    beta_true = beta_true,
    model = model_type,
    learning_rate = learning_rate,
    max_iter = max_iter,
    beta_init = rep(0, ncol(X)),
    momentum = momentum,
    stochastic = stochastic,
    step_size_control = step_size_control,
    decay_rate = decay_rate
  )
  return(result)
}

# Function to run all optimizer variants
run_all_experiments <- function(n = 500, p = 11, rho = 0, sigma = 1, 
                                model_type = "linear", n_test = 5000,
                                learning_rate = 2e-4, max_iter = 10000,
                                momentum_value = 0.5, decay = 0.05,
                                seed = 123, stochastic=FALSE) {
  set.seed(seed)
  
  # Generate data once
  data <- generate_data(n, p, rho, sigma, model_type, n_test)
  beta_true <- data$beta_true
  cond_number <- data$condition_number
  
  # Define optimizers 
  optimizers <- if(stochastic) {
    # Stochastic variants
    list(
      list(name = "SGD", momentum = NULL, step_size_control = FALSE, stochastic=TRUE),
      list(name = "SGD+mom", momentum = momentum_value, step_size_control = FALSE, stochastic=TRUE),
      list(name = "SGD+decay", momentum = NULL, step_size_control = TRUE, stochastic=TRUE),
      list(name = "SGD+mom+decay", momentum = momentum_value, step_size_control = TRUE, stochastic=TRUE)
    )
  } else {
    # Standard GD variants
    list(
      list(name = "GD", momentum = NULL, step_size_control = FALSE, stochastic=FALSE),
      list(name = "GD+mom", momentum = momentum_value, step_size_control = FALSE, stochastic=FALSE),
      list(name = "GD+decay", momentum = NULL, step_size_control = TRUE, stochastic=FALSE),
      list(name = "GD+mom+decay", momentum = momentum_value, step_size_control = TRUE, stochastic=FALSE)
    )
  }
  
  
  # Initialize storage
  histories <- list(
    loss = list(), test_loss = list(), l2_diff = list(),
    beta = list(), time = list(), method_names = c(), beta_true=beta_true, cond_number=cond_number
  )
  
  # Run all optimizer variants
  for (opt in optimizers) {
    result <- run_gd_experiment(
      X = data$X, y = data$y, 
      X_test = data$X_test, y_test = data$y_test,
      beta_true = data$beta_true,
      model_type = model_type,
      learning_rate = learning_rate,
      max_iter = max_iter,
      momentum = opt$momentum,
      step_size_control = opt$step_size_control,
      decay_rate = decay,
      stochastic=opt$stochastic
    )
    
    # Store results
    i <- length(histories$method_names) + 1
    histories$loss[[i]] <- result$loss_history
    histories$test_loss[[i]] <- result$test_loss_history
    histories$l2_diff[[i]] <- result$l2_diff_history
    histories$beta[[i]] <- result$beta_history
    histories$time[[i]] <- result$time_history
    histories$method_names[i] <- opt$name
  }
  
  return(histories)
}

# Example usage:
#results <- run_all_experiments(
#  n = 500, p = 11, rho = 0, sigma = 1, 
#  model_type = "linear", n_test = 5000,
#  learning_rate = 2e-4, max_iter = 10000,
#  momentum_value = 0.5, decay = 0.05,
#  seed = 123, stochastic=FALSE
#)

# Plot results
#plot_optimization_results(
#  loss_histories = results$loss,
#  test_loss_histories = results$test_loss,
#  l2_diff_histories = results$l2_diff,
# Plot coefficient paths for multiple optimization methods side by side

################################################################################

# Function to plot coefficient paths for different optimizers
plot_coef_paths <- function(beta_histories, method_names, beta_true, 
                            methods_to_show = method_names) {
  
  # Create a list to store individual plots
  plots <- list()
  
  # Create coefficient path plot for each method
  for (i in seq_along(method_names)) {
    if (!method_names[i] %in% methods_to_show) next
    
    # Prepare data for plotting
    path <- beta_histories[[i]]
    
    # Add zero values at iteration 0
    zero_row <- matrix(0, nrow = 1, ncol = ncol(path))
    path <- rbind(zero_row, path)
    
    colnames(path) <- paste0("θ", seq_len(ncol(path)))
    path_df <- as.data.frame(path)
    
    # Create sequence from 0 to n, where n is (number of original rows)
    # If original had 10000 rows, this creates sequence 0 to 10000 (10001 values)
    path_df$Iteration <- seq(0, nrow(beta_histories[[i]]))
    
    path_long <- reshape2::melt(path_df, id.vars = "Iteration", 
                                variable.name = "Parameter",
                                value.name = "Value")
    
    # Create data frame for true coefficients
    beta_true_df <- data.frame(
      Iteration = max(path_df$Iteration),
      Parameter = paste0("θ", seq_len(length(beta_true))),
      Value = beta_true
    )
    
    # Create data frame for horizontal reference lines
    ref_lines_df <- data.frame(
      yintercept = beta_true,
      Parameter = paste0("θ", seq_len(length(beta_true)))
    )
    
    # Set y-axis label based on position
    y_lab <- if(i == 1) "Coefficient value" else ""
    
    # Create individual plot
    p <- ggplot() +
      # Add reference lines first (behind the coefficient paths)
      geom_hline(data = ref_lines_df, 
                 aes(yintercept = yintercept),
                 linetype = "dashed", color = "black", alpha = 0.6, linewidth=0.6) +
      # Add coefficient paths
      geom_line(data = path_long, 
                aes(x = Iteration, y = Value, color = Parameter),
                linewidth = 1.1) +
      labs(title = method_names[i],
           x = "Iteration", 
           y = y_lab) +
      theme_minimal() +
      theme(
        text = element_text(size = 20),
        axis.text = element_text(size = 13),
        plot.title = element_text(size = 20),
        legend.position = "none",
        plot.margin = margin(5, 10, 5, 10)
      )
    
    plots[[i]] <- p
  }
  
  # Combine plots
  combined_plot <- ggpubr::ggarrange(
    plotlist = plots[methods_to_show %in% method_names],
    ncol = 4,
    nrow = 1,
    align = "hv"
  )
  
  return(combined_plot)
}