# Used in: slides/05-multivariate-second-order/rsrc/NR_GD.R
#
# Helper functions for the logistic-regression simulation comparing
# Newton-Raphson, relaxed Newton-Raphson, and gradient descent with momentum.
# The helpers generate data, run optimizers, and create the comparison plots.

set.seed(123L)

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(grid)
  library(MASS)
  library(patchwork)
})

method_palette = c(
  "Newton-Raphson" = "#D55E00",
  "Newton-Raphson+step size" = "#0072B2",
  "GD+mom" = "#009E73"
)

make_covariance_matrix = function(n_features, rho) {
  if (rho == 0) {
    diag(n_features)
  } else {
    toeplitz(rho^(seq_len(n_features) - 1L))
  }
}

make_beta_truth = function(n_features) {
  half = floor(n_features / 2L)

  if (n_features %% 2L == 0L) {
    c(seq(-half, -1L), seq(1L, half))
  } else {
    seq(-half, half)
  }
}

clip_probability = function(probability, eps = 1e-10) {
  pmin(pmax(probability, eps), 1 - eps)
}

logistic_loss = function(x, y, beta) {
  probability = clip_probability(plogis(as.vector(x %*% beta)))
  -mean(y * log(probability) + (1 - y) * log(1 - probability))
}

log10_l2_distance = function(beta, beta_target) {
  distance = sqrt(sum((beta - beta_target)^2))
  log10(max(distance, .Machine$double.eps))
}

solve_spd_system = function(matrix, rhs) {
  chol_factor = chol(matrix)
  backsolve(chol_factor, forwardsolve(t(chol_factor), rhs))
}

simulate_logistic_data = function(n_train, n_test, n_features, rho, seed = 123L) {
  set.seed(seed)

  covariance = make_covariance_matrix(n_features, rho)
  beta_truth = make_beta_truth(n_features)
  x_train = mvrnorm(n = n_train, mu = rep(0, n_features), Sigma = covariance)
  x_test = mvrnorm(n = n_test, mu = rep(0, n_features), Sigma = covariance)
  train_probability = plogis(as.vector(x_train %*% beta_truth))
  test_probability = plogis(as.vector(x_test %*% beta_truth))
  y_train = rbinom(n_train, size = 1L, prob = train_probability)
  y_test = rbinom(n_test, size = 1L, prob = test_probability)
  glm_fit = suppressWarnings(glm.fit(x = x_train, y = y_train, family = binomial()))

  list(
    x_train = x_train,
    y_train = y_train,
    x_test = x_test,
    y_test = y_test,
    beta_truth = beta_truth,
    beta_target = as.numeric(glm_fit$coefficients),
    condition_number = kappa(crossprod(x_train))
  )
}

make_history = function(max_iter, n_features) {
  list(
    beta = matrix(NA_real_, nrow = max_iter, ncol = n_features),
    loss = rep(NA_real_, max_iter),
    test_loss = rep(NA_real_, max_iter),
    l2_diff = rep(NA_real_, max_iter),
    time = rep(NA_real_, max_iter)
  )
}

evaluate_logistic_fit = function(x_train, y_train, x_test, y_test, beta, beta_target) {
  list(
    train_loss = logistic_loss(x_train, y_train, beta),
    test_loss = logistic_loss(x_test, y_test, beta),
    l2_diff = log10_l2_distance(beta, beta_target)
  )
}

as_optimizer_result = function(beta, history) {
  list(
    beta = beta,
    beta_history = history$beta,
    loss_history = history$loss,
    test_loss_history = history$test_loss,
    l2_diff_history = history$l2_diff,
    time_history = round(history$time, 5L)
  )
}

store_optimizer_step = function(history, iteration, beta, metrics, start_time) {
  history$beta[iteration, ] = as.numeric(beta)
  history$loss[iteration] = metrics$train_loss
  history$test_loss[iteration] = metrics$test_loss
  history$l2_diff[iteration] = metrics$l2_diff
  history$time[iteration] = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  history
}

gradient_descent_logistic = function(x_train, y_train, x_test, y_test, beta_target, learning_rate, max_iter,
                                     beta_init = NULL, momentum = 0, stochastic = FALSE, decay_rate = 1,
                                     seed = 123L) {
  if (stochastic) {
    set.seed(seed)
  }

  n_train = nrow(x_train)
  n_features = ncol(x_train)
  beta = if (is.null(beta_init)) rep(0, n_features) else beta_init
  velocity = rep(0, n_features)
  history = make_history(max_iter, n_features)
  start_time = Sys.time()

  for (iteration in seq_len(max_iter)) {
    learning_rate_iter = learning_rate * decay_rate^(iteration / max_iter)

    if (stochastic) {
      row_id = sample.int(n_train, size = 1L)
      x_batch = x_train[row_id, , drop = FALSE]
      y_batch = y_train[row_id]
    } else {
      x_batch = x_train
      y_batch = y_train
    }

    probability = plogis(as.vector(x_batch %*% beta))
    gradient = as.vector(crossprod(x_batch, probability - y_batch)) / nrow(x_batch)
    velocity = momentum * velocity - learning_rate_iter * gradient
    beta = beta + velocity
    metrics = evaluate_logistic_fit(x_train, y_train, x_test, y_test, beta, beta_target)
    history = store_optimizer_step(history, iteration, beta, metrics, start_time)
  }

  as_optimizer_result(beta, history)
}

newton_raphson_logistic = function(x_train, y_train, x_test, y_test, beta_target, max_iter,
                                   beta_init = NULL, step_size = 1) {
  n_train = nrow(x_train)
  n_features = ncol(x_train)
  beta = if (is.null(beta_init)) rep(0, n_features) else beta_init
  history = make_history(max_iter, n_features)
  start_time = Sys.time()

  for (iteration in seq_len(max_iter)) {
    probability = plogis(as.vector(x_train %*% beta))
    weights = pmax(probability * (1 - probability), .Machine$double.eps)
    gradient = as.vector(crossprod(x_train, probability - y_train)) / n_train
    hessian = crossprod(x_train, x_train * weights) / n_train
    newton_step = as.vector(solve_spd_system(hessian, gradient))

    beta = beta - step_size * newton_step
    metrics = evaluate_logistic_fit(x_train, y_train, x_test, y_test, beta, beta_target)
    history = store_optimizer_step(history, iteration, beta, metrics, start_time)
  }

  as_optimizer_result(beta, history)
}

pad_optimizer_result = function(result, target_length) {
  current_length = length(result$loss_history)

  if (current_length < target_length) {
    padding_length = target_length - current_length
    result$loss_history = c(result$loss_history, rep(NA_real_, padding_length))
    result$test_loss_history = c(result$test_loss_history, rep(NA_real_, padding_length))
    result$l2_diff_history = c(result$l2_diff_history, rep(NA_real_, padding_length))
    result$time_history = c(result$time_history, rep(NA_real_, padding_length))
    result$beta_history = rbind(
      result$beta_history,
      matrix(NA_real_, nrow = padding_length, ncol = ncol(result$beta_history))
    )
  }

  result
}

run_newton_gd_experiments = function(n_train = 500L, n_features = 11L, rho = 0, n_test = 5000L,
                                     learning_rate_gd = 1, step_size = 0.7, max_iter = 50L,
                                     max_iter_gd = max_iter, seed = 123L) {
  data = simulate_logistic_data(
    n_train = n_train,
    n_test = n_test,
    n_features = n_features,
    rho = rho,
    seed = seed
  )

  optimizers = list(
    list(name = "Newton-Raphson", method = "newton", step_size = 1),
    list(name = "Newton-Raphson+step size", method = "newton", step_size = step_size),
    list(name = "GD+mom", method = "gd", learning_rate = learning_rate_gd, momentum = 0.8)
  )

  histories = list(
    loss = list(),
    test_loss = list(),
    l2_diff = list(),
    beta = list(),
    time = list(),
    method_names = character(),
    beta_true = data$beta_target,
    beta_ground_truth = data$beta_truth,
    cond_number = data$condition_number
  )

  for (optimizer in optimizers) {
    result = if (optimizer$method == "newton") {
      newton_result = newton_raphson_logistic(
        x_train = data$x_train,
        y_train = data$y_train,
        x_test = data$x_test,
        y_test = data$y_test,
        beta_target = data$beta_target,
        max_iter = max_iter,
        beta_init = rep(0, n_features),
        step_size = optimizer$step_size
      )
      pad_optimizer_result(newton_result, max_iter_gd)
    } else {
      gradient_descent_logistic(
        x_train = data$x_train,
        y_train = data$y_train,
        x_test = data$x_test,
        y_test = data$y_test,
        beta_target = data$beta_target,
        learning_rate = optimizer$learning_rate,
        max_iter = max_iter_gd,
        beta_init = rep(0, n_features),
        momentum = optimizer$momentum,
        stochastic = FALSE,
        decay_rate = 1,
        seed = seed
      )
    }

    result_id = length(histories$method_names) + 1L
    histories$loss[[result_id]] = result$loss_history
    histories$test_loss[[result_id]] = result$test_loss_history
    histories$l2_diff[[result_id]] = result$l2_diff_history
    histories$beta[[result_id]] = result$beta_history
    histories$time[[result_id]] = result$time_history
    histories$method_names[result_id] = optimizer$name
  }

  histories
}

make_history_table = function(loss_histories, test_loss_histories, l2_diff_histories, time_histories,
                              method_names) {
  rbindlist(lapply(seq_along(method_names), function(method_id) {
    data.table(
      iteration = seq_along(loss_histories[[method_id]]),
      time = time_histories[[method_id]],
      train_loss = loss_histories[[method_id]],
      test_loss = test_loss_histories[[method_id]],
      l2_diff = l2_diff_histories[[method_id]],
      method = method_names[[method_id]]
    )
  }))
}

make_x_breaks = function(values) {
  finite_values = values[is.finite(values)]

  if (length(finite_values) == 0L) {
    0
  } else {
    pretty(c(0, max(finite_values)), n = 5L)
  }
}

add_method_scale = function() {
  scale_color_manual(values = method_palette, breaks = names(method_palette))
}

comparison_theme = function(base_size = 19) {
  theme_minimal(base_size = base_size) +
    theme(
      legend.position = "bottom",
      legend.spacing.x = unit(1.5, "cm"),
      legend.title = element_text(size = base_size + 3),
      legend.text = element_text(size = base_size + 1)
    )
}

plot_optimization_results = function(loss_histories, test_loss_histories, l2_diff_histories, time_histories,
                                     method_names, xaxis = "iterations", methods_to_show = method_names) {
  xaxis = match.arg(xaxis, c("iterations", "time"))
  plot_data = make_history_table(
    loss_histories = loss_histories,
    test_loss_histories = test_loss_histories,
    l2_diff_histories = l2_diff_histories,
    time_histories = time_histories,
    method_names = method_names
  )
  plot_data = plot_data[method %in% methods_to_show]
  plot_data[!is.finite(train_loss), train_loss := NA_real_]

  if (xaxis == "time") {
    plot_data[, x_value := time]
    x_lab = "Time (seconds)"
  } else {
    plot_data[, x_value := iteration]
    x_lab = "Iterations"
  }

  x_breaks = make_x_breaks(plot_data$x_value)
  train_upper = min(15, max(plot_data$train_loss, na.rm = TRUE) + 0.1)
  test_lower = min(plot_data$test_loss, na.rm = TRUE) - 0.1
  test_upper = min(2, max(plot_data$test_loss, na.rm = TRUE) + 0.1)
  common_theme = comparison_theme()

  train_loss_plot = ggplot(plot_data, aes(x = x_value, y = train_loss, color = method)) +
    geom_line(linewidth = 1) +
    scale_x_continuous(breaks = x_breaks) +
    coord_cartesian(ylim = c(0, train_upper)) +
    add_method_scale() +
    labs(x = x_lab, y = "Train loss", color = "Method") +
    guides(color = guide_legend(nrow = 1L, byrow = TRUE, keywidth = unit(2.5, "cm"))) +
    common_theme

  test_loss_plot = ggplot(plot_data, aes(x = x_value, y = test_loss, color = method)) +
    geom_line(linewidth = 1) +
    scale_x_continuous(breaks = x_breaks) +
    coord_cartesian(ylim = c(test_lower, test_upper)) +
    add_method_scale() +
    labs(x = x_lab, y = "Test loss", color = "Method") +
    guides(color = guide_legend(nrow = 1L, byrow = TRUE, keywidth = unit(2.5, "cm"))) +
    common_theme

  optimization_error_plot = ggplot(plot_data, aes(x = x_value, y = l2_diff, color = method)) +
    geom_line(linewidth = 1) +
    scale_x_continuous(breaks = x_breaks) +
    add_method_scale() +
    labs(x = x_lab, y = "Optimization error (log10)", color = "Method") +
    guides(color = guide_legend(nrow = 1L, byrow = TRUE, keywidth = unit(1.5, "cm"))) +
    common_theme

  train_loss_plot + test_loss_plot + optimization_error_plot +
    plot_layout(ncol = 3L, guides = "collect")
}

make_coefficient_path_table = function(beta_history) {
  path = rbind(matrix(0, nrow = 1L, ncol = ncol(beta_history)), beta_history)
  colnames(path) = paste0("θ", seq_len(ncol(path)))
  path_data = as.data.table(path)
  path_data[, iteration := seq(0L, nrow(beta_history))]
  melt(path_data, id.vars = "iteration", variable.name = "parameter", value.name = "value")
}

plot_coef_paths = function(beta_histories, method_names, beta_true, methods_to_show = method_names) {
  plots = list()

  for (method_id in seq_along(method_names)) {
    method_name = method_names[[method_id]]

    if (!method_name %in% methods_to_show) {
      next
    }

    path_data = make_coefficient_path_table(beta_histories[[method_id]])
    reference_lines = data.table(
      parameter = paste0("θ", seq_along(beta_true)),
      yintercept = beta_true
    )
    y_lab = if (length(plots) == 0L) "Coefficient value" else NULL

    plots[[method_name]] = ggplot() +
      geom_hline(
        data = reference_lines,
        aes(yintercept = yintercept),
        linetype = "dashed",
        color = "black",
        alpha = 0.6,
        linewidth = 0.6
      ) +
      geom_line(data = path_data, aes(x = iteration, y = value, color = parameter), linewidth = 1.1) +
      labs(title = method_name, x = "Iteration", y = y_lab) +
      theme_minimal(base_size = 20) +
      theme(
        axis.text = element_text(size = 13),
        legend.position = "none",
        plot.margin = margin(5, 10, 5, 10)
      )
  }

  wrap_plots(plots, ncol = length(plots))
}

plot_runtime_comparison = function(loss_histories, l2_diff_histories, time_histories, method_names,
                                   methods_to_show = method_names) {
  plot_data = make_history_table(
    loss_histories = loss_histories,
    test_loss_histories = loss_histories,
    l2_diff_histories = l2_diff_histories,
    time_histories = time_histories,
    method_names = method_names
  )
  plot_data = plot_data[method %in% methods_to_show]

  plot_theme = theme_minimal(base_size = 17) +
    theme(
      plot.title = element_text(size = 20, hjust = 0.5),
      axis.text = element_text(size = 16),
      legend.position = "bottom",
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 18)
    )

  train_loss_plot = ggplot(plot_data, aes(x = time, y = train_loss, color = method)) +
    geom_line(linewidth = 1.2) +
    add_method_scale() +
    labs(x = "Seconds", y = "Training loss", title = "Training loss", color = "Method") +
    plot_theme

  optimization_error_plot = ggplot(plot_data, aes(x = time, y = l2_diff, color = method)) +
    geom_line(linewidth = 1.2) +
    add_method_scale() +
    labs(
      x = "Seconds",
      y = "Optimization error (log10)",
      title = "Parameter optimization error",
      color = "Method"
    ) +
    plot_theme

  train_loss_plot + optimization_error_plot +
    plot_layout(ncol = 2L, guides = "collect")
}
