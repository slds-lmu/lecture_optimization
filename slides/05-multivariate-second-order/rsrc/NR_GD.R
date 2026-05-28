# Used in: slides/05-multivariate-second-order/slides-multivar-second-order-5-comparison.tex
#
# Runs the logistic-regression comparison between Newton-Raphson, relaxed
# Newton-Raphson, and gradient descent with momentum. One configuration object
# drives all slide figures to keep the simulation parameters consistent.

set.seed(123L)

suppressPackageStartupMessages(library(ggplot2))

source("simu-linmods-functions.R")

output_dir = "../figure"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

save_plot = function(plot, filename, width, height) {
  ggsave(
    filename = file.path(output_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

methods_to_show = c("Newton-Raphson", "Newton-Raphson+step size", "GD+mom")

simulation_defaults = list(
  n_train = 500L,
  n_features = 11L,
  n_test = 5000L,
  learning_rate_gd = 1,
  step_size = 0.7,
  seed = 123L
)

experiments = list(
  list(
    rho = 0,
    max_iter = 50L,
    optimization_file = "NR_GD_log_indep_50iters",
    coefficient_file = "NR_GD_log_coef_50indep",
    runtime_file = "NR_GD_runtime_comparison"
  ),
  list(
    rho = 0,
    max_iter = 1000L,
    optimization_file = "NR_GD_log_indep_1000iters",
    coefficient_file = "NR_GD_log_coef_1000indep",
    runtime_file = NULL
  ),
  list(
    rho = 0.99,
    max_iter = 50L,
    optimization_file = "NR_GD_log_indep_50iters_corr",
    coefficient_file = "NR_GD_log_coef_50indep_corr",
    runtime_file = "NR_GD_runtime_comparison_corr"
  )
)

run_experiment = function(experiment) {
  results = run_newton_gd_experiments(
    n_train = simulation_defaults$n_train,
    n_features = simulation_defaults$n_features,
    rho = experiment$rho,
    n_test = simulation_defaults$n_test,
    learning_rate_gd = simulation_defaults$learning_rate_gd,
    step_size = simulation_defaults$step_size,
    max_iter = experiment$max_iter,
    max_iter_gd = experiment$max_iter,
    seed = simulation_defaults$seed
  )

  optimization_plot = plot_optimization_results(
    loss_histories = results$loss,
    test_loss_histories = results$test_loss,
    l2_diff_histories = results$l2_diff,
    time_histories = results$time,
    method_names = results$method_names,
    xaxis = "iterations",
    methods_to_show = methods_to_show
  )
  save_plot(plot = optimization_plot, filename = experiment$optimization_file, width = 15, height = 5)

  coefficient_plot = plot_coef_paths(
    beta_histories = results$beta,
    method_names = results$method_names,
    beta_true = results$beta_true,
    methods_to_show = methods_to_show
  )
  save_plot(plot = coefficient_plot, filename = experiment$coefficient_file, width = 15, height = 4)

  if (!is.null(experiment$runtime_file)) {
    runtime_plot = plot_runtime_comparison(
      loss_histories = results$loss,
      l2_diff_histories = results$l2_diff,
      time_histories = results$time,
      method_names = results$method_names,
      methods_to_show = methods_to_show
    )
    save_plot(plot = runtime_plot, filename = experiment$runtime_file, width = 15, height = 6)
  }

  invisible(results)
}

results = lapply(experiments, run_experiment)
