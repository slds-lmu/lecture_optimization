# ------------------------------------------------------------------------------
# multivariate first order
#
# FIG: optimizer comparison figures for synthetic linear and logistic models
# used in slide set 12. This script intentionally generates only the PDFs that
# are referenced from the slides.
# ------------------------------------------------------------------------------

set.seed(123)

library(ggplot2)

source("simu-linmods-functions.R")

# ------------------------------------------------------------------------------

out_dir = "../figure/simu_linmod"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

n = 500
p = 11
sigma = 1
n_test = 5000
momentum_value = 0.8
decay = 0.1
seed = 123

plot_methods = list(
  linear = list(
    gd = c("GD", "GD+decay", "GD+mom", "GD+mom+decay"),
    sgd = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
  ),
  logistic = list(
    gd = c("GD", "GD+decay", "GD+mom", "GD+mom+decay"),
    sgd = c("SGD", "SGD+decay", "SGD+mom", "SGD+mom+decay")
  )
)

experiments = list(
  list(
    iter_file = "GD_reg_small_lr_iters.pdf",
    coef_file = "GD_reg_coef_small.pdf",
    model_type = "linear",
    stochastic = FALSE,
    learning_rate = 3e-4,
    max_iter = 10000,
    rho = 0.0
  ),
  list(
    iter_file = "GD_reg_med_lr_iters.pdf",
    coef_file = "GD_reg_coef_med.pdf",
    model_type = "linear",
    stochastic = FALSE,
    learning_rate = 2e-3,
    max_iter = 10000,
    rho = 0.0
  ),
  list(
    iter_file = "GD_reg_large_lr_iters.pdf",
    coef_file = "GD_reg_coef_large.pdf",
    model_type = "linear",
    stochastic = FALSE,
    learning_rate = 1.5,
    max_iter = 50,
    rho = 0.0
  ),
  list(
    iter_file = "GD_reg_med_lr_corr_iters.pdf",
    coef_file = "GD_reg_coef_med_corr.pdf",
    model_type = "linear",
    stochastic = FALSE,
    learning_rate = 2e-3,
    max_iter = 10000,
    rho = 0.99
  ),
  list(
    iter_file = "SGD_reg_med_lr_iters.pdf",
    coef_file = "SGD_reg_coef_med.pdf",
    model_type = "linear",
    stochastic = TRUE,
    learning_rate = 2e-3,
    max_iter = 10000,
    rho = 0.0
  ),
  list(
    iter_file = "SGD_reg_large_lr_iters.pdf",
    coef_file = "SGD_reg_coef_large.pdf",
    model_type = "linear",
    stochastic = TRUE,
    learning_rate = 1e-2,
    max_iter = 10000,
    rho = 0.0
  ),
  list(
    iter_file = "GD_log_med_lr_iters.pdf",
    coef_file = "GD_log_coef_med.pdf",
    model_type = "logistic",
    stochastic = FALSE,
    learning_rate = 0.25,
    max_iter = 20000,
    rho = 0.0
  ),
  list(
    iter_file = "GD_log_med_lr_corr_iters.pdf",
    coef_file = "GD_log_coef_med_corr.pdf",
    model_type = "logistic",
    stochastic = FALSE,
    learning_rate = 0.25,
    max_iter = 20000,
    rho = 0.99
  ),
  list(
    iter_file = "SGD_log_med_lr_iters.pdf",
    coef_file = "SGD_log_coef_med.pdf",
    model_type = "logistic",
    stochastic = TRUE,
    learning_rate = 3e-2,
    max_iter = 20000,
    rho = 0.0
  ),
  list(
    iter_file = "SGD_log_large_lr_iters.pdf",
    coef_file = "SGD_log_coef_large.pdf",
    model_type = "logistic",
    stochastic = TRUE,
    learning_rate = 3e-1,
    max_iter = 20000,
    rho = 0.0
  )
)

save_experiment_figures = function(cfg) {
  results = run_all_experiments(
    n = n,
    p = p,
    rho = cfg$rho,
    sigma = sigma,
    model_type = cfg$model_type,
    n_test = n_test,
    learning_rate = cfg$learning_rate,
    max_iter = cfg$max_iter,
    momentum_value = momentum_value,
    decay = decay,
    seed = seed,
    stochastic = cfg$stochastic
  )

  methods_to_show = if (cfg$stochastic) {
    plot_methods[[cfg$model_type]]$sgd
  } else {
    plot_methods[[cfg$model_type]]$gd
  }

  iters_plot = plot_optimization_results(
    loss_histories = results$loss,
    test_loss_histories = results$test_loss,
    l2_diff_histories = results$l2_diff,
    time_histories = results$time,
    method_names = results$method_names,
    xaxis = "iterations",
    methods_to_show = methods_to_show
  )

  coef_plot = plot_coef_paths(
    beta_histories = results$beta,
    method_names = results$method_names,
    beta_true = results$beta_true,
    methods_to_show = methods_to_show
  )

  ggsave(
    filename = file.path(out_dir, cfg$iter_file),
    plot = iters_plot,
    width = 15,
    height = 5,
    device = cairo_pdf
  )

  ggsave(
    filename = file.path(out_dir, cfg$coef_file),
    plot = coef_plot,
    width = 15,
    height = 4,
    device = cairo_pdf
  )
}

for (cfg in experiments) {
  save_experiment_figures(cfg)
}
