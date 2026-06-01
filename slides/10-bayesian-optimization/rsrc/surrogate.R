# Used in: lecture_optimization/slides/10-bayesian-optimization/slides-bayesian-optimization-4-surrogate-models.tex
#
# Compares random-forest surrogate variants and benchmarks random search against
# Bayesian optimization with RF, ExtraTrees-style RF, and GP surrogates.

library(bbotk)
library(data.table)
library(ggplot2)
library(mlr3learners)
library(mlr3mbo)
library(mlr3misc)
library(patchwork)

source("bo-helpers.R")
source("LearnerRegrRangerCustom.R")
set.seed(123L)

benchmark_replicates = 2L
rf_num_trees = 80L
acq_optimizer_evals = 15L

make_ranger_learner = function(splitrule, replace, num_random_splits = NULL) {
  learner = LearnerRegrRangerCustom$new()
  learner$param_set$values$num.trees = rf_num_trees
  learner$param_set$values$min.node.size = 1L
  learner$param_set$values$se.method = "simple"
  learner$param_set$values$sample.fraction = 1
  learner$param_set$values$splitrule = splitrule
  learner$param_set$values$replace = replace

  if (!is.null(num_random_splits)) {
    learner$param_set$values$num.random.splits = num_random_splits
  }

  learner
}

objective = make_1d_objective()
instance = make_singlecrit_instance(objective)
instance$eval_batch(data.table(x = c(0.100, 0.300, 0.650, 1.000, 0.348, 0.400, 0.349)))

grid = make_grid(instance)

ranger_variants = list(
  no_bootstrap_no_random_splits = list(
    learner = make_ranger_learner("variance", FALSE),
    title = "No Bootstrap & No Random Splits"
  ),
  bootstrap_no_random_splits = list(
    learner = make_ranger_learner("variance", TRUE),
    title = "Bootstrap & No Random Splits"
  ),
  no_bootstrap_random_splits = list(
    learner = make_ranger_learner("extratrees", FALSE, 1L),
    title = "No Bootstrap & Random Splits"
  ),
  bootstrap_random_splits = list(
    learner = make_ranger_learner("extratrees", TRUE, 1L),
    title = "Bootstrap & Random Splits"
  )
)

plot_ranger_variant = function(variant) {
  surrogate = srlrn(variant$learner$clone(deep = TRUE), archive = instance$archive)
  surrogate$update()

  variant_grid = copy(grid)
  add_prediction_columns(variant_grid, surrogate, "x")

  plot_surrogate_1d(
    grid = variant_grid,
    archive = instance$archive$data,
    y_limits = c(-2, 2.4)
  ) +
    labs(title = variant$title)
}

rf_plots = lapply(ranger_variants, plot_ranger_variant)
save_figure((rf_plots[[1L]] + rf_plots[[2L]]) / (rf_plots[[3L]] + rf_plots[[4L]]),
  "surrogate_1.png",
  width = 10,
  height = 8
)

objective = make_ackley_objective()
instance = make_singlecrit_instance(objective, trm("evals", n_evals = 25L))

run_mbo_trace = function(initial_design, surrogate, method, repl) {
  instance$archive$clear()
  instance$eval_batch(initial_design)

  acq_function = acqf("ei")
  acq_optimizer = acqo(opt("nloptr", algorithm = "NLOPT_GN_DIRECT_L"), trm("evals", n_evals = acq_optimizer_evals))
  optimizer = opt("mbo", surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  optimizer$optimize(instance)

  collect_trace(instance, method, repl)
}

run_replicate = function(repl) {
  initial_design = generate_design_random(instance$search_space, n = 10L)$data

  instance$archive$clear()
  opt("random_search", batch_size = 1L)$optimize(instance)
  random_trace = collect_trace(instance, "Random", repl)

  rf_surrogate = srlrn(ranger_variants$bootstrap_no_random_splits$learner$clone(deep = TRUE))
  rf_et_surrogate = srlrn(ranger_variants$bootstrap_random_splits$learner$clone(deep = TRUE))
  gp_surrogate = make_km_surrogate(
    covtype = "matern3_2",
    optim_method = "BFGS",
    nugget.stability = 1e-8
  )

  bo_rf_trace = run_mbo_trace(initial_design, rf_surrogate, "BO_RF", repl)
  bo_rf_et_trace = run_mbo_trace(initial_design, rf_et_surrogate, "BO_RF_ET", repl)
  bo_gp_trace = run_mbo_trace(initial_design, gp_surrogate, "BO_GP", repl)

  rbind(random_trace, bo_rf_trace, bo_rf_et_trace, bo_gp_trace, fill = TRUE)
}

results = map_dtr(seq_len(benchmark_replicates), run_replicate)
trace_summary = summarize_traces(results)

save_figure(plot_trace_summary(trace_summary), "surrogate_2.png")
