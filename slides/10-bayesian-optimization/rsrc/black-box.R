# Used in: lecture_optimization/slides/10-bayesian-optimization/slides-bayesian-optimization-1-black-box.tex
#
# Creates design-space examples and a small Ackley benchmark comparing random
# search, CMA-ES, and Bayesian optimization.

library(adagio)
library(bbotk)
library(data.table)
library(ggplot2)
library(mlr3learners)
library(mlr3mbo)
library(mlr3misc)

source("bo-helpers.R")
set.seed(123L)

benchmark_replicates = 3L
benchmark_evals = 30L
acq_optimizer_evals = 20L

domain = ps(x1 = p_dbl(lower = 0, upper = 1), x2 = p_dbl(lower = 0, upper = 1))

random_design = generate_design_random(domain, n = 100L)$data
random_design[, method := "random"]

grid_design = generate_design_grid(domain, resolution = 10L)$data
grid_design[, method := "grid"]

save_figure(plot_design_2d(random_design, "Random Design"), "black_box_0.png")
save_figure(plot_design_2d(grid_design, "Grid Design"), "black_box_1.png")

objective = make_ackley_objective()
instance = make_singlecrit_instance(objective, trm("evals", n_evals = benchmark_evals))

run_replicate = function(repl) {
  initial_design = generate_design_random(instance$search_space, n = 10L)$data

  instance$archive$clear()
  opt("random_search", batch_size = 1L)$optimize(instance)
  random_trace = collect_trace(instance, "Random", repl)

  instance$archive$clear()
  opt("cmaes")$optimize(instance)
  cmaes_trace = collect_trace(instance, "CMA-ES", repl)

  instance$archive$clear()
  instance$eval_batch(initial_design)

  surrogate = make_km_surrogate(
    covtype = "matern3_2",
    optim_method = "BFGS",
    nugget.stability = 1e-8
  )
  acq_function = acqf("ei")
  acq_optimizer = acqo(opt("nloptr", algorithm = "NLOPT_GN_DIRECT_L"), trm("evals", n_evals = acq_optimizer_evals))
  optimizer = opt("mbo", surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  optimizer$optimize(instance)
  bo_trace = collect_trace(instance, "BO", repl)

  rbind(random_trace, cmaes_trace, bo_trace, fill = TRUE)
}

results = map_dtr(seq_len(benchmark_replicates), run_replicate)
trace_summary = summarize_traces(results)

save_figure(plot_trace_summary(trace_summary), "black_box_2.png")
