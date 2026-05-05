# ------------------------------------------------------------------------------
# bayesian optimization

# FIG: compare different surrogate models for Bayesian optimization 
#      using Gaussian Process Regression (GP) and Random Forest (RF).
# ------------------------------------------------------------------------------

library(bbotk)
library(data.table)
library(mlr3mbo)
library(mlr3learners)
library(ggplot2)
library(patchwork)
library(R6)
library(checkmate)
library(pammtools)
library(mlr3misc)

source("LearnerRegrRangerCustom.R")

set.seed(123)

# ------------------------------------------------------------------------------

objective = ObjectiveRFunDt$new(
 fun = function(xdt) data.table(y = 2 * xdt$x * sin(14 * xdt$x)),
 domain = ps(x = p_dbl(lower = 0, upper = 1)),
 codomain = ps(y = p_dbl(tags = "minimize"))
)
instance = OptimInstanceSingleCrit$new(
  objective = objective,
  terminator = trm("none")
)

xdt_old = data.table(x = c(0.100, 0.300, 0.650, 1.000, 0.348, 0.400, 0.349))
instance$eval_batch(xdt_old)

grid = generate_design_grid(instance$search_space, resolution = 1001L)$data
set(grid, j = "y", value = objective$eval_dt(grid)$y)

gp52 = srlrn(lrn("regr.km", covtype = "matern5_2", optim.method = "BFGS"), archive = instance$archive)
gp52$update()

prediction_gp52 = gp52$predict(grid)
grid_gp52 = copy(grid)
set(grid_gp52, j = "y_hat", value = prediction_gp52$mean)
set(grid_gp52, j = "y_min", value = prediction_gp52$mean - prediction_gp52$se)
set(grid_gp52, j = "y_max", value = prediction_gp52$mean + prediction_gp52$se)

g = ggplot(aes(x = x, y = y), data = grid_gp52) +
  geom_line() +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

ggsave(file.path("../figure_man/surrogate_0.png"), plot = g, width = 5, height = 4)

ranger = LearnerRegrRangerCustom$new()
ranger$param_set$values$num.trees = 1000L
ranger$param_set$values$min.node.size = 1L
ranger$param_set$values$se.method = "simple"
ranger$param_set$values$sample.fraction = 1

ranger1 = ranger$clone(deep = TRUE)
ranger1$param_set$values$splitrule = "variance"
ranger1$param_set$values$replace = FALSE
ranger2 = ranger$clone(deep = TRUE)
ranger2$param_set$values$splitrule = "variance"
ranger2$param_set$values$replace = TRUE
ranger3 = ranger$clone(deep = TRUE)
ranger3$param_set$values$splitrule = "extratrees"
ranger3$param_set$values$num.random.splits = 1L
ranger3$param_set$values$replace = FALSE
ranger4 = ranger$clone(deep = TRUE)
ranger4$param_set$values$splitrule = "extratrees"
ranger4$param_set$values$num.random.splits = 1L
ranger4$param_set$values$replace = TRUE

rf1 = srlrn(ranger1, archive = instance$archive)
rf1$update()
rf2 = srlrn(ranger2, archive = instance$archive)
rf2$update()
rf3 = srlrn(ranger3, archive = instance$archive)
rf3$update()
rf4 = srlrn(ranger4, archive = instance$archive)
rf4$update()

prediction_rf1 = rf1$predict(grid)
prediction_rf2 = rf2$predict(grid)
prediction_rf3 = rf3$predict(grid)
prediction_rf4 = rf4$predict(grid)

grid_rf1 = copy(grid)
grid_rf2 = copy(grid)
grid_rf3 = copy(grid)
grid_rf4 = copy(grid)

set(grid_rf1, j = "y_hat", value = prediction_rf1$mean)
set(grid_rf1, j = "y_min", value = prediction_rf1$mean - prediction_rf1$se)
set(grid_rf1, j = "y_max", value = prediction_rf1$mean + prediction_rf1$se)

set(grid_rf2, j = "y_hat", value = prediction_rf2$mean)
set(grid_rf2, j = "y_min", value = prediction_rf2$mean - prediction_rf2$se)
set(grid_rf2, j = "y_max", value = prediction_rf2$mean + prediction_rf2$se)

set(grid_rf3, j = "y_hat", value = prediction_rf3$mean)
set(grid_rf3, j = "y_min", value = prediction_rf3$mean - prediction_rf3$se)
set(grid_rf3, j = "y_max", value = prediction_rf3$mean + prediction_rf3$se)

set(grid_rf4, j = "y_hat", value = prediction_rf4$mean)
set(grid_rf4, j = "y_min", value = prediction_rf4$mean - prediction_rf4$se)
set(grid_rf4, j = "y_max", value = prediction_rf4$mean + prediction_rf4$se)

g_rf1 = ggplot(aes(x = x, y = y), data = grid_rf1) +
  geom_line() +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.4)) +
  labs(title = "No Bootstrap & No Random Splits") +
  theme_minimal()

g_rf2 = ggplot(aes(x = x, y = y), data = grid_rf2) +
  geom_line() +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.4)) +
  labs(title = "Bootstrap & No Random Splits") +
  theme_minimal()

g_rf3 = ggplot(aes(x = x, y = y), data = grid_rf3) +
  geom_line() +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.4)) +
  labs(title = "No Bootstrap & Random Splits") +
  theme_minimal()

g_rf4 = ggplot(aes(x = x, y = y), data = grid_rf4) +
  geom_line() +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.4)) +
  labs(title = "Bootstrap & Random Splits") +
  theme_minimal()

ggsave(file.path("../figure_man/surrogate_1.png"), plot = (g_rf1 + g_rf2) / (g_rf3 + g_rf4), width = 10, height = 8)

objective = ObjectiveRFunDt$new(
 fun = function(xdt) data.table(y = -20.0 * exp(-0.2 * sqrt(0.5 * (xdt$x1^2 + xdt$x2^2))) - 
                                    exp(0.5 * (cos(2 * pi * xdt$x1) + cos(2 * pi * xdt$x2))) + exp(1) + 20),
 domain = ps(x1 = p_dbl(lower = -5, upper = 5), x2 = p_dbl(lower = -5, upper = 5)),
 codomain = ps(y = p_dbl(tags = "minimize"))
)
instance = OptimInstanceSingleCrit$new(
  objective = objective,
  terminator = trm("evals", n_evals = 50L)
)

results = map_dtr(1:10, function(i) {
  design = generate_design_random(instance$search_space, n= 10L)$data

  instance$archive$clear()
  opt("random_search", batch_size = 1L)$optimize(instance)
  rs = copy(instance$archive$data)
  rs[, best := cummin(y)]
  rs[, method := "Random"]
  rs[, iter := seq_len(.N)]
  rs[, repl := i]

  instance$archive$clear()
  instance$eval_batch(design)
  surrogate = rf2
  acq_function = acqf("ei")
  acq_optimizer = acqo(opt("nloptr", algorithm = "NLOPT_GN_DIRECT_L"), trm("evals", n_evals = 100L))
  optimizer = opt("mbo", surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  optimizer$optimize(instance)
  mbo_rf = copy(instance$archive$data)
  mbo_rf[, best := cummin(y)]
  mbo_rf[, method := "BO_RF"]
  mbo_rf[, iter := seq_len(.N)]
  mbo_rf[, repl := i]

  instance$archive$clear()
  instance$eval_batch(design)
  surrogate = rf4
  acq_function = acqf("ei")
  acq_optimizer = acqo(opt("nloptr", algorithm = "NLOPT_GN_DIRECT_L"), trm("evals", n_evals = 100L))
  optimizer = opt("mbo", surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  optimizer$optimize(instance)
  mbo_rf_et = copy(instance$archive$data)
  mbo_rf_et[, best := cummin(y)]
  mbo_rf_et[, method := "BO_RF_ET"]
  mbo_rf_et[, iter := seq_len(.N)]
  mbo_rf_et[, repl := i]

  instance$archive$clear()
  instance$eval_batch(design)
  surrogate = srlrn(lrn("regr.km", covtype = "matern3_2", optim.method = "gen", control = list(trace = FALSE), nugget.stability = 10^-8))
  acq_function = acqf("ei")
  acq_optimizer = acqo(opt("nloptr", algorithm = "NLOPT_GN_DIRECT_L"), trm("evals", n_evals = 100L))
  optimizer = opt("mbo", surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  optimizer$optimize(instance)
  mbo_gp = copy(instance$archive$data)
  mbo_gp[, best := cummin(y)]
  mbo_gp[, method := "BO_GP"]
  mbo_gp[, iter := seq_len(.N)]
  mbo_gp[, repl := i]

  rbind(rs, mbo_rf, mbo_rf_et, mbo_gp, fill = TRUE)
})

agg = results[, .(mean_best = mean(best), se_best = sd(best) / sqrt(.N)), by = .(iter, method)]

g = ggplot(aes(x = iter, y = mean_best, colour = method, fill = method), data = agg) +
  geom_step() +
  geom_stepribbon(aes(x = iter, min = mean_best - se_best, max = mean_best + se_best), colour = NA, alpha = 0.25) +
  labs(x = "Nr. Function Evaluations", y = "Best Objective Value", colour = "Method", fill = "Method") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path("../figure_man/surrogate_2.png"), plot = g, width = 5, height = 4)

