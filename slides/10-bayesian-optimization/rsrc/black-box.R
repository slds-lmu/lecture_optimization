library(bbotk)
library(data.table)
library(mlr3mbo)
library(mlr3learners)
library(mlr3misc)
library(ggplot2)
library(pammtools)
library(patchwork)

set.seed(123)
domain = ps(x1 = p_dbl(lower = 0, upper = 1), x2 = p_dbl(lower = 0, upper = 1))

xdt_random = generate_design_random(domain, n = 100L)$data
xdt_random[, method := "random"]
xdt_gs = generate_design_grid(domain, resolution = 10L)$data
xdt_gs[, method := "gs"]
xdt = rbind(xdt_random, xdt_gs)

g = ggplot(aes(x = x1, y = x2), data = xdt[method == "random"]) +
  geom_point(size = 3L) +
  geom_rug() +
  labs(title = "Random Design", x = expression(x[1]), y = expression(x[2])) +
  theme_minimal()

ggsave(file.path("../figure_man/black_box_0.png"), plot = g, width = 5, height = 4)

g = ggplot(aes(x = x1, y = x2), data = xdt[method == "gs"]) +
  geom_point(size = 3L) +
  geom_rug() +
  labs(title = "Grid Design", x = expression(x[1]), y = expression(x[2])) +
  theme_minimal()

ggsave(file.path("../figure_man/black_box_1.png"), plot = g, width = 5, height = 4)

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
  opt("cmaes")$optimize(instance)
  cmaes = copy(instance$archive$data)
  cmaes[, best := cummin(y)]
  cmaes[, method := "CMAES"]
  cmaes[, iter := seq_len(.N)]
  cmaes[, repl := i]

  instance$archive$clear()
  instance$eval_batch(design)
  surrogate = srlrn(lrn("regr.km", covtype = "matern3_2", optim.method = "gen", control = list(trace = FALSE), nugget.stability = 10^-8))
  acq_function = acqf("ei")
  acq_optimizer = acqo(opt("nloptr", algorithm = "NLOPT_GN_DIRECT_L"), trm("evals", n_evals = 100L))
  optimizer = opt("mbo", surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)
  optimizer$optimize(instance)
  mbo = copy(instance$archive$data)
  mbo[, best := cummin(y)]
  mbo[, method := "BO"]
  mbo[, iter := seq_len(.N)]
  mbo[, repl := i]

  rbind(rs, cmaes, mbo, fill = TRUE)
})

agg = results[, .(mean_best = mean(best), se_best = sd(best) / sqrt(.N)), by = .(iter, method)]

g = ggplot(aes(x = iter, y = mean_best, colour = method, fill = method), data = agg) +
  geom_step() +
  geom_stepribbon(aes(x = iter, min = mean_best - se_best, max = mean_best + se_best), colour = NA, alpha = 0.25) +
  labs(x = "Nr. Function Evaluations", y = "Best Objective Value", colour = "Method", fill = "Method") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path("../figure_man/black_box_2.png"), plot = g, width = 5, height = 4)

