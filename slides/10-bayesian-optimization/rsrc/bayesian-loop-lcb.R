# ------------------------------------------------------------------------------
# bayesian optimization

# FIG: perform Bayesian Optimization using the Lower Confidence Bound (LCB).
#     visualize how the surrogate model, confidence intervals, 
#     and acquisition function evolve over iterations.
# ------------------------------------------------------------------------------

library(bbotk)
library(data.table)
library(mlr3mbo)
library(mlr3learners)
library(ggplot2)
library(patchwork)

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

# only use the four initial data points + 0.370
xdt = data.table(x = c(0.100, 0.300, 0.370, 0.650, 1.000))
instance$eval_batch(xdt)

grid = generate_design_grid(instance$search_space, resolution = 1001L)$data
set(grid, j = "y", value = objective$eval_dt(grid)$y)

surrogate = srlrn(lrn("regr.km", covtype = "matern5_2", optim.method = "BFGS"), archive = instance$archive)
acq_function = acqf("cb", surrogate = surrogate)

grid = generate_design_grid(instance$search_space, resolution = 1001L)$data
set(grid, j = "y", value = objective$eval_dt(grid)$y)

acq_function$surrogate$update()
prediction = surrogate$predict(grid)
set(grid, j = "y_hat", value = prediction$mean)
set(grid, j = "y_min", value = prediction$mean - prediction$se)
set(grid, j = "y_max", value = prediction$mean + prediction$se)

acq_function$update()
set(grid, j = "cb", value = acq_function$eval_dt(grid[, "x"])$acq_cb)
cb_argmin = grid[which.min(cb), ]

# initial design + surrogate prediction + arg max of cb + cb
g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line() +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  geom_line(aes(x = x, y = y_min), colour = "darkred") +
  xlim(c(0, 1)) +
  ylim(c(-2, 3.8)) +
  theme_minimal()

cb = ggplot(aes(x = x, y = cb), data = grid) +
  geom_line(colour = "darkred") +
  geom_point(aes(x = x, y = cb), size = 3L, colour = "darkred", data = cb_argmin) +
  xlim(c(0, 1)) +
  ylim(c(-2, 3.8)) +
  ylab(expression(paste("LCB, ", tau, " = 1"))) +
  theme_minimal()

g / cb
ggsave("../figure_man/bayesian_loop_lcb_0.png", plot = g / cb, width = 5, height = 4)

set(grid, j = "y_min", value = prediction$mean - 5 * prediction$se)
set(grid, j = "y_max", value = prediction$mean + 5 * prediction$se)

acq_function$constants$values$lambda = 5
acq_function$update()
set(grid, j = "cb", value = acq_function$eval_dt(grid[, "x"])$acq_cb)
cb_argmin = grid[which.min(cb), ]

# initial design + surrogate prediction + arg max of cb + cb
g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line() +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  geom_line(aes(x = x, y = y_min), colour = "darkred") +
  xlim(c(0, 1)) +
  ylim(c(-2, 3.8)) +
  theme_minimal()

cb = ggplot(aes(x = x, y = cb), data = grid) +
  geom_line(colour = "darkred") +
  geom_point(aes(x = x, y = cb), size = 3L, colour = "darkred", data = cb_argmin) +
  xlim(c(0, 1)) +
  ylim(c(-2, 3.8)) +
  ylab(expression(paste("LCB, ", tau, " = 5"))) +
  theme_minimal()

g / cb
ggsave("../figure_man/bayesian_loop_lcb_1.png", plot = g / cb, width = 5, height = 4)

set(grid, j = "y_min", value = prediction$mean - 10 * prediction$se)
set(grid, j = "y_max", value = prediction$mean + 10 * prediction$se)

acq_function$constants$values$lambda = 10
acq_function$update()
set(grid, j = "cb", value = acq_function$eval_dt(grid[, "x"])$acq_cb)
cb_argmin = grid[which.min(cb), ]

# initial design + surrogate prediction + arg max of cb + cb
g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line() +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  geom_line(aes(x = x, y = y_min), colour = "darkred") +
  xlim(c(0, 1)) +
  ylim(c(-2, 3.8)) +
  theme_minimal()

cb = ggplot(aes(x = x, y = cb), data = grid) +
  geom_line(colour = "darkred") +
  geom_point(aes(x = x, y = cb), size = 3L, colour = "darkred", data = cb_argmin) +
  xlim(c(0, 1)) +
  ylim(c(-2, 3.8)) +
  ylab(expression(paste("LCB, ", tau, " = 10"))) +
  theme_minimal()

g / cb
ggsave("../figure_man/bayesian_loop_lcb_2.png", plot = g / cb, width = 5, height = 4)

