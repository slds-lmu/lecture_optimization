# ------------------------------------------------------------------------------
# bayesian optimization

# FIG: perform Bayesian Optimization (BO) using Expected Improvement (EI). 
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

xdt_old = data.table(x = c(0.100, 0.300, 0.650, 1.000, 0.348, 0.400, 0.349))
instance$eval_batch(xdt_old)

grid = generate_design_grid(instance$search_space, resolution = 1001L)$data
set(grid, j = "y", value = objective$eval_dt(grid)$y)

g = ggplot() +
  geom_line(aes(x = x, y = y), data = grid) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  geom_rect(aes(xmin = c(0.25), xmax = c(0.45), ymin = - Inf, ymax = Inf, fill = c("well explored")), data = data.table(), alpha = 0.2, fill = "lightblue") +
  geom_rect(aes(xmin = c(0.7), xmax = c(0.9), ymin = - Inf, ymax = Inf, fill = c("insufficiently explored")), data = data.table(), alpha = 0.2, fill = "red") +
  geom_text(aes(x = c(0.35), y = -1.8, label = c("well explored")), data = data.table(), color = "lightblue4") +
  geom_text(aes(x = c(0.8), y = -1.8, label = c("insufficiently explored")), data = data.table(), color = "indianred4") +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

ggsave("../figure_man/bayesian_loop_ee.png", plot = g, width = 5, height = 4)

surrogate = srlrn(lrn("regr.km", covtype = "matern5_2", optim.method = "BFGS"), archive = instance$archive)
acq_function = acqf("ei", surrogate = surrogate)

grid = generate_design_grid(instance$search_space, resolution = 1001L)$data
set(grid, j = "y", value = objective$eval_dt(grid)$y)

acq_function$surrogate$update()
prediction = surrogate$predict(grid)
set(grid, j = "y_hat", value = prediction$mean)
set(grid, j = "y_min", value = prediction$mean - prediction$se)
set(grid, j = "y_max", value = prediction$mean + prediction$se)

acq_function$update()
set(grid, j = "ei", value = acq_function$eval_dt(grid[, "x"])$acq_ei)
ei_argmax = grid[which.max(ei), ]

# intial design + surrogate prediction
g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line() +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

ggsave("../figure_man/bayesian_loop_sm.png", plot = g, width = 5, height = 4)

# intial design + surrogate prediction + best
g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line() +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "#00A64F", data = instance$archive$best()) +  
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

ggsave("../figure_man/bayesian_loop_sm_fmin.png", plot = g, width = 5, height = 4)

# intial design + surrogate prediction + normal
ei_argmax_normal = data.table(y = seq(-2, 2.2, by = 0.01))
set(ei_argmax_normal, j = "x", value = dnorm(ei_argmax_normal$y, mean = ei_argmax$y_hat, sd = ei_argmax$y_max - ei_argmax$y_hat))
set(ei_argmax_normal, j = "x", value = (ei_argmax_normal$x / max(ei_argmax_normal$x)) / 10)
g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line(aes(x = x, y = y), data = grid) +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  geom_path(aes(x = x, y = y), data = ei_argmax_normal, colour = "darkgrey") +
  geom_point(aes(x = x, y = y_hat), data = ei_argmax, size = 3L, colour = "darkgrey") +
  geom_segment(aes(y = ei_argmax$y_hat, yend = ei_argmax$y_hat, x = 0, xend = 0.1), colour = "darkgrey") +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

ggsave("../figure_man/bayesian_loop_sm_normal.png", plot = g, width = 5, height = 4)

# intial design + surrogate prediction + best + normal
ei_argmax_normal = data.table(y = seq(-2, 2.2, by = 0.01))
set(ei_argmax_normal, j = "x", value = dnorm(ei_argmax_normal$y, mean = ei_argmax$y_hat, sd = ei_argmax$y_max - ei_argmax$y_hat))
set(ei_argmax_normal, j = "x", value = (ei_argmax_normal$x / max(ei_argmax_normal$x)) / 10)
g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line(aes(x = x, y = y), data = grid) +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "#00A64F", data = instance$archive$best()) +
  geom_hline(yintercept = instance$archive$best()$y, colour = "#00A64F", linetype = 2) +
  geom_path(aes(x = x, y = y), data = ei_argmax_normal, colour = "darkgrey") +
  geom_point(aes(x = x, y = y_hat), data = ei_argmax, size = 3L, colour = "darkgrey") +
  geom_segment(aes(y = ei_argmax$y_hat, yend = ei_argmax$y_hat, x = 0, xend = 0.1), colour = "darkgrey") +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

ggsave("../figure_man/bayesian_loop_sm_normal_fmin.png", plot = g, width = 5, height = 4)

# initial design + surrogate prediction + arg max of ei + ei

# only use the four initial data points + 0.370
instance$archive$clear()
xdt = data.table(x = c(0.100, 0.300, 0.370, 0.650, 1.000))
instance$eval_batch(xdt)

acq_function$surrogate$update()
prediction = surrogate$predict(grid)
set(grid, j = "y_hat", value = prediction$mean)
set(grid, j = "y_min", value = prediction$mean - prediction$se)
set(grid, j = "y_max", value = prediction$mean + prediction$se)

acq_function$update()
set(grid, j = "ei", value = acq_function$eval_dt(grid[, "x"])$acq_ei)
ei_argmax = grid[which.max(ei), ]

g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line() +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

ei = ggplot(aes(x = x, y = ei), data = grid) +
  geom_line(colour = "darkred") +
  geom_point(aes(x = x, y = ei), size = 3L, colour = "darkred", data = ei_argmax) +
  xlim(c(0, 1)) +
  ylab("EI") +
  theme_minimal()

ggsave("../figure_man/bayesian_loop_1.png", plot = g / ei, width = 5, height = 4)

old_ei_argmax = ei_argmax

instance$eval_batch(ei_argmax[, "x", with = FALSE])

for (i in 2:6) {
  acq_function$surrogate$update()
  prediction = surrogate$predict(grid)
  set(grid, j = "y_hat", value = prediction$mean)
  set(grid, j = "y_min", value = prediction$mean - prediction$se)
  set(grid, j = "y_max", value = prediction$mean + prediction$se)
  
  acq_function$update()
  set(grid, j = "ei", value = acq_function$eval_dt(grid[, "x"])$acq_ei)
  ei_argmax = grid[which.max(ei), ]

  # initial design + surrogate prediction + arg max of ei + ei
  g = ggplot(aes(x = x, y = y), data = grid) +
    geom_line() +
    geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
    geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
    geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
    geom_point(aes(x = x, y = y), size = 3L, colour = "grey", data = old_ei_argmax) +
    xlim(c(0, 1)) +
    ylim(c(-2, 2.2)) +
    theme_minimal()
  
  ei = ggplot(aes(x = x, y = ei), data = grid) +
    geom_line(colour = "darkred") +
    geom_point(aes(x = x, y = ei), size = 3L, colour = "darkred", data = ei_argmax) +
    xlim(c(0, 1)) +
    ylab("EI") +
    theme_minimal()
  
  ggsave(sprintf("../figure_man/bayesian_loop_%i.png", i), plot = g / ei, width = 5, height = 4)

  old_ei_argmax = ei_argmax
  
  instance$eval_batch(ei_argmax[, "x", with = FALSE])
}
