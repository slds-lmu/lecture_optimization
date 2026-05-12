# ------------------------------------------------------------------------------
# bayesian optimization

# FIG: perform Bayesian optimization while considering noisy function evaluations.
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

xdt = data.table(x = c(0.1, 0.3, 0.65, 1))
instance$eval_batch(xdt)
instance$archive$data[, y := y + rnorm(4L, sd = 0.5)]

grid = generate_design_grid(instance$search_space, resolution = 1001L)$data
set(grid, j = "y", value = objective$eval_dt(grid)$y)

g = ggplot() +
  geom_line(aes(x = x, y = y), data = grid) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

ggsave(file.path("../figure_man/noisy_0.png"), plot = g, width = 5, height = 4)

surrogate = srlrn(lrn("regr.km", covtype = "matern5_2", optim.method = "BFGS", nugget.stability = 10^-8), archive = instance$archive)
surrogate$update()
prediction = surrogate$predict(grid)

surrogate_noisy = srlrn(lrn("regr.km", covtype = "matern5_2", optim.method = "BFGS", nugget = 0.01, jitter = 1e-12), archive = instance$archive)
surrogate_noisy$update()
prediction_noisy = surrogate_noisy$predict(grid)

set(grid, j = "y_hat", value = prediction$mean)
set(grid, j = "y_min", value = prediction$mean - prediction$se)
set(grid, j = "y_max", value = prediction$mean + prediction$se)

set(grid, j = "y_noisy_hat", value = prediction_noisy$mean)
set(grid, j = "y_noisy_min", value = prediction_noisy$mean - prediction_noisy$se)
set(grid, j = "y_noisy_max", value = prediction_noisy$mean + prediction_noisy$se)

g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line() +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.4)) +
  theme_minimal()

g_noisy = ggplot(aes(x = x, y = y), data = grid) +
  geom_line() +
  geom_line(aes(x = x, y = y_noisy_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_noisy_min, max = y_noisy_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.4)) +
  theme_minimal()

ggsave(file.path("../figure_man/noisy_1.png"), plot = g, width = 5, height = 4)

ggsave(file.path("../figure_man/noisy_2.png"), plot = g_noisy, width = 5, height = 4)

acq_function = acqf("aei", surrogate = surrogate_noisy)
acq_function$surrogate$update()
prediction = surrogate_noisy$predict(grid)
set(grid, j = "y_noisy_hat", value = prediction$mean)
set(grid, j = "y_noisy_min", value = prediction$mean - prediction$se)
set(grid, j = "y_noisy_max", value = prediction$mean + prediction$se)

acq_function$update()
set(grid, j = "aei", value = acq_function$eval_dt(grid[, "x"])$acq_aei)
aei_argmax = grid[which.max(aei), ]

g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line() +
  geom_line(aes(x = x, y = y_noisy_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_noisy_min, max = y_noisy_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.4)) +
  theme_minimal()

aei = ggplot(aes(x = x, y = aei), data = grid) +
  geom_line(colour = "darkred") +
  geom_point(aes(x = x, y = aei), size = 3L, colour = "darkred", data = aei_argmax) +
  xlim(c(0, 1)) +
  ylab("AEI") +
  theme_minimal()

ggsave(file.path("../figure_man/noisy_3.png"), plot = g / aei, width = 5, height = 4)

rp_instance = instance$clone(deep = TRUE)
rp_instance$archive$data[, y := surrogate_noisy$predict(instance$archive$data[, instance$archive$cols_x, with = FALSE])$mean]

acq_function = acqf("ei", surrogate = surrogate)

acq_function$surrogate$archive = rp_instance$archive
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
  geom_line(aes(x = x, y = y_noisy_hat), colour = "grey", linetype = 2) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "grey", data = rp_instance$archive$data) +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.4)) +
  theme_minimal()

ei = ggplot(aes(x = x, y = ei), data = grid) +
  geom_line(colour = "darkred") +
  geom_point(aes(x = x, y = ei), size = 3L, colour = "darkred", data = ei_argmax) +
  xlim(c(0, 1)) +
  ylab("EI") +
  theme_minimal()

ggsave(file.path("../figure_man/noisy_4.png"), plot = g / ei, width = 5, height = 4)

final_instance = instance$clone(deep = TRUE)
final_instance$archive$clear()
final_instance$eval_batch(data.table(x = c(0.1, 0.3, 0.65, 0.8, 1)))
final_instance$archive$data[, y.noisy := y + c(-0.1, -0.35, 0.3, 0.7, -0.05)]

g = ggplot() +
  geom_line(aes(x = x, y = y), data = grid) +
  geom_segment(aes(x = x, xend = x, y = y, yend = y.noisy), color = "orange", data = final_instance$archive$data) +
  geom_point(aes(x = x, y = y.noisy), size = 3L, colour = "black", data = final_instance$archive$data) +
  geom_text(aes(x = 0.15, y = -1, label = "Overrated"), color = "orange") +
  geom_text(aes(x = 0.75, y = 1.5, label = "Underrated"), color = "orange") +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

ggsave(file.path("../figure_man/noisy_5.png"), plot = g, width = 5, height = 4)

