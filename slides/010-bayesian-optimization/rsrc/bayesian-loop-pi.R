library(bbotk)
library(data.table)
library(mlr3mbo)
library(mlr3learners)
library(ggplot2)
library(patchwork)

set.seed(123)
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

surrogate = srlrn(lrn("regr.km", covtype = "matern5_2", optim.method = "BFGS"), archive = instance$archive)
acq_function = acqf("pi", surrogate = surrogate)

grid = generate_design_grid(instance$search_space, resolution = 1001L)$data
set(grid, j = "y", value = objective$eval_dt(grid)$y)

acq_function$surrogate$update()
prediction = surrogate$predict(grid)
set(grid, j = "y_hat", value = prediction$mean)
set(grid, j = "y_min", value = prediction$mean - prediction$se)
set(grid, j = "y_max", value = prediction$mean + prediction$se)

# x = 0
pi_normal = data.table(y = seq(-2, 2.2, length.out = 1001L))
pi_normal[, d := dnorm(y, mean = prediction[1L, ]$mean, sd = prediction[1L, ]$se)]

g = ggplot(aes(x = y, y = d), data = pi_normal) +
  geom_area(data = pi_normal[y <= instance$archive$best()$y]) +
  geom_line(colour = "darkgrey") +
  geom_vline(xintercept = instance$archive$best()$y, colour = "#00A64F", linetype = 2) +
  labs(x = "Y(x)", y = "Density") +
  theme_minimal()

ggsave(file.path("../figure_man/bayesian_loop_pi_0.png"), plot = g, width = 5, height = 4)

acq_function$update()
set(grid, j = "pi", value = acq_function$eval_dt(grid[, "x"])$acq_pi)
pi_argmax = grid[which.max(pi), ]

# initial design + surrogate prediction + arg max of pi + pi

# only use the four initial data points + 0.37
instance$archive$clear()
xdt = data.table(x = c(0.100, 0.300, 0.370, 0.650, 1.000))
instance$eval_batch(xdt)

acq_function$surrogate$update()
prediction = surrogate$predict(grid)
set(grid, j = "y_hat", value = prediction$mean)
set(grid, j = "y_min", value = prediction$mean - prediction$se)
set(grid, j = "y_max", value = prediction$mean + prediction$se)

acq_function$update()
set(grid, j = "pi", value = acq_function$eval_dt(grid[, "x"])$acq_pi)
pi_argmax = grid[which.max(pi), ]

g = ggplot(aes(x = x, y = y), data = grid) +
  geom_line() +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
  geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
  geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
  xlim(c(0, 1)) +
  ylim(c(-2, 2.2)) +
  theme_minimal()

pi = ggplot(aes(x = x, y = pi), data = grid) +
  geom_line(colour = "darkred") +
  geom_point(aes(x = x, y = pi), size = 3L, colour = "darkred", data = pi_argmax) +
  xlim(c(0, 1)) +
  ylab("PI") +
  theme_minimal()

ggsave(file.path("../figure_man/bayesian_loop_pi_1.png"), plot = g / pi, width = 5, height = 4)

old_pi_argmax = pi_argmax

instance$eval_batch(pi_argmax[, "x", with = FALSE])

for (i in 2:9) {
  acq_function$surrogate$update()
  prediction = surrogate$predict(grid)
  set(grid, j = "y_hat", value = prediction$mean)
  set(grid, j = "y_min", value = prediction$mean - prediction$se)
  set(grid, j = "y_max", value = prediction$mean + prediction$se)
  
  acq_function$update()
  set(grid, j = "pi", value = acq_function$eval_dt(grid[, "x"])$acq_pi)
  pi_argmax = grid[which.max(pi), ]

  # initial design + surrogate prediction + arg max of pi + pi
  g = ggplot(aes(x = x, y = y), data = grid) +
    geom_line() +
    geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2) +
    geom_ribbon(aes(min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1) +
    geom_point(aes(x = x, y = y), size = 3L, colour = "black", data = instance$archive$data) +
    geom_point(aes(x = x, y = y), size = 3L, colour = "grey", data = old_pi_argmax) +
    xlim(c(0, 1)) +
    ylim(c(-2, 2.2)) +
    theme_minimal()
  
  pi = ggplot(aes(x = x, y = pi), data = grid) +
    geom_line(colour = "darkred") +
    geom_point(aes(x = x, y = pi), size = 3L, colour = "darkred", data = pi_argmax) +
    xlim(c(0, 1)) +
    ylab("PI") +
    theme_minimal()
 
  ggsave(file.path(sprintf("../figure_man/bayesian_loop_pi_%i.png", i)), plot = g / pi, width = 5, height = 4)

  old_pi_argmax = pi_argmax
  
  instance$eval_batch(pi_argmax[, "x", with = FALSE])
}

