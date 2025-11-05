# ------------------------------------------------------------------------------
# bayesian optimization

# FIG: perform Bayesian optimization for a multi-objective function.
# ------------------------------------------------------------------------------

library(bbotk)
library(data.table)
library(mlr3mbo)
library(mlr3learners)
library(mlr3misc)
library(ggplot2)
library(patchwork)

set.seed(123)

# ------------------------------------------------------------------------------

objective = ObjectiveRFunDt$new(
 fun = function(xdt) data.table(y1 = xdt$x^2, y2 = (xdt$x - 2) ^ 2),
 domain = ps(x = p_dbl(lower = -1, upper = 3)),
 codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
)
instance = OptimInstanceMultiCrit$new(
  objective = objective,
  terminator = trm("none")
)
instance$eval_batch(generate_design_random(instance$search_space, n = 100L)$data)

instance$archive$data
instance$archive$data[, type := "dominated"]
instance$archive$data[!is_dominated(t(instance$archive$data[, c("y1", "y2")])), type := "Pareto"]

g = ggplot(aes(x = y1, y = y2), data = instance$archive$data) +
  geom_point(aes(x = y1, y = y2, colour = type), size = 3L, data = instance$archive$data[type == "dominated"]) +
  geom_point(aes(x = y1, y = y2, colour = type), size = 3L, data = instance$archive$data[type == "Pareto"]) +
  geom_step(direction = "hv", data = instance$archive$data[type == "Pareto"]) +
  labs(x = expression(y[1]), y = expression(y[2]), colour = "") +
  xlim(c(0, 9)) +
  ylim(c(0, 9)) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("../figure_man/multicrit_0.png", plot = g, width = 5, height = 4)

pareto_dat = instance$archive$data[type == "Pareto"][, c("y1", "y2"), with = FALSE]
pareto_dat = setorderv(pareto_dat, cols = "y1")
pareto_dat = pareto_dat[c(10, 15, 25, 35), ]

poly_dat = copy(pareto_dat)
poly_dat = map_dtr(seq_len(nrow(poly_dat)), function(i) {
  if (i < nrow(poly_dat)) {
    tmp = poly_dat[i, ]
    tmp[["y1"]] = poly_dat[i + 1][["y1"]]
  } else {
    tmp = data.table()
  }
  rbind(poly_dat[i, ], tmp)
})
poly_dat = rbind(data.table(y1 = c(9, poly_dat$y1[1L]), y2 = c(9, 9)), poly_dat, data.table(y1 = 9, y2 = poly_dat$y2[nrow(poly_dat)]))

g = ggplot(aes(x = y1, y = y2), data = pareto_dat) +
  geom_point(aes(x = y1, y = y2), size = 3L) +
  geom_point(aes(x = 9, y = 9), colour = "darkred", size = 3L) +
  geom_step(direction = "hv") +
  geom_polygon(aes(x = y1, y = y2), data = poly_dat, alpha = 0.2) +
  labs(x = expression(y[1]), y = expression(y[2]), colour = "") +
  theme_minimal() +
  xlim(c(0, 9)) +
  ylim(c(0, 9)) +
  theme(legend.position = "bottom")

ggsave("../figure_man/multicrit_1.png", plot = g, width = 5, height = 4)

instance$archive$clear()

xdt = data.table(x = c(-0.9, 0.1, 1.1, 2.3))
instance$eval_batch(xdt)

grid = generate_design_grid(instance$search_space, resolution = 1001L)$data
set(grid, j = "y1", value = objective$eval_dt(grid[, "x"])$y1)
set(grid, j = "y2", value = objective$eval_dt(grid[, "x"])$y2)
grid[, type := "dominated"]
grid[!is_dominated(t(grid[, c("y1", "y2")])), type := "pareto"]

g = ggplot() +
  geom_line(aes(x = y1, y = y2), data = grid[type == "pareto"]) +
  geom_point(aes(x = y1, y = y2), size = 3L, colour = "black", data = instance$archive$data) +
  theme_minimal()

ggsave("../figure_man/multicrit_2.png", plot = g, width = 5, height = 4)

lambdas = mlr3mbo:::calculate_parego_weights(s = 100L, k = 2L)

data = instance$archive$data
ydt = data[, instance$archive$cols_y, with = FALSE]
ydt = Map(function(y) (y - min(y, na.rm = TRUE)) / diff(range(y, na.rm = TRUE)), ydt)  # scale y to [0, 1]

lambda = lambdas[sample.int(nrow(lambdas), 1L), , drop = TRUE]
mult = Map("*", ydt, lambda)
yscal = Reduce("+", mult)
yscal = do.call(pmax, mult) + 0.05 * yscal  # augmented Tchebycheff function
data[, y_scal := yscal]

surrogate = srlrn(lrn("regr.km", covtype = "matern5_2", optim.method = "BFGS", nugget.stability = 10^-8), 
                  archive = instance$archive)
surrogate$cols_y = "y_scal"
acq_function = acqf("ei", surrogate = surrogate)
acq_function$surrogate$update()
prediction = surrogate$predict(grid)

set(grid, j = "y_hat", value = prediction$mean)
set(grid, j = "y_min", value = prediction$mean - prediction$se)
set(grid, j = "y_max", value = prediction$mean + prediction$se)

acq_function$update()
set(grid, j = "ei", value = acq_function$eval_dt(grid[, "x"])$acq_ei)
ei_argmax = grid[which.max(ei), ]

scal = ggplot() +
  geom_point(aes(x = x, y = y_scal), size = 3L, colour = "black", data = instance$archive$data) +
  geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2, data = grid) +
  geom_ribbon(aes(x = x, min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1, data = grid) +
  labs(y = expression(y[scalarized])) +
  theme_minimal()

ei = ggplot(aes(x = x, y = ei), data = grid) +
  geom_line(colour = "darkred") +
  geom_point(aes(x = x, y = ei), size = 3L, colour = "darkred", data = ei_argmax) +
  ylab("EI") +
  theme_minimal()

ggsave("../figure_man/multicrit_3.png", plot = g + (scal / ei), width = 10, height = 4)

old_ei_argmax = ei_argmax

instance$eval_batch(ei_argmax[, "x", with = FALSE])

for (i in 4:11) {
  data = instance$archive$data
  ydt = data[, instance$archive$cols_y, with = FALSE]
  ydt = Map(function(y) (y - min(y, na.rm = TRUE)) / diff(range(y, na.rm = TRUE)), ydt)  # scale y to [0, 1]
  
  lambda = lambdas[sample.int(nrow(lambdas), 1L), , drop = TRUE]
  mult = Map("*", ydt, lambda)
  yscal = Reduce("+", mult)
  yscal = do.call(pmax, mult) + 0.05 * yscal  # augmented Tchebycheff function
  data[, y_scal := yscal]

  acq_function$surrogate$update()
  prediction = surrogate$predict(grid)
  set(grid, j = "y_hat", value = prediction$mean)
  set(grid, j = "y_min", value = prediction$mean - prediction$se)
  set(grid, j = "y_max", value = prediction$mean + prediction$se)
  
  acq_function$update()
  set(grid, j = "ei", value = acq_function$eval_dt(grid[, "x"])$acq_ei)
  ei_argmax = grid[which.max(ei), ]

  g = ggplot() +
    geom_line(aes(x = y1, y = y2), data = grid[type == "pareto"]) +
    geom_point(aes(x = y1, y = y2), size = 3L, colour = "black", data = instance$archive$data) +
    geom_point(aes(x = y1, y = y2), size = 3L, colour = "grey", data = old_ei_argmax) +
    theme_minimal()

  scal = ggplot() +
    geom_point(aes(x = x, y = y_scal), size = 3L, colour = "black", data = instance$archive$data) +
    geom_line(aes(x = x, y = y_hat), colour = "steelblue", linetype = 2, data = grid) +
    geom_ribbon(aes(x = x, min = y_min, max = y_max), fill = "steelblue", colour = NA, alpha = 0.1, data = grid) +
    labs(y = expression(y[scalarized])) +
    theme_minimal()
  
  ei = ggplot(aes(x = x, y = ei), data = grid) +
    geom_line(colour = "darkred") +
    geom_point(aes(x = x, y = ei), size = 3L, colour = "darkred", data = ei_argmax) +
    ylab("EI") +
    theme_minimal()
  
  ggsave(file.path(sprintf("../figure_man/multicrit_%i.png", i)), plot = g + (scal / ei), width = 10, height = 4)

  old_ei_argmax = ei_argmax
  
  instance$eval_batch(ei_argmax[, "x", with = FALSE])
}

