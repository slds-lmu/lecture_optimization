# ------------------------------------------------------------------------------
# bayesian optimization

# FIG: perform Bayesian optimization for a 2D function using the Expected Improvement (EI).
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
 fun = function(xdt) data.table(y = cos(xdt$x1 - xdt$x2)),
 domain = ps(x1 = p_dbl(lower = 0, upper = 10), x2 = p_dbl(lower = 0, upper = 10)),
 codomain = ps(y = p_dbl(tags = "minimize"))
)
instance = OptimInstanceSingleCrit$new(
  objective = objective,
  terminator = trm("none")
)

design = generate_design_random(instance$search_space, n = 10L)$data
instance$eval_batch(design)

surrogate = srlrn(lrn("regr.km", covtype = "matern5_2", optim.method = "BFGS"), archive = instance$archive)
acq_function = acqf("ei", surrogate = surrogate)

grid = generate_design_grid(instance$search_space, resolution = 101L)$data
acq_function$surrogate$update()
acq_function$update()
set(grid, j = "ei", value = acq_function$eval_dt(grid[, c("x1", "x2"), with = FALSE])$acq_ei)
ei_argmax = grid[which.max(ei), ]

# intial design + surrogate prediction
g = ggplot(aes(x = x1, y = x2, z = ei), data = grid) +
  geom_contour_filled(show.legend = FALSE) +
  labs(x = expression(x[1]), y = expression(x[2])) +
  theme_minimal()

ggsave(file.path("../figure_man/practical.png"), plot = g, width = 5, height = 4)

