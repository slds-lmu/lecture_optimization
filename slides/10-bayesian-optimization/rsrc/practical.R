# Used in: lecture_optimization/slides/10-bayesian-optimization/slides-bayesian-optimization-7-practical.tex
#
# Creates a two-dimensional Expected Improvement contour plot.

library(bbotk)
library(data.table)
library(ggplot2)
library(mlr3learners)
library(mlr3mbo)

source("bo-helpers.R")
set.seed(123L)

objective = ObjectiveRFunDt$new(
  fun = function(xdt) data.table(y = cos(xdt$x1 - xdt$x2)),
  domain = ps(x1 = p_dbl(lower = 0, upper = 10), x2 = p_dbl(lower = 0, upper = 10)),
  codomain = ps(y = p_dbl(tags = "minimize"))
)
instance = make_singlecrit_instance(objective)

initial_design = generate_design_random(instance$search_space, n = 10L)$data
instance$eval_batch(initial_design)

surrogate = make_km_surrogate(instance)
acq_function = acqf("ei", surrogate = surrogate)

grid = generate_design_grid(instance$search_space, resolution = 101L)$data
acq_function$surrogate$update()
add_acquisition_column(acq_function, grid, c("x1", "x2"), "ei")

plot = ggplot(grid, aes(x = x1, y = x2, z = ei)) +
  geom_contour_filled(show.legend = FALSE) +
  labs(x = expression(x[1]), y = expression(x[2])) +
  bo_theme()

save_figure(plot, "practical.png")
