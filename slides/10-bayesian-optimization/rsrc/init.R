# Used in: lecture_optimization/slides/10-bayesian-optimization/slides-bayesian-optimization-2-loop.tex
#
# Compares purely random initialization with Latin hypercube sampling in a
# two-dimensional unit square.

library(bbotk)
library(data.table)
library(ggplot2)
library(lhs)
library(mlr3learners)
library(mlr3mbo)
library(patchwork)

source("bo-helpers.R")
set.seed(5L)

domain = ps(x1 = p_dbl(lower = 0, upper = 1), x2 = p_dbl(lower = 0, upper = 1))
grid_breaks = seq(from = 0, to = 1, length.out = 11L)

plot_initial_design = function(design, title) {
  scatter = ggplot(design, aes(x = x1, y = x2)) +
    geom_point(size = 3L) +
    geom_vline(xintercept = grid_breaks, linetype = 2) +
    geom_hline(yintercept = grid_breaks, linetype = 2) +
    labs(title = title, x = expression(x[1]), y = expression(x[2])) +
    bo_theme()

  x_hist = ggplot(design, aes(x = x1)) +
    geom_histogram(breaks = grid_breaks, colour = "white", fill = "grey65") +
    theme_void()

  y_hist = ggplot(design, aes(x = x2)) +
    geom_histogram(breaks = grid_breaks, colour = "white", fill = "grey65") +
    coord_flip() +
    theme_void()

  (x_hist + plot_spacer()) / (scatter + y_hist) +
    plot_layout(widths = c(4, 1), heights = c(1, 4))
}

random_design = generate_design_random(domain, n = 10L)$data
lhs_design = generate_design_lhs(domain, n = 10L)$data

save_figure(plot_initial_design(random_design, "Random Design"), "init_0.png")
save_figure(plot_initial_design(lhs_design, "LHS"), "init_1.png")
