# ------------------------------------------------------------------------------
# bayesian optimization

# FIG: compare two sampling strategies for initializing design points
#      in black-box optimization. 
# ------------------------------------------------------------------------------

library(bbotk)
library(data.table)
library(mlr3mbo)
library(mlr3learners)
library(ggplot2)
library(ggExtra)
library(lhs)

set.seed(5)

# ------------------------------------------------------------------------------

domain = ps(x1 = p_dbl(lower = 0, upper = 1), x2 = p_dbl(lower = 0, upper = 1))

qs = seq(from = 0, to = 1, length.out = 11)

xdt_random = generate_design_random(domain, n = 10L)$data
xdt_random[, method := "random"]
xdt_lhs = generate_design_lhs(domain, n = 10L)$data
xdt_lhs[, method := "lhs"]
xdt = rbind(xdt_random, xdt_lhs)

g = ggplot(aes(x = x1, y = x2), data = xdt[method == "random"]) +
  geom_point(size = 3L) +
  geom_vline(xintercept = qs, linetype = 2) +
  geom_hline(yintercept = qs, linetype = 2) +
  labs(title = "Random Design", x = expression(x[1]), y = expression(x[2])) +
  theme_minimal()

g = ggMarginal(g, type = "histogram", bins = 11)

ggsave("../figure_man/init_0.png", plot = g, width = 5, height = 4)

g = ggplot(aes(x = x1, y = x2), data = xdt[method == "lhs"]) +
  geom_point(size = 3L) +
  geom_vline(xintercept = qs, linetype = 2) +
  geom_hline(yintercept = qs, linetype = 2) +
  labs(title = "LHS", x = expression(x[1]), y = expression(x[2])) +
  theme_minimal()

g = ggMarginal(g, type = "histogram", bins = 11)

ggsave("../figure_man/init_1.png", plot = g, width = 5, height = 4)

