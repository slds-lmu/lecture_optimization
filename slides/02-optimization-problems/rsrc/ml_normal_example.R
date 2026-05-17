# Used in: ../slides-problems-1-unconstrained.tex
#
# Simulate a normal sample, show the density with the observed data, and
# visualize the negative log-likelihood as a function of μ.

set.seed(1L)

library(data.table)
library(ggplot2)

negative_log_likelihood = function(mu, sample) {
  n_obs = length(sample)
  (n_obs / 2) * log(2 * pi) + 0.5 * sum((sample - mu)^2)
}

# Simulate one sample and build the density plot.
x_grid = seq(-1, 6, by = 0.1)
observed_sample = rnorm(30L, mean = 2.5, sd = 1)

density_data = data.table(
  x = x_grid,
  density = dnorm(x_grid, mean = 2.5, sd = 1)
)
observed_points = data.table(
  x = observed_sample,
  density = 0,
  type = "Data"
)

density_plot = ggplot(density_data, aes(x = x, y = density)) +
  geom_line() +
  geom_point(
    data = observed_points,
    aes(x = x, y = density, colour = type),
    inherit.aes = FALSE
  ) +
  labs(x = "x", y = expression(f(x ~ "|" ~ mu ~ "," ~ sigma))) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = c(0.85, 0.85),
    legend.title = element_blank(),
    legend.key = element_rect(fill = alpha("white", 0))
  )
if (interactive()) {
  print(density_plot)
}
ggsave("../figure/ml_normal_example_dnorm.pdf", density_plot, width = 3, height = 3)

# Evaluate the negative log-likelihood on a grid of candidate means.
mu_grid = seq(-1, 6, by = 0.1)
likelihood_data = data.table(
  mu = mu_grid,
  neg_log_lik = vapply(mu_grid, negative_log_likelihood, numeric(1), sample = observed_sample)
)

neglog_plot = ggplot(likelihood_data, aes(x = mu, y = neg_log_lik)) +
  geom_line() +
  labs(x = expression(mu), y = expression(-log(L(mu)))) +
  theme_bw(base_size = 12)
if (interactive()) {
  print(neglog_plot)
}
ggsave("../figure/ml_normal_example_negloglike_nooptim.pdf", neglog_plot, width = 3, height = 3)

sample_mean = mean(observed_sample)
neglog_with_solution_plot = neglog_plot +
  geom_vline(xintercept = sample_mean, colour = "#f8766d")
if (interactive()) {
  print(neglog_with_solution_plot)
}
ggsave("../figure/ml_normal_example_negloglike.pdf", neglog_with_solution_plot, width = 3, height = 3)
