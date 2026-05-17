# Used in: ../slides-problems-1-unconstrained.tex
#
# Load the Poisson sample, drop the largest outlier, and visualize the log-
# likelihood and negative log-likelihood as functions of λ.

set.seed(1L)

library(data.table)
library(ggplot2)

theme_set(theme_bw(base_size = 12))

load("e100.RData")

log_likelihood = function(lambda, sample) {
  sum(dpois(sample, lambda = lambda, log = TRUE))
}

# Remove the largest observation to focus on the well-behaved part of the
# likelihood surface shown in the slides.
sample_wo_outlier = e100[-which.max(e100)]
lambda_grid = seq(0, 1, by = 0.01)
log_lik_data = data.table(
  lambda = lambda_grid,
  log_lik = vapply(lambda_grid, log_likelihood, numeric(1), sample = sample_wo_outlier)
)

optimum = optimize(log_likelihood, interval = c(0, 1), maximum = TRUE, sample = sample_wo_outlier)

log_lik_plot = ggplot(log_lik_data, aes(x = lambda, y = log_lik)) +
  geom_line() +
  geom_point(
    data = data.table(lambda = optimum$maximum, log_lik = optimum$objective),
    aes(x = lambda, y = log_lik),
    colour = "#1f77b4"
  ) +
  labs(x = expression(lambda), y = expression(log(L(lambda))))
if (interactive()) {
  print(log_lik_plot)
}
ggsave("../figure/ml_poisson_example_1.pdf", log_lik_plot, width = 3, height = 2)

neg_log_plot = ggplot(log_lik_data, aes(x = lambda, y = -log_lik)) +
  geom_line() +
  geom_point(
    data = data.table(lambda = optimum$maximum, neg_log_lik = -optimum$objective),
    aes(x = lambda, y = neg_log_lik),
    colour = "#1f77b4"
  ) +
  labs(x = expression(lambda), y = expression(-log(L(lambda))))
if (interactive()) {
  print(neg_log_plot)
}
ggsave("../figure/ml_poisson_example_2.pdf", neg_log_plot, width = 3, height = 2)
