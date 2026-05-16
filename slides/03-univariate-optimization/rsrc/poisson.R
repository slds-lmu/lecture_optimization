# Used in: slides/03-univariate-optimization/slides-univariate-2-brent.tex
#
# Compare the optimization traces of golden section search and Brent's method
# for the Poisson negative log-likelihood.

set.seed(1234)

library(data.table)
library(ggplot2)

dir.create("../figure", recursive = TRUE, showWarnings = FALSE)

samples = rpois(250L, lambda = 1)

negative_log_likelihood = function(lambda, data) {
  -sum(dpois(data, lambda = lambda, log = TRUE))
}

make_traced_objective = function(objective_function) {
  trace_env = new.env(parent = emptyenv())
  trace_env$lambda = numeric()
  trace_env$value = numeric()

  traced_objective = function(lambda) {
    objective_value = objective_function(lambda)
    trace_env$lambda = c(trace_env$lambda, as.numeric(lambda))
    trace_env$value = c(trace_env$value, as.numeric(objective_value))
    objective_value
  }

  trace_archive = function(method_label) {
    data.table(
      iter = seq_along(trace_env$lambda),
      lambda = trace_env$lambda,
      objective_value = trace_env$value,
      method = method_label
    )
  }

  list(
    objective = traced_objective,
    archive = trace_archive
  )
}

golden_section_trace = function(objective_function, x_left, x_right, tolerance = 1e-10, max_iter = 1000L) {
  phi = (sqrt(5) - 1) / 2
  left = x_left
  right = x_right
  c_point = right - phi * (right - left)
  d_point = left + phi * (right - left)
  f_c = objective_function(c_point)
  f_d = objective_function(d_point)

  iter = 0L

  while ((right - left) > tolerance && iter < max_iter) {
    iter = iter + 1L

    if (f_c < f_d) {
      right = d_point
      d_point = c_point
      f_d = f_c
      c_point = right - phi * (right - left)
      f_c = objective_function(c_point)
    } else {
      left = c_point
      c_point = d_point
      f_c = f_d
      d_point = left + phi * (right - left)
      f_d = objective_function(d_point)
    }
  }

  invisible((left + right) / 2)
}

objective_function = function(lambda) {
  negative_log_likelihood(lambda = lambda, data = samples)
}

lambda_grid = seq(0.75, 1.25, length.out = 1000L)
likelihood_grid = data.table(
  lambda = lambda_grid,
  objective_value = vapply(lambda_grid, objective_function, numeric(1L))
)

golden_section_run = make_traced_objective(objective_function)
golden_section_trace(
  objective_function = golden_section_run$objective,
  x_left = 0.5,
  x_right = 1.5,
  tolerance = 1e-10
)
golden_section_archive = golden_section_run$archive("Golden section search")

brent_run = make_traced_objective(objective_function)
optimize(
  f = brent_run$objective,
  lower = 0.5,
  upper = 1.5,
  tol = 1e-10
)
brent_archive = brent_run$archive("Brent's method")

trace_archive = rbindlist(list(golden_section_archive, brent_archive))

poisson_plot = ggplot(
  likelihood_grid,
  aes(x = lambda, y = objective_value)
) +
  geom_line(linewidth = 0.8, colour = "#2F3A4A") +
  geom_point(
    data = trace_archive,
    aes(colour = method),
    alpha = 0.55,
    size = 1.7
  ) +
  scale_colour_manual(
    values = c(
      "Golden section search" = "#C84B6C",
      "Brent's method" = "#0F8B8D"
    ),
    name = NULL
  ) +
  labs(
    x = expression(lambda),
    y = "Negative log-likelihood"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.box.margin = margin(0, 0, 0, 0),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "../figure/poisson.pdf",
  plot = poisson_plot,
  width = 4,
  height = 2
)
