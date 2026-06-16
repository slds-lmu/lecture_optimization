# Used in: slides-multivar-first-order-8-quadratic-forms-momentum.tex
#
# Generates lollipop plots of the eigenspace error component w_i over iterations
# for GD with momentum, illustrating five convergence behaviors.
# Recursion: u^{t+1} = φ u^t + α λ w^t,  w^{t+1} = w^t - u^{t+1}
# with λ = 1, w₀ = 1, u₀ = 0.
# Produces:
#   figure/momentum_traj_ripples.pdf   (complex EVs — damped ripples)
#   figure/momentum_traj_mono.pdf      (real positive EVs — monotone decay)
#   figure/momentum_traj_1step.pdf     (one-step convergence)
#   figure/momentum_traj_osc.pdf       (real negative EVs — damped oscillations)
#   figure/momentum_traj_div.pdf       (diverging iterates)

library(ggplot2)

set.seed(123L)

run_momentum = function(phi, alpha, lambda = 1, n_iter = 50L, w0 = 1, u0 = 0) {
  w = numeric(n_iter + 1L)
  u = numeric(n_iter + 1L)
  w[1L] = w0
  u[1L] = u0
  for (t in seq_len(n_iter)) {
    u[t + 1L] = phi * u[t] + alpha * lambda * w[t]
    w[t + 1L] = w[t] - u[t + 1L]
  }
  data.frame(t = 0:n_iter, w = w)
}

make_lollipop = function(df) {
  ggplot(df, aes(x = t, y = w)) +
    geom_hline(yintercept = 0, color = "red") +
    geom_segment(aes(xend = t, yend = 0), color = "dodgerblue") +
    geom_point(color = "dodgerblue") +
    labs(x = "Iteration", y = expression(w[i]^{(t)})) +
    theme_bw() +
    theme(panel.grid = element_blank())
}

scenarios = list(
  ripples = list(phi = 0.8,  alpha = 1.4,  n_iter = 50L),
  mono    = list(phi = 0.1,  alpha = 0.25, n_iter = 30L),
  `1step` = list(phi = 0.0,  alpha = 1.0,  n_iter = 15L),
  osc     = list(phi = 0.1,  alpha = 2.0,  n_iter = 50L),
  div     = list(phi = 0.1,  alpha = 2.23, n_iter = 30L)
)

for (nm in names(scenarios)) {
  s  = scenarios[[nm]]
  df = run_momentum(phi = s$phi, alpha = s$alpha, n_iter = s$n_iter)
  ggsave(
    paste0("../figure/momentum_traj_", nm, ".pdf"),
    plot = make_lollipop(df), width = 4, height = 3
  )
}
