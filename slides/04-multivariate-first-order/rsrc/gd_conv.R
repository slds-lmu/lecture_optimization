# Used in: slides-multivar-first-order-7-gd-quadratic-forms.tex
#
# Generates stacked bar chart of GD regret decomposed by eigenvector mode,
# using the closed-form expression from the slide:
#   q(x^t) - q(x*) = 1/2 * sum_i (1 - 2*alpha*lambda_i)^(2t) * lambda_i * w_i^[0]^2
# A = [[2,1],[1,2]] (eigenvalues lambda_1=1, lambda_2=3), alpha=0.05,
# w0 chosen so both modes have equal initial contribution.
# Produces:
#   figure/gd_conv.pdf

library(ggplot2)

set.seed(123L)

A = matrix(c(2, 1, 1, 2), nrow = 2L)
lambdas = sort(eigen(A)$values)  # c(1, 3), ascending: lambda_1 <= lambda_2

alpha = 0.05
# equal initial contributions: 1/2 * lambda_i * w0_i^2 = 1.5 for both modes
w0 = c(sqrt(3), 1)
n_iter = 25L

t_seq = 0:n_iter

contributions = do.call(rbind, lapply(seq_along(lambdas), function(i) {
  contrib = 0.5 * lambdas[i] * (1 - 2 * alpha * lambdas[i])^(2 * t_seq) * w0[i]^2
  data.frame(t = t_seq, contribution = contrib, mode = paste0("mode", i))
}))
contributions$mode = factor(contributions$mode, levels = c("mode2", "mode1"))

p = ggplot(contributions, aes(x = t, y = contribution, fill = mode)) +
  geom_col() +
  scale_fill_manual(
    values = c("mode1" = "dodgerblue", "mode2" = "orange"),
    breaks = c("mode1", "mode2"),
    labels = expression(lambda[1] == 1, lambda[2] == 3),
    name = NULL
  ) +
  labs(x = "Iterations", y = expression(q(x^(t)) - q(x^"*"))) +
  theme_bw() +
  theme(legend.position = "top")

ggsave("../figure/gd_conv.pdf", plot = p, width = 4, height = 3)
