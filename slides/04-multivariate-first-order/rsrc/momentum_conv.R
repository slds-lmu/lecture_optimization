# Used in: slides-multivar-first-order-8-quadratic-forms-momentum.tex
#
# Generates convergence region heatmaps showing max{|σ₁|, |σ₂|} over (α, φ)
# for the momentum recursion matrix with λ = 1.
# Each behavior variant adds a red dot at a representative (α, φ) point:
#   ripples: complex EVs (α = 1.4, φ = 0.8)
#   mono:    real positive EVs (α = 0.25, φ = 0.1)
#   1step:   one-step convergence (α = 1.0, φ = 0.0)
#   osc:     real negative EVs (α = 2.0, φ = 0.1)
#   div:     diverging (α = 2.23, φ = 0.1)
# Produces:
#   figure/momentum_conv.pdf
#   figure/momentum_conv_ripples.pdf
#   figure/momentum_conv_mono.pdf
#   figure/momentum_conv_1step.pdf
#   figure/momentum_conv_osc.pdf
#   figure/momentum_conv_div.pdf

library(ggplot2)

set.seed(123L)

n = 400L
grid = expand.grid(
  alpha = seq(0, 2.5, length.out = n),
  phi   = seq(0, 1,   length.out = n)
)

compute_rate = function(alpha, phi) {
  tr   = 1 + phi - alpha
  disc = tr^2 - 4 * phi
  s1   = (tr + sqrt(as.complex(disc))) / 2
  s2   = (tr - sqrt(as.complex(disc))) / 2
  pmin(pmax(Mod(s1), Mod(s2)), 1)
}

grid$rate = mapply(compute_rate, grid$alpha, grid$phi)

make_heatmap = function(dot = NULL) {
  p = ggplot(grid, aes(x = alpha, y = phi, fill = rate)) +
    geom_raster() +
    scale_fill_viridis_c(
      option = "magma", direction = -1, limits = c(0, 1),
      name = "Convergence\nRate"
    ) +
    labs(x = expression(alpha), y = expression(varphi)) +
    theme_bw()
  if (!is.null(dot)) {
    dot_df = data.frame(alpha = dot[1L], phi = dot[2L])
    p = p +
      geom_point(
        data = dot_df, aes(x = alpha, y = phi),
        color = "red", size = 3, inherit.aes = FALSE
      ) +
      guides(fill = "none")
  }
  p
}

dots = list(
  ripples = c(1.4,  0.8),
  mono    = c(0.25, 0.1),
  `1step` = c(1.0,  0.0),
  osc     = c(2.0,  0.1),
  div     = c(2.23, 0.1)
)

ggsave("../figure/momentum_conv.pdf",         make_heatmap(),                 width = 5, height = 3)
ggsave("../figure/momentum_conv_ripples.pdf",  make_heatmap(dots$ripples),    width = 4, height = 3)
ggsave("../figure/momentum_conv_mono.pdf",     make_heatmap(dots$mono),       width = 4, height = 3)
ggsave("../figure/momentum_conv_1step.pdf",    make_heatmap(dots[["1step"]]), width = 4, height = 3)
ggsave("../figure/momentum_conv_osc.pdf",      make_heatmap(dots$osc),        width = 4, height = 3)
ggsave("../figure/momentum_conv_div.pdf",      make_heatmap(dots$div),        width = 4, height = 3)
