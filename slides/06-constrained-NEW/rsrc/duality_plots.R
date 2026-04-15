library(ggplot2)
library(gridExtra)

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(size = 12)
    )
)

cols <- list(
  contour = "#A7B4C2",
  feasible_fill = "#DCEAF6",
  feasible_line = "#2C7FB8",
  primal = "#12344D",
  dual = "#D95F02",
  optimum = "#B14D6C",
  saddle = "#1B9E77",
  bound = "#6A3D9A"
)

# -----------------------------------------------------------------------------
# Figure 1: primal optimum and dual lower bound on a 1D toy problem
# -----------------------------------------------------------------------------

primal_df <- data.frame(w = seq(-0.5, 3.2, length.out = 400))
primal_df$F <- (primal_df$w - 2)^2 + 0.4

w_star <- 1
p_star <- (w_star - 2)^2 + 0.4
w_uncon <- 2

dual_df <- data.frame(alpha = seq(0, 4, length.out = 300))
dual_df$L <- 0.4 + dual_df$alpha - dual_df$alpha^2 / 4
alpha_star <- 2

p_left <- ggplot(primal_df, aes(x = w, y = F)) +
  annotate(
    "rect",
    xmin = -Inf, xmax = 1, ymin = -Inf, ymax = Inf,
    fill = cols$feasible_fill, alpha = 0.60
  ) +
  geom_line(color = cols$primal, linewidth = 1.2) +
  geom_vline(xintercept = 1, color = cols$feasible_line, linewidth = 0.9, linetype = "dashed") +
  geom_point(
    data = data.frame(
      w = c(w_star, w_uncon),
      F = c(p_star, 0.4),
      kind = c("constrained", "unconstrained")
    ),
    aes(fill = kind),
    shape = 21,
    size = 3.2,
    color = "white",
    stroke = 0.35,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      constrained = cols$optimum,
      unconstrained = cols$dual
    )
  ) +
  annotate("text", x = 0.15, y = 3.35, label = "feasible set:  w <= 1", color = cols$feasible_line, size = 4.1, fontface = "bold") +
  annotate("text", x = w_star + 0.18, y = p_star + 0.18, label = "primal optimum", color = cols$optimum, size = 4.0) +
  annotate("text", x = w_uncon - 0.08, y = 0.15, label = "unconstrained\nminimum", color = cols$dual, size = 3.8) +
  labs(
    title = "Primal problem",
    subtitle = "min F(w) subject to w - 1 <= 0",
    x = "primal variable w",
    y = "objective"
  ) +
  coord_cartesian(xlim = c(-0.4, 3.0), ylim = c(0, 3.6)) +
  theme(
    axis.line = element_line(color = "grey40"),
    legend.position = "none"
  )

p_right <- ggplot(dual_df, aes(x = alpha, y = L)) +
  geom_line(color = cols$bound, linewidth = 1.2) +
  geom_hline(yintercept = p_star, color = cols$optimum, linewidth = 0.9, linetype = "dashed") +
  annotate("point", x = alpha_star, y = p_star, color = cols$optimum, size = 3.0) +
  annotate("text", x = 3.15, y = p_star + 0.09, label = "primal value p*", color = cols$optimum, size = 4.0) +
  annotate("text", x = alpha_star + 0.35, y = p_star - 0.23, label = "maximize lower bound", color = cols$bound, size = 4.0) +
  labs(
    title = "Dual function",
    subtitle = expression(L(alpha) == min[w]~(F(w) + alpha * g(w))),
    x = expression("multiplier " * alpha),
    y = expression(L(alpha))
  ) +
  coord_cartesian(xlim = c(0, 4), ylim = c(0.2, 1.7)) +
  theme(
    axis.line = element_line(color = "grey40"),
    legend.position = "none"
  )

lower_bound_plot <- arrangeGrob(p_left, p_right, ncol = 2)
ggsave("../figure/duality_lower_bound.pdf", lower_bound_plot, width = 10.0, height = 4.5)

# -----------------------------------------------------------------------------
# Figure 2: ordering matters in minimax problems
# -----------------------------------------------------------------------------

make_surface_panel <- function(df, title, subtitle, saddle = NULL) {
  p <- ggplot(df, aes(x = w, y = alpha, fill = z)) +
    geom_raster(interpolate = TRUE) +
    geom_contour(data = df, aes(x = w, y = alpha, z = z), inherit.aes = FALSE, color = "white", linewidth = 0.32, alpha = 0.75, bins = 10) +
    scale_fill_gradient2(
      low = "#2C7FB8",
      mid = "#F6F7F8",
      high = "#D95F02",
      midpoint = 0
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "minimize over w",
      y = expression("maximize over " * alpha)
    ) +
    theme(
      axis.line = element_line(color = "grey40"),
      legend.position = "none"
    )

  if (!is.null(saddle)) {
    p <- p +
      geom_point(
        data = data.frame(w = saddle[1], alpha = saddle[2]),
        aes(x = w, y = alpha),
        inherit.aes = FALSE,
        color = cols$saddle,
        fill = "white",
        shape = 21,
        size = 3.4,
        stroke = 1
      ) +
      annotate("text", x = saddle[1] + 0.34, y = saddle[2] + 0.14, label = "saddle point", color = cols$saddle, size = 4.0, fontface = "bold")
  }

  p
}

df_nonconvex <- expand.grid(
  w = seq(-pi, pi, length.out = 260),
  alpha = seq(-pi, pi, length.out = 260)
)
df_nonconvex$z <- sin(df_nonconvex$w + df_nonconvex$alpha)

p_nonconvex <- make_surface_panel(
  df_nonconvex,
  title = expression(H(w, alpha) == sin(w + alpha)),
  subtitle = "order matters: max min < min max"
)

df_saddle <- expand.grid(
  w = seq(-1.1, 1.1, length.out = 260),
  alpha = seq(-1.1, 1.1, length.out = 260)
)
df_saddle$z <- df_saddle$w^2 - df_saddle$alpha^2

p_saddle <- make_surface_panel(
  df_saddle,
  title = expression(H(w, alpha) == w^2 - alpha^2),
  subtitle = "convex-concave: saddle point and equal orders",
  saddle = c(0, 0)
)

minimax_plot <- arrangeGrob(p_nonconvex, p_saddle, ncol = 2)
ggsave("../figure/duality_minimax.pdf", minimax_plot, width = 10.2, height = 4.6)
