library(ggplot2)
library(gridExtra)

theme_set(
  theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12)
    )
)

cols <- list(
  primal = "#2C7FB8",
  primal_fill = "#DCEAF6",
  dual = "#D95F02",
  dual_fill = "#FBE4D5",
  accent = "#1B9E77",
  dark = "#12344D"
)

rescale01 <- function(x) {
  rng <- range(x)
  if (diff(rng) < 1e-12) {
    return(rep(0.5, length(x)))
  }
  (x - rng[1]) / diff(rng)
}

tile_df <- function(mat, x0, y0, cell = 0.34) {
  nr <- nrow(mat)
  nc <- ncol(mat)
  df <- expand.grid(row = seq_len(nr), col = seq_len(nc))
  df$value <- as.vector(mat[nr:1, , drop = FALSE])
  df$x <- x0 + (df$col - 1) * cell
  df$y <- y0 + (df$row - 1) * cell
  df$cell <- cell
  df
}

vector_df <- function(values, x0, y0, width = 0.42, gap = 0.12) {
  data.frame(
    xmin = x0 + (seq_along(values) - 1) * (width + gap),
    xmax = x0 + (seq_along(values) - 1) * (width + gap) + width,
    ymin = y0,
    ymax = y0 + pmax(values, 0) * 1.05
  )
}

# -----------------------------------------------------------------------------
# Figure 1: feature-space versus sample-space representation
# -----------------------------------------------------------------------------

X <- matrix(
  c(
    1.1, 0.2, -0.3, 0.8, 0.4,
    0.9, 0.5, -0.2, 0.6, 0.1,
    0.3, 1.0, 0.4, -0.1, -0.4,
    -0.2, 0.7, 1.1, 0.2, -0.6,
    -0.5, 0.1, 0.8, 0.9, -0.2,
    0.0, -0.4, 0.6, 1.0, 0.7,
    -0.3, -0.7, 0.1, 0.5, 1.2
  ),
  nrow = 7,
  byrow = TRUE
)

scatter <- crossprod(X)
similarity <- tcrossprod(X)
w_vals <- c(0.85, 0.35, 0.65, 0.55, 0.42)
alpha_vals <- c(0.20, 0.78, 0.10, 0.56, 0.92, 0.38, 0.66)

scatter_df <- tile_df(rescale01(scatter), x0 = 1.05, y0 = 1.10, cell = 0.43)
sim_df <- tile_df(rescale01(similarity), x0 = 10.35, y0 = 0.84, cell = 0.31)
w_df <- vector_df(w_vals, x0 = 3.95, y0 = 1.20, width = 0.42, gap = 0.12)
alpha_df <- vector_df(alpha_vals, x0 = 13.55, y0 = 1.05, width = 0.27, gap = 0.09)

p_views <- ggplot() +
  annotate("rect", xmin = 0.25, xmax = 8.10, ymin = 0.35, ymax = 5.55, fill = cols$primal_fill, color = cols$primal, linewidth = 1.0) +
  annotate("rect", xmin = 9.15, xmax = 17.10, ymin = 0.35, ymax = 5.55, fill = cols$dual_fill, color = cols$dual, linewidth = 1.0) +
  geom_tile(
    data = scatter_df,
    aes(x = x, y = y, fill = value),
    width = scatter_df$cell,
    height = scatter_df$cell,
    color = "white",
    linewidth = 0.25
  ) +
  geom_rect(
    data = w_df,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = cols$primal,
    alpha = 0.88,
    color = "white",
    linewidth = 0.25
  ) +
  geom_tile(
    data = sim_df,
    aes(x = x, y = y, fill = value),
    width = sim_df$cell,
    height = sim_df$cell,
    color = "white",
    linewidth = 0.20
  ) +
  geom_rect(
    data = alpha_df,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = cols$dual,
    alpha = 0.88,
    color = "white",
    linewidth = 0.22
  ) +
  scale_fill_gradient(low = "#EEF3F7", high = cols$accent, guide = "none") +
  annotate("segment", x = 3.25, y = 2.35, xend = 3.70, yend = 2.35, color = cols$dark, linewidth = 0.8, arrow = arrow(length = grid::unit(0.18, "cm"))) +
  annotate("segment", x = 12.70, y = 2.05, xend = 13.20, yend = 2.05, color = cols$dark, linewidth = 0.8, arrow = arrow(length = grid::unit(0.18, "cm"))) +
  annotate("text", x = 4.18, y = 5.05, label = "Primal", color = cols$primal, size = 6.2, fontface = "bold") +
  annotate("text", x = 4.18, y = 4.52, label = "feature-space coefficients", color = cols$dark, size = 4.2) +
  annotate("text", x = 2.00, y = 3.65, label = expression(X^T * X), color = cols$dark, size = 5.0, fontface = "bold") +
  annotate("text", x = 2.00, y = 0.75, label = expression(d %*% d ~ "scatter matrix"), color = cols$dark, size = 4.0) +
  annotate("text", x = 5.15, y = 3.58, label = expression(w %in% R^d), color = cols$primal, size = 4.7, fontface = "bold") +
  annotate("text", x = 5.15, y = 0.75, label = "one coefficient per feature", color = cols$dark, size = 4.0) +
  annotate("text", x = 13.12, y = 5.05, label = "Dual", color = cols$dual, size = 6.2, fontface = "bold") +
  annotate("text", x = 13.12, y = 4.52, label = "sample-space multipliers", color = cols$dark, size = 4.2) +
  annotate("text", x = 11.33, y = 3.82, label = expression(X %*% X^T), color = cols$dark, size = 5.0, fontface = "bold") +
  annotate("text", x = 11.33, y = 0.52, label = expression(n %*% n ~ "similarity / kernel matrix"), color = cols$dark, size = 3.8) +
  annotate("text", x = 14.45, y = 3.42, label = expression(alpha %in% R^n), color = cols$dual, size = 4.7, fontface = "bold") +
  annotate("text", x = 14.45, y = 0.75, label = "one weight per point / constraint", color = cols$dark, size = 4.0) +
  annotate("text", x = 8.62, y = 2.97, label = "same model,\ndifferent coordinates", color = cols$dark, size = 4.1, fontface = "bold") +
  coord_cartesian(xlim = c(0, 17.4), ylim = c(0, 5.85), expand = FALSE) +
  theme_void()

ggsave("../figure/primal_dual_views.pdf", p_views, width = 8.2, height = 4.5)

# -----------------------------------------------------------------------------
# Figure 2: computational regime comparison
# -----------------------------------------------------------------------------

regime_df <- expand.grid(
  log_n = seq(1.8, 4.3, length.out = 220),
  log_d = seq(1.8, 4.3, length.out = 220)
)
regime_df$pref <- ifelse(regime_df$log_d <= regime_df$log_n, "Primal often cheaper", "Dual often cheaper")

p_regime <- ggplot(regime_df, aes(x = log_n, y = log_d, fill = pref)) +
  geom_raster(interpolate = TRUE) +
  geom_abline(intercept = 0, slope = 1, color = "white", linewidth = 1.0, linetype = "dashed") +
  scale_fill_manual(
    values = c(
      "Primal often cheaper" = cols$primal,
      "Dual often cheaper" = cols$dual
    )
  ) +
  annotate("text", x = 3.55, y = 2.35, label = "d < n\nprimal often cheaper", color = "white", size = 4.2, fontface = "bold") +
  annotate("text", x = 2.35, y = 3.62, label = "n < d\ndual often cheaper", color = "white", size = 4.2, fontface = "bold") +
  annotate("text", x = 3.78, y = 4.15, label = "n = d", color = cols$dark, size = 4.2, fontface = "bold", angle = 45) +
  labs(
    title = "Dimension regime heuristic",
    subtitle = "actual cost still depends on solver, sparsity, and matrix formation",
    x = expression(log[10] * "(number of points n)"),
    y = expression(log[10] * "(number of variables d)")
  ) +
  theme(
    legend.position = "none",
    axis.line = element_line(color = "grey40")
  )

size_df <- data.frame(
  scenario = rep(c("Many points,\nfew variables", "Few points,\nmany variables"), each = 2),
  matrix_type = factor(
    rep(c("X^T X", "X X^T"), times = 2),
    levels = c("X^T X", "X X^T")
  ),
  entries = c(100^2, 10000^2, 10000^2, 100^2),
  label = c("1e4", "1e8", "1e8", "1e4")
)

p_sizes <- ggplot(size_df, aes(x = scenario, y = entries, fill = matrix_type)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  geom_text(
    aes(label = label),
    position = position_dodge(width = 0.72),
    vjust = -0.35,
    size = 4.0,
    color = cols$dark,
    fontface = "bold"
  ) +
  scale_y_log10() +
  scale_fill_manual(
    values = c(cols$primal, cols$dual)
  ) +
  labs(
    title = "If dense core matrices are formed",
    subtitle = "example: entry counts for two opposite regimes",
    x = NULL,
    y = "entries (log scale)",
    fill = "matrix"
  ) +
  theme(
    axis.line.y = element_line(color = "grey40"),
    legend.position = "top"
  )

regimes_plot <- arrangeGrob(p_regime, p_sizes, ncol = 1)
ggsave("../figure/primal_dual_regimes.pdf", regimes_plot, width = 4.8, height = 10.2)
