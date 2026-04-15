library(ggplot2)
library(grid)
library(gridExtra)

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0) {
  normalizePath(sub("^--file=", "", script_arg[1]))
} else {
  normalizePath(getwd())
}
script_dir <- if (file.info(script_path)$isdir) script_path else dirname(script_path)
figure_dir <- file.path(script_dir, "..", "figure")

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
  contour = "#B8C4CF",
  feasible_line = "#2C7FB8",
  feasible_fill = "#DCEAF6",
  min_dir = "#1B9E77",
  max_dir = "#D95F02",
  point = "#11324D",
  accent = "#B14D6C",
  data = "#6C8AA3"
)

rot_mat <- function(angle) {
  matrix(
    c(cos(angle), -sin(angle), sin(angle), cos(angle)),
    nrow = 2,
    byrow = TRUE
  )
}

# -----------------------------------------------------------------------------
# Figure 1: quadratic form on the unit circle
# -----------------------------------------------------------------------------

eigvals <- c(0.55, 2.20)
rotation <- rot_mat(pi / 6)
A <- rotation %*% diag(eigvals) %*% t(rotation)
eig <- eigen(A)
v_max <- eig$vectors[, 1]
v_min <- eig$vectors[, 2]

quad_value <- function(x1, x2, A) {
  A[1, 1] * x1^2 + 2 * A[1, 2] * x1 * x2 + A[2, 2] * x2^2
}

grid_df <- expand.grid(
  x1 = seq(-1.55, 1.55, length.out = 260),
  x2 = seq(-1.55, 1.55, length.out = 260)
)
grid_df$z <- quad_value(grid_df$x1, grid_df$x2, A)

theta <- seq(0, 2 * pi, length.out = 500)
circle_df <- data.frame(
  x1 = cos(theta),
  x2 = sin(theta)
)

dir_df <- data.frame(
  x = c(0, 0),
  y = c(0, 0),
  xend = c(v_min[1], v_max[1]),
  yend = c(v_min[2], v_max[2]),
  kind = c("min", "max")
)

point_df <- data.frame(
  x1 = c(v_min[1], -v_min[1], v_max[1], -v_max[1]),
  x2 = c(v_min[2], -v_min[2], v_max[2], -v_max[2]),
  kind = c("min", "min", "max", "max")
)

p1 <- ggplot() +
  geom_contour(
    data = grid_df,
    aes(x = x1, y = x2, z = z),
    bins = 10,
    color = cols$contour,
    linewidth = 0.45
  ) +
  geom_path(
    data = circle_df,
    aes(x = x1, y = x2),
    color = cols$feasible_line,
    linewidth = 1.25
  ) +
  geom_segment(
    data = dir_df,
    aes(x = x, y = y, xend = xend, yend = yend, color = kind),
    linewidth = 1.2,
    arrow = arrow(length = unit(0.18, "cm"))
  ) +
  geom_point(
    data = point_df,
    aes(x = x1, y = x2, fill = kind),
    shape = 21,
    size = 3.3,
    color = "white",
    stroke = 0.35,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c(min = cols$min_dir, max = cols$max_dir)) +
  scale_fill_manual(values = c(min = cols$min_dir, max = cols$max_dir)) +
  annotate("text", x = -1.00, y = 1.18, label = "||x||2 = 1", color = cols$feasible_line, size = 4.3, fontface = "bold") +
  annotate("text", x = 0.82 * v_min[1], y = 0.82 * v_min[2] + 0.12, label = "min direction", color = cols$min_dir, size = 4.0, fontface = "bold") +
  annotate("text", x = 0.78 * v_max[1] + 0.05, y = 0.78 * v_max[2] - 0.16, label = "max direction", color = cols$max_dir, size = 4.0, fontface = "bold") +
  coord_equal(xlim = c(-1.45, 1.45), ylim = c(-1.35, 1.35), expand = FALSE) +
  labs(x = expression(x[1]), y = expression(x[2])) +
  theme(
    axis.line = element_line(color = "grey40"),
    legend.position = "none"
  )

ggsave(file.path(figure_dir, "norm_constrained_circle.pdf"), p1, width = 7.1, height = 5.1)

# -----------------------------------------------------------------------------
# Figure 2: smallest vs largest k eigenvalues
# -----------------------------------------------------------------------------

spectrum <- c(-1.8, -1.1, -0.5, 0.2, 0.9, 1.6, 2.4)
k <- 3
spec_df <- data.frame(
  idx = seq_along(spectrum),
  lambda = spectrum
)

make_spectrum_panel <- function(select_kind) {
  df <- spec_df
  if (select_kind == "min") {
    df$selected <- ifelse(df$idx <= k, "selected", "other")
    title <- "Minimization variant"
    subtitle <- "pick the smallest k eigenvalues"
    select_col <- cols$min_dir
  } else {
    df$selected <- ifelse(df$idx > nrow(df) - k, "selected", "other")
    title <- "Maximization variant"
    subtitle <- "pick the largest k eigenvalues"
    select_col <- cols$max_dir
  }

  ggplot(df, aes(x = factor(idx), y = lambda, fill = selected)) +
    geom_col(width = 0.72, color = "white") +
    geom_hline(yintercept = 0, color = "grey55", linewidth = 0.5) +
    scale_fill_manual(values = c(selected = select_col, other = cols$feasible_fill)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "sorted eigenvalue index",
      y = expression(lambda[i])
    ) +
    theme(
      legend.position = "none",
      axis.line = element_line(color = "grey40")
    )
}

p2_left <- make_spectrum_panel("min")
p2_right <- make_spectrum_panel("max")
p2 <- arrangeGrob(p2_left, p2_right, ncol = 2)

ggsave(file.path(figure_dir, "norm_constrained_spectrum.pdf"), p2, width = 10.0, height = 4.4)

# -----------------------------------------------------------------------------
# Figure 3: PCA as a norm-constrained eigenvector problem
# -----------------------------------------------------------------------------

set.seed(2026)
n <- 260
cov_rot <- rot_mat(0.62)
cov_mat <- cov_rot %*% diag(c(3.9, 0.75)) %*% t(cov_rot)
z <- matrix(rnorm(2 * n), ncol = 2)
samples <- z %*% chol(cov_mat)
samples <- scale(samples, center = TRUE, scale = FALSE)
samples_df <- data.frame(x1 = samples[, 1], x2 = samples[, 2])

sample_cov <- cov(samples)
pc <- eigen(sample_cov)
pc1 <- 1.95 * sqrt(pc$values[1]) * pc$vectors[, 1]
pc2 <- 1.45 * sqrt(pc$values[2]) * pc$vectors[, 2]

pc_df <- data.frame(
  x = c(0, 0),
  y = c(0, 0),
  xend = c(pc1[1], pc2[1]),
  yend = c(pc1[2], pc2[2]),
  kind = c("PC1", "PC2")
)

p3 <- ggplot(samples_df, aes(x = x1, y = x2)) +
  geom_point(color = cols$data, alpha = 0.55, size = 1.9) +
  geom_segment(
    data = pc_df,
    aes(x = x, y = y, xend = xend, yend = yend, color = kind),
    inherit.aes = FALSE,
    linewidth = 1.3,
    arrow = arrow(length = unit(0.18, "cm"))
  ) +
  scale_color_manual(values = c(PC1 = cols$max_dir, PC2 = cols$min_dir)) +
  annotate("text", x = pc1[1] + 0.15, y = pc1[2] + 0.12, label = "PC1: max variance", color = cols$max_dir, size = 4.1, fontface = "bold") +
  annotate("text", x = pc2[1] + 0.10, y = pc2[2] - 0.22, label = "PC2: orthogonal next direction", color = cols$min_dir, size = 4.0, fontface = "bold") +
  coord_equal(expand = FALSE) +
  labs(x = expression(x[1]), y = expression(x[2])) +
  theme(
    axis.line = element_line(color = "grey40"),
    legend.position = "none"
  )

ggsave(file.path(figure_dir, "norm_constrained_pca.pdf"), p3, width = 7.1, height = 5.0)
