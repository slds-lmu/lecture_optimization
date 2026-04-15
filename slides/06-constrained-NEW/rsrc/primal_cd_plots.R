library(ggplot2)
library(grid)
library(gridExtra)

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 13),
      axis.title = element_text(size = 12)
    )
)

cols <- list(
  contour = "#B8C4CF",
  feasible_fill = "#DCEAF6",
  feasible_line = "#2C7FB8",
  interval = "#1B9E77",
  path = "#D95F02",
  point = "#11324D",
  optimum = "#B14D6C",
  clip = "#C0392B",
  bound = "#4C78A8"
)

quad_value <- function(x1, x2, center, H) {
  dx1 <- x1 - center[1]
  dx2 <- x2 - center[2]
  0.5 * (H[1, 1] * dx1^2 + 2 * H[1, 2] * dx1 * dx2 + H[2, 2] * dx2^2)
}

# -----------------------------------------------------------------------------
# Figure 1: Convex slices become intervals
# -----------------------------------------------------------------------------

a <- 1.25
b <- 0.92
theta <- seq(0, 2 * pi, length.out = 500)
ellipse_df <- data.frame(
  x1 = a * cos(theta),
  x2 = b * sin(theta)
)

x2_fix <- 0.34
x1_fix <- -0.58

h_half <- a * sqrt(1 - (x2_fix / b)^2)
v_half <- b * sqrt(1 - (x1_fix / a)^2)

p1 <- ggplot() +
  geom_polygon(
    data = ellipse_df,
    aes(x = x1, y = x2),
    fill = cols$feasible_fill,
    color = cols$feasible_line,
    linewidth = 1.15,
    alpha = 0.95
  ) +
  geom_segment(
    aes(x = -1.55, y = x2_fix, xend = 1.55, yend = x2_fix),
    color = "grey40",
    linewidth = 0.7
  ) +
  geom_segment(
    aes(x = x1_fix, y = -1.18, xend = x1_fix, yend = 1.18),
    color = "grey40",
    linewidth = 0.7
  ) +
  annotate(
    "segment",
    x = -h_half, y = x2_fix, xend = h_half, yend = x2_fix,
    color = cols$interval, linewidth = 2.4
  ) +
  annotate(
    "segment",
    x = x1_fix, y = -v_half, xend = x1_fix, yend = v_half,
    color = cols$interval, linewidth = 2.4
  ) +
  geom_point(
    aes(x = x1_fix, y = x2_fix),
    color = cols$point,
    size = 2.8
  ) +
  annotate("text", x = 0.12, y = 1.04, label = "convex feasible set S", color = cols$feasible_line, size = 4.6, fontface = "bold") +
  annotate("text", x = 0.70, y = x2_fix + 0.11, label = "fix w2 -> optimize w1", color = "grey20", size = 4.0) +
  annotate("text", x = x1_fix + 0.34, y = -0.76, label = "fix w1 -> optimize w2", angle = 90, color = "grey20", size = 4.0) +
  annotate("text", x = 0.05, y = x2_fix - 0.10, label = "1D feasible interval", color = cols$interval, size = 4.1, fontface = "bold") +
  annotate("text", x = x1_fix - 0.16, y = -0.03, label = "1D feasible interval", color = cols$interval, size = 4.1, angle = 90, fontface = "bold") +
  coord_equal(xlim = c(-1.55, 1.55), ylim = c(-1.12, 1.12), expand = FALSE) +
  labs(x = expression(w[1]), y = expression(w[2])) +
  theme(
    legend.position = "none",
    axis.line = element_line(color = "grey40")
  )

ggsave("../figure/primal_cd_intervals.pdf", p1, width = 7.3, height = 5.1)

# -----------------------------------------------------------------------------
# Figure 2: Coordinate descent path in a box
# -----------------------------------------------------------------------------

box_center <- c(1.35, 0.05)
box_hessian <- matrix(c(1.40, 0.45, 0.45, 1.10), nrow = 2)

box_value <- function(x1, x2) {
  quad_value(x1, x2, center = box_center, H = box_hessian)
}

coord_update_x1 <- function(x2) {
  unclipped <- box_center[1] - box_hessian[1, 2] / box_hessian[1, 1] * (x2 - box_center[2])
  pmin(pmax(unclipped, 0), 1)
}

coord_update_x2 <- function(x1) {
  unclipped <- box_center[2] - box_hessian[1, 2] / box_hessian[2, 2] * (x1 - box_center[1])
  pmin(pmax(unclipped, 0), 0.9)
}

path_pts <- matrix(NA_real_, nrow = 5, ncol = 2)
path_pts[1, ] <- c(0.14, 0.76)
path_pts[2, ] <- c(coord_update_x1(path_pts[1, 2]), path_pts[1, 2])
path_pts[3, ] <- c(path_pts[2, 1], coord_update_x2(path_pts[2, 1]))
path_pts[4, ] <- c(coord_update_x1(path_pts[3, 2]), path_pts[3, 2])
path_pts[5, ] <- c(path_pts[4, 1], coord_update_x2(path_pts[4, 1]))
path_df <- data.frame(
  x1 = path_pts[, 1],
  x2 = path_pts[, 2],
  step = seq_len(nrow(path_pts))
)
path_seg_df <- data.frame(
  x = path_pts[-nrow(path_pts), 1],
  y = path_pts[-nrow(path_pts), 2],
  xend = path_pts[-1, 1],
  yend = path_pts[-1, 2]
)

box_grid <- expand.grid(
  x1 = seq(-0.02, 1.40, length.out = 220),
  x2 = seq(-0.02, 0.98, length.out = 220)
)
box_grid$z <- box_value(box_grid$x1, box_grid$x2)

opt_box <- c(coord_update_x1(coord_update_x2(1)), coord_update_x2(1))
opt_box <- c(1, coord_update_x2(1))

p2 <- ggplot() +
  geom_contour(
    data = box_grid,
    aes(x = x1, y = x2, z = z),
    bins = 10,
    color = cols$contour,
    linewidth = 0.35
  ) +
  geom_rect(
    aes(xmin = 0, xmax = 1, ymin = 0, ymax = 0.9),
    fill = cols$feasible_fill,
    color = cols$feasible_line,
    linewidth = 1.15,
    alpha = 0.92
  ) +
  geom_path(
    data = path_df,
    aes(x = x1, y = x2),
    color = cols$path,
    linewidth = 1.15
  ) +
  geom_segment(
    data = path_seg_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = cols$path,
    linewidth = 1.15,
    arrow = arrow(length = unit(0.17, "cm"))
  ) +
  geom_point(
    data = path_df,
    aes(x = x1, y = x2),
    fill = cols$point,
    color = "white",
    shape = 21,
    size = 3.0,
    stroke = 0.35
  ) +
  geom_point(
    aes(x = opt_box[1], y = opt_box[2]),
    fill = cols$optimum,
    color = "white",
    shape = 21,
    size = 3.2,
    stroke = 0.35
  ) +
  annotate("text", x = 0.57, y = 0.97, label = "box-constrained feasible set", color = cols$feasible_line, size = 4.4, fontface = "bold") +
  annotate("text", x = 0.25, y = 0.82, label = "start", color = cols$point, size = 4.0) +
  annotate("text", x = 0.52, y = 0.70, label = "x1 update", color = cols$path, size = 4.0) +
  annotate("text", x = 0.89, y = 0.48, label = "x2 update", color = cols$path, size = 4.0, angle = 90) +
  annotate("text", x = 0.84, y = 0.13, label = "constrained optimum", color = cols$optimum, size = 4.1) +
  coord_equal(xlim = c(-0.02, 1.30), ylim = c(-0.02, 0.98), expand = FALSE) +
  labs(x = expression(w[1]), y = expression(w[2])) +
  theme(
    legend.position = "none",
    axis.line = element_line(color = "grey40")
  )

ggsave("../figure/primal_cd_path.pdf", p2, width = 7.5, height = 5.0)

# -----------------------------------------------------------------------------
# Figure 3: Truncation / clipping in one dimension
# -----------------------------------------------------------------------------

make_trunc_panel <- function(center, lower = 0.15, upper = 1.00, title) {
  df <- data.frame(z = seq(-0.05, 1.45, length.out = 300))
  df$q <- 0.95 * (df$z - center)^2 + 0.18
  q_lower <- 0.95 * (lower - center)^2 + 0.18
  q_upper <- 0.95 * (upper - center)^2 + 0.18
  clipped <- pmin(pmax(center, lower), upper)
  q_clip <- 0.95 * (clipped - center)^2 + 0.18

  ggplot(df, aes(x = z, y = q)) +
    annotate(
      "rect",
      xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf,
      fill = cols$feasible_fill,
      alpha = 0.5
    ) +
    geom_line(color = cols$bound, linewidth = 1.1) +
    geom_vline(xintercept = c(lower, upper), color = cols$feasible_line, linewidth = 0.8, linetype = "dashed") +
    annotate("point", x = center, y = 0.18, color = cols$path, size = 3.0) +
    annotate("point", x = clipped, y = q_clip, color = cols$clip, size = 3.1) +
    annotate("text", x = (lower + upper) / 2, y = max(df$q) + 0.04, label = "feasible interval", color = cols$feasible_line, size = 4.0, fontface = "bold") +
    annotate("text", x = center, y = 0.08, label = "unconstrained\nminimizer", color = cols$path, size = 3.7) +
    annotate("text", x = clipped, y = q_clip + 0.10, label = "after truncation", color = cols$clip, size = 3.7) +
    labs(x = expression(w[i]), y = "1D objective", title = title) +
    coord_cartesian(xlim = c(-0.05, 1.45)) +
    theme(
      legend.position = "none",
      axis.line = element_line(color = "grey40")
    )
}

p3_left <- make_trunc_panel(center = 0.62, title = "No clipping")
p3_right <- make_trunc_panel(center = 1.22, title = "Clip to upper bound")
p3 <- arrangeGrob(p3_left, p3_right, ncol = 2)

ggsave("../figure/primal_cd_truncation.pdf", p3, width = 9.4, height = 4.4)
