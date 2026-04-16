library(ggplot2)
library(grid)
library(gridExtra)
source("box_toy_geometry.R")

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 13),
      axis.title = element_text(size = 12)
    )
)

cols <- list(
  contour = "#AAB4BF",
  feasible_fill = "#DCEAF6",
  feasible_line = "#2C7FB8",
  raw_step = "#E67E22",
  projected_step = "#1B9E77",
  point = "#11324D",
  optimum = "#B14D6C"
)

objective_center <- c(1.55, -0.15)
objective_hessian <- matrix(c(1.45, 0.38, 0.38, 1.10), nrow = 2)

quad_value <- function(x1, x2, center = objective_center, H = objective_hessian) {
  dx1 <- x1 - center[1]
  dx2 <- x2 - center[2]
  0.5 * (H[1, 1] * dx1^2 + 2 * H[1, 2] * dx1 * dx2 + H[2, 2] * dx2^2)
}

quad_grad <- function(x, center = objective_center, H = objective_hessian) {
  drop(H %*% (x - center))
}

rot_mat <- function(angle) {
  matrix(
    c(cos(angle), -sin(angle), sin(angle), cos(angle)),
    nrow = 2,
    byrow = TRUE
  )
}

ellipse_points <- function(center, axes, angle, n = 500) {
  theta <- seq(0, 2 * pi, length.out = n)
  basis <- rbind(axes[1] * cos(theta), axes[2] * sin(theta))
  pts <- t(rot_mat(angle) %*% basis)
  data.frame(
    x1 = center[1] + pts[, 1],
    x2 = center[2] + pts[, 2],
    theta = theta
  )
}

inside_ellipse <- function(x, center, axes, angle) {
  local <- drop(rot_mat(-angle) %*% (x - center))
  sum((local / axes)^2) <= 1 + 1e-9
}

project_to_ellipse <- function(z, center, axes, angle) {
  obj <- function(theta) {
    candidate <- center + drop(rot_mat(angle) %*% c(axes[1] * cos(theta), axes[2] * sin(theta)))
    sum((z - candidate)^2)
  }
  theta_star <- optimize(obj, interval = c(0, 2 * pi))$minimum
  center + drop(rot_mat(angle) %*% c(axes[1] * cos(theta_star), axes[2] * sin(theta_star)))
}

contour_df <- expand.grid(
  x1 = seq(-1.30, 2.30, length.out = 240),
  x2 = seq(-1.20, 1.90, length.out = 220)
)
contour_df$z <- quad_value(contour_df$x1, contour_df$x2)

base_contours <- function(df = contour_df) {
  list(
    geom_contour(
      data = df,
      aes(x = x1, y = x2, z = z),
      bins = 10,
      color = cols$contour,
      linewidth = 0.35
    ),
    coord_equal(xlim = c(-1.15, 2.10), ylim = c(-1.00, 1.75), expand = FALSE),
    labs(x = expression(x[1]), y = expression(x[2])),
    theme(
      legend.position = "none",
      panel.border = element_blank(),
      axis.line = element_line(color = "grey40")
    )
  )
}

# -----------------------------------------------------------------------------
# Figure 1: generic projected gradient step on a convex set
# -----------------------------------------------------------------------------

ellipse_center <- c(-0.05, 0.58)
ellipse_axes <- c(1.18, 0.72)
ellipse_angle <- 0.55
ellipse_df <- ellipse_points(ellipse_center, ellipse_axes, ellipse_angle)

x_t <- c(0.40, 0.95)
alpha <- 1.05
raw_step <- x_t - alpha * quad_grad(x_t)
while (inside_ellipse(raw_step, ellipse_center, ellipse_axes, ellipse_angle)) {
  alpha <- alpha * 1.15
  raw_step <- x_t - alpha * quad_grad(x_t)
}
x_next <- project_to_ellipse(raw_step, ellipse_center, ellipse_axes, ellipse_angle)

p1 <- ggplot() +
  base_contours() +
  geom_polygon(
    data = ellipse_df,
    aes(x = x1, y = x2),
    inherit.aes = FALSE,
    fill = cols$feasible_fill,
    color = cols$feasible_line,
    linewidth = 1.15,
    alpha = 0.9
  ) +
  geom_segment(
    aes(x = x_t[1], y = x_t[2], xend = raw_step[1], yend = raw_step[2]),
    color = cols$raw_step,
    linewidth = 1.1,
    arrow = arrow(length = unit(0.20, "cm"))
  ) +
  geom_segment(
    aes(x = raw_step[1], y = raw_step[2], xend = x_next[1], yend = x_next[2]),
    color = cols$projected_step,
    linewidth = 1.1,
    arrow = arrow(length = unit(0.20, "cm"))
  ) +
  geom_point(
    data = data.frame(
      x1 = c(x_t[1], raw_step[1], x_next[1], objective_center[1]),
      x2 = c(x_t[2], raw_step[2], x_next[2], objective_center[2]),
      kind = factor(c("start", "raw", "proj", "opt"))
    ),
    aes(x = x1, y = x2, fill = kind),
    shape = 21,
    size = 3.1,
    color = "white",
    stroke = 0.35,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      start = cols$point,
      raw = cols$raw_step,
      proj = cols$projected_step,
      opt = cols$optimum
    )
  ) +
  annotate("text", x = -0.75, y = 1.42, label = "feasible set S", color = cols$feasible_line, size = 4.4, fontface = "bold") +
  annotate("text", x = x_t[1] - 0.08, y = x_t[2] + 0.12, label = "x[t]", parse = TRUE, color = cols$point, size = 4.4) +
  annotate("text", x = raw_step[1] + 0.16, y = raw_step[2] - 0.02, label = "tilde(x)[t+1]", parse = TRUE, color = cols$raw_step, size = 4.2) +
  annotate("text", x = x_next[1] - 0.12, y = x_next[2] - 0.13, label = "x[t+1]", parse = TRUE, color = cols$projected_step, size = 4.4) +
  annotate("text", x = objective_center[1] - 0.02, y = objective_center[2] - 0.14, label = "unconstrained minimum", color = cols$optimum, size = 4.0)

ggsave("../figure/primal_gd_projected_step.pdf", p1, width = 7.6, height = 5.1)

# -----------------------------------------------------------------------------
# Figure 2: equality constraints => project onto tangent space
# -----------------------------------------------------------------------------

A <- matrix(c(1, 1), nrow = 1)
P_null <- diag(2) - t(A) %*% solve(A %*% t(A)) %*% A
x_t_eq <- c(0.20, 0.80)
g_eq <- quad_grad(x_t_eq)
raw_eq <- x_t_eq - 0.48 * g_eq
d_eq <- -drop(P_null %*% g_eq)
x_next_eq <- x_t_eq + 0.75 * d_eq

line_df <- data.frame(x1 = seq(-0.15, 1.45, length.out = 200))
line_df$x2 <- 1 - line_df$x1

p2 <- ggplot() +
  base_contours() +
  geom_path(
    data = line_df,
    aes(x = x1, y = x2),
    color = cols$feasible_line,
    linewidth = 1.25
  ) +
  geom_segment(
    aes(x = x_t_eq[1], y = x_t_eq[2], xend = raw_eq[1], yend = raw_eq[2]),
    color = cols$raw_step,
    linewidth = 1.1,
    arrow = arrow(length = unit(0.20, "cm"))
  ) +
  geom_segment(
    aes(x = x_t_eq[1], y = x_t_eq[2], xend = x_next_eq[1], yend = x_next_eq[2]),
    color = cols$projected_step,
    linewidth = 1.1,
    arrow = arrow(length = unit(0.20, "cm"))
  ) +
  geom_point(
    data = data.frame(
      x1 = c(x_t_eq[1], raw_eq[1], x_next_eq[1]),
      x2 = c(x_t_eq[2], raw_eq[2], x_next_eq[2]),
      kind = factor(c("start", "raw", "proj"))
    ),
    aes(x = x1, y = x2, fill = kind),
    shape = 21,
    size = 3.1,
    color = "white",
    stroke = 0.35,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      start = cols$point,
      raw = cols$raw_step,
      proj = cols$projected_step
    )
  ) +
  annotate("text", x = 0.94, y = 0.18, label = "A x = b", color = cols$feasible_line, size = 4.4, fontface = "bold") +
  annotate("text", x = x_t_eq[1] - 0.05, y = x_t_eq[2] + 0.12, label = "x[t]", parse = TRUE, color = cols$point, size = 4.4) +
  annotate("text", x = raw_eq[1] + 0.17, y = raw_eq[2] - 0.02, label = "raw GD step", color = cols$raw_step, size = 4.0) +
  annotate("text", x = x_next_eq[1] + 0.10, y = x_next_eq[2] - 0.08, label = "projected step", color = cols$projected_step, size = 4.0)

ggsave("../figure/primal_gd_equality.pdf", p2, width = 7.2, height = 5.0)

# -----------------------------------------------------------------------------
# Figure 3: box constraints => coordinate-wise clipping
# -----------------------------------------------------------------------------

box_grid <- box_toy_grid(
  x1_lim = c(-0.02, 1.30),
  x2_lim = c(-0.02, 0.98),
  n1 = 240,
  n2 = 220
)

x_t_box <- c(0.22, 0.78)
alpha_box <- 0.72
raw_box <- x_t_box - alpha_box * box_toy_grad(x_t_box)
proj_box <- box_toy_project(raw_box)

p3 <- ggplot() +
  geom_contour(
    data = box_grid,
    aes(x = x1, y = x2, z = z),
    bins = 10,
    color = cols$contour,
    linewidth = 0.35
  ) +
  geom_rect(
    aes(
      xmin = box_toy_bounds$w1[1],
      xmax = box_toy_bounds$w1[2],
      ymin = box_toy_bounds$w2[1],
      ymax = box_toy_bounds$w2[2]
    ),
    fill = cols$feasible_fill,
    color = cols$feasible_line,
    linewidth = 1.1,
    alpha = 0.9
  ) +
  geom_segment(
    aes(x = x_t_box[1], y = x_t_box[2], xend = raw_box[1], yend = raw_box[2]),
    color = cols$raw_step,
    linewidth = 1.08,
    arrow = arrow(length = unit(0.18, "cm"))
  ) +
  geom_segment(
    aes(x = raw_box[1], y = raw_box[2], xend = proj_box[1], yend = proj_box[2]),
    color = cols$projected_step,
    linewidth = 1.08,
    arrow = arrow(length = unit(0.18, "cm"))
  ) +
  geom_point(
    data = data.frame(
      x1 = c(
        x_t_box[1],
        raw_box[1],
        proj_box[1],
        box_toy_center[1],
        box_toy_optimum[1]
      ),
      x2 = c(
        x_t_box[2],
        raw_box[2],
        proj_box[2],
        box_toy_center[2],
        box_toy_optimum[2]
      ),
      kind = factor(
        c("start", "raw", "proj", "unconstrained", "optimum"),
        levels = c("start", "raw", "proj", "unconstrained", "optimum")
      )
    ),
    aes(x = x1, y = x2, fill = kind),
    shape = 21,
    size = 3.0,
    color = "white",
    stroke = 0.35,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      start = cols$point,
      raw = cols$raw_step,
      proj = cols$projected_step,
      unconstrained = cols$raw_step,
      optimum = cols$optimum
    )
  ) +
  annotate("text", x = 0.46, y = 0.98, label = "feasible box", color = cols$feasible_line, size = 4.5, fontface = "bold") +
  annotate("text", x = x_t_box[1] - 0.02, y = x_t_box[2] + 0.10, label = "w[t]", parse = TRUE, color = cols$point, size = 4.2) +
  annotate("text", x = raw_box[1] + 0.03, y = raw_box[2] + 0.08, label = "raw GD step", color = cols$raw_step, size = 4.0) +
  annotate("text", x = proj_box[1] - 0.02, y = proj_box[2] + 0.09, label = "after clipping", color = cols$projected_step, size = 4.0) +
  annotate("text", x = box_toy_center[1] + 0.01, y = box_toy_center[2] - 0.12, label = "unconstrained minimum", color = cols$raw_step, size = 4.0) +
  annotate("text", x = box_toy_optimum[1] - 0.10, y = box_toy_optimum[2] - 0.12, label = "constrained optimum", color = cols$optimum, size = 4.0) +
  coord_equal(xlim = c(-0.02, 1.30), ylim = c(-0.02, 0.98), expand = FALSE) +
  labs(x = expression(w[1]), y = expression(w[2])) +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    axis.line = element_line(color = "grey40")
  )

ggsave("../figure/primal_gd_box.pdf", p3, width = 7.4, height = 5.0)
