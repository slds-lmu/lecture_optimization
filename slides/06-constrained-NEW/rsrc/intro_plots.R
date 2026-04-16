library(ggplot2)
library(grid)
library(gridExtra)
source("box_toy_geometry.R")

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
  contour = "#AAB4BF",
  feasible_fill = "#DCEAF6",
  feasible_line = "#2C7FB8",
  unconstrained = "#D95F02",
  constrained = "#B14D6C",
  point = "#12344D",
  primal = "#2C7FB8",
  dual = "#D95F02",
  hybrid = "#1B9E77",
  hybrid_fill = "#DFF2EB",
  norm_fill = "#FBE4D5",
  dark = "#12344D"
)

box_grid <- box_toy_grid(
  x1_lim = c(-0.05, 1.32),
  x2_lim = c(-0.05, 0.98),
  n1 = 240,
  n2 = 220
)

box_panel <- function() {
  list(
    geom_contour(
      data = box_grid,
      aes(x = x1, y = x2, z = z),
      bins = 10,
      color = cols$contour,
      linewidth = 0.35
    ),
    geom_rect(
      aes(
        xmin = box_toy_bounds$w1[1],
        xmax = box_toy_bounds$w1[2],
        ymin = box_toy_bounds$w2[1],
        ymax = box_toy_bounds$w2[2]
      ),
      fill = cols$feasible_fill,
      color = cols$feasible_line,
      linewidth = 1.15,
      alpha = 0.90
    ),
    coord_equal(
      xlim = c(-0.02, 1.27),
      ylim = c(-0.02, 0.96),
      expand = FALSE
    ),
    labs(x = expression(w[1]), y = expression(w[2])),
    theme(
      legend.position = "none",
      panel.border = element_blank(),
      axis.line = element_line(color = "grey40")
    )
  )
}

# -----------------------------------------------------------------------------
# Figure 1: feasible-region viewpoint on the recurring toy problem
# -----------------------------------------------------------------------------

p1 <- ggplot() +
  box_panel() +
  geom_point(
    data = data.frame(
      x1 = c(box_toy_center[1], box_toy_optimum[1]),
      x2 = c(box_toy_center[2], box_toy_optimum[2]),
      kind = factor(c("unconstrained", "constrained"), levels = c("unconstrained", "constrained"))
    ),
    aes(x = x1, y = x2, fill = kind),
    shape = 21,
    size = 3.3,
    color = "white",
    stroke = 0.35,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      unconstrained = cols$unconstrained,
      constrained = cols$constrained
    )
  ) +
  annotate("text", x = 0.35, y = 0.94, label = "feasible box S", color = cols$feasible_line, size = 4.4, fontface = "bold") +
  annotate("text", x = 1.08, y = 0.52, label = "constrained optimum", color = cols$constrained, size = 4.0, hjust = 0) +
  annotate("text", x = 1.12, y = 0.10, label = "unconstrained minimum", color = cols$unconstrained, size = 4.0, hjust = 0) +
  annotate(
    "curve",
    x = 1.03,
    y = 0.47,
    xend = box_toy_optimum[1] + 0.01,
    yend = box_toy_optimum[2] + 0.01,
    curvature = 0.18,
    color = cols$constrained,
    linewidth = 0.7,
    arrow = arrow(length = unit(0.16, "cm"))
  ) +
  annotate(
    "curve",
    x = 1.10,
    y = 0.13,
    xend = box_toy_center[1] - 0.01,
    yend = box_toy_center[2],
    curvature = -0.12,
    color = cols$unconstrained,
    linewidth = 0.7,
    arrow = arrow(length = unit(0.16, "cm"))
  )

ggsave("../figure/intro_feasible_region.pdf", p1, width = 7.4, height = 5.0)

# -----------------------------------------------------------------------------
# Figure 2: a raw unconstrained step can leave feasibility
# -----------------------------------------------------------------------------

x_t <- c(0.22, 0.78)
alpha <- 0.72
raw_step <- x_t - alpha * box_toy_grad(x_t)

p2 <- ggplot() +
  box_panel() +
  geom_segment(
    aes(x = x_t[1], y = x_t[2], xend = raw_step[1], yend = raw_step[2]),
    color = cols$unconstrained,
    linewidth = 1.15,
    arrow = arrow(length = unit(0.20, "cm"))
  ) +
  geom_point(
    data = data.frame(
      x1 = c(x_t[1], raw_step[1]),
      x2 = c(x_t[2], raw_step[2]),
      kind = factor(c("start", "raw"), levels = c("start", "raw"))
    ),
    aes(x = x1, y = x2, fill = kind),
    shape = 21,
    size = 3.2,
    color = "white",
    stroke = 0.35,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c(
      start = cols$point,
      raw = cols$unconstrained
    )
  ) +
  annotate("text", x = 0.28, y = 0.91, label = "x[t]", parse = TRUE, color = cols$point, size = 4.4) +
  annotate("text", x = 0.39, y = 0.84, label = "feasible", color = cols$point, size = 3.8, hjust = 0) +
  annotate("text", x = 1.02, y = 0.80, label = "tilde(x)[t+1]", parse = TRUE, color = cols$unconstrained, size = 4.2, hjust = 0) +
  annotate("text", x = 1.03, y = 0.71, label = "outside S", color = cols$unconstrained, size = 3.8, hjust = 0) +
  annotate("text", x = 0.28, y = 0.07, label = "same issue for many unconstrained update rules", color = cols$dark, size = 4.0)

ggsave("../figure/intro_unconstrained_step.pdf", p2, width = 7.4, height = 5.0)

# -----------------------------------------------------------------------------
# Figure 3: primal, dual, and hybrid roadmap
# -----------------------------------------------------------------------------

roadmap_boxes <- data.frame(
  xmin = c(4.1, 0.5, 4.4, 8.3, 0.7, 0.7, 4.6, 8.5, 8.5, 3.1),
  xmax = c(7.9, 3.5, 7.4, 11.3, 3.3, 3.3, 7.2, 11.1, 11.1, 8.7),
  ymin = c(5.0, 3.4, 3.4, 3.4, 2.1, 1.2, 1.65, 2.1, 1.2, 0.3),
  ymax = c(6.1, 4.45, 4.45, 4.45, 2.95, 2.05, 2.5, 2.95, 2.05, 1.15),
  fill = c(
    "#F4F6F8",
    cols$feasible_fill,
    cols$norm_fill,
    cols$hybrid_fill,
    cols$feasible_fill,
    cols$feasible_fill,
    cols$norm_fill,
    cols$hybrid_fill,
    cols$hybrid_fill,
    "#F4F6F8"
  ),
  color = c(
    cols$dark,
    cols$primal,
    cols$dual,
    cols$hybrid,
    cols$primal,
    cols$primal,
    cols$dual,
    cols$hybrid,
    cols$hybrid,
    cols$dark
  )
)

p3 <- ggplot() +
  geom_rect(
    data = roadmap_boxes,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = roadmap_boxes$fill,
    color = roadmap_boxes$color,
    linewidth = 1.0
  ) +
  annotate("segment", x = 6.0, y = 5.0, xend = 2.0, yend = 4.45, color = cols$dark, linewidth = 0.8, arrow = arrow(length = unit(0.16, "cm"))) +
  annotate("segment", x = 6.0, y = 5.0, xend = 5.9, yend = 4.45, color = cols$dark, linewidth = 0.8, arrow = arrow(length = unit(0.16, "cm"))) +
  annotate("segment", x = 6.0, y = 5.0, xend = 9.8, yend = 4.45, color = cols$dark, linewidth = 0.8, arrow = arrow(length = unit(0.16, "cm"))) +
  annotate("segment", x = 2.0, y = 3.4, xend = 2.0, yend = 2.95, color = cols$primal, linewidth = 0.8, arrow = arrow(length = unit(0.16, "cm"))) +
  annotate("segment", x = 2.0, y = 2.1, xend = 2.0, yend = 2.05, color = cols$primal, linewidth = 0.8, arrow = arrow(length = unit(0.16, "cm"))) +
  annotate("segment", x = 5.9, y = 3.4, xend = 5.9, yend = 2.5, color = cols$dual, linewidth = 0.8, arrow = arrow(length = unit(0.16, "cm"))) +
  annotate("segment", x = 9.8, y = 3.4, xend = 9.8, yend = 2.95, color = cols$hybrid, linewidth = 0.8, arrow = arrow(length = unit(0.16, "cm"))) +
  annotate("segment", x = 9.8, y = 2.1, xend = 9.8, yend = 2.05, color = cols$hybrid, linewidth = 0.8, arrow = arrow(length = unit(0.16, "cm"))) +
  annotate("segment", x = 2.0, y = 1.2, xend = 5.1, yend = 1.15, color = cols$dark, linewidth = 0.8, arrow = arrow(length = unit(0.16, "cm"))) +
  annotate("segment", x = 5.9, y = 1.65, xend = 5.9, yend = 1.15, color = cols$dark, linewidth = 0.8, arrow = arrow(length = unit(0.16, "cm"))) +
  annotate("segment", x = 9.8, y = 1.2, xend = 6.7, yend = 1.15, color = cols$dark, linewidth = 0.8, arrow = arrow(length = unit(0.16, "cm"))) +
  annotate("text", x = 6.0, y = 5.72, label = "Constrained problem", color = cols$dark, size = 5.0, fontface = "bold") +
  annotate("text", x = 6.0, y = 5.28, label = "minimize over a feasible set S", color = cols$dark, size = 3.8) +
  annotate("text", x = 2.0, y = 4.03, label = "Primal", color = cols$primal, size = 4.9, fontface = "bold") +
  annotate("text", x = 2.0, y = 3.65, label = "keep or repair feasibility", color = cols$dark, size = 3.8) +
  annotate("text", x = 5.9, y = 4.03, label = "Dual", color = cols$dual, size = 4.9, fontface = "bold") +
  annotate("text", x = 5.9, y = 3.65, label = "relax with multipliers", color = cols$dark, size = 3.8) +
  annotate("text", x = 9.8, y = 4.03, label = "Hybrid", color = cols$hybrid, size = 4.9, fontface = "bold") +
  annotate("text", x = 9.8, y = 3.65, label = "blend primal and dual ideas", color = cols$dark, size = 3.8) +
  annotate("text", x = 2.0, y = 2.53, label = "Projected GD", color = cols$primal, size = 4.2, fontface = "bold") +
  annotate("text", x = 2.0, y = 1.63, label = "Constrained\ncoordinate descent", color = cols$primal, size = 4.0, fontface = "bold") +
  annotate("text", x = 5.9, y = 2.08, label = "Lagrangian\nrelaxation", color = cols$dual, size = 4.1, fontface = "bold") +
  annotate("text", x = 9.8, y = 2.53, label = "Penalty\nmethods", color = cols$hybrid, size = 4.0, fontface = "bold") +
  annotate("text", x = 9.8, y = 1.63, label = "Barrier / interior-point", color = cols$hybrid, size = 3.8, fontface = "bold") +
  annotate("text", x = 5.9, y = 0.76, label = "Final comparison: primal vs dual", color = cols$dark, size = 4.2, fontface = "bold") +
  coord_cartesian(xlim = c(0, 11.8), ylim = c(0, 6.4), expand = FALSE) +
  theme_void()

ggsave("../figure/intro_primal_dual_roadmap.pdf", p3, width = 9.6, height = 4.9)

# -----------------------------------------------------------------------------
# Figure 4: two recurring constraint classes
# -----------------------------------------------------------------------------

poly_df <- data.frame(
  x = c(-0.95, -0.35, 0.45, 1.00, 0.55, -0.30),
  y = c(-0.30, 0.95, 1.10, 0.10, -0.78, -0.95)
)

line_df <- data.frame(x = seq(-1.20, 1.15, length.out = 200))
line_df$y <- 0.25 - 0.55 * line_df$x

p_convex <- ggplot() +
  geom_polygon(
    data = poly_df,
    aes(x = x, y = y),
    fill = cols$feasible_fill,
    color = cols$feasible_line,
    linewidth = 1.05,
    alpha = 0.92
  ) +
  geom_path(
    data = line_df,
    aes(x = x, y = y),
    color = cols$primal,
    linewidth = 1.0,
    linetype = "dashed"
  ) +
  annotate("point", x = 0.05, y = 0.22, color = cols$dark, size = 2.5) +
  annotate("text", x = 0.05, y = 1.32, label = "Linear / convex constraints", color = cols$dark, size = 4.6, fontface = "bold") +
  annotate("text", x = -0.10, y = 0.48, label = "feasible set", color = cols$feasible_line, size = 4.0, fontface = "bold") +
  annotate("text", x = 0.80, y = -0.40, label = "A x <= b", color = cols$primal, size = 4.0) +
  annotate("text", x = -0.72, y = 0.86, label = "C x = d", color = cols$primal, size = 4.0, angle = -29) +
  coord_equal(xlim = c(-1.25, 1.20), ylim = c(-1.15, 1.45), expand = FALSE) +
  theme_void()

theta <- seq(0, 2 * pi, length.out = 400)
circle_df <- data.frame(x = cos(theta), y = sin(theta))
v1 <- c(0.88, 0.48)
v2 <- c(-0.48, 0.88)

p_norm <- ggplot() +
  geom_polygon(
    data = circle_df,
    aes(x = x, y = y),
    fill = cols$norm_fill,
    color = cols$dual,
    linewidth = 1.05,
    alpha = 0.92
  ) +
  geom_segment(
    aes(x = 0, y = 0, xend = v1[1], yend = v1[2]),
    color = cols$dual,
    linewidth = 1.0,
    arrow = arrow(length = unit(0.18, "cm"))
  ) +
  geom_segment(
    aes(x = 0, y = 0, xend = v2[1], yend = v2[2]),
    color = cols$hybrid,
    linewidth = 1.0,
    arrow = arrow(length = unit(0.18, "cm"))
  ) +
  annotate("point", x = 0, y = 0, color = cols$dark, size = 2.2) +
  annotate("text", x = 0.0, y = 1.33, label = "Norm constraints", color = cols$dark, size = 4.6, fontface = "bold") +
  annotate("text", x = 0.0, y = -1.16, label = "||x||2 <= r", color = cols$dual, size = 4.1) +
  annotate("text", x = 0.58, y = 0.72, label = "direction", color = cols$dual, size = 3.8) +
  annotate("text", x = -0.66, y = 0.84, label = "orthogonality", color = cols$hybrid, size = 3.8) +
  coord_equal(xlim = c(-1.25, 1.25), ylim = c(-1.25, 1.45), expand = FALSE) +
  theme_void()

p4 <- arrangeGrob(p_convex, p_norm, ncol = 2)
ggsave("../figure/intro_constraint_classes.pdf", p4, width = 9.6, height = 4.6)
