library(ggplot2)
library(grid)
library(gridExtra)
source("box_toy_geometry.R")

theme_set(
  theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 13)
    )
)

cols <- list(
  contour = "#AEB9C4",
  feasible_fill = "#DCEAF6",
  feasible_line = "#2C7FB8",
  penalty = "#D95F02",
  barrier = "#1B9E77",
  point = "#11324D",
  optimum = "#B14D6C",
  unconstrained = "#7B3294"
)

# -----------------------------------------------------------------------------
# Shared geometry: recurring box-constrained quadratic
# -----------------------------------------------------------------------------

grid_df <- box_toy_grid(
  x1_lim = c(-0.02, 1.30),
  x2_lim = c(-0.02, 0.98),
  n1 = 260,
  n2 = 220
)

base_panel <- function(title, subtitle) {
  ggplot() +
    geom_contour(
      data = grid_df,
      aes(x = x1, y = x2, z = z),
      bins = 11,
      color = cols$contour,
      linewidth = 0.34
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
      linewidth = 1.15,
      alpha = 0.93
    ) +
    geom_point(
      data = data.frame(
        x1 = c(box_toy_center[1], box_toy_optimum[1]),
        x2 = c(box_toy_center[2], box_toy_optimum[2]),
        kind = c("unconstrained", "constrained")
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
        unconstrained = cols$unconstrained,
        constrained = cols$optimum
      )
    ) +
  annotate(
    "text",
    x = 0.48,
    y = 0.98,
    label = "feasible box",
    color = cols$feasible_line,
    size = 4.5,
    fontface = "bold"
  ) +
    annotate(
      "text",
    x = box_toy_center[1] + 0.02,
    y = box_toy_center[2] - 0.13,
    label = "unconstrained\nminimum",
    color = cols$unconstrained,
    size = 4.0
  ) +
    annotate(
      "text",
    x = box_toy_optimum[1] - 0.08,
    y = box_toy_optimum[2] - 0.14,
    label = "constrained optimum",
    color = cols$optimum,
    size = 4.1
  ) +
    coord_equal(xlim = c(-0.02, 1.30), ylim = c(-0.02, 0.98), expand = FALSE) +
    labs(
      title = title,
      subtitle = subtitle,
      x = expression(w[1]),
      y = expression(w[2])
    ) +
    theme(
      legend.position = "none",
      axis.line = element_line(color = "grey40")
    )
}

# -----------------------------------------------------------------------------
# Figure 1: penalty continuation approaches the boundary from outside
# -----------------------------------------------------------------------------

penalty_rhos <- c(0.35, 1.20, 4.50)

penalty_pts <- data.frame(rho = penalty_rhos)
penalty_pts$x1 <- 1 + 0.6 / (3 + 2 * penalty_pts$rho)
penalty_pts$x2 <- 0.3 - 0.3 / (3 + 2 * penalty_pts$rho)
penalty_pts$idx <- seq_len(nrow(penalty_pts))

penalty_path <- base_panel(
  title = "Quadratic penalty path",
  subtitle = "larger rho reduces the infeasible overshoot"
) +
  geom_path(
    data = penalty_pts,
    aes(x = x1, y = x2),
    color = cols$penalty,
    linewidth = 1.1,
    arrow = arrow(length = unit(0.17, "cm"))
  ) +
  geom_point(
    data = penalty_pts,
    aes(x = x1, y = x2),
    fill = cols$penalty,
    color = "white",
    shape = 21,
    size = 3.0,
    stroke = 0.35
  ) +
  annotate(
    "text",
    x = penalty_pts$x1 + c(0.02, 0.02, 0.02),
    y = penalty_pts$x2 + c(-0.08, 0.07, 0.10),
    label = sprintf("rho = %.2f", penalty_pts$rho),
    color = cols$penalty,
    size = 3.8
  ) +
  annotate(
    "text",
    x = 1.10,
    y = 0.08,
    label = "outside -> boundary",
    color = cols$penalty,
    size = 4.0,
    fontface = "bold"
  )

ggsave("../figure/primal_dual_penalty_path.pdf", penalty_path, width = 5.2, height = 4.7)

# -----------------------------------------------------------------------------
# Figure 2: barrier continuation approaches the boundary from inside
# -----------------------------------------------------------------------------

barrier_mus <- c(0.16, 0.06, 0.02)

barrier_objective <- function(w, mu) {
  if (w[1] <= 0 || w[1] >= 1 || w[2] <= 0 || w[2] >= 0.9) {
    return(Inf)
  }
  box_toy_value(w[1], w[2]) -
    mu * (log(w[1]) + log(1 - w[1]) + log(w[2]) + log(0.9 - w[2]))
}

barrier_pts <- data.frame(mu = barrier_mus, idx = seq_along(barrier_mus))
start <- c(0.52, 0.44)
for (i in seq_along(barrier_mus)) {
  fit <- optim(
    par = start,
    fn = barrier_objective,
    mu = barrier_mus[i],
    method = "L-BFGS-B",
    lower = c(1e-4, 1e-4),
    upper = c(1 - 1e-4, 0.9 - 1e-4)
  )
  barrier_pts$x1[i] <- fit$par[1]
  barrier_pts$x2[i] <- fit$par[2]
  start <- fit$par
}

barrier_path <- base_panel(
  title = "Log-barrier path",
  subtitle = "smaller mu moves the iterate toward the active wall from inside"
) +
  geom_path(
    data = barrier_pts,
    aes(x = x1, y = x2),
    color = cols$barrier,
    linewidth = 1.1,
    arrow = arrow(length = unit(0.17, "cm"))
  ) +
  geom_point(
    data = barrier_pts,
    aes(x = x1, y = x2),
    fill = cols$barrier,
    color = "white",
    shape = 21,
    size = 3.0,
    stroke = 0.35
  ) +
  annotate(
    "text",
    x = barrier_pts$x1 + c(-0.02, 0.03, 0.03),
    y = barrier_pts$x2 + c(0.14, 0.11, -0.10),
    label = sprintf("mu = %.2f", barrier_pts$mu),
    color = cols$barrier,
    size = 3.8
  ) +
  annotate(
    "text",
    x = 0.78,
    y = 0.08,
    label = "interior -> boundary",
    color = cols$barrier,
    size = 4.0,
    fontface = "bold"
  )

ggsave("../figure/primal_dual_barrier_path.pdf", barrier_path, width = 5.2, height = 4.7)

combined_path <- arrangeGrob(penalty_path, barrier_path, ncol = 2)
ggsave("../figure/primal_dual_penalty_barrier.pdf", combined_path, width = 10.2, height = 4.8)

# -----------------------------------------------------------------------------
# Figure 3: central path and perturbed complementary slackness
# -----------------------------------------------------------------------------

mus <- c(0.60, 0.25, 0.08)
slack_grid <- seq(0.05, 2.2, length.out = 300)

central_df <- do.call(
  rbind,
  lapply(seq_along(mus), function(i) {
    data.frame(
      slack = slack_grid,
      lambda = mus[i] / slack_grid,
      mu = factor(sprintf("mu = %.2f", mus[i]), levels = sprintf("mu = %.2f", mus))
    )
  })
)

mu_cols <- c("#2C7FB8", "#41AE76", "#D95F02")
names(mu_cols) <- levels(central_df$mu)

rep_points <- data.frame(
  slack = c(1.35, 0.85, 0.35),
  mu = factor(sprintf("mu = %.2f", mus), levels = levels(central_df$mu))
)
rep_points$lambda <- mus / rep_points$slack

central_path <- ggplot(central_df, aes(x = slack, y = lambda, color = mu)) +
  geom_path(linewidth = 1.2) +
  geom_point(
    data = rep_points,
    aes(x = slack, y = lambda, fill = mu),
    shape = 21,
    size = 3.1,
    color = "white",
    stroke = 0.35,
    show.legend = FALSE
  ) +
  annotate("segment", x = 0, y = 2.25, xend = 0, yend = 0, linewidth = 0.7, color = "grey40") +
  annotate("segment", x = 0, y = 0, xend = 2.25, yend = 0, linewidth = 0.7, color = "grey40") +
  scale_color_manual(values = mu_cols) +
  scale_fill_manual(values = mu_cols) +
  annotate(
    "text",
    x = 1.72,
    y = 1.95,
    label = "lambda_i s_i = mu",
    color = "grey20",
    size = 4.7,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 0.46,
    y = 2.08,
    label = "mu decreases",
    color = cols$optimum,
    size = 4.0,
    fontface = "bold"
  ) +
  coord_cartesian(xlim = c(0, 2.25), ylim = c(0, 2.35), expand = FALSE) +
  labs(
    title = "Central path",
    subtitle = "positive slack and positive multiplier while the barrier parameter decreases",
    x = expression("slack " * s[i]),
    y = expression("multiplier " * lambda[i])
  ) +
  theme(
    axis.line = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave("../figure/primal_dual_central_path.pdf", central_path, width = 6.4, height = 4.9)
