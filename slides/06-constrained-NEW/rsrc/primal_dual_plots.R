library(ggplot2)
library(grid)
library(gridExtra)

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10.8),
      axis.title = element_text(size = 12)
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
# Shared geometry: quadratic objective + circular feasible set
# -----------------------------------------------------------------------------

objective_center <- c(1.70, 0.28)
radius <- sqrt(sum(objective_center^2))
u <- objective_center / radius

objective_value <- function(x1, x2, center = objective_center) {
  0.5 * ((x1 - center[1])^2 + (x2 - center[2])^2)
}

circle_points <- function(r = 1, n = 500) {
  theta <- seq(0, 2 * pi, length.out = n)
  data.frame(
    x1 = r * cos(theta),
    x2 = r * sin(theta)
  )
}

grid_df <- expand.grid(
  x1 = seq(-1.20, 2.05, length.out = 280),
  x2 = seq(-1.15, 1.35, length.out = 240)
)
grid_df$z <- objective_value(grid_df$x1, grid_df$x2)

circle_df <- circle_points(1)
opt_boundary <- u

base_panel <- function(title, subtitle) {
  ggplot() +
    geom_contour(
      data = grid_df,
      aes(x = x1, y = x2, z = z),
      bins = 11,
      color = cols$contour,
      linewidth = 0.34
    ) +
    geom_polygon(
      data = circle_df,
      aes(x = x1, y = x2),
      fill = cols$feasible_fill,
      color = cols$feasible_line,
      linewidth = 1.15,
      alpha = 0.93
    ) +
    geom_point(
      data = data.frame(
        x1 = c(objective_center[1], opt_boundary[1]),
        x2 = c(objective_center[2], opt_boundary[2]),
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
      x = -0.30,
      y = 1.12,
      label = "feasible set",
      color = cols$feasible_line,
      size = 4.2,
      fontface = "bold"
    ) +
    annotate(
      "text",
      x = objective_center[1] - 0.05,
      y = objective_center[2] - 0.16,
      label = "unconstrained\nminimum",
      color = cols$unconstrained,
      size = 3.7
    ) +
    annotate(
      "text",
      x = opt_boundary[1] - 0.06,
      y = opt_boundary[2] + 0.18,
      label = "constrained optimum",
      color = cols$optimum,
      size = 3.8
    ) +
    coord_equal(xlim = c(-1.15, 1.95), ylim = c(-1.05, 1.25), expand = FALSE) +
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

penalty_alphas <- c(0.18, 0.55, 2.40)

penalty_radius <- function(alpha) {
  objective <- function(t) {
    0.5 * (t - radius)^2 + 0.5 * alpha * pmax(t^2 - 1, 0)^2
  }
  optimize(objective, interval = c(0, radius + 0.15))$minimum
}

penalty_pts <- data.frame(alpha = penalty_alphas)
penalty_pts$r <- vapply(penalty_alphas, penalty_radius, numeric(1))
penalty_pts$x1 <- penalty_pts$r * u[1]
penalty_pts$x2 <- penalty_pts$r * u[2]
penalty_pts$idx <- seq_len(nrow(penalty_pts))

penalty_path <- base_panel(
  title = "Quadratic penalty path",
  subtitle = expression(R(w, alpha) == F(w) + alpha / 2 %.% max(0, g(w))^2)
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
    x = penalty_pts$x1 + c(0.05, 0.05, 0.02),
    y = penalty_pts$x2 + c(-0.10, 0.10, 0.12),
    label = c(expression(alpha[1]), expression(alpha[2]), expression(alpha[3])),
    parse = TRUE,
    color = cols$penalty,
    size = 4.0
  ) +
  annotate(
    "text",
    x = 1.16,
    y = -0.58,
    label = expression(alpha %*% up),
    color = cols$penalty,
    size = 4.1,
    fontface = "bold"
  )

ggsave("../figure/primal_dual_penalty_path.pdf", penalty_path, width = 5.2, height = 4.7)

# -----------------------------------------------------------------------------
# Figure 2: barrier continuation approaches the boundary from inside
# -----------------------------------------------------------------------------

barrier_mus <- c(0.55, 0.22, 0.07)

barrier_radius <- function(mu) {
  objective <- function(t) {
    0.5 * (t - radius)^2 - mu * log(1 - t^2)
  }
  optimize(objective, interval = c(0, 0.999))$minimum
}

barrier_pts <- data.frame(mu = barrier_mus, idx = seq_along(barrier_mus))
barrier_pts$r <- vapply(barrier_mus, barrier_radius, numeric(1))
barrier_pts$x1 <- barrier_pts$r * u[1]
barrier_pts$x2 <- barrier_pts$r * u[2]

barrier_path <- base_panel(
  title = "Log-barrier path",
  subtitle = expression(B(w, mu) == F(w) - mu %.% log(delta^2 - w[1]^2 - w[2]^2))
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
    x = barrier_pts$x1 + c(-0.03, 0.03, 0.02),
    y = barrier_pts$x2 + c(0.18, 0.15, -0.11),
    label = c(expression(mu[1]), expression(mu[2]), expression(mu[3])),
    parse = TRUE,
    color = cols$barrier,
    size = 4.0
  ) +
  annotate(
    "text",
    x = 0.93,
    y = -0.56,
    label = expression(mu %*% downarrow),
    color = cols$barrier,
    size = 4.1,
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
    label = "alpha_i f_i(w) = mu",
    color = "grey20",
    size = 4.3,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 1.56,
    y = 0.16,
    label = "complementary slackness:\nalpha_i f_i(w) = 0",
    color = cols$optimum,
    size = 3.9
  ) +
  annotate(
    "segment",
    x = 1.05,
    y = 0.72,
    xend = 0.47,
    yend = 0.22,
    arrow = arrow(length = unit(0.16, "cm")),
    color = "grey25",
    linewidth = 0.8
  ) +
  coord_cartesian(xlim = c(0, 2.25), ylim = c(0, 2.35), expand = FALSE) +
  labs(
    title = "Central path in primal-dual interior-point methods",
    subtitle = "Each curve keeps slack positive while the barrier parameter decreases",
    x = expression("constraint slack " * f[i](w)),
    y = expression("estimated multiplier " * alpha[i])
  ) +
  theme(
    axis.line = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave("../figure/primal_dual_central_path.pdf", central_path, width = 6.3, height = 4.8)
