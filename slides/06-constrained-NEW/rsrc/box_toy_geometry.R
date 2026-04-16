box_toy_Q <- matrix(c(2, 1, 1, 2), nrow = 2)
box_toy_center <- c(1.2, 0.2)
box_toy_bounds <- list(
  w1 = c(0, 1),
  w2 = c(0, 0.9)
)
box_toy_optimum <- c(1, 0.3)

box_toy_value <- function(x1, x2, center = box_toy_center, Q = box_toy_Q) {
  dx1 <- x1 - center[1]
  dx2 <- x2 - center[2]
  0.5 * (Q[1, 1] * dx1^2 + 2 * Q[1, 2] * dx1 * dx2 + Q[2, 2] * dx2^2)
}

box_toy_grad <- function(x, center = box_toy_center, Q = box_toy_Q) {
  drop(Q %*% (x - center))
}

box_toy_project <- function(z, bounds = box_toy_bounds) {
  c(
    pmin(pmax(z[1], bounds$w1[1]), bounds$w1[2]),
    pmin(pmax(z[2], bounds$w2[1]), bounds$w2[2])
  )
}

box_toy_coord_update_x1 <- function(w2, bounds = box_toy_bounds) {
  unclipped <- 1.3 - 0.5 * w2
  pmin(pmax(unclipped, bounds$w1[1]), bounds$w1[2])
}

box_toy_coord_update_x2 <- function(w1, bounds = box_toy_bounds) {
  unclipped <- 0.8 - 0.5 * w1
  pmin(pmax(unclipped, bounds$w2[1]), bounds$w2[2])
}

box_toy_grid <- function(
    x1_lim = c(-0.05, 1.35),
    x2_lim = c(-0.05, 0.98),
    n1 = 220,
    n2 = 220) {
  df <- expand.grid(
    x1 = seq(x1_lim[1], x1_lim[2], length.out = n1),
    x2 = seq(x2_lim[1], x2_lim[2], length.out = n2)
  )
  df$z <- box_toy_value(df$x1, df$x2)
  df
}
