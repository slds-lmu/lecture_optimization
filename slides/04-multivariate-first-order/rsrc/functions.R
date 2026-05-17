# Used in: rsrc/armijo.R, rsrc/saddle_point_grad_norm.R
#
# Provides small numerical helpers for the first-order optimization slides.

set.seed(1L)

library(colorspace)

grad = function(..., FUN, eps = .Machine$double.eps^0.5, type = c("centered", "forward")) {
  type = match.arg(type)

  grad_component = switch(
    type,
    forward = function(vars, j) {
      vars_forward = vars
      vars_forward[[j]] = vars_forward[[j]] + eps

      (do.call(FUN, vars_forward) - do.call(FUN, vars)) / eps
    },
    centered = function(vars, j) {
      vars_forward = vars
      vars_backward = vars
      vars_forward[[j]] = vars_forward[[j]] + eps
      vars_backward[[j]] = vars_backward[[j]] - eps

      (do.call(FUN, vars_forward) - do.call(FUN, vars_backward)) / (2 * eps)
    }
  )

  vars = list(...)
  gradient = NULL
  for (j in seq_len(length(vars))) {
    gradient = cbind(gradient, grad_component(vars, j))
  }

  if (length(vars) < 2L) {
    drop(gradient)
  } else {
    colnames(gradient) = names(vars)
    gradient
  }
}

persp2 = function(x, y, z, col = terrain_hcl, ...) {
  n_cols = ncol(z)
  n_rows = nrow(z)

  if (is.function(col)) {
    col = col(n_cols * n_rows)
  }

  facet_values = z[-1, -1] + z[-1, -n_cols] + z[-n_rows, -1] + z[-n_rows, -n_cols]
  facet_colours = cut(facet_values, length(col))

  persp(x, y, z, col = col[facet_colours], ...)
}

optim0 = function(..., FUN, tol = 1e-8, maxit = 100L, maximum = TRUE, alpha = 1) {
  optimization_path = list()
  current_point = list(...)
  iter = 1L
  keep_going = TRUE

  while (keep_going && iter < maxit) {
    gradient = as.numeric(-do.call(grad, c(current_point, FUN = FUN)) * if (maximum) -1 else 1)
    next_point = current_point

    for (j in seq_along(current_point)) {
      next_point[[j]] = current_point[[j]] + alpha * gradient[j]
    }

    current_point = next_point
    keep_going = !isTRUE(all.equal(gradient, rep(0, length(gradient)), tol = tol))
    optimization_path[[iter]] = current_point
    iter = iter + 1L
  }

  optimization_path
}

sd_plot = function(col = terrain_hcl, theta = 40, phi = 40, xlab = "x", ylab = "y") {
  if (is.function(col)) {
    col = col(nrow(z) * ncol(z))
  }

  par(mfrow = c(1, 2))
  par(mar = rep(0, 4))

  projection = persp2(
    x,
    y,
    z,
    theta = theta,
    phi = phi,
    ticktype = "detailed",
    xlab = xlab,
    ylab = ylab,
    zlab = "",
    col = col,
    lwd = 0.5
  )

  for (j in seq_along(p)) {
    point_3d = trans3d(p[[j]][[1]], p[[j]][[2]], do.call(foo, p[[j]]), projection)

    if (j > 1L) {
      previous_point_3d = trans3d(p[[j - 1L]][[1]], p[[j - 1L]][[2]], do.call(foo, p[[j - 1L]]), projection)
      lines(c(point_3d$x, previous_point_3d$x), c(point_3d$y, previous_point_3d$y))
      points(x = previous_point_3d$x, y = previous_point_3d$y, pch = 16, col = heat_hcl(1L))
    }

    points(x = point_3d$x, y = point_3d$y, pch = 16, col = heat_hcl(1L))
  }

  par(mar = c(4.1, 4.1, 1.1, 1.1))
  image(x, y, z, col = col, xlab = xlab, ylab = ylab)
  contour(x, y, z, add = TRUE)

  for (j in seq_along(p)) {
    if (j > 1L) {
      lines(c(p[[j]][1], p[[j - 1L]][1]), c(p[[j]][2], p[[j - 1L]][2]))
      points(p[[j - 1L]][1], p[[j - 1L]][2], pch = 16, col = heat_hcl(1L))
    }

    points(p[[j]][1], p[[j]][2], pch = 16, col = heat_hcl(1L))
  }

  invisible(NULL)
}
