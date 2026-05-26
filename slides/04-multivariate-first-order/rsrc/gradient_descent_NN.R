# Used in: slides-multivar-first-order-1-GD.tex,
#   slides-multivar-first-order-6-momentum.tex,
#   slides-multivar-first-order-9-sgd.tex
#
# Generates synthetic neural-network regression figures used to illustrate
# optimization behavior and the effect of mini-batch sizes / momentum.

set.seed(1234L)

library(data.table)
library(ggplot2)
library(mlr3)
library(plot3D)

source("NN_helpers.R")

objective_fun = function(x1, x2) x1^2 + x2^2

n_obs = 200L
training_data = data.table(
  x1 = runif(n_obs, -5, 5),
  x2 = runif(n_obs, -5, 5)
)
training_data[, y := objective_fun(x1, x2) + rnorm(.N, 0, 1)]

task = as_task_regr(as.data.frame(training_data), target = "y")

x1_scale = seq(-5, 5, by = 1)
x2_scale = seq(-5, 5, by = 1)
surface_values = outer(x1_scale, x2_scale, objective_fun)

# --- Helpers -----------------------------------------------------------------
save_surface_pdf = function(surface_matrix, filename) {
  pdf(filename, 5, 5, colormodel = "cmyk")
  persp3D(
    x = x1_scale,
    y = x2_scale,
    z = surface_matrix,
    xlab = "x1",
    ylab = "x2",
    zlab = "y",
    expand = 0.5,
    d = 2,
    phi = 10,
    theta = -60,
    resfac = 2,
    image = FALSE,
    colkey = list(side = 1, length = 0.5),
    shade = FALSE,
    alpha = 0.5
  )
  dev.off()
}

save_history_pdf = function(loss, filename, x_max = NULL, y_max = NULL) {
  df = data.frame(iteration = seq_along(loss), loss = loss)
  p = ggplot(df, aes(x = iteration, y = loss)) +
    geom_line() +
    labs(x = "Iteration", y = expression(R[emp])) +
    coord_cartesian(
      xlim = c(1, if (!is.null(x_max)) x_max else max(df$iteration)),
      ylim = c(0, if (!is.null(y_max)) y_max else max(df$loss))
    ) +
    theme_bw(base_size = 12)
  ggsave(filename, plot = p, width = 4, height = 3)
}

# --- Flags: set TRUE for each deck whose figures should be regenerated ------
# slides-multivar-first-order-1-GD.tex
run_gd = TRUE
# slides-multivar-first-order-6-momentum.tex
run_momentum = TRUE
# slides-multivar-first-order-9-sgd.tex
run_sgd = TRUE

# --- Initial figure: true surface + data (used by gd and momentum decks) ----
if (run_gd || run_momentum) {
  pdf("../figure/gradient_descent_NN_0.pdf", 5, 5, colormodel = "cmyk")
  persp3D(
    x = x1_scale,
    y = x2_scale,
    z = surface_values,
    xlab = "x1",
    ylab = "x2",
    zlab = "y",
    expand = 0.5,
    d = 2,
    phi = 10,
    theta = -60,
    resfac = 2,
    image = FALSE,
    colkey = list(side = 1, length = 0.5),
    shade = FALSE,
    alpha = 0.5
  )
  scatter3D(
    training_data$x1,
    training_data$x2,
    training_data$y,
    col = "red",
    size = 30,
    add = TRUE,
    type = "h",
    bty = "b2",
    pch = 16
  )
  dev.off()
}

# --- Surface snapshots and history for GD and momentum decks ----------------
# Each (momentum, epoch) pair is trained from the same seed for comparability.
# momentum=0   → surfaces + history used by gd; history_0.pdf used by momentum
# momentum=0.5 → surfaces + history used by momentum
epoch_snapshots = c(10L, 100L, 300L)

for (momentum_val in c(0, 0.5)) {
  needed = (momentum_val == 0 && (run_gd || run_momentum)) ||
    (momentum_val == 0.5 && run_momentum)
  if (!needed) {
    next
  }

  suffix = if (momentum_val == 0) "" else paste0("_", momentum_val)

  # Run all snapshots and collect results
  snapshot_results = lapply(epoch_snapshots, function(ep) {
    set_nn_seed(1111L)
    train_model(task, epochs = ep, x1_scale = x1_scale, x2_scale = x2_scale,
                momentum = momentum_val)
  })

  # Shared axis limits: x up to max epochs, y from 0 to highest initial loss
  x_max = max(epoch_snapshots)
  y_max = max(history_loss(snapshot_results[[length(epoch_snapshots)]]$history))

  for (i in seq_along(epoch_snapshots)) {
    ep     = epoch_snapshots[i]
    result = snapshot_results[[i]]
    loss   = history_loss(result$history)
    save_surface_pdf(result$surface_matrix,
                     sprintf("../figure/gradient_descent_NN_%d_surface%s.pdf", ep, suffix))
    save_history_pdf(loss,
                     sprintf("../figure/gradient_descent_NN_%d_history%s.pdf", ep, suffix),
                     x_max = x_max, y_max = y_max)
    if (ep == 300L && momentum_val == 0 && run_momentum)
      save_history_pdf(loss, "../figure/gradient_descent_NN_300_history_0.pdf",
                       x_max = x_max, y_max = y_max)
  }
}

# --- SGD batch fraction comparison (slides-multivar-first-order-9-sgd.tex) --
if (run_sgd) {
  set_nn_seed(1111L)
  epochs = 100L
  learning_rate = 0.001
  batch_fractions = c(1, 1 / 200, 0.1, 0.5)

  batch_fraction_data = rbindlist(lapply(batch_fractions, function(bf) {
    result = train_model(
      task,
      epochs = epochs,
      x1_scale = x1_scale,
      x2_scale = x2_scale,
      bs_fraction = bf,
      learning_rate = learning_rate
    )
    data.table(
      epoch = seq_len(epochs),
      loss = history_loss(result$history),
      batch_fraction = factor(bf)
    )
  }))

  sgd_plot = ggplot(batch_fraction_data, aes(x = epoch, y = loss, colour = batch_fraction)) +
    geom_line() +
    labs(x = "Epoch", y = expression(R[emp]), colour = "Batch fraction") +
    theme_bw(base_size = 12)

  ggsave(
    filename = "../figure/gradient_descent_NN_SGD_vs_no_SGD.pdf",
    plot = sgd_plot,
    width = 7,
    height = 4
  )
}
