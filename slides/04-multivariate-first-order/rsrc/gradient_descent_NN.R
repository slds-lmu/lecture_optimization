# Used in: slides-multivar-first-order-1-GD.tex,
#   slides-multivar-first-order-6-momentum.tex,
#   slides-multivar-first-order-9-sgd.tex
#
# Generates the synthetic neural-network regression figures used to illustrate
# optimization behavior and the effect of mini-batch sizes.

set.seed(1234L)

library(data.table)
library(ggplot2)
library(mlr3)
library(plot3D)

source("NN_helpers.R")

objective_fun = function(x1, x2) {
  x1^2 + x2^2
}

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

pdf("../figure/gradient_descent_NN_0.pdf", 5, 5, colormodel = "cmyk")
par(mfrow = c(1, 1))

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

set_nn_seed(1111L)
momentum_histories = lapply(c(0, 0.5), function(momentum) {
  history = train_model(task, epochs = 300L, momentum = momentum)

  data.table(
    epoch = seq_along(history_loss(history)),
    loss = history_loss(history),
    momentum = factor(momentum)
  )
})

momentum_data = rbindlist(momentum_histories)
momentum_plot = ggplot(momentum_data, aes(x = epoch, y = loss, colour = momentum)) +
  geom_line() +
  coord_cartesian(ylim = c(0, 130)) +
  labs(colour = "Momentum") +
  theme_bw(base_size = 12)

if (interactive()) {
  print(momentum_plot)
}

set_nn_seed(1111L)
epochs = 100L
learning_rate = 0.001
batch_fractions = c(1, 1 / 200, 0.1, 0.5)

batch_fraction_histories = lapply(batch_fractions, function(batch_fraction) {
  history = train_model(
    task,
    epochs = epochs,
    bs_fraction = batch_fraction,
    learning_rate = learning_rate
  )

  data.table(
    epoch = seq_len(epochs),
    loss = history_loss(history),
    batch_fraction = factor(batch_fraction)
  )
})

batch_fraction_data = rbindlist(batch_fraction_histories)

sgd_plot = ggplot(batch_fraction_data, aes(x = epoch, y = loss, colour = batch_fraction)) +
  geom_line() +
  labs(
    x = "Epoch",
    y = "MSE",
    colour = "Batch fraction",
    title = "SGD with different batch sizes"
  ) +
  theme_bw(base_size = 12)

if (interactive()) {
  print(sgd_plot)
}

ggsave(
  filename = "../figure/gradient_descent_NN_SGD_vs_no_SGD.pdf",
  plot = sgd_plot,
  width = 7,
  height = 4
)
