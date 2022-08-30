library(ggplot2)

theme_set(theme_bw())

x = runif(20, 0, 5)
df = data.frame(x = x, y = x + rnorm(20))

p = ggplot() + geom_point(data = df, aes(x = x, y = y)) + geom_abline(intercept = 0, slope = 1, colour = "blue", lty = 2)
p
ggsave("figure_man/ml_linreg_example_1.pdf", p, width = 3, height = 2.5)


fun = function(theta) {
	sum((df$y - matrix(c(rep(1, nrow(df)), df$x), ncol = 2) %*% theta)^2)
}

grid <- expand.grid(theta0 = seq(-20, 20, by = 0.5), theta1 = seq(-20, 20, by = 0.5))
grid$l2_loss = apply(grid, 1, fun)

p = ggplot() + stat_contour_filled(data = grid, aes(x = theta0, y = theta1, z = l2_loss)) + xlab(expression(theta[0])) + ylab(expression(theta[1]))    
p = p + geom_point(data = data.frame(x = 0, y = 1), aes(x = x, y = y), colour = "orange")
p = p + guides(fill = "none")
ggsave("../figure_man/ml_linreg_example_2.pdf", p, width = 3, height = 2)

library(plotly)


fun = function(theta0, theta1) {
	yh = theta0 + theta1 * df$x
	sum((df$y - yh)^2)
}

fun_v = Vectorize(fun)
theta0 = seq(-20, 20, by = 0.5)
theta1 = seq(-20, 20, by = 0.5)
z = outer(theta0, theta1, fun_v)

fig <- plot_ly(x = theta0, y = theta1, z = z) %>% add_surface() %>%
  layout(scene = list(xaxis = list(title = "theta0"), yaxis = list(title = "theta1"), zaxis = list(title = "loss")))

fig <- fig %>% layout(
    scene = list(
      camera=list(
        eye = list(x=1.87, y=0.88, z=-0.64)
        )
      )
  )

fig = fig %>% layout(showlegend = FALSE)

fig
