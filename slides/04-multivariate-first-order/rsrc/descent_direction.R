library(ggplot2)
library(gridExtra)
library(BBmisc)

theme_set(theme_bw())

# Helper functions

compute_grid_data = function(xrange, yrange, fun) {

	grid <- expand.grid(x = xrange, y = yrange)
	grid$f = sapply(seq_len(nrow(grid)), function(i) f(grid[i, 1], grid[i, 2]))

	return(grid = grid)
}


plot_contour_data = function(f, grid) {
	p = ggplot() + geom_contour_filled(data = grid, aes(x = x, y = y, z = f)) 
	# p = p + geom_point(data = data.frame(x = 0, y = 1), aes(x = x, y = y), colour = "orange")
	p = p + geom_point(data = data.frame(x = 0, y = 0), aes(x = x, y = y), colour = "green", size = 3)
	p = p + xlab("x1") + ylab("x2")
	p = p + guides(fill = FALSE) + theme_minimal()

	return(p)
}


# Multivariate example: Diagonal matrix  

f = function(x, y) 2 * x^2 + y^2

grid = compute_grid_data(seq(-2, 2, by = 0.05), seq(-2, 2, by = 0.05), f)

## quadratic_functions_2D_diag_1.png

x = c(1, 1)
grad = c(4 * x[1], 2 * x[2])
grad = - grad / sqrt(sum(grad^2))

grad_orth = c(1, - grad[1] / grad[2])
grad_orth = grad_orth / sqrt(sum(grad_orth^2))

d = c(0, -1)

p = plot_contour_data(f, grid)
p = p + geom_segment(aes(x = x[1], y = x[2], xend = x[1] + d[1], yend = x[2] + d[2]), arrow = arrow(length = unit(0.1, "inches")), colour = "orange")
temp <- paste("d")
p = p + annotate("text", x = x[1] - 0.3, y = x[2] - 0.4, colour = "orange", label = temp, parse = TRUE, size = 3)
p = p + geom_segment(aes(x = x[1], y = x[2], xend = x[1] - grad[1], yend = x[2] - grad[2]), arrow = arrow(length = unit(0.1, "inches")), colour = "red")
temp <- paste("nabla * f(x)")
p = p + annotate("text", x = x[1] + 0.3, y = x[2] + 0.4, colour = "red", label = temp, parse = TRUE, size = 3)

p = p + geom_segment(aes(x = x[1], y = x[2], xend = x[1] - grad_orth[1], yend = x[2] - grad_orth[2]), colour = "red", lty = "dashed")
p = p + geom_segment(aes(x = x[1], y = x[2], xend = x[1] + grad_orth[1], yend = x[2] + grad_orth[2]), colour = "red", lty = "dashed")
temp <- paste("90")
p = p + annotate("text", x = x[1] + 0.6, y = x[2] - 0.8, colour = "red", label = temp, parse = TRUE, size = 2)
temp <- paste("270")
p = p + annotate("text", x = x[1] - 0.2, y = x[2] + 0.8, colour = "red", label = temp, parse = TRUE, size = 2)

ggsave(filename = "figure_man/descent_direction.pdf", p, width = 3, height = 3)

