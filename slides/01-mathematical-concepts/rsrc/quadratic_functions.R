# ------------------------------------------------------------------------------
# mathematical concepts

# FIG: quadratic function related plots (function, eigen-decomposition)
# ------------------------------------------------------------------------------

set.seed(1L)

library(ggplot2)
library(gridExtra)
library(BBmisc)

theme_set(theme_bw())

# ------------------------------------------------------------------------------

# Helper functions

compute_grid_data = function(xrange, yrange, fun) {
	z = outer(xrange, yrange, function(x, y) f(x, y))

	grid <- expand.grid(x = xrange, y = yrange)
	grid$f = sapply(seq_len(nrow(grid)), function(i) f(grid[i, 1], grid[i, 2]))

	nbcol = 20

	nrz = nrow(z)
	ncz = ncol(z)
	# Color palette (100 colors)
	col.pal<-colorRampPalette(c("blue", "green", "yellow"))
	colors<-col.pal(nbcol)

	# Compute the z-value at the facet centres
	zfacet = z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
	# Recode facet z-values into color indices
	facetcol = cut(zfacet, nbcol)

	return(list(grid = grid, z = z, color = colors[facetcol]))
}


plot_contour_data = function(f, grid) {
	p = ggplot() + stat_contour(data = grid, aes(x = x, y = y, z = f)) 
	# p = p + geom_point(data = data.frame(x = 0, y = 1), aes(x = x, y = y), colour = "orange")
	p = p + geom_point(data = data.frame(x = 0, y = 0), aes(x = x, y = y), colour = "blue", size = 3)
	p = p + xlab("x1") + ylab("x2")
	p = p + guides(fill = FALSE) + theme_minimal()

	return(p)
}

plot_eigenvector = function(p, v, col = "orange") {

	vnorm = norm(v, "2")
	p1 = p + geom_segment(aes(x = 0, y = 0, xend = 1 / vnorm * v[1], yend = 1 / vnorm * v[2]), arrow = arrow(length = unit(0.1, "inches")), colour = col)
	print(v[1] == 0)
	if (v[1] == 0) {
		p1 = p1 + geom_vline(xintercept = 0, lty = 3, alpha = 0.5, colour = col)		
	} else {
		p1 = p1 + geom_abline(slope = v[2] / v[1], lty = 3, alpha = 0.5, colour = col)		
	}

	return(p1)
}

plot_direction_along_eigenvector = function(grid, eigenval, col) {
	p2 = ggplot(data = data.frame(x = c(min(grid$x), max(grid$x))), aes(x = x)) + stat_function(fun = function(x) eigenval * x^2, colour = col) + xlab("x")
	p2
}



# Univariate examples 

## quadratic_functions_1D

p1 = ggplot(data.frame(x = c(-2, 2)), aes(x)) + stat_function(fun = function(x) x^2, colour = "orange") + ylim(c(0, 4))
p2 = ggplot(data.frame(x = c(-2, 2)), aes(x)) + stat_function(fun = function(x) - x^2, colour = "blue") + ylim(c(-4, 0)) 

combined_1d = arrangeGrob(p1, p2, nrow = 1)
ggsave("../figure/quadratic_functions_1D.png", combined_1d, width = 6, height = 3)



## quadratic_functions_1D_curvature

p3 = p1 + stat_function(fun = function(x) 2 * x^2, colour = "darkgreen") 
p4 = p2 + stat_function(fun = function(x) - 3 * x^2, colour = "violet") 

combined_1d_curvature = arrangeGrob(p3, p4, nrow = 1)
ggsave("../figure/quadratic_functions_1D_curvature.png", combined_1d_curvature, width = 6, height = 3)


## quadratic_functions_1D_derivative.png

df = data.frame(x = c(-2, -1, 0, 2), y = c(4, 1, 0, 4), slope = c(-4, -2, 0, 4))
df$intercept = df$y - df$slope * df$x

plist = lapply(seq_row(df), function(i) {
	p1 = ggplot(data.frame(x = c(-3, 3)), aes(x)) + stat_function(fun = function(x) x^2, colour = "orange") 
	p1 = p1 + geom_point(data = df[i, ], aes(x = x, y = y), size = 2)
	p1 = p1 + geom_abline(data = df[i, ], aes(intercept = intercept, slope = slope))
})

combined_1d_derivative = do.call(arrangeGrob, c(plist, nrow = 1))
ggsave("../figure/quadratic_functions_1D_derivative.png", combined_1d_derivative, width = 12, height = 3)





# Multidimensional Example (I)

f = function(x, y) 2 * x^2 - 2 * x * y + 2 * y^2

x = seq(-3, 3, by = 0.01)
y = seq(-3, 3, by = 0.01)

griddata = compute_grid_data(x, y, f)
grid = griddata$grid
colors = griddata$color
z = griddata$z

png("../figure/quadratic_functions_2D_example_1_1.png")
persp(x, y, z, col = colors, phi = 30, theta = -20, border = NA, xlab = "x1", ylab = "x2", zlab = "y")
dev.off()

p = plot_contour_data(f, grid)
ggsave("../figure/quadratic_functions_2D_example_1_2.png", p, width = 3, height = 3)

# Multidimensional Example (II)

f = function(x, y) (-1) * x^2 - 2 * x * y + y^2

x = seq(-3, 3, by = 0.05)
y = seq(-3, 3, by = 0.05)

griddata = compute_grid_data(x, y, f)
grid = griddata$grid

v1 = c(1 - sqrt(2), 1); e1 = - sqrt(2)
v2 = c(1 + sqrt(2), 1); e2 = lambda2 = 2 * sqrt(2)


p1 = ggplot(data = grid, aes(x = x, y = y, z = f)) 
p1 = p1 + geom_raster(aes(fill = f)) + geom_contour(colour = "white") + xlab("x1") + ylab("x2")
p1 = plot_eigenvector(p1, v1, "orange") + coord_fixed() + guides(fill = FALSE)
p1 = plot_eigenvector(p1, v2, "magenta")

combined_2d_example = arrangeGrob(p1, nrow = 1)
ggsave("../figure/quadratic_functions_2D_example_2_4.png", combined_2d_example, width = 4, height = 3)



f = function(x, y) 5 * x^2 - 2 * x * y + 1 * y^2

x = seq(-3, 3, by = 0.01)
y = seq(-3, 3, by = 0.01)
z = outer(x, y, function(x, y) f(x, y))

nbcol = 20

nrz = nrow(z)
ncz = ncol(z)
# Color palette (100 colors)
col.pal<-colorRampPalette(c("blue", "green", "yellow"))
colors<-col.pal(nbcol)

# Compute the z-value at the facet centres
zfacet = z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol = cut(zfacet, nbcol)



