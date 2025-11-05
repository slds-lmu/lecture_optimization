# ------------------------------------------------------------------------------
# mathematical concepts

# FIG: quadratic function related plots (function, eigen-decomposition)
# ------------------------------------------------------------------------------

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

ggsave("../figure_man/quadratic_functions_1D.png", grid.arrange(p1, p2, nrow = 1), width = 6, height = 3)



## quadratic_functions_1D_curvature

p3 = p1 + stat_function(fun = function(x) 2 * x^2, colour = "darkgreen") 
p4 = p2 + stat_function(fun = function(x) - 3 * x^2, colour = "violet") 

ggsave("../figure_man/quadratic_functions_1D_curvature.png", grid.arrange(p3, p4, nrow = 1), width = 6, height = 3)


## quadratic_functions_1D_derivative.png

df = data.frame(x = c(-2, -1, 0, 2), y = c(4, 1, 0, 4), slope = c(-4, -2, 0, 4))
df$intercept = df$y - df$slope * df$x

plist = lapply(seq_row(df), function(i) {
	p1 = ggplot(data.frame(x = c(-3, 3)), aes(x)) + stat_function(fun = function(x) x^2, colour = "orange") 
	p1 = p1 + geom_point(data = df[i, ], aes(x = x, y = y), size = 2)
	p1 = p1 + geom_abline(data = df[i, ], aes(intercept = intercept, slope = slope))
})

ggsave("../figure_man/quadratic_functions_1D_derivative.png", do.call(grid.arrange, c(plist, nrow = 1)), width = 12, height = 3)





# Multivariate example: Diagonal matrix  


f = function(x, y) 2 * x^2 + y^2

grid = compute_grid_data(seq(-3, 3, by = 0.01), seq(-3, 3, by = 0.01), f)$grid

## quadratic_functions_2D_diag_1.png
p = plot_contour_data(f, grid)
ggsave(filename = "../figure_man/quadratic_functions_2D_example_diag_1.png", p, width = 3, height = 3)


v1 = c(1, 0); e1 = 2
v2 = c(0, 1); e2 = 1

# quadratic_functions_2D_diag_2.png
p1 = plot_eigenvector(p, v1, "orange") + coord_fixed()
p2 = plot_direction_along_eigenvector(grid, e1, "orange") + ylim(c(0, 8))

ggsave("../figure_man/quadratic_functions_2D_example_diag_2.png", grid.arrange(p1, p2, nrow = 1), width = 8, height = 3)

# quadratic_functions_2D_diag_3.png
p1 = plot_eigenvector(p1, v2, "magenta")
p2 = plot_direction_along_eigenvector(grid, e2, "magenta") + ylim(c(0, 8))

ggsave("../figure_man/quadratic_functions_2D_example_diag_3.png", grid.arrange(p1, p2, nrow = 1), width = 8, height = 3)




# Multidimensional Example (I)

f = function(x, y) 2 * x^2 - 2 * x * y + 2 * y^2

x = seq(-3, 3, by = 0.01)
y = seq(-3, 3, by = 0.01)

griddata = compute_grid_data(x, y, f)
grid = griddata$grid
colors = griddata$color
z = griddata$z

png("../figure_man/quadratic_functions_2D_example_1_1.png")
persp(x, y, z, col = colors, phi = 30, theta = -20, border = NA, xlab = "x1", ylab = "x2", zlab = "y")
dev.off()

p = plot_contour_data(f, grid)
ggsave("../figure_man/quadratic_functions_2D_example_1_2.png", p, width = 3, height = 3)


v1 = c(1, 0); e1 = 2.5
v2 = c(0, 1); e2 = 2.5

# quadratic_functions_2D_diag_2.png
p1 = plot_eigenvector(p, v1, "black") + coord_fixed()
p2 = plot_direction_along_eigenvector(grid, e1, "black") + ylim(c(0, 8))

ggsave("../figure_man/quadratic_functions_2D_example_1_3.png", grid.arrange(p1, p2, nrow = 1), width = 8, height = 3)

# quadratic_functions_2D_diag_3.png
p1 = plot_eigenvector(p1, v2, "black")
p2 = plot_direction_along_eigenvector(grid, e2, "black") + ylim(c(0, 8))

ggsave("../figure_man/quadratic_functions_2D_example_1_4.png", grid.arrange(p1, p2, nrow = 1), width = 8, height = 3)


v1 = c(1, 1); e1 = 2
v2 = c(-1, 1); e2 = 3

# quadratic_functions_2D_diag_2.png
p1 = plot_eigenvector(p, v1, "orange") + coord_fixed()
p2 = plot_direction_along_eigenvector(grid, e1, "orange") + ylim(c(0, 8))

ggsave("../figure_man/quadratic_functions_2D_example_1_5.png", grid.arrange(p1, p2, nrow = 1), width = 8, height = 3)

# quadratic_functions_2D_diag_3.png
p1 = plot_eigenvector(p1, v2, "magenta")
p2 = plot_direction_along_eigenvector(grid, e2, "magenta") + ylim(c(0, 8))

ggsave("../figure_man/quadratic_functions_2D_example_1_6.png", grid.arrange(p1, p2, nrow = 1), width = 8, height = 3)

ggsave("../figure_man/quadratic_functions_2D_example_1_7.png", p1, width = 4, height = 3)





# Multidimensional Example (II)

f = function(x, y) (-1) * x^2 - 2 * x * y + y^2

x = seq(-3, 3, by = 0.05)
y = seq(-3, 3, by = 0.05)

griddata = compute_grid_data(x, y, f)
grid = griddata$grid
colors = griddata$color
z = griddata$z

png("../figure_man/quadratic_functions_2D_example_2_1.png")
persp(x, y, z, col = colors, phi = 30, theta = -20, border = NA, xlab = "x1", ylab = "x2", zlab = "y")
dev.off()

p = ggplot() + xlim(c(-3, 3)) + ylim(c(-3, 3))

v1 = c(1 - sqrt(2), 1); e1 = - sqrt(2)
v2 = c(1 + sqrt(2), 1); e2 = lambda2 = 2 * sqrt(2)

# quadratic_functions_2D_diag_2.png
p1 = plot_eigenvector(p, v1, "orange") + coord_fixed()
p2 = plot_direction_along_eigenvector(grid, e1, "orange") + ylim(c(-8, 8))

ggsave("../figure_man/quadratic_functions_2D_example_2_2.png", p1, width = 4, height = 3)

# quadratic_functions_2D_diag_3.png
p1 = plot_eigenvector(p1, v2, "magenta")
p2 = plot_direction_along_eigenvector(grid, e2, "magenta") + ylim(c(-8, 8))

ggsave("../figure_man/quadratic_functions_2D_example_2_3.png", p1, width = 4, height = 3)


p1 = ggplot(data = grid, aes(x = x, y = y, z = f)) 
p1 = p1 + geom_raster(aes(fill = f)) + geom_contour(colour = "white") + xlab("x1") + ylab("x2")
p1 = plot_eigenvector(p1, v1, "orange") + coord_fixed() + guides(fill = FALSE)
p1 = plot_eigenvector(p1, v2, "magenta")

ggsave("../figure_man/quadratic_functions_2D_example_2_4.png", grid.arrange(p1, nrow = 1), width = 4, height = 3)



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

png("../figure_man/quadratic_functions_2D_bad_cond_1.png")
persp(x, y, z, col = colors[facetcol], phi = 30, theta = -20, border = NA, xlab = "x1", ylab = "x2", zlab = "y")
dev.off()

grid <- expand.grid(x = x, y = y)
grid$f = sapply(seq_len(nrow(grid)), function(i) f(grid[i, 1], grid[i, 2]))

p = ggplot() + stat_contour(data = grid, aes(x = x, y = y, z = f)) 
# p = p + geom_point(data = data.frame(x = 0, y = 1), aes(x = x, y = y), colour = "orange")
p = p + geom_point(data = data.frame(x = 0, y = 0), aes(x = x, y = y), colour = "blue", size = 3)
p = p + xlab("x1") + ylab("x2")
p = p + guides(fill = FALSE) + theme_minimal()

ggsave("../figure_man/quadratic_functions_2D_bad_cond_2.png", p, width = 3, height = 3)




