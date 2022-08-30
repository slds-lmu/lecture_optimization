library(ggplot2)

f = function(x, y) x^2 + y^2

x = seq(-1, 1, by = 0.01)
y = seq(-1, 1, by = 0.01)
z = outer(x, y, function(x, y) f(x, y))

nbcol = 20

nrz = nrow(z)
ncz = ncol(z)
# Color palette (100 colors)
col.pal<-colorRampPalette(c("darkblue", "blue", "green", "yellow"))
colors<-col.pal(nbcol)

# Compute the z-value at the facet centres
zfacet = z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol = cut(zfacet, nbcol)

png("figure_man/gradient2.png")
persp(x, y, z, col = colors[facetcol], phi = 30, theta = -30, border = NA, xlab = "x1", ylab = "x2", zlab = "y")
dev.off()

grid <- expand.grid(x = x, y = y)
grid$f = sapply(seq_len(nrow(grid)), function(i) f(grid[i, 1], grid[i, 2]))

p = ggplot() + stat_contour_filled(data = grid, aes(x = x, y = y, z = f)) 
# p = p + geom_point(data = data.frame(x = 0, y = 1), aes(x = x, y = y), colour = "orange")
p = p + geom_point(data = data.frame(x = sqrt(0.125), y = sqrt(0.125)), aes(x = x, y = y), colour = "red", size = 2)
p = p + geom_segment(data = data.frame(x = sqrt(0.125), y = sqrt(0.125), xend = 2 * sqrt(0.125), yend = 2 * sqrt(0.125)), aes(x = x, y = y, xend = xend, yend = yend), colour = "red", 
  arrow = arrow(length = unit(0.3,"cm")))
p = p + guides(fill = FALSE) + theme_void()

ggsave(filename = "figure_man/gradient1.png", p)

