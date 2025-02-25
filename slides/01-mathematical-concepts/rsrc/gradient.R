# ------------------------------------------------------------------------------
# mathematical concepts

# FIG: plot gradients
# ------------------------------------------------------------------------------

library(ggplot2)
library(plotly)
library(pracma)
library(ggquiver)
library(dplyr)

# ------------------------------------------------------------------------------

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

png("../figure_man/gradient5.png")
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

p
ggsave(filename = "../figure_man/gradient1.png", p, height=4, width=4)


# Define function f(x, y)
f <- function(x, y) (0.5 * x^2 + y^2 + x * y)

# Define gradient ∇f(x, y)
grad_f <- function(x, y) {
  grad_x <- 1 * x + y
  grad_y <- 2 * y + x
  return(c(grad_x, grad_y))
}

# Define grid
xmin <- -2.5
xmax <- 2.5
xv <- seq(xmin, xmax, length.out = 100)
yv <- seq(xmin, xmax, length.out = 100)
grid <- expand.grid(x = xv, y = yv)
grid$z <- with(grid, f(x, y))

# Define gradient at a specific point (xp, yp)
xp <- 0.5
yp <- 0.5
grad_values <- grad_f(xp, yp)

# 2D Contour Plot with Gradient Arrows
p <- ggplot(grid, aes(x = x, y = y, z = z)) +
  geom_contour_filled(aes(z = z, fill = after_stat(level)), bins = 8) +  # Contour shading
  
  # Main gradient vector at (xp, yp)
  geom_segment(aes(x = xp, y = yp, xend = xp + grad_values[1], yend = yp + grad_values[2]), 
               arrow = arrow(length = unit(0.15, "inches")), color = "red") +
  
  # Partial derivative arrows
  geom_segment(aes(x = xp, y = yp, xend = xp, yend = yp + grad_values[2]), 
               arrow = arrow(length = unit(0.1, "inches")), color = "red", alpha = 0.5) +
  geom_segment(aes(x = xp, y = yp, xend = xp + grad_values[1], yend = yp), 
               arrow = arrow(length = unit(0.1, "inches")), color = "red", alpha = 0.5) +
  
  # Labels for arrows (Fixed using expression())
  annotate("text", x = 1.0, y = 0.9, label = expression(nabla * f(x[1], x[2])), color = "red", size = 6) +
  annotate("text", x = 1.15, y = 0.1, label = expression(frac(partialdiff * f(x[1], x[2]), partialdiff * x[1])), color = "red", size = 5) +
  annotate("text", x = -0.4, y = 1.8, label = expression(frac(partialdiff * f(x[1], x[2]), partialdiff * x[2])), color = "red", size = 5) +
  
  # Formatting
  theme_minimal() +
  labs(x = "x1", y = "x2") +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  coord_fixed()

ggsave(filename = "../figure_man/gradient_unit_vectors.png", p)

# 3D Surface Plot
fig <- plot_ly(
  x = xv, y = yv, z = outer(xv, yv, f),
  type = "surface",
  colorscale = "Viridis"
)

fig <- layout(
  fig,
  scene = list(
    xaxis = list(title = "x₁", range = c(-2.5, 2.5)),
    yaxis = list(title = "x₂", range = c(-2.5, 2.5)), 
    zaxis = list(title = "f(x₁, x₂)", range = c(0, 15)), 
    aspectmode = "manual",
    aspectratio = list(x = 1, y = 1, z = 0.7),
    camera = list(
      eye = list(x = 0.9, y = -1.5, z = 0.7)
    )
  )
)

fig
htmlwidgets::saveWidget(fig, "../figure_man/gradient2.html", selfcontained = TRUE)
