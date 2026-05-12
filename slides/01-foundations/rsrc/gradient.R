# ------------------------------------------------------------------------------
# mathematical concepts

# FIG: plot gradients
# ------------------------------------------------------------------------------

set.seed(1L)

library(ggplot2)

# ------------------------------------------------------------------------------
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

ggsave(filename = "../figure/gradient_unit_vectors.png", p)
