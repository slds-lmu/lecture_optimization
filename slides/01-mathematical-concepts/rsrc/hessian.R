# ------------------------------------------------------------------------------
# mathematical concepts

# FIG: 
#   (1) 3D visualization of the function f(x,y)=sin(x)cos(y).
#   (2) same function as a contour map, with marked concavity.
# ------------------------------------------------------------------------------

set.seed(1L)

library(ggplot2)

# DATA -------------------------------------------------------------------------

f <- function(x, y) sin(x) * cos(y)

x_seq <- seq(-3, 3, length.out = 50)
y_seq <- seq(-3, 3, length.out = 50)
grid <- expand.grid(x = x_seq, y = y_seq)
grid$z <- with(grid, f(x, y))

# Compute Hessian matrix and eigenvalues
calc_concav <- function(point) {
  matrix(c(-cos(point[2]) * sin(point[1]), -cos(point[1]) * sin(point[2]),
           -cos(point[1]) * sin(point[2]), -cos(point[2]) * sin(point[1])), nrow = 2)
}

a <- c(pi/2, 0)
wa <- eigen(calc_concav(a))$values  # lambda1 = lambda2 = -1

b <- c(0, -pi/2)
eig_b <- eigen(calc_concav(b))$values  # lambda1 = -1, lambda2 = 1

c <- c(-pi/2, 0)
eig_c <- eigen(calc_concav(c))$values  # lambda1 = lambda2 = 1

# PLOT -------------------------------------------------------------------------

contour_plot <- ggplot(grid, aes(x = x, y = y, z = z)) +
  geom_contour_filled(bins = 10) +
  geom_text(aes(x = pi/2, y = 0, label = "a"), color = "limegreen", size = 7) +
  geom_text(aes(x = 0, y = -pi/2, label = "b"), color = "purple", size = 7) +
  geom_text(aes(x = -pi/2, y = 0, label = "c"), color = "orange", size = 7) +
  
  # Arrows for Hessian eigenvectors
  geom_segment(aes(x = pi/2, y = 0, xend = pi/2, yend = 1), arrow = arrow(), color = "black") +
  geom_segment(aes(x = pi/2, y = 0, xend = pi/2 + 1, yend = 0), arrow = arrow(), color = "black") +
  
  geom_segment(aes(x = 0, y = -pi/2, xend = -1/sqrt(2), yend = -pi/2 + 1/sqrt(2)), arrow = arrow(), color = "black") +
  geom_segment(aes(x = 0, y = -pi/2, xend = 1/sqrt(2), yend = -pi/2 + 1/sqrt(2)), arrow = arrow(), color = "black") +
  
  geom_segment(aes(x = -pi/2, y = 0, xend = -pi/2, yend = 1), arrow = arrow(), color = "black") +
  geom_segment(aes(x = -pi/2, y = 0, xend = -pi/2 + 1, yend = 0), arrow = arrow(), color = "black") +
  
  labs(
    x = "x",
    y = "y"
  ) +
  theme_minimal()

contour_plot
# ../figure_man/hessian_contour.png" is a manually created screenshot
