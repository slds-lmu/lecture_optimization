# ------------------------------------------------------------------------------
# optimization problems

# FIG: contour plot of x + y with unit circle
# ------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)

# DATA -------------------------------------------------------------------------

f <- function(x, y) x + y
g <- function(x, y) x^2 + y^2 - 1

xmin <- -1.5
xmax <- 1.5

xv <- seq(xmin, xmax, length.out = 100)
yv <- seq(xmin, xmax, length.out = 100)
grid <- expand.grid(x = xv, y = yv)
grid$z <- with(grid, f(x, y))

# unit circle
t <- seq(0, 2 * pi, length.out = 300)
circle <- data.frame(x = cos(t), y = sin(t))

point_x <- -sqrt(2) / 2
point_y <- -sqrt(2) / 2

# PLOT -------------------------------------------------------------------------

plot <- ggplot() +
  # Contour plot of x + y
  geom_contour_filled(data = grid, aes(x = x, y = y, z = z)) +
  
  # Unit circle
  geom_path(data = circle, aes(x = x, y = y), color = "black", linewidth = 1) +
  
  # Red point at (-sqrt(2)/2, -sqrt(2)/2)
  geom_point(aes(x = point_x, y = point_y), color = "red", size = 5) +
  
  labs(
    x = expression(x[1]), 
    y = expression(x[2]), 
    fill = expression(f(x[1], x[2]))
  ) +
  xlim(xmin, xmax) + ylim(xmin, xmax) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    legend.position = "right"
  ) +
  coord_fixed()  # Ensure aspect ratio is 1:1

plot
ggsave("../figure_man/unit_circle.png", plot, width = 8, height = 6)
