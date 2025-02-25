# ------------------------------------------------------------------------------
# multivariate first order

# FIG: plot negative gradients
# ------------------------------------------------------------------------------

library(ggplot2)

# ------------------------------------------------------------------------------

# Define the function
f <- function(x) { x^4 + 7*x^3 + 5*x^2 - 17*x + 3 }

# Generate x and y values
x_vals <- seq(-6, 2, length.out = 50)
y_vals <- f(x_vals)

# Global minimum (from Python)
nsx <- -4.48
nsy <- f(nsx)

# Define vertical dashed lines
point1 <- data.frame(x = c(nsx, nsx), y = c(nsy, -60))
point2 <- data.frame(x = c(nsx, -6.5), y = c(nsy, nsy))

# Define negative gradient arrows
arrows <- data.frame(
  xstart = c(-6, -2.5, -1, 2),
  ystart = c(20, 0, 15, 40),
  xend = c(-6 + 0.3, -2.5 - 1, -1 + 0.85, 2 - 0.4),
  yend = c(20 - 40, 0 - 35, 15 - 15, 40 - 30)
)

# Create the plot
p <- ggplot() +
  # Plot function curve
  geom_line(data = data.frame(x = x_vals, y = y_vals), aes(x, y), color = "red") +
  
  # Add vertical dashed lines
  geom_line(data = point1, aes(x, y), linetype = "dashed", color = "blue") +
  geom_line(data = point2, aes(x, y), linetype = "dashed", color = "blue") +
  
  # Add negative gradient arrows
  geom_segment(data = arrows, aes(x = xstart, y = ystart, xend = xend, yend = yend), 
               arrow = arrow(length = unit(0.2, "inches")), color = "black") +
  annotate("text", x = 1, y = -50, label = expression(f(x) == x^4 + 7*x^3 + 5*x^2 - 17*x + 3), 
           hjust = 1, size = 5, color = "black") +
  labs(x = "x", y = "y") +
  
  xlim(-6.5, 2.5) + ylim(-60, 70) +  # Match x and y limits
  theme_minimal()

p
ggsave("../figure_man/negative_gradients.png", p, width = 6, height = 4)
