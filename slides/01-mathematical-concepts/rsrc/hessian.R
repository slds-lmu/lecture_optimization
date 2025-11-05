# ------------------------------------------------------------------------------
# mathematical concepts

# FIG: 
#   (1) 3D visualization of the function f(x,y)=sin(x)cos(y).
#   (2) same function as a contour map, with marked concavity.
# ------------------------------------------------------------------------------

library(ggplot2)
library(plotly)

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

fig_3d <- plot_ly(
  data = grid,
  x = ~x, y = ~y, z = ~z, type = "scatter3d",
  mode = "markers",
  marker = list(size = 4, color = ~z, symbol='diamond',
                colorscale = "RdBu", opacity = 0.7)
)

fig_3d <- fig_3d %>% add_trace(
  x = rep(x_seq, each = length(y_seq)), 
  y = rep(y_seq, times = length(x_seq)), 
  z = rep(min(grid$z), length(grid$z)),  # Project at minimum z-plane
  type = "scatter3d",
  mode = "lines",
  line = list(color = "black", width = 1),
  opacity = 0.3
)

# Apply layout settings
fig_3d <- fig_3d %>%
  layout(
    title = "",
    scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "f(x,y)", range = c(-1.2, 1.2)),
      aspectmode = "cube",
      camera = list(
        eye = list(x = -2.3, y = 1, z = 0.6)
    )
  )
)

fig_3d
htmlwidgets::saveWidget(fig_3d, "../figure_man/hessian_3d.html", selfcontained = TRUE)

# Contour plot
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
ggsave("../figure_man/hessian_contour.png", contour_plot, width = 10, height = 8)
