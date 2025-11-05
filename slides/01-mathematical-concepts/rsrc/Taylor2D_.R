# ------------------------------------------------------------------------------
# mathematical concepts

# FIG: Plot function surface sin(2x) + cos(y) and 
#      the Taylor series approximations (1st and 2nd order)
# ------------------------------------------------------------------------------

library(plotly)
library(pracma)

# ------------------------------------------------------------------------------

# Define grid ranges
xmin <- -1
xmax <- 3
xv <- seq(xmin, xmax, length.out = 50)
yv <- seq(xmin, xmax, length.out = 50)

# Create meshgrid
Xv <- repmat(xv, length(yv), 1)
Yv <- t(repmat(yv, length(xv), 1))

# Expansion point
xp <- 1
yp <- 1
off <- 1
xt <- seq(xp - off, xp + off, length.out = 100)
yt <- seq(yp - off, yp + off, length.out = 100)

# Meshgrid for Taylor approximations
Xt <- repmat(xt, length(yt), 1)
Yt <- t(repmat(yt, length(xt), 1))

# Define function and derivatives
f <- function(x, y) sin(2*x) + cos(y)
dfdx <- function(x, y) 2 * cos(2*x)
dfdy <- function(x, y) -sin(y)
dfdxdx <- function(x, y) -4 * sin(2*x)
dfdxdy <- function(x, y) 0
dfdydy <- function(x, y) -cos(y)

# 1st Order Taylor Expansion
taylor1 <- function(x, y, xp, yp) {
  f(xp, yp) + (x - xp) * dfdx(xp, yp) + (y - yp) * dfdy(xp, yp)
}

# 2nd Order Taylor Expansion
taylor2 <- function(x, y, xp, yp) {
  taylor1(x, y, xp, yp) + 0.5 * ((x - xp)^2 * dfdxdx(xp, yp) +
                                   (y - yp)^2 * dfdydy(xp, yp) +
                                   2 * (x - xp) * (y - yp) * dfdxdy(xp, yp))
}

# Compute function and Taylor values
Zf <- f(Xv, Yv)
Ztaylor1 <- taylor1(Xt, Yt, xp, yp)
Ztaylor2 <- taylor2(Xt, Yt, xp, yp)

camera_views <- list(
  list(eye = list(x = 0.1, y = 2.5, z = 0.3)),
  list(eye = list(x = 1.8, y = -1.8, z = 0.3))
)

# Generate and save each perspective
names = c('100', '301')
for (i in 1:length(camera_views)) {
  fig <- plot_ly() %>%
    add_surface(x = xv, y = yv, z = Zf, opacity = 0.5, colorscale = "Viridis") %>%
    add_markers(x = xp, y = yp, z = f(xp, yp), marker = list(color = "red", size = 5, symbol = "x")) %>%
    add_surface(x = xt, y = yt, z = Ztaylor1, colorscale = "Blues") %>%
    layout(
      scene = list(
        xaxis = list(title = "x₁"),
        yaxis = list(title = "x₂"),
        zaxis = list(title = "f(x₁, x₂)"),
        camera = camera_views[[i]],
        aspectmode = "cube"
      )
    )
  
  # Save each plot as an HTML file
  fig
  file_name <- paste0("../figure_man/Taylor2D_1st", names[i], ".html")
  htmlwidgets::saveWidget(fig, file_name, selfcontained = TRUE)
}

for (i in 1:length(camera_views)) {
  fig <- plot_ly() %>%
    add_surface(x = xv, y = yv, z = Zf, opacity = 0.5, colorscale = "Viridis") %>%
    add_markers(x = xp, y = yp, z = f(xp, yp), marker = list(color = "red", size = 5, symbol = "x")) %>%
    add_surface(x = xt, y = yt, z = Ztaylor2, colorscale = "Blues") %>%
    layout(
      scene = list(
        xaxis = list(title = "x₁"),
        yaxis = list(title = "x₂"),
        zaxis = list(title = "f(x₁, x₂)"),
        camera = camera_views[[i]],
        aspectmode = "cube"
      )
    )
  
  # Save each plot as an HTML file
  fig
  file_name <- paste0("../figure_man/Taylor2D_2nd-", names[i], ".html")
  htmlwidgets::saveWidget(fig, file_name, selfcontained = TRUE)
}
