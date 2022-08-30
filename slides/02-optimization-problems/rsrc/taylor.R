library(plotly)

# Taylor Series 2D

x1 = seq(-1, 1, by = 0.01)
x2 = seq(-1, 1, by = 0.01)

# value of function f
z = outer(x1, x2, function(x, y) x^2 + y^2)

taylor = function(x1, x2) {
  res = 0.5 + x1 + x2 - 1
}

# value of taylor approximation
z2 = outer(x1, x2, function(x, y) taylor(x, y))

p = plot_ly() %>% add_surface(x = x1, y = x2, z = z) %>% add_surface(x = x1, y = x2, z = z2, color = "red", alpha = 0.2) %>%
  layout(
    title = "",
    scene = list(
      xaxis = list(title = "x1"),
      yaxis = list(title = "x2"),
      zaxis = list(title = "z")
    ))

p
