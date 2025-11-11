# ------------------------------------------------------------------------------
# derivative free

# FIG: plot griewank function
# ------------------------------------------------------------------------------

library(plotly)

# ------------------------------------------------------------------------------

x1 = seq(-300, 300, by = 1)
x2 = seq(-300, 300, by = 1)

griewank = function(x1, x2) {
  1 + 1 / 4000 * (x1^2 + x2^2) - cos(x1) * cos(x2 / sqrt(2))
}

z = outer(x1, x2, griewank)

p <- plot_ly() %>% add_surface(x = x1, y = x2, z = z) %>%
  layout(
    title = "Griewank Funktion",
    scene = list(
      xaxis = list(title = "x1"),
      yaxis = list(title = "x2"),
      zaxis = list(title = "y")
    ))
p

htmlwidgets::saveWidget(p, "../figure_man/griewank.html", selfcontained = TRUE)
