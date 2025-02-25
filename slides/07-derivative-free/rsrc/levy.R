# ------------------------------------------------------------------------------
# derivative free

# FIG: plot levy function
# ------------------------------------------------------------------------------

library(plotly)

# ------------------------------------------------------------------------------

x1 = seq(-5, 5, by = 0.1)
x2 = seq(-5, 5, by = 0.1)

levy = function(x1, x2) {
  sin(3 * pi * x1)^2 + (x1 - 1)^2 * (1 + sin(3 * pi * x2)^2) +
    (x2 - 1)^2 * (1 + sin(2 * pi * x2)^2)
}

z = outer(x1, x2, levy)

p <- plot_ly() %>% add_surface(x = x1, y = x2, z = z) %>%
  layout(
    title = "Levy Funktion",
    scene = list(
      xaxis = list(title = "x1"),
      yaxis = list(title = "x2"),
      zaxis = list(title = "y")
    ))


htmlwidgets::saveWidget(p, "../figure_man/levy.html", selfcontained = TRUE)
