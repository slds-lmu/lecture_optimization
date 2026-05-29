# Used in: slides-evolutionary-algorithms-2-ea-numeric.tex
#
# Generates a reproducible grid of non-overlapping circles. Each new circle is
# placed uniformly in the box and receives the largest feasible radius up to a
# random cap, which creates a varied but readable toy search landscape.

set.seed(124L)

library(data.table)
library(ggplot2)
library(ggforce)

distance_to_existing_circles = function(circles, x, y) {
  center_distance = sqrt((circles$x - x)^2 + (circles$y - y)^2)
  center_distance - circles$radius
}

place_random_circles = function(n_circles, box_size = 10, max_attempts = 10000L) {
  circles = data.table(x = numeric(), y = numeric(), radius = numeric())
  attempts = 0L

  while (nrow(circles) < n_circles && attempts < max_attempts) {
    attempts = attempts + 1L
    x = runif(1L, min = 0, max = box_size)
    y = runif(1L, min = 0, max = box_size)
    boundary_radius = min(x, box_size - x, y, box_size - y)
    random_radius_cap = runif(1L, min = 0.5, max = 1.5)

    feasible_radius = if (nrow(circles) == 0L) {
      min(boundary_radius, random_radius_cap)
    } else {
      distance_to_circles = distance_to_existing_circles(circles, x, y)

      if (any(distance_to_circles <= 0)) {
        next
      }

      min(boundary_radius, min(distance_to_circles), random_radius_cap)
    }

    circles = rbind(
      circles,
      data.table(x = x, y = y, radius = feasible_radius)
    )
  }

  if (nrow(circles) < n_circles) {
    stop("Could not place all circles within the attempt budget.")
  }

  circles
}

plot_circle_grid = function(circles, box_size = 10) {
  ggplot() +
    geom_circle(
      data = circles,
      aes(x0 = x, y0 = y, r = radius),
      fill = "grey75",
      color = "grey55",
      alpha = 0.8
    ) +
    geom_rect(
      aes(xmin = 0, xmax = box_size, ymin = 0, ymax = box_size),
      fill = NA,
      color = "grey45",
      linewidth = 0.5
    ) +
    coord_equal(xlim = c(0, box_size), ylim = c(0, box_size), expand = FALSE) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

circle_grid = place_random_circles(20L)
circle_grid_plot = plot_circle_grid(circle_grid)

dir.create("../figure", showWarnings = FALSE)

ggsave("../figure/grid.png", circle_grid_plot, width = 5, height = 5, dpi = 300, bg = "white")
