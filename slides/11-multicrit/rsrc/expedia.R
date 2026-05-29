# Used in: slides/11-multicrit/slides-multicrit-1a-intro.tex
#
# Recreates the hotel-choice example from a small Expedia data snapshot. All
# objectives are shown in minimization form: price and negative rating.

library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)

set.seed(1L)

output_dir = "../figure"
rating_limits = c(-5.5, -2)

theme_set(theme_bw(base_size = 10))

save_plot = function(plot, filename, width, height) {
  ggsave(file.path(output_dir, filename), plot, width = width, height = height, dpi = 300)
}

base_hotel_plot = function(data = hotels, point_size = 1.5) {
  ggplot(data, aes(x = mean_price, y = negative_rating)) +
    geom_point(size = point_size) +
    coord_cartesian(ylim = rating_limits) +
    labs(x = "Price per night", y = "Negative rating")
}

add_pareto_front = function(plot, front, color = "#F58518") {
  plot +
    geom_point(data = front, color = color, size = 2) +
    geom_step(data = front, color = color, linewidth = 0.55, direction = "vh")
}

hotels = as.data.table(readRDS("expedia_example.rds"))
hotels[, negative_rating := -mean_rating]
hotels[, weighted_sum := mean_price + 50 * negative_rating]

pareto_front = copy(hotels)
setorder(pareto_front, negative_rating, mean_price)
pareto_front = pareto_front[!duplicated(cummin(mean_price))]
setorder(pareto_front, mean_price)

dominance_points = copy(hotels[c(16L, 17L)])
dominance_points[, relation := factor(c("dominates", "dominated"), levels = c("dominates", "dominated"))]

incomparable_points = copy(hotels[c(10L, 16L)])
incomparable_points[, relation := "incomparable"]

best_weighted_hotel = hotels[which.min(weighted_sum)]
best_rating_hotels = hotels[negative_rating == min(negative_rating)]
best_lexicographic_hotel = best_rating_hotels[which.min(mean_price)]

selected_posterior_hotel = pareto_front[which.min(abs(negative_rating + 4.5))]

expedia_scatter = base_hotel_plot()

dominance_plot = base_hotel_plot() +
  geom_point(data = dominance_points, aes(color = relation), size = 2.4, show.legend = FALSE) +
  scale_color_manual(values = c(dominates = "#009E73", dominated = "#D55E00"))

incomparability_plot = base_hotel_plot(point_size = 2) +
  geom_point(data = incomparable_points, color = "#F58518", size = 2.4)

dominance_comparison = arrangeGrob(dominance_plot, incomparability_plot, ncol = 2L)

pareto_plot = add_pareto_front(base_hotel_plot(point_size = 2), pareto_front)

weighted_sum_plot = ggplot(hotels, aes(x = weighted_sum, y = 0)) +
  geom_point(size = 2) +
  geom_point(data = best_weighted_hotel, color = "#009E73", size = 2.4) +
  labs(x = "Weighted sum", y = NULL) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

weighted_objective_plot = base_hotel_plot(point_size = 2) +
  geom_point(data = best_weighted_hotel, color = "#009E73", size = 2.4)

weighted_sum_comparison = arrangeGrob(weighted_sum_plot, weighted_objective_plot, ncol = 2L)

lexicographic_rating_plot = base_hotel_plot(point_size = 2) +
  geom_point(data = best_rating_hotels, color = "#F58518", size = 2.4) +
  labs(title = "1) min. negative rating")

lexicographic_price_plot = lexicographic_rating_plot +
  geom_point(data = best_lexicographic_hotel, color = "#009E73", size = 2.8) +
  labs(title = "2) min. price")

lexicographic_comparison = arrangeGrob(lexicographic_rating_plot, lexicographic_price_plot, ncol = 2L)

posterior_choice_plot = pareto_plot +
  geom_point(data = selected_posterior_hotel, color = "#009E73", size = 2.8)

posterior_comparison = arrangeGrob(pareto_plot, posterior_choice_plot, ncol = 2L)

if (interactive()) {
  print(expedia_scatter)
  grid.draw(dominance_comparison)
  print(incomparability_plot)
  print(pareto_plot)
  grid.draw(weighted_sum_comparison)
  grid.draw(lexicographic_comparison)
  grid.draw(posterior_comparison)
}

save_plot(expedia_scatter, "expedia-1-1.pdf", width = 4, height = 2)
save_plot(dominance_comparison, "expedia-2-1.pdf", width = 6.5, height = 2)
save_plot(incomparability_plot, "expedia-4-1.pdf", width = 4, height = 2)
save_plot(pareto_plot, "expedia-5-1.pdf", width = 4, height = 2)
save_plot(weighted_sum_comparison, "expedia-9-1.pdf", width = 4, height = 2)
save_plot(lexicographic_comparison, "expedia-10-1.pdf", width = 6.5, height = 2)
save_plot(posterior_comparison, "expedia-11-1.pdf", width = 6.5, height = 2)
