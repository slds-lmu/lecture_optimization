# Used in: slides/11-multicrit/slides-multicrit-2-evolutionary.tex
#
# Creates the NSGA-II teaching figures: population snapshots, non-dominated
# fronts, and crowding-distance illustrations in two-objective space.

library(data.table)
library(ecr)
library(ggplot2)
library(grid)
library(gridExtra)
library(mco)
library(smoof)

set.seed(666L)

output_dir = "../figure"
population_size = 24L
snapshot_iterations = c(1L, 3L, 10L)

param_set = makeNumericParamSet("x", lower = c(0.1, 0), upper = c(1, 5))
lower_bounds = getLower(param_set)
upper_bounds = getUpper(param_set)

min_ex = makeMultiObjectiveFunction(
  name = "Min-Ex",
  fn = function(x) {
    c(x[1L], (1 + x[2L]) / (x[1L] * 60))
  },
  par.set = param_set
)

theme_set(theme_bw(base_size = 11))

save_plot = function(plot, filename, width, height) {
  ggsave(file.path(output_dir, filename), plot, width = width, height = height, dpi = 300)
}

plot_objective_space = function(objective) {
  design = generateRandomDesign(n = 10000L, par.set = getParamSet(objective))
  values = t(apply(design, 1L, objective))
  objective_values = data.table(f1 = values[, 1L], f2 = values[, 2L])

  ggplot(objective_values, aes(x = f1, y = f2)) +
    geom_point(size = 0.7, color = "grey55", alpha = 0.2) +
    labs(x = expression(f[1]), y = expression(f[2]))
}

run_nsga2_snapshot = function(generations) {
  set.seed(666L)
  nsga2(
    fn = min_ex,
    idim = 2L,
    odim = 2L,
    lower.bounds = lower_bounds,
    upper.bounds = upper_bounds,
    popsize = population_size,
    generations = generations,
    cprob = 0.7,
    cdist = 15,
    mprob = 0.2,
    mdist = 25
  )
}

as_population_table = function(population) {
  population_table = as.data.table(population$value)
  setnames(population_table, c("f1", "f2"))
  population_table
}

make_population_snapshot_plot = function(iteration, population, objective_space_plot) {
  population_table = as_population_table(population)

  objective_space_plot +
    geom_point(data = population_table, color = "#2C7FB8", size = 1.6) +
    labs(title = sprintf("Iteration %d", iteration))
}

choose_crowding_front = function(ranked_population, preferred_front = 3L, min_points = 6L) {
  front_sizes = table(ranked_population$front)
  front_candidates = as.integer(names(front_sizes[front_sizes >= min_points]))

  if (preferred_front %in% front_candidates) {
    preferred_front
  } else if (length(front_candidates) > 0L) {
    front_candidates[1L]
  } else {
    as.integer(names(front_sizes)[which.max(front_sizes)])
  }
}

make_crowding_rectangles = function(front, selected_points) {
  ordered_front = copy(front)
  setorder(ordered_front, f1)
  ordered_front[, f1_rank := .I]

  selected = merge(selected_points, ordered_front[, .(f1, f1_rank)], by = "f1")

  rectangles = rbindlist(lapply(seq_len(nrow(selected)), function(row_id) {
    point_rank = selected$f1_rank[row_id]
    previous_point = ordered_front[pmax(point_rank - 1L, 1L)]
    next_point = ordered_front[pmin(point_rank + 1L, nrow(ordered_front))]

    data.table(
      point = selected$point[row_id],
      f1 = selected$f1[row_id],
      f2 = selected$f2[row_id],
      xmin = min(previous_point$f1, next_point$f1),
      xmax = max(previous_point$f1, next_point$f1),
      ymin = min(previous_point$f2, next_point$f2),
      ymax = max(previous_point$f2, next_point$f2)
    )
  }))

  rectangles[, point := factor(point, levels = levels(selected_points$point))]
  rectangles
}

objective_space_plot = plot_objective_space(min_ex)
populations = lapply(snapshot_iterations, run_nsga2_snapshot)
names(populations) = as.character(snapshot_iterations)

snapshot_plots = Map(
  make_population_snapshot_plot,
  iteration = snapshot_iterations,
  population = populations,
  MoreArgs = list(objective_space_plot = objective_space_plot)
)

nsga2_steps = arrangeGrob(grobs = snapshot_plots, ncol = length(snapshot_plots))
if (interactive()) {
  grid.draw(nsga2_steps)
}

population_objectives = t(populations[["1"]]$value)
sorted_population = doNondominatedSorting(population_objectives)
rank_levels = seq_len(max(sorted_population$ranks))

ranked_population = as_population_table(populations[["1"]])
ranked_population[, front := factor(sorted_population$ranks, ordered = TRUE, levels = rank_levels)]

nsga2_nds = objective_space_plot +
  geom_line(
    data = ranked_population[order(front, f1)],
    aes(color = front, group = front),
    linetype = "dashed",
    linewidth = 0.45
  ) +
  geom_point(data = ranked_population, aes(color = front), size = 1.8) +
  labs(color = "Front") +
  theme(legend.position = "right")

crowding_front_id = choose_crowding_front(ranked_population)
crowding_front = ranked_population[as.integer(as.character(front)) == crowding_front_id, .(f1, f2)]
setorder(crowding_front, f1)
crowding_front[, crowding_distance := computeCrowdingDistance(t(as.matrix(.SD))), .SDcols = c("f1", "f2")]

low_crowding_points = crowding_front[order(crowding_distance)][seq_len(min(5L, .N))]
high_crowding_points = crowding_front[order(-crowding_distance)][seq_len(min(5L, .N))]

crowding_front_base = objective_space_plot +
  geom_line(data = crowding_front, linetype = "dashed", alpha = 0.45) +
  geom_point(data = crowding_front, alpha = 0.45) +
  theme(legend.position = "none")

low_crowding_plot = crowding_front_base +
  geom_point(data = low_crowding_points, size = 3, shape = 17) +
  labs(title = "Low crowding distance")

high_crowding_plot = crowding_front_base +
  geom_point(data = high_crowding_points, size = 3, shape = 17) +
  labs(title = "High crowding distance")

nsga2_crowding_examples = arrangeGrob(low_crowding_plot, high_crowding_plot, ncol = 2L)
if (interactive()) {
  grid.draw(nsga2_crowding_examples)
}

finite_crowding_front = crowding_front[is.finite(crowding_distance)]
selected_regions = rbindlist(list(
  copy(finite_crowding_front[which.max(crowding_distance)])[, point := "High CD"],
  copy(finite_crowding_front[which.min(crowding_distance)])[, point := "Low CD"]
))
selected_regions[, point := factor(point, levels = c("High CD", "Low CD"))]

crowding_rectangles = make_crowding_rectangles(crowding_front, selected_regions)

nsga2_crowding_regions = objective_space_plot +
  geom_line(data = crowding_front, linetype = "dashed") +
  geom_point(data = crowding_front) +
  geom_rect(
    data = crowding_rectangles,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = point, fill = point),
    alpha = 0.18
  ) +
  geom_point(data = crowding_rectangles, aes(x = f1, y = f2, color = point), size = 3) +
  scale_color_manual(values = c("High CD" = "#D55E00", "Low CD" = "#0072B2")) +
  scale_fill_manual(values = c("High CD" = "#D55E00", "Low CD" = "#0072B2")) +
  labs(color = NULL, fill = NULL) +
  theme(legend.position = "bottom")

if (interactive()) {
  print(nsga2_nds)
  print(nsga2_crowding_regions)
}

save_plot(nsga2_steps, "NSGA2_steps.png", width = 8, height = 4)
save_plot(nsga2_nds, "NSGA2_NDS.png", width = 4, height = 3)
save_plot(nsga2_crowding_examples, "NSGA2_CS1.png", width = 6, height = 3)
save_plot(nsga2_crowding_regions, "NSGA2_CS2.png", width = 3.2, height = 3.1)
