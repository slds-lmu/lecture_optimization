# Used in: slides/11-multicrit/slides-multicrit-4-practical.tex
#
# Recreates the ROC optimization figures for an SVM on the spam data set. The
# expensive ParEGO/random-search benchmark is cached in example_parego_res.rds;
# delete that file to recompute the benchmark from scratch.

library(data.table)
library(emoa)
library(ggplot2)
library(mlr)
library(mlr3misc)
library(mlrMBO)
library(parallelMap)

set.seed(1L)

output_dir = "../figure"
cache_file = "example_parego_res.rds"
n_evals = 100L
measures = list(tpr, fpr)

theme_set(theme_bw(base_size = 11))

save_plot = function(plot, filename, width = 6, height = 5) {
  ggsave(file.path(output_dir, filename), plot, width = width, height = height, dpi = 300)
}

make_search_space = function() {
  makeParamSet(
    makeNumericParam("cost", lower = -15, upper = 15, trafo = function(x) 2^x),
    makeNumericParam("gamma", lower = -15, upper = 15, trafo = function(x) 2^x),
    makeNumericParam("thresh", lower = 0, upper = 1)
  )
}

threshold_curve = function(prediction, task) {
  probabilities = sort(unique(getPredictionProbabilities(prediction, cl = task$task.desc$negative)))

  curve = map_dtr(probabilities, function(probability) {
    threshold_prediction = setThreshold(prediction, probability)
    as.data.frame(t(performance(threshold_prediction, measures = measures)))
  })

  unique(curve)
}

make_tuning_objective = function(search_space, benchmark_data) {
  makeMultiObjectiveFunction(
    name = "mlr",
    fn = function(x, data) {
      learner_params = x[intersect(names(x), getParamIds(getParamSet(data$learner)))]
      resampling_result = resample(
        learner = setHyperPars(data$learner, par.vals = learner_params),
        task = data$task,
        resampling = data$resampling,
        measures = measures
      )

      threshold_data = threshold_curve(resampling_result$pred, data$task)
      threshold_prediction = setThreshold(resampling_result$pred, x$thresh)
      scores = performance(threshold_prediction, measures = measures)
      attr(scores, "extras") = list(".curve" = threshold_data)
      scores
    },
    has.simple.signature = FALSE,
    par.set = search_space,
    noisy = FALSE,
    minimize = map_lgl(measures, "minimize"),
    n.objectives = length(measures)
  )
}

evaluate_configuration = function(x, search_space, learner, task_train, task_test, trafo = TRUE) {
  if (trafo) {
    x = trafoValue(par = search_space, x = x)
  }

  learner_params = x[intersect(names(x), getParamIds(getParamSet(learner)))]
  tuned_learner = setHyperPars(learner, par.vals = learner_params)
  model = train(tuned_learner, task_train)
  prediction = predict(model, task = task_test)
  threshold_data = threshold_curve(prediction, task_train)
  threshold_prediction = setThreshold(prediction, x$thresh)
  scores = as.data.table(as.list(performance(threshold_prediction, measures = measures)))
  scores[, ".curve" := list(list(threshold_data))]
  scores
}

run_benchmark = function() {
  task = spam.task
  learner = makeLearner("classif.svm", predict.type = "prob")
  search_space = make_search_space()

  outer_resampling = makeResampleInstance(makeResampleDesc("Holdout"), task)
  task_train = subsetTask(task, outer_resampling$train.inds[[1L]])
  task_test = subsetTask(task, outer_resampling$test.inds[[1L]])

  benchmark_data = list(
    learner = learner,
    task = task_train,
    resampling = makeResampleInstance(makeResampleDesc("CV", iters = 5L), task_train)
  )

  objective = make_tuning_objective(search_space, benchmark_data)
  mbo_control = makeMBOControl(n.objectives = 2L, y.name = map_chr(measures, "id"))
  mbo_control = setMBOControlTermination(mbo_control, max.evals = n_evals, iters = n_evals)
  mbo_control = setMBOControlMultiObj(mbo_control, method = "parego")

  parallel_started = FALSE
  on.exit(
    if (parallel_started) {
      parallelStop()
    },
    add = TRUE
  )

  parallelStartMulticore(4L, level = "mlr.resample")
  parallel_started = TRUE

  set.seed(1L)
  res_mbo = mbo(objective, control = mbo_control, more.args = list(data = benchmark_data))

  set.seed(1L)
  random_design = generateRandomDesign(n = mbo_control$max.evals, par.set = getParamSet(objective))
  random_xs = dfRowsToList(random_design, par.set = getParamSet(objective))
  random_xs = lapply(random_xs, trafoValue, par = getParamSet(objective))
  random_scores = parallelLapply(xs = random_xs, fun = objective, data = benchmark_data)

  parallelStop()
  parallel_started = FALSE

  res_rs = as.data.table(do.call(rbind, random_scores))
  res_rs[, tpr_loss := 1 - tpr]
  res_rs[, dominated := is_dominated(t(as.matrix(.SD))), .SDcols = c("fpr", "tpr_loss")]
  res_rs[, tpr_loss := NULL]
  res_rs[, extras := lapply(random_scores, attr, "extras")]
  res_rs[, xs := random_xs]
  res_rs = cbind(res_rs, random_design)

  y_outer_mbo = map_dtr(
    res_mbo$pareto.set,
    evaluate_configuration,
    search_space = search_space,
    learner = learner,
    task_train = task_train,
    task_test = task_test
  )
  y_outer_rs = map_dtr(
    res_rs[dominated == FALSE]$xs,
    evaluate_configuration,
    search_space = search_space,
    learner = learner,
    task_train = task_train,
    task_test = task_test,
    trafo = FALSE
  )

  result = list(
    res_mbo = res_mbo,
    y_outer_mbo = y_outer_mbo,
    res_rs = res_rs,
    y_outer_rs = y_outer_rs
  )
  saveRDS(result, cache_file)
  result
}

as_minimization_matrix = function(data) {
  t(as.matrix(data[, .(fpr, tpr_loss = 1 - tpr)]))
}

dominated_hv = function(data) {
  dominated_hypervolume(points = as_minimization_matrix(data), ref = c(1, 1))
}

mark_dominated = function(data) {
  data = copy(as.data.table(data))
  data[, dominated := is_dominated(as_minimization_matrix(data))]
  data
}

result = if (file.exists(cache_file)) {
  readRDS(cache_file)
} else {
  run_benchmark()
}

res_mbo = result$res_mbo
y_outer_mbo = as.data.table(result$y_outer_mbo)
y_outer_rs = as.data.table(result$y_outer_rs)
res_rs = as.data.table(result$res_rs)

random_search_path = copy(res_rs)
random_search_path[, tuner := "Random search"]

mbo_path = as.data.table(as.data.frame(res_mbo$opt.path))
pareto_front = unique(as.data.table(as.data.frame(res_mbo$pareto.front)))
pareto_front[, dominated := FALSE]
mbo_path = merge(mbo_path, pareto_front, by = colnames(res_mbo$pareto.front), all.x = TRUE)
mbo_path[, tuner := "ParEGO"]
mbo_path[is.na(dominated), dominated := TRUE]

tuning_path = rbindlist(list(mbo_path, random_search_path), fill = TRUE)

tuning_plot = ggplot(tuning_path[dominated == FALSE], aes(x = fpr, y = tpr, color = tuner)) +
  geom_point(data = tuning_path, alpha = 0.45, size = 1.6) +
  geom_step(linewidth = 0.6) +
  coord_cartesian(ylim = c(0.5, 1), xlim = c(0, 0.5)) +
  labs(
    title = "Tuning: SVM (cost, gamma, threshold) on spam data",
    subtitle = "Positive class: nonspam",
    x = "False positive rate",
    y = "True positive rate",
    color = "Optimizer"
  ) +
  theme(legend.position = "bottom")

outer_mbo = copy(y_outer_mbo)
outer_rs = copy(y_outer_rs)
outer_mbo[, `:=`(tuner = "ParEGO", type = "validation")]
outer_rs[, `:=`(tuner = "Random search", type = "validation")]
tuning_path[, type := "tuning"]
validation_points = rbindlist(list(outer_mbo, outer_rs), fill = TRUE)

validation_plot = ggplot(tuning_path[dominated == FALSE], aes(x = fpr, y = tpr, color = tuner, shape = type)) +
  geom_point(alpha = 0.5, size = 1.6) +
  geom_step(alpha = 0.5, linewidth = 0.6) +
  geom_point(data = validation_points, size = 2.2) +
  coord_cartesian(ylim = c(0.5, 1), xlim = c(0, 0.5)) +
  labs(
    title = "Tuning validation: SVM on spam data",
    subtitle = "Positive class: nonspam",
    x = "False positive rate",
    y = "True positive rate",
    color = "Optimizer",
    shape = "Evaluation"
  ) +
  theme(legend.position = "bottom")

outer_mbo = mark_dominated(outer_mbo)
outer_rs = mark_dominated(outer_rs)
validation_front = rbindlist(list(outer_mbo, outer_rs), fill = TRUE)

validation_pareto_plot = ggplot(validation_front[dominated == FALSE], aes(x = fpr, y = tpr, color = tuner)) +
  geom_point(data = validation_front, alpha = 0.5, size = 1.7) +
  geom_step(linewidth = 0.6) +
  coord_cartesian(ylim = c(0.5, 1), xlim = c(0, 0.5)) +
  labs(
    title = "Validation Pareto front: SVM on spam data",
    subtitle = "Positive class: nonspam",
    x = "False positive rate",
    y = "True positive rate",
    color = "Optimizer"
  ) +
  theme(legend.position = "bottom")

tuning_hv = tuning_path[, .(hypervolume = dominated_hv(.SD)), by = tuner, .SDcols = c("fpr", "tpr")]
validation_hv = validation_front[, .(hypervolume = dominated_hv(.SD)), by = tuner, .SDcols = c("fpr", "tpr")]

if (interactive()) {
  print(tuning_plot)
  print(validation_plot)
  print(validation_pareto_plot)
  print(tuning_hv)
  print(validation_hv)
}

save_plot(tuning_plot, "example_parego_spam.png")
save_plot(validation_plot, "example_parego_spam_outer.png")
save_plot(validation_pareto_plot, "example_parego_spam_outer_pareto.png")
