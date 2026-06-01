# Used in: lecture_optimization/slides/10-bayesian-optimization/rsrc/surrogate.R
#
# Custom mlr3 ranger learner for the surrogate-model figures. It exposes
# response standard errors by using per-tree predictions from ranger.

library(R6)
library(mlr3)
library(mlr3learners)
library(mlr3misc)
library(paradox)
library(ranger)

set.seed(1L)

convert_ratio = getFromNamespace("convert_ratio", "mlr3learners")
ordered_features = getFromNamespace("ordered_features", "mlr3learners")

LearnerRegrRangerCustom = R6Class(
  "LearnerRegrRangerCustom",
  inherit = LearnerRegr,

  public = list(
    initialize = function() {
      param_set = ps(
        alpha = p_dbl(default = 0.5, tags = "train"),
        always.split.variables = p_uty(tags = "train"),
        holdout = p_lgl(default = FALSE, tags = "train"),
        importance = p_fct(
          c("none", "impurity", "impurity_corrected", "permutation"),
          tags = "train"
        ),
        keep.inbag = p_lgl(default = FALSE, tags = "train"),
        max.depth = p_int(default = NULL, lower = 0L, special_vals = list(NULL), tags = "train"),
        min.node.size = p_int(1L, default = 5L, special_vals = list(NULL), tags = "train"),
        min.prop = p_dbl(default = 0.1, tags = "train"),
        minprop = p_dbl(default = 0.1, tags = "train"),
        mtry = p_int(lower = 1L, special_vals = list(NULL), tags = "train"),
        mtry.ratio = p_dbl(lower = 0, upper = 1, tags = "train"),
        num.random.splits = p_int(1L, default = 1L, tags = "train"),
        num.threads = p_int(1L, default = 1L, tags = c("train", "predict", "threads")),
        num.trees = p_int(1L, default = 500L, tags = c("train", "predict", "hotstart")),
        oob.error = p_lgl(default = TRUE, tags = "train"),
        quantreg = p_lgl(default = FALSE, tags = "train"),
        regularization.factor = p_uty(default = 1, tags = "train"),
        regularization.usedepth = p_lgl(default = FALSE, tags = "train"),
        replace = p_lgl(default = TRUE, tags = "train"),
        respect.unordered.factors = p_fct(
          c("ignore", "order", "partition"),
          default = "ignore",
          tags = "train"
        ),
        sample.fraction = p_dbl(0, 1, tags = "train"),
        save.memory = p_lgl(default = FALSE, tags = "train"),
        scale.permutation.importance = p_lgl(default = FALSE, tags = "train"),
        se.method = p_fct(c("jack", "infjack", "simple"), default = "infjack", tags = "predict"),
        seed = p_int(default = NULL, special_vals = list(NULL), tags = c("train", "predict")),
        split.select.weights = p_uty(default = NULL, tags = "train"),
        splitrule = p_fct(c("variance", "extratrees", "maxstat"), default = "variance", tags = "train"),
        verbose = p_lgl(default = TRUE, tags = c("train", "predict")),
        write.forest = p_lgl(default = TRUE, tags = "train")
      )

      param_set$values = list(num.threads = 1L)
      param_set$add_dep("num.random.splits", "splitrule", CondEqual$new("extratrees"))
      param_set$add_dep("alpha", "splitrule", CondEqual$new("maxstat"))
      param_set$add_dep("minprop", "splitrule", CondEqual$new("maxstat"))
      param_set$add_dep("scale.permutation.importance", "importance", CondEqual$new("permutation"))

      super$initialize(
        id = "regr.ranger_custom",
        param_set = param_set,
        predict_types = c("response", "se"),
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = c("weights", "importance", "oob_error", "hotstart_backward"),
        packages = c("mlr3learners", "ranger"),
        man = "mlr3learners::mlr_learners_regr.ranger_custom"
      )
    },

    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      if (self$model$importance.mode == "none") {
        stopf("No importance stored")
      }

      sort(self$model$variable.importance, decreasing = TRUE)
    },

    oob_error = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }

      self$model$prediction.error
    }
  ),

  private = list(
    .train = function(task) {
      param_values = self$param_set$get_values(tags = "train")
      param_values = convert_ratio(param_values, "mtry", "mtry.ratio", length(task$feature_names))
      case_weights = task$weights_learner

      if (self$predict_type == "se") {
        param_values$keep.inbag = TRUE
      }

      invoke(
        ranger,
        dependent.variable.name = task$target_names,
        data = task$data(),
        case.weights = case_weights,
        .args = param_values
      )
    },

    .predict = function(task) {
      param_values = self$param_set$get_values(tags = "predict")
      newdata = ordered_features(task, self)

      if (isTRUE(param_values$se.method == "simple")) {
        prediction = invoke(
          predict,
          self$model,
          data = newdata,
          type = "response",
          .args = param_values[setdiff(names(param_values), "se.method")],
          predict.all = TRUE
        )
        response = rowMeans(prediction$predictions)
        variance = rowMeans(prediction$predictions^2) - response^2
        list(response = response, se = sqrt(pmax(variance, 0)))
      } else {
        prediction = invoke(
          predict,
          self$model,
          data = newdata,
          type = self$predict_type,
          .args = param_values
        )
        list(response = prediction$predictions, se = prediction$se)
      }
    },

    .hotstart = function(task) {
      model = self$model
      model$num.trees = self$param_set$values$num.trees
      model
    }
  )
)

default_values.LearnerRegrRangerCustom = function(x, search_space, task, ...) {
  special_defaults = list(
    mtry = floor(sqrt(length(task$feature_names))),
    mtry.ratio = floor(sqrt(length(task$feature_names))) / length(task$feature_names),
    sample.fraction = 1
  )
  defaults = insert_named(default_values(x$param_set), special_defaults)
  defaults[search_space$ids()]
}
