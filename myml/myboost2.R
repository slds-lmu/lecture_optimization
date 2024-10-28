library(mlr3)
library(ggplot2)
set.seed(1)

l2b_train = function(data, target, n_iters, base_learner, alpha) {
  y = data[, target]
  intercept = mean(y)
  base_models = vector("list", n_iters)
  alphas = rep(alpha, n_iters)
  data2 = data
  preds = matrix(0, nrow = nrow(data), ncol = n_iters)  

  for (i in 1:n_iters) {
    cum_preds = preds %*% alphas + intercept
    r = y - cum_preds
    data2[, target] = r
    task = as_task_regr(id = "foo", data = data2, target = target)
    base_learner$train(task)
    base_models[[i]] = base_learner$clone()
    preds[, i] = base_learner$predict_newdata(data2)$response
    message(sprintf("iter=%i, mse=%g", i, sum(r*r)))
  }

  list(
    intercept = intercept,
    base_models = base_models,
    alphas = alphas
  )
}

l2b_predict = function(l2b_mod, newdata, iter) {
  bls = l2b_mod$base_models[1:iter]
  preds = lapply(bls, function(bl) bl$predict_newdata(newdata)$response)
  preds = do.call(cbind, preds)
  cum_preds = preds %*% l2b_mod$alphas[1:iter] + l2b_mod$intercept
  return(as.numeric(cum_preds))
}

n = 100; sd = 0.1
xs = seq(0, 2*pi, length.out = n)
ys = sapply(xs, sin) + rnorm(n, sd = sd)
dd = data.frame(x = xs, y = ys)


blrn = lrn("regr.rpart", maxdepth = 1, minsplit = 1, minbucket= 1, cp = 0)
n_iters = 400
mm = l2b_train(data = dd, target = "y", n_iters = n_iters,
  base_learner = blrn, alpha = 1)
#pp = l2b_predict(mm, newdata = dd, iter = n_iters-1)
#print(sum((pp - dd$y)^2))

myplot = function(mod, newdata, iter) {
  pp = l2b_predict(mod, newdata = newdata, 
    iter = iter)
  pl = ggplot(newdata, aes(x = x, y = y)) 
  pl = pl + geom_point()
  pdata = data.frame(x = newdata$x, p = pp)
  pl = pl + geom_line(data = pdata, 
    mapping = aes(x = x, y = p))
  print(pl)
}

myplot(mm,dd, iter = n_iters)