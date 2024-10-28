# https://stackoverflow.com/questions/76282769/r-performance-of-running-linear-regressions-with-lm-vs-calculating-with-matr
# https://www.r-bloggers.com/2011/07/even-faster-linear-model-fits-with-r-using-rcppeigen/

# dont runt this with devtools!!!
# otherwise timings will be wrong.
# use "R CMD INSTALL ."

library(microbenchmark)
library(data.table)
library(ggplot2)
library(devtools)
library(mlbench)
library(mlr3)
library(mlr3measures)
library( mlr3learners)
# library(myml)
load_all()

set.seed(1)
options(warn = 1)
options(width = 200)

calc_metrics = function(theta, z, task) {
  y_fct = task$truth()
  y_01 = as.numeric(y_fct == "1")

  ms = msr("classif.logloss")
  p = 1 / (1 + exp(-z$X0 %*% theta))
  p = cbind(p , 1-p)
  colnames(p) = c("1", "0")
  loss = ms$fun(truth = y_fct, prob = p)
    
  r = p[,1] - y_01
  g = (t(z$X0) %*% r) / nrow(z$X0)  
  g_norm = sqrt(sum(g^2))
  list(loss = loss, g_norm = g_norm)
}


check_accuracy = function(n, p, nrep = 1) {
  vals = numeric()
  rep = 1:nrep
  g = expand.grid(n = n, p = p, rep = rep)
  g = as.data.table(g)

  stepsize = 0.01
  eps = 1e-5
  max_iter = 1e5

  for (i in 1:nrow(g)) {
    # FIXME: higher dim p not supported yet
    z = gen_data_classif(n = g$n[i])
    task = as_task_classif(z$df, target = "y", positive = "1")
    lrn = lrn("classif.log_reg", predict_type = "prob")
    m1 = lrn$train(task)
    theta0 = coef(lrn$model)
    met = calc_metrics(theta0, z, task)
    g$glm_loss[i] = met$loss
    g$glm_gnorm[i] = met$g_norm

    m_cpp_gd = mylogreg_1(z$X, z$y, stepsize = stepsize, max_iter = max_iter, eps = eps)
    met = calc_metrics(m_cpp_gd$theta, z, task)
    g$cpp_gd_linf[i] = max(abs(m_cpp_gd$theta - theta0))
    g$cpp_gd_loss[i] = met$loss
    g$cpp_gd_gnorm[i] = met$g_norm
    g$cpp_gd_niters[i] = m_cpp_gd$n_iters

    #beta_r_gd = mylogreg_r_gd(z$X, z$y, stepsize = 0.001, max_iter = 100000)
    # l_inf_r_gd = max(abs(beta_r_gd - beta0))
    # g$l_inf_r_gd[i] = l_inf_r_gd
  }
#   g = g[, .(
#     linf_cpp_gd = max(linf_cpp_gd)
# #    , linf_r_gd = max(linf_r_gd)
#   ), 
#   by = .(n, p)]
  return(g)
}


benchmark = function(n, p, nrep = 1) {
  print(c(n, p))
  # FIXME: pass p
  z = gen_data_classif(n)
  b = microbenchmark(times = nrep, unit = "ms"
    , glm = glm(y ~ ., data = z$df, family = "binomial")
    , mylogreg_1 = myml:::mylogreg_1(z$X, z$y, stepsize = 0.01)
  )
  return(b)
}

bench_n = function(p = 5, mult = 10000, nrep = 30) {
  n_seq = 2^(1:7) * mult
  bs = lapply(n_seq, function(n) {
    summary(benchmark(n, p = p, nrep = nrep))
  })
  cns = as.character(bs[[1]]$expr)
  b = as.data.table(do.call(rbind, lapply(bs, function(x) x$mean)))
  colnames(b) = cns
  b = cbind(data.table(n = n_seq), b)
  print(b)
}

bench_p = function(n = 1e5, mult = 10, nrep = 30) {
  p_seq = 2^(1:7) * mult
  bs = lapply(p_seq, function(p) {
    summary(benchmark(n = n, p = p, nrep = nrep))
  })
  cns = as.character(bs[[1]]$expr)
  b = as.data.table(do.call(rbind, lapply(bs, function(x) x$mean)))
  colnames(b) = cns
  b = cbind(data.table(p = p_seq), b)
}

plot_scale = function(dt, slow_down = NULL) {
  if (!is.null(slow_down)) {
    dt[, 2:ncol(dt)] = dt[, 2:ncol(dt)] / dt[[slow_down]]
  }
  xvar = colnames(dt)[1]
  gg = melt(dt, id.vars = xvar, variable.name = "method", value = "time")
  pl = ggplot(gg, aes_string(x = xvar, y = "time", col = "method"))
  pl = pl + geom_line(lwd = 2)
  ggsave("plot_n.png")
}


z = check_accuracy(n = 1e2 * 2^(1:7), p = 2, nrep = 1)
print(z)
# print(max(z$l_inf))
# b = benchmark(n = 1e2, p = 4L, nrep = 2)
# print(b)
# dt = bench_n(p = 2, mult = 100, nrep = 1)  
# plot_scale(dt, slow_down = NULL)
# plot_scale(dt, slow_down = "mylm3_c")

