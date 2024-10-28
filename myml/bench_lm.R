# https://stackoverflow.com/questions/76282769/r-performance-of-running-linear-regressions-with-lm-vs-calculating-with-matr
# https://www.r-bloggers.com/2011/07/even-faster-linear-model-fits-with-r-using-rcppeigen/

# dont runt this with devtools!!!
# otherwise timings will be wrong.
# use "R CMD INSTALL ."

library(microbenchmark)
library(myml)
library(data.table)
library(ggplot2)
set.seed(1)
options(warn = 2)
options(width = 200)

benchmark = function(n, p, nrep = 1) {
  print(c(n, p))
  z = gen_data(n, p)
  b = microbenchmark(times = nrep, unit = "ms"
    , lm = lm(y ~ ., data = z$df)
    , lmfit = lm.fit(cbind(1, z$X), z$y)
    , r1 = fit_lm_r1(z$X, z$y)
    , r2 = fit_lm_r2(z$X, z$y)
    , mylm1_a = myml:::mylm1_a(z$X, z$y)
    , mylm1_b = myml:::mylm1_b(z$X, z$y)
    , mylm2 = myml:::mylm2(z$X, z$y)
    , mylm3_a = myml:::mylm3_a(z$X, z$y)
    , mylm3_b = myml:::mylm3_b(z$X, z$y)
    , mylm3_c = myml:::mylm3_c(z$X, z$y)
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

# b = benchmark(n = 1e5, p = 40L, nrep = 20)
# print(b)

dt = bench_p(n = 2e4, mult = 10, nrep = 1)  
# plot_scale(dt, slow_down = NULL)
plot_scale(dt, slow_down = "mylm3_c")

