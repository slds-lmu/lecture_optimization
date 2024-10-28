gen_data = function(n, p) {
  X = matrix(runif(n * p, -1, 1), n, p)
  y = 1:n + rnorm(n, 0, 3)
  dd = as.data.frame(X)
  dd$y = y
  X0 = cbind(1, X)
  list(X = X, X0 = X0, y = y, df = dd)
}


gen_data_regr = function(n, p) {
  X = matrix(runif(n * p, -1, 1), n, p)
  y = rbinom(n, size = 1, prob = 0.5)
  dd = as.data.frame(X)
  dd$y = y
  X0 = cbind(1, X)
  list(X = X, X0 = X0, y = y, df = dd)
}

gen_data_classif = function(n, sd = 2) {
  dd = as.data.frame(mlbench.2dnormals(n = n, sd = sd))
  colnames(dd)[3] = "y"
  X = as.matrix(dd[, 1:2])
  X0 = cbind(1, X)
  beta_true = c(1, -1, 1)
  f = X0 %*% beta_true
  p = 1/(1+exp(f))
  y = rbinom(n, 1, p) 
  y = as.numeric(y) 
  dd$y = as.factor(y)
  list(X = X, X0 = X0, y = y, df = dd)
}
