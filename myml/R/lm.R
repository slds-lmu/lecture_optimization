fit_lm_r1 = function(X, y) {
  X = cbind(1, X)
  beta = solve(t(X) %*% X) %*% t(X) %*% y
  return(beta)
}

fit_lm_r2 = function(X, y) {
  X = cbind(1, X)
  beta = solve(t(X) %*% X, t(X) %*% y)
  return(beta)
}

