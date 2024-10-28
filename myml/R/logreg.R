# mylogreg_r_gd:
# first try implementation
# - we do GD on the loss, y = 0,1
# - f = theta^T x; s(f) = (1 + exp(f))^-1
# - ds / df = s(f) *(1 -s(f)); dL / df = s(f)-y
# - dR / dtheta = sum_i (s(f_i) - y_i) x_i

mylogreg_r_gd = function(X, y, stepsize, max_iter = 1000, eps = 1e-2) {
  X0 = cbind(1, X)
  theta = rep(0, ncol(X)+1)

  for (i in 0:max_iter) {
    f = X0 %*% theta
    s = 1 / (1 + exp(-f))
    r = (s-y)
    g = t(X0) %*% r
    g_norm = sqrt(sum(g^2))
    theta = theta - stepsize * g
    if (log_trace) {
      p = pmax(eps, pmin(1 - eps, p))
      -mean(log(p), sample_weights)
      catf("iter=%i; gradnorm = %g loss=%g\n", i, g_norm, loss)
    }
    if (g_norm < eps) break
  }
  list(theta = theta, n_iters = n_iters) 
}

mylogreg_r_gd_bt = function(X, y, stepsize, max_iter = 1000, eps = 1e-2) {
  X0 = cbind(1, X)
  theta = rep(0, ncol(X)+1)
  alpha = 1
  # FIXME: add option to normalize grad

  for (i in 0:max_iter) {
    f = X0 %*% theta
    s = 1 / (1 + exp(-f))
    r = (s-y)
    g = t(X0) %*% r
    g_norm = sqrt(sum(g^2))
    theta = theta - stepsize * g
    #catf("iter=%i; gradnorm = %g loss=%g\n", i, g_norm, loss)
    if (g_norm < eps) break
  }
  list(theta = theta, n_iters = n_iters) 
}