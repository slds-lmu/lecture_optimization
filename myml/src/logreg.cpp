#include <Rcpp.h>
#include <RcppEigen.h>
#include <math.h>
#include "myml_types.h"

// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;

using Eigen::MatrixXd;
using Eigen::VectorXd;
using Eigen::Lower;


// mylogreg_1:
// first try implementation. 
//  - we do GD on the loss, y = 0,1
//  - f = theta^T x; s(f) = (1 + exp(f))^-1
//  - ds / df = s(f) *(1 -s(f)); dL / df = s(f)-y; 
//  - dR / dtheta = sum_i (s(f_i) - y_i) x_i 
// [[Rcpp::export]]
List mylogreg_1(Md X, Vd y, double stepsize, unsigned int max_iter = 1000, double eps = 1e-2) { 
  const unsigned int n(X.rows()), p(X.cols());
  // we need to copy X here
  MatrixXd X0(n, p+1);
  X0.block(0, 1, n, p) = X;
  X0.col(0) = Vd::Ones(n);
  Vd theta(p+1); theta.setZero();
  unsigned int i;
  // FIXME: we probably dont need the overshoot count
  unsigned int n_overshoots = 0;
  double loss_cur, loss_old;

  for (i=0; i<max_iter; i++) {
    Vd f = X0 * theta;
    Vd s = 1 / (1 + (-f.array()).exp());
    // FIXME: maybe have option to disable this, to make faster?
    loss_cur = (-y.array() * s.array().log() - (1 - y.array()) * (1 - s.array()).log()).mean();
    if (loss_cur > loss_old) n_overshoots++;
    Vd r = (s-y);
    // we really need to divide by n here to get a stable steptsize
    Vd g = (X0.transpose() * r) / n;
    double g_norm = g.norm();
    theta = theta - stepsize * g;
    // Rprintf("iter=%i; gradnorm = %g loss=%g\n", i, g_norm, loss);
    if (g_norm < eps) break;
    loss_old = loss_cur;
  }
  List res = List::create(Named("theta") = theta, _["n_iters"] = i, _["n_overshoots"] = n_overshoots);
  return(res);
}

