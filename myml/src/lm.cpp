#include <Rcpp.h>
#include <RcppEigen.h>
#include "myml_types.h"

// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;

using Eigen::MatrixXd;
using Eigen::VectorXd;
using Eigen::Lower;


// mylm1_a:
// stupid first try implementation. 
//  - we copy X and add 1-vec for intercept
//  - stupid implementation of normal equations with matrix inversion
//  - we also dont map read-only input args, to they get copied, which is slower
// [[Rcpp::export]]
Vd mylm1_a(Md X, Vd y) { 
  const unsigned int n(X.rows()), p(X.cols());
  // we need to copy X here
  MatrixXd X0(n, p+1);
  X0.block(0, 1, n, p) = X;
  X0.col(0) = Vd::Ones(n);

  Vd theta = (X0.transpose() * X0).inverse() * X0.transpose() * y;
  return(theta);
}

// mylm1_b:
// - same as mylm1_a
// - but we map input args
// - we could do many other optims here, but this is still supposed to be the "stupid" version...

// [[Rcpp::export]]
Vd mylm1_b(const MapMd X, const MapVd y) { 
  const unsigned int n(X.rows()), p(X.cols());
  // we need to copy X here
  MatrixXd X0(n, p+1);
  X0.block(0, 1, n, p) = X;
  X0.col(0) = Vd::Ones(n);

  Vd theta = (X0.transpose() * X0).inverse() * X0.transpose() * y;
  return(theta);
}


// mylm2:
// - here we use the QR decomp, which is numericall quite stable

// [[Rcpp::export]]
Vd mylm2(MapMd X, MapVd y) {
  const unsigned int n(X.rows()), p(X.cols());
  // we need to copy X here
  MatrixXd X0(n, p+1);
  X0.block(0, 1, n, p) = X;
  X0.col(0) = Vd::Ones(n);

  Vd theta = X0.colPivHouseholderQr().solve(y);
  return(theta);
}

// mylm3_a:
// this is a simpler implementation of solving the normal equations via Cholesky

// [[Rcpp::export]]
Vd mylm3_a(MapMd X, MapVd y) { 
  const unsigned int n(X.rows()), p(X.cols());
  // we need to copy X here
  Md X0(n, p+1);
  X0.block(0, 1, n, p) = X;
  X0.col(0) = Vd::Ones(n);
  
  // we operate on some symmetric matrix here an lose some time
  // due to duplicated computations
  Md K = X0.transpose() * X0;
  Vd rhs = X0.transpose() * y;
  Vd theta = K.ldlt().solve(rhs);
  return(theta);
}

// mylm3_b:
// same as mylm3_a, but we use the selfadjointView to avoid the duplication of computations

// [[Rcpp::export(rng = false)]]
Vd mylm3_b(MapMd X, MapVd y) { 
  const unsigned int n(X.rows()), p(X.cols());
  // we need to copy X here
  MatrixXd X0(n, p+1);
  X0.block(0, 1, n, p) = X;
  X0.col(0) = Vd::Ones(n);
	
  Md XtX = Md::Zero(p+1,p+1);
  // compute X^T X and exploit symmetry
  XtX.selfadjointView<Lower>().rankUpdate(X0.transpose());
  // use cholesky and exploit symmetry
	Vd theta = XtX.selfadjointView<Lower>().ldlt().solve(X0.transpose() * y);
  return(theta);
}

// without extra 1-column
// as the rcppeigen package benchmark for LS
// does the col handling outside of the benchmark...

// [[Rcpp::export(rng = false)]]
Vd mylm3_c(MapMd X, MapVd y) { 
  const unsigned int n(X.rows()), p(X.cols());
	
  Md XtX = Md::Zero(p, p);
  // compute X^T X and exploit symmetry
  XtX.selfadjointView<Lower>().rankUpdate(X.transpose());
  // use cholesky and exploit symmetry
	Vd theta = XtX.selfadjointView<Lower>().ldlt().solve(X.transpose() * y);
  return(theta);
}

