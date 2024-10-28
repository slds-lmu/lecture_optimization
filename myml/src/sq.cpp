#include <Rcpp.h>
#include <RcppEigen.h>

// [[Rcpp::export]]
Eigen::VectorXd compute_square_rcpp(Eigen::VectorXd x) { 
  Eigen::VectorXd y = x.array().square();
  return(y);
}
