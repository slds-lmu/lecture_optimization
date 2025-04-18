// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "myml_types.h"
#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// mylm1_a
Vd mylm1_a(Md X, Vd y);
RcppExport SEXP _myml_mylm1_a(SEXP XSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Md >::type X(XSEXP);
    Rcpp::traits::input_parameter< Vd >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(mylm1_a(X, y));
    return rcpp_result_gen;
END_RCPP
}
// mylm1_b
Vd mylm1_b(const MapMd X, const MapVd y);
RcppExport SEXP _myml_mylm1_b(SEXP XSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const MapMd >::type X(XSEXP);
    Rcpp::traits::input_parameter< const MapVd >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(mylm1_b(X, y));
    return rcpp_result_gen;
END_RCPP
}
// mylm2
Vd mylm2(MapMd X, MapVd y);
RcppExport SEXP _myml_mylm2(SEXP XSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MapMd >::type X(XSEXP);
    Rcpp::traits::input_parameter< MapVd >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(mylm2(X, y));
    return rcpp_result_gen;
END_RCPP
}
// mylm3_a
Vd mylm3_a(MapMd X, MapVd y);
RcppExport SEXP _myml_mylm3_a(SEXP XSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< MapMd >::type X(XSEXP);
    Rcpp::traits::input_parameter< MapVd >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(mylm3_a(X, y));
    return rcpp_result_gen;
END_RCPP
}
// mylm3_b
Vd mylm3_b(MapMd X, MapVd y);
RcppExport SEXP _myml_mylm3_b(SEXP XSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< MapMd >::type X(XSEXP);
    Rcpp::traits::input_parameter< MapVd >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(mylm3_b(X, y));
    return rcpp_result_gen;
END_RCPP
}
// mylm3_c
Vd mylm3_c(MapMd X, MapVd y);
RcppExport SEXP _myml_mylm3_c(SEXP XSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< MapMd >::type X(XSEXP);
    Rcpp::traits::input_parameter< MapVd >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(mylm3_c(X, y));
    return rcpp_result_gen;
END_RCPP
}
// mylogreg_1
List mylogreg_1(Md X, Vd y, double stepsize, unsigned int max_iter, double eps);
RcppExport SEXP _myml_mylogreg_1(SEXP XSEXP, SEXP ySEXP, SEXP stepsizeSEXP, SEXP max_iterSEXP, SEXP epsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Md >::type X(XSEXP);
    Rcpp::traits::input_parameter< Vd >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type stepsize(stepsizeSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type max_iter(max_iterSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    rcpp_result_gen = Rcpp::wrap(mylogreg_1(X, y, stepsize, max_iter, eps));
    return rcpp_result_gen;
END_RCPP
}
// compute_square_rcpp
Eigen::VectorXd compute_square_rcpp(Eigen::VectorXd x);
RcppExport SEXP _myml_compute_square_rcpp(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::VectorXd >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_square_rcpp(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_myml_mylm1_a", (DL_FUNC) &_myml_mylm1_a, 2},
    {"_myml_mylm1_b", (DL_FUNC) &_myml_mylm1_b, 2},
    {"_myml_mylm2", (DL_FUNC) &_myml_mylm2, 2},
    {"_myml_mylm3_a", (DL_FUNC) &_myml_mylm3_a, 2},
    {"_myml_mylm3_b", (DL_FUNC) &_myml_mylm3_b, 2},
    {"_myml_mylm3_c", (DL_FUNC) &_myml_mylm3_c, 2},
    {"_myml_mylogreg_1", (DL_FUNC) &_myml_mylogreg_1, 5},
    {"_myml_compute_square_rcpp", (DL_FUNC) &_myml_compute_square_rcpp, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_myml(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
