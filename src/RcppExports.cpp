// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcpp_hello_world
List rcpp_hello_world();
RcppExport SEXP _mashproto_rcpp_hello_world() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(rcpp_hello_world());
    return rcpp_result_gen;
END_RCPP
}
// movement_init
List movement_init(List parameters);
RcppExport SEXP _mashproto_movement_init(SEXP parametersSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type parameters(parametersSEXP);
    rcpp_result_gen = Rcpp::wrap(movement_init(parameters));
    return rcpp_result_gen;
END_RCPP
}
// movement_step
List movement_step(List module, NumericVector time_step);
RcppExport SEXP _mashproto_movement_step(SEXP moduleSEXP, SEXP time_stepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type module(moduleSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type time_step(time_stepSEXP);
    rcpp_result_gen = Rcpp::wrap(movement_step(module, time_step));
    return rcpp_result_gen;
END_RCPP
}
// movements_of_human
NumericVector movements_of_human(List movement_list, IntegerVector human);
RcppExport SEXP _mashproto_movements_of_human(SEXP movement_listSEXP, SEXP humanSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type movement_list(movement_listSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type human(humanSEXP);
    rcpp_result_gen = Rcpp::wrap(movements_of_human(movement_list, human));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_mashproto_rcpp_hello_world", (DL_FUNC) &_mashproto_rcpp_hello_world, 0},
    {"_mashproto_movement_init", (DL_FUNC) &_mashproto_movement_init, 1},
    {"_mashproto_movement_step", (DL_FUNC) &_mashproto_movement_step, 2},
    {"_mashproto_movements_of_human", (DL_FUNC) &_mashproto_movements_of_human, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_mashproto(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}