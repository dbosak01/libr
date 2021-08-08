// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// bychanges
CharacterVector bychanges(CharacterVector dat);
RcppExport SEXP _libr_bychanges(SEXP datSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type dat(datSEXP);
    rcpp_result_gen = Rcpp::wrap(bychanges(dat));
    return rcpp_result_gen;
END_RCPP
}
// byfirst
LogicalVector byfirst(CharacterVector dat);
RcppExport SEXP _libr_byfirst(SEXP datSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type dat(datSEXP);
    rcpp_result_gen = Rcpp::wrap(byfirst(dat));
    return rcpp_result_gen;
END_RCPP
}
// bylast
LogicalVector bylast(CharacterVector dat);
RcppExport SEXP _libr_bylast(SEXP datSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type dat(datSEXP);
    rcpp_result_gen = Rcpp::wrap(bylast(dat));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_libr_bychanges", (DL_FUNC) &_libr_bychanges, 1},
    {"_libr_byfirst", (DL_FUNC) &_libr_byfirst, 1},
    {"_libr_bylast", (DL_FUNC) &_libr_bylast, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_libr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
