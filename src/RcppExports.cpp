// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// cut_c
List cut_c(List edges, NumericMatrix coord, double R);
RcppExport SEXP _spatgraphs_cut_c(SEXP edgesSEXP, SEXP coordSEXP, SEXP RSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type edges(edgesSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type coord(coordSEXP);
    Rcpp::traits::input_parameter< double >::type R(RSEXP);
    rcpp_result_gen = Rcpp::wrap(cut_c(edges, coord, R));
    return rcpp_result_gen;
END_RCPP
}
// prune_c
List prune_c(List edges, int level, int verbose);
RcppExport SEXP _spatgraphs_prune_c(SEXP edgesSEXP, SEXP levelSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type edges(edgesSEXP);
    Rcpp::traits::input_parameter< int >::type level(levelSEXP);
    Rcpp::traits::input_parameter< int >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(prune_c(edges, level, verbose));
    return rcpp_result_gen;
END_RCPP
}
// remove_nodes_sym_c
List remove_nodes_sym_c(List edges, IntegerVector set, bool fuse);
RcppExport SEXP _spatgraphs_remove_nodes_sym_c(SEXP edgesSEXP, SEXP setSEXP, SEXP fuseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type edges(edgesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type set(setSEXP);
    Rcpp::traits::input_parameter< bool >::type fuse(fuseSEXP);
    rcpp_result_gen = Rcpp::wrap(remove_nodes_sym_c(edges, set, fuse));
    return rcpp_result_gen;
END_RCPP
}
// spatcluster_c
List spatcluster_c(List x, int verbose);
RcppExport SEXP _spatgraphs_spatcluster_c(SEXP xSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(spatcluster_c(x, verbose));
    return rcpp_result_gen;
END_RCPP
}
// spatgraph_c
List spatgraph_c(NumericMatrix coord, int type, NumericVector parameters, double maxR, List preGraph, int verbose);
RcppExport SEXP _spatgraphs_spatgraph_c(SEXP coordSEXP, SEXP typeSEXP, SEXP parametersSEXP, SEXP maxRSEXP, SEXP preGraphSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type coord(coordSEXP);
    Rcpp::traits::input_parameter< int >::type type(typeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type parameters(parametersSEXP);
    Rcpp::traits::input_parameter< double >::type maxR(maxRSEXP);
    Rcpp::traits::input_parameter< List >::type preGraph(preGraphSEXP);
    Rcpp::traits::input_parameter< int >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(spatgraph_c(coord, type, parameters, maxR, preGraph, verbose));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_spatgraphs_cut_c", (DL_FUNC) &_spatgraphs_cut_c, 3},
    {"_spatgraphs_prune_c", (DL_FUNC) &_spatgraphs_prune_c, 3},
    {"_spatgraphs_remove_nodes_sym_c", (DL_FUNC) &_spatgraphs_remove_nodes_sym_c, 3},
    {"_spatgraphs_spatcluster_c", (DL_FUNC) &_spatgraphs_spatcluster_c, 2},
    {"_spatgraphs_spatgraph_c", (DL_FUNC) &_spatgraphs_spatgraph_c, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_spatgraphs(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
