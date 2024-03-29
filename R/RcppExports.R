# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

cut_c <- function(edges, coord, R) {
    .Call('_spatgraphs_cut_c', PACKAGE = 'spatgraphs', edges, coord, R)
}

prune_c <- function(edges, level, verbose) {
    .Call('_spatgraphs_prune_c', PACKAGE = 'spatgraphs', edges, level, verbose)
}

remove_nodes_sym_c <- function(edges, set, fuse) {
    .Call('_spatgraphs_remove_nodes_sym_c', PACKAGE = 'spatgraphs', edges, set, fuse)
}

spatcluster_c <- function(x, verbose) {
    .Call('_spatgraphs_spatcluster_c', PACKAGE = 'spatgraphs', x, verbose)
}

spatgraph_c <- function(coord, type, parameters, maxR, preGraph, verbose) {
    .Call('_spatgraphs_spatgraph_c', PACKAGE = 'spatgraphs', coord, type, parameters, maxR, preGraph, verbose)
}

