#' @title Substitute branching times keeping the topology
#' @description Given a tree topology and a set of branching times,
#' it combines them together. They have to be compatible.
#' @inheritParams default_params_doc
#' @return a tree
#' @author Giovanni Laudanno, David Bapst
#' @export
combine_brts_and_topology <- function(
  brts,
  tree
) {
  if (length(brts) != ape::Nnode(tree)) {
    stop("brts must be same length as number of nodes on input tree")
  }
  # if(!is.null(tree$edge.lengths)){
  #   message("Warning: input tree has $edge.lengths present, these
  #           will be replaced")}
  tree$edge.length <- NULL # nolint

  #add zero ages for tips
  all_ages <- c(rep(0, ape::Ntip(tree)), brts)

  # get mother node age for each edge
  mom_ages <- all_ages[tree$edge[, 1]]

  # get node ages for child nodes of each edge
  child_ages <- all_ages[tree$edge[, 2]]

  #edge lengths = mom - child
  edge_lengths <- mom_ages - child_ages
  tree$edge.length <- edge_lengths #nolint
  return(tree)
}
