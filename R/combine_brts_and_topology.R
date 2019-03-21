#' Substitute branching times keeping the topology
#'
#' Set the branching times (in time units before the present)
#' of a phylogeny, while preserving its topology.
#' @inheritParams default_params_doc
#' @return a phylogeny of class \link[ape]{phylo}
#' @author Giovanni Laudanno, David Bapst, Richel J.C. Bilderbeek
#' @examples
#'   library(testthat)
#'
#'   # Branching times as 3 (crown age) and 2 (branch of A and B) time units ago
#'   phylogeny <- ape::read.tree(text = "((A:2, B:2):1, C:3);")
#'   expect_equal(c(3, 2), as.numeric(ape::branching.times(phylogeny)))
#'   expect_equal(
#'     2,
#'     ape::dist.nodes(phylogeny)[1, ape::getMRCA(phylogeny, c("A", "B"))]
#'   )
#'
#'   # Create a new phylogeny with the same topology, but with
#'   # branching times at 5 (crown age) and 4 (branch of A and B) time units ago
#'   new_phylogeny <- combine_brts_and_topology(
#'     brts = c(5, 4),
#'     tree = phylogeny
#'   )
#'   expect_equal(c(5, 4), as.numeric(ape::branching.times(new_phylogeny)))
#'   expect_equal(
#'     4,
#'     ape::dist.nodes(new_phylogeny)[
#'       1, ape::getMRCA(new_phylogeny, c("A", "B"))
#'     ]
#'   )
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
