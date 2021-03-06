#' Create a function that can simulate the twin tree from the true tree,
#' by just copying the true tree
#' @return a function
#' @seealso
#' Use \link{create_sim_yule_twin_tree_fun} to get a function to
#' produce a Yule tree.
#' Use \link{get_sim_bd_twin_tree_fun} to get a function to
#' produce a Birth-Death tree.
#' @examples
#' f <- create_copy_twtr_from_true_fun()
#' phylo_in <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
#' f(phylo_in)
#' @author Richèl J.C. Bilderbeek
#' @export
create_copy_twtr_from_true_fun <- function() {
  function(true_phylogeny) true_phylogeny
}
