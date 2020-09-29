#' Create a partially evaluated function to to \link{sim_yule_twin_tree}.
#'
#' The function \link{sim_yule_twin_tree} simulates a twin tree
#' using the Yule speciation model.
#' @inheritParams default_params_doc
#' @return a function
#' @seealso
#' Use \link{get_sim_bd_twin_tree_fun} to get a partially
#' evaluated function to produce a Birth-Death tree.
#' Use \link{create_copy_twtr_from_true_fun} to get a
#' function to simply copy the tree
#' @examples
#'
#' f <- create_sim_yule_twin_tree_fun()
#' phylo_in <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
#' f(phylo_in)
#' @author Richèl J.C. Bilderbeek
#' @export
create_sim_yule_twin_tree_fun <- function(
  method = "random_tree",
  n_replicates = 1e4
) {
  pryr::partial(
    sim_yule_twin_tree,
    method = method,
    n_replicates = n_replicates
  )
}
