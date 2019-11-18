#' Create a partially evaluated function to to \link{sim_yule_twin_tree}.
#'
#' The function \link{sim_yule_twin_tree} simulates a twin tree
#' using the Yule speciation model.
#' @inheritParams default_params_doc
#' @return a function
#' @seealso
<<<<<<< HEAD:R/create_sim_yule_twin_tree_function.R
#' Use \link{create_sim_bd_twin_tree_function} to get a partially
#' evaluated function to produce a Birth-Death tree.
#' Use \link{create_copy_twin_tree_from_true_function} to get a
=======
#' Use \link{create_sim_bd_twin_tree_fun} to get a partially
#' evaluated function to produce a Birth-Death tree.
#' Use \link{create_copy_twin_tree_from_true_fun} to get a
>>>>>>> b31a67ccf7a115ac420237774dfccbe724a0a7fa:R/create_sim_yule_twin_tree_fun.R
#' function to simply copy the tree
#' @examples
#' library(testthat)
#'
#' f <- create_sim_yule_twin_tree_fun()
#' phylo_in <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
#' phylo_out <- f(phylo_in)
#' expect_true(
#'   !all(ape::branching.times(phylo_in) == ape::branching.times(phylo_out))
#' )
#' @author Richèl J.C. Bilderbeek
#' @export
create_sim_yule_twin_tree_fun <- function(
  seed = 0,
  method = "random_tree",
  n_replicates = 1e4
) {
  pryr::partial(
    sim_yule_twin_tree,
    seed = seed,
    method = method,
    n_replicates = n_replicates
  )
}
