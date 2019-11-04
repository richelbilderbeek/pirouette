#' Create a function that can simulate the twin tree from the true tree,
#' using the BD speciation model
#' @inheritParams default_params_doc
#' @return a function
#' @seealso
#' Use \link{create_sim_yule_twin_tree_function} to get a function to
#' produce a Yule tree.
#' Use \link{create_copy_twin_tree_from_true_function} to get a
#' function to simply copy the tree
#' @examples
#' library(testthat)
#'
#' f <- create_sim_bd_twin_tree_function()
#' phylo_in <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
#' phylo_out <- f(phylo_in)
#' expect_true(
#'   !all(ape::branching.times(phylo_in) == ape::branching.times(phylo_out))
#' )
#' @author Richèl J.C. Bilderbeek
#' @export
create_sim_bd_twin_tree_function <- function(
  seed = 0,
  method = "random_tree",
  n_replicates = 1e4
) {
  pryr::partial(
    twin_to_bd_tree,
    seed = seed,
    method = method,
    n_replicates = n_replicates
  )
}
