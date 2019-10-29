#' Create a function that can simulate the twin tree from the true tree,
#' using the BD speciation model
#' @inheritParams default_params_doc
#' @return a function
#' @examples
#' f <- create_sim_bd_twin_tree_function()
#' phylo_in <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
#' phylo_out <- f(phylo_in)
#' expect_true(
#'   !all(ape::branching.times(phylo_in) == ape::branching.times(phylo_out))
#' )
#' @export
create_sim_bd_twin_tree_function <- function(
  seed = 0,
  method = "random_tree",
  n_replicates = 1e4
) {
  functional::Curry(
    twin_to_bd_tree,
    seed = seed,
    method = method,
    n_replicates = n_replicates
  )
}
