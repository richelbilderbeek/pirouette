#' Create a function that can simulate the twin tree from the true tree,
#' by just copying the true tree
#' @return a function
#' @export
create_copy_twin_tree_from_true_function <- function() {
  function(phylogeny) phylogeny
}
