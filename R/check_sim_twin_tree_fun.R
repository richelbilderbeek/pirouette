#' Check if the \code{sim_twin_tree_fun} is valid
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @seealso
#' Use \link{check_sim_twal_fun} to check a function to
#' generate a twin alignment.
#' @export
check_sim_twin_tree_fun <- function(sim_twin_tree_fun) {
  if (!is.function(sim_twin_tree_fun)) {
    stop("'sim_twin_tree_fun' must be a function")
  }
  # check if sim_twin_tree_fun is indeed a function with 1 parameter
  arguments <- utils::capture.output(
    utils::str(args(sim_twin_tree_fun))
  )
  if (stringr::str_count(string = arguments, pattern = ",") > 0) {
    stop(
      "'sim_twin_tree_fun' must be a function with one argument"
    )
  }
  out <- NA
  tryCatch({
      out <- sim_twin_tree_fun(
      true_phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);")
    )
    }, error = function(c) {
      stop(
        "'sim_twin_tree_fun' must be a function ",
        "with one argument called 'true_phylogeny'"
      )
    }
  )

  # sim_twin_tree_fun must return a phylo
  if (!beautier::is_phylo(out)) {
    stop(
      "'sim_twin_tree_fun' must be a function that returns an ape::phylo"
    )
  }
  # sim_twin_tree_fun must return an ultrametric tree
  if (!ape::is.ultrametric(out)) {
    stop(
      "'sim_twin_tree_fun' must return an ultrametric tree"
    )
  }
  # sim_twin_tree_fun must return a tree with the same number of taxa
  test_true_phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  if (
    ape::Ntip(test_true_phylogeny) !=
    ape::Ntip(sim_twin_tree_fun(test_true_phylogeny))
  ) {
    stop(
      "'sim_twin_tree_fun' must return a tree with the same number of taxa"
    )
  }
  # sim_twin_tree_fun must return a tree with the same tip labels
  test_true_phylogeny <- ape::read.tree(text = "((A:1, B:1):1, C:2);")
  if (any(
    test_true_phylogeny$tip.label !=
    sim_twin_tree_fun(test_true_phylogeny)$tip.label
  )) {
    stop(
      "'sim_twin_tree_fun' must return a tree with the same taxon labels"
    )
  }
}
