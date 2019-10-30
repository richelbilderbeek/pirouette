#' Check if the \code{sim_twin_tree_function} is valid
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @export
check_sim_twin_tree_function <- function(sim_twin_tree_function) {
  if (!is.function(sim_twin_tree_function)) {
    stop("'sim_twin_tree_function' must be a function")
  }
  # check if sim_twin_tree_function is indeed a function with 1 parameter
  arguments <- utils::capture.output(
    utils::str(args(sim_twin_tree_function))
  )
  if (stringr::str_count(string = arguments, pattern = ",") > 0) {
    stop(
      "'sim_twin_tree_function' must be a function with one argument"
    )
  }
  out <- NA
  tryCatch({
      out <- sim_twin_tree_function(
      true_phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);")
    )
    }, condition = function(c) {
      stop(
        "'sim_twin_tree_function' must be a function ",
        "with one argument called 'true_phylogeny'"
      )
    }
  )


  # sim_twin_tree_function must return a phylo
  if (!beautier::is_phylo(out)) {
    stop(
      "'sim_twin_tree_function' must be a function that returns an ape::phylo"
    )
  }

}
