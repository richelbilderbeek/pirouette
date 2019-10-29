#' Checks if the argument is a valid twinning parameters structure.
#'
#' Will \link{stop} if not.
#' A valid twinning parameters structure can be created
#' by \link{create_twinning_params}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if nit
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' expect_silent(check_twinning_params(create_twinning_params()))
#' expect_error(check_twinning_params("nonsense"))
#' expect_error(check_twinning_params(NULL))
#' expect_error(check_twinning_params(NA))
#' @export
check_twinning_params <- function(
  twinning_params
) {
  # Check that the structure has all the list elements with the right names
  pirouette::check_twinning_params_names(twinning_params)

  # sim_twin_tree_function
  if (!is.function(twinning_params$sim_twin_tree_function)) {
    stop("'sim_twin_tree_function' must be a function")
  }
  # check if sim_twin_tree_function is indeed a function with 1 parameter
  arguments <- utils::capture.output(
    utils::str(args(twinning_params$sim_twin_tree_function))
  )
  if (stringr::str_count(string = arguments, pattern = ",") > 0) {
    stop(
      "'sim_twin_tree_function' must be a function with one argument"
    )
  }
  # sim_twin_tree_function must return a phylo
  if (!beautier::is_phylo(
    twinning_params$sim_twin_tree_function(
      ape::read.tree(text = "((A:1, B:1):1, C:2);"))
    )
  ) {
    stop(
      "'sim_twin_tree_function' must be a function that returns an ape::phylo"
    )
  }


  if (!beautier::is_one_int(twinning_params$rng_seed_twin_tree)) {
    stop("'rng_seed_twin_tree' must be a whole number")
  }
  if (!beautier::is_one_int(twinning_params$rng_seed_twin_alignment)) {
    stop("'rng_seed_twin_alignment' must be a whole number")
  }
  if (!assertive::is_a_string(twinning_params$twin_tree_filename)) {
    stop("'twin_tree_filename' must be a character vector")
  }
  if (!assertive::is_a_string(twinning_params$twin_alignment_filename)) {
    stop("'twin_alignment_filename' must be a character vector")
  }
  if (!is.character(twinning_params$twin_evidence_filename)) {
    stop("'twin_evidence_filename' must be a character vector")
  }
}
