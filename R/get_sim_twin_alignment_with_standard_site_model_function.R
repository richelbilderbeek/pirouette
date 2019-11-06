#' Get a function to simulate a twin alignment which uses a standard
#' site model
#' @inheritParams default_params_doc
#' @return the function \link{sim_twin_alignment_with_standard_site_model}
#' @examples
#' library(testthat)
#'
#' f <- get_sim_twin_alignment_with_standard_site_model_function()
#' # This adapter function must be a sim_twin_alignment function
#' expect_silent(
#'   check_sim_twin_alignment_function(f)
#' )
#'
#' # Simulate a twin DNA alignment
#'
#' alignment <- f(
#'   twin_phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
#'   root_sequence = "aaaa",
#'   mutation_rate = 0.1
#' )
#' expect_silent(check_alignment(alignment))
#' @seealso
#' Use \link{get_sim_twin_alignment_with_same_n_mutation_function} to
#' get a function that simulates a twin alignment using a standard
#' site model, that has the number number of mutations (comparing
#' to the root sequence) as the true alignment has.
#' Use \link{sim_twin_alignment} to use this function to
#' create a twin alignment.
#' @export
get_sim_twin_alignment_with_standard_site_model_function <- function() {
  sim_twin_alignment_with_standard_site_model
}
