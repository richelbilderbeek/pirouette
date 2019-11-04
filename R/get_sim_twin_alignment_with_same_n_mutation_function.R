#' Simulate a twin alignment from a
#' twin phylogeny and a true alignment, of which the twin
#' alignment has an equal amount of mutations from the root sequence
#' as the true alignment has
#'
#' This is an adapter function
#' (see \url{https://en.wikipedia.org/wiki/Adapter_pattern}),
#' with the purpose of passing \link{check_sim_twin_alignment_function},
#' by being a function with the function arguments \code{twin_phylogeny}
#' and \code{true_alignment}.
#' @inheritParams default_params_doc
#' @return the function \link{sim_twin_alignment_with_same_n_mutation}
#' @seealso
#' Use \link{get_sim_twin_alignment_with_standard_site_model_function} to
#' get a function that simulates a twin alignment ignoring the number
#' of mutations in the true alignment
#' @examples
#' library(testthat)
#'
#' if (1 == 2) {
#' expect_silent(
#'   check_sim_twin_alignment_function(
#'     get_sim_twin_alignment_with_same_n_mutation_function
#'   )
#' )
#' }
#' @export
get_sim_twin_alignment_with_same_n_mutation_function <- function() {
  sim_twin_alignment_with_same_n_mutation
}
