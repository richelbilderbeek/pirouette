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
#' See \link{check_sim_twin_alignment_function} to the the other
#' functions to simulate a twin alignment.
#' Use \link{sim_twin_alignment} to use this function to
#' create a twin alignment.
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
get_sim_twin_alignment_with_same_n_mutation_function <- function(
  mutation_rate = 1.0
) {
  pryr::partial(
    sim_twin_alignment_with_same_n_mutation,
    mutation_rate = mutation_rate
  )
}
