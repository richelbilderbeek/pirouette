#  Get a function to simulate a twin alignment with the same number
#' of mutations as the true alignment has.
#'
#' The twin alignment is simulated from the twin tree.
#' The number of mutations it will have is counted by comparing
#' it to the root sequence.
#' The twin alignment will have an equal amount of mutations
#' as the true alignment.
#'
#' This is an adapter function
#' (see \url{https://en.wikipedia.org/wiki/Adapter_pattern}),
#' with the purpose of passing \link{check_sim_twal_fun},
#' by being a function with the function arguments \code{twin_phylogeny}
#' and \code{true_alignment}.
#' @inheritParams default_params_doc
#' @return the function \link{sim_twal_with_same_n_mutation}
#' @seealso
#' See \link{check_sim_twal_fun} to the the other
#' functions to simulate a twin alignment.
#' Use \link{sim_twin_alignment} to use this function to
#' create a twin alignment.
#' @examples
#'
#' if (1 == 2) {
#' expect_silent(
#'   check_sim_twal_fun(
#'     get_sim_twal_same_n_muts_fun
#'   )
#' )
#' }
#' @export
get_sim_twal_same_n_muts_fun <- function(
  mutation_rate = 1.0,
  site_model = beautier::create_jc69_site_model(),
  max_n_tries = 100,
  verbose = FALSE
) {
  pirouette::check_mutation_rate(mutation_rate)
  beautier::check_site_model(site_model)
  pryr::partial(
    sim_twal_with_same_n_mutation,
    mutation_rate = mutation_rate,
    site_model = site_model,
    max_n_tries = max_n_tries,
    verbose = verbose
  )
}
