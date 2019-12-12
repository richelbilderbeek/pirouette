#' Get a function to simulate a twin alignment which uses a standard
#' site model
#' @inheritParams default_params_doc
#' @return a partially evaluated function of
#' \link{sim_twal_with_std_nsm}
#' @examples
#' library(testthat)
#'
#' f <- get_sim_twal_with_std_nsm_fun(
#'   mutation_rate = 0.1
#' )
#' # This adapter function must be a sim_twin_alignment function
#' expect_silent(
#'   check_sim_twal_fun(f)
#' )
#'
#' # Simulate a twin DNA alignment
#'
#' alignment <- f(
#'   twin_phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
#'   root_sequence = "aaaa"
#' )
#' expect_silent(check_alignment(alignment))
#' @seealso
#' See \link{check_sim_twal_fun} to the the other
#' functions to simulate a twin alignment.
#' Use \link{sim_twin_alignment} to use this function to
#' create a twin alignment.
#' @export
get_sim_twal_with_std_nsm_fun <- function(
  mutation_rate = 0.1,
  site_model = beautier::create_jc69_site_model()
) {
  pryr::partial(
    sim_twal_with_std_nsm,
    mutation_rate = mutation_rate,
    site_model = site_model
  )
}
