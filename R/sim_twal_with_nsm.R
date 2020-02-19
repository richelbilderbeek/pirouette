#' Simulate a twin alignment using a standard site model
#'
#' This is an adapter function
#' (see \url{https://en.wikipedia.org/wiki/Adapter_pattern}),
#' with the purpose of passing \link{check_sim_twal_fun},
#' by being a function with the function arguments \code{twin_phylogeny}
#' and \code{true_alignment}
#' @inheritParams default_params_doc
#' @examples
#' library(testthat)
#'
#' # This adapter function must be a sim_twin_alignment function
#' expect_silent(
#'   check_sim_twal_fun(
#'     sim_twal_with_std_nsm
#'   )
#' )
#'
#' # Simulate a twin DNA alignment
#' alignment <- sim_twal_with_std_nsm(
#'   twin_phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
#'   root_sequence = "aaaa",
#'   mutation_rate = 0.1
#' )
#' expect_silent(check_alignment(alignment))
#' @seealso Use \link{sim_twin_alignment} to use this function to
#' create a twin alignment
#' @author Richèl J.C. Bilderbeek
#' @export
sim_twal_with_std_nsm <- function(
  twin_phylogeny,
  root_sequence,
  true_alignment = "irrelevant",
  mutation_rate = 1.0,
  site_model = beautier::create_jc69_site_model()
) {
  alignment <- pirouette::sim_alignment_with_std_nsm(
    phylogeny = twin_phylogeny,
    root_sequence = root_sequence,
    mutation_rate = mutation_rate,
    site_model = site_model
  )
  pirouette::check_alignment(alignment)
  alignment
}
