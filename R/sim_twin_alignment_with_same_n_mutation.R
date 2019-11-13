#' Simulate a twin alignment using a standard site model
#'
#' This is an adapter function
#' (see \url{https://en.wikipedia.org/wiki/Adapter_pattern}),
#' with the purpose of passing \link{check_sim_twin_alignment_function},
#' by being a function with the function arguments \code{twin_phylogeny}
#' and \code{true_alignment}.
#' @inheritParams default_params_doc
#' @examples
#' library(testthat)
#'
#' # This adapter function must be a sim_twin_alignment function
#' expect_silent(
#'   check_sim_twin_alignment_function(
#'     sim_twin_alignment_with_std_site_model
#'   )
#' )
#'
#' # Simulate a twin DNA alignment
#'
#' alignment <- sim_twin_alignment_with_std_site_model(
#'   twin_phylogeny = ape::read.tree(text = "((A:1, B:1):2, C:3);"),
#'   root_sequence = "aaaa",
#'   mutation_rate = 0.1
#' )
#' expect_silent(check_alignment(alignment))
#' @seealso Use \link{sim_twin_alignment} to use this function to
#' create a twin alignment
#' @export
sim_twin_alignment_with_same_n_mutation <- function(
  twin_phylogeny,
  true_alignment,
  root_sequence,
  mutation_rate = 1.0,
  site_model = beautier::create_jc69_site_model(),
  max_n_tries = 100,
  verbose = FALSE
) {
  beautier::check_phylogeny(twin_phylogeny)
  pirouette::check_alignment(true_alignment)
  pirouette::check_root_sequence(root_sequence)
  pirouette::check_mutation_rate(mutation_rate)
  beautier::check_site_model(site_model)
  testthat::expect_equal(
    ape::Ntip(twin_phylogeny),
    pirouette::get_alignment_n_taxa(true_alignment)
  )
  testthat::expect_equal(
    nchar(root_sequence),
    pirouette::get_alignment_sequence_length(true_alignment)
  )

  twin_alignment <- pirouette::sim_alignment_with_n_mutations(
    phylogeny = twin_phylogeny,
    root_sequence = root_sequence,
    n_mutations = pirouette::count_n_mutations(
      alignment = true_alignment,
      root_sequence = root_sequence
    ),
    mutation_rate = mutation_rate,
    site_model = site_model,
    max_n_tries = max_n_tries,
    verbose = verbose
  )
  pirouette::check_alignment(twin_alignment)
  testthat::expect_equal(
    pirouette::get_alignment_n_taxa(twin_alignment),
    pirouette::get_alignment_n_taxa(true_alignment)
  )
  testthat::expect_equal(
    pirouette::get_alignment_sequence_length(twin_alignment),
    pirouette::get_alignment_sequence_length(true_alignment)
  )
  twin_alignment
}
