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
#'     sim_twin_alignment_with_standard_site_model
#'   )
#' )
#'
#' # Simulate a twin DNA alignment
#'
#' alignment <- sim_twin_alignment_with_standard_site_model(
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
  mutation_rate,
  site_model = beautier::create_jc69_site_model()
) {
  alignment <- sim_alignment_with_n_mutations(
    phylogeny = twin_phylogeny,
    alignment_params = create_alignment_params(
      root_sequence = root_sequence,
      mutation_rate = mutation_rate,
      site_model = site_model
    ),
    n_mutations = count_n_mutations(alignment = true_alignment, root_sequence = root_sequence)
  )
  check_alignment(alignment)
  alignment
}
