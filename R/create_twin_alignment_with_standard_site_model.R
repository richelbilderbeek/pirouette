#' Create a twin alignment using a standard site model
#'
#' This is an adapter function
#' (see \url{https://en.wikipedia.org/wiki/Adapter_pattern}),
#' with the purpose of passing \link{check_sim_twin_alignment_function},
#' by being a function with the function arguments \code{twin_phylogeny}
#' and \code{true_alignment}.
#' @inheritParams default_params_doc
#' @examples
#' check_sim_twin_alignment_function(
#'   sim_twin_alignment_function = create_twin_alignment_with_standard_site_model
#' )
#' @export
create_twin_alignment_with_standard_site_model <- function(
  twin_phylogeny,
  true_alignment = "irrelevant",
  root_sequence = "acgt",
  mutation_rate = 0.1,
  site_model = beautier::create_jc69_site_model()
) {
  alignment <- create_alignment_with_standard_site_model_raw(
    phylogeny = twin_phylogeny,
    root_sequence = root_sequence,
    mutation_rate = mutation_rate,
    site_model = site_model
  )
  check_alignment(alignment)
  alignment
}
