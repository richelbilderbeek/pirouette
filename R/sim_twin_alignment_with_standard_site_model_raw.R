#' Adapter function to create an alignment with a standard site model
#' @inheritParams default_params_doc
#' @return an alignment of type \code{DNAbin}
#' @export
sim_twin_alignment_with_standard_site_model_raw <- function(
  twin_phylogeny,
  true_alignment,
  root_sequence,
  mutation_rate,
  site_model
) {
  sim_alignment_with_std_site_model(
    phylogeny = twin_phylogeny,
    root_sequence = root_sequence,
    mutation_rate = mutation_rate,
    site_model = site_model
  )
}
