#' Create an alignment with a standard site model
#' @inheritParams default_params_doc
#' @return an alignment of type \code{DNAbin}
#' @export
create_alignment_with_std_site_model <- function(
  phylogeny,
  alignment_params
) {
  pirouette::sim_alignment_with_std_site_model(
    phylogeny = phylogeny,
    root_sequence = alignment_params$root_sequence,
    mutation_rate = alignment_params$mutation_rate,
    site_model = alignment_params$site_model
  )
}
