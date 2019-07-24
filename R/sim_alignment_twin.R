#' Converts a twin phylogeny to a random DNA alignment
#' @inheritParams default_params_doc
#' @return an alignment of type \code{DNAbin}
#' @seealso Use \link{sim_alignment_twin_file}
#'   to save the simulated alignment directly to a file
#' @author Richèl J.C. Bilderbeek
#' @export
sim_alignment_twin <- function(
  twin_phylogeny,
  root_sequence,
  rng_seed_twin_alignment,
  mutation_rate,
  site_model,
  n_mutations = NA
) {
  sim_alignment_raw( # nolint pirouette function
    phylogeny = twin_phylogeny,
    root_sequence = root_sequence,
    rng_seed = rng_seed_twin_alignment,
    mutation_rate = mutation_rate,
    site_model = site_model,
    n_mutations = n_mutations
  )

}
