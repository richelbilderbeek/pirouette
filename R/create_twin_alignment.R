#' Creates a twin alignment.
#'
#' A twin alignment is an alignment that has as much
#' mutations accumulated from crown to the tips
#' @inheritParams default_params_doc
#' @return an alignment of class DNAbin that has as much
#'   mutations accumulated from crown to the tips as the
#'   original, 'true' alignment
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_twin_alignment <- function(
  twin_phylogeny,
  true_alignment,
  alignment_params,
  twinning_params
) {
  # Check inputs
  if (!is_phylo(twin_phylogeny)) {
    stop("'twin_phylogeny' must be a of class 'phylo'")
  }
  if (class(true_alignment) != "DNAbin") {
    stop("'true_alignmnent' must be a of class 'DNAbin'")
  }
  check_alignment_params(alignment_params)
  check_twinning_params(twinning_params)

  # Count the goal number of mutations
  n_mutations_true <- count_n_mutations(
    alignment = true_alignment,
    root_sequence = alignment_params$root_sequence
  )

  # Stub: if the sum of branch lengths between the true and twin tree
  # differ, one may want to increase/decrease the mutation rate to
  # reach the same number of mutations more easy.
  #
  # If the tests still pass easily, no worries, and this is great.
  # If the tests take too long, then yes, this correction must be made
  mutation_rate <- alignment_params$mutation_rate

  # Will increase the RNG seed up until
  # a twin alignment is found
  rng_seed <- twinning_params$rng_seed_alignment

  # Prepare for the search for the twin alignment
  n_mutations_twin <- -1 # nonsense value
  twin_alignment <- NA

  while (n_mutations_true != n_mutations_twin) {
    twin_alignment <- sim_alignment_raw(
      phylogeny = twin_phylogeny,
      root_sequence = alignment_params$root_sequence,
      rng_seed = rng_seed,
      mutation_rate = mutation_rate,
      site_model = alignment_params$site_model
    )
    n_mutations_twin <- count_n_mutations(
      alignment = twin_alignment,
      root_sequence = alignment_params$root_sequence
    )

    rng_seed <- rng_seed + 1
  }
  twin_alignment
}
