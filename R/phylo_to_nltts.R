#' Create the NLTT statistics distribution files from a given phylogeny.
#' Additional information about mcmc and number of base pairs are needed
#' @inheritParams default_params_doc
#' @return nltt statistics
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
phylo_to_nltts <- function(
  phylogeny,
  mcmc,
  n_base_pairs,
  seed
) {
  if (class(phylogeny) != "phylo") {
    stop("parameter 'phylogeny' must be a phylogeny")
  }
  if (!is.numeric(seed)) {
    stop("'seed' must be a number")
  }
  posterior <- phylo_to_posterior(
    phylogeny = phylogeny,
    alignment_params = create_alignment_params(
      root_sequence = create_blocked_dna(length = n_base_pairs),
      mutation_rate = 0.1,
      rng_seed = seed
    ),
    inference_params = create_inference_params(
      mcmc = mcmc,
      rng_seed = seed
    )
  )
  nltt_diffs <- create_nltts(phylogeny = phylogeny, posterior = posterior)
  nltt_diffs
}
