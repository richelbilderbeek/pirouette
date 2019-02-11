#' Measure the error BEAST2 makes from a true phylogeny.
#'
#' The supplied phylogeny (\code{phylogeny}) is the true/known phylogeny.
#' From the phylogeny, already an alignment is simulated and saved
#' as a FASTA file with name \code{alignment_params$fasta_filename}.
#'
#' The posterior phylogenies are compared to the true/known phylogeny
#' using the nLTT statistics. These nLTT statistics, all with values
#' betweem (including) zero and (including) one, are returned.
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @export
phylo_to_errors <- function(
  phylogeny,
  alignment_params,
  experiment = create_experiment(),
  error_measure_params = create_error_measure_params()
) {
  testit::assert(experiment$beast2_options$overwrite == TRUE)
  # Run
  trees <- alignment_params_to_posterior_trees(
    alignment_params = alignment_params,
    experiment = experiment
  )

  # Check the number of trees
  mcmc <- experiment$inference_model$mcmc
  testit::assert(!is_nested_sampling_mcmc(mcmc))
  if (mcmc$store_every != -1) {
    expected_n_trees <- 1 + (mcmc$chain_length / mcmc$store_every)
    if (length(trees) != expected_n_trees) {
      warning(
        "Number of trees differ between the expected number (",
        expected_n_trees, ") and the actual number (",
        length(trees), "). This is a repeated warning of Issue #99"
      )
    }
  }

  # Measure error by comparing true tree with BEAST2 posterior trees
  # Old version: nLTT::nltts_diff(tree = phylogeny, trees = trees)
  all_errors <- error_measure_params$error_function(phylogeny, trees)

  # Then remove the burn-in
  tracerer::remove_burn_in(
    trace = all_errors,
    burn_in_fraction = error_measure_params$burn_in_fraction
  )
}
