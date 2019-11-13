#' Measure the error BEAST2 makes from a true phylogeny.
#'
#' The supplied phylogeny (\code{phylogeny}) is the true/known phylogeny.
#' From the phylogeny, already an alignment is simulated and saved
#' as a FASTA file with name \code{alignment_params$fasta_filename}.
#'
#' The posterior phylogenies are compared to the true/known phylogeny
#' using the nLTT statistics. These nLTT statistics, all with values
#' betweem (including) zero and (including) one, are returned.
#' @return a numerical vector of error values
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
#'
#' # 'phylo_to_errors' expects an alignment file to be present
#' alignment_params <- create_test_alignment_params()
#'
#' # Create the alignment
#' create_true_alignment_file(
#'   phylogeny = phylogeny,
#'   alignment_params = alignment_params
#' )
#' expect_true(file.exists(alignment_params$fasta_filename))
#'
#' experiment <- create_test_gen_experiment()
#'
#' # A normal user should never need to initialize the experiment,
#' # as this is done by 'pir_run'.
#' # A develop, however, that wants to call 'phylo_to_errors',
#' # should initialaze as such
#' experiment <- init_experiment(
#'   experiment = experiment,
#'   alignment_params = alignment_params
#' )
#'
#' experiments <- list(experiment)
#'
#' if (rappdirs::app_dir()$os != "win" &&
#'   is_on_ci() && is_beast2_installed()
#' ) {
#'   nltts <- phylo_to_errors(
#'     phylogeny = phylogeny,
#'     alignment_params = alignment_params,
#'     experiment = experiment
#'   )
#'
#'   expect_true(is.numeric(nltts))
#'   expect_true(length(nltts) > 0)
#'   expect_true(all(nltts > 0) & all(nltts < 1))
#' }
#' @export
phylo_to_errors <- function(
  phylogeny,
  alignment_params,
  experiment,
  error_measure_params = create_error_measure_params(),
  verbose = FALSE
) {
  testit::assert(experiment$beast2_options$overwrite == TRUE)
  # Run
  trees <- alignment_params_to_posterior_trees(
    alignment_params = alignment_params,
    experiment = experiment,
    verbose = verbose
  )

  # Check the number of trees
  mcmc <- experiment$inference_model$mcmc
  testit::assert(!beautier::is_nested_sampling_mcmc(mcmc))
  if (mcmc$store_every != -1) {
    expected_n_trees <- 1 + (mcmc$chain_length / mcmc$store_every)
    testit::assert(length(trees) == expected_n_trees)
  }

  # Measure error by comparing true tree with BEAST2 posterior trees
  all_errors <- error_measure_params$error_function(phylogeny, trees)

  # Then remove the burn-in
  tracerer::remove_burn_in(
    trace = all_errors,
    burn_in_fraction = error_measure_params$burn_in_fraction
  )
}
