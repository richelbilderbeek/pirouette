#' Creates a posterior of phylogenies from a known alignment.
#'
#' Ignores the jointly-estimated posterior estimates
#' @inheritParams default_params_doc
#' @return a list of phylogenies in the posterior,
#'   as a \link[ape]{multiphylo}
#' @author Richel J.C. Bilderbeek
#' @examples
#'   library(testthat)
#'
#'   if (beastier::is_on_travis() && beastier::is_beast2_installed()) {
#'
#'     alignment_params <- create_test_alignment_params()
#'     sim_alignment_file(
#'       phylogeny = ape::read.tree(text = "((A:1, B:1):1, C:2);"),
#'       alignment_params = alignment_params
#'     )
#'
#'     expect_true(file.exists(alignment_params$fasta_filename))
#'
#'     trees <- alignment_params_to_posterior_trees(
#'       alignment_params = alignment_params,
#'       experiment = create_test_experiment()
#'     )
#'     expect_equal("multiPhylo", class(trees))
#'  }
#' @noRd
alignment_params_to_posterior_trees <- function(# nolint indeed a long name
  alignment_params,
  experiment = create_experiment()
) {
  check_alignment_params(alignment_params) # nolint pirouette function
  check_experiment(experiment) # nolint pirouette function
  testit::assert(file.exists(alignment_params$fasta_filename))
  testit::assert(
    !beautier::is_nested_sampling_mcmc(experiment$inference_model$mcmc)
  )

  bbt_out <- babette::bbt_run_from_model(
    fasta_filename = alignment_params$fasta_filename,
    inference_model = experiment$inference_model,
    beast2_options = experiment$beast2_options
  )

  trees <- c(bbt_out[[grep(x = names(bbt_out), pattern = "trees")]])

  # Check the number of trees
  mcmc <- experiment$inference_model$mcmc
  testit::assert(!beautier::is_nested_sampling_mcmc(mcmc))
  if (mcmc$store_every != -1) {
    expected_n_trees <- 1 + (mcmc$chain_length / mcmc$store_every)
    testit::assert(length(trees) == expected_n_trees)
  }

  trees
}
