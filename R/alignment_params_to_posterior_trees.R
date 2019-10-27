#' Creates a posterior of phylogenies from a known alignment.
#'
#' Ignores the jointly-estimated posterior estimates
#' @inheritParams default_params_doc
#' @return a list of phylogenies in the posterior,
#'   as a \link[ape]{multiphylo}
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   library(testthat)
#'
#'   if (is_on_travis() && is_beast2_installed()) {
#'
#'     alignment_params <- create_test_alignment_params()
#'     create_alignment_file(
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
  experiment,
  verbose = FALSE
) {
  pirouette::check_alignment_params(alignment_params)
  pirouette::check_experiment(experiment)
  beautier::check_file_exists(alignment_params$fasta_filename)
  testit::assert(
    !beautier::is_nested_sampling_mcmc(experiment$inference_model$mcmc)
  )

  bbt_out <- babette::bbt_run_from_model(
    fasta_filename = alignment_params$fasta_filename,
    inference_model = experiment$inference_model,
    beast2_options = experiment$beast2_options
  )
  if (verbose) {
    print(
      paste0(
        "Saved BEAST2 input file to '",
        experiment$beast2_options$input_filename, "'"
      )
    )
    print(
      paste0(
        "Saved BEAST2 output log file to '",
        experiment$inference_model$mcmc$tracelog$filename, "'"
      )
    )
    print(
      paste0(
        "Saved BEAST2 output trees file to '",
        experiment$inference_model$mcmc$treelog$filename, "'"
      )
    )
    print(
      paste0(
        "Saved BEAST2 output state file to '",
        experiment$beast2_options$output_state_filename, "'"
      )
    )
  }
  beautier::check_file_exists(experiment$beast2_options$input_filename)
  beautier::check_file_exists(experiment$inference_model$mcmc$tracelog$filename)
  beautier::check_file_exists(experiment$inference_model$mcmc$treelog$filename)
  beautier::check_file_exists(experiment$beast2_options$output_state_filename)

  trees <- c(bbt_out[[grep(x = names(bbt_out), pattern = "trees")]])

  # Check the number of trees
  mcmc <- experiment$inference_model$mcmc
  testit::assert(!beautier::is_nested_sampling_mcmc(mcmc))
  if (mcmc$treelog$log_every != -1) {
    expected_n_trees <- 1 + (mcmc$chain_length / mcmc$treelog$log_every)
    if (length(trees) != expected_n_trees) {
      stop(
        "Mismatch between number of trees and expected number of trees. \n",
        "Number of trees (read from .trees file): ", length(trees), " \n",
        "Expected number of trees: ", expected_n_trees, " \n",
        ".trees filename: ",
          experiment$inference_model$mcmc$treelog$filename, " \n",
        "MCMC chain length: ", mcmc$chain_length, " \n",
        "MCMC treelog$log_every: ", mcmc$treelog$log_every, " \n",
        "Maybe .trees file of older experiment is used?"
      )
    }
  }

  trees
}
