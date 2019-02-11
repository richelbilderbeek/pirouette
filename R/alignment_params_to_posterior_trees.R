#' Creates a posterior of phylogenies from a known alignment.
#'
#' @inheritParams default_params_doc
#' @return a list of:
#' \itemize{
#'   \item \code{trees}: the phylogenies in the posterior,
#'     as a \link[ape]{multiphylo}
#'   \item \code{estimates}: the BEAST2 estimates, as a \link{data.frame}
#' }
#' @author Richel J.C. Bilderbeek
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
  testit::assert(!is_nested_sampling_mcmc(mcmc))
  if (mcmc$store_every != -1) {
    expected_n_trees <- 1 + (mcmc$chain_length / mcmc$store_every)
    if (length(trees) != expected_n_trees) {
      warning(
        "Number of trees differ between the expected number (",
        expected_n_trees, ") and the actual number (",
        length(trees), "). This is Issue #99"
      )
    }
  }

  trees
}
