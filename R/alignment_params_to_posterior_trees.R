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
  inference_model,
  inference_params,
  experiment = create_experiment()
) {
  check_alignment_params(alignment_params) # nolint pirouette function
  check_old_skool_inference_model(inference_model) # nolint pirouette function
  tryCatch(
    check_inference_params(inference_params),
    error = function(msg) {
      msg <- paste0(
        "'inference_params' must be a set of inference parameters. ",
        msg
      )
      stop(msg)
    }
  )
  check_experiment(experiment) # nolint pirouette function

  testit::assert(file.exists(alignment_params$fasta_filename))
  babette_out <- babette::bbt_run(
    fasta_filename = alignment_params$fasta_filename,
    site_model = inference_model$site_model,
    clock_model = inference_model$clock_model,
    tree_prior = inference_model$tree_prior,
    mrca_prior = inference_params$mrca_prior,
    mcmc = inference_params$mcmc,
    rng_seed = inference_params$rng_seed,
    verbose = inference_params$verbose,
    beast2_input_filename = inference_model$beast2_input_filename,
    beast2_output_log_filename = inference_model$beast2_output_log_filename,
    beast2_output_trees_filenames =
      inference_model$beast2_output_trees_filename,
    beast2_output_state_filename = inference_model$beast2_output_state_filename,
    beast2_path = inference_params$beast2_path
  )

  c(babette_out[[grep(x = names(babette_out), pattern = "trees")]])
}
