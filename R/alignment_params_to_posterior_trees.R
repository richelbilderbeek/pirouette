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
alignment_params_to_posterior_trees <- function( # nolint indeed a long name
  alignment_params,
  site_model,
  clock_model,
  tree_prior,
  inference_param,
  beast2_input_filename = tempfile(pattern = "beast2_", fileext = ".xml"),
  beast2_output_log_filename = tempfile(
    pattern = "beast2_", fileext = "log"
  ),
  beast2_output_trees_filenames = tempfile(
    pattern = paste0(
      "beast2_",
      beautier::get_alignment_ids(fasta_filename), "_"
    ),
    fileext = ".trees"
  ),
  beast2_output_state_filename = tempfile(
    pattern = "beast2_", fileext = ".xml.state"
  )
) {
  check_alignment_params(alignment_params)
  tryCatch(
    check_inference_param(inference_param),
    error = function(msg) {
      msg <- paste0(
        "'inference_param' must be a set of inference parameters. ",
        msg
      )
      stop(msg)
    }
  )
  testit::assert(file.exists(alignment_params$fasta_filename))
  babette_out <- babette::bbt_run(
    fasta_filename = alignment_params$fasta_filename,
    site_model = site_model,
    clock_model = clock_model,
    tree_prior = tree_prior,
    mrca_prior = inference_param$mrca_prior,
    mcmc = inference_param$mcmc,
    rng_seed = inference_param$rng_seed,
    verbose = inference_param$verbose,
    beast2_input_filename = beast2_input_filename,
    beast2_output_log_filename = beast2_output_log_filename,
    beast2_output_trees_filenames = beast2_output_trees_filenames,
    beast2_output_state_filename = beast2_output_state_filename,
    beast2_path = inference_param$beast2_path
  )

  c(babette_out[[grep(x = names(babette_out), pattern = "trees")]])
}
