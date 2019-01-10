#' Creates a posterior from an alignment
#' @inheritParams default_params_doc
#' @return a list of:
#' \itemize{
#'   \item \code{trees}: the phylogenies in the posterior,
#'     as a \link[ape]{multiphylo}
#'   \item \code{estimates}: the BEAST2 estimates, as a \link{data.frame}
#' }
#' @author Richel J.C. Bilderbeek
alignment_params_to_posterior_trees <- function(
  alignment_params,
  site_model,
  clock_model,
  tree_prior,
  inference_params
) {
  check_alignment_params(alignment_params)
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
  testit::assert(file.exists(alignment_params$fasta_filename))
  babette_out <- babette::bbt_run(
    fasta_filename = alignment_params$fasta_filename,
    site_model = site_model,
    clock_model = clock_model,
    tree_prior = tree_prior,
    mrca_prior = inference_params$mrca_prior,
    mcmc = inference_params$mcmc,
    rng_seed = inference_params$rng_seed,
    verbose = inference_params$verbose,
    beast2_path = inference_params$beast2_path
  )

  c(babette_out[[grep(x = names(babette_out), pattern = "trees")]])
}
