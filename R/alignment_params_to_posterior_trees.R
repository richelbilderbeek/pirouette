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
  inference_params,
  experiment = create_experiment()
) {
  check_alignment_params(alignment_params) # nolint pirouette function
  check_experiment(experiment) # nolint pirouette function

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

  bbt_out <- babette::bbt_run_from_model(
    fasta_filename = alignment_params$fasta_filename,
    inference_model = experiment$inference_model,
    beast2_options = experiment$beast2_options
  )
  c(bbt_out[[grep(x = names(bbt_out), pattern = "trees")]])
}
