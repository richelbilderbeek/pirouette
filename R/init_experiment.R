#' Initialize the \code{experiment}.
#'
#' A normal user should never need to call this function.
#' @inheritParams default_params_doc
#' @export
init_experiment <- function(
  experiment,
  alignment_params
) {

  # A tracelog's filename is set to NA by default.
  # Here, do what BEAUti does...
  tracelog_filename <- experiment$inference_model$mcmc$tracelog$filename
  if (is.na(tracelog_filename)) {
    experiment$inference_model$mcmc$tracelog$filename <-
    paste0(
      beautier::get_alignment_id(alignment_params$fasta_filename),
      ".log"
    )
  }

  # BEAUti offers the '$(tree)' shorthand notation.
  # Here, do what BEAUti does...
  treelog_filename <- experiment$inference_model$mcmc$treelog$filename

  new_treelog_filename <- gsub(
    x = treelog_filename,
    pattern = "\\$\\(tree\\)",
    replacement = beautier::get_alignment_id(
      alignment_params$fasta_filename
    )
  )
  experiment$inference_model$mcmc$treelog$filename <- new_treelog_filename

  experiment
}
