#' Initialize the \code{experiment}.
#'
#' A normal user should never need to call this function.
#'
#' It does the following:
#' \itemize{
#'   \item if an MCMC's treelog filename is \code{$(tree).trees},
#'     replace it to \code{[alignment_folder]/[alignment_id].trees}.
#'   \item if an MCMC's tracelog filename is \link{NA},
#'     replace it to \code{[alignment_folder]/[alignment_id].log}
#' }
#' Both is done for the regular MCMC in \code{experiment$inference_model} and in
#' \code{experiment$est_evidence_mcmc}.
#'
#' \code{[alignment_id]} is obtained by using
#' \link{get_alignment_id} on the \code{alignment_params$fasta_filename}.
#' \code{[alignment_folder]} is obtained by using \link{dirname}
#' on \code{alignment_params$fasta_filename}
#' @inheritParams default_params_doc
#' @export
init_experiment <- function(
  experiment,
  alignment_params
) {
  alignment_folder <- dirname(alignment_params$fasta_filename)
  ##############################################################################
  # The regular MCMC
  ##############################################################################
  # A tracelog's filename is set to NA by default.
  # Here, do what BEAUti does...
  tracelog_filename <- experiment$inference_model$mcmc$tracelog$filename
  if (is.na(tracelog_filename)) {
    experiment$inference_model$mcmc$tracelog$filename <- file.path(
      alignment_folder,
      paste0(
        beautier::get_alignment_id(alignment_params$fasta_filename),
        ".log"
      )
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
  experiment$inference_model$mcmc$treelog$filename <- file.path(
    alignment_folder,
    new_treelog_filename
  )

  ##############################################################################
  # The marginal likelihood MCMC
  ##############################################################################
  # A tracelog's filename is set to NA by default.
  # Here, do what BEAUti does...
  tracelog_filename <- experiment$est_evidence_mcmc$tracelog$filename
  if (is.na(tracelog_filename)) {
    experiment$est_evidence_mcmc$tracelog$filename <- file.path(
      alignment_folder,
      paste0(
        beautier::get_alignment_id(alignment_params$fasta_filename),
        ".log"
      )
    )
  }

  # BEAUti offers the '$(tree)' shorthand notation.
  # Here, do what BEAUti does...
  treelog_filename <- experiment$est_evidence_mcmc$treelog$filename

  new_treelog_filename <- gsub(
    x = treelog_filename,
    pattern = "\\$\\(tree\\)",
    replacement = beautier::get_alignment_id(
      alignment_params$fasta_filename
    )
  )
  experiment$est_evidence_mcmc$treelog$filename <- file.path(
    alignment_folder,
    new_treelog_filename
  )

  experiment
}
