#' Rename the \code{pir_params} filenames to follow a standard naming scheme.
#'
#' By default, \code{pir_params} uses temporary filenames for all files.
#' For \link{pir_run}, when only a computer reads those filenames,
#' this is fine. This function conformizes the filenames to
#' a human-friendly form.
#'
#' The standard naming scheme is this:
#' \itemize{
#'   \item \code{pir_params$alignment_params$fasta_filename}
#'     becomes \code{[folder_name]/alignment.fas}
#' }
#'
#' For the (zero or one) experiment at index \code{i}
#' that is generative:
#'
#' \itemize{
#'   \item \code{pir_params$experiments[[i]]$beast2_options$input_filename}
#'     becomes \code{[folder_name]/gen.xml}
#'   \item
#'     \code{pir_params$experiments[[i]]$beast2_options$output_state_filename}
#'     becomes \code{[folder_name]/gen.xml.state}
#'   \item
#'     \code{pir_params$experiments[[i]]$beast2_options$input_filename}
#'     becomes \code{[folder_name]/gen_errors.csv},
#'   \item
#'     \code{pir_params$experiments[[i]]$inference_model$mcmc$tracelog$filename}
#'     becomes \code{[folder_name]/gen.log}
#'   \item
#'     \code{pir_params$experiments[[i]]$inference_model$mcmc$screenlog$filename}
#'     becomes \code{[folder_name]/gen.csv}
#'   \item
#'     \code{pir_params$experiments[[i]]$inference_model$mcmc$treelog$filename}
#'     becomes \code{[folder_name]/gen.trees}
#'   \item
#'     \code{pir_params$experiments[[i]]$est_evidence_mcmc$tracelog$filename}
#'     becomes \code{[folder_name]/gen_evidence.log}
#'   \item
#'     \code{pir_params$experiments[[i]]$est_evidence_mcmc$screenlog$filename}
#'     becomes \code{[folder_name]/gen_evidence.csv}
#'   \item
#'     \code{pir_params$experiments[[i]]$est_evidence_mcmc$treelog$filename}
#'     becomes \code{[folder_name]/gen_evidence.trees}
#' }
#' @inheritParams default_params_doc
#' @seealso Use \link{get_pir_params_filenames} to obtain all the filenames
#' @export
pir_rename_to_std <- function(
  pir_params,
  folder_name
) {
  pirouette::check_pir_params(pir_params)

  pir_params$alignment_params$fasta_filename <-
    file.path(folder_name, "alignment.fas")

  for (i in seq_along(pir_params$experiments)) {
    if (pir_params$experiments[[i]]$inference_conditions$model_type ==
      "generative"
    ) {
      pir_params$experiments[[i]]$beast2_options$input_filename <-
        file.path(folder_name, "gen.xml")
      pir_params$experiments[[i]]$beast2_options$output_state_filename <-
        file.path(folder_name, "gen.xml.state")
      pir_params$experiments[[i]]$errors_filename <-
        file.path(folder_name, "gen_errors.csv")
      pir_params$experiments[[i]]$inference_model$mcmc$tracelog$filename <-
        file.path(folder_name, "gen.log")
      pir_params$experiments[[i]]$inference_model$mcmc$screenlog$filename <-
        file.path(folder_name, "gen.csv")
      pir_params$experiments[[i]]$inference_model$mcmc$treelog$filename <-
        file.path(folder_name, "gen.trees")
      pir_params$experiments[[i]]$est_evidence_mcmc$tracelog$filename <-
        file.path(folder_name, "gen_evidence.log")
      pir_params$experiments[[i]]$est_evidence_mcmc$screenlog$filename <-
        file.path(folder_name, "gen_evidence.csv")
      pir_params$experiments[[i]]$est_evidence_mcmc$treelog$filename <-
        file.path(folder_name, "gen_evidence.trees")
    } else {
      testthat::expect_equal(
        pir_params$experiments[[i]]$inference_conditions$model_type,
        "candidate"
      )
      pir_params$experiments[[i]]$beast2_options$input_filename <-
        file.path(folder_name, "best.xml")
      pir_params$experiments[[i]]$beast2_options$output_state_filename <-
        file.path(folder_name, "best.xml.state")
      pir_params$experiments[[i]]$errors_filename <-
        file.path(folder_name, "best_errors.csv")
      pir_params$experiments[[i]]$inference_model$mcmc$tracelog$filename <-
        file.path(folder_name, "best.log")
      pir_params$experiments[[i]]$inference_model$mcmc$screenlog$filename <-
        file.path(folder_name, "best.csv")
      pir_params$experiments[[i]]$inference_model$mcmc$treelog$filename <-
        file.path(folder_name, "best.trees")
      pir_params$experiments[[i]]$est_evidence_mcmc$tracelog$filename <-
        file.path(folder_name, "best_evidence.log")
      pir_params$experiments[[i]]$est_evidence_mcmc$screenlog$filename <-
        file.path(folder_name, "best_evidence.csv")
      pir_params$experiments[[i]]$est_evidence_mcmc$treelog$filename <-
        file.path(folder_name, "best_evidence.trees")
    }
  }
  pir_params
}
