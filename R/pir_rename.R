#' Rename the filenames in a \code{pir_params}
#' using a rename function.
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @export
pir_rename <- function(
  pir_params,
  rename_fun
) {
  pirouette::check_pir_params(pir_params)
  pirouette::check_rename_fun(rename_fun)

  # alignment params
  pir_params$alignment_params$fasta_filename <-
    rename_fun(pir_params$alignment_params$fasta_filename)

  # experiments
  for (i in seq_along(pir_params$experiments)) {
    # experiments' inference models
    pir_params$experiments[[i]]$inference_model$mcmc$tracelog$filename <-
      rename_fun(
        pir_params$experiments[[i]]$inference_model$mcmc$tracelog$filename
      )
    pir_params$experiments[[i]]$inference_model$mcmc$screenlog$filename <-
      rename_fun(
        pir_params$experiments[[i]]$inference_model$mcmc$screenlog$filename
      )
    pir_params$experiments[[i]]$inference_model$mcmc$treelog$filename <-
      rename_fun(
        pir_params$experiments[[i]]$inference_model$mcmc$treelog$filename
      )
    pir_params$experiments[[i]]$inference_model$tipdates_filename <-
      rename_fun(
        pir_params$experiments[[i]]$inference_model$tipdates_filename
      )
    # experiments' BEAST2 options
    pir_params$experiments[[i]]$beast2_options$input_filename <-
      rename_fun(
        pir_params$experiments[[i]]$beast2_options$input_filename
      )
    pir_params$experiments[[i]]$beast2_options$output_state_filename <-
      rename_fun(
        pir_params$experiments[[i]]$beast2_options$output_state_filename
      )
    # experiments estimate evidence MCMC
    pir_params$experiments[[i]]$est_evidence_mcmc$tracelog$filename <-
      rename_fun(
        pir_params$experiments[[i]]$est_evidence_mcmc$tracelog$filename
      )
    pir_params$experiments[[i]]$est_evidence_mcmc$screenlog$filename <-
      rename_fun(
        pir_params$experiments[[i]]$est_evidence_mcmc$screenlog$filename
      )
    pir_params$experiments[[i]]$est_evidence_mcmc$treelog$filename <-
      rename_fun(
        pir_params$experiments[[i]]$est_evidence_mcmc$treelog$filename
      )
    # experiments' error
    pir_params$experiments[[i]]$errors_filename <-
      rename_fun(
        pir_params$experiments[[i]]$errors_filename
      )
  }

  # evidence filename
  pir_params$evidence_filename <-
    rename_fun(
      pir_params$evidence_filename
    )

  pir_params
}
