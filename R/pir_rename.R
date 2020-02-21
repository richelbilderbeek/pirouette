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
    pir_params$experiments[[i]]$inference_model <-
      beautier::rename_inference_model_files(
        pir_params$experiments[[i]]$inference_model,
        rename_fun = rename_fun
      )

    # experiments' BEAST2 options
    pir_params$experiments[[i]]$beast2_options <-
      beastier::rename_beast2_options_filenames(
        beast2_options = pir_params$experiments[[i]]$beast2_options,
        rename_fun = rename_fun
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

  # Twinning parameters
  if (!beautier::is_one_na(pir_params$twinning_params)) {
    pir_params$twinning_params$twin_tree_filename <-
      rename_fun(pir_params$twinning_params$twin_tree_filename)
    pir_params$twinning_params$twin_alignment_filename <-
      rename_fun(pir_params$twinning_params$twin_alignment_filename)
    pir_params$twinning_params$twin_evidence_filename <-
      rename_fun(pir_params$twinning_params$twin_evidence_filename)
  }

  pir_params
}
