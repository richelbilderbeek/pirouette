#' Create the twin parameters for \code{pirouette}
#' @inheritParams default_params_doc
#' @return a list with all \code{pirouette} twin parameters
#' @export
#' @author Giovanni Laudanno
create_pir_params_twin <- function(
  pir_params,
  pir_out
) {
  pir_params_twin <- pir_params
  pir_params_twin$alignment_params$fasta_filename <-
    pir_params$twinning_params$twin_alignment_filename
  pir_params_twin$evidence_filename <-
    pir_params$twinning_params$twin_evidence_filename
  for (i in seq_along(pir_params$experiments)) {
    filenames <- pir_params$experiments[[i]]$beast2_options[
      grepl(
        "filename",
        names(pir_params$experiments[[i]]$beast2_options)
      )
      ]
    for (ii in seq_along(filenames)) {
      pir_params_twin$experiments[[i]]$beast2_options[ii] <-
        to_twin_filename(filenames[ii]) # nolint pirouette function
    }
  }
  if (pir_params$twinning_params$rng_seed == "same_seed") {
    pir_params_twin$twinning_params$rng_seed <-
      pir_params$alignment_params$rng_seed
  }
  if (pir_params$twinning_params$twin_model == "import_from_main_pipeline") {
    if (pir_out$tree_prior %in% get_twin_models()) {
      pir_params_twin$twinning_params$twin_model <- pir_out$tree_prior
    } else {
      pir_params_twin$twinning_params$twin_model <- "bd"
    }
  }
  check_pir_params(pir_params_twin) # nolint pirouette function
  pir_params_twin
}
