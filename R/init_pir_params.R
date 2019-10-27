#' Initialize the \code{pir_params}.
#'
#' A normal user should never need to call this function.
#' @export
init_pir_params <- function(pir_params) {

  for (i in seq_along(pir_params$experiments)) {
    # A tracelog's filename is set to NA by default.
    # Here, do what BEAUti does...
    tracelog_filename <- pir_params$experiments[[i]]$inference_model$mcmc$tracelog$filename # nolint sorry Demeter
    if (is.na(tracelog_filename)) {
      pir_params$experiments[[i]]$inference_model$mcmc$tracelog$filename <-
      paste0(
        beautier::get_alignment_id(pir_params$alignment_params$fasta_filename),
        ".log"
      )
    }

    # BEAUti offers the '$(tree)' shorthand notation.
    # Here, do what BEAUti does...
    treelog_filename <- pir_params$experiments[[i]]$inference_model$mcmc$treelog$filename # nolint sorry Demeter

    new_treelog_filename <- gsub(
      x = treelog_filename,
      pattern = "\\$\\(tree\\)",
      replacement = beautier::get_alignment_id(
        pir_params$alignment_params$fasta_filename
      )
    )
    pir_params$experiments[[i]]$inference_model$mcmc$treelog$filename <- new_treelog_filename # nolint sorry Demeter
  }
  pir_params
}
