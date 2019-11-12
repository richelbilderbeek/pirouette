#' Check if the \code{pir_params} is initialized
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @export
check_init_pir_params <- function(pir_params) {
  pirouette::check_pir_params(pir_params)

  for (i in seq_along(pir_params$experiments)) {
    experiment <- pir_params$experiments[[i]]
    mcmc <- experiment$inference_model$mcmc
    if (is.na(mcmc$tracelog$filename)) {
      stop(
        "pir_params$experiments[[", i, "]]$inference_model$mcmc$tracelog$",
        "filename is NA. \n",
        "This should never happen to a normal user. \n",
        "Tip: initialize the 'pir_params' with 'init_pir_pirams'"
      )
    }
    if (mcmc$treelog$filename == "$(tree).trees") {
      stop(
        "pir_params$experiments[[", i, "]]$inference_model$mcmc$treelog$",
        "filename is '$(tree).trees'. \n",
        "This should never happen to a normal user. \n",
        "Tip: initialize the 'pir_params' with 'init_pir_pirams'"
      )
    }
  }
}
