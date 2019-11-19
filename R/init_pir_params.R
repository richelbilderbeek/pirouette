#' Initialize the \code{pir_params}.
#'
#' A normal user should never need to call this function.
#' @inheritParams default_params_doc
#' @export
init_pir_params <- function(pir_params) {

  for (i in seq_along(pir_params$experiments)) {
    pir_params$experiments[[i]] <- pirouette::init_experiment(
      experiment = pir_params$experiments[[i]],
      alignment_params = pir_params$alignment_params
    )
  }
  pir_params
}
