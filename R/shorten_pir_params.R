#' Shorten the \code{pir_params}
#' @inheritParams default_params_doc
#' @return a `pir_params`
#' @export
shorten_pir_params <- function(pir_params) {
  pirouette::check_pir_params(pir_params)
  pir_params$experiments <- pirouette::shorten_experiments(
    pir_params$experiments
  )
  pir_params
}
