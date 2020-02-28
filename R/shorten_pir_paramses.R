#' Shorten the list of \code{pir_params}
#' @inheritParams default_params_doc
#' @export
shorten_pir_paramses <- function(pir_paramses) {
  pirouette::check_pir_paramses(pir_paramses)
  for (i in seq_along(pir_paramses)) {
    pir_paramses[[i]] <- pirouette::shorten_pir_params(pir_paramses[[i]])
  }
  pir_paramses
}
