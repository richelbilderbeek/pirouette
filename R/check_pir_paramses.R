#' Check if all elements in the list of \code{pir_params} are valid.
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @export
check_pir_paramses <- function(pir_paramses) {
  if (!is.list(pir_paramses)) {
    stop("'pir_paramses' must be a list. Actual class: ", class(pir_paramses))
  }
  for (i in seq_along(pir_paramses)) {
    tryCatch(
      check_pir_params(pir_paramses[[i]]),
      error = function(e) {
        stop(
          "Element #", i, " is not a valid pir_params. ",
          "Error message: ", e$message, ". ",
          "Value: '", pir_paramses[[i]], "'"
        )
      }
    )
  }
}
