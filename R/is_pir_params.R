#' Determine if the \code{pir_params} is valid.
#' @inheritParams default_params_doc
#' @return a boolean
#' @export
is_pir_params <- function(pir_params, verbose = FALSE
) {
  is_valid <- FALSE
  tryCatch({
      pirouette::check_pir_params(pir_params)
      is_valid <- TRUE
    },
    error = function(e) {
      if (verbose) {
        message(e$message)
      }
    }
  )
  is_valid
}
