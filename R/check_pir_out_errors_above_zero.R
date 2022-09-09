#' Checks that \code{pir_out} errors all are more than zero
#' @inheritParams default_params_doc
#' @return nothing
#' @author Richèl J.C. Bilderbeek
#' @export
check_pir_out_errors_above_zero <- function(pir_out) { # nolint indeed long function name, which is fine for an internal function

  col_first_error <- which(colnames(pir_out) == "error_1")
  col_last_error <- ncol(pir_out)
  n_rows <- nrow(pir_out)
  for (i in seq(col_first_error, col_last_error)) {
    error_index <- 1 + i - col_first_error
    for (row_index in seq(1, n_rows)) {
      error <- pir_out[row_index, i]
      if (!beautier::is_one_double(error)) {
        stop("'error_", error_index, "[", row_index, "]' must be a number")
      }
      if (error < 0.0) {
        stop(
          "'error_", error_index, "[", row_index,
          "]' cannot be less than zero"
        )
      }
    }
  }
  invisible(pir_out)
}
