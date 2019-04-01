#' Checks if the argument is a valid \link{pirouette} parameter set.
#'
#' Will \link{stop} if not.
#' A valid \link{pirouette} parameter set
#' can be created by \link{create_pir_params}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#'   library(testthat)
#'
#'   expect_silent(check_pir_params(create_test_pir_params()))
#'   expect_error(check_pir_params("nonsense"))
#'   expect_error(check_pir_params(NULL))
#'   expect_error(check_pir_params(NA))
#' @export
check_pir_params <- function(
  pir_params
) {
  argument_names <- c(
    "alignment_params",
    "twinning_params",
    "experiments",
    "error_measure_params",
    "evidence_filename",
    "verbose"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(pir_params)) {
      stop(
        "'", arg_name, "' must be an element of an 'pir_params'.\n",
        "Tip: use 'create_pir_params'"
      )
    }
  }

  tryCatch(
    check_alignment_params(pir_params$alignment_params), # nolint pirouette function
    error = function(e) {
      msg <- paste0(
        "'alignment_params' must be a set of alignment parameters.\n",
        "Tip: use 'create_alignment_params'\n",
        "Error message: ", e$message, "\n",
        "Actual value: ", pir_params$alignment_params
      )
      stop(msg)
    }
  )
  tryCatch(
    check_error_measure_params(pir_params$error_measure_params), # nolint pirouette function
    error = function(e) {
      msg <- paste0(
        "'error_measure_params' must be a set of error measurement ",
        "parameters.\n",
        "Tip: use 'create_error_measure_params'\n",
        "Error message: ", e$message, "\n",
        "Actual value: ", pir_params$error_measure_params
      )
      stop(msg)
    }
  )
  tryCatch(
    check_experiments(pir_params$experiments), # nolint pirouette function
    error = function(e) {
      msg <- paste0(
        "'experiments' must be one experiment or a list of one or more ",
        "experiments.\n",
        "Tip: use 'create_experiments'\n",
        "Error message: ", e$message, "\n",
        "Actual value: ", pir_params$experiments
      )
      stop(msg)
    }
  )
  filename <- pir_params$evidence_filename
  if (!is.character(pir_params$evidence_filename)) {
    stop("'evidence_filename' must be a string")
  }
  file_extenstion <- substr(
    basename(filename),
    nchar(basename(filename)) - 3,
    nchar(basename(filename))
  )
  if (file_extenstion != ".csv") {
    stop("'evidence_filename' must be a csv filename")
  }
  if (length(pir_params$verbose) != 1 ||
    is.na(pir_params$verbose) ||
    !is.logical(pir_params$verbose)) {
    stop("'verbose' must be one boolean")
  }
}
