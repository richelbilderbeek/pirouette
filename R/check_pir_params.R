#' Checks if the argument is a valid \link{pirouette} parameter set.
#'
#' Will \link{stop} if not.
#' A valid \link{pirouette} parameter set
#' can be created by \link{create_pir_params}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' expect_silent(check_pir_params(create_test_pir_params()))
#' expect_error(check_pir_params("nonsense"))
#' expect_error(check_pir_params(NULL))
#' expect_error(check_pir_params(NA))
#' @export
check_pir_params <- function(
  pir_params
) {
  pirouette::check_pir_params_names(pir_params)
  pirouette::check_pir_params_data_types(pir_params)
  filename <- pir_params$evidence_filename
  file_extenstion <- substr(
    basename(filename),
    nchar(basename(filename)) - 3,
    nchar(basename(filename))
  )
  if (file_extenstion != ".csv") {
    stop("'evidence_filename' must be a csv filename")
  }
}

#' Checks if the \code{pir_params} has all the named elements needed
#'
#' Will \link{stop} if not.
#' A valid \link{pirouette} parameter set
#' can be created by \link{create_pir_params}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @author Richèl J.C. Bilderbeek
#' @export
check_pir_params_names <- function(pir_params) {
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
}

#' Checks if the \code{pir_params} elements are all of the right data type.
#'
#' Will \link{stop} if not.
#' A valid \link{pirouette} parameter set
#' can be created by \link{create_pir_params}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @author Richèl J.C. Bilderbeek
#' @export
check_pir_params_data_types <- function(pir_params) {
  tryCatch(
    pirouette::check_alignment_params(pir_params$alignment_params),
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
    pirouette::check_error_measure_params(pir_params$error_measure_params),
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
    pirouette::check_experiments(pir_params$experiments),
    error = function(e) {
      msg <- paste0(
        "'experiments' must be one experiment or a list of one or more ",
        "experiments. \n",
        "Tip: use a list of 'create_experiment'. \n",
        "Error message: ", e$message, " \n",
        "Actual value: ", pir_params$experiments
      )
      stop(msg)
    }
  )
  if (!is.character(pir_params$evidence_filename)) {
    stop("'evidence_filename' must be a string")
  }
  if (!beautier::is_one_bool(pir_params$verbose)) {
    stop("'verbose' must be one boolean")
  }
}
