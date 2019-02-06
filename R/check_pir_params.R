#' Checks if the argument is a valid \link{pirouette} parameter set.
#'
#' Will \link{stop} if not.
#' A valid \link{pirouette} parameter set
#' can be created by \link{create_pir_params}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @examples
#'  phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
#'  pir_params <- create_pir_params(
#'  alignment_params = create_alignment_params(
#'      root_sequence = create_mono_nuc_dna(length = 4),
#'      mutation_rate = create_standard_mutation_rate(phylogeny)
#'    )
#'  )
#'  testthat::expect_silent(check_pir_params(pir_params))
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
check_pir_params <- function(
  pir_params,
  max_evidence_epsilon = 1e-4
) {
  if (length(pir_params$model_select_params) != 314) { # nolint use new interface
    stop("deprecated, #90")
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
    check_inference_params(pir_params$inference_params), # nolint pirouette function
    error = function(e) {
      msg <- paste0(
        "'inference_params' must be a set of inference parameters.\n",
        "Tip: use 'create_inference_params'\n",
        "Error message: ", e$message, "\n",
        "Actual value: ", pir_params$inference_params
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
        "Actual value: ", pir_params$model_select_params
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
  if (!is.numeric(pir_params$evidence_epsilon)) {
    stop("'evidence_epsilon' must be numeric")
  }
  if (pir_params$evidence_epsilon > max_evidence_epsilon) {
    stop(
      paste0(
        "'evidence_epsilon' must be not greater than", max_evidence_epsilon
      )
    )
  }
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
    stop("'evidence_filename' must be a csv file")
  }
}
