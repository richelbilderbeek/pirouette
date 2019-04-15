#' Checks if the argument is a valid \link{pirouette} parameter set.
#'
#' Will \link{stop} if not.
#' A valid \link{pirouette} parameter set
#' can be created by \link{create_pir_out}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   library(testthat)
#'
#'   expect_silent(check_pir_out(create_test_pir_run_output()))
#'   expect_error(check_pir_out("nonsense"))
#'   expect_error(check_pir_out(NULL))
#'   expect_error(check_pir_out(NA))
#' @export
check_pir_out <- function(
  pir_out
) {
  argument_names <- c(
    "tree", "inference_model", "inference_model_weight", "site_model",
    "clock_model", "tree_prior", "error_1"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(pir_out)) {
      stop(
        "'", arg_name, "' must be an element of an 'pir_out'.\n",
        "Tip: use 'create_pir_out'"
      )
    }
  }
  testit::assert(is.factor(pir_out$tree))
  testit::assert(is.factor(pir_out$inference_model))
  testit::assert(is.factor(pir_out$site_model))
  testit::assert(!is.factor(pir_out$inference_model_weight))
  testit::assert(is.factor(pir_out$clock_model))
  testit::assert(is.factor(pir_out$tree_prior))
  testit::assert(!is.factor(pir_out$error_1))

  if (!all(pir_out$tree %in% c("true", "twin"))) {
    stop("Invalid 'tree' value")
  }

  if (!all(pir_out$inference_model %in% c("generative", "candidate"))) {
    stop("Invalid 'inference_model' value")
  }


  if (!all(is.na(pir_out$inference_model_weight) | pir_out$inference_model_weight >= 0.0 )) {
    stop("Invalid 'inference_model_weight' value")
  }

  if (!all(pir_out$site_model %in% beautier::get_site_model_names())) {
    stop("Invalid 'site_model' value")
  }

  if (!all(pir_out$clock_model %in% beautier::get_clock_model_names())) {
    stop("Invalid 'clock_model' value")
  }

  if (!all(pir_out$tree_prior %in% beautier::get_tree_prior_names())) {
    stop("Invalid 'tree_prior' value")
  }

  # Errors more than zero
  col_first_error <- which(colnames(pir_out) == "error_1")
  col_last_error <- ncol(pir_out)
  if (any(pir_out[, col_first_error:col_last_error] < 0.0)) {
    stop("'errors' cannot be less than zero")
  }

}
