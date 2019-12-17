#' Checks if the argument is a valid \link{pirouette} parameter set.
#'
#' Will \link{stop} if not.
#' A valid \link{pirouette} parameter set
#' can be created by \link{create_pir_params}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' expect_silent(check_pir_out(create_test_pir_run_output()))
#' expect_error(check_pir_out("nonsense"))
#' expect_error(check_pir_out(NULL))
#' expect_error(check_pir_out(NA))
#' @export
check_pir_out <- function(
  pir_out
) {
  pirouette::check_pir_out_names(pir_out)

  testit::assert(is.factor(pir_out$tree))
  testit::assert(is.factor(pir_out$inference_model))
  testit::assert(is.factor(pir_out$site_model))
  testit::assert(!is.factor(pir_out$inference_model_weight))
  testit::assert(is.factor(pir_out$clock_model))
  testit::assert(is.factor(pir_out$tree_prior))
  testit::assert(!is.factor(pir_out$error_1))

  pirouette::check_tree_types(pir_out$tree)

  if (!all(pir_out$inference_model %in% get_model_type_names())) {
    stop("Invalid 'inference_model' value")
  }

  for (i in seq_along(pir_out$inference_model_weight)) {
    weight <- pir_out$inference_model_weight[i]
    if (beautier::is_one_na(weight)) next
    if (!beautier::is_one_double(weight)) {
      stop("Each 'model_weight' must be NA or a double")
    }
    if (weight < 0.0 || weight > 1.0) {
      stop("Each 'model_weight' must be a double in range [0.0, 1.0]")
    }
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
  pirouette::check_pir_out_errors_above_zero(pir_out)
}



#' Checks if the \code{pir_out} has elements with
#' the names needed.
#'
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if not
#' @author Richèl J.C. Bilderbeek
#' @export
check_pir_out_names <- function(
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
}
