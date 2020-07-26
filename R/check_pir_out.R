#' Checks if the argument is a valid \link{pirouette} parameter set.
#'
#' Will \link{stop} if not.
#' A valid \link{pirouette} parameter set
#' can be created by \link{create_pir_params}.
#'
#' @details
#' A \code{pir_out} contains:
#'
#' \enumerate{
#'   \item tree true or twin tree
#'   \item inference_model generative or candidate
#'   \item inference_model_weight
#'   \item site_model the site model name
#'   \item clock_model the clock model name
#'   \item tree_prior the tree model name
#'   \item error_1, error_2, etcetera: inference errors
#' }
#'
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
  pirouette::check_inference_model_type_names(
    model_type_names = pir_out$inference_model
  )
  pirouette::check_inference_model_weights(
    inference_model_weight = pir_out$inference_model_weight
  )
  for (i in seq_along(pir_out$site_model)) {
     if (!pir_out$site_model[i] %in% beautier::get_site_model_names()) {
      stop(
        "Invalid 'site_model' value. ",
        "Actual value of 'pir_out$site_model[", i, "]': ", pir_out$site_model[i]
      )
    }
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
