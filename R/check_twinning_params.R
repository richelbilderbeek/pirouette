#' Checks if the argument is a valid twinning parameters structure,
#' as created by \link{create_twinning_params}.
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if nit
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  library(testthat)
#'
#'  expect_silent(check_twinning_params(create_twinning_params()))
#'  expect_error(check_twinning_params("nonsense"))
#'  expect_error(check_twinning_params(NULL))
#'  expect_error(check_twinning_params(NA))
#' @export
check_twinning_params <- function(
  twinning_params
) {
  argument_names <- c(
    "rng_seed",
    "twin_model",
    "method",
    "n_replicates",
    "twin_tree_filename",
    "twin_alignment_filename",
    "twin_evidence_filename"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(twinning_params)) {
      stop(
        "'", arg_name, "' must be an element of an 'twinning_params'. ",
        "Tip: use 'create_twinning_params'"
      )
    }
  }
  if (!(twinning_params$rng_seed == "same_seed")) {
    if (!is.numeric(twinning_params$rng_seed)) {
      stop("'rng_seed' must be a number or 'same_seed'")
    }
  }
  if (!is.character(twinning_params$twin_model)) {
    stop("'twin_model' must be a character vector")
  }
  if (!(twinning_params$twin_model %in% get_twin_models())) {
    stop(
      "'twin model' is not implemented. \n",
      "Possible values: '", get_twin_models(), "'. \n",
      "Actual value: '", twinning_params$twin_model, "'"
    )
  }
  if (!is.character(twinning_params$method)) {
    stop("'method' must be a character vector")
  }
  if (!(twinning_params$method %in% get_twin_methods())) {
    stop("This 'method' is not implemented")
  }
  if (!is.numeric(twinning_params$n_replicates)) {
    stop("'n_replicates' must be a number")
  }
  if (
    is.infinite(twinning_params$n_replicates) |
    !(twinning_params$n_replicates %% 1 == 0) |
    twinning_params$n_replicates < 0
  ) {
    stop("'n_replicates' must be a finite positive integer number")
  }
  if (!is.character(twinning_params$twin_tree_filename)) {
    stop("'twin_tree_filename' must be a character vector")
  }
  if (!is.character(twinning_params$twin_alignment_filename)) {
    stop("'twin_alignment_filename' must be a character vector")
  }
  if (!is.character(twinning_params$twin_evidence_filename)) {
    stop("'twin_evidence_filename' must be a character vector")
  }

}
