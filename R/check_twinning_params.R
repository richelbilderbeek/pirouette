#' Checks if the argument is a valid twinning parameters structure.
#'
#' Will \link{stop} if not.
#' A valid twinning parameters structure can be created
#' by \link{create_twinning_params}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if nit
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' expect_silent(check_twinning_params(create_twinning_params()))
#' expect_error(check_twinning_params("nonsense"))
#' expect_error(check_twinning_params(NULL))
#' expect_error(check_twinning_params(NA))
#' @export
check_twinning_params <- function(
  twinning_params
) {
  argument_names <- c(
    "rng_seed_twin_tree",
    "rng_seed_twin_alignment",
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
  if (!beautier::is_one_int(twinning_params$rng_seed_twin_tree)) {
    stop("'rng_seed_twin_tree' must be a number")
  }

  if (!beautier::is_one_int(twinning_params$rng_seed_twin_alignment)) {
    stop("'rng_seed_twin_alignment' must be a number")
  }
  if (!is.character(twinning_params$twin_model)) {
    stop("'twin_model' must be a character vector")
  }
  if (!(twinning_params$twin_model %in% get_twin_models())) {
    stop(
      "'twin model' is not implemented. \n",
      "Possible values: '",
        paste0(get_twin_models(), collapse = ", "), "'. \n",
      "Actual value: '", twinning_params$twin_model, "'"
    )
  }
  if (!is.character(twinning_params$method)) {
    stop("'method' must be a character vector")
  }
  if (!(twinning_params$method %in% get_twin_methods())) {
    stop("This 'method' is not implemented")
  }
  if (!beautier::is_one_int(twinning_params$n_replicates)) {
    stop("'n_replicates' must be a whole number")
  }
  if (twinning_params$n_replicates < 0) {
    stop("'n_replicates' must be a finite positive integer number")
  }

  if (!assertive::is_a_string(twinning_params$twin_tree_filename)) {
    stop("'twin_tree_filename' must be a character vector")
  }
  if (!assertive::is_a_string(twinning_params$twin_alignment_filename)) {
    stop("'twin_alignment_filename' must be a character vector")
  }
  if (!is.character(twinning_params$twin_evidence_filename)) {
    stop("'twin_evidence_filename' must be a character vector")
  }
}
