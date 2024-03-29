#' Checks if the argument is a valid twinning parameters structure.
#'
#' Will \link{stop} if not.
#' A valid twinning parameters structure can be created
#' by \link{create_twinning_params}.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if nit
#' @author Richèl J.C. Bilderbeek
#' @examples
#' if (beautier::is_on_ci()) {
#'   check_twinning_params(create_twinning_params())
#' }
#' @export
check_twinning_params <- function(
  twinning_params
) {
  # Check that the structure has all the list elements with the right names
  pirouette::check_twinning_params_names(twinning_params)

  pirouette::check_sim_twin_tree_fun(
    sim_twin_tree_fun = twinning_params$sim_twin_tree_fun
  )
  pirouette::check_sim_twal_fun(
    sim_twal_fun = twinning_params$sim_twal_fun
  )

  if (!beautier::is_one_int(twinning_params$rng_seed_twin_tree)) {
    stop("'rng_seed_twin_tree' must be a whole number")
  }
  if (!beautier::is_one_int(twinning_params$rng_seed_twin_alignment)) {
    stop("'rng_seed_twin_alignment' must be a whole number")
  }
  beautier::check_filename(twinning_params$twin_tree_filename)
  beautier::check_filename(twinning_params$twin_alignment_filename)
  beautier::check_filename(
    twinning_params$twin_evidence_filename,
    allow_na = TRUE
  )
  invisible(twinning_params)
}
