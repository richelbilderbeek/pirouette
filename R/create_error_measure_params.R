#' Create the parameters to specify hwo the error
#' between teh given phylogeny and the Bayesian posterior trees is measured.
#' @inheritParams default_params_doc
#' @param rng_seed the random number generator seed as used in the
#'   simulation of a twin tree
#' @return a error_measure parameter set
#' @examples
#'  error_measure_params <- create_error_measure_params()
#'  testit::assert("rng_seed" %in% names(error_measure_params))
#'  testit::assert("twin_tree_filename" %in% names(error_measure_params))
#' @export
#' @author Richel J.C. Bilderbeek
create_error_measure_params <- function(
  burn_in_fraction = 0.1,
  error_function = get_nltt_error_function()
) {
  error_measure_params <- list(
    burn_in_fraction = burn_in_fraction,
    error_function = error_function
  )
  check_error_measure_params(error_measure_params = error_measure_params) # nolint pirouette function
  error_measure_params
}
