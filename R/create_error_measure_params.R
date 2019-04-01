#' Create the parameters to specify how the error
#' between the given phylogeny and the Bayesian posterior trees is measured.
#'
#' @inheritParams default_params_doc
#' @return an error measurement parameter set
#' @examples
#'  library(testthat)
#'
#'  # Default
#'  error_measure_params <- create_error_measure_params()
#'  expect_true("burn_in_fraction" %in% names(error_measure_params))
#'  expect_true("error_function" %in% names(error_measure_params))
#'
#'  # Use the nLTT statistic with a burn-in of 10%
#'  error_measure_params <- create_error_measure_params(
#'    burn_in_fraction = 0.1,
#'    error_function = get_nltt_error_function()
#'  )
#'
#'  # Use the gamma statistic with a burn-in of 20%
#'  error_measure_params <- create_error_measure_params(
#'    burn_in_fraction = 0.2,
#'    error_function = get_gamma_error_function()
#'  )
#'
#'  pir_params <- create_pir_params(
#'    alignment_params = create_test_alignment_params(),
#'    experiments = list(create_test_experiment()),
#'    error_measure_params = error_measure_params
#'  )
#'
#'  if (is_on_ci()) {
#'    pir_out <- pir_run(
#'      phylogeny = ape::read.tree(text = "((A:2, B:2):1, C:3);"),
#'      pir_params = pir_params
#'    )
#'    pir_plot(pir_out)
#'  }
#' @export
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
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
