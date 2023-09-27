#' Create the parameters to specify how the error
#' between the given phylogeny and the Bayesian posterior trees is measured.
#'
#' @inheritParams default_params_doc
#' @return an error measurement parameter set
#' @examples
#' if (beautier::is_on_ci()) {
#'
#'   # Default
#'   error_measure_params <- create_error_measure_params()
#'
#'   # Use the nLTT statistic with a burn-in of 10%
#'   error_measure_params <- create_error_measure_params(
#'     burn_in_fraction = 0.1,
#'     error_fun = get_nltt_error_fun()
#'   )
#'
#'   # Use the gamma statistic with a burn-in of 20%
#'   error_measure_params <- create_error_measure_params(
#'     burn_in_fraction = 0.2,
#'     error_fun = get_gamma_error_fun()
#'   )
#'
#'   pir_params <- create_pir_params(
#'     alignment_params = create_test_alignment_params(),
#'     experiments = list(create_test_gen_experiment()),
#'     error_measure_params = error_measure_params
#'   )
#'
#'   if (rappdirs::app_dir()$os != "win" &&
#'     beautier::is_on_ci() && beastier::is_beast2_installed()
#'   ) {
#'     pir_out <- pir_run(
#'       phylogeny = ape::read.tree(text = "((A:2, B:2):1, C:3);"),
#'       pir_params = pir_params
#'     )
#'     pir_plot(pir_out)
#'   }
#' }
#' @export
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
create_error_measure_params <- function(
  burn_in_fraction = 0.1,
  error_fun = get_nltt_error_fun()
) {
  error_measure_params <- list(
    burn_in_fraction = burn_in_fraction,
    error_fun = error_fun
  )
  pirouette::check_error_measure_params(error_measure_params)
  error_measure_params
}
