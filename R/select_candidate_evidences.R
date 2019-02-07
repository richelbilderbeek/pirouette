#' Select the evidences for candidate experiments
#'
#' @author Richel J.C. Bilderbeek
#' @export
select_candidate_evidences <- function(
  experiments = list(create_test_experiment()),
  marg_liks = create_test_marg_liks()
) {
  check_experiments(experiments) # nolint pirouette function
  marg_liks
}
