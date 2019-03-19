#' Create a result
#' @inheritParams default_params_doc
#' @return a result
#' @seealso Use \link{check_result} to check a result
#' @author Richel J.C. Bilderbeek
#' @examples
#'   testthat::expect_silent(
#'     check_result(create_test_result())
#'   )
#' @export
create_result <- function(
  log_evidence,
  weight,
  errors
) {
  result <- list(
    log_evidence = log_evidence,
    weight = weight,
    errors = errors
  )
  check_result(result)
  result
}
