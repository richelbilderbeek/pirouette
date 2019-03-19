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
create_test_result <- function(
  log_evidence = -123.456,
  weight = 0.5,
  errors = c(0.3, 0.1, 0.4, 0.1, 0.5, 0.9, 0.3)
) {
  create_result(
    log_evidence = log_evidence,
    weight = weight,
    errors = errors
  )
}
