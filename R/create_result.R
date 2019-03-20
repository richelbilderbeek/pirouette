#' Create a result
#' @inheritParams default_params_doc
#' @return a result
#' @seealso Use \link{check_result} to check a result
#' @author Richel J.C. Bilderbeek
#' @examples
#'   log_evidence <- -123.456
#'   weight <- 0.1234
#'   errors <- c(3, 1, 4, 1, 5, 9, 3)
#'
#'   result <- create_result(
#'     log_evidence = log_evidence,
#'     weight = weight,
#'     errors = errors
#'   )
#'
#'   expect_equal(log_evidence, result$log_evidence)
#'   expect_equal(weight, result$weight)
#'   expect_equal(errors, result$errors)
#'
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
