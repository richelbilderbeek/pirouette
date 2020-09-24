#' Determine if these \code{pir_params} use twinning
#' @inheritParams default_params_doc
#' @return TRUE if the pir_params uses twinnning
#' @examples
#'
#' pir_params <- create_test_pir_params()
#' expect_false(has_twinning(pir_params))
#'
#' pir_params <- create_test_pir_params(
#'   twinning_params = create_twinning_params()
#' )
#' expect_true(has_twinning(pir_params))
#' @export
has_twinning <- function(pir_params) {
  pirouette::check_pir_params(pir_params)
  !beautier::is_one_na(pir_params$twinning_params)
}
