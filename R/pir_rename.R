#' Rename the filenames in a \code{pir_params}
#' using a rename function.
#' @author Richèl J.C. Bilderbeek
#' @export
pir_rename <- function(
  pir_params,
  rename_fun
) {
  pirouette::check_pir_params(pir_params)
  pirouette::check_rename_fun(rename_fun)

}
