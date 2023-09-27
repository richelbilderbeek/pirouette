#' Deletes all files
#' @inheritParams default_params_doc
#' @return Nothing
#' @author Richèl J.C. Bilderbeek
#' @examples
#' if (beautier::is_on_ci()) {
#'
#'   pir_params <- create_test_pir_params(
#'     experiments = list(create_test_gen_experiment())
#'   )
#'
#'   filenames <- get_pir_params_filenames(pir_params)
#'
#'   if (beautier::is_on_ci() && beastier::is_beast2_installed()) {
#'
#'     # Minimal pirouette run
#'     errors <- pir_run(
#'       phylogeny = ape::read.tree(text = "((A:2, B:2):1, C:3);"),
#'       pir_params = pir_params
#'     )
#"
#'     # Removing the files
#'     rm_pir_param_files(pir_params)
#'   }
#' }
#' @export
rm_pir_param_files <- function(pir_params) {

  filenames <- pirouette::get_pir_params_filenames(pir_params)
  file.remove(filenames[file.exists(filenames)])
  testthat::expect_true(all(!file.exists(filenames)))
}
