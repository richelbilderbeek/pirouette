#' Deletes all files
#' @inheritParams default_params_doc
#' @return Nothing
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' pir_params <- create_test_pir_params(
#'   experiments = list(create_test_gen_experiment())
#' )
#'
#' filenames <- c(
#'   pir_params$alignment_params$fasta_filename,
#'   pir_params$experiments[[1]]$beast2_options$input_filename,
#'   pir_params$experiments[[1]]$beast2_options$output_log_filename,
#'   pir_params$experiments[[1]]$beast2_options$output_trees_filenames,
#'   pir_params$experiments[[1]]$beast2_options$output_state_filename
#' )
#'
#' if (is_on_travis() && is_beast2_installed()) {
#'
#'   # Minimal pirouette run
#'   errors <- pir_run(
#'     phylogeny = ape::read.tree(text = "((A:2, B:2):1, C:3);"),
#'     pir_params = pir_params
#'   )
#"
#'   # Files are created
#'   expect_true(all(file.exists(filenames)))
#'
#'   # Removing the files
#'   rm_pir_param_files(pir_params)
#'
#'   # All files should be gone
#'   expect_true(all(!file.exists(filenames)))
#' }
#' @export
rm_pir_param_files <- function(pir_params) {

  filenames <- get_pir_params_filenames(pir_params)
  file.remove(filenames[file.exists(filenames)])
  testit::assert(all(!file.exists(filenames)))
}
