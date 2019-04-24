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

  filenames <- c(
    pir_params$alignment_params$fasta_filename,
    pir_params$evidence_filename
  )

  for (experiment in pir_params$experiments) {
    filenames <- c(
      filenames,
      c(
        experiment$beast2_options$input_filename,
        experiment$beast2_options$output_log_filename,
        experiment$beast2_options$output_trees_filenames,
        experiment$beast2_options$output_state_filename
      )
    )
  }

  if (!beautier::is_one_na(pir_params$twinning_params)) {
    filenames <- c(filenames,
      c(
        pir_params$twinning_params$twin_tree_filename,
        pir_params$twinning_params$twin_alignment_filename,
        pir_params$twinning_params$twin_evidence_filename
      )
    )
  }

  file.remove(filenames[file.exists(filenames)])
  testit::assert(all(!file.exists(filenames)))
}
