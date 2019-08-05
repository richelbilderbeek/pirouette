#' Extract the filenames from a \code{pir_params}
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @examples
#' pir_params <- create_pir_params(
#'   alignment_params = create_test_alignment_params(),
#'   experiments = list(create_test_experiment())
#' )
#' get_pir_params_filenames(
#'   pir_params = pir_params
#' )
#' @export
get_pir_params_filenames <- function(pir_params) {
  check_pir_params(pir_params) # nolint pirouette function

  # If there is at least one experiment that has its evidence/marginal
  # likelihood measured, willl there be a file wih evidences
  has_evidence_file <- FALSE
  for (experiment in pir_params$experiments) {
    if (experiment$inference_conditions$do_measure_evidence) {
      has_evidence_file <- TRUE
      break
    }
  }

  filenames <- c(
    get_experiments_filenames(pir_params$experiments),
    pir_params$alignment_params$fasta_filename
  )
  if (has_evidence_file) {
    filenames <- c(filenames, pir_params$evidence_filename)
  }

  if (!beautier::is_one_na(pir_params$twinning_params)) {
    filenames <- c(
      to_twin_filenames(get_experiments_filenames(pir_params$experiments)), # nolint pirouette function
      filenames,
      pir_params$twinning_params$twin_tree_filename,
      pir_params$twinning_params$twin_alignment_filename
    )
    if (has_evidence_file) {
      filenames <- c(
        filenames,
        pir_params$twinning_params$twin_evidence_filename
      )
    }
  }
  unique(sort(filenames))
}
