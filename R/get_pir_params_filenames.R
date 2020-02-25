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
get_pir_params_filenames <- function(
  pir_params
) {
  pirouette::check_pir_params(pir_params)

  # Initialize so the tracelog and treelog filenames are filled in
  pir_params <- pirouette::init_pir_params(pir_params)

  # If there is at least one experiment that has its evidence/marginal
  # likelihood measured, willl there be a file wih evidences
  has_evidence_file <- FALSE
  for (experiment in pir_params$experiments) {
    if (experiment$inference_conditions$do_measure_evidence) {
      has_evidence_file <- TRUE
      break
    }
  }

  filenames <- NA
  flat_pir_params <- unlist(pir_params)
  filename_indices <- stringr::str_detect(
    string = names(flat_pir_params),
    pattern = "filename"
  )
  # List has a nice legend
  filenames_as_list <- flat_pir_params[filename_indices]
  filenames <- as.character(unlist(filenames_as_list))

  # screenlog may be two quotes
  filenames <- stats::na.omit(filenames)
  filenames <- filenames[filenames != ""]
  testthat::expect_true(all(filenames != ""))
  if (pirouette::has_twinning(pir_params)) {
    twin_filenames <- pirouette::get_experiments_filenames(
      pir_params$experiments
    )
    twin_filenames <- stats::na.omit(twin_filenames)
    twin_filenames <- twin_filenames[twin_filenames != ""]
    twin_filenames <- pirouette::to_twin_filenames(twin_filenames)
    filenames <- c(filenames, twin_filenames)
  }

  # Remove evidence files
  if (!has_evidence_file) {
    # Normal evidence
    filenames <- filenames[filenames != pir_params$evidence_filename]
    # Twin evidence
    if (pirouette::has_twinning(pir_params)) {
      filenames <- filenames[
        filenames != pir_params$twinning_params$twin_evidence_filename
      ]
    }
  }
  unique(sort(filenames))
}
