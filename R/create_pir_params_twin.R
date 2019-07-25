#' Create the twin parameters for \link{pirouette}
#'
#' Or: puts the twin in the front row, by copying all parameters
#' for the twin-something to the spots for the true-something
#' @inheritParams default_params_doc
#' @return a list with all \link{pirouette} twin parameters
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#'   library(testthat)
#'
#'   pir_params <- create_test_pir_params(
#'     twinning_params = create_twinning_params()
#'   )
#'
#'  expect_false(
#'    pir_params$alignment_params$fasta_filename ==
#'    pir_params$twinning_params$twin_alignment_filename
#'  )
#'
#'  pir_params <- create_pir_params_twin(pir_params)
#'
#'  expect_true(
#'   pir_params$alignment_params$fasta_filename ==
#'   pir_params$twinning_params$twin_alignment_filename
#'  )
#' @export
create_pir_params_twin <- function(
  pir_params
) {
  if (beautier::is_one_na(pir_params$twinning_params)) {
    stop(
      "'pir_params$twinning_params' must be a twinning parameter set. \n",
      "Tip: use 'create_twinning_params' \n"
    )
  }

  pir_params_twin <- pir_params

  # file names
  pir_params_twin$alignment_params$fasta_filename <-
    pir_params$twinning_params$twin_alignment_filename
  pir_params_twin$evidence_filename <-
    pir_params$twinning_params$twin_evidence_filename
  for (i in seq_along(pir_params$experiments)) {
    filenames <- as.character(
        pir_params$experiments[[i]]$beast2_options[
        grepl(
          "filename",
          names(pir_params$experiments[[i]]$beast2_options)
        )
        ]
    )
    for (ii in seq_along(filenames)) {
      if (!is.character(filenames[ii])) {
        stop(
          "ii:\n", ii, " \n",
          "filenames[ii]:\n", filenames[ii], " \n",
          "class(filenames[ii]):\n", class(filenames[ii]), " \n",
          "pir_params$experiments[[i]]$beast2_options:\n",
          pir_params$experiments[[i]]$beast2_options, " \n"
        )
      }
      testit::assert(is.character(filenames[ii]))
      pir_params_twin$experiments[[i]]$beast2_options[ii] <-
        to_twin_filename(filenames[ii]) # nolint pirouette function
    }
  }

  check_pir_params(pir_params_twin) # nolint pirouette function
  pir_params_twin
}
