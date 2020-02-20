#' Extract the filenames from a \code{pir_params}
#' @inheritParams default_params_doc
#' @param method temporary argument to specify how the filenames
#' are obtained. Use either 'flat' or 'oldskool'
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
  pir_params,
  method = "flat"
) {
  pirouette::check_pir_params(pir_params)
  testthat::expect_true(method %in% c("flat", "oldskool"))
  flat_filenames <- get_pir_params_filenames_flat(pir_params)
  oldskool_filenames <- get_pir_params_filenames_oldskool(pir_params)
  testthat::expect_equivalent(flat_filenames, oldskool_filenames)
  if (method == "flat") {
    return(get_pir_params_filenames_flat(pir_params))
  }
  if (method == "oldskool") {
    return(get_pir_params_filenames_oldskool(pir_params))
  }
}

#' Extract the filenames from a \code{pir_params}
#' using the \code{oldskool} method.
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @export
get_pir_params_filenames_oldskool <- function( # nolint indeed a long function name
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
  filenames <- c(
    pirouette::get_experiments_filenames(pir_params$experiments),
    pir_params$alignment_params$fasta_filename,
    pir_params$experiments[[1]]$inference_model$mcmc$tracelog$filename,
    pir_params$experiments[[1]]$inference_model$mcmc$treelog$filename
  )
  if (has_evidence_file) {
    filenames <- c(filenames, pir_params$evidence_filename)
  }

  if (!beautier::is_one_na(pir_params$twinning_params)) {
    filenames <- c(
      pirouette::to_twin_filenames(
        pirouette::get_experiments_filenames(pir_params$experiments)
      ),
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

#' Extract the filenames from a \code{pir_params}
#' using the \code{flat} method.
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @export
get_pir_params_filenames_flat <- function(
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
  if (!beautier::is_one_na(pir_params$twinning_params)) {
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
    if (!beautier::is_one_na(pir_params$twinning_params)) {
      filenames <- filenames[
        filenames != pir_params$twinning_params$twin_evidence_filename
      ]
    }
  }
  unique(sort(filenames))
}
