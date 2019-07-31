#' Estimate the evidences
#' @inheritParams default_params_doc
#' @return a data frame with evidences. Returns NULL if there
#'   are no experiments that have their evidence measured.
#' @author Richèl J.C. Bilderbeek
#' @examples
#' fasta_filename <- system.file(
#'   "extdata", "alignment.fas", package = "pirouette"
#' )
#'
#' # Create a single one candidate experiment
#' experiments <- list(create_test_cand_experiment())
#'
#' # Be sloppy amd fast in estimating the evidence
#' evidence_epsilon <- 100.0
#'
#' if (is_on_ci() && is_beast2_installed() && is_beast2_ns_pkg_installed()) {
#'   evidences <- est_evidences(
#'     fasta_filename = fasta_filename,
#'     experiments = experiments,
#'     evidence_epsilon = evidence_epsilon
#'   )
#'
#'   library(testthat)
#'   expect_true("site_model_name" %in% names(evidences))
#'   expect_true("clock_model_name" %in% names(evidences))
#'   expect_true("tree_prior_name" %in% names(evidences))
#'   expect_true("marg_log_lik" %in% names(evidences))
#'   expect_true("marg_log_lik_sd" %in% names(evidences))
#'   expect_true("weight" %in% names(evidences))
#'
#'   # As the only experiment, its weight is 1.0
#'   expect_equal(1.0, evidences$weight[1])
#' }
#' @export
est_evidences <- function(
  fasta_filename,
  experiments = list(create_test_experiment()),
  evidence_epsilon = 1e-12,
  evidence_filename = tempfile(pattern = "evidence_", fileext = ".csv"),
  verbose = FALSE
) {
  if (!beastier::is_beast2_installed()) {
    stop("BEAST2 not installed. Tip: use 'beastier::install_beast2()'")
  }
  if (!file.exists(fasta_filename)) {
    stop(
      "'fasta_filename' must be the name of an existing file. ",
      "File '", fasta_filename, "' not found"
    )
  }
  check_experiments(experiments) # nolint pirouette function
  if (!beautier::is_one_double(evidence_epsilon)) {
    stop(
      "'evidence_epsilon' must be one numerical value. ",
      "Actual value(s): ", evidence_epsilon
    )
  }

  # Collect inference models and BEAST2 optionses
  inference_models <- list()
  beast2_optionses <- list()
  i <- 1
  for (experiment in experiments) {
    if (experiment$inference_conditions$do_measure_evidence) {
      testit::assert(
        beautier::is_nested_sampling_mcmc(experiment$est_evidence_mcmc)
      )
      inference_models[[i]] <- experiment$inference_model
      # Overwrite Nested Sampling MCMC
      inference_models[[i]]$mcmc <- experiment$est_evidence_mcmc
      beast2_optionses[[i]] <- experiment$beast2_options
      # Change the filenames, else good BEAST2 runs may be deleted
      beast2_optionses[[i]]$input_filename <- to_evidence_filename(
        beast2_optionses[[i]]$input_filename
      )
      beast2_optionses[[i]]$output_log_filename <- to_evidence_filename(
        beast2_optionses[[i]]$output_log_filename
      )
      beast2_optionses[[i]]$output_trees_filenames <- to_evidence_filename(
        beast2_optionses[[i]]$output_trees_filenames
      )
      beast2_optionses[[i]]$output_state_filename <- to_evidence_filename(
        beast2_optionses[[i]]$output_state_filename
      )
      # Overwrite BEAST2 bin path
      beast2_optionses[[i]]$beast2_path <- experiment$beast2_bin_path
      i <- i + 1
    }
  }
  testit::assert(length(inference_models) == length(beast2_optionses))
  if (length(inference_models) == 0) {
    return(NULL)
  }

  testit::assert(length(inference_models) > 0)
  beautier::check_inference_models(inference_models)
  beastier::check_beast2_optionses(beast2_optionses)
  check_is_ns_beast2_pkg_installed() # nolint long function name indeed
  marg_liks <- mcbette::est_marg_liks_from_models(
    fasta_filename = fasta_filename,
    inference_models = inference_models,
    beast2_optionses =  beast2_optionses,
    epsilon = evidence_epsilon,
    verbose = verbose
  )
  if (verbose == TRUE) {
    print(marg_liks)
  }
  tryCatch(
    utils::write.csv(
      x = marg_liks, file = evidence_filename
    ),
    error = function(e) {
      stop(
        "Saving evidence to file failed. \n",
        "Filename: ", evidence_filename, "\n",
        "Error: ", e$message
      )
    }
  )

  # Delete files
  for (experiment in experiments) {
    if (file.exists(experiment$beast2_options$output_log_filename)) {
      file.remove(experiment$beast2_options$output_log_filename)
    }
    if (file.exists(experiment$beast2_options$output_state_filename)) {
      file.remove(experiment$beast2_options$output_state_filename)
    }
    if (file.exists(experiment$beast2_options$output_trees_filenames)) {
      file.remove(experiment$beast2_options$output_trees_filenames)
    }
  }
  sum_marg_liks <- sum(marg_liks$weight)
  tolerance <- 0.1
  if (abs(1.0 - sum_marg_liks) > tolerance) {
    stop(
      "Sum of evidences (aka marginal likelihoods) deviates too much from ",
        " one \n",
      "Sum: ", sum_marg_liks, "\n",
      "Tolerance: ", tolerance, "\n"
    )
  }
  marg_liks
}
