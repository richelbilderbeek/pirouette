#' Measure the error BEAST2 makes from a phylogeny
#'
#' The phylogeny can be the true tree or its twin.
#' @inheritParams default_params_doc
#' @return a data frame with errors, with as many rows as model selection
#'   parameter sets
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#'
#' pir_params <- create_test_pir_params()
#'
#' # The user should never need to initialize the pir_params
#' # but a develop calling this function needs to
#' pir_params <- init_pir_params(pir_params)
#'
#'
#' if (
#'   rappdirs::app_dir()$os != "win" &&
#'   is_on_ci() &&
#'   is_beast2_installed()
#' ) {
#'   df <- pir_run_true_tree(
#'     true_phylogeny = ape::rcoal(4),
#'     pir_params = pir_params
#'   )
#'
#'   expect_equal(1, nrow(df))
#'   expected_col_names <- c(
#'     "tree", "inference_model", "inference_model_weight", "site_model",
#'     "clock_model", "tree_prior", "error_1", "error_2", "error_3"
#'   )
#'   expect_true(all(expected_col_names %in% names(df)))
#' }
#' @export
pir_run_true_tree <- function(
  true_phylogeny,
  pir_params
) {
  # pir_params are initialized by pir_run, so a normal user
  # need not do so. If this check gives an error to a developer using
  # this function directly, he/she can use 'init_pir_params'
  pirouette::check_init_pir_params(pir_params)

  # Simulate the true alignment and save it to file
  pirouette::create_tral_file(
    phylogeny = true_phylogeny,
    alignment_params = pir_params$alignment_params,
    verbose = pir_params$verbose
  )

  # Select the alignment file for model comparison
  fasta_filename <- pir_params$alignment_params$fasta_filename
  beautier::check_file_exists(fasta_filename, "fasta_filename")

  # Select the evidence filename the model comparison is written to
  evidence_filename <- pir_params$evidence_filename

  # Estimate evidences (aka marginal likelihoods) if needed
  # marg_liks will be NULL if this was unneeded, for example, when
  # interested in the generative model only
  marg_liks <- pirouette::est_evidences(
    fasta_filename = fasta_filename,
    experiments = pir_params$experiments,
    evidence_filename = evidence_filename,
    verbose = pir_params$verbose
  )

  # Select the experiments
  # to do inference with
  experiments <- pirouette::select_experiments(
    experiments = pir_params$experiments,
    marg_liks = marg_liks, # For most evidence
    verbose = pir_params$verbose
  )
  testit::assert(length(experiments) > 0)

  # Measure the errors per inference model
  errorses <- list() # Reduplicated plural, a list of errors
  for (i in seq_along(experiments)) {
    experiment <- experiments[[i]]

    errorses[[i]] <- pirouette::phylo_to_errors(
      phylogeny = true_phylogeny,
      alignment_params = pir_params$alignment_params,
      error_measure_params = pir_params$error_measure_params,
      experiment = experiment,
      verbose = pir_params$verbose
    )
    testit::assert(is.numeric(errorses[[i]]))

    # Save errors to file
    errors_filename <- experiment$errors_filename
    if (isTRUE(pir_params$verbose)) {
      print(
        paste0("Saving experiment #", i, " errors to '", errors_filename, "'")
      )
    }
    # Create a sub-sub-sub folder, don't warn when it already exists
    dir.create(
      dirname(errors_filename),
      showWarnings = FALSE, recursive = TRUE
    )
    utils::write.csv(
      x = errorses[[i]],
      file = errors_filename
    )
    beautier::check_file_exists(errors_filename)
  }

  df <- pirouette::errorses_to_data_frame(
    errorses = errorses,
    experiments = experiments,
    marg_liks = marg_liks
  )
  df$tree <- "true"
  df$tree <- as.factor(df$tree)
  df
}
