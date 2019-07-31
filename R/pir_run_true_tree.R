#' Measure the error BEAST2 makes from a phylogeny
#'
#' The phylogeny can be the true tree or its twin.
#' @inheritParams default_params_doc
#' @return a data frame with errors, with as many rows as model selection
#'   parameter sets
#' @export
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
pir_run_true_tree <- function(
  true_phylogeny,
  pir_params = create_test_pir_params()
) {

  # Simulate the true alignment and save it to file
  create_alignment_file(
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
  marg_liks <- est_evidences(
    fasta_filename = fasta_filename,
    experiments = pir_params$experiments,
    evidence_filename = evidence_filename,
    verbose = pir_params$verbose
  )

  # Select the experiments
  # to do inference with
  experiments <- select_experiments(
    experiments = pir_params$experiments,
    marg_liks = marg_liks, # For most evidence
    verbose = pir_params$verbose
  )
  testit::assert(length(experiments) > 0)

  # Measure the errors per inference model
  errorses <- list() # Reduplicated plural, a list of errors
  for (i in seq_along(experiments)) {
    experiment <- experiments[[i]]

    errorses[[i]] <- phylo_to_errors(
      phylogeny = true_phylogeny,
      alignment_params = pir_params$alignment_params,
      error_measure_params = pir_params$error_measure_params,
      experiment = experiment
    )

    # Save errors to file
    errors_filename <- experiment$errors_filename
    if (isTRUE(pir_params$verbose)) {
      print(
        paste0("Saving experiment #", i, " errors to '", errors_filename, "'")
      )
    }
    utils::write.csv(
      x = errorses[[i]],
      file = errors_filename
    )
    beautier::check_file_exists(errors_filename)
  }

  df <- errorses_to_data_frame(
    errorses = errorses,
    experiments = experiments,
    marg_liks = marg_liks
  )
  df$tree <- "true"
  df$tree <- as.factor(df$tree)
  df
}
