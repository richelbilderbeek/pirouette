#' Measure the error BEAST2 makes from a phylogeny
#'
#' The phylogeny can be the true tree or its twin.
#' @inheritParams default_params_doc
#' @return a data frame with errors, with as many rows as model selection
#'   parameter sets
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#'
#' # Create a true phylogeny to simulate the DNA sequences on
#' n_taxa <- 5
#' set.seed(1)
#' phylogeny <- ape::rcoal(n_taxa)
#'
#' # Simulate and save the true alignment
#' alignment_params <- create_test_alignment_params()
#' create_tral_file(
#'   phylogeny = phylogeny,
#'   alignment_params = alignment_params
#' )
#'
#' # Create a twin phylogeny to simulate the DNA sequences on
#' set.seed(2)
#' twin_phylogeny <- ape::rcoal(n_taxa)
#' twinning_params <- create_twinning_params()
#'
#' # Simulate and save the twin alignment
#' alignment <- create_twal_file(
#'   twin_phylogeny = twin_phylogeny,
#'   alignment_params = alignment_params,
#'   twinning_params = twinning_params
#' )
#'
#' # Bundle parameters in pir_params
#' pir_params <- create_test_pir_params()
#' pir_params$alignment_params <- alignment_params
#' pir_params$twinning_params <- twinning_params
#' pir_params <- init_pir_params(pir_params)
#'
#' pir_run_twin_tree(
#'   twin_phylogeny = twin_phylogeny,
#'   pir_params = pir_params
#' )
#' @export
pir_run_twin_tree <- function(
  twin_phylogeny,
  pir_params = create_test_pir_params()
) {
  # Simulate the twin alignment and save it to file
  create_twal_file(
    twin_phylogeny = twin_phylogeny,
    alignment_params = pir_params$alignment_params,
    twinning_params = pir_params$twinning_params
  )

  # Select the alignment file for model comparison
  fasta_filename <- pir_params$twinning_params$twin_alignment_filename
  beautier::check_file_exists(fasta_filename, "fasta_filename")

  # Select the evidence filename the model comparison is written to
  evidence_filename <- pir_params$twinning_params$twin_evidence_filename

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

    experiment$beast2_options$input_filename <- pirouette::to_twin_filename(
      experiment$beast2_options$input_filename
    )
    experiment$inference_model$mcmc$tracelog$filename <-
      pirouette::to_twin_filename(
        experiment$inference_model$mcmc$tracelog$filename
    )
    experiment$inference_model$mcmc$treelog$filename <-
      pirouette::to_twin_filename(
      experiment$inference_model$mcmc$treelog$filename
    )
    experiment$beast2_options$output_state_filename <-
      pirouette::to_twin_filename(
      experiment$beast2_options$output_state_filename
    )
    experiment$errors_filename <- pirouette::to_twin_filename(
      experiment$errors_filename
    )

    # Dirty hack: use a modified alignment_params for informing
    # 'phylo_to_errors' about the filename of the alignment
    #
    # I would have preferred two functions, 'true_phylo_to_errors'
    # and 'twin_phylo_to_errors' that probably call a same
    # function 'phylo_to_errors_impl' in the back
    alignment_params <- pir_params$alignment_params
    alignment_params$fasta_filename <-
      pir_params$twinning_params$twin_alignment_filename # sorry Demeter

    errorses[[i]] <- phylo_to_errors(
      phylogeny = twin_phylogeny,
      alignment_params = alignment_params,
      error_measure_params = pir_params$error_measure_params,
      experiment = experiment,
      verbose = pir_params$verbose
    )

    # Save errors to file
    errors_filename <- experiment$errors_filename
    if (isTRUE(pir_params$verbose)) {
      print(
        paste0(
          "Saving twin experiment #", i, " errors to '", errors_filename, "'"
        )
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
    beautier::check_file_exists(errors_filename, "errors_filename")
  }
  df <- errorses_to_data_frame(
    errorses = errorses,
    experiments = experiments,
    marg_liks = marg_liks
  )
  df$tree <- "twin"
  df$tree <- as.factor(df$tree)
  df
}
