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
    alignment_params = pir_params$alignment_params
  )

  # Select the alignment file for model comparison
  fasta_filename <- pir_params$alignment_params$fasta_filename
  testit::assert(file.exists(fasta_filename))

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

    # Select the filename the errors are written to
    errors_filename <- experiment$errors_filename

    # Save errors to file
    utils::write.csv(
      x = errorses[[i]],
      file = errors_filename
    )
  }
  testit::assert(length(errorses) > 0)
  testit::assert(length(experiments) == length(errorses))
  if (length(errorses) > 1) {
    if (length(errorses[[1]]) != length(errorses[[2]])) {
      warning(
        "Lengths between errorses differ (", length(errorses[[1]]),
        " vs ", length(errorses[[2]]), "). This is related to #99. ",
        "Fixing this by shortening the longer errorses"
      )
      shortest <- min(length(errorses[[1]]), length(errorses[[2]]))
      errorses[[1]] <- errorses[[1]][1:shortest]
      errorses[[2]] <- errorses[[2]][1:shortest]
    }
    testit::assert(length(errorses[[1]]) == length(errorses[[2]]))
  }

  # Put inference models and errors a data frame
  n_rows <- length(experiments)
  df <- data.frame(
    tree = rep(NA, n_rows),
    inference_model = rep(NA, n_rows),
    inference_model_weight = rep(NA, n_rows),
    site_model = rep(NA, n_rows),
    clock_model = rep(NA, n_rows),
    tree_prior = rep(NA, n_rows)
  )

  error_col_names <- paste0("error_", seq(1, length(errorses[[1]])))
  df[, error_col_names] <- NA

  for (i in seq_along(experiments)) {
    experiment <- experiments[[i]]
    nltts <- errorses[[i]]

    df$tree[i] <- "true"
    check_experiment(experiment) # nolint pirouette function
    df$inference_model[i] <- experiment$inference_conditions$model_type
    df$inference_model_weight[i] <- NA
    df$site_model[i] <- experiment$inference_model$site_model$name
    df$clock_model[i] <- experiment$inference_model$clock_model$name
    df$tree_prior[i] <- experiment$inference_model$tree_prior$name
    from_col_idx <- which(colnames(df) == "error_1")
    df[i, from_col_idx:ncol(df)] <- nltts
  }

  # Add evidence (marginal likelihoods) in columns
  if (!is.null(marg_liks)) {
    for (i in seq_along(experiments)) {
      experiment <- experiments[[i]]
      marg_liks_row <- which(
        marg_liks$site_model_name ==
          experiment$inference_model$site_model$name &
        marg_liks$clock_model_name ==
          experiment$inference_model$clock_model$name &
        marg_liks$tree_prior_name ==
          experiment$inference_model$tree_prior$name
      )
      # if there is no row, 'which' returns a zero-length vector
      # Happens when the generative model is not part of the models
      # under selection
      if (length(marg_liks_row) != 0) {
        df$inference_model_weight[i] <- marg_liks$weight[marg_liks_row]
      }
    }
  }

  df$tree <- as.factor(df$tree)
  df$inference_model <- as.factor(df$inference_model)
  df$site_model <- as.factor(df$site_model)
  df$clock_model <- as.factor(df$clock_model)
  df$tree_prior <- as.factor(df$tree_prior)

  df
}
