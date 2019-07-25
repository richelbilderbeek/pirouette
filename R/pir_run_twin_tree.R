#' Measure the error BEAST2 makes from a phylogeny
#'
#' The phylogeny can be the true tree or its twin.
#' @inheritParams default_params_doc
#' @return a data frame with errors, with as many rows as model selection
#'   parameter sets
#' @export
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
pir_run_twin_tree <- function(
  twin_phylogeny,
  pir_params = create_test_pir_params()
) {
  # Shorthand notations
  alignment_params <- pir_params$alignment_params
  twinning_params <- pir_params$twinning_params
  experiments <- pir_params$experiments
  error_measure_params <- pir_params$error_measure_params

  # If alignment_params$mutation_rate is function, apply it to the phylogeny
  if (is.function(alignment_params$mutation_rate)) {
    mutation_function <- alignment_params$mutation_rate
    mutation_rate <- mutation_function(twin_phylogeny)
    # Write it to both shorthand form and function argument:
    # the reader of this code expects these are the same
    alignment_params$mutation_rate <- mutation_rate
    pir_params$alignment_params$mutation_rate <- mutation_rate
  }

  # Simulate the twin alignm  ent and save it to file
  create_twin_alignment_file(
    twin_phylogeny = twin_phylogeny,
    alignment_params = alignment_params,
    twinning_params = twinning_params
  )
  testit::assert(file.exists(twinning_params$twin_alignment_filename))

  # Select the alignment file for model comparison
  fasta_filename <- twinning_params$twin_alignment_filename

  # Select the evidence filename the model comparison is written to
  evidence_filename <- twinning_params$twin_evidence_filename

  # Estimate evidences (aka marginal likelihoods) if needed
  # marg_liks will be NULL if this was unneeded, for example, when
  # interested in the generative model only
  marg_liks <- est_evidences(
    fasta_filename = fasta_filename,
    experiments = experiments,
    evidence_filename = evidence_filename,
    verbose = pir_params$verbose
  )

  # Select the experiments
  # to do inference with
  experiments <- select_experiments(
    experiments = experiments,
    marg_liks = marg_liks, # For most evidence
    verbose = pir_params$verbose
  )
  testit::assert(length(experiments) > 0)

  # Measure the errors per inference model
  errorses <- list() # Reduplicated plural, a list of errors
  for (i in seq_along(experiments)) {
    experiment <- experiments[[i]]

    errorses[[i]] <- phylo_to_errors(
      phylogeny = twin_phylogeny,
      alignment_params = alignment_params,
      error_measure_params = error_measure_params,
      experiment = experiment
    )

    # Save errors to file
    errors_filename <- experiment$errors_filename

    # Unique for twin
    errors_filename <- to_twin_filename(errors_filename) # nolint pirouette function

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

    df$tree[i] <- "twin"
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
