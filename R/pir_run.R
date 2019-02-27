#' Measure the error BEAST2 makes from a known phylogeny.
#'
#' From a phylogeny of (un)known speciation model,
#' an alignment is created using a known site model and clock model,
#' as given by \code{alignment_params}.
#'
#' @inheritParams default_params_doc
#' @return a data frame with errors, with as many rows as model selection
#'   parameter sets.
#'   Tip: use \link{pir_plot} to display it
#' @export
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
pir_run <- function(
  phylogeny,
  pir_params = create_pir_params(
    alignment_params = create_alignment_params(
      mutation_rate = create_standard_mutation_rate
    ),
    twinning_params = create_twinning_params(rng_seed = "same_seed")
  )
) {

  # Check the inputs
  if (!beautier::is_phylo(phylogeny)) {
    stop("'phylogeny' must be of class 'phylo'")
  }
  check_pir_params(pir_params) # nolint pirouette function

  # Higher-level checks
  for (experiment in seq_along(pir_params$experiments)) {
    is_cbs
    experiment$inference_model
  }

  # Run for the true tree
  df <- pir_run_tree(
    phylogeny = phylogeny,
    tree_type = "true",
    alignment_params = pir_params$alignment_params,
    experiments = pir_params$experiments,
    error_measure_params = pir_params$error_measure_params,
    evidence_filename = pir_params$evidence_filename,
    verbose = pir_params$verbose
  )

  # Run for the twin tree
  if (!beautier::is_one_na(pir_params$twinning_params)) {

    # Create specific twin pir_params
    pir_params_twin <- pir_params
    pir_params_twin$alignment_params$fasta_filename <-
      pir_params$twinning_params$twin_alignment_filename
    pir_params_twin$evidence_filename <-
      pir_params$twinning_params$twin_evidence_filename
    for (i in seq_along(pir_params$experiments)) {
      filenames <- pir_params$experiments[[i]]$beast2_options[
        grepl(
          "filename",
          names(pir_params$experiments[[i]]$beast2_options)
        )
        ]
      for (ii in seq_along(filenames)) {
        pir_params_twin$experiments[[i]]$beast2_options[ii] <-
          to_twin_filename(filenames[ii]) # nolint pirouette function
      }
    }
    if (pir_params$twinning_params$rng_seed == "same_seed") {
      pir_params_twin$twinning_params$rng_seed <-
        pir_params$alignment_params$rng_seed
    }

    # Create and save twin tree
    twin_tree <- create_twin_tree(
      phylogeny,
      twinning_params = pir_params_twin$twinning_params
    ) # nolint pirouette function
    ape::write.tree(
      phy = twin_tree,
      file = pir_params_twin$twinning_params$twin_tree_filename
    )

    # Re-run pir_run for the twin
    df_twin <- pir_run_tree(
      phylogeny = twin_tree,
      tree_type = "twin",
      alignment_params = pir_params_twin$alignment_params,
      experiments = pir_params_twin$experiments,
      error_measure_params = pir_params_twin$error_measure_params,
      evidence_filename = pir_params_twin$evidence_filename,
      verbose = pir_params_twin$verbose
    )
    df <- rbind(df, df_twin)
  }
  df
}

#' Measure the error BEAST2 makes from a phylogeny
#'
#' The phylogeny can be the true tree or its twin.
#' @inheritParams default_params_doc
#' @return a data frame with errors, with as many rows as model selection
#'   parameter sets
#' @export
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
pir_run_tree <- function(
  phylogeny,
  tree_type = "true",
  alignment_params,
  experiments = list(create_experiment()),
  error_measure_params = create_error_measure_params(),
  evidence_filename = tempfile(fileext = ".csv"),
  verbose = FALSE
) {
  testit::assert(tree_type %in% c("true", "twin"))

  # If alignment_params$mutation_rate is function, apply it to the phylogeny
  if (is.function(alignment_params$mutation_rate)) {
    mutation_function <- alignment_params$mutation_rate
    mutation_rate <- mutation_function(phylogeny)
    alignment_params$mutation_rate <- mutation_rate
  }

  # Simulate an alignment and save it to file (specified in alignment_params)
  sim_alignment_file(
    phylogeny = phylogeny,
    alignment_params = alignment_params
  )
  testit::assert(file.exists(alignment_params$fasta_filename))

  # Estimate evidences (aka marginal likelihoods) if needed
  # marg_liks will be NULL if this was unneeded, for example, when
  # interested in the generative model only
  marg_liks <- est_evidences(
    fasta_filename = alignment_params$fasta_filename,
    experiments = experiments,
    evidence_filename = evidence_filename,
    verbose = verbose
  )

  # Select the experiments
  # to do inference with
  experiments <- select_experiments(
    experiments = experiments,
    marg_liks = marg_liks, # For most evidence
    verbose = verbose
  )
  testit::assert(length(experiments) > 0)

  # Measure the errors per inference model
  errorses <- list() # Reduplicated plural, a list of errors
  for (i in seq_along(experiments)) {
    experiment <- experiments[[i]]

    errorses[[i]] <- phylo_to_errors(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      error_measure_params = error_measure_params,
      experiment = experiment
    )

    # Save errors to file
    errors_filename <- experiment$errors_filename
    if (tree_type == "twin") {
      errors_filename <- to_twin_filename(errors_filename) # nolint pirouette function
    }
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

    df$tree[i] <- tree_type
    check_experiment(experiment) # nolint pirouette function
    df$inference_model[i] <- experiment$model_type
    df$inference_model_weight[i] <- NA
    df$site_model[i] <- experiment$inference_model$site_model$name
    df$clock_model[i] <- experiment$inference_model$clock_model$name
    df$tree_prior[i] <- experiment$inference_model$tree_prior$name
    from_col_idx <- which(colnames(df) == "error_1")
    if (verbose == TRUE) {
      print(paste("from_col_idx:", from_col_idx))
      print(paste("ncol(df):", ncol(df)))
      print(paste("length(nltts):", length(nltts)))
    }
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
