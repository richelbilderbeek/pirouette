#' Measure the error BEAST2 makes from a known phylogeny.
#'
#' From a phylogeny of (un)known speciation model,
#' an alignment is created using a known site model and clock model,
#' as given by \code{alignment_params}.
#'
#' From each of the one or more model selection parameters in
#' \code{model_select_params}, one inference model is concluded.
#' One such parameter setup is to use the generative site and clock model
#' the alignment is created with (\code{"generative"}), or use the model
#' that has the most evidence (\code{"most evidence"}).
#'
#' Each inference model is used to run BEAST2 and measures the
#' difference between the known/true/given phylogeny and the
#' ones created by BEAST2.
#'
#' @inheritParams default_params_doc
#' @return a data frame with errors, with as many rows as model selection
#'   parameter sets.
#'   Tip: use \link{pir_plot} to display it
#' @examples
#'   phylogeny <- ape::read.tree(text = "(((A:1, B:1):1, C:2):1, D:3);")
#'   alignment_params <- create_alignment_params(
#'     mutation_rate = 0.01
#'   )
#'   errors <- pir_run(
#'     phylogeny = phylogeny,
#'     alignment_params = alignment_params,
#'     model_select_params = create_gen_model_select_param(
#'       alignment_params = alignment_params
#'     ),
#'     inference_params = create_inference_params(
#'       mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000)
#'     )
#'   )
#'   pir_plot(errors)
#' @export
#' @author Richel J.C. Bilderbeek
pir_run <- function(
  phylogeny,
  twinning_params = NA,
  alignment_params,
  model_select_params = create_gen_model_select_param(alignment_params), # nolint obsolete, #69
  inference_params = create_inference_params(), # obsolete, #69
  experiments = list(create_experiment()),
  error_measure_params = create_error_measure_params()
) {
  # List model_select_params
  model_select_params <- list_model_select_params(model_select_params) # nolint pirouette function

  # Check the inputs
  pir_run_check_inputs(
    phylogeny = phylogeny,
    alignment_params = alignment_params,
    model_select_params = model_select_params, # obsolete, #69
    inference_params = inference_params, # obsolete, #69
    experiments = experiments,
    error_measure_params = error_measure_params
  )
  # Run for the true tree
  df <- pir_run_tree(
    phylogeny = phylogeny,
    tree_type = "true",
    alignment_params = alignment_params,
    model_select_params = model_select_params, # obsolete, #69
    inference_params = inference_params, # obsolete, #69
    experiments = experiments,
    error_measure_params = error_measure_params
  )

  # Run for the twin tree
  if (!beautier:::is_one_na(twinning_params)) {
    twin_tree <- create_twin_tree(phylogeny) # nolint beautier function
    ape::write.tree(phy = twin_tree, file = twinning_params$twin_tree_filename)
    twin_alignment_params <- alignment_params
    twin_alignment_params$fasta_filename <- twinning_params$twin_alignment_filename # nolint long param names indeed ...

    df_twin <- pir_run_tree(
      phylogeny = twin_tree,
      tree_type = "twin",
      alignment_params = twin_alignment_params,
      model_select_params = model_select_params, # obsolete, #69
      inference_params = inference_params, # obsolete, #69
      experiments = experiments
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
#' @author Richel J.C. Bilderbeek
pir_run_tree <- function(
  phylogeny,
  tree_type = "true",
  alignment_params,
  model_select_params = create_gen_model_select_param(alignment_params),
  inference_params = create_inference_params(),
  experiments = list(create_experiment()),
  error_measure_params = create_error_measure_params(),
  evidence_epsilon = 1e-12,
  evidence_filename = tempfile(fileext = ".csv")

) {
  testit::assert(tree_type %in% c("true", "twin"))
  # Simulate an alignment and save it to file (specified in alignment_params)
  sim_alignment_file(
    phylogeny = phylogeny,
    alignment_params = alignment_params
  )
  testit::assert(file.exists(alignment_params$fasta_filename))

  # Estimate evidences (aka marginal likelihoods) if needed
  # marg_liks will be NULL if this was unneeded
  marg_liks <- est_evidences(
    fasta_filename = alignment_params$fasta_filename,
    model_select_params = model_select_params,
    experiments = experiments
  )

  # Select the models to do inference with
  inference_models <- select_inference_models(
    alignment_params = alignment_params, # Both need alignment file
    model_select_params = model_select_params, # To pick which one
    marg_liks = marg_liks # For most evidence
  )
  testit::assert(length(inference_models) == length(model_select_params))
  check_old_skool_inference_model(inference_models[[1]]) # nolint pirouette function

  # Measure the errors per inference model
  errorses <- list() # Gollumese plural, a list of errors
  for (i in seq_along(inference_models)) {
    inference_model <- inference_models[[i]]
    check_old_skool_inference_model(inference_model) # nolint pirouette function

    errorses[[i]] <- phylo_to_errors(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      inference_model = inference_model,
      inference_params = inference_params,
      error_measure_params = error_measure_params,
      experiment = experiments[[1]] # stub #69
    )
  }
  testit::assert(length(inference_models) == length(errorses))

  # Put inference models and errors a data frame
  n_rows <- length(inference_models)
  df <- data.frame(
    tree = rep(NA, n_rows),
    inference_model = rep(NA, n_rows),
    inference_model_weight = rep(NA, n_rows),
    site_model = rep(NA, n_rows),
    clock_model = rep(NA, n_rows),
    tree_prior = rep(NA, n_rows),
    beast2_input_filename = rep(NA, n_rows),
    beast2_output_log_filename = rep(NA, n_rows),
    beast2_output_trees_filename = rep(NA, n_rows),
    beast2_output_state_filename = rep(NA, n_rows)
  )

  error_col_names <- paste0("error_", seq(1, length(errorses[[1]])))
  df[, error_col_names] <- NA

  for (i in seq_along(inference_models)) {
    model_select_param <- model_select_params[[i]]
    inference_model <- inference_models[[i]]
    nltts <- errorses[[i]]

    df$tree[i] <- tree_type
    df$inference_model[i] <- model_select_param$type
    df$inference_model_weight[i] <- NA
    df$site_model[i] <- inference_model$site_model$name
    df$clock_model[i] <- inference_model$clock_model$name
    df$tree_prior[i] <- inference_model$tree_prior$name

    df$beast2_input_filename[i] <- inference_model$beast2_input_filename
    df$beast2_output_log_filename[i] <-
      inference_model$beast2_output_log_filename
    df$beast2_output_trees_filename[i] <-
      inference_model$beast2_output_trees_filename
    df$beast2_output_state_filename[i] <-
      inference_model$beast2_output_state_filename

    from_col_idx <- which(colnames(df) == "error_1")
    df[i, from_col_idx:ncol(df)] <- nltts
  }

  # Add evidence (marginal likelihoods) in columns
  if (!is.null(marg_liks)) {
    for (i in seq_along(inference_models)) {
      inference_model <- inference_models[[i]]
      marg_liks_row <- which(
        marg_liks$site_model_name == inference_model$site_model$name &
        marg_liks$clock_model_name == inference_model$clock_model$name &
        marg_liks$tree_prior_name == inference_model$tree_prior$name
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

#' Checks the \link{pir_run} inputs.
#'
#' Calls \link{stop} if an input is invalud
#'
#' @inheritParams default_params_doc
#' @return nothing
#' @export
#' @author Richel J.C. Bilderbeek
pir_run_check_inputs <- function(
  phylogeny,
  alignment_params,
  model_select_params,
  inference_params,
  experiments,
  error_measure_params
) {
  if (!beautier::is_phylo(phylogeny)) {
    stop("'phylogeny' must be of class 'phylo'")
  }
  tryCatch(
    check_alignment_params(alignment_params), # nolint pirouette function
    error = function(e) {
      msg <- paste0(
        "'alignment_params' must be a set of alignment parameters.\n",
        "Tip: use 'create_alignment_params'\n",
        "Error message: ", e$message, "\n",
        "Actual value: ", alignment_params
      )
      stop(msg)
    }
  )
  tryCatch(
    check_inference_params(inference_params), # nolint pirouette function
    error = function(e) {
      msg <- paste0(
        "'inference_params' must be a set of inference parameters.\n",
        "Tip: use 'create_inference_params'\n",
        "Error message: ", e$message, "\n",
        "Actual value: ", inference_params
      )
      stop(msg)
    }
  )
  tryCatch(
    check_model_select_params(model_select_params), # nolint pirouette function
    error = function(e) {
      msg <- paste0(
        "'model_select_params' must be a list of one or more model selection ",
        "Tip: use 'create_model_select_params'\n",
        "parameters sets.\n",
        "Error message: ", e$message, "\n",
        "Actual value: ", model_select_params
      )
      stop(msg)
    }
  )
  tryCatch(
    check_error_measure_params(error_measure_params), # nolint pirouette function
    error = function(e) {
      msg <- paste0(
        "'error_measure_params' must be a set of error measurement ",
        "parameters.\n",
        "Tip: use 'create_error_measure_params'\n",
        "Error message: ", e$message, "\n",
        "Actual value: ", model_select_params
      )
      stop(msg)
    }
  )
  tryCatch(
    check_experiments(experiments), # nolint pirouette function
    error = function(e) {
      msg <- paste0(
        "'experiments' must be one experiment or a list of one or more ",
        "experiments.\n",
        "Tip: use 'create_experiments'\n",
        "Error message: ", e$message, "\n",
        "Actual value: ", experiments
      )
      stop(msg)
    }
  )
}
