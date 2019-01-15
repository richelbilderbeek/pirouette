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
#'   parameter sets
#' @export
#' @author Richel J.C. Bilderbeek
pir_run <- function(
  phylogeny,
  twinning_params = NA,
  alignment_params,
  model_select_params = list(create_gen_model_select_param(alignment_params)),
  inference_param # The shared BEAST2 setup parameters
) {
  # Check the inputs
  pir_run_check_inputs(
    phylogeny = phylogeny,
    alignment_params = alignment_params,
    model_select_params = model_select_params,
    inference_param = inference_param
  )
  # Run for the true tree
  df <- pir_run_tree(
    phylogeny = phylogeny,
    tree_type = "true",
    alignment_params = alignment_params,
    model_select_params = model_select_params,
    inference_param = inference_param
  )

  # Run for the twin tree
  if (!beautier:::is_one_na(twinning_params)) {
    twin_tree <- create_twin_tree(phylogeny)
    twin_alignment_params <- alignment_params
    twin_alignment_params$fasta_filename <- twinning_params$twin_tree_filename

    df_twin <- pir_run_tree(
      phylogeny = twin_tree,
      tree_type = "twin",
      alignment_params = twin_alignment_params,
      model_select_params = model_select_params,
      inference_param = inference_param
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
  model_select_params = list(create_gen_model_select_param(alignment_params)),
  inference_param # The shared BEAST2 setup parameters
) {
  testit::assert(tree_type %in% c("true", "twin"))
  # Simulate an alignment and save it to file (specified in alignment_params)
  sim_alignment_file(
    fasta_filename = alignment_params$fasta_filename,
    phylogeny = phylogeny,
    alignment_params = alignment_params
  )
  testit::assert(file.exists(alignment_params$fasta_filename))

  # Estimate marginal likelihoods if needed
  marg_liks <- NULL
  for (model_select_param in model_select_params) {
    if ("most_evidence" %in% model_select_param$type) {
      marg_liks <- mcbette::est_marg_liks(
        fasta_filename = alignment_params$fasta_filename,
        site_models = model_select_param$site_models,
        clock_models = model_select_param$clock_models,
        tree_priors = model_select_param$tree_priors,
        epsilon = model_select_param$epsilon,
        verbose = model_select_param$verbose
      )
    }
  }

  # Select the models to do inference with
  inference_models <- select_inference_models(
    alignment_params = alignment_params, # Both need alignment file
    model_select_params = model_select_params, # To pick which one
    marg_liks = marg_liks # For most evidence
  )
  testit::assert(length(inference_models) == length(model_select_params))
  testit::assert(
    all(c("site_model", "clock_model", "tree_prior") %in%
    names(inference_models[[1]]))
  )

  # Measure the errors per inference model
  errorses <- list() # Gollumese plural, a list of errors
  for (i in seq_along(inference_models)) {
    inference_model <- inference_models[[i]]
    testit::assert(
      all(c("site_model", "clock_model", "tree_prior") %in%
      names(inference_model))
    )

    errorses[[i]] <- phylo_to_errors(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      site_model = inference_model$site_model,
      clock_model = inference_model$clock_model,
      tree_prior = inference_model$tree_prior,
      inference_param = inference_param
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
    tree_prior = rep(NA, n_rows)
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
  inference_param
) {
  tryCatch(
    check_alignment_params(alignment_params), # nolint pirouette function
    error = function(msg) {
      msg <- paste0(
        "'alignment_params' must be a set of alignment parameters.\n",
        "Error message: ", msg, "\n",
        "Actual value: ", alignment_params
      )
      stop(msg)
    }
  )
  tryCatch(
    check_inference_param(inference_param), # nolint pirouette function
    error = function(msg) {
      msg <- paste0(
        "'inference_param' must be a set of inference parameters.\n",
        "Error message: ", msg, "\n",
        "Actual value: ", inference_param
      )
      stop(msg)
    }
  )
  tryCatch(
    check_model_select_params(model_select_params), # nolint pirouette function
    error = function(msg) {
      msg <- paste0(
        "'model_select_params' must be a list of one or more model selection ",
        "parameters sets.\n",
        "Error message: ", msg, "\n",
        "Actual value: ", model_select_params
      )
      stop(msg)
    }
  )
}
