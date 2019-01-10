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
#' @param alignment_filename name of the file a simulated alignment is
#'   saved to. By default, this file will cleaned up by the
#'   operating system
#' @return a data frame with errors, with as many rows as model selection
#'   parameter sets
#' @export
#' @author Richel J.C. Bilderbeek
pir_run <- function(
  phylogeny,
  alignment_params,
  model_select_params = create_gen_model_select_param(alignment_params),
  inference_params # The shared BEAST2 setup parameters
) {
  # Check the inputs
  pir_run_check_inputs(
    phylogeny = phylogeny,
    alignment_params = alignment_params,
    model_select_params = model_select_params,
    inference_params = inference_params
  )

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
    if ("most_evidence" %in% model_select_param$model_selections) {
      marg_liks <- mcbette::est_marg_liks(
        fasta_filename = alignment_params$fasta_filename,
        site_models = model_select_param$site_models,
        clock_models = model_select_param$clock_models,
        tree_priors = model_select_param$tree_priors
      )
    }
  }

  # Select the models to do inference with
  inference_models <- select_inference_models(
    alignment_params = alignment_params, # Both need alignment file
    model_select_params = model_select_params, # To pick which one
    inference_params = inference_params, # Shared BEAST2 params
    marg_liks = marg_liks # For most evidence
  )
  testit::assert(length(inference_models) == length(model_select_params))
  testit::assert(all(c("site_model", "clock_model", "tree_prior") %in% names(inference_models[[1]])))

  # Measure the errors per inference model
  errorses <- list() # Gollumese plural, a list of errors
  for (i in seq_along(inference_models)) {
    inference_model <- inference_models[[i]]
    testit::assert(all(c("site_model", "clock_model", "tree_prior") %in% names(inference_model)))

    errorses[[i]] <- phylo_to_errors(
      phylogeny = phylogeny,
      alignment_params = alignment_params,
      site_model = inference_model$site_model,
      clock_model = inference_model$clock_model,
      tree_prior = inference_model$tree_prior,
      inference_params = inference_params
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

  for (i in seq_along(inference_models)) {
    model_select_param <- model_select_params[[i]]
    inference_model <- inference_models[[i]]
    nltts <- errorses[[i]]

    df$tree[i] <- "true"
    df$inference_model[i] <- model_select_param$model_selection
    df$inference_model_weight[i] <- NA
    df$site_model[i] <- inference_model$site_model$name
    df$clock_model[i] <- inference_model$clock_model$name
    df$tree_prior[i] <- inference_model$tree_prior$name

    error_col_names <- paste0("error_", seq(1, length(nltts)))
    this_df[i, error_col_names] <- 0.0
    from_col_idx <- which(colnames(this_df) == "error_1")
    df[i, from_col_idx:ncol(this_df)] <- nltts
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
  df
}

#' Meaure the error BEAST2 makes from a known phylogeny
#' using one inference model selection strategy
#' @inheritParams default_params_doc
#' @return a data frame with errors
#' @author Richel J.C. Bilderbeek
#' @noRd
pir_run_one <- function(
  phylogeny,
  alignment_params,
  site_model = site_model,
  clock_model = clock_model,
  tree_prior = tree_prior,
  inference_params,
  model_selection,
  marg_liks = NULL
) {
  testit::assert(length(model_selection) == 1)
  if (!model_selection %in% get_model_selections()) {
    stop("'model_selection' must be in 'get_model_selections()'")
  }


  df <- data.frame(
    tree = "true",
    inference_model = model_selection,
    inference_model_weight = NA,
    site_model = inference_params$site_models$name,
    clock_model = inference_params$clock_models$name,
    tree_prior = inference_params$tree_priors$name
  )
  error_col_names <- paste0("error_", seq(1, length(nltts)))
  df[, error_col_names] <- 0.0

  from_col_idx <- which(colnames(df) == "error_1")
  df[1, from_col_idx:ncol(df)] <- nltts
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
  inference_params
) {
  tryCatch(
    check_alignment_params(alignment_params),
    error = function(msg) {
      msg <- paste0(
        "'alignment_params' must be a set of alignment parameters.\n",
        "Actual value: ", msg
      )
      stop(msg)
    }
  )
  tryCatch(
    check_inference_params(inference_params),
    error = function(msg) {
      msg <- paste0(
        "'inference_params' must be a set of inference parameters.\n",
        "Actual value: ", msg
      )
      stop(msg)
    }
  )
  tryCatch(
    check_model_select_params(model_select_params),
    error = function(msg) {
      msg <- paste0(
        "'inference_params' must be a list of one or more inference parameter ",
        "sets.\n",
        "Actual value: ", msg
      )
      stop(msg)
    }
  )
}
