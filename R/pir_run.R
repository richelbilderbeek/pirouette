#' Measure the error BEAST2 makes from a known phylogeny.
#'
#' From a phylogeny of (un)known speciation model,
#' an alignment is created using a known site model and clock model,
#' as given by \code{alignment_params}.
#' From the model selection parameters in \code{model_select_params},
#' models are selected to run BEAST2 with. One such model is to use
#' the generative site and clock model the alignment is created,
#' @inheritParams default_params_doc
#' @return a data frame with errors
#' @export
#' @author Richel J.C. Bilderbeek
pir_run <- function(
  phylogeny,
  alignment_params,
  model_select_params = create_gen_model_select_params(alignment_params),
  inference_params
) {
  tryCatch(
    check_alignment_params(alignment_params),
    error = function(msg) {
      msg <- paste0(
        "'alignment_params' must be a set of alignment parameters. ",
        msg
      )
      stop(msg)
    }
  )
  tryCatch(
    check_inference_params(inference_params),
    error = function(msg) {
      msg <- paste0(
        "'inference_params' must be a set of inference parameters. ",
        msg
      )
      stop(msg)
    }
  )
  tryCatch(
    check_model_select_params(model_select_params),
    error = function(msg) {
      msg <- paste0(
        "'inference_params' must be a set of inference parameters. ",
        msg
      )
      stop(msg)
    }
  )
  if (!all(model_select_params$model_selections %in% get_model_selections())) {
    stop("All values of 'model_selections' must be in 'get_model_selections()'")
  }

  # Create alignment, sets alignment RNG seed in 'sim_alignment'
  alignment <- sim_alignment(
    phylogeny = phylogeny,
    alignment_params = alignment_params
  )
  # Save alignment to file
  temp_fasta_filename <- tempfile(pattern = "pirouette_", fileext = ".fasta")
  phangorn::write.phyDat(
    alignment,
    file = temp_fasta_filename,
    format = "fasta"
  )

  # Estimate marginal likelihoods if needed
  marg_liks <- NULL
  if ("most_evidence" %in% model_select_params$model_selections) {
    marg_liks <- mcbette::est_marg_liks(
      fasta_filename = temp_fasta_filename,
      site_models = model_select_params$site_models,
      clock_models = model_select_params$clock_models,
      tree_priors = model_select_params$tree_priors
    )
  }

  df <- data.frame()
  for (model_selection in model_select_params$model_selections) {
    this_df <- pir_run_one(
      phylogeny = phylogeny,
      alignment = alignment,
      alignment_params = alignment_params,
      inference_params = inference_params,
      model_selection = model_selection,
      marg_liks = marg_liks
    )
    if (!is.null(marg_liks)) {
      marg_liks_row <- which(
        marg_liks$site_model_name == as.character(this_df$site_model) &
        marg_liks$clock_model_name == as.character(this_df$clock_model) &
        marg_liks$tree_prior_name == as.character(this_df$tree_prior)
      )
      this_df$inference_model_weight <- marg_liks$weight[marg_liks_row]
    }
    df <- rbind(df, this_df)
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
  alignment,
  alignment_params,
  inference_params,
  model_selection,
  marg_liks = NULL
) {
  testit::assert(length(model_selection) == 1)
  if (!model_selection %in% get_model_selections()) {
    stop("'model_selection' must be in 'get_model_selections()'")
  }

  # Pick the inference parameters
  if (model_selection == "generative") {
    testit::assert(!is.null(alignment_params$site_model))
    testit::assert(!is.null(alignment_params$clock_model))
    inference_params$site_model <- alignment_params$site_model
    inference_params$clock_model <- alignment_params$clock_model
  } else if (model_selection == "most_evidence") {
    testit::assert(!is.null(marg_liks))
    best_row_index <- which(marg_liks$weight == max(marg_liks$weight))
    inference_params$site_model <- beautier::create_site_model_from_name(
      marg_liks$site_model_name[best_row_index]
    )
    inference_params$clock_model <- beautier::create_clock_model_from_name(
      marg_liks$clock_model_name[best_row_index]
    )
    inference_params$tree_prior <- beautier::create_tree_prior_from_name(
      marg_liks$tree_prior_name[best_row_index]
    )
  }

  # Run
  trees <- alignment_to_posterior_trees(
    alignment = alignment,
    inference_params = inference_params
  )

  # Analyse
  nltts <- nLTT::nltts_diff(tree = phylogeny, trees = trees)

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
