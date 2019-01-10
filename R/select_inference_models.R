#' Select inference model
#' @inheritParams default_params_doc
select_inference_models <- function(
  alignment_params,
  model_select_params,
  marg_liks = NULL
) {
  check_model_select_params(model_select_params)

  inference_models <- list()
  for (i in seq_along(model_select_params)) {
    model_select_param <- model_select_params[[i]]
    check_model_select_param(model_select_param)

    inference_model <- list()

    if (model_select_param$type == "generative") {
      # Pick the one from the alignment
      testit::assert(!is.null(alignment_params$site_model))
      testit::assert(!is.null(alignment_params$clock_model))
      inference_model$site_model <- alignment_params$site_model
      inference_model$clock_model <- alignment_params$clock_model

      testit::assert(beautier::is_tree_prior(model_select_param$tree_priors[[1]]))
      inference_model$tree_prior <- model_select_param$tree_priors[[1]]
      testit::assert(beautier::is_tree_prior(inference_model$tree_prior))
    } else {
      # Pick the one with the highest evidence
      testit::assert(model_select_param$type == "most_evidence")
      testit::assert(!is.null(marg_liks))
      best_row_index <- which(marg_liks$weight == max(marg_liks$weight))
      inference_model$site_model <- beautier::create_site_model_from_name(
        marg_liks$site_model_name[best_row_index]
      )
      inference_model$clock_model <- beautier::create_clock_model_from_name(
        marg_liks$clock_model_name[best_row_index]
      )
      inference_model$tree_prior <- beautier::create_tree_prior_from_name(
        marg_liks$tree_prior_name[best_row_index]
      )
    }
    testit::assert(beautier::is_tree_prior(inference_model$tree_prior))
    inference_models[[i]] <- inference_model
  }
  testit::assert(length(inference_models) == length(model_select_params))
  inference_models
}
