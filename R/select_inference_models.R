#' Select inference models.
#'
#' This selection is determined by the one or more model selection parameters.
#'
#' @inheritParams default_params_doc
#' @return a list of inference models,
#'   with the same length as \code{model_select_params}.
#'   Each element of this list has one site model, clock model and tree prior.
#' @examples
#'   # Select to use the generative model
#'   alignment_params <- create_alignment_params()
#'
#'   model_select_params <- list( # must be a list
#'     create_gen_model_select_param(
#'       alignment_params = alignment_params
#'     )
#'   )
#'
#'   inference_models <- select_inference_models(
#'     alignment_params = alignment_params,
#'     model_select_params = model_select_params
#'   )
#'   testthat::expect_equal(length(inference_models), 1)
#'   inference_model <- inference_models[[1]]
#'   testthat::expect_equal(inference_model$site_model, alignment_params$site_model)
#'   testthat::expect_equal(inference_model$clock_model, alignment_params$clock_model)
#'
#'
#'  # Select to use the model with the highest evidence/marginal likelihood
#'  alignment_params <- create_alignment_params()
#'    model_select_params <- list( # Must be a list
#'      create_best_model_select_param()
#'    )
#'
#'  # Use a fake table of evidences/marginal likelihoods, as this
#'  # is a costly calculation
#'  marg_liks <- create_test_marg_liks()
#'
#'  inference_models <- select_inference_models(
#'    alignment_params = alignment_params,
#'    model_select_params = model_select_params,
#'    marg_liks = marg_liks
#'  )
#'  testthat::expect_equal(length(inference_models), 1)
#'  inference_model <- inference_models[[1]]
#'
#'  # Should pick the inference model with the highest evidence
#'  most_evidence_row <- which(marg_liks$weight == max(marg_liks$weight))
#'  testthat::expect_equal(
#'    marg_liks$site_model_name[most_evidence_row],
#'    inference_model$site_model$name
#'  )
#'  testthat::expect_equal(
#'    marg_liks$clock_model_name[most_evidence_row],
#'    inference_model$clock_model$name
#'  )
#'  testthat::expect_equal(
#'    marg_liks$tree_prior_name[most_evidence_row],
#'    inference_model$tree_prior$name
#'  )
#' @author Richel J.C. Bilderbeek
#' @export
select_inference_models <- function(
  alignment_params,
  model_select_params,
  marg_liks = NULL
) {
  check_model_select_params(model_select_params) # nolint pirouette function

  inference_models <- list()
  for (i in seq_along(model_select_params)) {
    model_select_param <- model_select_params[[i]]
    check_model_select_param(model_select_param) # nolint pirouette function

    inference_model <- list()
    inference_model$input_filename <- tempfile(fileext = ".xml")
    inference_model$log_filename <- tempfile(fileext = ".log")
    inference_model$trees_filename <- tempfile(fileext = ".trees")
    inference_model$state_filename <- tempfile(fileext = ".xml.state")

    if (model_select_param$type == "generative") {
      # Pick the one from the alignment
      testit::assert(!is.null(alignment_params$site_model))
      testit::assert(!is.null(alignment_params$clock_model))
      inference_model$site_model <- alignment_params$site_model
      inference_model$clock_model <- alignment_params$clock_model

      testit::assert(
        beautier::is_tree_prior(model_select_param$tree_priors[[1]])
      )
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
    check_inference_model(inference_model)
    inference_models[[i]] <- inference_model
  }
  testit::assert(length(inference_models) == length(model_select_params))
  inference_models
}
