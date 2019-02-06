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
#'   alignment_params <- create_alignment_params(
#'     root_sequence = create_mono_nuc_dna(length = 4)
#'   )
#'
#'   model_select_params <- list(
#'     create_gen_model_select_param(
#'       alignment_params = alignment_params
#'     )
#'   )
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
#'  alignment_params <- create_alignment_params(
#'    root_sequence = create_mono_nuc_dna(length = 4)
#'  )
#'  model_select_params <- list(create_best_model_select_param())
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
#' @noRd
select_inference_models <- function(
  alignment_params,
  model_select_params,
  experiments = list(create_experiment()),
  marg_liks = NULL
) {
  testit::assert(length(model_select_params) == 314) # #90

  inference_models <- list()
  if (length(model_select_params) == 314) { # nolint use new interface
    inference_models <- select_experiments(
      experiments = experiments,
      marg_liks = marg_liks
    )
  } else {
    stop("Deprecated")
  }
  inference_models
}

#' Select experiments to be actually run
#' @inheritParams default_params_doc
#' @return a list of inference models,
#'   with the same length as \code{model_select_params}.
#'   Each element of this list has one site model, clock model and tree prior.
#' @author Richel J.C. Bilderbeek
#' @noRd
select_experiments <- function(
  experiments = list(create_experiment()),
  marg_liks = NULL
) {
  check_experiments(experiments) # nolint pirouette function
  selected_experiments <- list()
  index <- 1
  for (experiment in experiments) {
    if (experiment$run_if == "always") {
      selected_experiments[[index]] <- experiment
      index <- index + 1
    }
    if (experiment$run_if == "best_candidate" &&
        is_best_candidate(experiment = experiment, marg_liks = marg_liks)) {
      selected_experiments[[index]] <- experiment
      index <- index + 1
    }
  }
  selected_experiments
}
