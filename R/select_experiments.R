#' Select the experiments to do a Bayesian inference with.
#' @inheritParams default_params_doc
#' @return a list of inference models
#' @author Richel J.C. Bilderbeek
#' @noRd
select_experiments <- function(
  experiments = list(create_experiment()),
  marg_liks = NULL,
  verbose = FALSE
) {
  check_experiments(experiments) # nolint pirouette function

  # Keep only the candidate marginal likelihoods
  candidate_marg_liks <- select_candidate_evidences(experiments, marg_liks) # nolint pirouette function

  selected_experiments <- list()
  index <- 1
  for (experiment in experiments) {
    if (experiment$run_if == "always") {
      selected_experiments[[index]] <- experiment
      index <- index + 1
    } else if (experiment$run_if == "best_candidate" &&
        is_best_candidate(
          experiment = experiment,
          marg_liks = candidate_marg_liks
        )
      ) {
      selected_experiments[[index]] <- experiment
      index <- index + 1
    }
  }

  if (verbose == TRUE) {
    for (i in seq_along(selected_experiments)) {
      inference_model <- selected_experiments[[i]]$inference_model
      print(
        paste0(
          "Selected model ", i, "/", length(selected_experiments), " with ",
          inference_model$site_model$name,
          " site model, ",
          inference_model$clock_model$name,
          " clock model and ",
          inference_model$tree_prior$name,
          " tree prior"
        )
      )
    }
  }
  selected_experiments
}
