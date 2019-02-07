#' Select the experiments to do a Bayesian inference with.
#' @inheritParams default_params_doc
#' @return a list of inference models
#' @author Richel J.C. Bilderbeek
#' @noRd
select_experiments <- function(
  experiments = list(create_experiment()),
  marg_liks = NULL
) {
  check_experiments(experiments) # nolint pirouette function

  # Keep only the candidate marginal likelihoods
  candidate_marg_liks <- select_candidate_evidences(experiments, marg_liks)

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
  selected_experiments
}
