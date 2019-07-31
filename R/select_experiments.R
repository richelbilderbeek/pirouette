#' Select the experiments to do a Bayesian inference with.
#' @inheritParams default_params_doc
#' @return a list of inference models
#' @seealso Use \link{check_experiments} to check if an object
#'   is a list of experiments
#' @author Richèl J.C. Bilderbeek
#' @examples
#' evidences <- create_test_marg_liks(
#'   site_models = list(create_jc69_site_model()),
#'   clock_models = list(create_strict_clock_model()),
#'   tree_priors = list(create_yule_tree_prior(), create_bd_tree_prior())
#' )
#' evidences$weight <- c(0.9, 0.1) # in favor of Yule
#'
#' experiment_yule <- create_test_cand_experiment()
#' experiment_bd <- create_test_cand_experiment()
#' experiment_bd$inference_model$tree_prior <- create_bd_tree_prior()
#' experiment_yule$beast2_options <- experiment_bd$beast2_options
#' experiments <- list(experiment_yule, experiment_bd)
#'
#' # Select the experiment.
#' # In this case, select the candidate experiment with the highest evidence
#' selected <- select_experiments(
#'   experiments = experiments,
#'   marg_liks = evidences
#' )
#'
#' library(testthat)
#' expect_equal(1, length(selected))
#' expect_equal("yule", selected[[1]]$inference_model$tree_prior$name)
#' @export
select_experiments <- function(
  experiments = list(create_test_experiment()),
  marg_liks = NULL,
  verbose = FALSE
) {
  check_experiments(experiments) # nolint pirouette function

  # Keep only the candidate marginal likelihoods
  candidate_marg_liks <- select_candidate_evidences( # nolint pirouette function
    experiments,
    marg_liks
  )

  selected_experiments <- list()
  index <- 1
  for (experiment in experiments) {
    if (experiment$inference_conditions$run_if == "always") {
      selected_experiments[[index]] <- experiment
      index <- index + 1
    } else if (experiment$inference_conditions$run_if == "best_candidate" &&
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
