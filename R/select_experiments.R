#' Select the experiments to do a Bayesian inference with.
#' @inheritParams default_params_doc
#' @return a list of inference models
#' @seealso Use \link{check_experiments} to check if an object
#'   is a list of experiments
#' @author Richèl J.C. Bilderbeek
#' @examples
#' if (beautier::is_on_ci()) {
#'
#'   evidences <- create_test_marg_liks(
#'     site_models = list(beautier::create_jc69_site_model()),
#'     clock_models = list(beautier::create_strict_clock_model()),
#'     tree_priors = list(
#'       beautier::create_yule_tree_prior(), 
#'       beautier::create_bd_tree_prior()
#'     )
#'   )
#'   evidences$weight <- c(0.9, 0.1) # in favor of Yule
#'
#'   if (rappdirs::app_dir()$os != "win") {
#'     experiment_yule <- create_test_cand_experiment()
#'     experiment_bd <- create_test_cand_experiment()
#'     experiment_bd$inference_model$tree_prior <- 
#'       beautier::create_bd_tree_prior()
#'     experiment_yule$beast2_options <- experiment_bd$beast2_options
#'     experiment_yule$inference_model$mcmc <- experiment_bd$inference_model$mcmc
#'     experiment_yule$errors_filename <- experiment_bd$errors_filename
#'     experiments <- list(experiment_yule, experiment_bd)
#'
#'     # Select the experiment.
#'     # In this case, select the candidate experiment with the highest evidence
#'     select_experiments(
#'       experiments = experiments,
#'       marg_liks = evidences
#'     )
#'   }
#'
#' }
#' @export
select_experiments <- function(
  experiments = list(create_test_experiment()),
  marg_liks = NULL,
  verbose = FALSE
) {
  pirouette::check_experiments(experiments)

  # Keep only the candidate marginal likelihoods
  candidate_marg_liks <- pirouette::select_candidate_evidences(
    experiments = experiments,
    marg_liks = marg_liks
  )

  selected_experiments <- list()
  index <- 1
  for (experiment in experiments) {
    if (experiment$inference_conditions$run_if == "always") {
      selected_experiments[[index]] <- experiment
      index <- index + 1
    } else if (experiment$inference_conditions$run_if == "best_candidate" &&
               pirouette::is_best_candidate(
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
      message(
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
