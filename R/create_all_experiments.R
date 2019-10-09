#' Create all \link{pirouette} experiments.
#'
#' These experiments are used in the \link{create_pir_params} function
#' @inheritParams default_params_doc
#' @return all \link{pirouette} experiments.
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#'   library(testthat)
#'
#'   if (rappdirs::app_dir()$os != "win") {
#'      # it does not work on Windows
#'      experiments <- create_all_experiments()
#'      check_experiments(experiments)
#'
#'      expect_true(length(experiments) >= 40)
#'
#'      pir_params <- create_pir_params(
#'        alignment_params = create_test_alignment_params(),
#'        experiments = experiments
#'      )
#'   }
#'
#' @export
create_all_experiments <- function(
  site_models = beautier::create_site_models(),
  clock_models = beautier::create_clock_models(),
  tree_priors = beautier::create_tree_priors(),
  mcmc = beautier::create_mcmc(store_every = 1000),
  exclude_model = NA
) {
  beautier::check_site_models(site_models)
  beautier::check_clock_models(clock_models)
  beautier::check_tree_priors(tree_priors)
  if (!all(is.na(exclude_model))) {
    beautier::check_inference_model(exclude_model)
  }

  all_experiments <- list()

  # All experiments use the same BEAST2 options,
  # or at least the filenames should be the same
  beast2_options <- beastier::create_beast2_options(
    input_filename = tempfile(
      pattern = "beast2_", fileext = ".xml"
    ),
    output_log_filename = tempfile(
      pattern = "beast2_", fileext = ".log"
    ),
    output_trees_filenames = tempfile(
      pattern = "beast2_", fileext = "trees"
    ),
    output_state_filename = tempfile(
      pattern = "beast2_", fileext = ".state.xml"
    )
  )
  errors_filename <- tempfile(
    pattern = "errors_", fileext = ".csv"
  )

  i <- 1
  for (site_model in site_models) {
    for (clock_model in clock_models) {
      for (tree_prior in tree_priors) {
        new_experiment <- create_experiment(
          inference_conditions = create_inference_conditions(
            model_type = "candidate",
            run_if = "best_candidate",
            do_measure_evidence = TRUE
          ),
          inference_model = beautier::create_inference_model(
            site_model = site_model,
            clock_model = clock_model,
            tree_prior = tree_prior,
            mcmc = mcmc
          ),
          beast2_options = beast2_options,
          errors_filename = errors_filename
        )
        new_model <- new_experiment$inference_model
        if (all(is.na(exclude_model))) {
          all_experiments[[i]] <- new_experiment
          i <- i + 1
        } else if (
          !(
            identical(new_model$site_model, exclude_model$site_model) &&
            identical(new_model$clock_model, exclude_model$clock_model) &&
            identical(new_model$tree_prior, exclude_model$tree_prior)
          )
        ) {
          all_experiments[[i]] <- new_experiment
          i <- i + 1
        }
      }
    }
  }
  names(all_experiments) <- seq_along(all_experiments)
  all_experiments[sapply(all_experiments, is.null)] <- NULL

  pirouette::check_experiments(all_experiments)

  all_experiments
}
