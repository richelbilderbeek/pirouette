#' Create all \link{pirouette} experiments.
#' @inheritParams default_params_doc
#' @return all \link{pirouette} experiments.
#' @export
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
create_all_experiments <- function(
  site_models = beautier::create_site_models(),
  clock_models = beautier::create_clock_models(),
  tree_priors = beautier::create_tree_priors(),
  mcmc = create_mcmc(store_every = 1000)
) {
  check_site_models(site_models) # nolint pirouette function
  check_clock_models(clock_models) # nolint pirouette function
  check_tree_priors(tree_priors) # nolint pirouette function

  all_experiments <- vector(
    "list",
    length(site_models) *
      length(clock_models) *
      length(tree_priors)
  )
  i <- 1
  for (site_model in site_models) {
    for (clock_model in clock_models) {
      for (tree_prior in tree_priors) {
        all_experiments[[i]] <- create_experiment(
          inference_conditions = create_inference_conditions(
            model_type = "candidate",
            run_if = "best_candidate",
            do_measure_evidence = TRUE
          ),
          inference_model = create_inference_model(
            site_model = site_model,
            clock_model = clock_model,
            tree_prior = tree_prior,
            mcmc = mcmc
          ),
          beast2_options = create_beast2_options(
            input_filename = tempfile(
              pattern = paste0("beast2_", i, "_"), fileext = ".xml"
            ),
            output_log_filename = tempfile(
              pattern = paste0("beast2_", i, "_"), fileext = ".log"
            ),
            output_trees_filenames = tempfile(
              pattern = paste0("beast2_", i, "_"), fileext = "trees"
            ),
            output_state_filename = tempfile(
              pattern = paste0("beast2_", i, "_"), fileext = ".state.xml"
            )
          )
        )
        i <- i + 1
      }
    }
  }
  all_experiments
}
