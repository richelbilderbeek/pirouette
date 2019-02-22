#' Create all \link{pirouette} experiments.
#' @inheritParams default_params_doc
#' @return all \link{pirouette} experiments.
#' @export
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
create_all_experiments <- function() {
  site_models <- beautier::create_site_models()
  clock_models <- beautier::create_clock_models()
  tree_priors <- beautier::create_tree_priors()
  all_experiments <- vector(
    "list",
    length(site_models) *
      length(clock_models) *
      length(tree_priors)
  )
  i <- 0
  for (site_model in site_models) {
    for (clock_model in clock_models) {
      for (tree_prior in tree_priors) {
        all_experiments[[i <- i + 1]] <- create_experiment(
          model_type = "candidate",
          run_if = "best_candidate",
          do_measure_evidence = TRUE,
          inference_model = create_inference_model(
            site_model = site_model,
            clock_model = clock_model,
            tree_prior = tree_prior
          )
        )
      }
    }
  }
  all_experiments
}
