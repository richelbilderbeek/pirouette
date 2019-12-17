#' Combine a combination of site models, clock models and tree priors
#' into a collection of inference models
#' @export
combine_models <- function(
  site_models = beautier::create_site_models(),
  clock_models = beautier::create_clock_models(),
  tree_priors = beautier::create_tree_priors()
) {
  inference_models <- list()

  i <- 1
  for (site_model in site_models) {
    for (clock_model in clock_models) {
      for (tree_prior in tree_priors) {
        inference_models[[i]] <- beautier::create_inference_model(
          site_model = site_model,
          clock_model = clock_model,
          tree_prior = tree_prior
        )
        i <- i + 1
      }
    }
  }
  inference_models
}
