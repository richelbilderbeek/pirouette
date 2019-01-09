#' Create a list of all model combinations needed
#' @return a list of all model combinations. Each element contains
#'   one site model, one clock model and one tree prior
#' @author Richel J.C. Bilderbeek
#' @export
collect_models <- function(model_select_params) {
  check_model_select_params(model_select_params)

  models <- list()

  model_index <- 1
  for (site_model in model_select_params$site_models) {
    for (clock_model in model_select_params$clock_models) {
      for (tree_prior in model_select_params$tree_priors) {
        models[[model_index]] <- list(
          site_model = site_model,
          clock_model = clock_model,
          tree_prior = tree_prior
        )
        model_index <- model_index + 1
      }
    }
  }
  models
}
