#' Checks if the argument is a valid model selection parameters structure,
#' as created by \link{create_model_select_params}.
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if the object is not
#' @author Richel J.C. Bilderbeek
check_model_select_params <- function(
  model_select_params
) {
  argument_names <- c(
    "model_selections",
    "site_models", "clock_models", "tree_priors"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(model_select_params)) {
      stop(
        "'", arg_name, "' must be an element of an 'model_select_params'. ",
        "Tip: use 'create_model_select_params'"
      )
    }
  }
  if (!all(model_select_params$model_selections %in% get_model_selections())) {
    stop(
      "All elements of 'model_select_params$model_selections' ",
      "must be in 'get_model_selections()'"
    )
  }
  if (!beautier::are_site_models(model_select_params$site_models)) {
    stop(
      "All elements of 'model_select_params$site_models' ",
      "must be site models"
    )
  }
  if (!beautier::are_clock_models(model_select_params$clock_models)) {
    stop(
      "All elements of 'model_select_params$clock_models' ",
      "must be clock models"
    )
  }
  if (!beautier::are_tree_priors(model_select_params$tree_priors)) {
    stop(
      "All elements of 'model_select_params$tree_priors' ",
      "must be tree priors"
    )
  }
}
