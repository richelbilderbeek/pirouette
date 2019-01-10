#' Checks if the argument is a valid model selection parameters structure,
#' as created by \link{create_model_select_param}.
#' Will \link{stop} if not.
#' @inheritParams default_params_doc
#' @return nothing. Will \link{stop} if the object is not
#' @author Richel J.C. Bilderbeek
check_model_select_param <- function(
  model_select_param
) {
  argument_names <- c(
    "type",
    "site_models", "clock_models", "tree_priors"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(model_select_param)) {
      stop(
        "'", arg_name, "' must be an element of a 'model_select_param'. ",
        "Tip: use 'create_model_select_param'"
      )
    }
  }
  if (!all(model_select_param$model_selections %in% get_model_selections())) {
    stop(
      "All elements of 'model_select_param$model_selections' ",
      "must be in 'get_model_selections()'"
    )
  }
  if (!beautier::are_site_models(model_select_param$site_models)) {
    stop(
      "All elements of 'model_select_param$site_models' ",
      "must be site models"
    )
  }
  if (!beautier::are_clock_models(model_select_param$clock_models)) {
    stop(
      "All elements of 'model_select_param$clock_models' ",
      "must be clock models"
    )
  }
  if (!beautier::are_tree_priors(model_select_param$tree_priors)) {
    stop(
      "All elements of 'model_select_param$tree_priors' ",
      "must be tree priors"
    )
  }
}
